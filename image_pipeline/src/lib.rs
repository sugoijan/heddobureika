use std::io::Cursor;

use image::codecs::avif::AvifEncoder;
use image::imageops::FilterType;
use image::{ExtendedColorType, ImageBuffer, ImageEncoder, Rgb, Rgba};
use img_parts::{Bytes, ImageEXIF, ImageICC};
use moxcms::{
    curve_from_gamma, CicpColorPrimaries, CicpProfile, ColorProfile, Layout, MatrixCoefficients,
    TransferCharacteristics, TransformOptions,
};

#[derive(Debug, thiserror::Error)]
pub enum PipelineError {
    #[error("image decode failed: {0}")]
    Decode(String),
    #[error("image encode failed: {0}")]
    Encode(String),
    #[error("color profile error: {0}")]
    Profile(String),
    #[error("color transform failed: {0}")]
    Transform(String),
    #[error("invalid image dimensions")]
    Dimensions,
}

#[derive(Debug, Clone, Default)]
pub struct ImageMetadata {
    pub icc_profile: Option<Vec<u8>>,
    pub srgb: bool,
    pub gamma: Option<f64>,
    pub has_hdr_metadata: bool,
    pub cicp: Option<PngCicp>,
    pub orientation: Option<u16>,
}

#[derive(Debug, Clone, Copy)]
pub struct PngCicp {
    pub primaries: u8,
    pub transfer: u8,
    pub matrix: u8,
    pub full_range: bool,
}

#[derive(Debug, Clone, Copy)]
enum TransferKind {
    Pq,
    Hlg,
    Other,
}

impl TransferKind {
    fn is_hdr(self) -> bool {
        matches!(self, TransferKind::Pq | TransferKind::Hlg)
    }
}

struct SourceProfileInfo {
    profile: ColorProfile,
    transfer: TransferKind,
    cicp: Option<CicpProfile>,
    from_icc: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum OutputKind {
    SrgbRgb8,
    SrgbRgb16,
    SrgbRgba8,
}

#[derive(Debug, Clone, Copy)]
pub enum AlphaMode {
    Discard,
    Preserve,
}

#[derive(Debug, Clone, Copy)]
pub enum ToneMap {
    Auto,
}

#[derive(Debug, Clone, Copy)]
pub struct PipelineConfig {
    pub quality: u8,
    pub speed: u8,
    pub max_dim: Option<u32>,
    pub alpha_mode: AlphaMode,
    pub tone_map: ToneMap,
    pub dither: bool,
    pub output: OutputKind,
    pub assume_opaque: bool,
}

impl Default for PipelineConfig {
    fn default() -> Self {
        Self {
            quality: 95,
            speed: 6,
            max_dim: None,
            alpha_mode: AlphaMode::Discard,
            tone_map: ToneMap::Auto,
            dither: false,
            output: OutputKind::SrgbRgb8,
            assume_opaque: true,
        }
    }
}

pub struct ImagePipeline {
    config: PipelineConfig,
}

impl ImagePipeline {
    pub fn new(config: PipelineConfig) -> Self {
        Self { config }
    }

    pub fn process(&self, bytes: &[u8]) -> Result<PipelineImage, PipelineError> {
        let (rgba, metadata) = decode_rgba16(bytes)?;
        let rgba = resize_rgba16_to_max_dim(rgba, self.config.max_dim);
        let (width, height) = rgba.dimensions();
        if width == 0 || height == 0 {
            return Err(PipelineError::Dimensions);
        }

        let pixels = match self.config.output {
            OutputKind::SrgbRgb8 => {
                let rgb = map_rgba16_to_srgb_rgb8(
                    &rgba,
                    &metadata,
                    self.config.assume_opaque,
                    self.config.dither,
                )?;
                PixelData::SrgbRgb8(rgb)
            }
            OutputKind::SrgbRgb16 => {
                let rgb = map_rgba16_to_srgb_rgb16(&rgba, &metadata, self.config.assume_opaque)?;
                PixelData::SrgbRgb16(rgb)
            }
            OutputKind::SrgbRgba8 => {
                let rgba = map_rgba16_to_srgb_rgba8(&rgba, &metadata, self.config.dither)?;
                PixelData::SrgbRgba8(rgba)
            }
        };

        Ok(PipelineImage {
            width,
            height,
            pixels,
            metadata,
        })
    }
}

#[derive(Debug, Clone)]
pub struct PipelineImage {
    pub width: u32,
    pub height: u32,
    pub pixels: PixelData,
    pub metadata: ImageMetadata,
}

#[derive(Debug, Clone)]
pub struct TranscodeResult {
    pub bytes: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub src_metadata: ImageMetadata,
}

#[derive(Debug, Clone)]
pub enum PixelData {
    SrgbRgb8(image::RgbImage),
    SrgbRgb16(ImageBuffer<Rgb<u16>, Vec<u16>>),
    SrgbRgba8(image::RgbaImage),
}

pub fn transcode_to_avif(
    bytes: &[u8],
    config: PipelineConfig,
) -> Result<TranscodeResult, PipelineError> {
    let (rgba, metadata) = decode_rgba16(bytes)?;
    let rgba = resize_rgba16_to_max_dim(rgba, config.max_dim);
    let (width, height) = rgba.dimensions();
    if width == 0 || height == 0 {
        return Err(PipelineError::Dimensions);
    }

    let mut out = Vec::new();
    let quality = config.quality.min(100);
    let speed = config.speed.min(10);
    let encoder = AvifEncoder::new_with_speed_quality(&mut out, speed, quality)
        .with_num_threads(Some(1));

    match config.alpha_mode {
        AlphaMode::Discard => {
            let rgb = map_rgba16_to_srgb_rgb8(
                &rgba,
                &metadata,
                config.assume_opaque,
                config.dither,
            )?;
            encoder
                .write_image(&rgb, width, height, ExtendedColorType::Rgb8)
                .map_err(|err| PipelineError::Encode(err.to_string()))?;
        }
        AlphaMode::Preserve => {
            let rgba8 = map_rgba16_to_srgb_rgba8(&rgba, &metadata, config.dither)?;
            encoder
                .write_image(&rgba8, width, height, ExtendedColorType::Rgba8)
                .map_err(|err| PipelineError::Encode(err.to_string()))?;
        }
    }

    Ok(TranscodeResult {
        bytes: out,
        width,
        height,
        src_metadata: metadata,
    })
}

pub fn decode_rgba16(
    bytes: &[u8],
) -> Result<(ImageBuffer<Rgba<u16>, Vec<u16>>, ImageMetadata), PipelineError> {
    let metadata = read_image_metadata(bytes)?;
    let image = image::load_from_memory(bytes).map_err(|err| PipelineError::Decode(err.to_string()))?;
    let rgba = image.to_rgba16();
    let rgba = apply_exif_orientation(rgba, metadata.orientation);
    Ok((rgba, metadata))
}

pub fn decode_rgba32f(
    bytes: &[u8],
) -> Result<(ImageBuffer<Rgba<f32>, Vec<f32>>, ImageMetadata), PipelineError> {
    let metadata = read_image_metadata(bytes)?;
    let image = image::load_from_memory(bytes).map_err(|err| PipelineError::Decode(err.to_string()))?;
    let rgba = image.to_rgba32f();
    let rgba = apply_exif_orientation(rgba, metadata.orientation);
    Ok((rgba, metadata))
}

fn linear_srgb_profile() -> ColorProfile {
    let mut profile = ColorProfile::new_srgb();
    let curve = curve_from_gamma(1.0);
    profile.red_trc = Some(curve.clone());
    profile.green_trc = Some(curve.clone());
    profile.blue_trc = Some(curve);
    profile.cicp = Some(CicpProfile {
        color_primaries: CicpColorPrimaries::Bt709,
        transfer_characteristics: TransferCharacteristics::Linear,
        matrix_coefficients: MatrixCoefficients::Bt709,
        full_range: false,
    });
    profile
}

fn rgba16_to_rgb_f32(rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>) -> Vec<f32> {
    const INV_U16_MAX: f32 = 1.0 / 65535.0;
    let mut out = Vec::with_capacity(rgba.as_raw().len() / 4 * 3);
    for chunk in rgba.as_raw().chunks_exact(4) {
        out.push(chunk[0] as f32 * INV_U16_MAX);
        out.push(chunk[1] as f32 * INV_U16_MAX);
        out.push(chunk[2] as f32 * INV_U16_MAX);
    }
    out
}

fn map_rgba16_to_linear_srgb_f32(
    rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>,
    src_profile: &ColorProfile,
) -> Result<Vec<f32>, PipelineError> {
    let options = TransformOptions {
        allow_use_cicp_transfer: true,
        ..Default::default()
    };
    let src_rgb = rgba16_to_rgb_f32(rgba);
    let mut dst_rgb = vec![0.0f32; src_rgb.len()];
    let linear_profile = linear_srgb_profile();
    let transform = src_profile
        .create_transform_f32(Layout::Rgb, &linear_profile, Layout::Rgb, options)
        .map_err(|err| PipelineError::Transform(err.to_string()))?;
    transform
        .transform(&src_rgb, &mut dst_rgb)
        .map_err(|err| PipelineError::Transform(err.to_string()))?;
    Ok(dst_rgb)
}

fn transform_rgb16_with_profile(
    rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>,
    src_profile: &ColorProfile,
    assume_opaque: bool,
) -> Result<Vec<u16>, PipelineError> {
    let options = TransformOptions {
        // Enables CICP transfer functions when available (PQ/HLG, etc.).
        allow_use_cicp_transfer: true,
        ..Default::default()
    };
    let dst_profile = ColorProfile::new_srgb();

    // NOTE: moxcms does not currently handle alpha; drop it unless a later
    // pipeline stage explicitly composites it.
    let src_rgb = if assume_opaque {
        rgba16_to_rgb16(rgba)
    } else {
        rgba16_to_rgb16(rgba)
    };
    let mut dst_rgb = vec![0u16; src_rgb.len()];

    let transform = src_profile
        .create_transform_16bit(Layout::Rgb, &dst_profile, Layout::Rgb, options)
        .map_err(|err| PipelineError::Transform(err.to_string()))?;
    transform
        .transform(&src_rgb, &mut dst_rgb)
        .map_err(|err| PipelineError::Transform(err.to_string()))?;
    Ok(dst_rgb)
}

fn tone_map_hdr_rgb(rgb: &mut [f32], transfer: TransferKind) {
    const LUMA_R: f32 = 0.2126;
    const LUMA_G: f32 = 0.7152;
    const LUMA_B: f32 = 0.0722;

    for chunk in rgb.chunks_exact_mut(3) {
        let r = chunk[0];
        let g = chunk[1];
        let b = chunk[2];
        let y = (r * LUMA_R + g * LUMA_G + b * LUMA_B).max(0.0);
        let mapped = match transfer {
            TransferKind::Pq => tone_map_pq_luma(y),
            TransferKind::Hlg => tone_map_hlg_luma(y),
            TransferKind::Other => y,
        };
        let scale = if y > 0.0 { mapped / y } else { 0.0 };
        chunk[0] = r * scale;
        chunk[1] = g * scale;
        chunk[2] = b * scale;
    }
}

fn tone_map_pq_luma(value: f32) -> f32 {
    const SDR_WHITE_NITS: f32 = 100.0;
    const MASTERING_WHITE_NITS: f32 = 10_000.0;
    const TARGET_WHITE_NITS: f32 = 100.0;
    const TARGET_BLACK_NITS: f32 = 0.0;
    const MASTERING_BLACK_NITS: f32 = 0.0;

    let e_lb = pq_oetf(MASTERING_BLACK_NITS / MASTERING_WHITE_NITS);
    let e_lw = pq_oetf(1.0);
    let e = pq_oetf(value.clamp(0.0, 1.0));
    let denom = (e_lw - e_lb).max(f32::EPSILON);
    let e1 = ((e - e_lb) / denom).clamp(0.0, 1.0);

    let min_lum = (pq_oetf(TARGET_BLACK_NITS / MASTERING_WHITE_NITS) - e_lb) / denom;
    let max_lum = (pq_oetf(TARGET_WHITE_NITS / MASTERING_WHITE_NITS) - e_lb) / denom;
    let ks = (1.5 * max_lum - 0.5).clamp(0.0, 1.0);
    let b = min_lum;

    let e2 = if e1 < ks {
        e1
    } else {
        let t = ((e1 - ks) / (1.0 - ks)).clamp(0.0, 1.0);
        let t2 = t * t;
        let t3 = t2 * t;
        (2.0 * t3 - 3.0 * t2 + 1.0) * ks
            + (t3 - 2.0 * t2 + t) * (1.0 - ks)
            + (-2.0 * t3 + 3.0 * t2) * max_lum
    };

    let e3 = (e2 + b * (1.0 - e2).powi(4)).clamp(0.0, 1.0);
    let e4 = e3 * (e_lw - e_lb) + e_lb;
    let mapped = pq_eotf(e4) * (MASTERING_WHITE_NITS / SDR_WHITE_NITS);
    mapped.clamp(0.0, 1.0)
}

fn tone_map_hlg_luma(value: f32) -> f32 {
    const REF_GAMMA: f32 = 1.2;
    const TARGET_LW: f32 = 100.0;
    let gamma_target = 1.2 + 0.42 * (TARGET_LW / 1000.0).log10();

    if value <= 0.0 {
        return 0.0;
    }

    let y_s = value.powf(1.0 / REF_GAMMA);
    let y_d = y_s.powf(gamma_target);
    y_d.clamp(0.0, 1.0)
}

fn pq_oetf(linear: f32) -> f32 {
    const M1: f32 = 0.159_301_76;
    const M2: f32 = 78.843_75;
    const C1: f32 = 0.835_937_5;
    const C2: f32 = 18.851_562;
    const C3: f32 = 18.687_5;

    let l = linear.max(0.0);
    let l_m1 = l.powf(M1);
    let num = C1 + C2 * l_m1;
    let den = 1.0 + C3 * l_m1;
    (num / den).powf(M2)
}

fn pq_eotf(encoded: f32) -> f32 {
    const M1: f32 = 0.159_301_76;
    const M2: f32 = 78.843_75;
    const C1: f32 = 0.835_937_5;
    const C2: f32 = 18.851_562;
    const C3: f32 = 18.687_5;

    let e = encoded.max(0.0);
    let e_1_m2 = e.powf(1.0 / M2);
    let num = (e_1_m2 - C1).max(0.0);
    let den = (C2 - C3 * e_1_m2).max(f32::EPSILON);
    (num / den).powf(1.0 / M1)
}

fn linear_to_srgb(linear: f32) -> f32 {
    let value = linear.max(0.0);
    if value <= 0.003_130_8 {
        12.92 * value
    } else {
        1.055 * value.powf(1.0 / 2.4) - 0.055
    }
}

fn linear_to_u8_srgb(linear: f32) -> u8 {
    let value = linear_to_srgb(linear).clamp(0.0, 1.0);
    (value * 255.0 + 0.5).floor() as u8
}

fn linear_to_u16_srgb(linear: f32) -> u16 {
    let value = linear_to_srgb(linear).clamp(0.0, 1.0);
    (value * 65535.0 + 0.5).floor() as u16
}

const BAYER_4X4: [u8; 16] = [
    0, 8, 2, 10,
    12, 4, 14, 6,
    3, 11, 1, 9,
    15, 7, 13, 5,
];

fn dither_offset_4x4(x: u32, y: u32) -> f32 {
    let idx = ((y & 3) << 2 | (x & 3)) as usize;
    (BAYER_4X4[idx] as f32 + 0.5) / 16.0 - 0.5
}

fn linear_to_u8_srgb_dithered(linear: f32, offset: f32) -> u8 {
    let value = linear_to_srgb(linear).clamp(0.0, 1.0);
    let quant = (value * 255.0 + offset).floor().clamp(0.0, 255.0);
    quant as u8
}

pub fn map_rgba16_to_srgb_rgb16(
    rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>,
    metadata: &ImageMetadata,
    assume_opaque: bool,
) -> Result<ImageBuffer<Rgb<u16>, Vec<u16>>, PipelineError> {
    let (width, height) = rgba.dimensions();
    if width == 0 || height == 0 {
        return Err(PipelineError::Dimensions);
    }

    let mut src_info = build_src_profile(metadata)?;
    loop {
        if src_info.transfer.is_hdr() {
            match map_rgba16_to_linear_srgb_f32(rgba, &src_info.profile) {
                Ok(mut linear_rgb) => {
                    if src_info.transfer.is_hdr() {
                        tone_map_hdr_rgb(&mut linear_rgb, src_info.transfer);
                    }
                    let mut out = Vec::with_capacity(linear_rgb.len());
                    for value in linear_rgb {
                        out.push(linear_to_u16_srgb(value));
                    }
                    return ImageBuffer::from_raw(width, height, out)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        } else {
            match transform_rgb16_with_profile(rgba, &src_info.profile, assume_opaque) {
                Ok(dst_rgb) => {
                    return ImageBuffer::from_raw(width, height, dst_rgb)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        }
    }
}

pub fn map_rgba16_to_srgb_rgb8(
    rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>,
    metadata: &ImageMetadata,
    assume_opaque: bool,
    dither: bool,
) -> Result<image::RgbImage, PipelineError> {
    let (width, height) = rgba.dimensions();
    let mut src_info = build_src_profile(metadata)?;
    loop {
        if src_info.transfer.is_hdr() {
            match map_rgba16_to_linear_srgb_f32(rgba, &src_info.profile) {
                Ok(mut linear_rgb) => {
                    if src_info.transfer.is_hdr() {
                        tone_map_hdr_rgb(&mut linear_rgb, src_info.transfer);
                    }
                    let mut out = Vec::with_capacity(linear_rgb.len());
                    for (idx, chunk) in linear_rgb.chunks_exact(3).enumerate() {
                        if dither {
                            let x = (idx as u32) % width;
                            let y = (idx as u32) / width;
                            let offset = dither_offset_4x4(x, y);
                            out.push(linear_to_u8_srgb_dithered(chunk[0], offset));
                            out.push(linear_to_u8_srgb_dithered(chunk[1], offset));
                            out.push(linear_to_u8_srgb_dithered(chunk[2], offset));
                        } else {
                            out.push(linear_to_u8_srgb(chunk[0]));
                            out.push(linear_to_u8_srgb(chunk[1]));
                            out.push(linear_to_u8_srgb(chunk[2]));
                        }
                    }
                    return image::RgbImage::from_raw(width, height, out)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        } else {
            match transform_rgb16_with_profile(rgba, &src_info.profile, assume_opaque) {
                Ok(dst_rgb) => {
                    let mut out = Vec::with_capacity(dst_rgb.len());
                    for value in dst_rgb {
                        out.push(u16_to_u8(value));
                    }
                    return image::RgbImage::from_raw(width, height, out)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        }
    }
}

pub fn map_rgba16_to_srgb_rgba8(
    rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>,
    metadata: &ImageMetadata,
    dither: bool,
) -> Result<image::RgbaImage, PipelineError> {
    let (width, height) = rgba.dimensions();
    let mut src_info = build_src_profile(metadata)?;
    loop {
        if src_info.transfer.is_hdr() {
            match map_rgba16_to_linear_srgb_f32(rgba, &src_info.profile) {
                Ok(mut linear_rgb) => {
                    tone_map_hdr_rgb(&mut linear_rgb, src_info.transfer);
                    let mut out = Vec::with_capacity((width as usize) * (height as usize) * 4);
                    for (idx, chunk) in linear_rgb.chunks_exact(3).enumerate() {
                        if dither {
                            let x = (idx as u32) % width;
                            let y = (idx as u32) / width;
                            let offset = dither_offset_4x4(x, y);
                            out.push(linear_to_u8_srgb_dithered(chunk[0], offset));
                            out.push(linear_to_u8_srgb_dithered(chunk[1], offset));
                            out.push(linear_to_u8_srgb_dithered(chunk[2], offset));
                        } else {
                            out.push(linear_to_u8_srgb(chunk[0]));
                            out.push(linear_to_u8_srgb(chunk[1]));
                            out.push(linear_to_u8_srgb(chunk[2]));
                        }
                        out.push(u16_to_u8(rgba.as_raw()[idx * 4 + 3]));
                    }
                    return image::RgbaImage::from_raw(width, height, out)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        } else {
            match transform_rgb16_with_profile(rgba, &src_info.profile, false) {
                Ok(dst_rgb) => {
                    let mut out = Vec::with_capacity((width as usize) * (height as usize) * 4);
                    let mut rgb_iter = dst_rgb.chunks_exact(3);
                    let mut alpha_iter = rgba.as_raw().chunks_exact(4);
                    while let (Some(rgb), Some(src)) = (rgb_iter.next(), alpha_iter.next()) {
                        out.push(u16_to_u8(rgb[0]));
                        out.push(u16_to_u8(rgb[1]));
                        out.push(u16_to_u8(rgb[2]));
                        out.push(u16_to_u8(src[3]));
                    }
                    return image::RgbaImage::from_raw(width, height, out)
                        .ok_or(PipelineError::Dimensions);
                }
                Err(err) => {
                    if let Some(fallback) = build_fallback_profile(metadata, &src_info) {
                        src_info = fallback;
                        continue;
                    }
                    return Err(err);
                }
            }
        }
    }
}

pub fn process_to_srgb_rgb8(bytes: &[u8]) -> Result<image::RgbImage, PipelineError> {
    let pipeline = ImagePipeline::new(PipelineConfig::default());
    let output = pipeline.process(bytes)?;
    match output.pixels {
        PixelData::SrgbRgb8(image) => Ok(image),
        PixelData::SrgbRgb16(image) => {
            let (width, height) = image.dimensions();
            let mut out = Vec::with_capacity((width as usize) * (height as usize) * 3);
            for value in image.into_raw() {
                out.push(u16_to_u8(value));
            }
            image::RgbImage::from_raw(width, height, out).ok_or(PipelineError::Dimensions)
        }
        PixelData::SrgbRgba8(image) => rgba8_to_rgb8(image),
    }
}

pub fn extract_icc_profile(bytes: &[u8]) -> Option<Vec<u8>> {
    let data = Bytes::copy_from_slice(bytes);

    if let Ok(jpeg) = img_parts::jpeg::Jpeg::from_bytes(data.clone()) {
        if let Some(profile) = jpeg.icc_profile() {
            return Some(profile.as_ref().to_vec());
        }
    }
    if let Ok(png) = img_parts::png::Png::from_bytes(data.clone()) {
        if let Some(profile) = png.icc_profile() {
            return Some(profile.as_ref().to_vec());
        }
    }
    if let Ok(webp) = img_parts::webp::WebP::from_bytes(data.clone()) {
        if let Some(profile) = webp.icc_profile() {
            return Some(profile.as_ref().to_vec());
        }
    }
    None
}

pub fn extract_exif(bytes: &[u8]) -> Option<Vec<u8>> {
    let data = Bytes::copy_from_slice(bytes);

    if let Ok(jpeg) = img_parts::jpeg::Jpeg::from_bytes(data.clone()) {
        if let Some(exif) = jpeg.exif() {
            return Some(exif.as_ref().to_vec());
        }
    }
    if let Ok(png) = img_parts::png::Png::from_bytes(data.clone()) {
        if let Some(exif) = png.exif() {
            return Some(exif.as_ref().to_vec());
        }
    }
    if let Ok(webp) = img_parts::webp::WebP::from_bytes(data.clone()) {
        if let Some(exif) = webp.exif() {
            return Some(exif.as_ref().to_vec());
        }
    }
    None
}

pub fn extract_exif_orientation(bytes: &[u8]) -> Option<u16> {
    let exif = extract_exif(bytes)?;
    parse_exif_orientation(&exif)
}

pub fn read_image_metadata(bytes: &[u8]) -> Result<ImageMetadata, PipelineError> {
    let mut metadata = ImageMetadata::default();
    metadata.icc_profile = extract_icc_profile(bytes);
    metadata.orientation = extract_exif_orientation(bytes);

    if is_png(bytes) {
        if let Ok(png_meta) = read_png_metadata(bytes) {
            if metadata.icc_profile.is_none() {
                metadata.icc_profile = png_meta.icc_profile;
            }
            metadata.srgb = png_meta.srgb;
            metadata.gamma = png_meta.gamma;
            metadata.has_hdr_metadata = png_meta.has_hdr_metadata;
            metadata.cicp = png_meta.cicp;
        }
    }

    Ok(metadata)
}

fn read_png_metadata(bytes: &[u8]) -> Result<ImageMetadata, PipelineError> {
    let decoder = png::Decoder::new(Cursor::new(bytes));
    let reader = decoder
        .read_info()
        .map_err(|err| PipelineError::Decode(err.to_string()))?;
    let info = reader.info();

    let mut metadata = ImageMetadata::default();
    if let Some(icc) = info.icc_profile.as_ref() {
        metadata.icc_profile = Some(icc.as_ref().to_vec());
    }
    metadata.srgb = info.srgb.is_some();
    metadata.gamma = info
        .gama_chunk
        .map(|value| f64::from(value.into_value()));

    if let Some(cicp) = info.coding_independent_code_points.as_ref() {
        metadata.cicp = Some(PngCicp {
            primaries: cicp.color_primaries,
            transfer: cicp.transfer_function,
            matrix: cicp.matrix_coefficients,
            full_range: cicp.is_video_full_range_image,
        });
        metadata.has_hdr_metadata = true;
    }
    if info.mastering_display_color_volume.is_some() || info.content_light_level.is_some() {
        metadata.has_hdr_metadata = true;
    }

    Ok(metadata)
}

fn transfer_kind_from_cicp(cicp: &CicpProfile) -> TransferKind {
    match cicp.transfer_characteristics {
        TransferCharacteristics::Smpte2084 => TransferKind::Pq,
        TransferCharacteristics::Hlg => TransferKind::Hlg,
        _ => TransferKind::Other,
    }
}

fn source_profile_info_from_profile(profile: ColorProfile, from_icc: bool) -> SourceProfileInfo {
    let cicp = profile.cicp.clone();
    let transfer = cicp
        .as_ref()
        .map(transfer_kind_from_cicp)
        .unwrap_or(TransferKind::Other);
    SourceProfileInfo {
        profile,
        transfer,
        cicp,
        from_icc,
    }
}

fn cicp_profile_from_png(cicp: &PngCicp) -> Option<CicpProfile> {
    let primaries = CicpColorPrimaries::try_from(cicp.primaries).ok()?;
    let transfer = TransferCharacteristics::try_from(cicp.transfer).ok()?;
    let matrix = MatrixCoefficients::try_from(cicp.matrix).ok()?;
    Some(CicpProfile {
        color_primaries: primaries,
        transfer_characteristics: transfer,
        matrix_coefficients: matrix,
        full_range: cicp.full_range,
    })
}

fn build_src_profile_from_metadata(
    metadata: &ImageMetadata,
    fallback_cicp: Option<CicpProfile>,
) -> Result<SourceProfileInfo, PipelineError> {
    if let Some(cicp) = fallback_cicp {
        let profile = ColorProfile::new_from_cicp(cicp.clone());
        return Ok(SourceProfileInfo {
            profile,
            transfer: transfer_kind_from_cicp(&cicp),
            cicp: Some(cicp),
            from_icc: false,
        });
    }

    if let Some(cicp) = metadata.cicp.as_ref().and_then(cicp_profile_from_png) {
        let profile = ColorProfile::new_from_cicp(cicp.clone());
        return Ok(SourceProfileInfo {
            profile,
            transfer: transfer_kind_from_cicp(&cicp),
            cicp: Some(cicp),
            from_icc: false,
        });
    }

    if metadata.srgb {
        return Ok(source_profile_info_from_profile(
            ColorProfile::new_srgb(),
            false,
        ));
    }

    if let Some(gamma) = metadata.gamma {
        if gamma > 0.0 {
            let mut profile = ColorProfile::new_srgb();
            let decode_gamma = (1.0 / gamma as f32).max(0.0001);
            let curve = curve_from_gamma(decode_gamma);
            profile.red_trc = Some(curve.clone());
            profile.green_trc = Some(curve.clone());
            profile.blue_trc = Some(curve);
            profile.cicp = None;
            return Ok(source_profile_info_from_profile(profile, false));
        }
    }

    Ok(source_profile_info_from_profile(
        ColorProfile::new_srgb(),
        false,
    ))
}

fn build_src_profile(metadata: &ImageMetadata) -> Result<SourceProfileInfo, PipelineError> {
    if let Some(icc) = metadata.icc_profile.as_ref() {
        let profile = ColorProfile::new_from_slice(icc)
            .map_err(|err| PipelineError::Profile(err.to_string()))?;
        return Ok(source_profile_info_from_profile(profile, true));
    }

    build_src_profile_from_metadata(metadata, None)
}

fn build_fallback_profile(
    metadata: &ImageMetadata,
    primary: &SourceProfileInfo,
) -> Option<SourceProfileInfo> {
    if !primary.from_icc {
        return None;
    }
    build_src_profile_from_metadata(metadata, primary.cicp.clone()).ok()
}

#[derive(Clone, Copy)]
enum ExifEndian {
    Little,
    Big,
}

fn parse_exif_orientation(exif: &[u8]) -> Option<u16> {
    let data = if exif.starts_with(b"Exif\0\0") {
        exif.get(6..)?
    } else {
        exif
    };
    if data.len() < 8 {
        return None;
    }
    let endian = match &data[..2] {
        b"II" => ExifEndian::Little,
        b"MM" => ExifEndian::Big,
        _ => return None,
    };
    if read_exif_u16(data, 2, endian)? != 42 {
        return None;
    }
    let ifd_offset = read_exif_u32(data, 4, endian)? as usize;
    if ifd_offset + 2 > data.len() {
        return None;
    }
    let entries = read_exif_u16(data, ifd_offset, endian)? as usize;
    let mut entry_offset = ifd_offset + 2;
    for _ in 0..entries {
        if entry_offset + 12 > data.len() {
            break;
        }
        let tag = read_exif_u16(data, entry_offset, endian)?;
        if tag == 0x0112 {
            let field_type = read_exif_u16(data, entry_offset + 2, endian)?;
            let count = read_exif_u32(data, entry_offset + 4, endian)?;
            if field_type == 3 && count >= 1 {
                let inline = &data[entry_offset + 8..entry_offset + 12];
                let value = match endian {
                    ExifEndian::Little => u16::from_le_bytes([inline[0], inline[1]]),
                    ExifEndian::Big => u16::from_be_bytes([inline[0], inline[1]]),
                };
                if value != 0 {
                    return Some(value);
                }
            }
            if field_type == 3 && count > 1 {
                let value_offset = read_exif_u32(data, entry_offset + 8, endian)? as usize;
                if value_offset + 2 <= data.len() {
                    let value = read_exif_u16(data, value_offset, endian)?;
                    if value != 0 {
                        return Some(value);
                    }
                }
            }
            return None;
        }
        entry_offset += 12;
    }
    None
}

fn read_exif_u16(data: &[u8], offset: usize, endian: ExifEndian) -> Option<u16> {
    let bytes = data.get(offset..offset + 2)?;
    Some(match endian {
        ExifEndian::Little => u16::from_le_bytes([bytes[0], bytes[1]]),
        ExifEndian::Big => u16::from_be_bytes([bytes[0], bytes[1]]),
    })
}

fn read_exif_u32(data: &[u8], offset: usize, endian: ExifEndian) -> Option<u32> {
    let bytes = data.get(offset..offset + 4)?;
    Some(match endian {
        ExifEndian::Little => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        ExifEndian::Big => u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
    })
}

fn rgba16_to_rgb16(rgba: &ImageBuffer<Rgba<u16>, Vec<u16>>) -> Vec<u16> {
    let mut out = Vec::with_capacity(rgba.as_raw().len() / 4 * 3);
    for chunk in rgba.as_raw().chunks_exact(4) {
        out.push(chunk[0]);
        out.push(chunk[1]);
        out.push(chunk[2]);
    }
    out
}

fn resize_rgba16_to_max_dim(
    rgba: ImageBuffer<Rgba<u16>, Vec<u16>>,
    max_dim: Option<u32>,
) -> ImageBuffer<Rgba<u16>, Vec<u16>> {
    let Some(max_dim) = max_dim else {
        return rgba;
    };
    if max_dim == 0 {
        return rgba;
    }
    let (width, height) = rgba.dimensions();
    let max_axis = width.max(height);
    if max_axis <= max_dim {
        return rgba;
    }
    let scale = max_dim as f32 / max_axis as f32;
    let next_width = ((width as f32) * scale).round().max(1.0) as u32;
    let next_height = ((height as f32) * scale).round().max(1.0) as u32;
    image::imageops::resize(&rgba, next_width, next_height, FilterType::Lanczos3)
}

fn apply_exif_orientation<P>(
    image: ImageBuffer<P, Vec<P::Subpixel>>,
    orientation: Option<u16>,
) -> ImageBuffer<P, Vec<P::Subpixel>>
where
    P: image::Pixel + 'static,
    P::Subpixel: 'static,
{
    match orientation {
        Some(2) => image::imageops::flip_horizontal(&image),
        Some(3) => image::imageops::rotate180(&image),
        Some(4) => image::imageops::flip_vertical(&image),
        Some(5) => {
            let flipped = image::imageops::flip_horizontal(&image);
            image::imageops::rotate270(&flipped)
        }
        Some(6) => image::imageops::rotate90(&image),
        Some(7) => {
            let flipped = image::imageops::flip_horizontal(&image);
            image::imageops::rotate90(&flipped)
        }
        Some(8) => image::imageops::rotate270(&image),
        _ => image,
    }
}

fn rgba8_to_rgb8(image: image::RgbaImage) -> Result<image::RgbImage, PipelineError> {
    let (width, height) = image.dimensions();
    let mut out = Vec::with_capacity((width as usize) * (height as usize) * 3);
    for chunk in image.into_raw().chunks_exact(4) {
        out.push(chunk[0]);
        out.push(chunk[1]);
        out.push(chunk[2]);
    }
    image::RgbImage::from_raw(width, height, out).ok_or(PipelineError::Dimensions)
}

fn u16_to_u8(value: u16) -> u8 {
    ((value as u32 + 128) / 257) as u8
}

fn is_png(bytes: &[u8]) -> bool {
    bytes.starts_with(b"\x89PNG\r\n\x1a\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SRGB_PNG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/srgb.png"
    ));
    const GAMA_PNG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/gama.png"
    ));
    const PQ_PNG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/pq_cicp.png"
    ));
    const HLG_PNG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/hlg_cicp.png"
    ));
    const ICC_JPEG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/icc.jpg"
    ));
    const EXIF_JPEG: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/exif_orientation_6.jpg"
    ));

    #[test]
    fn metadata_reads_srgb_chunk() {
        let metadata = read_image_metadata(SRGB_PNG).expect("metadata");
        assert!(metadata.srgb);
        assert!(metadata.gamma.is_none());
    }

    #[test]
    fn metadata_reads_gamma_chunk() {
        let metadata = read_image_metadata(GAMA_PNG).expect("metadata");
        let gamma = metadata.gamma.expect("gamma");
        let expected = 0.45455f64;
        assert!((gamma - expected).abs() < 1e-4);
    }

    #[test]
    fn metadata_reads_cicp_pq_hlg() {
        let pq = read_image_metadata(PQ_PNG).expect("metadata");
        let pq_cicp = pq.cicp.expect("pq cicp");
        assert_eq!(pq_cicp.primaries, 9);
        assert_eq!(pq_cicp.transfer, 16);
        assert_eq!(pq_cicp.matrix, 0);

        let hlg = read_image_metadata(HLG_PNG).expect("metadata");
        let hlg_cicp = hlg.cicp.expect("hlg cicp");
        assert_eq!(hlg_cicp.primaries, 9);
        assert_eq!(hlg_cicp.transfer, 18);
        assert_eq!(hlg_cicp.matrix, 0);
    }

    #[test]
    fn metadata_reads_icc_profile() {
        let metadata = read_image_metadata(ICC_JPEG).expect("metadata");
        assert!(metadata.icc_profile.is_some());
        let profile = build_src_profile(&metadata).expect("profile");
        assert!(profile.from_icc);
    }

    #[test]
    fn metadata_reads_exif_orientation() {
        let metadata = read_image_metadata(EXIF_JPEG).expect("metadata");
        assert_eq!(metadata.orientation, Some(6));
    }

    #[test]
    fn tone_map_outputs_within_bounds() {
        let (rgba, metadata) = decode_rgba16(PQ_PNG).expect("decode");
        let rgb = map_rgba16_to_srgb_rgb8(&rgba, &metadata, true, false).expect("map");
        for value in rgb.into_raw() {
            assert!(value <= 255);
        }

        let (rgba, metadata) = decode_rgba16(HLG_PNG).expect("decode");
        let rgb = map_rgba16_to_srgb_rgb8(&rgba, &metadata, true, false).expect("map");
        for value in rgb.into_raw() {
            assert!(value <= 255);
        }
    }

    #[test]
    fn profile_selection_prefers_icc_over_cicp() {
        let mut metadata = read_image_metadata(ICC_JPEG).expect("metadata");
        metadata.cicp = Some(PngCicp {
            primaries: 9,
            transfer: 16,
            matrix: 0,
            full_range: true,
        });
        let profile = build_src_profile(&metadata).expect("profile");
        assert!(profile.from_icc);
    }
}
