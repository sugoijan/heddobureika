use crate::core::{
    GridChoice, Piece, PiecePaths, CORNER_RADIUS_RATIO, EMBOSS_OPACITY, EMBOSS_RIM,
    WGPU_EDGE_AA_DEFAULT, WGPU_RENDER_SCALE_MIN,
};
use bytemuck::{Pod, Zeroable};
use glyphon::{
    Attrs, Buffer, Cache, Color, Family, FontSystem, Metrics, Resolution, Shaping, SwashCache,
    TextArea, TextAtlas, TextBounds, TextRenderer, Viewport,
};
use glyphon::cosmic_text::Align;
use js_sys::Date;
use std::rc::Rc;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{console, CanvasRenderingContext2d, HtmlCanvasElement, HtmlImageElement, Path2d};
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct Vertex {
    pos: [f32; 2],
}

impl Vertex {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[wgpu::VertexAttribute {
                format: wgpu::VertexFormat::Float32x2,
                offset: 0,
                shader_location: 0,
            }],
        }
    }
}

const QUAD_VERTICES: [Vertex; 4] = [
    Vertex { pos: [-0.5, -0.5] },
    Vertex { pos: [0.5, -0.5] },
    Vertex { pos: [0.5, 0.5] },
    Vertex { pos: [-0.5, 0.5] },
];

const QUAD_INDICES: [u16; 6] = [0, 1, 2, 0, 2, 3];

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub(crate) struct Instance {
    pub(crate) pos: [f32; 2],
    pub(crate) size: [f32; 2],
    pub(crate) rotation: f32,
    pub(crate) flip: f32,
    pub(crate) hover: f32,
    pub(crate) _pad: f32,
    pub(crate) piece_origin: [f32; 2],
    pub(crate) mask_origin: [f32; 2],
}

#[derive(Clone, Copy)]
pub(crate) struct InstanceBatch {
    pub(crate) start: u32,
    pub(crate) count: u32,
    pub(crate) draw_outline: bool,
}

pub(crate) struct InstanceSet {
    pub(crate) instances: Vec<Instance>,
    pub(crate) batches: Vec<InstanceBatch>,
}

impl Instance {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Instance>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Instance,
            attributes: &[
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 0,
                    shader_location: 1,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 8,
                    shader_location: 2,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 16,
                    shader_location: 3,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 20,
                    shader_location: 4,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 24,
                    shader_location: 5,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 32,
                    shader_location: 6,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 40,
                    shader_location: 7,
                },
            ],
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub(crate) struct FrameInstance {
    pub(crate) pos: [f32; 2],
    pub(crate) size: [f32; 2],
    pub(crate) rotation: f32,
    pub(crate) _pad: f32,
}

impl FrameInstance {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<FrameInstance>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Instance,
            attributes: &[
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 0,
                    shader_location: 1,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 8,
                    shader_location: 2,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 16,
                    shader_location: 3,
                },
            ],
        }
    }
}

#[repr(C, align(16))]
#[derive(Copy, Clone, Pod, Zeroable)]
struct Globals {
    view_min: [f32; 2],
    view_size: [f32; 2],
    image_size: [f32; 2],
    atlas_size: [f32; 2],
    piece_size: [f32; 2],
    mask_pad: [f32; 2],
    render_mode: f32,
    output_gamma: f32,
    emboss_strength: f32,
    emboss_rim: f32,
    outline_width_px: f32,
    edge_aa: f32,
    _pad: [f32; 2],
}

#[repr(C, align(16))]
#[derive(Copy, Clone, Pod, Zeroable)]
struct FrameGlobals {
    view_min: [f32; 2],
    view_size: [f32; 2],
    color: [f32; 4],
    output_gamma: f32,
    _pad: [f32; 7],
}

struct FrameSegment {
    start: [f32; 2],
    len: f32,
    dir: [f32; 2],
}

fn push_arc_points(
    points: &mut Vec<[f32; 2]>,
    center: [f32; 2],
    radius: f32,
    start_angle: f32,
    end_angle: f32,
    steps: usize,
) {
    if steps == 0 {
        return;
    }
    for i in 1..=steps {
        let t = start_angle + (end_angle - start_angle) * (i as f32 / steps as f32);
        points.push([center[0] + radius * t.cos(), center[1] + radius * t.sin()]);
    }
}

fn build_frame_dashes(
    frame_x: f32,
    frame_y: f32,
    frame_w: f32,
    frame_h: f32,
    corner_radius: f32,
    dash_len: f32,
    gap_len: f32,
    stroke: f32,
) -> Vec<FrameInstance> {
    if frame_w <= 0.0 || frame_h <= 0.0 {
        return Vec::new();
    }
    let r = corner_radius
        .max(0.0)
        .min(frame_w * 0.5)
        .min(frame_h * 0.5);
    let right = frame_x + frame_w;
    let bottom = frame_y + frame_h;
    let mut points = Vec::new();
    points.push([frame_x + r, frame_y]);
    points.push([right - r, frame_y]);
    if r > 0.0 {
        let steps = ((r / 3.0).ceil() as usize).clamp(4, 16);
        push_arc_points(
            &mut points,
            [right - r, frame_y + r],
            r,
            -std::f32::consts::FRAC_PI_2,
            0.0,
            steps,
        );
    }
    points.push([right, bottom - r]);
    if r > 0.0 {
        let steps = ((r / 3.0).ceil() as usize).clamp(4, 16);
        push_arc_points(
            &mut points,
            [right - r, bottom - r],
            r,
            0.0,
            std::f32::consts::FRAC_PI_2,
            steps,
        );
    }
    points.push([frame_x + r, bottom]);
    if r > 0.0 {
        let steps = ((r / 3.0).ceil() as usize).clamp(4, 16);
        push_arc_points(
            &mut points,
            [frame_x + r, bottom - r],
            r,
            std::f32::consts::FRAC_PI_2,
            std::f32::consts::PI,
            steps,
        );
    }
    points.push([frame_x, frame_y + r]);
    if r > 0.0 {
        let steps = ((r / 3.0).ceil() as usize).clamp(4, 16);
        push_arc_points(
            &mut points,
            [frame_x + r, frame_y + r],
            r,
            std::f32::consts::PI,
            std::f32::consts::PI * 1.5,
            steps,
        );
    }
    if points.len() > 2 {
        let first = points[0];
        if let Some(last) = points.last() {
            let dx = last[0] - first[0];
            let dy = last[1] - first[1];
            if dx * dx + dy * dy < 1e-3 {
                points.pop();
            }
        }
    }

    let mut segments = Vec::new();
    for i in 0..points.len() {
        let p0 = points[i];
        let p1 = points[(i + 1) % points.len()];
        let dx = p1[0] - p0[0];
        let dy = p1[1] - p0[1];
        let len = (dx * dx + dy * dy).sqrt();
        if len > 1e-3 {
            segments.push(FrameSegment {
                start: p0,
                len,
                dir: [dx / len, dy / len],
            });
        }
    }
    if segments.is_empty() {
        return Vec::new();
    }
    let total_len: f32 = segments.iter().map(|seg| seg.len).sum();
    if total_len <= 1e-3 {
        return Vec::new();
    }

    let mut instances = Vec::new();
    let dash = dash_len.max(0.5);
    let gap = gap_len.max(0.5);
    let mut seg_index = 0usize;
    let mut seg_offset = 0.0;
    let mut remaining = total_len;
    let mut draw_phase = true;
    let mut phase_left = dash;

    while remaining > 1e-3 {
        let seg = &segments[seg_index];
        let available = (seg.len - seg_offset).max(0.0);
        if available <= 1e-3 {
            seg_index = (seg_index + 1) % segments.len();
            seg_offset = 0.0;
            continue;
        }
        let step = phase_left.min(available).min(remaining);
        if draw_phase && step > 0.1 {
            let sx = seg.start[0] + seg.dir[0] * seg_offset;
            let sy = seg.start[1] + seg.dir[1] * seg_offset;
            let ex = sx + seg.dir[0] * step;
            let ey = sy + seg.dir[1] * step;
            let cx = (sx + ex) * 0.5;
            let cy = (sy + ey) * 0.5;
            let rotation = seg.dir[1].atan2(seg.dir[0]);
            instances.push(FrameInstance {
                pos: [cx, cy],
                size: [step, stroke],
                rotation,
                _pad: 0.0,
            });
        }
        seg_offset += step;
        phase_left -= step;
        remaining -= step;
        if phase_left <= 1e-3 {
            draw_phase = !draw_phase;
            phase_left = if draw_phase { dash } else { gap };
        }
        if seg_offset >= seg.len - 1e-3 {
            seg_index = (seg_index + 1) % segments.len();
            seg_offset = 0.0;
        }
    }
    instances
}

pub(crate) struct MaskAtlasData {
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) pixels: Vec<u8>,
    pub(crate) origins: Vec<[f32; 2]>,
}

const FPS_SAMPLE_WINDOW_MS: f32 = 500.0;
const FPS_LABEL_FALLBACK: &str = "-- fps";
const FPS_FONT_FAMILY: &str = "KaoriGel";
const FPS_FONT_SIZE: f32 = 12.0;
const FPS_LINE_HEIGHT: f32 = 14.0;
const FPS_TEXT_PADDING_X: f32 = 12.0;
const FPS_TEXT_PADDING_Y: f32 = 12.0;
const FPS_TEXT_WIDTH: f32 = 80.0;

struct FpsTracker {
    last_ms: f32,
    frame_count: u32,
}

impl FpsTracker {
    fn new() -> Self {
        Self {
            last_ms: now_ms(),
            frame_count: 0,
        }
    }

    fn reset(&mut self) {
        self.last_ms = now_ms();
        self.frame_count = 0;
    }

    fn record_frame(&mut self, now_ms: f32) -> Option<f32> {
        self.frame_count = self.frame_count.saturating_add(1);
        let elapsed = now_ms - self.last_ms;
        if elapsed >= FPS_SAMPLE_WINDOW_MS {
            let fps = self.frame_count as f32 * 1000.0 / elapsed;
            self.last_ms = now_ms;
            self.frame_count = 0;
            Some(fps)
        } else {
            None
        }
    }
}

struct GlyphonState {
    font_system: FontSystem,
    swash_cache: SwashCache,
    atlas: TextAtlas,
    renderer: TextRenderer,
    viewport: Viewport,
    buffer: Buffer,
    last_text: String,
}

fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
}

pub(crate) struct WgpuRenderer {
    _canvas: HtmlCanvasElement,
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    _config: wgpu::SurfaceConfiguration,
    pipeline_outline: wgpu::RenderPipeline,
    pipeline_fill: wgpu::RenderPipeline,
    frame_pipeline: wgpu::RenderPipeline,
    bind_group_outline: wgpu::BindGroup,
    bind_group_fill: wgpu::BindGroup,
    globals_buffer_outline: wgpu::Buffer,
    globals_buffer_fill: wgpu::Buffer,
    frame_bind_group: wgpu::BindGroup,
    _frame_globals_buffer: wgpu::Buffer,
    frame_bg_bind_group: wgpu::BindGroup,
    _frame_bg_globals_buffer: wgpu::Buffer,
    frame_stroke_bind_group: wgpu::BindGroup,
    _frame_stroke_globals_buffer: wgpu::Buffer,
    globals: Globals,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    instance_buffer: wgpu::Buffer,
    frame_instance_buffer: wgpu::Buffer,
    index_count: u32,
    instance_count: u32,
    instance_capacity: u32,
    instance_batches: Vec<InstanceBatch>,
    frame_instance_count: u32,
    frame_bg_instance_buffer: wgpu::Buffer,
    frame_bg_instance_count: u32,
    frame_stroke_instance_buffer: wgpu::Buffer,
    frame_stroke_instance_count: u32,
    frame_clear_color: wgpu::Color,
    text_state: Option<GlyphonState>,
    show_fps: bool,
    fps_tracker: FpsTracker,
    fps_text: String,
    fps_color: Color,
    fps_logged_missing_text_state: bool,
    fps_logged_prepare_error: bool,
    render_scale: f32,
}

impl WgpuRenderer {
    pub(crate) async fn new(
        canvas: HtmlCanvasElement,
        image: HtmlImageElement,
        _pieces: Vec<Piece>,
        _paths: Vec<PiecePaths>,
        grid: GridChoice,
        piece_width: f32,
        piece_height: f32,
        view_min_x: f32,
        view_min_y: f32,
        view_width: f32,
        view_height: f32,
        mask_atlas: Rc<MaskAtlasData>,
        mask_pad: f32,
        render_scale: f32,
        is_dark_theme: bool,
    ) -> Result<Self, wasm_bindgen::JsValue> {
        let (art_pixels, art_width, art_height) = image_to_rgba(&image)?;
        let logical_width = piece_width * grid.cols as f32;
        let logical_height = piece_height * grid.rows as f32;

        let css_width = view_width.max(1.0);
        let css_height = view_height.max(1.0);
        let dpr = web_sys::window()
            .map(|window| window.device_pixel_ratio())
            .unwrap_or(1.0) as f32;
        let render_scale = render_scale.max(WGPU_RENDER_SCALE_MIN) * dpr;
        let canvas_width = (css_width * render_scale).max(1.0).ceil() as u32;
        let canvas_height = (css_height * render_scale).max(1.0).ceil() as u32;
        canvas.set_width(canvas_width);
        canvas.set_height(canvas_height);
        let _ = canvas.set_attribute(
            "style",
            &format!("width: {:.2}px; height: auto;", css_width),
        );

        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::BROWSER_WEBGPU | wgpu::Backends::GL,
            ..Default::default()
        });
        let surface = instance
            .create_surface(wgpu::SurfaceTarget::Canvas(canvas.clone()))
            .map_err(|err| {
                wasm_bindgen::JsValue::from_str(&format!("create_surface failed: {err:?}"))
            })?;
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .map_err(|err| {
                wasm_bindgen::JsValue::from_str(&format!("request_adapter failed: {err:?}"))
            })?;
        let limits =
            wgpu::Limits::downlevel_webgl2_defaults().using_resolution(adapter.limits());
        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: Some("heddobureika-device"),
                    required_features: wgpu::Features::empty(),
                    required_limits: limits,
                    ..Default::default()
                },
            )
            .await
            .map_err(|err| {
                wasm_bindgen::JsValue::from_str(&format!("request_device failed: {err:?}"))
            })?;

        let mut config = surface
            .get_default_config(&adapter, canvas_width, canvas_height)
            .ok_or_else(|| wasm_bindgen::JsValue::from_str("surface config failed"))?;
        let caps = surface.get_capabilities(&adapter);
        if let Some(format) = caps.formats.iter().copied().find(|fmt| fmt.is_srgb()) {
            config.format = format;
        }
        if caps
            .alpha_modes
            .iter()
            .any(|mode| *mode == wgpu::CompositeAlphaMode::Opaque)
        {
            config.alpha_mode = wgpu::CompositeAlphaMode::Opaque;
        }
        surface.configure(&device, &config);

        let is_srgb_output = config.format.is_srgb();
        let output_gamma = if is_srgb_output { 1.0 } else { 1.0 / 2.2 };
        let (body_color, workspace_fill, workspace_stroke, dash_color) = if is_dark_theme {
            (
                [0x14 as f32 / 255.0, 0x13 as f32 / 255.0, 0x12 as f32 / 255.0],
                [
                    32.0 / 255.0,
                    30.0 / 255.0,
                    28.0 / 255.0,
                    0.92,
                ],
                [1.0, 1.0, 1.0, 0.08],
                [1.0, 1.0, 1.0, 0.28],
            )
        } else {
            (
                [1.0, 1.0, 1.0],
                [
                    250.0 / 255.0,
                    247.0 / 255.0,
                    241.0 / 255.0,
                    0.9,
                ],
                [0.0, 0.0, 0.0, 0.08],
                [0.0, 0.0, 0.0, 0.25],
            )
        };
        let to_linear = |value: f32| value.powf(2.2);
        let (clear_r, clear_g, clear_b) = if is_srgb_output {
            (
                to_linear(body_color[0]),
                to_linear(body_color[1]),
                to_linear(body_color[2]),
            )
        } else {
            (body_color[0], body_color[1], body_color[2])
        };
        let frame_clear_color = wgpu::Color {
            r: clear_r as f64,
            g: clear_g as f64,
            b: clear_b as f64,
            a: 1.0,
        };
        let globals = Globals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            image_size: [logical_width, logical_height],
            atlas_size: [mask_atlas.width as f32, mask_atlas.height as f32],
            piece_size: [piece_width, piece_height],
            mask_pad: [mask_pad, mask_pad],
            render_mode: 0.0,
            output_gamma,
            emboss_strength: 0.0,
            emboss_rim: EMBOSS_RIM,
            outline_width_px: 2.0,
            edge_aa: WGPU_EDGE_AA_DEFAULT,
            _pad: [0.0; 2],
        };
        let globals_buffer_fill = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("globals-buffer-fill"),
            contents: bytemuck::bytes_of(&globals),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        let mut globals_outline = globals;
        globals_outline.render_mode = 1.0;
        let globals_buffer_outline = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("globals-buffer-outline"),
            contents: bytemuck::bytes_of(&globals_outline),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("piece-shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("wgpu_piece.wgsl").into()),
        });

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("piece-sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });

        let art_view = create_texture_view_from_pixels(
            &device,
            &queue,
            &art_pixels,
            art_width,
            art_height,
            wgpu::TextureFormat::Rgba8UnormSrgb,
            "art-texture",
        )?;
        let mask_view = create_texture_view_from_pixels(
            &device,
            &queue,
            &mask_atlas.pixels,
            mask_atlas.width,
            mask_atlas.height,
            wgpu::TextureFormat::Rgba8Unorm,
            "mask-texture",
        )?;

        let bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("piece-bind-group-layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 3,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });
        let make_bind_group = |label: &'static str,
                               globals_buffer: &wgpu::Buffer|
         -> wgpu::BindGroup {
            device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some(label),
                layout: &bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: globals_buffer.as_entire_binding(),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::TextureView(&art_view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: wgpu::BindingResource::TextureView(&mask_view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 3,
                        resource: wgpu::BindingResource::Sampler(&sampler),
                    },
                ],
            })
        };
        let bind_group_outline = make_bind_group("piece-bind-group-outline", &globals_buffer_outline);
        let bind_group_fill = make_bind_group("piece-bind-group-fill", &globals_buffer_fill);

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("piece-pipeline-layout"),
            bind_group_layouts: &[&bind_group_layout],
            immediate_size: 0,
        });

        let make_pipeline =
            |label: &'static str, blend: wgpu::BlendState| -> wgpu::RenderPipeline {
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some(label),
                    layout: Some(&pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &shader,
                        entry_point: Some("vs_main"),
                        buffers: &[Vertex::layout(), Instance::layout()],
                        compilation_options: wgpu::PipelineCompilationOptions::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &shader,
                        entry_point: Some("fs_main"),
                        targets: &[Some(wgpu::ColorTargetState {
                            format: config.format,
                            blend: Some(blend),
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                        compilation_options: wgpu::PipelineCompilationOptions::default(),
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        strip_index_format: None,
                        front_face: wgpu::FrontFace::Ccw,
                        cull_mode: None,
                        ..Default::default()
                    },
                    depth_stencil: None,
                    multisample: wgpu::MultisampleState::default(),
                    multiview_mask: None,
                    cache: None,
                })
            };
        let pipeline_outline =
            make_pipeline("piece-outline-pipeline", wgpu::BlendState::ALPHA_BLENDING);
        let pipeline_fill =
            make_pipeline("piece-fill-pipeline", wgpu::BlendState::ALPHA_BLENDING);

        let frame_dash_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: dash_color,
            output_gamma,
            _pad: [0.0; 7],
        };
        let frame_globals_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("frame-globals-buffer"),
            contents: bytemuck::bytes_of(&frame_dash_globals),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        let frame_bg_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: workspace_fill,
            output_gamma,
            _pad: [0.0; 7],
        };
        let frame_bg_globals_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("frame-bg-globals-buffer"),
                contents: bytemuck::bytes_of(&frame_bg_globals),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });
        let frame_stroke_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: workspace_stroke,
            output_gamma,
            _pad: [0.0; 7],
        };
        let frame_stroke_globals_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("frame-stroke-globals-buffer"),
                contents: bytemuck::bytes_of(&frame_stroke_globals),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });
        let frame_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("frame-shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("wgpu_frame.wgsl").into()),
        });
        let frame_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("frame-bind-group-layout"),
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
            });
        let frame_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("frame-bind-group"),
            layout: &frame_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: frame_globals_buffer.as_entire_binding(),
            }],
        });
        let frame_bg_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("frame-bg-bind-group"),
            layout: &frame_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: frame_bg_globals_buffer.as_entire_binding(),
            }],
        });
        let frame_stroke_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("frame-stroke-bind-group"),
            layout: &frame_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: frame_stroke_globals_buffer.as_entire_binding(),
            }],
        });
        let frame_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("frame-pipeline-layout"),
                bind_group_layouts: &[&frame_bind_group_layout],
                immediate_size: 0,
            });
        let frame_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("frame-pipeline"),
            layout: Some(&frame_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &frame_shader,
                entry_point: Some("vs_main"),
                buffers: &[Vertex::layout(), FrameInstance::layout()],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &frame_shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: None,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("quad-vertex-buffer"),
            contents: bytemuck::cast_slice(&QUAD_VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("quad-index-buffer"),
            contents: bytemuck::cast_slice(&QUAD_INDICES),
            usage: wgpu::BufferUsages::INDEX,
        });

        let instances = vec![Instance {
            pos: [0.0, 0.0],
            size: [0.0, 0.0],
            rotation: 0.0,
            flip: 0.0,
            hover: 0.0,
            _pad: 0.0,
            piece_origin: [0.0, 0.0],
            mask_origin: [0.0, 0.0],
        }];
        let instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("piece-instance-buffer"),
            contents: bytemuck::cast_slice(&instances),
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        });

        let mut frame_corner_radius = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
        let max_corner_radius = piece_width.min(piece_height) * 0.45;
        if frame_corner_radius > max_corner_radius {
            frame_corner_radius = max_corner_radius;
        }
        let workspace_stroke = 2.0;
        let workspace_center = [
            view_min_x + view_width * 0.5,
            view_min_y + view_height * 0.5,
        ];
        let frame_bg_instances = vec![FrameInstance {
            pos: workspace_center,
            size: [view_width, view_height],
            rotation: 0.0,
            _pad: 0.0,
        }];
        let frame_bg_instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("frame-bg-instance-buffer"),
            contents: bytemuck::cast_slice(&frame_bg_instances),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let half_stroke = workspace_stroke * 0.5;
        let left_x = view_min_x + half_stroke;
        let right_x = view_min_x + view_width - half_stroke;
        let top_y = view_min_y + half_stroke;
        let bottom_y = view_min_y + view_height - half_stroke;
        let frame_stroke_instances = vec![
            FrameInstance {
                pos: [workspace_center[0], top_y],
                size: [view_width, workspace_stroke],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [workspace_center[0], bottom_y],
                size: [view_width, workspace_stroke],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [left_x, workspace_center[1]],
                size: [workspace_stroke, view_height],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [right_x, workspace_center[1]],
                size: [workspace_stroke, view_height],
                rotation: 0.0,
                _pad: 0.0,
            },
        ];
        let frame_stroke_instance_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("frame-stroke-instance-buffer"),
                contents: bytemuck::cast_slice(&frame_stroke_instances),
                usage: wgpu::BufferUsages::VERTEX,
            });
        let frame_inset = 0.0;
        let frame_stroke = 1.5;
        let frame_x = frame_inset + frame_stroke * 0.5;
        let frame_y = frame_inset + frame_stroke * 0.5;
        let frame_w = logical_width - 2.0 * (frame_inset + frame_stroke * 0.5);
        let frame_h = logical_height - 2.0 * (frame_inset + frame_stroke * 0.5);
        let corner_radius = (frame_corner_radius - frame_inset - frame_stroke * 0.5).max(0.0);
        let frame_instances = build_frame_dashes(
            frame_x,
            frame_y,
            frame_w,
            frame_h,
            corner_radius,
            6.0,
            4.0,
            frame_stroke,
        );
        let frame_instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("frame-instance-buffer"),
            contents: bytemuck::cast_slice(&frame_instances),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let fps_color = if is_dark_theme {
            Color::rgb(255, 255, 255)
        } else {
            Color::rgb(24, 24, 24)
        };

        let renderer = Self {
            _canvas: canvas,
            surface,
            device,
            queue,
            _config: config,
            pipeline_outline,
            pipeline_fill,
            frame_pipeline,
            bind_group_outline,
            bind_group_fill,
            globals_buffer_outline,
            globals_buffer_fill,
            frame_bind_group,
            _frame_globals_buffer: frame_globals_buffer,
            frame_bg_bind_group,
            _frame_bg_globals_buffer: frame_bg_globals_buffer,
            frame_stroke_bind_group,
            _frame_stroke_globals_buffer: frame_stroke_globals_buffer,
            globals,
            vertex_buffer,
            index_buffer,
            instance_buffer,
            frame_instance_buffer,
            index_count: QUAD_INDICES.len() as u32,
            instance_count: 0,
            instance_capacity: instances.len() as u32,
            instance_batches: Vec::new(),
            frame_instance_count: frame_instances.len() as u32,
            frame_bg_instance_buffer,
            frame_bg_instance_count: frame_bg_instances.len() as u32,
            frame_stroke_instance_buffer,
            frame_stroke_instance_count: frame_stroke_instances.len() as u32,
            frame_clear_color,
            text_state: None,
            show_fps: false,
            fps_tracker: FpsTracker::new(),
            fps_text: String::new(),
            fps_color,
            fps_logged_missing_text_state: false,
            fps_logged_prepare_error: false,
            render_scale,
        };
        Ok(renderer)
    }

    pub(crate) fn render(&mut self) {
        let frame = match self.surface.get_current_texture() {
            Ok(frame) => frame,
            Err(_) => return,
        };
        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("piece-render-encoder"),
        });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("frame-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(self.frame_clear_color),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            render_pass.set_pipeline(&self.frame_pipeline);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass
                .set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
            if self.frame_bg_instance_count > 0 {
                render_pass.set_bind_group(0, &self.frame_bg_bind_group, &[]);
                render_pass.set_vertex_buffer(1, self.frame_bg_instance_buffer.slice(..));
                render_pass.draw_indexed(0..self.index_count, 0, 0..self.frame_bg_instance_count);
            }
            if self.frame_stroke_instance_count > 0 {
                render_pass.set_bind_group(0, &self.frame_stroke_bind_group, &[]);
                render_pass
                    .set_vertex_buffer(1, self.frame_stroke_instance_buffer.slice(..));
                render_pass
                    .draw_indexed(0..self.index_count, 0, 0..self.frame_stroke_instance_count);
            }
            if self.frame_instance_count > 0 {
                render_pass.set_bind_group(0, &self.frame_bind_group, &[]);
                render_pass.set_vertex_buffer(1, self.frame_instance_buffer.slice(..));
                render_pass.draw_indexed(0..self.index_count, 0, 0..self.frame_instance_count);
            }
        }

        let mut globals_outline = self.globals;
        globals_outline.render_mode = 1.0;
        self.queue.write_buffer(
            &self.globals_buffer_outline,
            0,
            bytemuck::bytes_of(&globals_outline),
        );
        let mut globals_fill = self.globals;
        globals_fill.render_mode = 0.0;
        self.queue.write_buffer(
            &self.globals_buffer_fill,
            0,
            bytemuck::bytes_of(&globals_fill),
        );
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("piece-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass
                .set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
            let stride = std::mem::size_of::<Instance>() as wgpu::BufferAddress;
            for batch in &self.instance_batches {
                if batch.count == 0 {
                    continue;
                }
                let start = batch.start as wgpu::BufferAddress * stride;
                let end = (batch.start + batch.count) as wgpu::BufferAddress * stride;
                let slice = self.instance_buffer.slice(start..end);
                if batch.draw_outline {
                    render_pass.set_pipeline(&self.pipeline_outline);
                    render_pass.set_bind_group(0, &self.bind_group_outline, &[]);
                    render_pass.set_vertex_buffer(1, slice.clone());
                    render_pass.draw_indexed(0..self.index_count, 0, 0..batch.count);
                }
                render_pass.set_pipeline(&self.pipeline_fill);
                render_pass.set_bind_group(0, &self.bind_group_fill, &[]);
                render_pass.set_vertex_buffer(1, slice);
                render_pass.draw_indexed(0..self.index_count, 0, 0..batch.count);
            }
        }

        let mut draw_text = false;
        if self.show_fps {
            if let Some(fps) = self.fps_tracker.record_frame(now_ms()) {
                self.fps_text = format!("{:.0} fps", fps);
            }
            if self.fps_text.is_empty() {
                self.fps_text = FPS_LABEL_FALLBACK.to_string();
            }
            if let Some(text_state) = self.text_state.as_mut() {
                self.fps_logged_missing_text_state = false;
                if text_state.last_text != self.fps_text {
                    let attrs = Attrs::new().family(Family::Name(FPS_FONT_FAMILY));
                    text_state.buffer.set_text(
                        &mut text_state.font_system,
                        &self.fps_text,
                        &attrs,
                        Shaping::Advanced,
                        Some(Align::Right),
                    );
                    text_state
                        .buffer
                        .shape_until_scroll(&mut text_state.font_system, false);
                    text_state.last_text.clear();
                    text_state.last_text.push_str(&self.fps_text);
                }
                text_state.viewport.update(
                    &self.queue,
                    Resolution {
                        width: self._config.width,
                        height: self._config.height,
                    },
                );
                let bounds = TextBounds {
                    left: 0,
                    top: 0,
                    right: self._config.width as i32,
                    bottom: self._config.height as i32,
                };
                let text_scale = self.render_scale;
                let text_width = FPS_TEXT_WIDTH * text_scale;
                let pad_x = FPS_TEXT_PADDING_X * text_scale;
                let pad_y = FPS_TEXT_PADDING_Y * text_scale;
                let left = (self._config.width as f32 - text_width - pad_x).max(0.0);
                let top = pad_y;
                let text_area = TextArea {
                    buffer: &text_state.buffer,
                    left,
                    top,
                    scale: 1.0,
                    bounds,
                    default_color: self.fps_color,
                    custom_glyphs: &[],
                };
                match text_state.renderer.prepare(
                    &self.device,
                    &self.queue,
                    &mut text_state.font_system,
                    &mut text_state.atlas,
                    &text_state.viewport,
                    [text_area],
                    &mut text_state.swash_cache,
                ) {
                    Ok(()) => {
                        self.fps_logged_prepare_error = false;
                        draw_text = true;
                    }
                    Err(err) => {
                        if !self.fps_logged_prepare_error {
                            console::warn_1(&JsValue::from_str(&format!(
                                "WGPU FPS: text prepare failed: {err:?}"
                            )));
                            self.fps_logged_prepare_error = true;
                        }
                    }
                }
            } else if !self.fps_logged_missing_text_state {
                console::warn_1(&JsValue::from_str(
                    "WGPU FPS: enabled but font not loaded yet",
                ));
                self.fps_logged_missing_text_state = true;
            }
        }
        if draw_text {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("text-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            if let Some(text_state) = self.text_state.as_ref() {
                let _ = text_state
                    .renderer
                    .render(&text_state.atlas, &text_state.viewport, &mut render_pass);
            }
        }

        self.queue.submit(Some(encoder.finish()));
        frame.present();
    }

    pub(crate) fn set_font_bytes(&mut self, font_bytes: Vec<u8>) {
        if self.text_state.is_some() {
            return;
        }
        let font_len = font_bytes.len();
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(font_bytes);
        let swash_cache = SwashCache::new();
        let cache = Cache::new(&self.device);
        let mut atlas = TextAtlas::new(&self.device, &self.queue, &cache, self._config.format);
        let viewport = Viewport::new(&self.device, &cache);
        let renderer =
            TextRenderer::new(&mut atlas, &self.device, wgpu::MultisampleState::default(), None);
        let scale = self.render_scale;
        let font_size = FPS_FONT_SIZE * scale;
        let line_height = FPS_LINE_HEIGHT * scale;
        let text_width = FPS_TEXT_WIDTH * scale;
        let mut buffer = Buffer::new(&mut font_system, Metrics::new(font_size, line_height));
        buffer.set_size(
            &mut font_system,
            Some(text_width),
            Some(self._config.height as f32),
        );
        let mut state = GlyphonState {
            font_system,
            swash_cache,
            atlas,
            renderer,
            viewport,
            buffer,
            last_text: String::new(),
        };
        if !self.fps_text.is_empty() {
            let attrs = Attrs::new().family(Family::Name(FPS_FONT_FAMILY));
            state.buffer.set_text(
                &mut state.font_system,
                &self.fps_text,
                &attrs,
                Shaping::Advanced,
                Some(Align::Right),
            );
            state
                .buffer
                .shape_until_scroll(&mut state.font_system, false);
            state.last_text.push_str(&self.fps_text);
        }
        self.text_state = Some(state);
        console::log_1(&JsValue::from_str(&format!(
            "WGPU FPS: font loaded ({} bytes)",
            font_len
        )));
    }

    pub(crate) fn set_edge_aa(&mut self, edge_aa: f32) {
        self.globals.edge_aa = edge_aa.max(0.0);
    }

    pub(crate) fn set_show_fps(&mut self, enabled: bool) {
        if self.show_fps == enabled {
            return;
        }
        self.show_fps = enabled;
        self.fps_tracker.reset();
        self.fps_logged_missing_text_state = false;
        self.fps_logged_prepare_error = false;
        if enabled {
            console::log_1(&JsValue::from_str("WGPU FPS: enabled"));
            self.fps_text = FPS_LABEL_FALLBACK.to_string();
        } else {
            console::log_1(&JsValue::from_str("WGPU FPS: disabled"));
            self.fps_text.clear();
        }
    }

    pub(crate) fn update_instances(&mut self, set: InstanceSet) {
        let needed = set.instances.len() as u32;
        self.instance_count = needed;
        if needed > self.instance_capacity {
            self.instance_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("piece-instance-buffer"),
                contents: bytemuck::cast_slice(&set.instances),
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            });
            self.instance_capacity = needed;
        } else {
            self.queue.write_buffer(
                &self.instance_buffer,
                0,
                bytemuck::cast_slice(&set.instances),
            );
        }
        self.instance_batches = set.batches;
    }

    pub(crate) fn set_emboss_enabled(&mut self, enabled: bool) {
        self.globals.emboss_strength = if enabled { EMBOSS_OPACITY } else { 0.0 };
    }
}

fn image_to_rgba(
    image: &HtmlImageElement,
) -> Result<(Vec<u8>, u32, u32), wasm_bindgen::JsValue> {
    let width = image.natural_width();
    let height = image.natural_height();
    let document = web_sys::window()
        .and_then(|window| window.document())
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no document"))?;
    let canvas = document
        .create_element("canvas")?
        .dyn_into::<HtmlCanvasElement>()?;
    canvas.set_width(width);
    canvas.set_height(height);
    let ctx = canvas
        .get_context("2d")?
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no 2d context"))?
        .dyn_into::<CanvasRenderingContext2d>()?;
    ctx.draw_image_with_html_image_element(image, 0.0, 0.0)?;
    let data = ctx.get_image_data(0.0, 0.0, width as f64, height as f64)?;
    Ok((data.data().to_vec(), width, height))
}

pub(crate) fn build_mask_atlas(
    pieces: &[Piece],
    paths: &[PiecePaths],
    piece_width: f32,
    piece_height: f32,
    grid: GridChoice,
    padding: f32,
) -> Result<MaskAtlasData, wasm_bindgen::JsValue> {
    let pad = padding.max(0.0).ceil();
    let slot_w = (piece_width + pad * 2.0).ceil() as u32;
    let slot_h = (piece_height + pad * 2.0).ceil() as u32;
    let atlas_width = slot_w * grid.cols;
    let atlas_height = slot_h * grid.rows;

    let document = web_sys::window()
        .and_then(|window| window.document())
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no document"))?;
    let canvas = document
        .create_element("canvas")?
        .dyn_into::<HtmlCanvasElement>()?;
    canvas.set_width(atlas_width);
    canvas.set_height(atlas_height);
    let ctx = canvas
        .get_context("2d")?
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no 2d context"))?
        .dyn_into::<CanvasRenderingContext2d>()?;
    ctx.set_fill_style_str("black");
    ctx.fill_rect(0.0, 0.0, atlas_width as f64, atlas_height as f64);

    let mut origins = vec![[0.0, 0.0]; pieces.len()];
    for (index, piece) in pieces.iter().enumerate() {
        let path = paths
            .get(index)
            .ok_or_else(|| wasm_bindgen::JsValue::from_str("missing piece path"))?;
        let path2d = Path2d::new_with_path_string(&path.outline)?;
        let atlas_x = piece.col * slot_w;
        let atlas_y = piece.row * slot_h;
        let offset_x = atlas_x as f64 + pad as f64;
        let offset_y = atlas_y as f64 + pad as f64;
        ctx.save();
        ctx.translate(offset_x, offset_y)?;
        ctx.set_fill_style_str("white");
        ctx.fill_with_path_2d(&path2d);
        ctx.restore();
        origins[index] = [atlas_x as f32 + pad, atlas_y as f32 + pad];
    }

    let data = ctx.get_image_data(0.0, 0.0, atlas_width as f64, atlas_height as f64)?;
    let pixels = data.data().to_vec();
    Ok(MaskAtlasData {
        width: atlas_width,
        height: atlas_height,
        pixels,
        origins,
    })
}

fn create_texture_view_from_pixels(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    pixels: &[u8],
    width: u32,
    height: u32,
    format: wgpu::TextureFormat,
    label: &str,
) -> Result<wgpu::TextureView, wasm_bindgen::JsValue> {
    let expected_len = width
        .checked_mul(height)
        .and_then(|value| value.checked_mul(4))
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("texture size overflow"))?
        as usize;
    if pixels.len() != expected_len {
        return Err(wasm_bindgen::JsValue::from_str(
            "pixel data size mismatch",
        ));
    }
    let texture = device.create_texture_with_data(
        queue,
        &wgpu::TextureDescriptor {
            label: Some(label),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format,
            usage: wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        },
        wgpu::util::TextureDataOrder::LayerMajor,
        pixels,
    );
    Ok(texture.create_view(&wgpu::TextureViewDescriptor::default()))
}
