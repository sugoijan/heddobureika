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
const OUTLINE_COLOR_DEFAULT: [f32; 4] = [1.0, 0.0, 0.0, 1.0];
const OUTLINE_COLOR_SOLVED: [f32; 4] = [0.165, 0.659, 0.29, 1.0];

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub(crate) struct Instance {
    pub(crate) pos: [f32; 2],
    pub(crate) size: [f32; 2],
    pub(crate) rotation: f32,
    pub(crate) flip: f32,
    pub(crate) hover: f32,
    pub(crate) drag: f32,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum UiTextId {
    Title,
    Progress,
    Credit,
    Success,
    Debug,
    MenuTitle,
    MenuSubtitle,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum UiRotationOrigin {
    Center,
    BottomLeft,
    BottomRight,
    TopLeft,
    TopRight,
}

#[derive(Clone, Debug)]
pub(crate) struct UiTextSpec {
    pub(crate) id: UiTextId,
    pub(crate) text: String,
    pub(crate) pos: [f32; 2],
    pub(crate) rotation_deg: f32,
    pub(crate) rotation_origin: UiRotationOrigin,
    pub(crate) rotation_offset: [f32; 2],
    pub(crate) font_size: f32,
    pub(crate) line_height: f32,
    pub(crate) color: [u8; 4],
}

pub(crate) const UI_ICON_ATLAS_CELL_PX: u32 = 64;
pub(crate) const UI_ICON_ATLAS_COLS: u32 = 2;
pub(crate) const UI_ICON_ATLAS_ROWS: u32 = 2;
pub(crate) const UI_ICON_ATLAS_WIDTH: u32 = UI_ICON_ATLAS_CELL_PX * UI_ICON_ATLAS_COLS;
pub(crate) const UI_ICON_ATLAS_HEIGHT: u32 = UI_ICON_ATLAS_CELL_PX * UI_ICON_ATLAS_ROWS;
const UI_PREVIEW_BLUR_MAX_DIM: u32 = 192;
const UI_PREVIEW_BLUR_FILTER_PX: f64 = 6.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum UiSpriteTexture {
    Preview,
    PreviewBlur,
    IconAtlas,
    SolidWhite,
}

#[derive(Clone, Debug)]
pub(crate) struct UiSpriteSpec {
    pub(crate) texture: UiSpriteTexture,
    pub(crate) pos: [f32; 2],
    pub(crate) size: [f32; 2],
    pub(crate) rotation_deg: f32,
    pub(crate) opacity: f32,
    pub(crate) uv_min: [f32; 2],
    pub(crate) uv_max: [f32; 2],
    pub(crate) color: [f32; 4],
    pub(crate) radius: f32,
    pub(crate) blur_uv: [f32; 2],
    pub(crate) desaturate: f32,
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
                    format: wgpu::VertexFormat::Float32,
                    offset: 28,
                    shader_location: 6,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 32,
                    shader_location: 7,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 40,
                    shader_location: 8,
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

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct UiInstance {
    pos: [f32; 2],
    size: [f32; 2],
    pivot: [f32; 2],
    rotation: f32,
    opacity: f32,
    uv_min: [f32; 2],
    uv_max: [f32; 2],
    color: [f32; 4],
    radius_blur: [f32; 4],
}

impl UiInstance {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<UiInstance>() as wgpu::BufferAddress,
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
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 16,
                    shader_location: 3,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 24,
                    shader_location: 4,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32,
                    offset: 28,
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
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 48,
                    shader_location: 8,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 64,
                    shader_location: 9,
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
    puzzle_scale: f32,
    _pad: [f32; 1],
    outline_color: [f32; 4],
}

#[repr(C, align(16))]
#[derive(Copy, Clone, Pod, Zeroable)]
struct FrameGlobals {
    view_min: [f32; 2],
    view_size: [f32; 2],
    color: [f32; 4],
    output_gamma: f32,
    puzzle_scale: f32,
    _pad: [f32; 2],
}

#[repr(C, align(16))]
#[derive(Copy, Clone, Pod, Zeroable)]
struct UiGlobals {
    view_min: [f32; 2],
    view_size: [f32; 2],
    viewport_px: [f32; 2],
    output_gamma: f32,
    _pad: f32,
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
const UI_FONT_FAMILY: &str = "KaoriGel";
const UI_TEXT_PADDING: f32 = 8.0;

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

struct UiTextState {
    font_system: FontSystem,
    swash_cache: SwashCache,
    atlas: TextAtlas,
    renderer: TextRenderer,
    viewport: Viewport,
}

struct UiSprite {
    id: UiTextId,
    text: String,
    font_size: f32,
    line_height: f32,
    color: [u8; 4],
    size_px: [u32; 2],
    buffer: Buffer,
    texture: wgpu::Texture,
    view: wgpu::TextureView,
    bind_group: wgpu::BindGroup,
    instance: UiInstance,
    instance_buffer: wgpu::Buffer,
}

struct UiOverlaySprite {
    texture: UiSpriteTexture,
    bind_group: Rc<wgpu::BindGroup>,
    instance: UiInstance,
    instance_buffer: wgpu::Buffer,
}

struct IconAtlas {
    _texture: wgpu::Texture,
    _view: wgpu::TextureView,
    bind_group: Rc<wgpu::BindGroup>,
}

fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
}

pub(crate) struct WgpuRenderer {
    _canvas: HtmlCanvasElement,
    backend: wgpu::Backend,
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
    workspace_min: [f32; 2],
    workspace_size: [f32; 2],
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
    ui_pipeline: wgpu::RenderPipeline,
    _ui_globals_buffer: wgpu::Buffer,
    ui_globals_bind_group: wgpu::BindGroup,
    ui_texture_bind_group_layout: wgpu::BindGroupLayout,
    ui_sampler: wgpu::Sampler,
    ui_sprites: Vec<UiSprite>,
    ui_text_state: Option<UiTextState>,
    ui_overlay_sprites: Vec<UiOverlaySprite>,
    ui_white_bind_group: Rc<wgpu::BindGroup>,
    ui_preview_bind_group: Option<Rc<wgpu::BindGroup>>,
    ui_preview_blur_bind_group: Option<Rc<wgpu::BindGroup>>,
    ui_icon_atlas: Option<IconAtlas>,
    show_frame: bool,
    frame_clear_color: wgpu::Color,
    frame_dash_color: [f32; 4],
    workspace_fill_color: [f32; 4],
    workspace_stroke_color: [f32; 4],
    text_state: Option<GlyphonState>,
    show_fps: bool,
    fps_tracker: FpsTracker,
    fps_text: String,
    fps_color: Color,
    fps_force_fallback: bool,
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
        workspace_min_x: f32,
        workspace_min_y: f32,
        workspace_width: f32,
        workspace_height: f32,
        puzzle_scale: f32,
        mask_atlas: Rc<MaskAtlasData>,
        mask_pad: f32,
        render_scale: f32,
        viewport_width: f32,
        viewport_height: f32,
        is_dark_theme: bool,
    ) -> Result<Self, wasm_bindgen::JsValue> {
        let (art_pixels, art_width, art_height) = image_to_rgba(&image)?;
        let logical_width = piece_width * grid.cols as f32;
        let logical_height = piece_height * grid.rows as f32;
        let puzzle_scale = puzzle_scale.max(1.0e-4);
        let inv_puzzle_scale = 1.0 / puzzle_scale;
        let workspace_min_x = workspace_min_x * inv_puzzle_scale;
        let workspace_min_y = workspace_min_y * inv_puzzle_scale;
        let workspace_width = workspace_width * inv_puzzle_scale;
        let workspace_height = workspace_height * inv_puzzle_scale;

        let css_width = viewport_width.max(1.0);
        let css_height = viewport_height.max(1.0);
        let dpr = web_sys::window()
            .map(|window| window.device_pixel_ratio())
            .unwrap_or(1.0) as f32;
        let render_scale = render_scale.max(WGPU_RENDER_SCALE_MIN) * dpr;
        let canvas_width = (css_width * render_scale).max(1.0).ceil() as u32;
        let canvas_height = (css_height * render_scale).max(1.0).ceil() as u32;
        canvas.set_width(canvas_width);
        canvas.set_height(canvas_height);

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
        let backend = adapter.get_info().backend;
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
            puzzle_scale,
            _pad: [0.0; 1],
            outline_color: OUTLINE_COLOR_DEFAULT,
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
        let (blur_pixels, blur_width, blur_height) =
            image_to_rgba_scaled(&image, UI_PREVIEW_BLUR_MAX_DIM, UI_PREVIEW_BLUR_FILTER_PX)?;
        let preview_blur_view = create_texture_view_from_pixels(
            &device,
            &queue,
            &blur_pixels,
            blur_width,
            blur_height,
            wgpu::TextureFormat::Rgba8UnormSrgb,
            "preview-blur-texture",
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
            puzzle_scale,
            _pad: [0.0; 2],
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
            puzzle_scale: 1.0,
            _pad: [0.0; 2],
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
            puzzle_scale: 1.0,
            _pad: [0.0; 2],
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

        let ui_globals = UiGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            viewport_px: [config.width as f32, config.height as f32],
            output_gamma,
            _pad: 0.0,
        };
        let ui_globals_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("ui-globals-buffer"),
            contents: bytemuck::bytes_of(&ui_globals),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        let ui_globals_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("ui-globals-bind-group-layout"),
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
        let ui_globals_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("ui-globals-bind-group"),
            layout: &ui_globals_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: ui_globals_buffer.as_entire_binding(),
            }],
        });
        let ui_texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("ui-texture-bind-group-layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });
        let ui_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("ui-sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });
        let white_view = create_texture_view_from_pixels(
            &device,
            &queue,
            &[255, 255, 255, 255],
            1,
            1,
            config.format,
            "ui-white",
        )?;
        let ui_white_bind_group = Rc::new(device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("ui-white-bind-group"),
            layout: &ui_texture_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&white_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&ui_sampler),
                },
            ],
        }));
        let ui_icon_atlas =
            build_icon_atlas(&device, &queue, config.format, &ui_texture_bind_group_layout, &ui_sampler)?;
        let ui_preview_bind_group = Rc::new(device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("ui-preview-bind-group"),
            layout: &ui_texture_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&art_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&ui_sampler),
                },
            ],
        }));
        let ui_preview_blur_bind_group =
            Rc::new(device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("ui-preview-blur-bind-group"),
                layout: &ui_texture_bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(&preview_blur_view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&ui_sampler),
                    },
                ],
            }));
        let ui_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("ui-shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("wgpu_ui.wgsl").into()),
        });
        let ui_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("ui-pipeline-layout"),
            bind_group_layouts: &[&ui_globals_bind_group_layout, &ui_texture_bind_group_layout],
            immediate_size: 0,
        });
        let ui_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("ui-pipeline"),
            layout: Some(&ui_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &ui_shader,
                entry_point: Some("vs_main"),
                buffers: &[Vertex::layout(), UiInstance::layout()],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &ui_shader,
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
            drag: 0.0,
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
        let workspace_stroke_width = 2.0 * inv_puzzle_scale;
        let workspace_center = [
            workspace_min_x + workspace_width * 0.5,
            workspace_min_y + workspace_height * 0.5,
        ];
        let frame_bg_instances = vec![FrameInstance {
            pos: workspace_center,
            size: [workspace_width, workspace_height],
            rotation: 0.0,
            _pad: 0.0,
        }];
        let frame_bg_instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("frame-bg-instance-buffer"),
            contents: bytemuck::cast_slice(&frame_bg_instances),
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        });
        let half_stroke = workspace_stroke_width * 0.5;
        let left_x = workspace_min_x + half_stroke;
        let right_x = workspace_min_x + workspace_width - half_stroke;
        let top_y = workspace_min_y + half_stroke;
        let bottom_y = workspace_min_y + workspace_height - half_stroke;
        let frame_stroke_instances = vec![
            FrameInstance {
                pos: [workspace_center[0], top_y],
                size: [workspace_width, workspace_stroke_width],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [workspace_center[0], bottom_y],
                size: [workspace_width, workspace_stroke_width],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [left_x, workspace_center[1]],
                size: [workspace_stroke_width, workspace_height],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [right_x, workspace_center[1]],
                size: [workspace_stroke_width, workspace_height],
                rotation: 0.0,
                _pad: 0.0,
            },
        ];
        let frame_stroke_instance_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("frame-stroke-instance-buffer"),
                contents: bytemuck::cast_slice(&frame_stroke_instances),
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
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
            backend,
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
            workspace_min: [workspace_min_x, workspace_min_y],
            workspace_size: [workspace_width, workspace_height],
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
            ui_pipeline,
            _ui_globals_buffer: ui_globals_buffer,
            ui_globals_bind_group,
            ui_texture_bind_group_layout,
            ui_sampler,
            ui_sprites: Vec::new(),
            ui_text_state: None,
            ui_overlay_sprites: Vec::new(),
            ui_white_bind_group,
            ui_preview_bind_group: Some(ui_preview_bind_group),
            ui_preview_blur_bind_group: Some(ui_preview_blur_bind_group),
            ui_icon_atlas: Some(ui_icon_atlas),
            show_frame: true,
            frame_clear_color,
            frame_dash_color: dash_color,
            workspace_fill_color: workspace_fill,
            workspace_stroke_color: workspace_stroke,
            text_state: None,
            show_fps: false,
            fps_tracker: FpsTracker::new(),
            fps_text: String::new(),
            fps_color,
            fps_force_fallback: false,
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
                label: Some("frame-bg-pass"),
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
        if !self.ui_sprites.is_empty() {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("ui-pass"),
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
            render_pass.set_pipeline(&self.ui_pipeline);
            render_pass.set_bind_group(0, &self.ui_globals_bind_group, &[]);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass
                .set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
            for sprite in &self.ui_sprites {
                if sprite.instance.opacity <= f32::EPSILON {
                    continue;
                }
                render_pass.set_bind_group(1, &sprite.bind_group, &[]);
                render_pass.set_vertex_buffer(1, sprite.instance_buffer.slice(..));
                render_pass.draw_indexed(0..self.index_count, 0, 0..1);
            }
        }

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("frame-lines-pass"),
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
            render_pass.set_pipeline(&self.frame_pipeline);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass
                .set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
            if self.frame_stroke_instance_count > 0 {
                render_pass.set_bind_group(0, &self.frame_stroke_bind_group, &[]);
                render_pass
                    .set_vertex_buffer(1, self.frame_stroke_instance_buffer.slice(..));
                render_pass
                    .draw_indexed(0..self.index_count, 0, 0..self.frame_stroke_instance_count);
            }
            if self.show_frame && self.frame_instance_count > 0 {
                render_pass.set_bind_group(0, &self.frame_bind_group, &[]);
                render_pass.set_vertex_buffer(1, self.frame_instance_buffer.slice(..));
                render_pass.draw_indexed(0..self.index_count, 0, 0..self.frame_instance_count);
            }
        }

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

        if !self.ui_overlay_sprites.is_empty() {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("ui-overlay-pass"),
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
            render_pass.set_pipeline(&self.ui_pipeline);
            render_pass.set_bind_group(0, &self.ui_globals_bind_group, &[]);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass
                .set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
            for sprite in &self.ui_overlay_sprites {
                if sprite.instance.opacity <= f32::EPSILON {
                    continue;
                }
                render_pass.set_bind_group(1, sprite.bind_group.as_ref(), &[]);
                render_pass.set_vertex_buffer(1, sprite.instance_buffer.slice(..));
                render_pass.draw_indexed(0..self.index_count, 0, 0..1);
            }
        }

        let mut draw_text = false;
        if self.show_fps {
            if self.fps_force_fallback {
                self.fps_force_fallback = false;
                self.fps_text = FPS_LABEL_FALLBACK.to_string();
                self.fps_tracker.reset();
            } else if let Some(fps) = self.fps_tracker.record_frame(now_ms()) {
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

    pub(crate) fn set_ui_font_bytes(&mut self, font_bytes: Vec<u8>) {
        if self.ui_text_state.is_some() {
            return;
        }
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(font_bytes);
        let swash_cache = SwashCache::new();
        let cache = Cache::new(&self.device);
        let mut atlas = TextAtlas::new(&self.device, &self.queue, &cache, self._config.format);
        let viewport = Viewport::new(&self.device, &cache);
        let renderer =
            TextRenderer::new(&mut atlas, &self.device, wgpu::MultisampleState::default(), None);
        self.ui_text_state = Some(UiTextState {
            font_system,
            swash_cache,
            atlas,
            renderer,
            viewport,
        });
    }

    pub(crate) fn set_ui_texts(&mut self, specs: &[UiTextSpec]) {
        let Some(ui_state) = self.ui_text_state.as_mut() else {
            return;
        };
        let mut keep_ids = Vec::with_capacity(specs.len());
        for spec in specs {
            keep_ids.push(spec.id);
        }
        self.ui_sprites.retain(|sprite| keep_ids.contains(&sprite.id));
        for spec in specs {
            let needs_new = self.ui_sprites.iter().all(|sprite| sprite.id != spec.id);
            if needs_new {
                let font_px = spec.font_size * self.render_scale;
                let line_px = spec.line_height * self.render_scale;
                let buffer = Buffer::new(&mut ui_state.font_system, Metrics::new(font_px, line_px));
                let texture = self.device.create_texture(&wgpu::TextureDescriptor {
                    label: Some("ui-texture"),
                    size: wgpu::Extent3d {
                        width: 1,
                        height: 1,
                        depth_or_array_layers: 1,
                    },
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: self._config.format,
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT
                        | wgpu::TextureUsages::TEXTURE_BINDING,
                    view_formats: &[],
                });
                let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
                let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some("ui-texture-bind-group"),
                    layout: &self.ui_texture_bind_group_layout,
                    entries: &[
                        wgpu::BindGroupEntry {
                            binding: 0,
                            resource: wgpu::BindingResource::TextureView(&view),
                        },
                        wgpu::BindGroupEntry {
                            binding: 1,
                            resource: wgpu::BindingResource::Sampler(&self.ui_sampler),
                        },
                    ],
                });
                let instance = UiInstance {
                    pos: spec.pos,
                    size: [1.0, 1.0],
                    pivot: spec.rotation_offset,
                    rotation: spec.rotation_deg,
                    opacity: 1.0,
                    uv_min: [0.0, 0.0],
                    uv_max: [1.0, 1.0],
                    color: [1.0, 1.0, 1.0, 1.0],
                    radius_blur: [0.0, 0.0, 0.0, 0.0],
                };
                let instance_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("ui-instance-buffer"),
                    contents: bytemuck::cast_slice(&[instance]),
                    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                });
                self.ui_sprites.push(UiSprite {
                    id: spec.id,
                    text: String::new(),
                    font_size: 0.0,
                    line_height: 0.0,
                    color: [0, 0, 0, 0],
                    size_px: [1, 1],
                    buffer,
                    texture,
                    view,
                    bind_group,
                    instance,
                    instance_buffer,
                });
            }
            let index = self
                .ui_sprites
                .iter()
                .position(|sprite| sprite.id == spec.id);
            let Some(index) = index else {
                continue;
            };
            let sprite = &mut self.ui_sprites[index];
            let needs_texture = sprite.text != spec.text
                || (sprite.font_size - spec.font_size).abs() > f32::EPSILON
                || (sprite.line_height - spec.line_height).abs() > f32::EPSILON
                || sprite.color != spec.color;
            if needs_texture {
                let font_px = spec.font_size * self.render_scale;
                let line_px = spec.line_height * self.render_scale;
                let metrics = Metrics::new(font_px, line_px);
                let pad_px = UI_TEXT_PADDING * self.render_scale;
                sprite.buffer = Buffer::new(&mut ui_state.font_system, metrics);
                sprite
                    .buffer
                    .set_size(&mut ui_state.font_system, None, None);
                let attrs = Attrs::new().family(Family::Name(UI_FONT_FAMILY));
                sprite.buffer.set_text(
                    &mut ui_state.font_system,
                    &spec.text,
                    &attrs,
                    Shaping::Advanced,
                    Some(Align::Left),
                );
                sprite
                    .buffer
                    .shape_until_scroll(&mut ui_state.font_system, false);
                let (text_w, text_h) = measure_text_bounds(&sprite.buffer);
                let tex_w = (text_w + pad_px * 2.0).ceil().max(1.0) as u32;
                let tex_h = (text_h + pad_px * 2.0).ceil().max(1.0) as u32;
                sprite
                    .buffer
                    .set_size(&mut ui_state.font_system, Some(tex_w as f32), None);
                sprite.buffer.set_text(
                    &mut ui_state.font_system,
                    &spec.text,
                    &attrs,
                    Shaping::Advanced,
                    Some(Align::Center),
                );
                sprite
                    .buffer
                    .shape_until_scroll(&mut ui_state.font_system, false);
                if tex_w != sprite.size_px[0] || tex_h != sprite.size_px[1] {
                    sprite.texture = self.device.create_texture(&wgpu::TextureDescriptor {
                        label: Some("ui-texture"),
                        size: wgpu::Extent3d {
                            width: tex_w,
                            height: tex_h,
                            depth_or_array_layers: 1,
                        },
                        mip_level_count: 1,
                        sample_count: 1,
                        dimension: wgpu::TextureDimension::D2,
                        format: self._config.format,
                        usage: wgpu::TextureUsages::RENDER_ATTACHMENT
                            | wgpu::TextureUsages::TEXTURE_BINDING,
                        view_formats: &[],
                    });
                    sprite.view = sprite.texture.create_view(&wgpu::TextureViewDescriptor::default());
                    sprite.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                        label: Some("ui-texture-bind-group"),
                        layout: &self.ui_texture_bind_group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::TextureView(&sprite.view),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Sampler(&self.ui_sampler),
                            },
                        ],
                    });
                    sprite.size_px = [tex_w, tex_h];
                }
                sprite.instance.size = [
                    tex_w as f32 / self.render_scale,
                    tex_h as f32 / self.render_scale,
                ];
                let color = Color::rgba(spec.color[0], spec.color[1], spec.color[2], spec.color[3]);
                ui_state.viewport.update(
                    &self.queue,
                    Resolution {
                        width: tex_w,
                        height: tex_h,
                    },
                );
                let bounds = TextBounds {
                    left: 0,
                    top: 0,
                    right: tex_w as i32,
                    bottom: tex_h as i32,
                };
                let text_area = TextArea {
                    buffer: &sprite.buffer,
                    left: 0.0,
                    top: pad_px,
                    scale: 1.0,
                    bounds,
                    default_color: color,
                    custom_glyphs: &[],
                };
                if ui_state
                    .renderer
                    .prepare(
                        &self.device,
                        &self.queue,
                        &mut ui_state.font_system,
                        &mut ui_state.atlas,
                        &ui_state.viewport,
                        [text_area],
                        &mut ui_state.swash_cache,
                    )
                    .is_ok()
                {
                    let mut encoder = self
                        .device
                        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                            label: Some("ui-text-encoder"),
                        });
                    {
                        let mut render_pass =
                            encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                                label: Some("ui-text-pass"),
                                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                                    view: &sprite.view,
                                    depth_slice: None,
                                    resolve_target: None,
                                    ops: wgpu::Operations {
                                        load: wgpu::LoadOp::Clear(wgpu::Color {
                                            r: 0.0,
                                            g: 0.0,
                                            b: 0.0,
                                            a: 0.0,
                                        }),
                                        store: wgpu::StoreOp::Store,
                                    },
                                })],
                                depth_stencil_attachment: None,
                                timestamp_writes: None,
                                occlusion_query_set: None,
                                multiview_mask: None,
                            });
                        let _ = ui_state
                            .renderer
                            .render(&ui_state.atlas, &ui_state.viewport, &mut render_pass);
                    }
                    self.queue.submit(Some(encoder.finish()));
                    sprite.text = spec.text.clone();
                    sprite.font_size = spec.font_size;
                    sprite.line_height = spec.line_height;
                    sprite.color = spec.color;
                }
            }
            sprite.instance.pos = spec.pos;
            sprite.instance.pivot = spec.rotation_offset;
            sprite.instance.rotation = spec.rotation_deg;
            sprite.instance.opacity = 1.0;
            sprite.instance.uv_min = [0.0, 0.0];
            sprite.instance.uv_max = [1.0, 1.0];
            sprite.instance.color = [1.0, 1.0, 1.0, 1.0];
            sprite.instance.radius_blur = [0.0, 0.0, 0.0, 0.0];
            self.queue.write_buffer(
                &sprite.instance_buffer,
                0,
                bytemuck::cast_slice(&[sprite.instance]),
            );
        }
    }

    pub(crate) fn set_ui_overlay_sprites(&mut self, specs: &[UiSpriteSpec]) {
        let mut filtered: Vec<(UiSpriteSpec, Rc<wgpu::BindGroup>)> = Vec::new();
        for spec in specs {
            let bind_group = match spec.texture {
                UiSpriteTexture::Preview => match self.ui_preview_bind_group.as_ref() {
                    Some(bind_group) => bind_group.clone(),
                    None => continue,
                },
                UiSpriteTexture::PreviewBlur => match self.ui_preview_blur_bind_group.as_ref() {
                    Some(bind_group) => bind_group.clone(),
                    None => continue,
                },
                UiSpriteTexture::IconAtlas => match self.ui_icon_atlas.as_ref() {
                    Some(atlas) => atlas.bind_group.clone(),
                    None => continue,
                },
                UiSpriteTexture::SolidWhite => self.ui_white_bind_group.clone(),
            };
            filtered.push((spec.clone(), bind_group));
        }
        if filtered.is_empty() {
            self.ui_overlay_sprites.clear();
            return;
        }
        if self.ui_overlay_sprites.len() != filtered.len() {
            self.ui_overlay_sprites.clear();
            for (spec, bind_group) in &filtered {
                let instance = UiInstance {
                    pos: spec.pos,
                    size: spec.size,
                    pivot: [0.0, 0.0],
                    rotation: spec.rotation_deg,
                    opacity: spec.opacity,
                    uv_min: spec.uv_min,
                    uv_max: spec.uv_max,
                    color: spec.color,
                    radius_blur: [
                        spec.radius,
                        spec.blur_uv[0],
                        spec.blur_uv[1],
                        spec.desaturate,
                    ],
                };
                let instance_buffer = self
                    .device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("ui-overlay-instance-buffer"),
                        contents: bytemuck::cast_slice(&[instance]),
                        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                    });
                self.ui_overlay_sprites.push(UiOverlaySprite {
                    texture: spec.texture,
                    bind_group: bind_group.clone(),
                    instance,
                    instance_buffer,
                });
            }
        }
        for (sprite, (spec, bind_group)) in
            self.ui_overlay_sprites.iter_mut().zip(filtered.iter())
        {
            if sprite.texture != spec.texture {
                sprite.texture = spec.texture;
                sprite.bind_group = bind_group.clone();
            }
            sprite.instance = UiInstance {
                pos: spec.pos,
                size: spec.size,
                pivot: [0.0, 0.0],
                rotation: spec.rotation_deg,
                opacity: spec.opacity,
                uv_min: spec.uv_min,
                uv_max: spec.uv_max,
                color: spec.color,
                radius_blur: [
                    spec.radius,
                    spec.blur_uv[0],
                    spec.blur_uv[1],
                    spec.desaturate,
                ],
            };
            self.queue.write_buffer(
                &sprite.instance_buffer,
                0,
                bytemuck::cast_slice(&[sprite.instance]),
            );
        }
    }

    pub(crate) fn resize(&mut self, pixel_width: u32, pixel_height: u32) {
        let width = pixel_width.max(1);
        let height = pixel_height.max(1);
        if self._config.width == width && self._config.height == height {
            return;
        }
        self._config.width = width;
        self._config.height = height;
        self._canvas.set_width(width);
        self._canvas.set_height(height);
        self.surface.configure(&self.device, &self._config);
        let ui_globals = UiGlobals {
            view_min: self.globals.view_min,
            view_size: self.globals.view_size,
            viewport_px: [self._config.width as f32, self._config.height as f32],
            output_gamma: self.globals.output_gamma,
            _pad: 0.0,
        };
        self.queue
            .write_buffer(&self._ui_globals_buffer, 0, bytemuck::bytes_of(&ui_globals));
    }

    pub(crate) fn set_view(
        &mut self,
        view_min_x: f32,
        view_min_y: f32,
        view_width: f32,
        view_height: f32,
        puzzle_scale: f32,
    ) {
        let puzzle_scale = puzzle_scale.max(1.0e-4);
        if self.globals.view_min == [view_min_x, view_min_y]
            && self.globals.view_size == [view_width, view_height]
            && (self.globals.puzzle_scale - puzzle_scale).abs() <= f32::EPSILON
        {
            return;
        }
        self.globals.view_min = [view_min_x, view_min_y];
        self.globals.view_size = [view_width, view_height];
        self.globals.puzzle_scale = puzzle_scale;
        self.queue.write_buffer(
            &self.globals_buffer_fill,
            0,
            bytemuck::bytes_of(&self.globals),
        );
        let mut globals_outline = self.globals;
        globals_outline.render_mode = 1.0;
        self.queue.write_buffer(
            &self.globals_buffer_outline,
            0,
            bytemuck::bytes_of(&globals_outline),
        );
        let frame_dash_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: self.frame_dash_color,
            output_gamma: self.globals.output_gamma,
            puzzle_scale,
            _pad: [0.0; 2],
        };
        self.queue.write_buffer(
            &self._frame_globals_buffer,
            0,
            bytemuck::bytes_of(&frame_dash_globals),
        );
        let frame_bg_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: self.workspace_fill_color,
            output_gamma: self.globals.output_gamma,
            puzzle_scale,
            _pad: [0.0; 2],
        };
        self.queue.write_buffer(
            &self._frame_bg_globals_buffer,
            0,
            bytemuck::bytes_of(&frame_bg_globals),
        );
        let frame_stroke_globals = FrameGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            color: self.workspace_stroke_color,
            output_gamma: self.globals.output_gamma,
            puzzle_scale,
            _pad: [0.0; 2],
        };
        self.queue.write_buffer(
            &self._frame_stroke_globals_buffer,
            0,
            bytemuck::bytes_of(&frame_stroke_globals),
        );
        let ui_globals = UiGlobals {
            view_min: [view_min_x, view_min_y],
            view_size: [view_width, view_height],
            viewport_px: [self._config.width as f32, self._config.height as f32],
            output_gamma: self.globals.output_gamma,
            _pad: 0.0,
        };
        self.queue
            .write_buffer(&self._ui_globals_buffer, 0, bytemuck::bytes_of(&ui_globals));
    }

    pub(crate) fn set_workspace_rect(
        &mut self,
        min_x: f32,
        min_y: f32,
        width: f32,
        height: f32,
    ) {
        let puzzle_scale = self.globals.puzzle_scale.max(1.0e-4);
        let inv_puzzle_scale = 1.0 / puzzle_scale;
        let min_x = min_x * inv_puzzle_scale;
        let min_y = min_y * inv_puzzle_scale;
        let width = width * inv_puzzle_scale;
        let height = height * inv_puzzle_scale;
        if self.workspace_min == [min_x, min_y] && self.workspace_size == [width, height] {
            return;
        }
        self.workspace_min = [min_x, min_y];
        self.workspace_size = [width, height];
        let workspace_stroke_width = 2.0 * inv_puzzle_scale;
        let workspace_center = [min_x + width * 0.5, min_y + height * 0.5];
        let frame_bg_instances = [FrameInstance {
            pos: workspace_center,
            size: [width, height],
            rotation: 0.0,
            _pad: 0.0,
        }];
        self.queue.write_buffer(
            &self.frame_bg_instance_buffer,
            0,
            bytemuck::cast_slice(&frame_bg_instances),
        );
        self.frame_bg_instance_count = frame_bg_instances.len() as u32;
        let half_stroke = workspace_stroke_width * 0.5;
        let left_x = min_x + half_stroke;
        let right_x = min_x + width - half_stroke;
        let top_y = min_y + half_stroke;
        let bottom_y = min_y + height - half_stroke;
        let frame_stroke_instances = [
            FrameInstance {
                pos: [workspace_center[0], top_y],
                size: [width, workspace_stroke_width],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [workspace_center[0], bottom_y],
                size: [width, workspace_stroke_width],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [left_x, workspace_center[1]],
                size: [workspace_stroke_width, height],
                rotation: 0.0,
                _pad: 0.0,
            },
            FrameInstance {
                pos: [right_x, workspace_center[1]],
                size: [workspace_stroke_width, height],
                rotation: 0.0,
                _pad: 0.0,
            },
        ];
        self.queue.write_buffer(
            &self.frame_stroke_instance_buffer,
            0,
            bytemuck::cast_slice(&frame_stroke_instances),
        );
        self.frame_stroke_instance_count = frame_stroke_instances.len() as u32;
    }

    pub(crate) fn set_edge_aa(&mut self, edge_aa: f32) {
        self.globals.edge_aa = edge_aa.max(0.0);
    }

    pub(crate) fn set_solved(&mut self, solved: bool) {
        let target = if solved {
            OUTLINE_COLOR_SOLVED
        } else {
            OUTLINE_COLOR_DEFAULT
        };
        if self.globals.outline_color != target {
            self.globals.outline_color = target;
        }
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

    pub(crate) fn backend_label(&self) -> &'static str {
        match self.backend {
            wgpu::Backend::BrowserWebGpu => "WebGPU",
            wgpu::Backend::Gl => "WebGL2",
            wgpu::Backend::Vulkan => "Vulkan",
            wgpu::Backend::Metal => "Metal",
            wgpu::Backend::Dx12 => "D3D12",
            wgpu::Backend::Noop => "Noop",
        }
    }

    pub(crate) fn force_fps_fallback(&mut self) {
        if !self.show_fps {
            return;
        }
        self.fps_force_fallback = true;
        self.fps_text = FPS_LABEL_FALLBACK.to_string();
        self.fps_tracker.reset();
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

fn measure_text_bounds(buffer: &Buffer) -> (f32, f32) {
    let mut max_width = 0.0;
    let mut max_height = 0.0;
    for run in buffer.layout_runs() {
        if run.line_w > max_width {
            max_width = run.line_w;
        }
        let bottom = run.line_top + run.line_height;
        if bottom > max_height {
            max_height = bottom;
        }
    }
    (max_width, max_height)
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
    ctx.draw_image_with_html_image_element(image, 0.0, 0.0)
        .map_err(|_| wasm_bindgen::JsValue::from_str("image_draw_failed"))?;
    let data = ctx
        .get_image_data(0.0, 0.0, width as f64, height as f64)
        .map_err(|_| wasm_bindgen::JsValue::from_str("image_read_failed"))?;
    Ok((data.data().to_vec(), width, height))
}

fn image_to_rgba_scaled(
    image: &HtmlImageElement,
    max_dim: u32,
    blur_px: f64,
) -> Result<(Vec<u8>, u32, u32), wasm_bindgen::JsValue> {
    let width = image.natural_width().max(1);
    let height = image.natural_height().max(1);
    let max_src = width.max(height);
    let scale = if max_src > max_dim {
        max_dim as f64 / max_src as f64
    } else {
        1.0
    };
    let target_w = ((width as f64) * scale).round().max(1.0) as u32;
    let target_h = ((height as f64) * scale).round().max(1.0) as u32;
    let document = web_sys::window()
        .and_then(|window| window.document())
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no document"))?;
    let canvas = document
        .create_element("canvas")?
        .dyn_into::<HtmlCanvasElement>()?;
    canvas.set_width(target_w);
    canvas.set_height(target_h);
    let ctx = canvas
        .get_context("2d")?
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no 2d context"))?
        .dyn_into::<CanvasRenderingContext2d>()?;
    ctx.draw_image_with_html_image_element_and_dw_and_dh(
        image,
        0.0,
        0.0,
        target_w as f64,
        target_h as f64,
    )
    .map_err(|_| wasm_bindgen::JsValue::from_str("image_draw_failed"))?;
    let data = ctx
        .get_image_data(0.0, 0.0, target_w as f64, target_h as f64)
        .map_err(|_| wasm_bindgen::JsValue::from_str("image_read_failed"))?;
    let mut pixels = data.data().to_vec();
    if blur_px > 0.0 {
        // TODO: consider moving preview blur to the GPU to avoid CPU cost.
        let sigma = blur_px as f32;
        let radius = (sigma * 3.0).ceil().max(1.0) as usize;
        gaussian_blur_rgba(&mut pixels, target_w, target_h, radius, sigma);
    }
    Ok((pixels, target_w, target_h))
}

fn gaussian_blur_rgba(pixels: &mut [u8], width: u32, height: u32, radius: usize, sigma: f32) {
    if radius == 0 || width == 0 || height == 0 || !sigma.is_finite() || sigma <= 0.0 {
        return;
    }
    let w = width as i32;
    let h = height as i32;
    let r = radius as i32;
    let stride = (width * 4) as usize;
    let mut kernel = Vec::with_capacity((radius * 2 + 1) as usize);
    let mut sum = 0.0f32;
    for i in -r..=r {
        let x = i as f32;
        let v = (-0.5 * (x * x) / (sigma * sigma)).exp();
        kernel.push(v);
        sum += v;
    }
    if sum > 0.0 {
        for v in &mut kernel {
            *v /= sum;
        }
    }

    let mut temp = vec![0f32; pixels.len()];
    for y in 0..h {
        let row = (y as usize) * stride;
        for x in 0..w {
            let mut acc = [0.0f32; 4];
            for k in -r..=r {
                let sx = (x + k).clamp(0, w - 1) as usize;
                let idx = row + sx * 4;
                let weight = kernel[(k + r) as usize];
                acc[0] += pixels[idx] as f32 * weight;
                acc[1] += pixels[idx + 1] as f32 * weight;
                acc[2] += pixels[idx + 2] as f32 * weight;
                acc[3] += pixels[idx + 3] as f32 * weight;
            }
            let out_idx = row + (x as usize) * 4;
            temp[out_idx] = acc[0];
            temp[out_idx + 1] = acc[1];
            temp[out_idx + 2] = acc[2];
            temp[out_idx + 3] = acc[3];
        }
    }

    for y in 0..h {
        let row = (y as usize) * stride;
        for x in 0..w {
            let mut acc = [0.0f32; 4];
            for k in -r..=r {
                let sy = (y + k).clamp(0, h - 1) as usize;
                let idx = sy * stride + (x as usize) * 4;
                let weight = kernel[(k + r) as usize];
                acc[0] += temp[idx] * weight;
                acc[1] += temp[idx + 1] * weight;
                acc[2] += temp[idx + 2] * weight;
                acc[3] += temp[idx + 3] * weight;
            }
            let out_idx = row + (x as usize) * 4;
            pixels[out_idx] = acc[0].round().clamp(0.0, 255.0) as u8;
            pixels[out_idx + 1] = acc[1].round().clamp(0.0, 255.0) as u8;
            pixels[out_idx + 2] = acc[2].round().clamp(0.0, 255.0) as u8;
            pixels[out_idx + 3] = acc[3].round().clamp(0.0, 255.0) as u8;
        }
    }
}

fn build_icon_atlas(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    format: wgpu::TextureFormat,
    layout: &wgpu::BindGroupLayout,
    sampler: &wgpu::Sampler,
) -> Result<IconAtlas, wasm_bindgen::JsValue> {
    let document = web_sys::window()
        .and_then(|window| window.document())
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no document"))?;
    let canvas = document
        .create_element("canvas")?
        .dyn_into::<HtmlCanvasElement>()?;
    canvas.set_width(UI_ICON_ATLAS_WIDTH);
    canvas.set_height(UI_ICON_ATLAS_HEIGHT);
    let ctx = canvas
        .get_context("2d")?
        .ok_or_else(|| wasm_bindgen::JsValue::from_str("no 2d context"))?
        .dyn_into::<CanvasRenderingContext2d>()?;
    ctx.set_stroke_style_str("white");
    ctx.set_fill_style_str("white");
    ctx.set_line_join("round");
    ctx.set_line_cap("round");

    let cell = UI_ICON_ATLAS_CELL_PX as f64;
    draw_icon_eye(&ctx, 0.0, 0.0, cell, false)?;
    draw_icon_eye(&ctx, cell, 0.0, cell, true)?;
    draw_icon_chevron(&ctx, 0.0, cell, cell)?;

    let data = ctx.get_image_data(
        0.0,
        0.0,
        UI_ICON_ATLAS_WIDTH as f64,
        UI_ICON_ATLAS_HEIGHT as f64,
    )?;
    let pixels = data.data().to_vec();
    let texture = device.create_texture_with_data(
        queue,
        &wgpu::TextureDescriptor {
            label: Some("ui-icon-atlas"),
            size: wgpu::Extent3d {
                width: UI_ICON_ATLAS_WIDTH,
                height: UI_ICON_ATLAS_HEIGHT,
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
        &pixels,
    );
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let bind_group = Rc::new(device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("ui-icon-bind-group"),
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(sampler),
            },
        ],
    }));
    let _ = UiSpriteTexture::IconAtlas;
    Ok(IconAtlas {
        _texture: texture,
        _view: view,
        bind_group,
    })
}

fn draw_icon_eye(
    ctx: &CanvasRenderingContext2d,
    x: f64,
    y: f64,
    size: f64,
    slashed: bool,
) -> Result<(), wasm_bindgen::JsValue> {
    ctx.save();
    ctx.translate(x, y)?;
    let scale = size / 24.0;
    ctx.scale(scale, scale)?;
    ctx.set_line_width(2.0);
    let outline = Path2d::new_with_path_string(
        "M2 12c2.4-4.2 5.8-6.4 10-6.4s7.6 2.2 10 6.4c-2.4 4.2-5.8 6.4-10 6.4S4.4 16.2 2 12z",
    )?;
    ctx.stroke_with_path(&outline);
    ctx.begin_path();
    ctx.arc(12.0, 12.0, 3.2, 0.0, std::f64::consts::TAU)?;
    ctx.fill();
    if slashed {
        ctx.begin_path();
        ctx.move_to(5.0, 19.0);
        ctx.line_to(19.0, 5.0);
        ctx.stroke();
    }
    ctx.restore();
    Ok(())
}

fn draw_icon_chevron(
    ctx: &CanvasRenderingContext2d,
    x: f64,
    y: f64,
    size: f64,
) -> Result<(), wasm_bindgen::JsValue> {
    ctx.save();
    ctx.translate(x, y)?;
    let scale = size / 24.0;
    ctx.scale(scale, scale)?;
    ctx.set_line_width(2.0);
    let path = Path2d::new_with_path_string("M9 18l6-6-6-6")?;
    ctx.stroke_with_path(&path);
    ctx.restore();
    Ok(())
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
