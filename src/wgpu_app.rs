use std::cell::{Cell, RefCell};
use std::rc::Rc;
use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{request_animation_frame, AnimationFrame};
use gloo::timers::callback::Timeout;
use glyphon::{Attrs, Buffer, Family, FontSystem, Metrics, Shaping};
use glyphon::cosmic_text::Align;
use js_sys::{Date, Function, Reflect};
use taffy::prelude::*;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    CanvasRenderingContext2d, Document, Element, Event, HtmlCanvasElement, HtmlImageElement,
    MouseEvent, Touch, TouchEvent, WheelEvent,
};

use crate::app_core::{AppCore, AppSnapshot, PuzzleAssets, ViewRect};
use crate::app_router;
use crate::sync_runtime;
use crate::core::*;
use crate::renderer::{
    build_mask_atlas, Instance, InstanceBatch, InstanceSet, MaskAtlasData, UiRotationOrigin,
    UiSpriteSpec, UiSpriteTexture, UiTextId, UiTextSpec, WgpuRenderer,
};
use crate::runtime::{CoreAction, GameSyncView, GameView, ViewHooks};
use crate::view_runtime;
use heddobureika_core::PuzzleInfo;

const CREDIT_TEXT: &str = "coded by すごいジャン";
const UI_TITLE_TEXT: &str = "ヘッドブレイカー";
const CREDIT_URL: &str = "https://github.com/sugoijan/heddobureika";
const FPS_FONT_BYTES: &[u8] = include_bytes!("../fonts/chirufont.ttf");
const UI_FONT_FAMILY: &str = "KaoriGel";
const UI_CREDIT_FONT_RATIO: f32 = 0.026;
const UI_CREDIT_ROTATION_DEG: f32 = -1.1;
const UI_MENU_SUB_FONT_RATIO: f32 = 0.028;
const UI_MENU_SUB_ROTATION_DEG: f32 = 1.1;
const UI_MENU_TITLE_FONT_RATIO: f32 = 0.05;
const UI_MENU_TITLE_ROTATION_DEG: f32 = -0.8;
const UI_PROGRESS_FONT_RATIO: f32 = 0.032;
const UI_PROGRESS_ROTATION_DEG: f32 = -89.6;
const UI_DEBUG_FONT_RATIO: f32 = 0.02;
const UI_SUCCESS_FONT_RATIO: f32 = 0.082;
const UI_SUCCESS_OFFSET_RATIO: f32 = 0.03;
const UI_SUCCESS_ROTATION_DEG: f32 = -1.3;
const UI_TEXT_ALPHA_SCALE: f32 = 0.8;
const UI_TEXT_LAYOUT_SCALE: f32 = 1.08;
const UI_TEXT_LAYOUT_PAD: f32 = 6.0;
const UI_TEXT_HITBOX_PAD: f32 = 8.0;
const UI_TITLE_ROTATION_DEG: f32 = 0.5;
const UI_TITLE_FONT_RATIO: f32 = 0.058;
const DRAG_SCALE: f32 = 1.01;
const DRAG_ROTATION_DEG: f32 = 1.0;
const OUTLINE_KIND_HOVER: f32 = 1.0;
const OUTLINE_KIND_DEBUG: f32 = 3.0;
const WGPU_FPS_CAP_DEFAULT: f32 = 60.0;
const WGPU_FPS_IDLE_RESET_MS: f32 = 800.0;
const PREVIEW_BOX_WIDTH: f32 = 170.0;
const PREVIEW_BOX_MIN_WIDTH: f32 = 120.0;
const PREVIEW_BOX_INIT_FRAC: f32 = 0.24;
const PREVIEW_BOX_MAX_FRAC: f32 = 0.28;
const PREVIEW_BOX_PADDING: f32 = 8.0;
const PREVIEW_BOX_MARGIN: f32 = 16.0;
const PREVIEW_PANEL_RADIUS: f32 = 12.0;
const PREVIEW_PANEL_BORDER: f32 = 1.0;
const PREVIEW_HIDDEN_BLUR_PX: f32 = 6.0;
const PREVIEW_HIDDEN_OPACITY: f32 = 0.6;
const PREVIEW_ANIM_SPEED: f32 = 12.0;
const PREVIEW_RESIZE_BORDER: f32 = 10.0;
const PREVIEW_MIN_VISIBLE: f32 = 32.0;
const PREVIEW_MIN_VISIBLE_FRAC: f32 = 0.35;
const PREVIEW_MOMENTUM_DECAY: f32 = 8.0;
const PREVIEW_VELOCITY_EPS: f32 = 10.0;
const PREVIEW_VELOCITY_SMOOTH: f32 = 0.25;
const PREVIEW_CLICK_SLOP: f32 = 4.0;
const PREVIEW_TILT_DEG: f32 = 1.1;
// Keep in sync with ViewState::fit_zoom_for_size in src/app_core.rs.
const PREVIEW_FIT_PADDING_RATIO: f32 = 0.02;
const PREVIEW_MAX_FIT_SCALE: f32 = 0.97;

#[derive(Clone, Copy)]
struct UiHitbox {
    center: [f32; 2],
    half_size: [f32; 2],
    rotation_deg: f32,
}

struct PanState {
    last_x: f32,
    last_y: f32,
    touch_id: Option<i32>,
}

struct PinchState {
    touch_a: i32,
    touch_b: i32,
    last_distance: f32,
}

struct TouchDragGate {
    touch_id: i32,
    start_x: f32,
    start_y: f32,
    moved: bool,
}

#[derive(Clone, Copy)]
struct AutoPanRect {
    left: f32,
    right: f32,
    top: f32,
    bottom: f32,
}

impl AutoPanRect {
    fn contains(&self, x: f32, y: f32) -> bool {
        x >= self.left && x <= self.right && y >= self.top && y <= self.bottom
    }
}

struct AutoPanZones {
    outer: AutoPanRect,
    inner: AutoPanRect,
    outer_inset: f32,
    inner_inset: f32,
}

struct AutoPanState {
    active: bool,
    seen_inner: bool,
    has_pointer: bool,
    last_screen_x: f32,
    last_screen_y: f32,
    last_tick_ms: Option<f64>,
    frame: Option<AnimationFrame>,
}

impl AutoPanState {
    fn new() -> Self {
        Self {
            active: false,
            seen_inner: false,
            has_pointer: false,
            last_screen_x: 0.0,
            last_screen_y: 0.0,
            last_tick_ms: None,
            frame: None,
        }
    }
}

struct DebugOverlay {
    root: Element,
    outer: Element,
    inner: Element,
}

impl DebugOverlay {
    fn new(document: &Document) -> Self {
        let root = document
            .create_element("div")
            .expect("create debug overlay root");
        root.set_class_name("debug-rect-overlay");
        let outer = document
            .create_element("div")
            .expect("create debug outer rect");
        outer.set_class_name("debug-rect debug-rect-outer");
        let inner = document
            .create_element("div")
            .expect("create debug inner rect");
        inner.set_class_name("debug-rect debug-rect-inner");
        let _ = root.append_child(&outer);
        let _ = root.append_child(&inner);
        Self { root, outer, inner }
    }

    fn set_visible(&self, visible: bool) {
        if visible {
            let _ = self.root.remove_attribute("style");
        } else {
            let _ = self.root.set_attribute("style", "display: none;");
        }
    }

    fn set_insets(&self, outer_inset: f32, inner_inset: f32) {
        let outer_style = format!("inset: {}px;", fmt_f32(outer_inset));
        let inner_style = format!("inset: {}px;", fmt_f32(inner_inset));
        let _ = self.outer.set_attribute("style", &outer_style);
        let _ = self.inner.set_attribute("style", &inner_style);
    }
}

struct GlyphonMeasureState {
    font_system: FontSystem,
}

impl GlyphonMeasureState {
    fn new() -> Self {
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(FPS_FONT_BYTES.to_vec());
        Self { font_system }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PreviewResizeHandle {
    Left,
    Right,
    Top,
    Bottom,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PreviewHoverTarget {
    None,
    Body,
    Resize(PreviewResizeHandle),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PreviewDragKind {
    Move,
    Resize,
    Pinch,
}

#[derive(Clone, Copy, Debug)]
enum PreviewDragState {
    Move {
        pointer_id: Option<i32>,
        grab_offset: [f32; 2],
        last_ms: f32,
    },
    Resize {
        pointer_id: Option<i32>,
        handle: PreviewResizeHandle,
        start_pos: [f32; 2],
        start_width: f32,
        last_ms: f32,
    },
    Pinch {
        touch_a: i32,
        touch_b: i32,
        start_distance: f32,
        start_width: f32,
        anchor_rel: [f32; 2],
    },
}

#[derive(Clone, Copy, Debug)]
struct PreviewClickState {
    pointer_id: Option<i32>,
    start: [f32; 2],
    moved: bool,
}

impl PreviewDragState {
    fn pointer_id(&self) -> Option<i32> {
        match self {
            PreviewDragState::Move { pointer_id, .. } => *pointer_id,
            PreviewDragState::Resize { pointer_id, .. } => *pointer_id,
            PreviewDragState::Pinch { .. } => None,
        }
    }

    fn kind(&self) -> PreviewDragKind {
        match self {
            PreviewDragState::Move { .. } => PreviewDragKind::Move,
            PreviewDragState::Resize { .. } => PreviewDragKind::Resize,
            PreviewDragState::Pinch { .. } => PreviewDragKind::Pinch,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Rect {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
}

impl Rect {
    fn contains(&self, x: f32, y: f32) -> bool {
        x >= self.x && x <= self.x + self.w && y >= self.y && y <= self.y + self.h
    }

    fn inset(&self, inset: f32) -> Self {
        let inset = inset.max(0.0);
        let w = (self.w - inset * 2.0).max(0.0);
        let h = (self.h - inset * 2.0).max(0.0);
        Self {
            x: self.x + inset,
            y: self.y + inset,
            w,
            h,
        }
    }

    fn center(&self) -> [f32; 2] {
        [self.x + self.w * 0.5, self.y + self.h * 0.5]
    }
}

struct PreviewLayout {
    box_rect: Rect,
    image_rect: Rect,
}

struct PreviewColors {
    panel_bg: [f32; 4],
    panel_border: [f32; 4],
}

thread_local! {
    static WGPU_VIEW: RefCell<Option<Rc<WgpuView>>> = RefCell::new(None);
    static WGPU_VIEW_ADAPTER: RefCell<Option<Rc<RefCell<WgpuViewAdapter>>>> = RefCell::new(None);
}

pub(crate) fn request_render() {
    WGPU_VIEW.with(|slot| {
        if let Some(view) = slot.borrow().as_ref() {
            view.request_render();
        }
    });
}

#[derive(Clone, PartialEq)]
struct PuzzleKey {
    src: String,
    cols: u32,
    rows: u32,
    width: u32,
    height: u32,
}

impl PuzzleKey {
    fn from_info(info: &PuzzleInfo) -> Self {
        Self {
            src: info.image_src.clone(),
            cols: info.cols,
            rows: info.rows,
            width: info.image_width,
            height: info.image_height,
        }
    }
}

pub(crate) fn run(core: Rc<AppCore>) {
    #[cfg(target_arch = "wasm32")]
    {
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document
            .get_element_by_id("wgpu-root")
            .expect("wgpu-root exists");

        let renderer_kind = app_router::load_renderer_preference();
        core.set_renderer_kind(renderer_kind);
        if renderer_kind != RendererKind::Wgpu {
            return;
        }

        root.set_class_name("app wgpu");

        let canvas = document
            .create_element("canvas")
            .expect("create canvas")
            .dyn_into::<HtmlCanvasElement>()
            .expect("canvas element");
        canvas.set_class_name("puzzle-canvas");
        root.append_child(&canvas).expect("append canvas");
        let sync_status = document
            .create_element("div")
            .expect("create sync status");
        sync_status.set_class_name("sync-status");
        sync_status
            .set_attribute("title", "Server disconnected")
            .ok();
        sync_status.set_text_content(Some("!"));
        root.append_child(&sync_status).ok();

        let render_settings = app_router::load_render_settings_with_init();
        let view = Rc::new(WgpuView::new(
            core.clone(),
            root.clone(),
            canvas,
            render_settings.wgpu.clone(),
        ));
        let debug_overlay = DebugOverlay::new(&document);
        root.append_child(&debug_overlay.root).ok();
        *view.debug_overlay.borrow_mut() = Some(debug_overlay);
        *view.sync_hook.borrow_mut() = Some(sync_runtime::register_sync_view_hook(Rc::new(
            move || {
                request_render();
            },
        )));
        view_runtime::set_wgpu_settings_hook(Some(Rc::new({
            let view = view.clone();
            move |settings| {
                view.apply_wgpu_settings(settings);
            }
        })));
        let adapter = Rc::new(RefCell::new(WgpuViewAdapter::new(view.clone())));
        let core_for_hooks = core.clone();
        adapter.borrow_mut().init(ViewHooks {
            on_action: Rc::new(move |action| {
                sync_runtime::dispatch_view_action(&core_for_hooks, action, true);
            }),
        });
        view.update_viewport_size();
        view.install_listeners();
        let core_for_render = core.clone();
        let adapter_for_render = adapter.clone();
        let subscription = core.subscribe(Rc::new(move || {
            let snapshot = core_for_render.snapshot();
            let sync_view = sync_runtime::sync_view();
            adapter_for_render.borrow_mut().render(&snapshot, &sync_view);
        }));
        *view.subscription.borrow_mut() = Some(subscription);
        WGPU_VIEW.with(|slot| {
            *slot.borrow_mut() = Some(view.clone());
        });
        WGPU_VIEW_ADAPTER.with(|slot| {
            *slot.borrow_mut() = Some(adapter);
        });
        view.request_render();
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("wgpu-only build is only supported on wasm32 targets");
    }
}

struct WgpuView {
    core: Rc<AppCore>,
    root: Element,
    canvas: HtmlCanvasElement,
    image: RefCell<Option<HtmlImageElement>>,
    renderer: RefCell<Option<WgpuRenderer>>,
    backend_label: RefCell<Option<String>>,
    creating: Cell<bool>,
    pending_instances: RefCell<Option<InstanceSet>>,
    pending_ui: RefCell<Option<Vec<UiTextSpec>>>,
    ui_measure: RefCell<GlyphonMeasureState>,
    ui_credit_hitbox: RefCell<Option<UiHitbox>>,
    ui_credit_hovered: Cell<bool>,
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    touch_drag_gate: RefCell<Option<TouchDragGate>>,
    auto_pan: RefCell<AutoPanState>,
    pending_snapshot: RefCell<Option<AppSnapshot>>,
    render_timer: RefCell<Option<Timeout>>,
    idle_fps_timer: RefCell<Option<Timeout>>,
    last_render_ms: Cell<f32>,
    idle_fps_rendered: Cell<bool>,
    force_fps_fallback: Cell<bool>,
    subscription: RefCell<Option<crate::app_core::AppSubscription>>,
    listeners: RefCell<Vec<EventListener>>,
    mask_atlas: RefCell<Option<Rc<MaskAtlasData>>>,
    last_view_size: Cell<(u32, u32)>,
    puzzle_epoch: Cell<u64>,
    last_puzzle: RefCell<Option<PuzzleKey>>,
    hooks: RefCell<Option<ViewHooks>>,
    sync_hook: RefCell<Option<sync_runtime::SyncViewHookHandle>>,
    preview_revealed: Cell<bool>,
    preview_hover: Cell<PreviewHoverTarget>,
    preview_pos: Cell<[f32; 2]>,
    preview_width: Cell<f32>,
    preview_velocity: Cell<[f32; 2]>,
    preview_drag: RefCell<Option<PreviewDragState>>,
    preview_click: RefCell<Option<PreviewClickState>>,
    preview_seeded: Cell<bool>,
    preview_blur: Cell<f32>,
    preview_target_blur: Cell<f32>,
    preview_anim_last_ms: Cell<f32>,
    preview_motion_last_ms: Cell<f32>,
    pending_ui_overlay: RefCell<Option<Vec<UiSpriteSpec>>>,
    debug_overlay: RefCell<Option<DebugOverlay>>,
    wgpu_settings: RefCell<WgpuRenderSettings>,
}

impl WgpuView {
    fn new(
        core: Rc<AppCore>,
        root: Element,
        canvas: HtmlCanvasElement,
        wgpu_settings: WgpuRenderSettings,
    ) -> Self {
        Self {
            core,
            root,
            canvas,
            image: RefCell::new(None),
            renderer: RefCell::new(None),
            backend_label: RefCell::new(None),
            creating: Cell::new(false),
            pending_instances: RefCell::new(None),
            pending_ui: RefCell::new(None),
            ui_measure: RefCell::new(GlyphonMeasureState::new()),
            ui_credit_hitbox: RefCell::new(None),
            ui_credit_hovered: Cell::new(false),
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            touch_drag_gate: RefCell::new(None),
            auto_pan: RefCell::new(AutoPanState::new()),
            pending_snapshot: RefCell::new(None),
            render_timer: RefCell::new(None),
            idle_fps_timer: RefCell::new(None),
            last_render_ms: Cell::new(0.0),
            idle_fps_rendered: Cell::new(false),
            force_fps_fallback: Cell::new(false),
            subscription: RefCell::new(None),
            listeners: RefCell::new(Vec::new()),
            mask_atlas: RefCell::new(None),
            last_view_size: Cell::new((0, 0)),
            puzzle_epoch: Cell::new(0),
            last_puzzle: RefCell::new(None),
            hooks: RefCell::new(None),
            sync_hook: RefCell::new(None),
            preview_revealed: Cell::new(false),
            preview_hover: Cell::new(PreviewHoverTarget::None),
            preview_pos: Cell::new([0.0, 0.0]),
            preview_width: Cell::new(PREVIEW_BOX_WIDTH),
            preview_velocity: Cell::new([0.0, 0.0]),
            preview_drag: RefCell::new(None),
            preview_click: RefCell::new(None),
            preview_seeded: Cell::new(false),
            preview_blur: Cell::new(PREVIEW_HIDDEN_BLUR_PX),
            preview_target_blur: Cell::new(PREVIEW_HIDDEN_BLUR_PX),
            preview_anim_last_ms: Cell::new(0.0),
            preview_motion_last_ms: Cell::new(0.0),
            pending_ui_overlay: RefCell::new(None),
            debug_overlay: RefCell::new(None),
            wgpu_settings: RefCell::new(wgpu_settings),
        }
    }

    fn set_image(&self, image: HtmlImageElement) {
        *self.image.borrow_mut() = Some(image);
    }

    fn apply_wgpu_settings(self: &Rc<Self>, settings: WgpuRenderSettings) {
        let mut current = self.wgpu_settings.borrow_mut();
        if *current == settings {
            return;
        }
        let render_scale_changed =
            (current.render_scale - settings.render_scale).abs() > f32::EPSILON;
        *current = settings.clone();
        drop(current);
        if let Some(renderer) = self.renderer.borrow_mut().as_mut() {
            renderer.set_edge_aa(settings.edge_aa);
            renderer.set_show_fps(settings.show_fps);
        }
        if render_scale_changed {
            self.last_puzzle.borrow_mut().take();
        }
        self.request_render();
    }

    fn is_current_puzzle(&self, key: &PuzzleKey) -> bool {
        self.last_puzzle.borrow().as_ref() == Some(key)
    }

    fn reset_puzzle_cache(&self) {
        self.puzzle_epoch
            .set(self.puzzle_epoch.get().wrapping_add(1));
        self.creating.set(false);
        self.renderer.borrow_mut().take();
        self.mask_atlas.borrow_mut().take();
        self.pending_instances.borrow_mut().take();
        self.pending_ui.borrow_mut().take();
        self.pending_ui_overlay.borrow_mut().take();
        self.preview_drag.borrow_mut().take();
        self.preview_click.borrow_mut().take();
        self.preview_seeded.set(false);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(0.0);
        *self.image.borrow_mut() = None;
    }

    fn update_viewport_size(&self) {
        let rect = self.root.get_bounding_client_rect();
        let width = rect.width() as f32;
        let height = rect.height() as f32;
        self.core.set_viewport_size(width, height);
    }

    fn load_image_for_puzzle(
        self: &Rc<Self>,
        key: PuzzleKey,
        image_max_dim: u32,
        render_scale: f32,
    ) {
        let img = HtmlImageElement::new().expect("create image element");
        let img_clone = img.clone();
        let view = Rc::clone(self);
        let key_for_onload = key.clone();
        let src = key.src.clone();
        let render_scale = render_scale.clamp(WGPU_RENDER_SCALE_MIN, WGPU_RENDER_SCALE_MAX);
        let source_cap = (IMAGE_MAX_DIMENSION_MAX as f32 * WGPU_RENDER_SCALE_MAX)
            .round()
            .max(1.0) as u32;
        let mut source_max_dim =
            ((image_max_dim as f32) * render_scale).round().max(1.0) as u32;
        if source_max_dim > source_cap {
            source_max_dim = source_cap;
        }
        let onload = Closure::<dyn FnMut()>::wrap(Box::new(move || {
            if !view.is_current_puzzle(&key_for_onload) {
                return;
            }
            let width = img_clone.natural_width();
            let height = img_clone.natural_height();
            let max_axis = width.max(height).max(1);
            let source_scale = if max_axis > source_max_dim {
                (source_max_dim as f64) / (max_axis as f64)
            } else {
                1.0
            };
            let target_w = ((width as f64) * source_scale).round().max(1.0) as u32;
            let target_h = ((height as f64) * source_scale).round().max(1.0) as u32;
            if target_w == width && target_h == height {
                view.set_image(img_clone.clone());
                view.request_render();
                return;
            }
            let document = match web_sys::window().and_then(|window| window.document()) {
                Some(doc) => doc,
                None => {
                    view.set_image(img_clone.clone());
                    view.request_render();
                    return;
                }
            };
            let canvas = match document
                .create_element("canvas")
                .ok()
                .and_then(|node| node.dyn_into::<HtmlCanvasElement>().ok())
            {
                Some(canvas) => canvas,
                None => {
                    view.set_image(img_clone.clone());
                    view.request_render();
                    return;
                }
            };
            canvas.set_width(target_w);
            canvas.set_height(target_h);
            let ctx = match canvas
                .get_context("2d")
                .ok()
                .flatten()
                .and_then(|ctx| ctx.dyn_into::<CanvasRenderingContext2d>().ok())
            {
                Some(ctx) => ctx,
                None => {
                    view.set_image(img_clone.clone());
                    view.request_render();
                    return;
                }
            };
            ctx.set_image_smoothing_enabled(true);
            if ctx
                .draw_image_with_html_image_element_and_dw_and_dh(
                    &img_clone,
                    0.0,
                    0.0,
                    target_w as f64,
                    target_h as f64,
                )
                .is_err()
            {
                view.set_image(img_clone.clone());
                view.request_render();
                return;
            }
            let data_url = match canvas.to_data_url() {
                Ok(data_url) => data_url,
                Err(_) => {
                    view.set_image(img_clone.clone());
                    view.request_render();
                    return;
                }
            };
            let scaled = match HtmlImageElement::new() {
                Ok(image) => image,
                Err(_) => {
                    view.set_image(img_clone.clone());
                    view.request_render();
                    return;
                }
            };
            let scaled_clone = scaled.clone();
            let view_scaled = view.clone();
            let key_scaled = key_for_onload.clone();
            let onload_scaled = Closure::<dyn FnMut()>::wrap(Box::new(move || {
                if !view_scaled.is_current_puzzle(&key_scaled) {
                    return;
                }
                view_scaled.set_image(scaled_clone.clone());
                view_scaled.request_render();
            }));
            scaled.set_onload(Some(onload_scaled.as_ref().unchecked_ref()));
            scaled.set_src(&data_url);
            onload_scaled.forget();
        }));
        img.set_onload(Some(onload.as_ref().unchecked_ref()));
        img.set_src(&src);
        onload.forget();
    }

    fn ensure_puzzle_image(self: &Rc<Self>, snapshot: &AppSnapshot) {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            self.last_puzzle.replace(None);
            return;
        };
        let key = PuzzleKey::from_info(info);
        {
            let mut last = self.last_puzzle.borrow_mut();
            if last.as_ref() == Some(&key) {
                return;
            }
            if last.is_none() && self.image.borrow().is_some() {
                *last = Some(key);
                return;
            }
            *last = Some(key.clone());
        }
        self.reset_puzzle_cache();
        let image_max_dim = self.core.image_max_dim();
        let render_scale = self.wgpu_settings.borrow().render_scale;
        self.load_image_for_puzzle(key, image_max_dim, render_scale);
    }

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        let canvas = self.canvas.clone();
        let canvas_for_down = canvas.clone();
        let core = self.core.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "mousedown",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
            let Some(event) = event.dyn_ref::<MouseEvent>() else {
                return;
            };
            if event.button() != 0 && event.button() != 2 {
                return;
            }
            let snapshot = core.snapshot();
            let rect = canvas_for_down.get_bounding_client_rect();
            let local_x = event.client_x() as f32 - rect.left() as f32;
            let local_y = event.client_y() as f32 - rect.top() as f32;
            if let Some(layout) = view.preview_layout(&snapshot, &rect) {
                let target = view.preview_hit_test(&layout, local_x, local_y);
                if target != PreviewHoverTarget::None {
                    if event.button() == 0 {
                        let [drag_x, drag_y] = view.preview_local_point(&layout, local_x, local_y);
                        view.begin_preview_drag(target, drag_x, drag_y, None);
                        if matches!(target, PreviewHoverTarget::Body) {
                            view.arm_preview_click(None, local_x, local_y);
                        } else {
                            view.clear_preview_click();
                        }
                        view.request_render();
                    }
                    if view.set_preview_hover(target) {
                        view.request_render();
                    }
                    view.update_canvas_class(&snapshot);
                    event.prevent_default();
                    return;
                }
            }
            let Some((view_x, view_y)) =
                event_to_canvas_coords(event, &canvas_for_down, snapshot.view)
            else {
                return;
            };
            view.clear_preview_click();
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                view.touch_drag_gate.borrow_mut().take();
                let right_click = event.button() == 2;
                view.dispatch_action(CoreAction::BeginDrag {
                    piece_id,
                    x: px,
                    y: py,
                    shift_key: event.shift_key(),
                    rotate_mode: event.ctrl_key(),
                    right_click,
                    touch_id: None,
                });
                let drag_snapshot = view.core.snapshot();
                view.update_auto_pan(event.client_x() as f32, event.client_y() as f32, &drag_snapshot);
                event.prevent_default();
                return;
            }
            if view.hit_credit(view_x, view_y) {
                open_credit_url();
                event.prevent_default();
                return;
            }
            if event.button() == 0 {
                view.ui_credit_hovered.set(false);
                *view.pan_state.borrow_mut() = Some(PanState {
                    last_x: event.client_x() as f32,
                    last_y: event.client_y() as f32,
                    touch_id: None,
                });
                view.update_canvas_class(&snapshot);
                event.prevent_default();
            }
        },
        );
        listeners.push(listener);

        let listener = EventListener::new_with_options(
            &canvas,
            "contextmenu",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let core = self.core.clone();
        let canvas_for_wheel = canvas.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "wheel",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<WheelEvent>() else {
                    return;
                };
                let snapshot = core.snapshot();
                let rect = canvas_for_wheel.get_bounding_client_rect();
                let rect_width = rect.width() as f32;
                let rect_height = rect.height() as f32;
                if rect_width <= 0.0 || rect_height <= 0.0 {
                    return;
                }
                let mut delta_x = event.delta_x() as f32;
                let mut delta_y = event.delta_y() as f32;
                match event.delta_mode() {
                    1 => {
                        delta_x *= 16.0;
                        delta_y *= 16.0;
                    }
                    2 => {
                        delta_x *= rect_width;
                        delta_y *= rect_height;
                    }
                    _ => {}
                }
                if event.ctrl_key() || event.meta_key() {
                    if let Some((dx_world, dy_world)) =
                        screen_delta_to_world(delta_x, delta_y, &canvas_for_wheel, snapshot.view)
                    {
                        core.pan_view(dx_world, dy_world);
                    }
                } else {
                    let Some((view_x, view_y)) = screen_to_view_coords(
                        event.client_x() as f32,
                        event.client_y() as f32,
                        &canvas_for_wheel,
                        snapshot.view,
                    ) else {
                        return;
                    };
                    let zoom_factor = (-delta_y * 0.0015).exp();
                    if zoom_factor.is_finite() && zoom_factor > 0.0 {
                        core.zoom_view_at(zoom_factor, view_x, view_y);
                    }
                }
                view.update_canvas_class(&snapshot);
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let core = self.core.clone();
        let canvas_for_move = canvas.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new(&canvas, "mousemove", move |event: &Event| {
            let Some(event) = event.dyn_ref::<MouseEvent>() else {
                return;
            };
            let snapshot = core.snapshot();
            if view.preview_drag.borrow().is_some() {
                return;
            }
            if !snapshot.dragging_members.is_empty() || view.pan_state.borrow().is_some() {
                if view.set_preview_hover(PreviewHoverTarget::None) {
                view.update_canvas_class(&snapshot);
                    view.request_render();
                }
                return;
            }
            let rect = canvas_for_move.get_bounding_client_rect();
            let local_x = event.client_x() as f32 - rect.left() as f32;
            let local_y = event.client_y() as f32 - rect.top() as f32;
            let mut preview_target = PreviewHoverTarget::None;
            if let Some(layout) = view.preview_layout(&snapshot, &rect) {
                preview_target = view.preview_hit_test(&layout, local_x, local_y);
            }
            if view.set_preview_hover(preview_target) {
                view.update_canvas_class(&snapshot);
                view.request_render();
            }
            if preview_target != PreviewHoverTarget::None {
                if view.ui_credit_hovered.get() {
                    view.ui_credit_hovered.set(false);
                view.update_canvas_class(&snapshot);
                }
                view.dispatch_action(CoreAction::SetHovered { hovered: None });
                return;
            }
            let Some((view_x, view_y)) =
                event_to_canvas_coords(event, &canvas_for_move, snapshot.view)
            else {
                view.dispatch_action(CoreAction::SetHovered { hovered: None });
                return;
            };
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            let hovered = view.pick_piece(px, py, &snapshot);
            if hovered.is_some() {
                if view.ui_credit_hovered.get() {
                    view.ui_credit_hovered.set(false);
                view.update_canvas_class(&snapshot);
                }
                view.dispatch_action(CoreAction::SetHovered { hovered });
                return;
            }
            let credit_hovered = view.hit_credit(view_x, view_y);
            if view.ui_credit_hovered.get() != credit_hovered {
                view.ui_credit_hovered.set(credit_hovered);
                view.update_canvas_class(&snapshot);
            }
            if credit_hovered {
                view.dispatch_action(CoreAction::SetHovered { hovered: None });
                return;
            }
            view.dispatch_action(CoreAction::SetHovered { hovered });
        });
        listeners.push(listener);

        let view = Rc::clone(self);
        let core_for_leave = self.core.clone();
        let listener = EventListener::new(&canvas, "mouseleave", move |_event: &Event| {
            view.ui_credit_hovered.set(false);
            if view.set_preview_hover(PreviewHoverTarget::None) {
                view.request_render();
            }
            let snapshot = core_for_leave.snapshot();
                view.update_canvas_class(&snapshot);
            view.dispatch_action(CoreAction::SetHovered { hovered: None });
        });
        listeners.push(listener);

        let core = self.core.clone();
        let canvas_for_touch = canvas.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "touchstart",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
            let Some(event) = event.dyn_ref::<TouchEvent>() else {
                return;
            };
            let touch_count = event.touches().length();
            let snapshot = core.snapshot();
            if touch_count >= 2 {
                view.touch_drag_gate.borrow_mut().take();
                event.prevent_default();
                if !snapshot.dragging_members.is_empty() {
                    view.dispatch_action(CoreAction::DragEnd {
                        touch_id: snapshot.drag_touch_id,
                    });
                }
                if let (Some(touch_a), Some(touch_b)) =
                    (event.touches().item(0), event.touches().item(1))
                {
                    let rect = canvas_for_touch.get_bounding_client_rect();
                    let local_ax = touch_a.client_x() as f32 - rect.left() as f32;
                    let local_ay = touch_a.client_y() as f32 - rect.top() as f32;
                    let local_bx = touch_b.client_x() as f32 - rect.left() as f32;
                    let local_by = touch_b.client_y() as f32 - rect.top() as f32;
                    if let Some(layout) = view.preview_layout(&snapshot, &rect) {
                        if view.preview_point_inside(&layout, local_ax, local_ay)
                            && view.preview_point_inside(&layout, local_bx, local_by)
                        {
                            view.clear_preview_click();
                            if view.begin_preview_pinch(&touch_a, &touch_b, &snapshot, &rect) {
                                view.update_canvas_class(&snapshot);
                                view.request_render();
                                event.prevent_default();
                                return;
                            }
                        }
                    }
                    let dx = touch_b.client_x() as f32 - touch_a.client_x() as f32;
                    let dy = touch_b.client_y() as f32 - touch_a.client_y() as f32;
                    let distance = (dx * dx + dy * dy).sqrt();
                    if distance > 0.0 {
                        *view.pinch_state.borrow_mut() = Some(PinchState {
                            touch_a: touch_a.identifier(),
                            touch_b: touch_b.identifier(),
                            last_distance: distance,
                        });
                        if view.pan_state.borrow_mut().take().is_some() {
                            core.settle_view();
                        }
                        view.update_canvas_class(&snapshot);
                        event.prevent_default();
                    }
                }
                return;
            }
            let touch = touch_from_event(event, None, true);
            let Some(touch) = touch else {
                return;
            };
            let screen_x = touch.client_x() as f32;
            let screen_y = touch.client_y() as f32;
            let rect = canvas_for_touch.get_bounding_client_rect();
            let local_x = screen_x - rect.left() as f32;
            let local_y = screen_y - rect.top() as f32;
                if let Some(layout) = view.preview_layout(&snapshot, &rect) {
                    let target = view.preview_hit_test(&layout, local_x, local_y);
                    if target != PreviewHoverTarget::None {
                        let [drag_x, drag_y] =
                            view.preview_local_point(&layout, local_x, local_y);
                        view.begin_preview_drag(
                            target,
                            drag_x,
                            drag_y,
                            Some(touch.identifier()),
                        );
                        if matches!(target, PreviewHoverTarget::Body) {
                            view.arm_preview_click(Some(touch.identifier()), local_x, local_y);
                        } else {
                            view.clear_preview_click();
                        }
                        view.update_canvas_class(&snapshot);
                        view.request_render();
                        event.prevent_default();
                        return;
                    }
                }
                view.clear_preview_click();
            let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, &canvas_for_touch, snapshot.view)
            else {
                return;
            };
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                *view.touch_drag_gate.borrow_mut() = Some(TouchDragGate {
                    touch_id: touch.identifier(),
                    start_x: px,
                    start_y: py,
                    moved: false,
                });
                view.dispatch_action(CoreAction::BeginDrag {
                    piece_id,
                    x: px,
                    y: py,
                    shift_key: false,
                    rotate_mode: false,
                    right_click: false,
                    touch_id: Some(touch.identifier()),
                });
                let drag_snapshot = view.core.snapshot();
                view.update_auto_pan(
                    touch.client_x() as f32,
                    touch.client_y() as f32,
                    &drag_snapshot,
                );
                event.prevent_default();
                return;
            }
            if view.hit_credit(view_x, view_y) {
                open_credit_url();
                event.prevent_default();
                return;
            }
            *view.pan_state.borrow_mut() = Some(PanState {
                last_x: screen_x,
                last_y: screen_y,
                touch_id: Some(touch.identifier()),
            });
            view.pinch_state.borrow_mut().take();
            view.ui_credit_hovered.set(false);
            view.update_canvas_class(&snapshot);
            event.prevent_default();
        },
        );
        listeners.push(listener);

        let window = web_sys::window().expect("window available");
        let core = self.core.clone();
        let canvas_for_drag = canvas.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &window,
            "mousemove",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<MouseEvent>() else {
                    return;
                };
                let snapshot = core.snapshot();
                let rect = canvas_for_drag.get_bounding_client_rect();
                let local_x = event.client_x() as f32 - rect.left() as f32;
                let local_y = event.client_y() as f32 - rect.top() as f32;
                view.update_preview_click_movement(None, local_x, local_y);
                if view.update_preview_drag(None, local_x, local_y, now_ms(), &snapshot, &rect) {
                    view.request_render();
                    event.prevent_default();
                    return;
                }
                if !snapshot.dragging_members.is_empty() {
                    view.update_auto_pan(event.client_x() as f32, event.client_y() as f32, &snapshot);
                    let Some((view_x, view_y)) =
                        event_to_canvas_coords(event, &canvas_for_drag, snapshot.view)
                    else {
                        return;
                    };
                    let (px, py) =
                        workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                    view.dispatch_action(CoreAction::DragMove { x: px, y: py });
                    event.prevent_default();
                    return;
                }
                let delta = {
                    let mut pan_state = view.pan_state.borrow_mut();
                    let Some(pan) = pan_state.as_mut() else {
                        return;
                    };
                    let dx = event.client_x() as f32 - pan.last_x;
                    let dy = event.client_y() as f32 - pan.last_y;
                    pan.last_x = event.client_x() as f32;
                    pan.last_y = event.client_y() as f32;
                    screen_delta_to_world(dx, dy, &canvas_for_drag, snapshot.view)
                };
                if let Some((dx_world, dy_world)) = delta {
                    core.pan_view(dx_world, dy_world);
                }
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let core = self.core.clone();
        let canvas_for_up = canvas.clone();
        let listener = EventListener::new_with_options(
            &window,
            "mouseup",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<MouseEvent>() else {
                    return;
                };
                let rect = canvas_for_up.get_bounding_client_rect();
                let local_x = event.client_x() as f32 - rect.left() as f32;
                let local_y = event.client_y() as f32 - rect.top() as f32;
                view.update_preview_click_movement(None, local_x, local_y);
                let preview_clicked = view.consume_preview_click(None);
                let preview_dragged = view.end_preview_drag(None);
                if preview_clicked {
                    view.toggle_preview_revealed();
                }
                if preview_dragged || preview_clicked {
                    view.request_render();
                    event.prevent_default();
                    return;
                }
                let snapshot = core.snapshot();
                view.touch_drag_gate.borrow_mut().take();
                view.stop_auto_pan();
                let mut pan_cleared = false;
                if view.pan_state.borrow().is_some() {
                    view.pan_state.borrow_mut().take();
                    pan_cleared = true;
                    view.update_canvas_class(&snapshot);
                }
                if pan_cleared {
                    core.settle_view();
                }
                if !snapshot.dragging_members.is_empty() {
                    if let Some((view_x, view_y)) =
                        event_to_canvas_coords(event, &canvas_for_up, snapshot.view)
                    {
                        let (px, py) = workspace_to_puzzle_coords(
                            snapshot.layout.puzzle_scale,
                            view_x,
                            view_y,
                        );
                        view.dispatch_action(CoreAction::DragMove { x: px, y: py });
                    }
                }
                view.dispatch_action(CoreAction::DragEnd { touch_id: None });
                let snapshot = core.snapshot();
                if !snapshot.dragging_members.is_empty() {
                    return;
                }
                let hovered = event_to_canvas_coords(event, &canvas_for_up, snapshot.view)
                    .and_then(|(view_x, view_y)| {
                        let (px, py) = workspace_to_puzzle_coords(
                            snapshot.layout.puzzle_scale,
                            view_x,
                            view_y,
                        );
                        view.pick_piece(px, py, &snapshot)
                    });
                view.dispatch_action(CoreAction::SetHovered { hovered });
            },
        );
        listeners.push(listener);

        let core = self.core.clone();
        let canvas_for_touch_move = canvas.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &window,
            "touchmove",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<TouchEvent>() else {
                    return;
                };
                let touch_count = event.touches().length();
            if touch_count >= 2 {
                event.prevent_default();
            }
            let snapshot = core.snapshot();
            let pinch_state = *view.preview_drag.borrow();
            if let Some(PreviewDragState::Pinch {
                touch_a,
                touch_b,
                start_distance,
                start_width,
                anchor_rel,
            }) = pinch_state
            {
                let touch_a = touch_from_event(event, Some(touch_a), false);
                let touch_b = touch_from_event(event, Some(touch_b), false);
                if let (Some(touch_a), Some(touch_b)) = (touch_a, touch_b) {
                    let rect = canvas_for_touch_move.get_bounding_client_rect();
                    if view.update_preview_pinch(
                        &touch_a,
                        &touch_b,
                        &snapshot,
                        &rect,
                        now_ms(),
                        start_distance,
                        start_width,
                        anchor_rel,
                    ) {
                        view.request_render();
                        event.prevent_default();
                        return;
                    }
                }
            }
            if let Some(pointer_id) = view.preview_drag_pointer_id() {
                if let Some(touch) = touch_from_event(event, Some(pointer_id), false) {
                    let rect = canvas_for_touch_move.get_bounding_client_rect();
                    let local_x = touch.client_x() as f32 - rect.left() as f32;
                    let local_y = touch.client_y() as f32 - rect.top() as f32;
                    view.update_preview_click_movement(Some(pointer_id), local_x, local_y);
                    if view.update_preview_drag(
                        Some(pointer_id),
                        local_x,
                        local_y,
                        now_ms(),
                        &snapshot,
                        &rect,
                    ) {
                        view.request_render();
                        event.prevent_default();
                        return;
                    }
                }
            }
            let pinch_opt = { view.pinch_state.borrow_mut().take() };
                if let Some(mut pinch) = pinch_opt {
                    let touch_a = touch_from_event(event, Some(pinch.touch_a), false);
                    let touch_b = touch_from_event(event, Some(pinch.touch_b), false);
                    if let (Some(touch_a), Some(touch_b)) = (touch_a, touch_b) {
                        let center_x =
                            (touch_a.client_x() as f32 + touch_b.client_x() as f32) * 0.5;
                        let center_y =
                            (touch_a.client_y() as f32 + touch_b.client_y() as f32) * 0.5;
                        let dx = touch_b.client_x() as f32 - touch_a.client_x() as f32;
                        let dy = touch_b.client_y() as f32 - touch_a.client_y() as f32;
                        let distance = (dx * dx + dy * dy).sqrt();
                        if distance > 0.0 {
                            let factor = distance / pinch.last_distance;
                            if factor.is_finite() && factor > 0.0 {
                                if let Some((view_x, view_y)) = screen_to_view_coords(
                                    center_x,
                                    center_y,
                                    &canvas_for_touch_move,
                                    snapshot.view,
                                ) {
                                    core.zoom_view_at(factor, view_x, view_y);
                                }
                                pinch.last_distance = distance;
                            }
                        }
                        if let Ok(mut slot) = view.pinch_state.try_borrow_mut() {
                            *slot = Some(pinch);
                        }
                        event.prevent_default();
                        return;
                    }
                }
                if touch_count >= 2 {
                    if let (Some(touch_a), Some(touch_b)) =
                        (event.touches().item(0), event.touches().item(1))
                    {
                        let dx = touch_b.client_x() as f32 - touch_a.client_x() as f32;
                        let dy = touch_b.client_y() as f32 - touch_a.client_y() as f32;
                        let distance = (dx * dx + dy * dy).sqrt();
                        if distance > 0.0 {
                            *view.pinch_state.borrow_mut() = Some(PinchState {
                                touch_a: touch_a.identifier(),
                                touch_b: touch_b.identifier(),
                                last_distance: distance,
                            });
                        }
                    }
                    return;
                }
                let delta = {
                    let mut pan_state = view.pan_state.borrow_mut();
                    if let Some(pan) = pan_state.as_mut() {
                        if let Some(touch) = touch_from_event(event, pan.touch_id, false) {
                            let dx = touch.client_x() as f32 - pan.last_x;
                            let dy = touch.client_y() as f32 - pan.last_y;
                            pan.last_x = touch.client_x() as f32;
                            pan.last_y = touch.client_y() as f32;
                            screen_delta_to_world(dx, dy, &canvas_for_touch_move, snapshot.view)
                        } else {
                            pan_state.take();
                            None
                        }
                    } else {
                        None
                    }
                };
                if let Some((dx_world, dy_world)) = delta {
                    core.pan_view(dx_world, dy_world);
                    event.prevent_default();
                    return;
                }
                if snapshot.dragging_members.is_empty() {
                    return;
                }
                if snapshot.drag_touch_id.is_none() {
                    return;
                }
                let Some((view_x, view_y, _touch_id)) = touch_event_to_canvas_coords(
                    event,
                    &canvas_for_touch_move,
                    snapshot.view,
                    snapshot.drag_touch_id,
                    false,
                ) else {
                    return;
                };
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                if let Some(gate) = view.touch_drag_gate.borrow_mut().as_mut() {
                    if let Some(drag_id) = snapshot.drag_touch_id {
                        if gate.touch_id != drag_id {
                            gate.moved = true;
                        } else if !gate.moved {
                            let click_tolerance =
                                snapshot.piece_width.min(snapshot.piece_height) * CLICK_MOVE_RATIO;
                            let slop = screen_slop_to_puzzle(
                                TOUCH_DRAG_SLOP_PX,
                                &canvas_for_touch_move,
                                snapshot.view,
                                snapshot.layout.puzzle_scale,
                            );
                            let drag_tolerance = click_tolerance.max(slop);
                            let dx = px - gate.start_x;
                            let dy = py - gate.start_y;
                            if dx * dx + dy * dy < drag_tolerance * drag_tolerance {
                                event.prevent_default();
                                return;
                            }
                            gate.moved = true;
                        }
                    }
                }
                if let Some(touch) = touch_from_event(event, snapshot.drag_touch_id, false) {
                    view.update_auto_pan(
                        touch.client_x() as f32,
                        touch.client_y() as f32,
                        &snapshot,
                    );
                }
                view.dispatch_action(CoreAction::DragMove { x: px, y: py });
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let core = self.core.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &window,
            "touchend",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<TouchEvent>() else {
                    return;
                };
                let changed = event.changed_touches();
                let click_state = *view.preview_click.borrow();
                if let Some(click) = click_state {
                    if let Some(id) = click.pointer_id {
                        let mut ended = false;
                        let mut touch_opt = None;
                        for idx in 0..changed.length() {
                            if let Some(touch) = changed.item(idx) {
                                if touch.identifier() == id {
                                    ended = true;
                                    touch_opt = Some(touch);
                                    break;
                                }
                            }
                        }
                        if ended {
                            if let Some(touch) = touch_opt {
                                let rect = view.canvas.get_bounding_client_rect();
                                let local_x = touch.client_x() as f32 - rect.left() as f32;
                                let local_y = touch.client_y() as f32 - rect.top() as f32;
                                view.update_preview_click_movement(Some(id), local_x, local_y);
                            }
                            let preview_clicked = view.consume_preview_click(Some(id));
                            if preview_clicked {
                                view.toggle_preview_revealed();
                                view.request_render();
                                event.prevent_default();
                                return;
                            }
                        }
                    }
                }
                let pinch_state = *view.preview_drag.borrow();
                if let Some(PreviewDragState::Pinch { touch_a, touch_b, .. }) = pinch_state {
                    let changed = event.changed_touches();
                    let mut ended = false;
                    for idx in 0..changed.length() {
                        if let Some(touch) = changed.item(idx) {
                            let id = touch.identifier();
                            if id == touch_a || id == touch_b {
                                ended = true;
                                break;
                            }
                        }
                    }
                    if ended && view.end_preview_drag(None) {
                        view.request_render();
                        event.prevent_default();
                        return;
                    }
                }
                if let Some(pointer_id) = view.preview_drag_pointer_id() {
                    let mut ended = false;
                    for idx in 0..changed.length() {
                        if let Some(touch) = changed.item(idx) {
                            if touch.identifier() == pointer_id {
                                ended = true;
                                break;
                            }
                        }
                    }
                    if ended && view.end_preview_drag(Some(pointer_id)) {
                        view.request_render();
                        event.prevent_default();
                        return;
                    }
                }
                let snapshot = core.snapshot();
                view.touch_drag_gate.borrow_mut().take();
                view.stop_auto_pan();
                let mut cleared = false;
                if let Some(pinch) = view.pinch_state.borrow_mut().take() {
                    let touch_a = touch_from_event(event, Some(pinch.touch_a), false);
                    let touch_b = touch_from_event(event, Some(pinch.touch_b), false);
                    if touch_a.is_some() && touch_b.is_some() {
                        *view.pinch_state.borrow_mut() = Some(pinch);
                    } else {
                        cleared = true;
                    }
                }
                {
                    let mut pan_state = view.pan_state.borrow_mut();
                    if let Some(pan) = pan_state.as_ref() {
                        if touch_from_event(event, pan.touch_id, true).is_some() {
                            pan_state.take();
                            cleared = true;
                        }
                    }
                }
                if cleared {
                view.update_canvas_class(&snapshot);
                    core.settle_view();
                }
                if snapshot.drag_touch_id.is_some() {
                    if let Some(touch) = touch_from_event(event, None, true) {
                        view.dispatch_action(CoreAction::DragEnd {
                            touch_id: Some(touch.identifier()),
                        });
                    } else {
                        view.dispatch_action(CoreAction::DragEnd { touch_id: None });
                    }
                }
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let core = self.core.clone();
        let listener = EventListener::new_with_options(
            &window,
            "touchcancel",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |_event: &Event| {
                view.pan_state.borrow_mut().take();
                view.pinch_state.borrow_mut().take();
                view.touch_drag_gate.borrow_mut().take();
                view.preview_drag.borrow_mut().take();
                view.preview_click.borrow_mut().take();
                view.preview_velocity.set([0.0, 0.0]);
                view.stop_auto_pan();
                let snapshot = core.snapshot();
                view.update_canvas_class(&snapshot);
                core.settle_view();
                view.dispatch_action(CoreAction::DragEnd { touch_id: None });
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new(&window, "resize", move |_event: &Event| {
            view.update_viewport_size();
        });
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &window,
            "gesturestart",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                view.pinch_state.borrow_mut().take();
                view.pan_state.borrow_mut().take();
                event.prevent_default();
                event.stop_propagation();
            },
        );
        listeners.push(listener);

        let listener = EventListener::new_with_options(
            &window,
            "gesturechange",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                event.prevent_default();
                event.stop_propagation();
            },
        );
        listeners.push(listener);

        let listener = EventListener::new_with_options(
            &window,
            "gestureend",
            EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            },
            move |event: &Event| {
                event.prevent_default();
                event.stop_propagation();
            },
        );
        listeners.push(listener);

        *self.listeners.borrow_mut() = listeners;
    }

    fn hit_credit(&self, x: f32, y: f32) -> bool {
        self.ui_credit_hitbox
            .borrow()
            .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
            .unwrap_or(false)
    }

    fn set_hooks(&self, hooks: ViewHooks) {
        *self.hooks.borrow_mut() = Some(hooks);
    }

    fn dispatch_action(&self, action: CoreAction) {
        if let Some(hooks) = self.hooks.borrow().as_ref() {
            (hooks.on_action)(action);
        }
    }


    fn update_canvas_class(&self, snapshot: &AppSnapshot) {
        let mut canvas_class = "puzzle-canvas".to_string();
        let is_panning = self.pan_state.borrow().is_some();
        let preview_target = self.preview_hover.get();
        let preview_hovered = preview_target != PreviewHoverTarget::None
            || self.preview_drag.borrow().is_some();
        if is_panning {
            canvas_class.push_str(" panning");
        }
        if !snapshot.dragging_members.is_empty() {
            canvas_class.push_str(" dragging");
        } else if !is_panning {
            if snapshot.hovered_id.is_some() {
                canvas_class.push_str(" hover");
            } else if !self.ui_credit_hovered.get() && !preview_hovered {
                canvas_class.push_str(" pan-ready");
            }
        }
        if self.ui_credit_hovered.get() || preview_hovered {
            canvas_class.push_str(" ui-link-hover");
        }
        let drag_target = self.preview_drag.borrow().as_ref().map(|drag| match drag {
            PreviewDragState::Move { .. } => PreviewHoverTarget::Body,
            PreviewDragState::Resize { handle, .. } => PreviewHoverTarget::Resize(*handle),
            PreviewDragState::Pinch { .. } => PreviewHoverTarget::Resize(PreviewResizeHandle::BottomRight),
        });
        let cursor_target = drag_target.unwrap_or(preview_target);
        if let Some(cursor_class) = preview_cursor_class(cursor_target) {
            canvas_class.push_str(" ");
            canvas_class.push_str(cursor_class);
        }
        if matches!(self.preview_drag.borrow().as_ref().map(|drag| drag.kind()), Some(PreviewDragKind::Move)) {
            canvas_class.push_str(" preview-dragging");
        }
        self.canvas.set_class_name(&canvas_class);
    }

    fn update_preview_animation(&self, now_ms: f32) -> bool {
        let target_blur = self.preview_target_blur.get();
        let mut blur = self.preview_blur.get();
        if (blur - target_blur).abs() <= 0.01 {
            self.preview_blur.set(target_blur);
            self.preview_anim_last_ms.set(now_ms);
            return false;
        }
        let last = self.preview_anim_last_ms.get();
        let dt = if last > 0.0 {
            ((now_ms - last).max(0.0)) / 1000.0
        } else {
            0.0
        };
        self.preview_anim_last_ms.set(now_ms);
        let t = 1.0 - (-PREVIEW_ANIM_SPEED * dt).exp();
        if t.is_finite() && t > 0.0 {
            blur += (target_blur - blur) * t;
        }
        if (blur - target_blur).abs() <= 0.01 {
            blur = target_blur;
        }
        self.preview_blur.set(blur);
        (blur - target_blur).abs() > 0.01
    }

    fn set_preview_hover(&self, target: PreviewHoverTarget) -> bool {
        if self.preview_hover.get() == target {
            return false;
        }
        self.preview_hover.set(target);
        true
    }

    fn toggle_preview_revealed(self: &Rc<Self>) {
        let revealed = !self.preview_revealed.get();
        self.preview_revealed.set(revealed);
        let target_blur = if revealed { 0.0 } else { PREVIEW_HIDDEN_BLUR_PX };
        self.preview_target_blur.set(target_blur);
        self.preview_anim_last_ms.set(0.0);
        self.request_render();
    }

    fn arm_preview_click(&self, pointer_id: Option<i32>, local_x: f32, local_y: f32) {
        *self.preview_click.borrow_mut() = Some(PreviewClickState {
            pointer_id,
            start: [local_x, local_y],
            moved: false,
        });
    }

    fn clear_preview_click(&self) {
        self.preview_click.borrow_mut().take();
    }

    fn update_preview_click_movement(&self, pointer_id: Option<i32>, local_x: f32, local_y: f32) {
        let mut click_ref = self.preview_click.borrow_mut();
        let Some(click) = click_ref.as_mut() else {
            return;
        };
        if click.pointer_id != pointer_id {
            return;
        }
        let dx = local_x - click.start[0];
        let dy = local_y - click.start[1];
        if dx * dx + dy * dy > PREVIEW_CLICK_SLOP * PREVIEW_CLICK_SLOP {
            click.moved = true;
        }
    }

    fn consume_preview_click(&self, pointer_id: Option<i32>) -> bool {
        let mut click_ref = self.preview_click.borrow_mut();
        let Some(click) = click_ref.take() else {
            return false;
        };
        if click.pointer_id != pointer_id {
            *click_ref = Some(click);
            return false;
        }
        !click.moved
    }

    fn preview_aspect(info: &PuzzleInfo) -> f32 {
        if info.image_width > 0 {
            info.image_height as f32 / info.image_width as f32
        } else {
            1.0
        }
    }

    fn preview_frame_max_width(
        info: &PuzzleInfo,
        layout: WorkspaceLayout,
        viewport_w: f32,
        viewport_h: f32,
    ) -> Option<f32> {
        if info.image_width == 0 || info.image_height == 0 {
            return None;
        }
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return None;
        }
        let scale = layout.puzzle_scale.max(1.0e-4);
        let frame_w = info.image_width as f32 * scale;
        let frame_h = info.image_height as f32 * scale;
        if !frame_w.is_finite() || !frame_h.is_finite() || frame_w <= 0.0 || frame_h <= 0.0 {
            return None;
        }
        let target_w = layout.view_width.max(1.0) * (1.0 + PREVIEW_FIT_PADDING_RATIO);
        let target_h = layout.view_height.max(1.0) * (1.0 + PREVIEW_FIT_PADDING_RATIO);
        let fit_zoom = (viewport_w / target_w).min(viewport_h / target_h);
        if !fit_zoom.is_finite() || fit_zoom <= 0.0 {
            return None;
        }
        let width = frame_w * fit_zoom * PREVIEW_MAX_FIT_SCALE + PREVIEW_BOX_PADDING * 2.0;
        if width.is_finite() && width > 0.0 {
            Some(width)
        } else {
            None
        }
    }

    fn preview_width_bounds(
        &self,
        aspect: f32,
        viewport_w: f32,
        viewport_h: f32,
        frame_max_w: Option<f32>,
    ) -> (f32, f32) {
        let min_w = PREVIEW_BOX_MIN_WIDTH.max(1.0);
        let mut max_w = (viewport_w - PREVIEW_BOX_MARGIN * 2.0).max(min_w);
        if aspect > 0.0 {
            let max_h = (viewport_h - PREVIEW_BOX_MARGIN * 2.0).max(1.0);
            let max_w_from_h =
                (max_h - PREVIEW_BOX_PADDING * 2.0).max(1.0) / aspect + PREVIEW_BOX_PADDING * 2.0;
            if max_w_from_h.is_finite() {
                max_w = max_w.min(max_w_from_h);
            }
        }
        if let Some(frame_max_w) = frame_max_w {
            if frame_max_w.is_finite() && frame_max_w > 0.0 {
                max_w = max_w.min(frame_max_w);
            }
        }
        if max_w < min_w {
            max_w = min_w;
        }
        (min_w, max_w)
    }

    fn clamp_preview_width(
        &self,
        aspect: f32,
        viewport_w: f32,
        viewport_h: f32,
        frame_max_w: Option<f32>,
    ) -> f32 {
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let current = self.preview_width.get();
        let mut next = if current.is_finite() && current > 0.0 {
            current
        } else {
            PREVIEW_BOX_WIDTH
        };
        if next < min_w {
            next = min_w;
        } else if next > max_w {
            next = max_w;
        }
        if (next - current).abs() > f32::EPSILON {
            self.preview_width.set(next);
        }
        next
    }

    fn preview_box_metrics(width: f32, aspect: f32) -> (f32, f32, f32, f32) {
        let aspect = if aspect.is_finite() && aspect > 0.0 {
            aspect
        } else {
            1.0
        };
        let image_w = (width - PREVIEW_BOX_PADDING * 2.0).max(1.0);
        let image_h = (image_w * aspect).max(1.0);
        let box_h = image_h + PREVIEW_BOX_PADDING * 2.0;
        (width, box_h, image_w, image_h)
    }

    fn clamp_preview_position(
        &self,
        pos: [f32; 2],
        box_w: f32,
        box_h: f32,
        viewport_w: f32,
        viewport_h: f32,
    ) -> ([f32; 2], [bool; 2]) {
        let min_dim = box_w.min(box_h).max(1.0);
        let mut min_visible = (min_dim * PREVIEW_MIN_VISIBLE_FRAC).max(PREVIEW_MIN_VISIBLE);
        if min_visible > min_dim {
            min_visible = min_dim;
        }
        let min_x = -box_w + min_visible;
        let max_x = viewport_w - min_visible;
        let min_y = -box_h + min_visible;
        let max_y = viewport_h - min_visible;
        let mut x = pos[0];
        let mut y = pos[1];
        let mut hit_x = false;
        let mut hit_y = false;
        if x < min_x {
            x = min_x;
            hit_x = true;
        } else if x > max_x {
            x = max_x;
            hit_x = true;
        }
        if y < min_y {
            y = min_y;
            hit_y = true;
        } else if y > max_y {
            y = max_y;
            hit_y = true;
        }
        ([x, y], [hit_x, hit_y])
    }

    fn ensure_preview_seeded(
        &self,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
        aspect: f32,
    ) {
        if self.preview_seeded.get() {
            return;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return;
        }
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let base_dim = viewport_w.min(viewport_h).max(1.0);
        let mut box_w = (base_dim * PREVIEW_BOX_INIT_FRAC).max(PREVIEW_BOX_MIN_WIDTH);
        box_w = box_w.min(viewport_w * PREVIEW_BOX_MAX_FRAC).max(min_w).min(max_w);
        let (_, box_h, _, _) = Self::preview_box_metrics(box_w, aspect);
        let base_x = PREVIEW_BOX_MARGIN;
        let base_y = viewport_h - PREVIEW_BOX_MARGIN - box_h;
        self.preview_pos.set([base_x, base_y]);
        self.preview_width.set(box_w);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_seeded.set(true);
    }

    fn begin_preview_drag(
        &self,
        target: PreviewHoverTarget,
        local_x: f32,
        local_y: f32,
        pointer_id: Option<i32>,
    ) {
        let pos = self.preview_pos.get();
        let now = now_ms();
        let state = match target {
            PreviewHoverTarget::Resize(handle) => PreviewDragState::Resize {
                pointer_id,
                handle,
                start_pos: pos,
                start_width: self.preview_width.get(),
                last_ms: now,
            },
            PreviewHoverTarget::Body => PreviewDragState::Move {
                pointer_id,
                grab_offset: [local_x - pos[0], local_y - pos[1]],
                last_ms: now,
            },
            PreviewHoverTarget::None => return,
        };
        *self.preview_drag.borrow_mut() = Some(state);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now);
    }

    fn begin_preview_pinch(
        &self,
        touch_a: &Touch,
        touch_b: &Touch,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let pos = self.preview_pos.get();
        if box_w <= 0.0 || box_h <= 0.0 {
            return false;
        }
        let mid_x = (touch_a.client_x() as f32 + touch_b.client_x() as f32) * 0.5
            - canvas_rect.left() as f32;
        let mid_y = (touch_a.client_y() as f32 + touch_b.client_y() as f32) * 0.5
            - canvas_rect.top() as f32;
        let center = [pos[0] + box_w * 0.5, pos[1] + box_h * 0.5];
        let dx = mid_x - center[0];
        let dy = mid_y - center[1];
        let (ux, uy) = rotate_vec(dx, dy, -PREVIEW_TILT_DEG);
        let mut anchor_rel = [0.5 + ux / box_w, 0.5 + uy / box_h];
        anchor_rel[0] = anchor_rel[0].clamp(0.0, 1.0);
        anchor_rel[1] = anchor_rel[1].clamp(0.0, 1.0);
        let dx = touch_b.client_x() as f32 - touch_a.client_x() as f32;
        let dy = touch_b.client_y() as f32 - touch_a.client_y() as f32;
        let distance = (dx * dx + dy * dy).sqrt();
        if distance <= 0.0 || !distance.is_finite() {
            return false;
        }
        *self.preview_drag.borrow_mut() = Some(PreviewDragState::Pinch {
            touch_a: touch_a.identifier(),
            touch_b: touch_b.identifier(),
            start_distance: distance,
            start_width: width,
            anchor_rel,
        });
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now_ms());
        true
    }

    fn update_preview_pinch(
        &self,
        touch_a: &Touch,
        touch_b: &Touch,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
        now_ms: f32,
        start_distance: f32,
        start_width: f32,
        anchor_rel: [f32; 2],
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) =
            self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let dx = touch_b.client_x() as f32 - touch_a.client_x() as f32;
        let dy = touch_b.client_y() as f32 - touch_a.client_y() as f32;
        let distance = (dx * dx + dy * dy).sqrt();
        if start_distance <= 0.0 || !distance.is_finite() {
            return false;
        }
        let scale = (distance / start_distance).max(0.01);
        let mut width = start_width * scale;
        if !width.is_finite() {
            width = start_width;
        }
        width = width.clamp(min_w, max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mid_x = (touch_a.client_x() as f32 + touch_b.client_x() as f32) * 0.5
            - canvas_rect.left() as f32;
        let mid_y = (touch_a.client_y() as f32 + touch_b.client_y() as f32) * 0.5
            - canvas_rect.top() as f32;
        let half_w = box_w * 0.5;
        let half_h = box_h * 0.5;
        let vx = (anchor_rel[0] - 0.5) * box_w;
        let vy = (anchor_rel[1] - 0.5) * box_h;
        let (rvx, rvy) = rotate_vec(vx, vy, PREVIEW_TILT_DEG);
        let center_x = mid_x - rvx;
        let center_y = mid_y - rvy;
        let mut pos = [center_x - half_w, center_y - half_h];
        let (clamped, _) =
            self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        pos = clamped;
        self.preview_width.set(width);
        self.preview_pos.set(pos);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now_ms);
        true
    }

    fn update_preview_drag(
        &self,
        pointer_id: Option<i32>,
        local_x: f32,
        local_y: f32,
        now_ms: f32,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) =
            self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let mut width = self.preview_width.get().clamp(min_w, max_w);
        self.preview_width.set(width);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mut pos = self.preview_pos.get();
        let mut velocity = self.preview_velocity.get();
        let mut drag_ref = self.preview_drag.borrow_mut();
        let Some(drag) = drag_ref.as_mut() else {
            return false;
        };
        if drag.pointer_id() != pointer_id {
            return false;
        }
        match drag {
            PreviewDragState::Move {
                grab_offset,
                last_ms,
                ..
            } => {
                let half_w = box_w * 0.5;
                let half_h = box_h * 0.5;
                let vx = grab_offset[0] - half_w;
                let vy = grab_offset[1] - half_h;
                let (rvx, rvy) = rotate_vec(vx, vy, PREVIEW_TILT_DEG);
                let target_pos = [local_x - half_w - rvx, local_y - half_h - rvy];
                let (clamped, _) = self.clamp_preview_position(
                    target_pos,
                    box_w,
                    box_h,
                    viewport_w,
                    viewport_h,
                );
                let dt = ((now_ms - *last_ms).max(0.0).min(50.0)) / 1000.0;
                if dt > 0.0 {
                    let inst_vx = (clamped[0] - pos[0]) / dt;
                    let inst_vy = (clamped[1] - pos[1]) / dt;
                    velocity[0] += (inst_vx - velocity[0]) * PREVIEW_VELOCITY_SMOOTH;
                    velocity[1] += (inst_vy - velocity[1]) * PREVIEW_VELOCITY_SMOOTH;
                }
                pos = clamped;
                *last_ms = now_ms;
            }
            PreviewDragState::Resize {
                handle,
                start_pos,
                start_width,
                last_ms,
                ..
            } => {
                let center = [pos[0] + box_w * 0.5, pos[1] + box_h * 0.5];
                let [local_x, local_y] = self.preview_unrotate_point(center, local_x, local_y);
                let (start_box_w, start_box_h, _, _) =
                    Self::preview_box_metrics(*start_width, aspect);
                let left = start_pos[0];
                let top = start_pos[1];
                let right = left + start_box_w;
                let bottom = top + start_box_h;
                let center_x = left + start_box_w * 0.5;
                let center_y = top + start_box_h * 0.5;
                let width_from_height = |height: f32| -> f32 {
                    if aspect > 0.0 {
                        (height - PREVIEW_BOX_PADDING * 2.0) / aspect + PREVIEW_BOX_PADDING * 2.0
                    } else {
                        height
                    }
                };
                let mut width_x = None;
                let mut width_y = None;
                match handle {
                    PreviewResizeHandle::Left => {
                        width_x = Some(right - local_x);
                    }
                    PreviewResizeHandle::Right => {
                        width_x = Some(local_x - left);
                    }
                    PreviewResizeHandle::Top => {
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::Bottom => {
                        width_y = Some(width_from_height(local_y - top));
                    }
                    PreviewResizeHandle::TopLeft => {
                        width_x = Some(right - local_x);
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::TopRight => {
                        width_x = Some(local_x - left);
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::BottomLeft => {
                        width_x = Some(right - local_x);
                        width_y = Some(width_from_height(local_y - top));
                    }
                    PreviewResizeHandle::BottomRight => {
                        width_x = Some(local_x - left);
                        width_y = Some(width_from_height(local_y - top));
                    }
                }
                let mut candidate = match (width_x, width_y) {
                    (Some(wx), Some(wy)) => {
                        if (wx - *start_width).abs() >= (wy - *start_width).abs() {
                            wx
                        } else {
                            wy
                        }
                    }
                    (Some(wx), None) => wx,
                    (None, Some(wy)) => wy,
                    (None, None) => *start_width,
                };
                if !candidate.is_finite() {
                    candidate = *start_width;
                }
                width = candidate.clamp(min_w, max_w);
                self.preview_width.set(width);
                let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
                pos = match handle {
                    PreviewResizeHandle::Left => {
                        [right - box_w, center_y - box_h * 0.5]
                    }
                    PreviewResizeHandle::Right => {
                        [left, center_y - box_h * 0.5]
                    }
                    PreviewResizeHandle::Top => {
                        [center_x - box_w * 0.5, bottom - box_h]
                    }
                    PreviewResizeHandle::Bottom => {
                        [center_x - box_w * 0.5, top]
                    }
                    PreviewResizeHandle::TopLeft => [right - box_w, bottom - box_h],
                    PreviewResizeHandle::TopRight => [left, bottom - box_h],
                    PreviewResizeHandle::BottomLeft => [right - box_w, top],
                    PreviewResizeHandle::BottomRight => [left, top],
                };
                let (clamped, _) = self.clamp_preview_position(
                    pos,
                    box_w,
                    box_h,
                    viewport_w,
                    viewport_h,
                );
                pos = clamped;
                velocity = [0.0, 0.0];
                *last_ms = now_ms;
            }
            PreviewDragState::Pinch { .. } => {
                return false;
            }
        }
        self.preview_pos.set(pos);
        self.preview_velocity.set(velocity);
        self.preview_motion_last_ms.set(now_ms);
        true
    }

    fn preview_drag_pointer_id(&self) -> Option<i32> {
        self.preview_drag
            .borrow()
            .as_ref()
            .and_then(|drag| drag.pointer_id())
    }

    fn end_preview_drag(&self, pointer_id: Option<i32>) -> bool {
        let mut drag_ref = self.preview_drag.borrow_mut();
        let Some(drag) = drag_ref.as_ref() else {
            return false;
        };
        if let Some(id) = pointer_id {
            if drag.pointer_id() != Some(id) {
                return false;
            }
        }
        let was_resize = matches!(drag.kind(), PreviewDragKind::Resize);
        drag_ref.take();
        if was_resize {
            self.preview_velocity.set([0.0, 0.0]);
        }
        self.preview_motion_last_ms.set(now_ms());
        true
    }

    fn update_preview_motion(
        &self,
        now_ms: f32,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        if self.preview_drag.borrow().is_some() {
            self.preview_motion_last_ms.set(now_ms);
            return false;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mut velocity = self.preview_velocity.get();
        let speed_sq = velocity[0] * velocity[0] + velocity[1] * velocity[1];
        if speed_sq <= PREVIEW_VELOCITY_EPS * PREVIEW_VELOCITY_EPS {
            self.preview_velocity.set([0.0, 0.0]);
            self.preview_motion_last_ms.set(now_ms);
            return false;
        }
        let last = self.preview_motion_last_ms.get();
        let dt = if last > 0.0 {
            ((now_ms - last).max(0.0).min(50.0)) / 1000.0
        } else {
            0.0
        };
        self.preview_motion_last_ms.set(now_ms);
        if dt <= 0.0 {
            return true;
        }
        let mut pos = self.preview_pos.get();
        pos[0] += velocity[0] * dt;
        pos[1] += velocity[1] * dt;
        let decay = (-PREVIEW_MOMENTUM_DECAY * dt).exp();
        velocity[0] *= decay;
        velocity[1] *= decay;
        let (clamped, hit) =
            self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        if hit[0] {
            velocity[0] *= 0.2;
        }
        if hit[1] {
            velocity[1] *= 0.2;
        }
        pos = clamped;
        self.preview_pos.set(pos);
        self.preview_velocity.set(velocity);
        velocity[0].abs() > PREVIEW_VELOCITY_EPS || velocity[1].abs() > PREVIEW_VELOCITY_EPS
    }

    fn preview_layout(
        &self,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> Option<PreviewLayout> {
        let info = snapshot.puzzle_info.as_ref()?;
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return None;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, image_w, image_h) = Self::preview_box_metrics(width, aspect);
        let mut pos = self.preview_pos.get();
        let (clamped, _) =
            self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        if clamped != pos {
            self.preview_pos.set(clamped);
            pos = clamped;
        }
        let box_rect = Rect {
            x: pos[0],
            y: pos[1],
            w: box_w,
            h: box_h,
        };
        let image_rect = Rect {
            x: pos[0] + PREVIEW_BOX_PADDING,
            y: pos[1] + PREVIEW_BOX_PADDING,
            w: image_w,
            h: image_h,
        };
        Some(PreviewLayout {
            box_rect,
            image_rect,
        })
    }

    fn preview_unrotate_point(&self, center: [f32; 2], x: f32, y: f32) -> [f32; 2] {
        if PREVIEW_TILT_DEG.abs() <= f32::EPSILON {
            return [x, y];
        }
        let dx = x - center[0];
        let dy = y - center[1];
        let (rx, ry) = rotate_vec(dx, dy, -PREVIEW_TILT_DEG);
        [center[0] + rx, center[1] + ry]
    }

    fn preview_local_point(&self, layout: &PreviewLayout, x: f32, y: f32) -> [f32; 2] {
        self.preview_unrotate_point(layout.box_rect.center(), x, y)
    }

    fn preview_point_inside(&self, layout: &PreviewLayout, x: f32, y: f32) -> bool {
        let [ux, uy] = self.preview_local_point(layout, x, y);
        layout.box_rect.contains(ux, uy)
    }

    fn preview_hit_test(&self, layout: &PreviewLayout, x: f32, y: f32) -> PreviewHoverTarget {
        let [x, y] = self.preview_local_point(layout, x, y);
        if !layout.box_rect.contains(x, y) {
            return PreviewHoverTarget::None;
        }
        let border = PREVIEW_RESIZE_BORDER.max(1.0);
        let left = x - layout.box_rect.x <= border;
        let right = layout.box_rect.x + layout.box_rect.w - x <= border;
        let top = y - layout.box_rect.y <= border;
        let bottom = layout.box_rect.y + layout.box_rect.h - y <= border;
        if left || right || top || bottom {
            let handle = if left && top {
                PreviewResizeHandle::TopLeft
            } else if right && top {
                PreviewResizeHandle::TopRight
            } else if left && bottom {
                PreviewResizeHandle::BottomLeft
            } else if right && bottom {
                PreviewResizeHandle::BottomRight
            } else if left {
                PreviewResizeHandle::Left
            } else if right {
                PreviewResizeHandle::Right
            } else if top {
                PreviewResizeHandle::Top
            } else {
                PreviewResizeHandle::Bottom
            };
            return PreviewHoverTarget::Resize(handle);
        }
        PreviewHoverTarget::Body
    }

    fn build_preview_overlay_specs(
        &self,
        snapshot: &AppSnapshot,
        assets: &PuzzleAssets,
        _image: &HtmlImageElement,
        canvas_rect: &web_sys::DomRect,
        is_dark: bool,
    ) -> Vec<UiSpriteSpec> {
        let mut specs = Vec::new();
        let Some(layout) = self.preview_layout(snapshot, canvas_rect) else {
            return specs;
        };
        let view_rect = snapshot.view;
        let view_scale_x = if canvas_rect.width() > 0.0 {
            view_rect.width / canvas_rect.width() as f32
        } else {
            0.0
        };
        let view_scale_y = if canvas_rect.height() > 0.0 {
            view_rect.height / canvas_rect.height() as f32
        } else {
            0.0
        };
        let view_scale = if view_scale_x > 0.0 && view_scale_y > 0.0 {
            (view_scale_x + view_scale_y) * 0.5
        } else {
            0.0
        };
        let colors = preview_colors(is_dark);
        let panel_radius_px = PREVIEW_PANEL_RADIUS;
        let panel_border_px = PREVIEW_PANEL_BORDER;
        let panel_rect_view = rect_to_view(layout.box_rect, view_rect, canvas_rect);
        if panel_border_px > 0.0 {
            specs.push(make_ui_sprite(
                UiSpriteTexture::SolidWhite,
                panel_rect_view,
                PREVIEW_TILT_DEG,
                1.0,
                [0.0, 0.0],
                [1.0, 1.0],
                colors.panel_border,
                panel_radius_px * view_scale,
                [0.0, 0.0],
                0.0,
            ));
            let inner_rect = layout.box_rect.inset(panel_border_px);
            let inner_view = rect_to_view(inner_rect, view_rect, canvas_rect);
            let inner_radius = (panel_radius_px - panel_border_px).max(0.0) * view_scale;
            specs.push(make_ui_sprite(
                UiSpriteTexture::SolidWhite,
                inner_view,
                PREVIEW_TILT_DEG,
                1.0,
                [0.0, 0.0],
                [1.0, 1.0],
                colors.panel_bg,
                inner_radius,
                [0.0, 0.0],
                0.0,
            ));
        } else {
            specs.push(make_ui_sprite(
                UiSpriteTexture::SolidWhite,
                panel_rect_view,
                PREVIEW_TILT_DEG,
                1.0,
                [0.0, 0.0],
                [1.0, 1.0],
                colors.panel_bg,
                panel_radius_px * view_scale,
                [0.0, 0.0],
                0.0,
            ));
        }

        let puzzle_width = assets.piece_width * assets.grid.cols as f32;
        let puzzle_corner = preview_corner_radius(assets.piece_width, assets.piece_height);
        let image_scale = if puzzle_width > 0.0 {
            layout.image_rect.w / puzzle_width
        } else {
            0.0
        };
        let image_radius_px = puzzle_corner * image_scale;
        let image_rect_view = rect_to_view(layout.image_rect, view_rect, canvas_rect);
        let revealed = self.preview_revealed.get();
        let base_opacity = if revealed { 1.0 } else { PREVIEW_HIDDEN_OPACITY };
        let blur_norm = if PREVIEW_HIDDEN_BLUR_PX > 0.0 {
            (self.preview_blur.get() / PREVIEW_HIDDEN_BLUR_PX).clamp(0.0, 1.0)
        } else {
            0.0
        };
        if blur_norm > 0.0 {
            specs.push(make_ui_sprite(
                UiSpriteTexture::PreviewBlur,
                image_rect_view,
                PREVIEW_TILT_DEG,
                base_opacity * blur_norm,
                [0.0, 0.0],
                [1.0, 1.0],
                [1.0, 1.0, 1.0, 1.0],
                image_radius_px * view_scale,
                [0.0, 0.0],
                0.2 * blur_norm,
            ));
        }
        if blur_norm < 1.0 {
            specs.push(make_ui_sprite(
                UiSpriteTexture::Preview,
                image_rect_view,
                PREVIEW_TILT_DEG,
                base_opacity * (1.0 - blur_norm),
                [0.0, 0.0],
                [1.0, 1.0],
                [1.0, 1.0, 1.0, 1.0],
                image_radius_px * view_scale,
                [0.0, 0.0],
                0.0,
            ));
        }

        specs
    }

    fn update_debug_overlay(&self, snapshot: &AppSnapshot) {
        let overlay_ref = self.debug_overlay.borrow();
        let Some(overlay) = overlay_ref.as_ref() else {
            return;
        };
        if !snapshot.app_settings.show_debug {
            overlay.set_visible(false);
            return;
        }
        let Some(zones) = self.compute_auto_pan_zones(snapshot) else {
            overlay.set_visible(false);
            return;
        };
        overlay.set_visible(true);
        overlay.set_insets(zones.outer_inset, zones.inner_inset);
    }

    fn compute_auto_pan_zones(&self, snapshot: &AppSnapshot) -> Option<AutoPanZones> {
        let rect = self.canvas.get_bounding_client_rect();
        let width = rect.width() as f32;
        let height = rect.height() as f32;
        if width <= 0.0 || height <= 0.0 {
            return None;
        }
        let min_dim = width.min(height);
        let max_inset = min_dim * 0.45;
        let outer_ratio = snapshot.view_settings.auto_pan_outer_ratio.max(0.0);
        let inner_ratio = snapshot.view_settings.auto_pan_inner_ratio.max(outer_ratio);
        let outer_inset = (outer_ratio * min_dim).min(max_inset);
        let inner_inset = (inner_ratio * min_dim).min(max_inset).max(outer_inset);
        let left = rect.left() as f32;
        let top = rect.top() as f32;
        let right = left + width;
        let bottom = top + height;
        let outer = AutoPanRect {
            left: left + outer_inset,
            right: right - outer_inset,
            top: top + outer_inset,
            bottom: bottom - outer_inset,
        };
        let inner = AutoPanRect {
            left: left + inner_inset,
            right: right - inner_inset,
            top: top + inner_inset,
            bottom: bottom - inner_inset,
        };
        Some(AutoPanZones {
            outer,
            inner,
            outer_inset,
            inner_inset,
        })
    }

    fn update_auto_pan(self: &Rc<Self>, screen_x: f32, screen_y: f32, snapshot: &AppSnapshot) {
        let zones = self.compute_auto_pan_zones(snapshot);
        let mut should_start = false;
        let mut should_stop = false;
        {
            let mut state = self.auto_pan.borrow_mut();
            state.has_pointer = true;
            state.last_screen_x = screen_x;
            state.last_screen_y = screen_y;
            if let Some(zones) = zones.as_ref() {
                let inside_outer = zones.outer.contains(screen_x, screen_y);
                let inside_inner = zones.inner.contains(screen_x, screen_y);
                if inside_inner {
                    state.seen_inner = true;
                }
                if state.active {
                    if inside_inner {
                        state.active = false;
                        state.last_tick_ms = None;
                        should_stop = true;
                    }
                } else if state.seen_inner && !inside_outer {
                    state.active = true;
                    state.last_tick_ms = None;
                    should_start = true;
                }
            } else if state.active {
                state.active = false;
                state.last_tick_ms = None;
                should_stop = true;
            }
        }
        if should_start {
            self.ensure_auto_pan_frame();
        } else if should_stop {
            self.clear_auto_pan_frame();
        }
    }

    fn clear_auto_pan_frame(&self) {
        self.auto_pan.borrow_mut().frame.take();
    }

    fn stop_auto_pan(&self) {
        let mut state = self.auto_pan.borrow_mut();
        state.active = false;
        state.seen_inner = false;
        state.has_pointer = false;
        state.last_tick_ms = None;
        state.frame.take();
    }

    fn ensure_auto_pan_frame(self: &Rc<Self>) {
        let needs_frame = self.auto_pan.borrow().frame.is_none();
        if !needs_frame {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |timestamp| {
            view.auto_pan_frame(timestamp);
        });
        self.auto_pan.borrow_mut().frame = Some(handle);
    }

    fn auto_pan_frame(self: &Rc<Self>, timestamp: f64) {
        let (active, seen_inner, has_pointer, screen_x, screen_y, dt_sec) = {
            let mut state = self.auto_pan.borrow_mut();
            state.frame.take();
            let dt_ms = match state.last_tick_ms {
                Some(prev) => (timestamp - prev).max(0.0),
                None => 0.0,
            };
            state.last_tick_ms = Some(timestamp);
            (
                state.active,
                state.seen_inner,
                state.has_pointer,
                state.last_screen_x,
                state.last_screen_y,
                (dt_ms.min(50.0) / 1000.0) as f32,
            )
        };
        if !active || !seen_inner || !has_pointer {
            return;
        }
        let snapshot = self.core.snapshot();
        if snapshot.dragging_members.is_empty() {
            self.stop_auto_pan();
            return;
        }
        let Some(zones) = self.compute_auto_pan_zones(&snapshot) else {
            self.stop_auto_pan();
            return;
        };
        let (dir_x, t_x) =
            edge_intensity(screen_x, zones.inner.left, zones.inner.right, zones.inner_inset);
        let (dir_y, t_y) =
            edge_intensity(screen_y, zones.inner.top, zones.inner.bottom, zones.inner_inset);
        let ease_x = smoothstep(t_x);
        let ease_y = smoothstep(t_y);
        let mut speed_ratio = snapshot.view_settings.auto_pan_speed_ratio.max(0.0);
        if let Some((view_x, view_y)) =
            screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
        {
            speed_ratio *= workspace_fade_scale(view_x, view_y, snapshot.layout, snapshot.view);
        }
        let dx_world = dir_x * ease_x * snapshot.view.width * speed_ratio * dt_sec;
        let dy_world = dir_y * ease_y * snapshot.view.height * speed_ratio * dt_sec;
        if dx_world != 0.0 || dy_world != 0.0 {
            self.core.pan_view(dx_world, dy_world);
            let snapshot = self.core.snapshot();
            if snapshot.dragging_members.is_empty() {
                self.stop_auto_pan();
                return;
            }
            if let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
            {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.dispatch_action(CoreAction::DragMove { x: px, y: py });
            }
        }
        if self.auto_pan.borrow().active {
            self.ensure_auto_pan_frame();
        }
    }

    fn pick_piece(&self, x: f32, y: f32, snapshot: &AppSnapshot) -> Option<usize> {
        let assets = self.core.assets()?;
        let mask_atlas = self.mask_atlas.borrow();
        let mask_atlas = mask_atlas.as_ref()?;
        pick_piece_at(
            x,
            y,
            &snapshot.core.positions,
            &snapshot.core.rotations,
            &snapshot.core.flips,
            &snapshot.z_order,
            mask_atlas,
            assets.grid.cols as usize,
            assets.piece_width,
            assets.piece_height,
            assets.mask_pad,
        )
    }

    fn request_render(self: &Rc<Self>) {
        self.queue_render_snapshot(self.core.snapshot());
    }

    fn queue_render_snapshot(self: &Rc<Self>, snapshot: AppSnapshot) {
        *self.pending_snapshot.borrow_mut() = Some(snapshot);
        self.idle_fps_rendered.set(false);
        self.schedule_render();
    }

    fn schedule_render(self: &Rc<Self>) {
        let now = now_ms();
        let min_interval = (1000.0 / WGPU_FPS_CAP_DEFAULT).max(1.0);
        let mut elapsed = now - self.last_render_ms.get();
        if elapsed < 0.0 {
            self.last_render_ms.set(now);
            elapsed = min_interval;
        }
        if elapsed >= min_interval {
            self.render_timer.borrow_mut().take();
            self.perform_render(true);
            return;
        }
        if self.render_timer.borrow().is_some() {
            return;
        }
        let delay_ms = (min_interval - elapsed).max(0.0).ceil() as u32;
        let view = Rc::clone(self);
        *self.render_timer.borrow_mut() = Some(Timeout::new(delay_ms, move || {
            view.render_timer.borrow_mut().take();
            view.perform_render(true);
        }));
    }

    fn perform_render(self: &Rc<Self>, schedule_idle: bool) {
        let snapshot = self
            .pending_snapshot
            .borrow_mut()
            .take()
            .unwrap_or_else(|| self.core.snapshot());
        let show_fps = self.wgpu_settings.borrow().show_fps;
        let sync_view = sync_runtime::sync_view();
        let now = now_ms();
        self.last_render_ms.set(now);
        let animating_preview = self.update_preview_animation(now);
        self.force_fps_fallback.set(!schedule_idle && show_fps);
        self.render_snapshot(&snapshot, &sync_view);
        if schedule_idle {
            self.schedule_idle_fps_reset(show_fps);
        }
        if animating_preview {
            self.request_render();
        }
    }

    fn schedule_idle_fps_reset(self: &Rc<Self>, show_fps: bool) {
        self.idle_fps_timer.borrow_mut().take();
        if !show_fps {
            return;
        }
        let view = Rc::clone(self);
        let scheduled_at = self.last_render_ms.get();
        *self.idle_fps_timer.borrow_mut() =
            Some(Timeout::new(WGPU_FPS_IDLE_RESET_MS as u32, move || {
                if view.last_render_ms.get() != scheduled_at {
                    return;
                }
                if view.idle_fps_rendered.get() {
                    return;
                }
                view.idle_fps_rendered.set(true);
                view.perform_render(false);
            }));
    }

    fn render_snapshot(self: &Rc<Self>, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let force_fps_fallback = self.force_fps_fallback.get();
        if force_fps_fallback {
            self.force_fps_fallback.set(false);
        }
        self.ensure_puzzle_image(snapshot);
        let disconnected =
            matches!(sync_view.mode(), InitMode::Online) && !sync_view.connected();
        if disconnected {
            self.root.set_class_name("app wgpu sync-disconnected");
        } else {
            self.root.set_class_name("app wgpu");
        }
        let assets = match self.core.assets() {
            Some(assets) => assets,
            None => return,
        };
        let image = match self.image.borrow().clone() {
            Some(image) => image,
            None => return,
        };
        if self.mask_atlas.borrow().is_none() {
            let mask_atlas_data = match build_mask_atlas(
                &assets.pieces,
                &assets.paths,
                assets.piece_width,
                assets.piece_height,
                assets.grid,
                assets.mask_pad,
            ) {
                Ok(atlas) => Rc::new(atlas),
                Err(err) => {
                    web_sys::console::error_1(&err);
                    return;
                }
            };
            *self.mask_atlas.borrow_mut() = Some(mask_atlas_data);
        }
        let layout = snapshot.layout;
        let view_rect = snapshot.view;
        self.update_canvas_class(snapshot);
        if snapshot.dragging_members.is_empty() {
            self.stop_auto_pan();
        }
        self.update_debug_overlay(snapshot);
        let rect = self.canvas.get_bounding_client_rect();
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return;
        }
        let animating_preview_motion = self.update_preview_motion(now_ms(), snapshot, &rect);
        let dpr = web_sys::window()
            .map(|window| window.device_pixel_ratio())
            .unwrap_or(1.0) as f32;
        let render_scale = self
            .wgpu_settings
            .borrow()
            .render_scale
            .max(WGPU_RENDER_SCALE_MIN);
        let render_scale_px = render_scale * dpr;
        let mut pixel_w = (viewport_w * render_scale_px).max(1.0).ceil() as u32;
        let mut pixel_h = (viewport_h * render_scale_px).max(1.0).ceil() as u32;
        if pixel_w > WGPU_CANVAS_MAX_PX || pixel_h > WGPU_CANVAS_MAX_PX {
            pixel_w = pixel_w.min(WGPU_CANVAS_MAX_PX);
            pixel_h = pixel_h.min(WGPU_CANVAS_MAX_PX);
        }
        let last_size = self.last_view_size.get();
        if last_size != (pixel_w, pixel_h) {
            self.last_view_size.set((pixel_w, pixel_h));
            if let Some(renderer) = self.renderer.borrow_mut().as_mut() {
                renderer.resize(pixel_w, pixel_h);
            }
        }
        let is_dark = is_dark_theme(snapshot.app_settings.theme_mode);
        let (connections_label, border_connections_label) = if let Some(info) = snapshot.puzzle_info.as_ref() {
            let cols = info.cols as usize;
            let rows = info.rows as usize;
            let total = cols * rows;
            if snapshot.core.connections.len() == total {
                let (connected, border_connected, total_expected, border_expected) =
                    count_connections(&snapshot.core.connections, cols, rows);
                (
                    format_progress(connected, total_expected),
                    format_progress(border_connected, border_expected),
                )
            } else {
                ("--".to_string(), "--".to_string())
            }
        } else {
            ("--".to_string(), "--".to_string())
        };
        let backend_label = if snapshot.app_settings.show_debug {
            let existing = self.backend_label.borrow().clone();
            if existing.is_some() {
                existing
            } else {
                let label = self
                    .renderer
                    .borrow()
                    .as_ref()
                    .map(|renderer| renderer.backend_label().to_string());
                if let Some(ref label) = label {
                    *self.backend_label.borrow_mut() = Some(label.clone());
                }
                label
            }
        } else {
            None
        };
        let mut measure_state = self.ui_measure.borrow_mut();
        let ui_specs = build_ui_specs(
            &mut measure_state,
            layout,
            assets.info.image_width as f32,
            assets.info.image_height as f32,
            UI_TITLE_TEXT,
            snapshot.solved,
            connections_label.as_str(),
            border_connections_label.as_str(),
            snapshot.app_settings.show_debug,
            backend_label.as_deref(),
            false,
            is_dark,
        );
        let credit_hitbox = ui_specs
            .iter()
            .find(|spec| matches!(spec.id, UiTextId::Credit))
            .map(|spec| ui_hitbox_for_spec(&mut measure_state, spec));
        *self.ui_credit_hitbox.borrow_mut() = credit_hitbox;
        if snapshot.puzzle_info.is_none() {
            if self.set_preview_hover(PreviewHoverTarget::None) {
                self.request_render();
                self.update_canvas_class(snapshot);
            }
        }
        let preview_specs =
            self.build_preview_overlay_specs(snapshot, &assets, &image, &rect, is_dark);
        if animating_preview_motion {
            self.request_render();
        }

        let mask_atlas = self.mask_atlas.borrow();
        let mask_atlas = match mask_atlas.as_ref() {
            Some(mask_atlas) => mask_atlas,
            None => return,
        };
        let highlight_members = if snapshot.dragging_members.is_empty() {
            None
        } else {
            Some(snapshot.dragging_members.as_slice())
        };
        let drag_origin = snapshot.drag_cursor;
        let drag_dir = if highlight_members.is_some() {
            if snapshot.drag_right_click { -1.0 } else { 1.0 }
        } else {
            0.0
        };
        let instances = build_wgpu_instances(
            &snapshot.core.positions,
            &snapshot.core.rotations,
            &snapshot.core.flips,
            &snapshot.z_order,
            &snapshot.core.connections,
            snapshot.hovered_id,
            snapshot.app_settings.show_debug,
            assets.grid.cols as usize,
            assets.grid.rows as usize,
            assets.piece_width,
            assets.piece_height,
            mask_atlas,
            highlight_members,
            drag_origin,
            drag_dir,
        );
        if let Some(renderer) = self.renderer.borrow_mut().as_mut() {
            if force_fps_fallback {
                renderer.force_fps_fallback();
            }
            renderer.set_view(
                view_rect.min_x,
                view_rect.min_y,
                view_rect.width,
                view_rect.height,
                layout.puzzle_scale,
            );
            renderer.set_workspace_rect(
                layout.view_min_x,
                layout.view_min_y,
                layout.view_width,
                layout.view_height,
            );
            let settings = self.wgpu_settings.borrow().clone();
            renderer.set_edge_aa(settings.edge_aa);
            renderer.set_show_fps(settings.show_fps);
            renderer.set_solved(snapshot.solved);
            renderer.update_instances(instances);
            renderer.set_ui_texts(&ui_specs);
            renderer.set_ui_overlay_sprites(&preview_specs);
            renderer.render();
            return;
        }
        if self.creating.get() {
            *self.pending_instances.borrow_mut() = Some(instances);
            *self.pending_ui.borrow_mut() = Some(ui_specs);
            *self.pending_ui_overlay.borrow_mut() = Some(preview_specs);
            return;
        }
        self.creating.set(true);
        let canvas = self.canvas.clone();
        let image = image.clone();
        let pieces = assets.pieces.clone();
        let paths = assets.paths.clone();
        let grid = assets.grid;
        let piece_width = assets.piece_width;
        let piece_height = assets.piece_height;
        let view_min_x = view_rect.min_x;
        let view_min_y = view_rect.min_y;
        let view_width = view_rect.width;
        let view_height = view_rect.height;
        let workspace_min_x = layout.view_min_x;
        let workspace_min_y = layout.view_min_y;
        let workspace_width = layout.view_width;
        let workspace_height = layout.view_height;
        let puzzle_scale = layout.puzzle_scale;
        let mask_atlas_data = match self.mask_atlas.borrow().clone() {
            Some(atlas) => atlas,
            None => {
                self.creating.set(false);
                return;
            }
        };
        let mask_pad = assets.mask_pad;
        let settings = self.wgpu_settings.borrow().clone();
        let render_scale = settings.render_scale;
        let edge_aa = settings.edge_aa;
        let show_fps = settings.show_fps;
        let solved = snapshot.solved;
        let is_dark_theme = is_dark;
        let epoch = self.puzzle_epoch.get();
        let view = Rc::clone(self);
        spawn_local(async move {
            let mut refresh_debug = false;
            match WgpuRenderer::new(
                canvas,
                image,
                pieces,
                paths,
                grid,
                piece_width,
                piece_height,
                view_min_x,
                view_min_y,
                view_width,
                view_height,
                workspace_min_x,
                workspace_min_y,
                workspace_width,
                workspace_height,
                puzzle_scale,
                mask_atlas_data,
                mask_pad,
                render_scale,
                viewport_w,
                viewport_h,
                is_dark_theme,
            )
            .await
            {
                Ok(mut renderer) => {
                    if view.puzzle_epoch.get() != epoch {
                        view.creating.set(false);
                        return;
                    }
                    if force_fps_fallback {
                        renderer.force_fps_fallback();
                    }
                    let backend_label = renderer.backend_label().to_string();
                    *view.backend_label.borrow_mut() = Some(backend_label);
                    refresh_debug = view.core.snapshot().app_settings.show_debug;
                    renderer.set_emboss_enabled(true);
                    renderer.set_font_bytes(FPS_FONT_BYTES.to_vec());
                    renderer.set_ui_font_bytes(FPS_FONT_BYTES.to_vec());
                    renderer.set_edge_aa(edge_aa);
                    renderer.set_show_fps(show_fps);
                    renderer.set_solved(solved);
                    if let Some(instances) = view.pending_instances.borrow_mut().take() {
                        renderer.update_instances(instances);
                    }
                    if let Some(specs) = view.pending_ui.borrow_mut().take() {
                        renderer.set_ui_texts(&specs);
                    }
                    if let Some(specs) = view.pending_ui_overlay.borrow_mut().take() {
                        renderer.set_ui_overlay_sprites(&specs);
                    }
                    renderer.render();
                    *view.renderer.borrow_mut() = Some(renderer);
                }
                Err(err) => {
                    web_sys::console::error_1(&err);
                }
            }
            view.creating.set(false);
            if refresh_debug {
                view.request_render();
            }
        });
        *self.pending_instances.borrow_mut() = Some(instances);
        *self.pending_ui.borrow_mut() = Some(ui_specs);
        *self.pending_ui_overlay.borrow_mut() = Some(preview_specs);
    }
}

pub(crate) struct WgpuViewAdapter {
    view: Rc<WgpuView>,
    _hooks: Option<ViewHooks>,
}

impl WgpuViewAdapter {
    fn new(view: Rc<WgpuView>) -> Self {
        Self { view, _hooks: None }
    }
}

impl GameView for WgpuViewAdapter {
    fn init(&mut self, hooks: ViewHooks) {
        self.view.set_hooks(hooks.clone());
        self._hooks = Some(hooks);
    }

    fn render(&mut self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let _ = sync_view;
        self.view.queue_render_snapshot(snapshot.clone());
    }

    fn shutdown(&mut self) {
        view_runtime::set_wgpu_settings_hook(None);
    }
}

fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
}

fn smoothstep(t: f32) -> f32 {
    let t = t.clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

fn edge_intensity(pos: f32, inner_min: f32, inner_max: f32, inner_inset: f32) -> (f32, f32) {
    if inner_inset <= 0.0 {
        return (0.0, 0.0);
    }
    if pos < inner_min {
        let dist = (inner_min - pos).min(inner_inset);
        (-1.0, dist / inner_inset)
    } else if pos > inner_max {
        let dist = (pos - inner_max).min(inner_inset);
        (1.0, dist / inner_inset)
    } else {
        (0.0, 0.0)
    }
}

fn workspace_fade_scale(x: f32, y: f32, layout: WorkspaceLayout, view: ViewRect) -> f32 {
    let min_x = layout.view_min_x;
    let min_y = layout.view_min_y;
    let max_x = min_x + layout.view_width;
    let max_y = min_y + layout.view_height;
    let dx = if x < min_x {
        min_x - x
    } else if x > max_x {
        x - max_x
    } else {
        0.0
    };
    let dy = if y < min_y {
        min_y - y
    } else if y > max_y {
        y - max_y
    } else {
        0.0
    };
    let dist = dx.max(dy);
    if dist <= 0.0 {
        return 1.0;
    }
    let falloff = view.width.max(view.height) * 0.2;
    if falloff <= 0.0 {
        return 0.0;
    }
    let t = (dist / falloff).clamp(0.0, 1.0);
    1.0 - smoothstep(t)
}

fn screen_to_view_coords(
    screen_x: f32,
    screen_y: f32,
    canvas: &HtmlCanvasElement,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = canvas.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let rect_left = rect.left() as f32;
    let rect_top = rect.top() as f32;
    let x = view.min_x + (screen_x - rect_left) * view.width / rect_width;
    let y = view.min_y + (screen_y - rect_top) * view.height / rect_height;
    Some((x, y))
}

fn screen_delta_to_world(
    dx_screen: f32,
    dy_screen: f32,
    canvas: &HtmlCanvasElement,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = canvas.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let scale_x = view.width / rect_width;
    let scale_y = view.height / rect_height;
    Some((-dx_screen * scale_x, -dy_screen * scale_y))
}

fn screen_slop_to_puzzle(
    slop_px: f32,
    canvas: &HtmlCanvasElement,
    view: ViewRect,
    puzzle_scale: f32,
) -> f32 {
    let rect = canvas.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return 0.0;
    }
    let scale_x = view.width / rect_width;
    let scale_y = view.height / rect_height;
    let slop_view = slop_px * scale_x.max(scale_y);
    let puzzle_scale = puzzle_scale.max(1.0e-4);
    slop_view / puzzle_scale
}

fn event_to_canvas_coords(
    event: &MouseEvent,
    canvas: &HtmlCanvasElement,
    view: ViewRect,
) -> Option<(f32, f32)> {
    screen_to_view_coords(
        event.client_x() as f32,
        event.client_y() as f32,
        canvas,
        view,
    )
}

fn touch_from_event(event: &TouchEvent, touch_id: Option<i32>, use_changed: bool) -> Option<Touch> {
    let list = if use_changed { event.changed_touches() } else { event.touches() };
    if let Some(id) = touch_id {
        for idx in 0..list.length() {
            if let Some(touch) = list.item(idx) {
                if touch.identifier() == id {
                    return Some(touch);
                }
            }
        }
        None
    } else {
        list.item(0)
    }
}

fn touch_event_to_canvas_coords(
    event: &TouchEvent,
    canvas: &HtmlCanvasElement,
    view: ViewRect,
    touch_id: Option<i32>,
    use_changed: bool,
) -> Option<(f32, f32, i32)> {
    let touch = touch_from_event(event, touch_id, use_changed)?;
    let (x, y) = screen_to_view_coords(
        touch.client_x() as f32,
        touch.client_y() as f32,
        canvas,
        view,
    )?;
    Some((x, y, touch.identifier()))
}

fn workspace_to_puzzle_coords(scale: f32, x: f32, y: f32) -> (f32, f32) {
    let scale = scale.max(1.0e-4);
    (x / scale, y / scale)
}

fn drag_angle_for_group(count: usize) -> f32 {
    let denom = (count.max(1) as f32).sqrt();
    DRAG_ROTATION_DEG / denom
}

fn drag_group_center(
    positions: &[(f32, f32)],
    members: &[usize],
    piece_width: f32,
    piece_height: f32,
) -> Option<(f32, f32)> {
    if members.is_empty() {
        return None;
    }
    let mut sum_x = 0.0;
    let mut sum_y = 0.0;
    let mut count = 0.0;
    for id in members {
        if let Some(pos) = positions.get(*id) {
            sum_x += pos.0 + piece_width * 0.5;
            sum_y += pos.1 + piece_height * 0.5;
            count += 1.0;
        }
    }
    if count > 0.0 {
        Some((sum_x / count, sum_y / count))
    } else {
        None
    }
}

fn drag_group_position(
    pos: (f32, f32),
    center: (f32, f32),
    scale: f32,
    rotation_deg: f32,
    piece_width: f32,
    piece_height: f32,
) -> (f32, f32) {
    let piece_center = (pos.0 + piece_width * 0.5, pos.1 + piece_height * 0.5);
    let mut dx = piece_center.0 - center.0;
    let mut dy = piece_center.1 - center.1;
    dx *= scale;
    dy *= scale;
    let (rx, ry) = rotate_vec(dx, dy, rotation_deg);
    let new_center = (center.0 + rx, center.1 + ry);
    (
        new_center.0 - piece_width * 0.5,
        new_center.1 - piece_height * 0.5,
    )
}

fn build_wgpu_instances(
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    connections: &[[bool; 4]],
    hovered_id: Option<usize>,
    show_debug: bool,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    mask_atlas: &MaskAtlasData,
    highlighted_members: Option<&[usize]>,
    drag_origin: Option<(f32, f32)>,
    drag_dir: f32,
) -> InstanceSet {
    let total = cols * rows;
    if total == 0 {
        return InstanceSet {
            instances: Vec::new(),
            batches: Vec::new(),
        };
    }
    let fallback_order = if z_order.len() == total {
        None
    } else {
        Some((0..total).collect::<Vec<_>>())
    };
    let order = match fallback_order.as_deref() {
        Some(slice) => slice,
        None => z_order,
    };
    let mut hovered_mask = vec![false; total];
    if show_debug {
        hovered_mask.fill(true);
    } else if let Some(members) = highlighted_members {
        for member in members {
            if *member < hovered_mask.len() {
                hovered_mask[*member] = true;
            }
        }
    } else if let Some(id) = hovered_id {
        if id < total && id < connections.len() {
            for member in collect_group(connections, id, cols, rows) {
                if member < hovered_mask.len() {
                    hovered_mask[member] = true;
                }
            }
        }
    }
    let mut drag_mask = vec![false; total];
    if let Some(members) = highlighted_members {
        for member in members {
            if *member < drag_mask.len() {
                drag_mask[*member] = true;
            }
        }
    }
    let drag_active = drag_dir.abs() > f32::EPSILON && drag_mask.iter().any(|val| *val);
    let drag_count = highlighted_members.map(|members| members.len()).unwrap_or(0);
    let drag_rotation = if drag_active {
        drag_angle_for_group(drag_count) * drag_dir.signum()
    } else {
        0.0
    };
    let drag_scale = if drag_active { DRAG_SCALE } else { 1.0 };
    let drag_center = if drag_active {
        drag_origin.or_else(|| {
            highlighted_members
                .and_then(|members| drag_group_center(positions, members, piece_width, piece_height))
        })
    } else {
        None
    };
    let mut group_id = vec![usize::MAX; total];
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut queue = Vec::new();
    for start in 0..total {
        if group_id[start] != usize::MAX {
            continue;
        }
        let gid = groups.len();
        let mut members = Vec::new();
        group_id[start] = gid;
        queue.push(start);
        while let Some(id) = queue.pop() {
            members.push(id);
            for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
                if connections
                    .get(id)
                    .map(|edges| edges[dir])
                    .unwrap_or(false)
                {
                    if let Some(neighbor) = neighbor_id(id, cols, rows, dir) {
                        if group_id[neighbor] == usize::MAX {
                            group_id[neighbor] = gid;
                            queue.push(neighbor);
                        }
                    }
                }
            }
        }
        groups.push(members);
    }
    let mut group_members: Vec<Vec<usize>> = vec![Vec::new(); groups.len()];
    let mut group_order = Vec::new();
    let mut group_seen = vec![false; groups.len()];
    for &id in order {
        let gid = group_id[id];
        group_members[gid].push(id);
        if !group_seen[gid] {
            group_seen[gid] = true;
            group_order.push(gid);
        }
    }
    let mut group_has_hover = vec![false; groups.len()];
    for (gid, members) in group_members.iter().enumerate() {
        if members.iter().any(|id| hovered_mask[*id]) {
            group_has_hover[gid] = true;
        }
    }
    let mut instances = Vec::with_capacity(order.len());
    let mut batches: Vec<InstanceBatch> = Vec::new();
    for gid in group_order {
        let members = &group_members[gid];
        if members.is_empty() {
            continue;
        }
        let start = instances.len() as u32;
        for &id in members {
            let col = id % cols;
            let row = id / cols;
            let base_x = col as f32 * piece_width;
            let base_y = row as f32 * piece_height;
            let pos = positions.get(id).copied().unwrap_or((base_x, base_y));
            let render_pos = if drag_mask.get(id).copied().unwrap_or(false) {
                if let Some(center) = drag_center {
                    drag_group_position(pos, center, drag_scale, drag_rotation, piece_width, piece_height)
                } else {
                    pos
                }
            } else {
                pos
            };
            let rotation = rotations.get(id).copied().unwrap_or(0.0);
            let flipped = flips.get(id).copied().unwrap_or(false);
            let hovered = hovered_mask.get(id).copied().unwrap_or(false);
            let mask_origin = mask_atlas.origins.get(id).copied().unwrap_or([0.0, 0.0]);
            instances.push(Instance {
                pos: [render_pos.0, render_pos.1],
                size: [piece_width, piece_height],
                rotation,
                flip: if flipped { 1.0 } else { 0.0 },
                hover: if show_debug {
                    OUTLINE_KIND_DEBUG
                } else if hovered {
                    OUTLINE_KIND_HOVER
                } else {
                    0.0
                },
                drag: if drag_mask.get(id).copied().unwrap_or(false) {
                    drag_rotation
                } else {
                    0.0
                },
                piece_origin: [base_x, base_y],
                mask_origin,
            });
        }
        let count = (instances.len() as u32) - start;
        if count == 0 {
            continue;
        }
        let draw_outline = show_debug || group_has_hover[gid];
        if !show_debug && !draw_outline {
            if let Some(last) = batches.last_mut() {
                if !last.draw_outline {
                    last.count += count;
                    continue;
                }
            }
        }
        batches.push(InstanceBatch {
            start,
            count,
            draw_outline,
        });
    }
    InstanceSet { instances, batches }
}

fn pick_piece_at(
    x: f32,
    y: f32,
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    mask_atlas: &MaskAtlasData,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    mask_pad: f32,
) -> Option<usize> {
    if cols == 0 || piece_width <= 0.0 || piece_height <= 0.0 {
        return None;
    }
    let center_x = piece_width * 0.5;
    let center_y = piece_height * 0.5;
    let min_x = -mask_pad;
    let min_y = -mask_pad;
    let max_x = piece_width + mask_pad;
    let max_y = piece_height + mask_pad;
    let atlas_width = mask_atlas.width as i32;
    let atlas_height = mask_atlas.height as i32;
    for &piece_id in z_order.iter().rev() {
        let col = piece_id % cols;
        let row = piece_id / cols;
        let base_x = col as f32 * piece_width;
        let base_y = row as f32 * piece_height;
        let pos = positions.get(piece_id).copied().unwrap_or((base_x, base_y));
        let rotation = rotations.get(piece_id).copied().unwrap_or(0.0);
        let flipped = flips.get(piece_id).copied().unwrap_or(false);
        let mut local_x = x - pos.0;
        let mut local_y = y - pos.1;
        if rotation.abs() > f32::EPSILON {
            let rot = if flipped { rotation } else { -rotation };
            let (rx, ry) = rotate_point(local_x, local_y, center_x, center_y, rot);
            local_x = rx;
            local_y = ry;
        }
        if flipped {
            local_x = piece_width - local_x;
        }
        if local_x < min_x || local_y < min_y || local_x > max_x || local_y > max_y {
            continue;
        }
        let mask_origin = match mask_atlas.origins.get(piece_id) {
            Some(origin) => *origin,
            None => continue,
        };
        let mask_x = (mask_origin[0] + local_x).floor() as i32;
        let mask_y = (mask_origin[1] + local_y).floor() as i32;
        if mask_x < 0 || mask_y < 0 || mask_x >= atlas_width || mask_y >= atlas_height {
            continue;
        }
        let idx = (mask_y as u32 * mask_atlas.width + mask_x as u32) as usize * 4;
        let alpha = mask_atlas.pixels.get(idx).copied().unwrap_or(0);
        if alpha > 16 {
            return Some(piece_id);
        }
    }
    None
}

fn estimate_text_width(text: &str, font_size: f32) -> f32 {
    let mut units = 0.0;
    for ch in text.chars() {
        if ch.is_ascii_whitespace() {
            units += 0.35;
        } else if ch.is_ascii() {
            units += 0.6;
        } else {
            units += 1.0;
        }
    }
    units * font_size
}

fn estimate_text_block(spec: &UiTextSpec) -> (f32, f32) {
    let mut max_width = 0.0;
    let lines: Vec<&str> = spec.text.lines().collect();
    for line in &lines {
        let width = estimate_text_width(line, spec.font_size);
        if width > max_width {
            max_width = width;
        }
    }
    let line_count = lines.len().max(1) as f32;
    let height = spec.line_height * line_count;
    (max_width, height)
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

fn measure_text_block(
    measure: &mut GlyphonMeasureState,
    spec: &UiTextSpec,
) -> (f32, f32) {
    let metrics = Metrics::new(spec.font_size, spec.line_height);
    let mut buffer = Buffer::new(&mut measure.font_system, metrics);
    buffer.set_size(&mut measure.font_system, None, None);
    let attrs = Attrs::new().family(Family::Name(UI_FONT_FAMILY));
    buffer.set_text(
        &mut measure.font_system,
        &spec.text,
        &attrs,
        Shaping::Advanced,
        Some(Align::Left),
    );
    buffer.shape_until_scroll(&mut measure.font_system, false);
    let (width, height) = measure_text_bounds(&buffer);
    if width <= 0.0 || height <= 0.0 {
        estimate_text_block(spec)
    } else {
        (width, height)
    }
}

fn layout_text_bounds(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> (f32, f32) {
    let (text_width, text_height) = measure_text_block(measure, spec);
    let scaled_width = text_width * UI_TEXT_LAYOUT_SCALE;
    let scaled_height = text_height * UI_TEXT_LAYOUT_SCALE;
    (
        (scaled_width + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
        (scaled_height + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
    )
}

fn rotation_offset_for(origin: UiRotationOrigin, width: f32, height: f32) -> [f32; 2] {
    let half_w = width * 0.5;
    let half_h = height * 0.5;
    match origin {
        UiRotationOrigin::Center => [0.0, 0.0],
        UiRotationOrigin::BottomLeft => [-half_w, half_h],
        UiRotationOrigin::BottomRight => [half_w, half_h],
        UiRotationOrigin::TopLeft => [-half_w, -half_h],
        UiRotationOrigin::TopRight => [half_w, -half_h],
    }
}

fn apply_taffy_layout(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    menu_visible: bool,
    specs: &mut [UiTextSpec],
) {
    let puzzle_width = width * layout.puzzle_scale;
    let puzzle_height = height * layout.puzzle_scale;
    let left_gutter = (-layout.view_min_x).max(0.0);
    let right_gutter = (layout.view_min_x + layout.view_width - puzzle_width).max(0.0);
    let top_gutter = (-layout.view_min_y).max(0.0);
    let bottom_gutter = (layout.view_min_y + layout.view_height - puzzle_height).max(0.0);
    let mut taffy: TaffyTree<()> = TaffyTree::new();
    let root_style = Style {
        display: Display::Grid,
        size: Size {
            width: length(layout.view_width),
            height: length(layout.view_height),
        },
        grid_template_columns: vec![
            length(left_gutter),
            length(puzzle_width),
            length(right_gutter),
        ],
        grid_template_rows: vec![length(top_gutter), length(puzzle_height), length(bottom_gutter)],
        justify_items: Some(JustifyItems::Center),
        align_items: Some(AlignItems::Center),
        ..Default::default()
    };
    let mut nodes: Vec<(UiTextId, NodeId)> = Vec::new();
    let root = if menu_visible {
        let mut menu_children = Vec::new();
        let mut menu_ids = Vec::new();
        let mut title_size = None;
        for spec in specs.iter_mut() {
            if !matches!(spec.id, UiTextId::MenuTitle | UiTextId::MenuSubtitle) {
                continue;
            }
            if matches!(spec.id, UiTextId::MenuTitle) {
                title_size = Some(spec.font_size);
            }
            let (text_width, text_height) = layout_text_bounds(measure, spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let child_style = Style {
                size: Size {
                    width: length(text_width),
                    height: length(text_height),
                },
                ..Default::default()
            };
            let Ok(node) = taffy.new_leaf(child_style) else {
                return;
            };
            menu_ids.push(node);
            menu_children.push((spec.id, node));
        }
        let gap = title_size.unwrap_or(0.0) * 0.4;
        let menu_style = Style {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            align_items: Some(AlignItems::Center),
            justify_content: Some(JustifyContent::Center),
            gap: Size {
                width: zero(),
                height: length(gap),
            },
            grid_row: line(2),
            grid_column: line(2),
            ..Default::default()
        };
        let Ok(menu_node) = taffy.new_with_children(menu_style, &menu_ids) else {
            return;
        };
        nodes.extend(menu_children);
        let Ok(root) = taffy.new_with_children(root_style, &[menu_node]) else {
            return;
        };
        root
    } else {
        let mut children = Vec::new();
        for spec in specs.iter_mut() {
            let (row, col, span_cols, justify, align) = match spec.id {
                UiTextId::Title => (1, 1, false, JustifySelf::Start, AlignSelf::Start),
                UiTextId::Progress => (3, 1, false, JustifySelf::Start, AlignSelf::End),
                UiTextId::Credit => (3, 3, false, JustifySelf::End, AlignSelf::End),
                UiTextId::Success => (1, 2, false, JustifySelf::Center, AlignSelf::Center),
                UiTextId::Debug => (1, 2, false, JustifySelf::End, AlignSelf::Start),
                UiTextId::MenuTitle | UiTextId::MenuSubtitle => {
                    (2, 2, false, JustifySelf::Center, AlignSelf::Center)
                }
            };
            let (text_width, text_height) = layout_text_bounds(measure, spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let mut child_style = Style {
                size: Size {
                    width: length(text_width),
                    height: length(text_height),
                },
                justify_self: Some(justify),
                align_self: Some(align),
                ..Default::default()
            };
            child_style.grid_row = line(row);
            child_style.grid_column = if span_cols { span(3) } else { line(col) };
            let Ok(node) = taffy.new_leaf(child_style) else {
                return;
            };
            children.push(node);
            nodes.push((spec.id, node));
        }
        let Ok(root) = taffy.new_with_children(root_style, &children) else {
            return;
        };
        root
    };
    if taffy
        .compute_layout(
            root,
            Size {
                width: length(layout.view_width),
                height: length(layout.view_height),
            },
        )
        .is_err()
    {
        return;
    }
    let success_offset = width.min(height) * UI_SUCCESS_OFFSET_RATIO;
    for spec in specs.iter_mut() {
        let node = nodes
            .iter()
            .find(|(id, _)| *id == spec.id)
            .map(|(_, node)| *node);
        let Some(node) = node else {
            continue;
        };
        let Ok(node_layout) = taffy.layout(node) else {
            continue;
        };
        spec.pos = [
            layout.view_min_x + node_layout.location.x + node_layout.size.width * 0.5,
            layout.view_min_y + node_layout.location.y + node_layout.size.height * 0.5,
        ];
        if matches!(spec.id, UiTextId::Success) {
            spec.pos[0] += success_offset;
        }
    }
}

fn ui_hitbox_for_spec(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> UiHitbox {
    let (text_width, text_height) = measure_text_block(measure, spec);
    let pad = UI_TEXT_HITBOX_PAD;
    let offset = spec.rotation_offset;
    let angle = spec.rotation_deg.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();
    let pivot = [spec.pos[0] + offset[0], spec.pos[1] + offset[1]];
    let rel_center = [-offset[0], -offset[1]];
    let center = [
        pivot[0] + rel_center[0] * cos - rel_center[1] * sin,
        pivot[1] + rel_center[0] * sin + rel_center[1] * cos,
    ];
    UiHitbox {
        center,
        half_size: [
            (text_width + pad * 2.0) * 0.5,
            (text_height + pad * 2.0) * 0.5,
        ],
        rotation_deg: spec.rotation_deg,
    }
}

fn point_in_ui_hitbox(x: f32, y: f32, hitbox: UiHitbox) -> bool {
    let dx = x - hitbox.center[0];
    let dy = y - hitbox.center[1];
    let angle = -hitbox.rotation_deg.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();
    let local_x = dx * cos - dy * sin;
    let local_y = dx * sin + dy * cos;
    local_x.abs() <= hitbox.half_size[0] && local_y.abs() <= hitbox.half_size[1]
}

fn ui_scale_alpha(color: [u8; 4], scale: f32) -> [u8; 4] {
    let scaled = (color[3] as f32 * scale).round().clamp(0.0, 255.0) as u8;
    [color[0], color[1], color[2], scaled]
}

fn ui_scaled_alpha(alpha: u8) -> u8 {
    (alpha as f32 * UI_TEXT_ALPHA_SCALE)
        .round()
        .clamp(0.0, 255.0) as u8
}

fn ui_text_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [235, 234, 230, 230]
    } else {
        [20, 20, 20, 220]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}

fn ui_accent_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [168, 255, 208, 240]
    } else {
        [24, 120, 62, 230]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}

fn ui_with_alpha(color: [u8; 4], alpha: u8) -> [u8; 4] {
    [color[0], color[1], color[2], alpha]
}

fn build_ui_specs(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    title_text: &str,
    solved: bool,
    connections_label: &str,
    border_connections_label: &str,
    show_debug: bool,
    backend_label: Option<&str>,
    menu_visible: bool,
    is_dark: bool,
) -> Vec<UiTextSpec> {
    let min_dim = width.min(height).max(1.0);
    let base_color = ui_text_color(is_dark);
    let muted_color = ui_with_alpha(base_color, ui_scaled_alpha(170));
    let accent_color = ui_accent_color(is_dark);
    let mut specs = Vec::new();
    if menu_visible {
        let title_size = min_dim * UI_MENU_TITLE_FONT_RATIO;
        let sub_size = min_dim * UI_MENU_SUB_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::MenuTitle,
            text: "Pick a puzzle".to_string(),
            pos: [0.0, 0.0],
            rotation_deg: UI_MENU_TITLE_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::Center,
            rotation_offset: [0.0, 0.0],
            font_size: title_size,
            line_height: title_size * 1.08,
            color: base_color,
        });
        specs.push(UiTextSpec {
            id: UiTextId::MenuSubtitle,
            text: "Scramble / Puzzle / Pieces".to_string(),
            pos: [0.0, 0.0],
            rotation_deg: UI_MENU_SUB_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::Center,
            rotation_offset: [0.0, 0.0],
            font_size: sub_size,
            line_height: sub_size * 1.08,
            color: muted_color,
        });
    } else {
        if !title_text.is_empty() {
            let title_size = min_dim * UI_TITLE_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Title,
                text: title_text.to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_TITLE_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::BottomLeft,
                rotation_offset: [0.0, 0.0],
                font_size: title_size,
                line_height: title_size * 1.08,
                color: base_color,
            });
        }
        let progress_text = format!(
            "{} of borders and {} in total complete",
            border_connections_label, connections_label
        );
        let progress_size = min_dim * UI_PROGRESS_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::Progress,
            text: progress_text,
            pos: [0.0, 0.0],
            rotation_deg: UI_PROGRESS_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::TopLeft,
            rotation_offset: [0.0, 0.0],
            font_size: progress_size,
            line_height: progress_size * 1.08,
            color: base_color,
        });

        let credit_text = CREDIT_TEXT.to_string();
        let credit_size = min_dim * UI_CREDIT_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::Credit,
            text: credit_text,
            pos: [0.0, 0.0],
            rotation_deg: UI_CREDIT_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::BottomLeft,
            rotation_offset: [0.0, 0.0],
            font_size: credit_size,
            line_height: credit_size * 1.08,
            color: base_color,
        });

        if solved {
            let success_size = min_dim * UI_SUCCESS_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Success,
                text: "Well done!".to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_SUCCESS_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::Center,
                rotation_offset: [0.0, 0.0],
                font_size: success_size,
                line_height: success_size * 1.1,
                color: accent_color,
            });
        }
    }
    if show_debug && !menu_visible {
        if let Some(label) = backend_label {
            let debug_size = min_dim * UI_DEBUG_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Debug,
                text: format!("WGPU backend: {label}"),
                pos: [0.0, 0.0],
                rotation_deg: 0.0,
                rotation_origin: UiRotationOrigin::Center,
                rotation_offset: [0.0, 0.0],
                font_size: debug_size,
                line_height: debug_size * 1.08,
                color: muted_color,
            });
        }
    }

    apply_taffy_layout(measure, layout, width, height, menu_visible, &mut specs);
    specs
}

fn rect_to_view(rect: Rect, view: ViewRect, canvas_rect: &web_sys::DomRect) -> Rect {
    let scale_x = if canvas_rect.width() > 0.0 {
        view.width / canvas_rect.width() as f32
    } else {
        0.0
    };
    let scale_y = if canvas_rect.height() > 0.0 {
        view.height / canvas_rect.height() as f32
    } else {
        0.0
    };
    Rect {
        x: view.min_x + rect.x * scale_x,
        y: view.min_y + rect.y * scale_y,
        w: rect.w * scale_x,
        h: rect.h * scale_y,
    }
}

fn preview_corner_radius(piece_width: f32, piece_height: f32) -> f32 {
    let mut corner_radius = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
    let max_corner = piece_width.min(piece_height) * 0.45;
    if corner_radius > max_corner {
        corner_radius = max_corner;
    }
    corner_radius
}

fn preview_cursor_class(target: PreviewHoverTarget) -> Option<&'static str> {
    match target {
        PreviewHoverTarget::None => None,
        PreviewHoverTarget::Body => Some("preview-grab"),
        PreviewHoverTarget::Resize(handle) => Some(match handle {
            PreviewResizeHandle::Left | PreviewResizeHandle::Right => "preview-resize-ew",
            PreviewResizeHandle::Top | PreviewResizeHandle::Bottom => "preview-resize-ns",
            PreviewResizeHandle::TopLeft | PreviewResizeHandle::BottomRight => "preview-resize-nwse",
            PreviewResizeHandle::TopRight | PreviewResizeHandle::BottomLeft => "preview-resize-nesw",
        }),
    }
}

fn rgba(r: u8, g: u8, b: u8, a: f32) -> [f32; 4] {
    [
        r as f32 / 255.0,
        g as f32 / 255.0,
        b as f32 / 255.0,
        a,
    ]
}

fn preview_colors(is_dark: bool) -> PreviewColors {
    if is_dark {
        PreviewColors {
            panel_bg: rgba(28, 28, 28, 0.92),
            panel_border: rgba(255, 255, 255, 0.12),
        }
    } else {
        PreviewColors {
            panel_bg: rgba(255, 255, 255, 0.9),
            panel_border: rgba(0, 0, 0, 0.12),
        }
    }
}

fn make_ui_sprite(
    texture: UiSpriteTexture,
    rect: Rect,
    rotation_deg: f32,
    opacity: f32,
    uv_min: [f32; 2],
    uv_max: [f32; 2],
    color: [f32; 4],
    radius: f32,
    blur_uv: [f32; 2],
    desaturate: f32,
) -> UiSpriteSpec {
    UiSpriteSpec {
        texture,
        pos: rect.center(),
        size: [rect.w, rect.h],
        rotation_deg,
        opacity,
        uv_min,
        uv_max,
        color,
        radius,
        blur_uv,
        desaturate,
    }
}

fn is_dark_theme(mode: ThemeMode) -> bool {
    match mode {
        ThemeMode::Dark => true,
        ThemeMode::Light => false,
        ThemeMode::System => prefers_dark_mode(),
    }
}

fn prefers_dark_mode() -> bool {
    let Some(window) = web_sys::window() else {
        return false;
    };
    let Ok(match_media) = Reflect::get(&window, &"matchMedia".into()) else {
        return false;
    };
    let Ok(match_media) = match_media.dyn_into::<Function>() else {
        return false;
    };
    let Ok(query) = match_media.call1(&window, &"(prefers-color-scheme: dark)".into()) else {
        return false;
    };
    Reflect::get(&query, &"matches".into())
        .ok()
        .and_then(|value| value.as_bool())
        .unwrap_or(false)
}

fn open_credit_url() {
    if let Some(window) = web_sys::window() {
        let _ = window.open_with_url_and_target(CREDIT_URL, "_blank");
    }
}
