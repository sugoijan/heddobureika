use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{request_animation_frame, AnimationFrame};
use glyphon::{Attrs, Buffer, Family, FontSystem, Metrics, Shaping};
use glyphon::cosmic_text::Align;
use taffy::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use js_sys::{Date, Function, Reflect};
use web_sys::{Document, Element, Event, HtmlImageElement, PointerEvent, WheelEvent};

use crate::boot;
use crate::app_core::{AppCore, AppSnapshot, PuzzleAssets, ViewRect};
use crate::app_router;
use crate::input::{
    screen_delta_to_world, screen_scroll_to_world, screen_slop_to_puzzle, screen_to_view_coords,
    workspace_to_puzzle_coords, ClickGesture, PointerKind, PointerPolicy, WheelIntent,
    WheelIntentTracker,
};
use crate::core::*;
use crate::renderer::{build_mask_atlas, MaskAtlasData, UiRotationOrigin, UiTextId, UiTextSpec};
use crate::runtime::{CoreAction, GameSyncView, GameView, SyncView, ViewHooks};
use crate::sync_runtime;
use crate::view_runtime;
use heddobureika_core::{PuzzleImageRef, PuzzleInfo};
use crate::persisted_store;
use crate::puzzle_image::{create_object_url, resolve_puzzle_image_src, revoke_object_url};

const CREDIT_TEXT: &str = "coded by すごいジャン";
const UI_TITLE_TEXT: &str = "ヘッドブレイカー";
const CREDIT_URL: &str = "https://github.com/sugoijan/heddobureika";
const UI_FONT_BYTES: &[u8] = include_bytes!("../fonts/chirufont.ttf");
const UI_FONT_FAMILY: &str = "KaoriGel";
const UI_CREDIT_FONT_RATIO: f32 = 0.026;
const UI_CREDIT_ROTATION_DEG: f32 = -1.1;
const UI_MENU_SUB_FONT_RATIO: f32 = 0.028;
const UI_MENU_SUB_ROTATION_DEG: f32 = 1.1;
const UI_MENU_TITLE_FONT_RATIO: f32 = 0.05;
const UI_MENU_TITLE_ROTATION_DEG: f32 = -0.8;
const UI_PROGRESS_FONT_RATIO: f32 = 0.032;
const UI_PROGRESS_ROTATION_DEG: f32 = -89.6;
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
const PREVIEW_BOX_WIDTH: f32 = 170.0;
const PREVIEW_BOX_MIN_WIDTH: f32 = 120.0;
const PREVIEW_BOX_INIT_FRAC: f32 = 0.24;
const PREVIEW_BOX_MAX_FRAC: f32 = 0.28;
const PREVIEW_BOX_PADDING: f32 = 8.0;
const PREVIEW_BOX_MARGIN: f32 = 16.0;
const PREVIEW_RESIZE_BORDER: f32 = 10.0;
const PREVIEW_MIN_VISIBLE: f32 = 32.0;
const PREVIEW_MIN_VISIBLE_FRAC: f32 = 0.35;
const PREVIEW_MOMENTUM_DECAY: f32 = 8.0;
const PREVIEW_VELOCITY_EPS: f32 = 10.0;
const PREVIEW_VELOCITY_SMOOTH: f32 = 0.25;
const PREVIEW_TILT_DEG: f32 = -1.5;
// Keep in sync with ViewState::fit_zoom_for_size in src/app_core.rs.
const PREVIEW_FIT_PADDING_RATIO: f32 = 0.02;
const PREVIEW_MAX_FIT_SCALE: f32 = 1.0;
const SVG_NS: &str = "http://www.w3.org/2000/svg";

#[derive(Clone, Copy)]
struct UiHitbox {
    center: [f32; 2],
    half_size: [f32; 2],
    rotation_deg: f32,
}

#[derive(Clone, Copy)]
struct PanState {
    last_x: f32,
    last_y: f32,
    pointer_id: Option<i32>,
}

#[derive(Clone, Copy)]
struct PinchState {
    touch_a: i32,
    touch_b: i32,
    last_distance: f32,
}

#[derive(Clone, Copy)]
struct PointerDragGate {
    pointer_id: i32,
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
    gesture: ClickGesture,
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

    fn center(&self) -> [f32; 2] {
        [self.x + self.w * 0.5, self.y + self.h * 0.5]
    }
}

struct PreviewLayout {
    box_rect: Rect,
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

#[derive(Clone, PartialEq)]
struct PuzzleKey {
    image_ref: PuzzleImageRef,
    cols: u32,
    rows: u32,
    width: u32,
    height: u32,
}

impl PuzzleKey {
    fn from_info(info: &PuzzleInfo) -> Self {
        Self {
            image_ref: info.image_ref.clone(),
            cols: info.cols,
            rows: info.rows,
            width: info.image_width,
            height: info.image_height,
        }
    }
}

#[derive(Clone)]
struct SvgPieceNodes {
    root: Element,
    outline_group: Element,
    surface_group: Element,
    internal_group: Element,
    outline_external: Element,
    outline_internal: Element,
    outline_simple: Element,
    debug_group: Element,
    debug_label: Element,
}

struct SvgView {
    core: Rc<AppCore>,
    document: Document,
    root: Element,
    svg: Element,
    defs: Element,
    workspace_rect: Element,
    ui_group: Element,
    puzzle_group: Element,
    puzzle_bounds: Element,
    pieces: RefCell<Vec<SvgPieceNodes>>,
    ui_credit_hitbox: RefCell<Option<UiHitbox>>,
    ui_credit_hovered: Cell<bool>,
    pointer_policy: RefCell<PointerPolicy>,
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    drag_gate: RefCell<Option<PointerDragGate>>,
    auto_pan: RefCell<AutoPanState>,
    hooks: RefCell<Option<ViewHooks>>,
    subscription: RefCell<Option<crate::app_core::AppSubscription>>,
    sync_hook: RefCell<Option<sync_runtime::SyncViewHookHandle>>,
    listeners: RefCell<Vec<EventListener>>,
    mask_atlas: RefCell<Option<Rc<MaskAtlasData>>>,
    last_assets_ptr: Cell<usize>,
    last_puzzle: RefCell<Option<PuzzleKey>>,
    private_image_url: RefCell<Option<String>>,
    private_image_hash: RefCell<Option<String>>,
    private_image_loading: Cell<bool>,
    private_image_error: RefCell<Option<String>>,
    multiplayer_active: Cell<bool>,
    last_z_order: RefCell<Vec<usize>>,
    last_svg_settings: RefCell<Option<SvgRenderSettings>>,
    svg_settings: RefCell<SvgRenderSettings>,
    pending_snapshot: RefCell<Option<(AppSnapshot, SyncView)>>,
    frame_handle: RefCell<Option<AnimationFrame>>,
    viewport_frame: RefCell<Option<AnimationFrame>>,
    viewport_retry: Cell<bool>,
    preview: RefCell<Option<Rc<PreviewOverlay>>>,
    preview_revealed: Cell<bool>,
    preview_hover: Cell<PreviewHoverTarget>,
    preview_pos: Cell<[f32; 2]>,
    preview_width: Cell<f32>,
    preview_velocity: Cell<[f32; 2]>,
    preview_drag: RefCell<Option<PreviewDragState>>,
    preview_click: RefCell<Option<PreviewClickState>>,
    preview_seeded: Cell<bool>,
    preview_motion_last_ms: Cell<f32>,
    preview_motion_frame: RefCell<Option<AnimationFrame>>,
    debug_overlay: RefCell<Option<DebugOverlay>>,
    wheel_intent: RefCell<WheelIntentTracker>,
}

fn drag_angle_for_group(count: usize) -> f32 {
    let denom = (count.max(1) as f32).sqrt();
    DRAG_ROTATION_DEG / denom
}

pub(crate) fn run(core: Rc<AppCore>) {
    #[cfg(target_arch = "wasm32")]
    {
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document
            .get_element_by_id("svg-root")
            .expect("svg-root exists");
        let renderer_kind = app_router::load_renderer_preference();
        core.set_renderer_kind(renderer_kind);
        if renderer_kind != RendererKind::Svg {
            return;
        }

        boot::set_phase("Initializing", "Starting SVG renderer.");
        root.set_class_name("app svg");
        let svg = create_svg_element(&document, "svg");
        let _ = svg.set_attribute("xmlns", SVG_NS);
        let _ = svg.set_attribute("class", "puzzle-image");
        let _ = svg.set_attribute("preserveAspectRatio", "xMidYMid meet");
        let _ = svg.set_attribute("width", "100%");
        let _ = svg.set_attribute("height", "100%");
        let defs = create_svg_element(&document, "defs");
        let workspace_rect = create_svg_element(&document, "rect");
        let _ = workspace_rect.set_attribute("class", "workspace-bounds");
        let ui_group = create_svg_element(&document, "g");
        let puzzle_group = create_svg_element(&document, "g");
        let puzzle_bounds = create_svg_element(&document, "rect");
        let _ = puzzle_bounds.set_attribute("class", "puzzle-bounds");
        let _ = svg.append_child(&defs);
        let _ = svg.append_child(&workspace_rect);
        let _ = svg.append_child(&ui_group);
        let _ = puzzle_group.append_child(&puzzle_bounds);
        let _ = svg.append_child(&puzzle_group);
        let _ = root.append_child(&svg);

        let sync_status = document
            .create_element("div")
            .expect("create sync status");
        sync_status.set_class_name("sync-status");
        let _ = sync_status.set_attribute("title", "Server disconnected");
        sync_status.set_text_content(Some("!"));
        let _ = root.append_child(&sync_status);

        let render_settings = app_router::load_render_settings_with_init();
        let view = Rc::new(SvgView::new(
            core.clone(),
            document.clone(),
            root.clone(),
            svg,
            defs,
            workspace_rect,
            ui_group,
            puzzle_group,
            puzzle_bounds,
            render_settings.svg.clone(),
        ));
        let preview = PreviewOverlay::new(&document);
        let _ = root.append_child(&preview.root);
        *view.preview.borrow_mut() = Some(preview);
        let debug_overlay = DebugOverlay::new(&document);
        let _ = root.append_child(&debug_overlay.root);
        *view.debug_overlay.borrow_mut() = Some(debug_overlay);
        *view.sync_hook.borrow_mut() = Some(sync_runtime::register_sync_view_hook(Rc::new({
            let view = Rc::clone(&view);
            move || {
                let snapshot = view.core.snapshot();
                let sync_view = sync_runtime::sync_view();
                view.queue_render_snapshot(snapshot, sync_view);
            }
        })));
        view_runtime::set_svg_settings_hook(Some(Rc::new({
            let view = view.clone();
            move |settings| {
                view.apply_svg_settings(settings);
            }
        })));
        let adapter = Rc::new(RefCell::new(SvgViewAdapter::new(view.clone())));
        let core_for_hooks = core.clone();
        adapter.borrow_mut().init(ViewHooks {
            on_action: Rc::new(move |action| {
                sync_runtime::dispatch_view_action(&core_for_hooks, action, true);
            }),
        });
        *view.subscription.borrow_mut() = Some(core.subscribe(Rc::new({
            let adapter_for_render = adapter.clone();
            let core = core.clone();
            move || {
                let snapshot = core.snapshot();
                let sync_view = sync_runtime::sync_view();
                adapter_for_render.borrow_mut().render(&snapshot, &sync_view);
            }
        })));
        view.install_listeners();
        view.ensure_viewport_size();
        let snapshot = core.snapshot();
        let sync_view = sync_runtime::sync_view();
        adapter.borrow_mut().render(&snapshot, &sync_view);
        SVG_VIEW.with(|slot| {
            *slot.borrow_mut() = Some(view);
        });
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("svg renderer is only supported on wasm32 targets");
    }
}

thread_local! {
    static SVG_VIEW: RefCell<Option<Rc<SvgView>>> = RefCell::new(None);
}

pub(crate) fn request_render() {
    SVG_VIEW.with(|slot| {
        if let Some(view) = slot.borrow().as_ref() {
            let snapshot = view.core.snapshot();
            let sync_view = sync_runtime::sync_view();
            view.queue_render_snapshot(snapshot, sync_view);
        }
    });
}

impl SvgView {
    #[allow(clippy::too_many_arguments)]
    fn new(
        core: Rc<AppCore>,
        document: Document,
        root: Element,
        svg: Element,
        defs: Element,
        workspace_rect: Element,
        ui_group: Element,
        puzzle_group: Element,
        puzzle_bounds: Element,
        svg_settings: SvgRenderSettings,
    ) -> Self {
        Self {
            core,
            document,
            root,
            svg,
            defs,
            workspace_rect,
            ui_group,
            puzzle_group,
            puzzle_bounds,
            pieces: RefCell::new(Vec::new()),
            ui_credit_hitbox: RefCell::new(None),
            ui_credit_hovered: Cell::new(false),
            pointer_policy: RefCell::new(PointerPolicy::new()),
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            drag_gate: RefCell::new(None),
            auto_pan: RefCell::new(AutoPanState::new()),
            hooks: RefCell::new(None),
            subscription: RefCell::new(None),
            sync_hook: RefCell::new(None),
            listeners: RefCell::new(Vec::new()),
            mask_atlas: RefCell::new(None),
            last_assets_ptr: Cell::new(0),
            last_puzzle: RefCell::new(None),
            private_image_url: RefCell::new(None),
            private_image_hash: RefCell::new(None),
            private_image_loading: Cell::new(false),
            private_image_error: RefCell::new(None),
            multiplayer_active: Cell::new(false),
            last_z_order: RefCell::new(Vec::new()),
            last_svg_settings: RefCell::new(None),
            svg_settings: RefCell::new(svg_settings),
            pending_snapshot: RefCell::new(None),
            frame_handle: RefCell::new(None),
            viewport_frame: RefCell::new(None),
            viewport_retry: Cell::new(false),
            preview: RefCell::new(None),
            preview_revealed: Cell::new(false),
            preview_hover: Cell::new(PreviewHoverTarget::None),
            preview_pos: Cell::new([0.0, 0.0]),
            preview_width: Cell::new(PREVIEW_BOX_WIDTH),
            preview_velocity: Cell::new([0.0, 0.0]),
            preview_drag: RefCell::new(None),
            preview_click: RefCell::new(None),
            preview_seeded: Cell::new(false),
            preview_motion_last_ms: Cell::new(0.0),
            preview_motion_frame: RefCell::new(None),
            debug_overlay: RefCell::new(None),
            wheel_intent: RefCell::new(WheelIntentTracker::new()),
        }
    }

    fn apply_svg_settings(self: &Rc<Self>, settings: SvgRenderSettings) {
        let mut current = self.svg_settings.borrow_mut();
        if *current == settings {
            return;
        }
        *current = settings;
        drop(current);
        self.queue_render_snapshot(self.core.snapshot(), sync_runtime::sync_view());
    }

    fn queue_render_snapshot(self: &Rc<Self>, snapshot: AppSnapshot, sync_view: SyncView) {
        *self.pending_snapshot.borrow_mut() = Some((snapshot, sync_view));
        if self.frame_handle.borrow().is_some() {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |_| {
            view.frame_handle.borrow_mut().take();
            let pending = view.pending_snapshot.borrow_mut().take();
            if let Some((snapshot, sync_view)) = pending {
                view.render_snapshot(&snapshot, &sync_view);
            }
        });
        *self.frame_handle.borrow_mut() = Some(handle);
    }

    fn update_viewport_size(&self) -> bool {
        let rect = self.root.get_bounding_client_rect();
        let width = rect.width() as f32;
        let height = rect.height() as f32;
        if width > 0.0 && height > 0.0 {
            self.core.set_viewport_size(width, height);
            true
        } else {
            false
        }
    }

    fn ensure_viewport_size(self: &Rc<Self>) {
        if self.update_viewport_size() {
            return;
        }
        if self.viewport_retry.replace(true) {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |_| {
            view.viewport_frame.borrow_mut().take();
            view.viewport_retry.set(false);
            view.ensure_viewport_size();
        });
        *self.viewport_frame.borrow_mut() = Some(handle);
    }

    fn pointer_kind(event: &PointerEvent) -> PointerKind {
        PointerKind::from_pointer_type(&event.pointer_type())
    }

    fn clear_hover_state(self: &Rc<Self>, snapshot: &AppSnapshot) {
        let mut update_preview = false;
        if self.set_preview_hover(PreviewHoverTarget::None) {
            update_preview = true;
        }
        if self.ui_credit_hovered.get() {
            self.ui_credit_hovered.set(false);
            self.update_svg_class(snapshot);
        }
        self.dispatch_action(CoreAction::SetHovered { hovered: None });
        if update_preview {
            let rect = self.root.get_bounding_client_rect();
            self.update_preview_overlay(snapshot, &rect);
        }
    }

    fn handle_preview_pointer_down(self: &Rc<Self>, event: &PointerEvent, preview_root: &Element) {
        let kind = Self::pointer_kind(event);
        let button = event.button();
        if matches!(kind, PointerKind::Mouse | PointerKind::Pen) && button != 0 && button != 2 {
            return;
        }
        let now_ms = now_ms();
        let pointer_id = event.pointer_id();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.insert_pointer(pointer_id, kind, screen_x, screen_y, event.buttons());
        }
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let _ = preview_root.set_pointer_capture(pointer_id);
        let rect = self.root.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                let (id_a, a) = touch_points[0];
                let (id_b, b) = touch_points[1];
                if let Some(layout) = self.preview_layout(&snapshot, &rect) {
                    let local_ax = a.screen_x - rect.left() as f32;
                    let local_ay = a.screen_y - rect.top() as f32;
                    let local_bx = b.screen_x - rect.left() as f32;
                    let local_by = b.screen_y - rect.top() as f32;
                    if self.preview_point_inside(&layout, local_ax, local_ay)
                        && self.preview_point_inside(&layout, local_bx, local_by)
                    {
                        self.clear_preview_click();
                        if self.begin_preview_pinch(
                            id_a,
                            id_b,
                            [a.screen_x, a.screen_y],
                            [b.screen_x, b.screen_y],
                            &snapshot,
                            &rect,
                        ) {
                            self.update_preview_overlay(&snapshot, &rect);
                            event.prevent_default();
                            return;
                        }
                    }
                }
                event.prevent_default();
                return;
            }
        }
        if let Some(layout) = self.preview_layout(&snapshot, &rect) {
            let target = self.preview_hit_test(&layout, local_x, local_y);
            if target != PreviewHoverTarget::None {
                if button == 0 {
                    let [drag_x, drag_y] = self.preview_local_point(&layout, local_x, local_y);
                    self.begin_preview_drag(target, drag_x, drag_y, Some(pointer_id));
                    if matches!(target, PreviewHoverTarget::Body) {
                        self.arm_preview_click(Some(pointer_id), local_x, local_y);
                    } else {
                        self.clear_preview_click();
                    }
                }
                if self.set_preview_hover(target) || button == 0 {
                    self.update_preview_overlay(&snapshot, &rect);
                }
                event.prevent_default();
                return;
            }
        }
        self.clear_preview_click();
    }

    fn handle_preview_pointer_move(self: &Rc<Self>, event: &PointerEvent) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.update_pointer(pointer_id, kind, screen_x, screen_y, event.buttons());
        }
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.root.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        self.update_preview_click_movement(Some(pointer_id), local_x, local_y);
        if let Some(PreviewDragState::Pinch {
            touch_a,
            touch_b,
            start_distance,
            start_width,
            anchor_rel,
        }) = *self.preview_drag.borrow()
        {
            let (point_a, point_b) = {
                let policy = self.pointer_policy.borrow();
                (policy.pointer_sample(touch_a), policy.pointer_sample(touch_b))
            };
            if let (Some(a), Some(b)) = (point_a, point_b) {
                if self.update_preview_pinch(
                    [a.screen_x, a.screen_y],
                    [b.screen_x, b.screen_y],
                    &snapshot,
                    &rect,
                    now_ms,
                    start_distance,
                    start_width,
                    anchor_rel,
                ) {
                    self.update_preview_overlay(&snapshot, &rect);
                    event.prevent_default();
                    return;
                }
            }
        }
        if self.update_preview_drag(Some(pointer_id), local_x, local_y, now_ms, &snapshot, &rect) {
            self.update_preview_overlay(&snapshot, &rect);
            event.prevent_default();
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) || event.buttons() != 0 {
            return;
        }
        if self.preview_drag.borrow().is_some() {
            return;
        }
        let mut target = PreviewHoverTarget::None;
        if let Some(layout) = self.preview_layout(&snapshot, &rect) {
            target = self.preview_hit_test(&layout, local_x, local_y);
        }
        if self.set_preview_hover(target) {
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn handle_preview_pointer_up(self: &Rc<Self>, event: &PointerEvent, preview_root: &Element) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.remove_pointer(pointer_id);
            policy.clear_active();
        }
        let _ = preview_root.release_pointer_capture(pointer_id);
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.root.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        self.update_preview_click_movement(Some(pointer_id), local_x, local_y);
        let preview_clicked = self.consume_preview_click(Some(pointer_id));
        let mut preview_dragged = false;
        if let Some(PreviewDragState::Pinch { touch_a, touch_b, .. }) =
            *self.preview_drag.borrow()
        {
            if pointer_id == touch_a || pointer_id == touch_b {
                preview_dragged = self.end_preview_drag(None);
            }
        }
        if !preview_dragged {
            preview_dragged = self.end_preview_drag(Some(pointer_id));
        }
        if preview_clicked {
            self.toggle_preview_revealed();
        }
        if preview_dragged || preview_clicked {
            self.update_preview_overlay(&snapshot, &rect);
            if preview_dragged {
                self.maybe_start_preview_momentum();
            }
            event.prevent_default();
        }
    }

    fn handle_preview_pointer_cancel(
        self: &Rc<Self>,
        event: &PointerEvent,
        preview_root: &Element,
    ) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.remove_pointer(pointer_id);
        }
        let _ = preview_root.release_pointer_capture(pointer_id);
        self.preview_drag.borrow_mut().take();
        self.preview_click.borrow_mut().take();
        self.preview_hover.set(PreviewHoverTarget::None);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(0.0);
        self.preview_motion_frame.borrow_mut().take();
        let snapshot = self.core.snapshot();
        let rect = self.root.get_bounding_client_rect();
        self.update_preview_overlay(&snapshot, &rect);
    }

    fn handle_preview_pointer_leave(self: &Rc<Self>, event: &PointerEvent) {
        let kind = Self::pointer_kind(event);
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) {
            return;
        }
        let now_ms = now_ms();
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
        }
        let snapshot = self.core.snapshot();
        if self.set_preview_hover(PreviewHoverTarget::None) {
            let rect = self.root.get_bounding_client_rect();
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn handle_svg_pointer_down(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
        let kind = Self::pointer_kind(event);
        let button = event.button();
        if matches!(kind, PointerKind::Mouse | PointerKind::Pen) && button != 0 && button != 2 {
            return;
        }
        let now_ms = now_ms();
        let pointer_id = event.pointer_id();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.insert_pointer(pointer_id, kind, screen_x, screen_y, event.buttons());
        }
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let _ = svg.set_pointer_capture(pointer_id);
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                if !snapshot.dragging_members.is_empty() {
                    self.dispatch_action(CoreAction::DragEnd {
                        pointer_id: snapshot.drag_pointer_id,
                    });
                }
                let (id_a, a) = touch_points[0];
                let (id_b, b) = touch_points[1];
                let dx = b.screen_x - a.screen_x;
                let dy = b.screen_y - a.screen_y;
                let distance = (dx * dx + dy * dy).sqrt();
                if distance > 0.0 {
                    *self.pinch_state.borrow_mut() = Some(PinchState {
                        touch_a: id_a,
                        touch_b: id_b,
                        last_distance: distance,
                    });
                    if self.pan_state.borrow_mut().take().is_some() {
                        self.core.settle_view();
                    }
                    self.update_svg_class(&snapshot);
                    event.prevent_default();
                }
                return;
            }
        }
        let Some((view_x, view_y)) =
            screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
        else {
            return;
        };
        let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
        if let Some(piece_id) = self.pick_piece(px, py, &snapshot) {
            let right_click = button == 2;
            if matches!(kind, PointerKind::Touch | PointerKind::Pen) {
                *self.drag_gate.borrow_mut() = Some(PointerDragGate {
                    pointer_id,
                    start_x: px,
                    start_y: py,
                    moved: false,
                });
            } else {
                self.drag_gate.borrow_mut().take();
            }
            let click_slop = screen_slop_to_puzzle(
                TOUCH_DRAG_SLOP_PX,
                svg,
                snapshot.view,
                snapshot.layout.puzzle_scale,
            );
            self.dispatch_action(CoreAction::BeginDrag {
                piece_id,
                x: px,
                y: py,
                shift_key: event.shift_key(),
                rotate_mode: event.ctrl_key(),
                right_click,
                click_slop,
                pointer_id: Some(pointer_id),
            });
            let drag_snapshot = self.core.snapshot();
            self.update_auto_pan(screen_x, screen_y, &drag_snapshot);
            event.prevent_default();
            return;
        }
        if self.hit_credit(view_x, view_y) {
            open_credit_url();
            event.prevent_default();
            return;
        }
        if button == 0 {
            self.ui_credit_hovered.set(false);
            *self.pan_state.borrow_mut() = Some(PanState {
                last_x: screen_x,
                last_y: screen_y,
                pointer_id: Some(pointer_id),
            });
            self.update_svg_class(&snapshot);
            event.prevent_default();
        }
    }

    fn handle_svg_pointer_move(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let buttons = event.buttons();
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.update_pointer(pointer_id, kind, screen_x, screen_y, buttons);
        }
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        if kind == PointerKind::Touch {
            let pinch_opt = { self.pinch_state.borrow_mut().take() };
            if let Some(mut pinch) = pinch_opt {
                let (point_a, point_b) = {
                    let policy = self.pointer_policy.borrow();
                    (
                        policy.pointer_sample(pinch.touch_a),
                        policy.pointer_sample(pinch.touch_b),
                    )
                };
                if let (Some(a), Some(b)) = (point_a, point_b) {
                    let center_x = (a.screen_x + b.screen_x) * 0.5;
                    let center_y = (a.screen_y + b.screen_y) * 0.5;
                    let dx = b.screen_x - a.screen_x;
                    let dy = b.screen_y - a.screen_y;
                    let distance = (dx * dx + dy * dy).sqrt();
                    if distance > 0.0 {
                        let factor = distance / pinch.last_distance;
                        if factor.is_finite() && factor > 0.0 {
                            if let Some((view_x, view_y)) =
                                screen_to_view_coords(center_x, center_y, svg, snapshot.view)
                            {
                                self.core.zoom_view_at(factor, view_x, view_y);
                            }
                            pinch.last_distance = distance;
                        }
                    }
                    if let Ok(mut slot) = self.pinch_state.try_borrow_mut() {
                        *slot = Some(pinch);
                    }
                    event.prevent_default();
                    return;
                }
            }
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                let (id_a, a) = touch_points[0];
                let (id_b, b) = touch_points[1];
                let dx = b.screen_x - a.screen_x;
                let dy = b.screen_y - a.screen_y;
                let distance = (dx * dx + dy * dy).sqrt();
                if distance > 0.0 {
                    *self.pinch_state.borrow_mut() = Some(PinchState {
                        touch_a: id_a,
                        touch_b: id_b,
                        last_distance: distance,
                    });
                }
                event.prevent_default();
                return;
            }
        }
        let delta = {
            let mut pan_state = self.pan_state.borrow_mut();
            if let Some(pan) = pan_state.as_mut() {
                if pan.pointer_id != Some(pointer_id) {
                    None
                } else {
                    let dx = screen_x - pan.last_x;
                    let dy = screen_y - pan.last_y;
                    pan.last_x = screen_x;
                    pan.last_y = screen_y;
                    screen_delta_to_world(dx, dy, svg, snapshot.view)
                }
            } else {
                None
            }
        };
        if let Some((dx_world, dy_world)) = delta {
            self.core.pan_view(dx_world, dy_world);
            event.prevent_default();
            return;
        }
        if !snapshot.dragging_members.is_empty() {
            if snapshot.drag_pointer_id != Some(pointer_id) {
                return;
            }
            let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
            else {
                return;
            };
            let (px, py) =
                workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(gate) = self.drag_gate.borrow_mut().as_mut() {
                if let Some(drag_id) = snapshot.drag_pointer_id {
                    if gate.pointer_id != drag_id {
                        gate.moved = true;
                    } else if !gate.moved {
                        let click_tolerance =
                            snapshot.piece_width.min(snapshot.piece_height) * CLICK_MOVE_RATIO;
                        let slop = screen_slop_to_puzzle(
                            TOUCH_DRAG_SLOP_PX,
                            svg,
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
            self.update_auto_pan(screen_x, screen_y, &snapshot);
            self.dispatch_action(CoreAction::DragMove { x: px, y: py });
            event.prevent_default();
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) || buttons != 0 {
            return;
        }
        if self.preview_drag.borrow().is_some() {
            return;
        }
        if self.pan_state.borrow().is_some() {
            return;
        }
        let Some((view_x, view_y)) = screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
        else {
            self.dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        };
        let (px, py) =
            workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
        let hovered = self.pick_piece(px, py, &snapshot);
        if hovered.is_some() {
            if self.ui_credit_hovered.get() {
                self.ui_credit_hovered.set(false);
                self.update_svg_class(&snapshot);
            }
            self.dispatch_action(CoreAction::SetHovered { hovered });
            return;
        }
        let credit_hovered = self.hit_credit(view_x, view_y);
        if self.ui_credit_hovered.get() != credit_hovered {
            self.ui_credit_hovered.set(credit_hovered);
            self.update_svg_class(&snapshot);
        }
        if credit_hovered {
            self.dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        }
        self.dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_svg_pointer_up(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        let screen_x = event.client_x() as f32;
        let screen_y = event.client_y() as f32;
        let kind_changed = { self.pointer_policy.borrow().kind_changed(kind) };
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.remove_pointer(pointer_id);
            policy.clear_active();
        }
        let _ = svg.release_pointer_capture(pointer_id);
        self.drag_gate.borrow_mut().take();
        let snapshot = self.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        self.stop_auto_pan();
        let mut cleared = false;
        let clear_pinch = {
            let pinch_state = self.pinch_state.borrow();
            pinch_state
                .as_ref()
                .map(|pinch| pinch.touch_a == pointer_id || pinch.touch_b == pointer_id)
                .unwrap_or(false)
        };
        if clear_pinch {
            self.pinch_state.borrow_mut().take();
            cleared = true;
        }
        let clear_pan = {
            let pan_state = self.pan_state.borrow();
            pan_state
                .as_ref()
                .map(|pan| pan.pointer_id == Some(pointer_id))
                .unwrap_or(false)
        };
        if clear_pan {
            self.pan_state.borrow_mut().take();
            cleared = true;
        }
        if cleared {
            self.update_svg_class(&snapshot);
            self.core.settle_view();
        }
        if !snapshot.dragging_members.is_empty() {
            if let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
            {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.dispatch_action(CoreAction::DragMove { x: px, y: py });
            }
            self.dispatch_action(CoreAction::DragEnd {
                pointer_id: Some(pointer_id),
            });
        }
        let snapshot = self.core.snapshot();
        if !snapshot.dragging_members.is_empty() {
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) {
            return;
        }
        let hovered = screen_to_view_coords(screen_x, screen_y, svg, snapshot.view).and_then(
            |(view_x, view_y)| {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.pick_piece(px, py, &snapshot)
            },
        );
        self.dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_svg_pointer_cancel(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
        let kind = Self::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.remove_pointer(pointer_id);
        }
        let _ = svg.release_pointer_capture(pointer_id);
        self.pan_state.borrow_mut().take();
        self.pinch_state.borrow_mut().take();
        self.drag_gate.borrow_mut().take();
        self.stop_auto_pan();
        let snapshot = self.core.snapshot();
        self.update_svg_class(&snapshot);
        self.core.settle_view();
        self.dispatch_action(CoreAction::DragEnd {
            pointer_id: snapshot.drag_pointer_id,
        });
    }

    fn handle_svg_pointer_leave(self: &Rc<Self>, event: &PointerEvent) {
        let kind = Self::pointer_kind(event);
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) {
            return;
        }
        let now_ms = now_ms();
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
        }
        self.ui_credit_hovered.set(false);
        let snapshot = self.core.snapshot();
        self.update_svg_class(&snapshot);
        self.dispatch_action(CoreAction::SetHovered { hovered: None });
    }

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        if let Some(preview) = self.preview.borrow().as_ref() {
            let preview_root = preview.root.clone();
            let preview_root_for_handler = preview_root.clone();
            let view = Rc::clone(self);
            let listener = EventListener::new_with_options(
                &preview_root,
                "pointerdown",
                EventListenerOptions {
                    phase: EventListenerPhase::Bubble,
                    passive: false,
                },
                move |event: &Event| {
                    let Some(event) = event.dyn_ref::<PointerEvent>() else {
                        return;
                    };
                    view.handle_preview_pointer_down(event, &preview_root_for_handler);
                },
            );
            listeners.push(listener);

            let preview_root = preview.root.clone();
            let preview_root_for_handler = preview_root.clone();
            let view = Rc::clone(self);
            let listener = EventListener::new_with_options(
                &preview_root,
                "pointermove",
                EventListenerOptions {
                    phase: EventListenerPhase::Bubble,
                    passive: false,
                },
                move |event: &Event| {
                    let Some(event) = event.dyn_ref::<PointerEvent>() else {
                        return;
                    };
                    view.handle_preview_pointer_move(event);
                },
            );
            listeners.push(listener);

            let preview_root = preview.root.clone();
            let view = Rc::clone(self);
            let listener = EventListener::new_with_options(
                &preview_root,
                "pointerup",
                EventListenerOptions {
                    phase: EventListenerPhase::Bubble,
                    passive: false,
                },
                move |event: &Event| {
                    let Some(event) = event.dyn_ref::<PointerEvent>() else {
                        return;
                    };
                    view.handle_preview_pointer_up(event, &preview_root_for_handler);
                },
            );
            listeners.push(listener);

            let preview_root = preview.root.clone();
            let preview_root_for_handler = preview_root.clone();
            let view = Rc::clone(self);
            let listener = EventListener::new_with_options(
                &preview_root,
                "pointercancel",
                EventListenerOptions {
                    phase: EventListenerPhase::Bubble,
                    passive: false,
                },
                move |event: &Event| {
                    let Some(event) = event.dyn_ref::<PointerEvent>() else {
                        return;
                    };
                    view.handle_preview_pointer_cancel(event, &preview_root_for_handler);
                },
            );
            listeners.push(listener);

            let view = Rc::clone(self);
            let listener = EventListener::new_with_options(
                &preview.root,
                "pointerleave",
                EventListenerOptions {
                    phase: EventListenerPhase::Bubble,
                    passive: false,
                },
                move |event: &Event| {
                    let Some(event) = event.dyn_ref::<PointerEvent>() else {
                        return;
                    };
                    view.handle_preview_pointer_leave(event);
                },
            );
            listeners.push(listener);

            let preview_root = preview.root.clone();
            let listener = EventListener::new_with_options(
                &preview_root,
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
        }

        let svg = self.svg.clone();
        let svg_for_handler = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
            "pointerdown",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_svg_pointer_down(event, &svg_for_handler);
            },
        );
        listeners.push(listener);

        let svg = self.svg.clone();
        let svg_for_handler = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
            "pointermove",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_svg_pointer_move(event, &svg_for_handler);
            },
        );
        listeners.push(listener);

        let svg = self.svg.clone();
        let svg_for_handler = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
            "pointerup",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_svg_pointer_up(event, &svg_for_handler);
            },
        );
        listeners.push(listener);

        let svg = self.svg.clone();
        let svg_for_handler = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
            "pointercancel",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_svg_pointer_cancel(event, &svg_for_handler);
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &self.svg,
            "pointerleave",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_svg_pointer_leave(event);
            },
        );
        listeners.push(listener);

        let listener = EventListener::new_with_options(
            &self.svg,
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
        let view = Rc::clone(self);
        let svg_for_wheel = self.svg.clone();
        let listener = EventListener::new_with_options(
            &self.svg,
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
                let rect = svg_for_wheel.get_bounding_client_rect();
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
                let intent = view.wheel_intent.borrow_mut().decide(event, now_ms());
                let wants_pan =
                    (event.ctrl_key() || event.meta_key()) && matches!(intent, WheelIntent::Pan);
                if wants_pan {
                    if let Some((dx_world, dy_world)) =
                        screen_scroll_to_world(delta_x, delta_y, &svg_for_wheel, snapshot.view)
                    {
                        core.pan_view(dx_world, dy_world);
                    }
                } else {
                    let Some((view_x, view_y)) = screen_to_view_coords(
                        event.client_x() as f32,
                        event.client_y() as f32,
                        &svg_for_wheel,
                        snapshot.view,
                    ) else {
                        return;
                    };
                    let zoom_factor = (-delta_y * 0.0015).exp();
                    if zoom_factor.is_finite() && zoom_factor > 0.0 {
                        core.zoom_view_at(zoom_factor, view_x, view_y);
                    }
                }
                view.update_svg_class(&snapshot);
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let window = web_sys::window().expect("window available");
        let view = Rc::clone(self);
        let listener = EventListener::new(&window, "resize", move |_event| {
            view.ensure_viewport_size();
        });
        listeners.push(listener);

        *self.listeners.borrow_mut() = listeners;
    }

    fn set_hooks(&self, hooks: ViewHooks) {
        *self.hooks.borrow_mut() = Some(hooks);
    }

    fn dispatch_action(&self, action: CoreAction) {
        if let Some(hooks) = self.hooks.borrow().as_ref() {
            (hooks.on_action)(action);
        }
    }

    fn update_svg_class(&self, snapshot: &AppSnapshot) {
        let mut class = "puzzle-image".to_string();
        if snapshot.app_settings.show_debug {
            class.push_str(" debug");
        }
        let svg_settings = self.svg_settings.borrow();
        if !svg_settings.animations {
            class.push_str(" no-anim");
        }
        if svg_settings.fast_render {
            class.push_str(" fast-render");
        }
        if snapshot.solved {
            class.push_str(" solved");
        }
        if self.pan_state.borrow().is_some() {
            class.push_str(" panning");
        }
        let _ = self.svg.set_attribute("class", &class);
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
        let rect = self.svg.get_bounding_client_rect();
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
            screen_to_view_coords(screen_x, screen_y, &self.svg, snapshot.view)
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
                screen_to_view_coords(screen_x, screen_y, &self.svg, snapshot.view)
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

    fn render_snapshot(self: &Rc<Self>, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        if snapshot.view.width <= 0.0 || snapshot.view.height <= 0.0 {
            self.ensure_viewport_size();
        }
        self.multiplayer_active
            .set(matches!(sync_view.mode(), InitMode::Online));
        let disconnected =
            matches!(sync_view.mode(), InitMode::Online) && !sync_view.connected();
        if disconnected {
            self.root.set_class_name("app svg sync-disconnected");
        } else {
            self.root.set_class_name("app svg");
        }
        if let Some(preview) = self.preview.borrow().as_ref() {
            if let Some(info) = snapshot.puzzle_info.as_ref() {
                if let Some(src) = self.resolve_image_src(info) {
                    preview.set_image_src(src.as_str());
                    preview.set_visible(true);
                    let rect = self.root.get_bounding_client_rect();
                    self.update_preview_overlay(snapshot, &rect);
                } else {
                    preview.set_visible(false);
                }
            } else {
                preview.set_visible(false);
            }
        }
        let Some(assets) = self.core.assets() else {
            self.clear_scene();
            return;
        };
        self.ensure_scene(snapshot, assets.clone());
        self.update_svg_class(snapshot);
        if snapshot.dragging_members.is_empty() {
            self.stop_auto_pan();
        }
        self.update_debug_overlay(snapshot);
        self.render_view(snapshot, &assets);
        self.render_ui(snapshot);
        self.render_pieces(snapshot, &assets);
        self.update_z_order(snapshot);
        boot::ready();
    }

    fn reset_preview_state(&self) {
        self.preview_drag.borrow_mut().take();
        self.preview_click.borrow_mut().take();
        self.preview_seeded.set(false);
        self.preview_hover.set(PreviewHoverTarget::None);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(0.0);
        self.preview_motion_frame.borrow_mut().take();
    }

    fn clear_private_image_cache(&self) {
        if let Some(url) = self.private_image_url.borrow_mut().take() {
            revoke_object_url(&url);
        }
        self.private_image_hash.borrow_mut().take();
        self.private_image_error.borrow_mut().take();
        self.private_image_loading.set(false);
    }

    fn note_private_image_error(&self, hash: &str, message: &str) {
        let mut last = self.private_image_error.borrow_mut();
        if last.as_deref() == Some(hash) {
            return;
        }
        *last = Some(hash.to_string());
        if !self.multiplayer_active.get() {
            boot::fail("image-missing", message, "Re-upload the image to this browser.");
        }
        self.private_image_loading.set(false);
    }

    fn ensure_private_image_src(self: &Rc<Self>, hash: &str, info: &PuzzleInfo) -> Option<String> {
        if self.private_image_hash.borrow().as_deref() == Some(hash) {
            if let Some(url) = self.private_image_url.borrow().as_ref() {
                return Some(url.clone());
            }
        }
        if self.private_image_loading.get() {
            return None;
        }
        self.private_image_loading.set(true);
        let hash_value = hash.to_string();
        let view = Rc::clone(self);
        let key = PuzzleKey::from_info(info);
        spawn_local(async move {
            let entry = match persisted_store::load_private_image(&hash_value).await {
                Ok(Some(entry)) => entry,
                Ok(None) => {
                    view.note_private_image_error(&hash_value, "Private image is missing.");
                    return;
                }
                Err(message) => {
                    view.note_private_image_error(&hash_value, &message);
                    return;
                }
            };
            let url = match create_object_url(&entry.bytes, &entry.mime) {
                Ok(url) => url,
                Err(_) => {
                    view.note_private_image_error(&hash_value, "Failed to prepare private image.");
                    return;
                }
            };
            if view
                .last_puzzle
                .borrow()
                .as_ref()
                .map(|current| current != &key)
                .unwrap_or(false)
            {
                revoke_object_url(&url);
                view.private_image_loading.set(false);
                return;
            }
            *view.private_image_url.borrow_mut() = Some(url.clone());
            *view.private_image_hash.borrow_mut() = Some(hash_value.clone());
            view.private_image_loading.set(false);
            let mut updated = entry;
            updated.last_used_at = js_sys::Date::now().max(0.0) as u64;
            let _ = persisted_store::save_private_image(&hash_value, updated).await;
            view.last_assets_ptr.set(0);
            view.queue_render_snapshot(view.core.snapshot(), sync_runtime::sync_view());
        });
        None
    }

    fn resolve_image_src(self: &Rc<Self>, info: &PuzzleInfo) -> Option<String> {
        match &info.image_ref {
            PuzzleImageRef::Private { hash } => self.ensure_private_image_src(hash, info),
            _ => resolve_puzzle_image_src(&info.image_ref),
        }
    }

    fn update_preview_overlay(&self, snapshot: &AppSnapshot, rect: &web_sys::DomRect) {
        let preview_ref = self.preview.borrow();
        let Some(preview) = preview_ref.as_ref() else {
            return;
        };
        let Some(layout) = self.preview_layout(snapshot, rect) else {
            return;
        };
        preview.set_layout(
            layout.box_rect.x,
            layout.box_rect.y,
            layout.box_rect.w,
            layout.box_rect.h,
            PREVIEW_TILT_DEG,
        );
        let mut class = String::from("preview-box");
        if self.preview_revealed.get() {
            class.push_str(" preview-revealed");
        } else {
            class.push_str(" preview-hidden");
        }
        let drag_target = self.preview_drag.borrow().as_ref().and_then(|drag| match drag {
            PreviewDragState::Move { .. } => Some(PreviewHoverTarget::Body),
            PreviewDragState::Resize { handle, .. } => Some(PreviewHoverTarget::Resize(*handle)),
            PreviewDragState::Pinch { .. } => None,
        });
        let cursor_target = drag_target.unwrap_or(self.preview_hover.get());
        if let Some(cursor_class) = preview_cursor_class(cursor_target) {
            class.push_str(" ");
            class.push_str(cursor_class);
        }
        if matches!(
            self.preview_drag.borrow().as_ref().map(|drag| drag.kind()),
            Some(PreviewDragKind::Move)
        ) {
            class.push_str(" preview-dragging");
        }
        preview.set_class(&class);
    }

    fn set_preview_hover(&self, target: PreviewHoverTarget) -> bool {
        if self.preview_hover.get() == target {
            return false;
        }
        self.preview_hover.set(target);
        true
    }

    fn toggle_preview_revealed(&self) {
        let revealed = !self.preview_revealed.get();
        self.preview_revealed.set(revealed);
    }

    fn arm_preview_click(&self, pointer_id: Option<i32>, local_x: f32, local_y: f32) {
        let mut gesture = ClickGesture::new_default();
        gesture.arm(local_x, local_y, now_ms());
        *self.preview_click.borrow_mut() = Some(PreviewClickState { pointer_id, gesture });
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
        click.gesture.update(local_x, local_y);
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
        click.gesture.is_click(now_ms())
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
        rect: &web_sys::DomRect,
        aspect: f32,
    ) {
        if self.preview_seeded.get() {
            return;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return;
        };
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
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
        pointer_a: i32,
        pointer_b: i32,
        screen_a: [f32; 2],
        screen_b: [f32; 2],
        snapshot: &AppSnapshot,
        rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, rect, aspect);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let pos = self.preview_pos.get();
        if box_w <= 0.0 || box_h <= 0.0 {
            return false;
        }
        let mid_x = (screen_a[0] + screen_b[0]) * 0.5 - rect.left() as f32;
        let mid_y = (screen_a[1] + screen_b[1]) * 0.5 - rect.top() as f32;
        let center = [pos[0] + box_w * 0.5, pos[1] + box_h * 0.5];
        let dx = mid_x - center[0];
        let dy = mid_y - center[1];
        let (ux, uy) = rotate_vec(dx, dy, -PREVIEW_TILT_DEG);
        let mut anchor_rel = [0.5 + ux / box_w, 0.5 + uy / box_h];
        anchor_rel[0] = anchor_rel[0].clamp(0.0, 1.0);
        anchor_rel[1] = anchor_rel[1].clamp(0.0, 1.0);
        let dx = screen_b[0] - screen_a[0];
        let dy = screen_b[1] - screen_a[1];
        let distance = (dx * dx + dy * dy).sqrt();
        if distance <= 0.0 || !distance.is_finite() {
            return false;
        }
        *self.preview_drag.borrow_mut() = Some(PreviewDragState::Pinch {
            touch_a: pointer_a,
            touch_b: pointer_b,
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
        screen_a: [f32; 2],
        screen_b: [f32; 2],
        snapshot: &AppSnapshot,
        rect: &web_sys::DomRect,
        now_ms: f32,
        start_distance: f32,
        start_width: f32,
        anchor_rel: [f32; 2],
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        let frame_max_w =
            Self::preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) =
            self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let dx = screen_b[0] - screen_a[0];
        let dy = screen_b[1] - screen_a[1];
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
        let mid_x = (screen_a[0] + screen_b[0]) * 0.5 - rect.left() as f32;
        let mid_y = (screen_a[1] + screen_b[1]) * 0.5 - rect.top() as f32;
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
        rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, rect, aspect);
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
                let (clamped, _) =
                    self.clamp_preview_position(target_pos, box_w, box_h, viewport_w, viewport_h);
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
                    PreviewResizeHandle::Left => [right - box_w, center_y - box_h * 0.5],
                    PreviewResizeHandle::Right => [left, center_y - box_h * 0.5],
                    PreviewResizeHandle::Top => [center_x - box_w * 0.5, bottom - box_h],
                    PreviewResizeHandle::Bottom => [center_x - box_w * 0.5, top],
                    PreviewResizeHandle::TopLeft => [right - box_w, bottom - box_h],
                    PreviewResizeHandle::TopRight => [left, bottom - box_h],
                    PreviewResizeHandle::BottomLeft => [right - box_w, top],
                    PreviewResizeHandle::BottomRight => [left, top],
                };
                let (clamped, _) =
                    self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
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
        rect: &web_sys::DomRect,
    ) -> bool {
        if self.preview_drag.borrow().is_some() {
            self.preview_motion_last_ms.set(now_ms);
            return false;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, rect, aspect);
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
        rect: &web_sys::DomRect,
    ) -> Option<PreviewLayout> {
        let info = snapshot.puzzle_info.as_ref()?;
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return None;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, rect, aspect);
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
        let _ = (image_w, image_h);
        Some(PreviewLayout { box_rect })
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

    fn ensure_preview_motion_frame(self: &Rc<Self>) {
        if self.preview_motion_frame.borrow().is_some() {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |_| {
            view.preview_motion_frame();
        });
        *self.preview_motion_frame.borrow_mut() = Some(handle);
    }

    fn preview_motion_frame(self: &Rc<Self>) {
        self.preview_motion_frame.borrow_mut().take();
        let snapshot = self.core.snapshot();
        let rect = self.root.get_bounding_client_rect();
        let now = now_ms();
        if self.update_preview_motion(now, &snapshot, &rect) {
            self.update_preview_overlay(&snapshot, &rect);
            self.ensure_preview_motion_frame();
        } else {
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn maybe_start_preview_momentum(self: &Rc<Self>) {
        let velocity = self.preview_velocity.get();
        if velocity[0].abs() > PREVIEW_VELOCITY_EPS || velocity[1].abs() > PREVIEW_VELOCITY_EPS {
            self.ensure_preview_motion_frame();
        }
    }

    fn clear_scene(&self) {
        self.pieces.borrow_mut().clear();
        self.mask_atlas.borrow_mut().take();
        self.last_assets_ptr.set(0);
        self.last_puzzle.borrow_mut().take();
        self.clear_private_image_cache();
        self.last_z_order.borrow_mut().clear();
        clear_children(&self.defs);
        clear_children(&self.puzzle_group);
        let _ = self.puzzle_group.append_child(&self.puzzle_bounds);
    }

    fn ensure_scene(self: &Rc<Self>, snapshot: &AppSnapshot, assets: Rc<PuzzleAssets>) {
        let ptr = Rc::as_ptr(&assets) as usize;
        let settings_changed = {
            let current = self.svg_settings.borrow().clone();
            let mut last = self.last_svg_settings.borrow_mut();
            if last.as_ref() != Some(&current) {
                *last = Some(current);
                true
            } else {
                false
            }
        };
        let puzzle_changed = self
            .last_puzzle
            .borrow()
            .as_ref()
            .map(|key| key != &PuzzleKey::from_info(&assets.info))
            .unwrap_or(true);
        let assets_changed = self.last_assets_ptr.get() != ptr;
        if puzzle_changed || assets_changed || settings_changed {
            self.last_assets_ptr.set(ptr);
            self.last_puzzle
                .borrow_mut()
                .replace(PuzzleKey::from_info(&assets.info));
            if puzzle_changed {
                self.reset_preview_state();
                self.clear_private_image_cache();
            }
            self.rebuild_defs(snapshot, &assets);
            let image_src = self
                .resolve_image_src(&assets.info)
                .unwrap_or_else(|| "".to_string());
            self.rebuild_pieces(snapshot, &assets, image_src.as_str());
        }
    }

    fn rebuild_defs(&self, _snapshot: &AppSnapshot, assets: &PuzzleAssets) {
        clear_children(&self.defs);
        self.mask_atlas.borrow_mut().take();
        let pattern = create_svg_element(&self.document, "pattern");
        let _ = pattern.set_attribute("id", "piece-back-pattern");
        let _ = pattern.set_attribute("patternUnits", "userSpaceOnUse");
        let _ = pattern.set_attribute("width", "28");
        let _ = pattern.set_attribute("height", "28");
        let rect = create_svg_element(&self.document, "rect");
        let _ = rect.set_attribute("width", "28");
        let _ = rect.set_attribute("height", "28");
        let _ = rect.set_attribute("fill", "#8f5b32");
        let circle1 = create_svg_element(&self.document, "circle");
        let _ = circle1.set_attribute("cx", "7");
        let _ = circle1.set_attribute("cy", "7");
        let _ = circle1.set_attribute("r", "2.8");
        let _ = circle1.set_attribute("fill", "#734423");
        let circle2 = create_svg_element(&self.document, "circle");
        let _ = circle2.set_attribute("cx", "21");
        let _ = circle2.set_attribute("cy", "21");
        let _ = circle2.set_attribute("r", "2.8");
        let _ = circle2.set_attribute("fill", "#734423");
        let circle3 = create_svg_element(&self.document, "circle");
        let _ = circle3.set_attribute("cx", "21");
        let _ = circle3.set_attribute("cy", "7");
        let _ = circle3.set_attribute("r", "1.8");
        let _ = circle3.set_attribute("fill", "#3a2418");
        let circle4 = create_svg_element(&self.document, "circle");
        let _ = circle4.set_attribute("cx", "7");
        let _ = circle4.set_attribute("cy", "21");
        let _ = circle4.set_attribute("r", "1.8");
        let _ = circle4.set_attribute("fill", "#3a2418");
        let _ = pattern.append_child(&rect);
        let _ = pattern.append_child(&circle1);
        let _ = pattern.append_child(&circle2);
        let _ = pattern.append_child(&circle3);
        let _ = pattern.append_child(&circle4);
        let _ = self.defs.append_child(&pattern);

        let emboss_filter = build_emboss_filter(
            &self.document,
            assets.piece_width,
            assets.piece_height,
            assets.mask_pad,
            self.svg_settings.borrow().fast_filter,
        );
        let _ = self.defs.append_child(&emboss_filter);

        let mask_x = fmt_f32(-assets.mask_pad);
        let mask_y = fmt_f32(-assets.mask_pad);
        let mask_width = fmt_f32(assets.piece_width + assets.mask_pad * 2.0);
        let mask_height = fmt_f32(assets.piece_height + assets.mask_pad * 2.0);
        for (piece, paths) in assets.pieces.iter().zip(assets.paths.iter()) {
            let mask = create_svg_element(&self.document, "mask");
            let _ = mask.set_attribute("id", &format!("piece-mask-{}", piece.id));
            let _ = mask.set_attribute("maskUnits", "userSpaceOnUse");
            let _ = mask.set_attribute("maskContentUnits", "userSpaceOnUse");
            let _ = mask.set_attribute("x", &mask_x);
            let _ = mask.set_attribute("y", &mask_y);
            let _ = mask.set_attribute("width", &mask_width);
            let _ = mask.set_attribute("height", &mask_height);
            let _ = mask.set_attribute("mask-type", "luminance");
            let rect = create_svg_element(&self.document, "rect");
            let _ = rect.set_attribute("x", &mask_x);
            let _ = rect.set_attribute("y", &mask_y);
            let _ = rect.set_attribute("width", &mask_width);
            let _ = rect.set_attribute("height", &mask_height);
            let _ = rect.set_attribute("fill", "black");
            let path = create_svg_element(&self.document, "path");
            let _ = path.set_attribute("d", paths.outline.as_str());
            let _ = path.set_attribute("fill", "white");
            let _ = mask.append_child(&rect);
            let _ = mask.append_child(&path);
            let _ = self.defs.append_child(&mask);
        }

        if self.mask_atlas.borrow().is_none() {
            boot::set_phase("Preparing pieces", "Building mask atlas.");
            match build_mask_atlas(
                &assets.pieces,
                &assets.paths,
                assets.piece_width,
                assets.piece_height,
                assets.grid,
                assets.mask_pad,
            ) {
                Ok(atlas) => {
                    *self.mask_atlas.borrow_mut() = Some(Rc::new(atlas));
                }
                Err(err) => {
                    boot::fail(
                        "mask-atlas",
                        "Failed to prepare puzzle rendering.",
                        "Reload the page or try another browser.",
                    );
                    web_sys::console::error_1(&err);
                }
            }
        }
    }

    fn rebuild_pieces(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets, image_src: &str) {
        clear_children(&self.puzzle_group);
        let _ = self.puzzle_group.append_child(&self.puzzle_bounds);
        self.pieces.borrow_mut().clear();
        for piece in &assets.pieces {
            let node = self.build_piece_node(snapshot, assets, piece.id, image_src);
            let _ = self.puzzle_group.append_child(&node.root);
            self.pieces.borrow_mut().push(node);
        }
        self.last_z_order.borrow_mut().clear();
    }

    fn build_piece_node(
        &self,
        _snapshot: &AppSnapshot,
        assets: &PuzzleAssets,
        piece_id: usize,
        image_src: &str,
    ) -> SvgPieceNodes {
        let root = create_svg_element(&self.document, "g");
        let outline_group = create_svg_element(&self.document, "g");
        let surface_group = create_svg_element(&self.document, "g");
        let internal_group = create_svg_element(&self.document, "g");
        let hitbox = create_svg_element(&self.document, "path");
        let outline_external = create_svg_element(&self.document, "path");
        let outline_internal = create_svg_element(&self.document, "path");
        let outline_simple = create_svg_element(&self.document, "path");
        let back_rect = create_svg_element(&self.document, "rect");
        let image = create_svg_element(&self.document, "image");
        let debug_group = create_svg_element(&self.document, "g");
        let debug_label = create_svg_element(&self.document, "text");
        let debug_center = create_svg_element(&self.document, "circle");

        let _ = root.set_attribute("class", "piece");
        let _ = outline_group.set_attribute("transform", "");
        let _ = internal_group.set_attribute("transform", "");
        let _ = surface_group.set_attribute("class", "piece-surface");
        let _ = hitbox.set_attribute("class", "piece-hitbox");
        let _ = outline_external.set_attribute(
            "class",
            "piece-outline piece-outline-group edge-external",
        );
        let _ = outline_internal.set_attribute(
            "class",
            "piece-outline piece-outline-group edge-internal",
        );
        let _ = outline_simple.set_attribute("class", "piece-outline piece-outline-simple");
        let _ = back_rect.set_attribute("class", "piece-back");
        let _ = image.set_attribute("class", "piece-image");
        let _ = debug_group.set_attribute("class", "piece-debug");
        let _ = debug_label.set_attribute("class", "piece-debug-label");
        let _ = debug_center.set_attribute("class", "piece-debug-center");

        let mask_ref = format!("url(#piece-mask-{piece_id})");
        let img_col = piece_id as u32 % assets.grid.cols;
        let img_row = piece_id as u32 / assets.grid.cols;
        let img_x = fmt_f32(-(img_col as f32 * assets.piece_width));
        let img_y = fmt_f32(-(img_row as f32 * assets.piece_height));
        let width = fmt_f32(assets.info.image_width as f32);
        let height = fmt_f32(assets.info.image_height as f32);
        let _ = back_rect.set_attribute("x", &img_x);
        let _ = back_rect.set_attribute("y", &img_y);
        let _ = back_rect.set_attribute("width", &width);
        let _ = back_rect.set_attribute("height", &height);
        let _ = back_rect.set_attribute("fill", "url(#piece-back-pattern)");
        let _ = back_rect.set_attribute("mask", &mask_ref);
        let _ = image.set_attribute("href", image_src);
        let _ = image.set_attribute("x", &img_x);
        let _ = image.set_attribute("y", &img_y);
        let _ = image.set_attribute("width", &width);
        let _ = image.set_attribute("height", &height);
        let _ = image.set_attribute("preserveAspectRatio", "xMidYMid meet");
        let _ = image.set_attribute("mask", &mask_ref);

        let path = assets.paths.get(piece_id).map(|p| p.outline.clone()).unwrap_or_default();
        let _ = hitbox.set_attribute("d", path.as_str());

        let _ = outline_group.append_child(&outline_external);
        let _ = outline_group.append_child(&hitbox);
        let _ = surface_group.append_child(&back_rect);
        let _ = surface_group.append_child(&image);
        let _ = internal_group.append_child(&outline_internal);
        let _ = internal_group.append_child(&outline_simple);
        let _ = internal_group.append_child(&debug_group);
        let _ = debug_group.append_child(&debug_center);
        let _ = debug_group.append_child(&debug_label);
        let _ = root.append_child(&outline_group);
        let _ = root.append_child(&surface_group);
        let _ = root.append_child(&internal_group);

        let _ = debug_center.set_attribute("r", "3");
        let center_x = fmt_f32(assets.piece_width * 0.5);
        let center_y = fmt_f32(assets.piece_height * 0.5);
        let _ = debug_center.set_attribute("cx", &center_x);
        let _ = debug_center.set_attribute("cy", &center_y);
        let _ = debug_label.set_attribute("x", &center_x);
        let _ = debug_label.set_attribute("y", &center_y);
        let _ = debug_label.set_text_content(Some(&format!("#{}", piece_id)));

        let _ = surface_group.set_attribute("filter", "none");
        let _ = outline_internal.set_attribute("mask", &mask_ref);

        SvgPieceNodes {
            root,
            outline_group,
            surface_group,
            internal_group,
            outline_external,
            outline_internal,
            outline_simple,
            debug_group,
            debug_label,
        }
    }

    fn render_view(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets) {
        let view = snapshot.view;
        let view_box = format!(
            "{} {} {} {}",
            fmt_f32(view.min_x),
            fmt_f32(view.min_y),
            fmt_f32(view.width),
            fmt_f32(view.height)
        );
        let _ = self.svg.set_attribute("viewBox", &view_box);
        let layout = snapshot.layout;
        let _ = self
            .workspace_rect
            .set_attribute("x", &fmt_f32(layout.view_min_x));
        let _ = self
            .workspace_rect
            .set_attribute("y", &fmt_f32(layout.view_min_y));
        let _ = self
            .workspace_rect
            .set_attribute("width", &fmt_f32(layout.view_width));
        let _ = self
            .workspace_rect
            .set_attribute("height", &fmt_f32(layout.view_height));
        let _ = self
            .puzzle_group
            .set_attribute("transform", &format!("scale({})", fmt_f32(layout.puzzle_scale)));
        let bounds_inset = 1.0;
        let _ = self
            .puzzle_bounds
            .set_attribute("x", &fmt_f32(bounds_inset));
        let _ = self
            .puzzle_bounds
            .set_attribute("y", &fmt_f32(bounds_inset));
        let _ = self.puzzle_bounds.set_attribute(
            "width",
            &fmt_f32(assets.info.image_width as f32 - 2.0 * bounds_inset),
        );
        let _ = self.puzzle_bounds.set_attribute(
            "height",
            &fmt_f32(assets.info.image_height as f32 - 2.0 * bounds_inset),
        );
        let mut frame_corner_radius = assets
            .piece_width
            .min(assets.piece_height)
            * CORNER_RADIUS_RATIO;
        let max_corner_radius = assets.piece_width.min(assets.piece_height) * 0.45;
        if frame_corner_radius > max_corner_radius {
            frame_corner_radius = max_corner_radius;
        }
        let radius = fmt_f32(frame_corner_radius);
        let _ = self.puzzle_bounds.set_attribute("rx", &radius);
        let _ = self.puzzle_bounds.set_attribute("ry", &radius);
    }

    fn render_ui(&self, snapshot: &AppSnapshot) {
        clear_children(&self.ui_group);
        let layout = snapshot.layout;
        let width = snapshot
            .puzzle_info
            .as_ref()
            .map(|info| info.image_width as f32)
            .unwrap_or(1.0);
        let height = snapshot
            .puzzle_info
            .as_ref()
            .map(|info| info.image_height as f32)
            .unwrap_or(1.0);
        let (connections_label, border_connections_label) = if let Some(info) =
            snapshot.puzzle_info.as_ref()
        {
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
        let prefers_dark = prefers_dark_mode();
        let is_dark = match snapshot.app_settings.theme_mode {
            ThemeMode::Dark => true,
            ThemeMode::Light => false,
            ThemeMode::System => prefers_dark,
        };
        let mut measure_state = GlyphonMeasureState::new();
        let ui_specs = build_ui_specs(
            &mut measure_state,
            layout,
            width,
            height,
            UI_TITLE_TEXT,
            snapshot.solved,
            connections_label.as_str(),
            border_connections_label.as_str(),
            false,
            is_dark,
        );
        let credit_hitbox = ui_specs
            .iter()
            .find(|spec| matches!(spec.id, UiTextId::Credit))
            .map(|spec| ui_hitbox_for_spec(&mut measure_state, spec));
        *self.ui_credit_hitbox.borrow_mut() = credit_hitbox;
        if ui_specs.is_empty() {
            return;
        }
        for spec in ui_specs {
            let text_node = create_svg_element(&self.document, "text");
            let class = match spec.id {
                UiTextId::Title => "ui-text ui-title",
                UiTextId::Progress => "ui-text ui-progress",
                UiTextId::Credit => "ui-text ui-credit",
                UiTextId::Success => "ui-text ui-success",
                UiTextId::Debug => "ui-text ui-debug",
                UiTextId::MenuTitle => "ui-text ui-menu-title",
                UiTextId::MenuSubtitle => "ui-text ui-menu-sub",
            };
            let _ = text_node.set_attribute("class", class);
            let _ = text_node.set_attribute("x", &fmt_f32(spec.pos[0]));
            let _ = text_node.set_attribute("y", &fmt_f32(spec.pos[1]));
            let rotation = fmt_f32(spec.rotation_deg);
            let pivot_x = fmt_f32(spec.pos[0] + spec.rotation_offset[0]);
            let pivot_y = fmt_f32(spec.pos[1] + spec.rotation_offset[1]);
            let transform = format!("rotate({rotation} {pivot_x} {pivot_y})");
            let _ = text_node.set_attribute("transform", &transform);
            let style = format!(
                "font-size: {}px; fill: {};",
                fmt_f32(spec.font_size * 0.925),
                ui_color_to_css(spec.color)
            );
            let _ = text_node.set_attribute("style", &style);
            let lines: Vec<&str> = spec.text.lines().collect();
            let line_count = lines.len().max(1) as f32;
            let total_height = spec.line_height * (line_count - 1.0).max(0.0);
            let mut dy = -total_height * 0.5;
            for (idx, line) in lines.iter().enumerate() {
                let tspan = create_svg_element(&self.document, "tspan");
                let _ = tspan.set_attribute("x", &fmt_f32(spec.pos[0]));
                let span_dy = if idx == 0 { dy } else { spec.line_height };
                let _ = tspan.set_attribute("dy", &fmt_f32(span_dy));
                tspan.set_text_content(Some(line));
                let _ = text_node.append_child(&tspan);
                dy = spec.line_height;
            }
            let _ = self.ui_group.append_child(&text_node);
        }
    }

    fn render_pieces(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets) {
        let total = assets.grid.cols as usize * assets.grid.rows as usize;
        if total == 0 {
            return;
        }
        let mut hovered_mask = vec![false; total];
        if !snapshot.dragging_members.is_empty() {
            for id in &snapshot.dragging_members {
                if *id < hovered_mask.len() {
                    hovered_mask[*id] = true;
                }
            }
        } else if let Some(id) = snapshot.hovered_id {
            if snapshot.core.connections.len() == total {
                for member in collect_group(
                    &snapshot.core.connections,
                    id,
                    assets.grid.cols as usize,
                    assets.grid.rows as usize,
                ) {
                    if member < hovered_mask.len() {
                        hovered_mask[member] = true;
                    }
                }
            } else if id < hovered_mask.len() {
                hovered_mask[id] = true;
            }
        }
        let mut dragging_mask = vec![false; total];
        for id in &snapshot.dragging_members {
            if *id < dragging_mask.len() {
                dragging_mask[*id] = true;
            }
        }
        let drag_dir: f32 = if snapshot.dragging_members.is_empty() {
            0.0
        } else if snapshot.drag_right_click {
            -1.0
        } else {
            1.0
        };
        let drag_rotation = if drag_dir.abs() > f32::EPSILON {
            drag_angle_for_group(snapshot.dragging_members.len()) * drag_dir.signum()
        } else {
            0.0
        };
        let drag_scale = if drag_dir.abs() > f32::EPSILON { DRAG_SCALE } else { 1.0 };
        let drag_center = if drag_dir.abs() > f32::EPSILON {
            snapshot
                .drag_cursor
                .or_else(|| drag_group_center(&snapshot.core.positions, &snapshot.dragging_members, assets.piece_width, assets.piece_height))
        } else {
            None
        };
        for (idx, node) in self.pieces.borrow().iter().enumerate() {
            if idx >= total {
                break;
            }
            let current = snapshot.core.positions.get(idx).copied().unwrap_or((
                (idx as u32 % assets.grid.cols) as f32 * assets.piece_width,
                (idx as u32 / assets.grid.cols) as f32 * assets.piece_height,
            ));
            let rotation = snapshot.core.rotations.get(idx).copied().unwrap_or(0.0);
            let flipped = snapshot.core.flips.get(idx).copied().unwrap_or(false);
            let is_dragging = dragging_mask.get(idx).copied().unwrap_or(false);
            let render_pos = if is_dragging {
                if let Some(center) = drag_center {
                    drag_group_position(
                        current,
                        center,
                        drag_scale,
                        drag_rotation,
                        assets.piece_width,
                        assets.piece_height,
                    )
                } else {
                    current
                }
            } else {
                current
            };
            let center_x = assets.piece_width * 0.5;
            let center_y = assets.piece_height * 0.5;
            let flip_transform = if flipped {
                format!(
                    " translate({} {}) scale(-1 1) translate(-{} -{})",
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(center_x),
                    fmt_f32(center_y)
                )
            } else {
                String::new()
            };
            let drag_transform = if is_dragging {
                format!(
                    " translate({} {}) scale({}) translate(-{} -{}) rotate({} {} {})",
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(drag_scale),
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(drag_rotation),
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                )
            } else {
                String::new()
            };
            let outer_transform = format!(
                "translate({} {})",
                fmt_f32(render_pos.0),
                fmt_f32(render_pos.1)
            );
            let inner_transform = format!(
                "{} rotate({} {} {}){}",
                flip_transform,
                fmt_f32(rotation),
                fmt_f32(center_x),
                fmt_f32(center_y),
                drag_transform
            );
            let _ = node.root.set_attribute("transform", &outer_transform);
            let _ = node.outline_group.set_attribute("transform", &inner_transform);
            let _ = node.surface_group.set_attribute("transform", &inner_transform);
            let _ = node.internal_group.set_attribute("transform", &inner_transform);

            let mut class = if is_dragging {
                if flipped {
                    "piece dragging flipped".to_string()
                } else {
                    "piece dragging".to_string()
                }
            } else if flipped {
                "piece flipped".to_string()
            } else {
                "piece".to_string()
            };
            if hovered_mask.get(idx).copied().unwrap_or(false) {
                class.push_str(" hovered");
            }
            let _ = node.root.set_attribute("class", &class);

            if self.svg_settings.borrow().emboss && !flipped {
                let _ = node.surface_group.set_attribute("filter", "url(#emboss)");
            } else {
                let _ = node.surface_group.set_attribute("filter", "none");
            }
            if self.svg_settings.borrow().emboss {
                let _ = node.outline_simple.set_attribute("display", "none");
            } else {
                let _ = node.outline_simple.set_attribute("display", "");
            }
            if snapshot.app_settings.show_debug {
                let _ = node.debug_group.set_attribute("display", "");
                let label = format!(
                    "#{}\nx:{}\ny:{}\nr:{}",
                    idx,
                    fmt_f32(current.0),
                    fmt_f32(current.1),
                    fmt_f32(rotation)
                );
                update_debug_label(&self.document, &node.debug_label, center_x, center_y, &label);
            } else {
                let _ = node.debug_group.set_attribute("display", "none");
            }
            if snapshot.core.connections.len() == total {
                let connection = snapshot.core.connections.get(idx).copied().unwrap_or([false; 4]);
                let mut external_path = String::new();
                if let Some(paths) = assets.paths.get(idx) {
                    for (dir, edge_path) in [
                        (DIR_UP, &paths.edges[DIR_UP]),
                        (DIR_RIGHT, &paths.edges[DIR_RIGHT]),
                        (DIR_DOWN, &paths.edges[DIR_DOWN]),
                        (DIR_LEFT, &paths.edges[DIR_LEFT]),
                    ] {
                        let connected = connection.get(dir).copied().unwrap_or(false);
                        if !connected {
                            if !external_path.is_empty() {
                                external_path.push(' ');
                            }
                            external_path.push_str(edge_path);
                        }
                    }
                }
                if external_path.is_empty() {
                    let _ = node.outline_external.set_attribute("d", "");
                    let _ = node.outline_internal.set_attribute("d", "");
                    let _ = node.outline_external.set_attribute("display", "none");
                    let _ = node.outline_internal.set_attribute("display", "none");
                } else if flipped {
                    let _ = node.outline_internal.set_attribute("d", &external_path);
                    let _ = node.outline_internal.set_attribute("display", "");
                    let _ = node.outline_external.set_attribute("d", "");
                    let _ = node.outline_external.set_attribute("display", "none");
                } else {
                    let _ = node.outline_external.set_attribute("d", &external_path);
                    let _ = node.outline_external.set_attribute("display", "");
                    let _ = node.outline_internal.set_attribute("d", "");
                    let _ = node.outline_internal.set_attribute("display", "none");
                }
            }
            if let Some(paths) = assets.paths.get(idx) {
                let _ = node.outline_simple.set_attribute("d", &paths.outline);
            }
        }
    }

    fn update_z_order(&self, snapshot: &AppSnapshot) {
        let total = snapshot.grid.cols as usize * snapshot.grid.rows as usize;
        if total == 0 {
            return;
        }
        let mut order = if snapshot.z_order.len() == total {
            snapshot.z_order.clone()
        } else {
            (0..total).collect()
        };
        if order == *self.last_z_order.borrow() {
            return;
        }
        *self.last_z_order.borrow_mut() = order.clone();
        for id in order.drain(..) {
            if let Some(node) = self.pieces.borrow().get(id) {
                let _ = self.puzzle_group.append_child(&node.root);
            }
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
            assets.grid.rows as usize,
            assets.piece_width,
            assets.piece_height,
            assets.mask_pad,
        )
    }

    fn hit_credit(&self, x: f32, y: f32) -> bool {
        self.ui_credit_hitbox
            .borrow()
            .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
            .unwrap_or(false)
    }
}

pub(crate) struct SvgViewAdapter {
    view: Rc<SvgView>,
    _hooks: Option<ViewHooks>,
}

impl SvgViewAdapter {
    fn new(view: Rc<SvgView>) -> Self {
        Self { view, _hooks: None }
    }
}

impl GameView for SvgViewAdapter {
    fn init(&mut self, hooks: ViewHooks) {
        self.view.set_hooks(hooks.clone());
        self._hooks = Some(hooks);
    }

    fn render(&mut self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let sync_view = snapshot_sync_view(sync_view);
        self.view
            .queue_render_snapshot(snapshot.clone(), sync_view);
    }
}

fn snapshot_sync_view(sync_view: &dyn GameSyncView) -> SyncView {
    SyncView::new(
        sync_view.mode(),
        sync_view.connected(),
        sync_view.client_id(),
        sync_view.init_required(),
        sync_view.room_id().map(|value| value.to_string()),
        sync_view.persistence(),
        sync_view.ownership_by_anchor(),
    )
}

struct PreviewOverlay {
    root: Element,
    img: HtmlImageElement,
}

impl PreviewOverlay {
    fn new(document: &Document) -> Rc<Self> {
        let root = document
            .create_element("aside")
            .expect("create preview root");
        root.set_class_name("preview-box preview-hidden");
        let img = HtmlImageElement::new().expect("create preview img");
        img.set_alt("preview");
        let _ = img.set_attribute("draggable", "false");
        let _ = root.append_child(&img);
        let overlay = Rc::new(Self { root, img });
        overlay
    }

    fn set_visible(&self, visible: bool) {
        if visible {
            let _ = self.root.remove_attribute("style");
        } else {
            let _ = self.root.set_attribute("style", "display: none;");
        }
    }

    fn set_image_src(&self, src: &str) {
        self.img.set_src(src);
    }

    fn set_layout(&self, x: f32, y: f32, w: f32, h: f32, rotation_deg: f32) {
        let style = format!(
            "left: {}px; top: {}px; width: {}px; height: {}px; transform: rotate({}deg);",
            fmt_f32(x),
            fmt_f32(y),
            fmt_f32(w),
            fmt_f32(h),
            fmt_f32(rotation_deg)
        );
        let _ = self.root.set_attribute("style", &style);
    }

    fn set_class(&self, class: &str) {
        self.root.set_class_name(class);
    }
}

struct GlyphonMeasureState {
    font_system: FontSystem,
}

impl GlyphonMeasureState {
    fn new() -> Self {
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(UI_FONT_BYTES.to_vec());
        Self { font_system }
    }
}

fn measure_text_block(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> (f32, f32) {
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

fn measure_text_bounds(buffer: &Buffer) -> (f32, f32) {
    let mut width: f32 = 0.0;
    let mut height: f32 = 0.0;
    for run in buffer.layout_runs() {
        let run_width = run
            .glyphs
            .last()
            .map(|glyph| glyph.x + glyph.w)
            .unwrap_or(0.0);
        width = width.max(run_width);
        height = (run.line_y + run.line_height).max(height);
    }
    (width, height)
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
    apply_taffy_layout(measure, layout, width, height, menu_visible, &mut specs);
    specs
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

fn ui_color_to_css(color: [u8; 4]) -> String {
    let alpha = (color[3] as f32 / 255.0).clamp(0.0, 1.0);
    format!("rgba({}, {}, {}, {})", color[0], color[1], color[2], alpha)
}

fn create_svg_element(document: &Document, tag: &str) -> Element {
    document
        .create_element_ns(Some(SVG_NS), tag)
        .expect("create svg element")
}

fn clear_children(parent: &Element) {
    while let Some(child) = parent.first_child() {
        let _ = parent.remove_child(&child);
    }
}

fn build_emboss_filter(
    document: &Document,
    piece_width: f32,
    piece_height: f32,
    mask_pad: f32,
    fast_filter: bool,
) -> Element {
    let emboss_filter = create_svg_element(document, "filter");
    let _ = emboss_filter.set_attribute("id", "emboss");
    let emboss_offset_neg = fmt_f32(-EMBOSS_OFFSET);
    let emboss_rim_radius = fmt_f32(EMBOSS_RIM);
    let base_w = piece_width + mask_pad * 2.0;
    let base_h = piece_height + mask_pad * 2.0;
    let base_diag = (base_w * base_w + base_h * base_h).sqrt();
    let rotation_pad = (base_diag - base_w.max(base_h)) * 0.5;
    let emboss_pad = EMBOSS_OFFSET.abs() + EMBOSS_RIM + rotation_pad;
    let emboss_x = fmt_f32(-mask_pad - emboss_pad);
    let emboss_y = fmt_f32(-mask_pad - emboss_pad);
    let emboss_width = fmt_f32(piece_width + (mask_pad + emboss_pad) * 2.0);
    let emboss_height = fmt_f32(piece_height + (mask_pad + emboss_pad) * 2.0);
    let _ = emboss_filter.set_attribute("x", &emboss_x);
    let _ = emboss_filter.set_attribute("y", &emboss_y);
    let _ = emboss_filter.set_attribute("width", &emboss_width);
    let _ = emboss_filter.set_attribute("height", &emboss_height);
    let _ = emboss_filter.set_attribute("filterUnits", "userSpaceOnUse");
    let _ = emboss_filter.set_attribute("primitiveUnits", "userSpaceOnUse");
    let _ = emboss_filter.set_attribute("color-interpolation-filters", "linearRGB");
    if fast_filter {
        let filter_res = format!(
            "{} {}",
            fmt_f32(piece_width * 0.2),
            fmt_f32(piece_height * 0.2)
        );
        let _ = emboss_filter.set_attribute("filterRes", &filter_res);
    }
    let emboss_opacity = fmt_f32(EMBOSS_OPACITY);
    let steps = vec![
        ("feComponentTransfer", vec![("in", "SourceAlpha"), ("result", "a")]),
        ("feFuncA", vec![("type", "linear"), ("slope", "1")]),
        ("feOffset", vec![("in", "a"), ("dx", emboss_offset_neg.as_str()), ("dy", emboss_offset_neg.as_str()), ("result", "aOff")]),
        ("feFlood", vec![("flood-color", "#000"), ("result", "black")]),
        ("feComposite", vec![("in", "black"), ("in2", "a"), ("operator", "in"), ("result", "blackShape")]),
        ("feFlood", vec![("flood-color", "#fff"), ("flood-opacity", "0.6"), ("result", "white")]),
        ("feComposite", vec![("in", "white"), ("in2", "aOff"), ("operator", "in"), ("result", "whiteShape")]),
        ("feMorphology", vec![("in", "whiteShape"), ("operator", "erode"), ("radius", "0.6"), ("result", "whiteThin")]),
        ("feGaussianBlur", vec![("in", "whiteThin"), ("stdDeviation", "0.5"), ("result", "whiteShapeBlur")]),
        ("feComposite", vec![("in", "whiteShapeBlur"), ("in2", "blackShape"), ("operator", "over"), ("result", "overlayFull")]),
        ("feMorphology", vec![("in", "a"), ("operator", "erode"), ("radius", emboss_rim_radius.as_str()), ("result", "aInner")]),
        ("feComposite", vec![("in", "a"), ("in2", "aInner"), ("operator", "arithmetic"), ("k1", "0"), ("k2", "1"), ("k3", "-1"), ("k4", "0"), ("result", "rim")]),
        ("feComposite", vec![("in", "overlayFull"), ("in2", "rim"), ("operator", "in"), ("result", "overlayRim")]),
        ("feComponentTransfer", vec![("in", "overlayRim"), ("result", "overlayRimOpacity")]),
        ("feFuncA", vec![("type", "linear"), ("slope", emboss_opacity.as_str())]),
    ];
    let mut iter = steps.into_iter();
    while let Some((name, attrs)) = iter.next() {
        let node = create_svg_element(document, name);
        for (key, value) in attrs {
            let _ = node.set_attribute(key, value);
        }
        if name == "feComponentTransfer" {
            if let Some((child_name, child_attrs)) = iter.next() {
                let child = create_svg_element(document, child_name);
                for (key, value) in child_attrs {
                    let _ = child.set_attribute(key, value);
                }
                let _ = node.append_child(&child);
            }
        }
        let _ = emboss_filter.append_child(&node);
    }
    let merge = create_svg_element(document, "feMerge");
    let merge_node1 = create_svg_element(document, "feMergeNode");
    let _ = merge_node1.set_attribute("in", "SourceGraphic");
    let merge_node2 = create_svg_element(document, "feMergeNode");
    let _ = merge_node2.set_attribute("in", "overlayRimOpacity");
    let _ = merge.append_child(&merge_node1);
    let _ = merge.append_child(&merge_node2);
    let _ = emboss_filter.append_child(&merge);
    emboss_filter
}

fn update_debug_label(document: &Document, label: &Element, x: f32, y: f32, text: &str) {
    clear_children(label);
    let _ = label.set_attribute("x", &fmt_f32(x));
    let _ = label.set_attribute("y", &fmt_f32(y - 12.0));
    for (idx, line) in text.lines().enumerate() {
        let tspan = create_svg_element(document, "tspan");
        let _ = tspan.set_attribute("x", &fmt_f32(x));
        let dy = if idx == 0 { "-20" } else { "20" };
        let _ = tspan.set_attribute("dy", dy);
        tspan.set_text_content(Some(line));
        let _ = label.append_child(&tspan);
    }
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

fn pick_piece_at(
    x: f32,
    y: f32,
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    mask_atlas: &MaskAtlasData,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    mask_pad: f32,
) -> Option<usize> {
    if cols == 0 || rows == 0 || piece_width <= 0.0 || piece_height <= 0.0 {
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
        let pos = positions.get(piece_id).copied().unwrap_or_else(|| {
            (
                base_x,
                base_y,
            )
        });
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

fn open_credit_url() {
    if let Some(window) = web_sys::window() {
        let _ = window.open_with_url_and_target(CREDIT_URL, "_blank");
    }
}

fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
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
