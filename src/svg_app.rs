use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{request_animation_frame, AnimationFrame};
use glyphon::{Attrs, Buffer, Family, FontSystem, Metrics, Shaping};
use glyphon::cosmic_text::Align;
use taffy::prelude::*;
use wasm_bindgen::JsCast;
use js_sys::{Function, Reflect};
use web_sys::{
    Document, Element, Event, HtmlImageElement, MouseEvent, Touch, TouchEvent, WheelEvent,
};

use crate::app_core::{AppCore, AppSnapshot, PuzzleAssets, ViewRect};
use crate::app_router;
use crate::core::*;
use crate::renderer::{build_mask_atlas, MaskAtlasData, UiRotationOrigin, UiTextId, UiTextSpec};
use crate::runtime::{CoreAction, GameSyncView};
use crate::sync_runtime;
use heddobureika_core::PuzzleInfo;

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
    touch_id: Option<i32>,
}

#[derive(Clone, Copy)]
struct PinchState {
    touch_a: i32,
    touch_b: i32,
    last_distance: f32,
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

#[derive(Clone)]
struct SvgPieceNodes {
    root: Element,
    outline_group: Element,
    surface_group: Element,
    internal_group: Element,
    hitbox: Element,
    outline_external: Element,
    outline_internal: Element,
    outline_simple: Element,
    back_rect: Element,
    image: Element,
    debug_group: Element,
    debug_label: Element,
    debug_center: Element,
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
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    auto_pan: RefCell<AutoPanState>,
    subscription: RefCell<Option<crate::app_core::AppSubscription>>,
    sync_hook: RefCell<Option<sync_runtime::SyncViewHookHandle>>,
    listeners: RefCell<Vec<EventListener>>,
    mask_atlas: RefCell<Option<Rc<MaskAtlasData>>>,
    last_assets_ptr: Cell<usize>,
    last_puzzle: RefCell<Option<PuzzleKey>>,
    last_z_order: RefCell<Vec<usize>>,
    last_svg_settings: RefCell<Option<SvgRenderSettings>>,
    pending_snapshot: RefCell<Option<AppSnapshot>>,
    frame_handle: RefCell<Option<AnimationFrame>>,
    viewport_frame: RefCell<Option<AnimationFrame>>,
    viewport_retry: Cell<bool>,
    preview: RefCell<Option<Rc<PreviewOverlay>>>,
    debug_overlay: RefCell<Option<DebugOverlay>>,
}

fn drag_angle_for_group(count: usize) -> f32 {
    let denom = (count.max(1) as f32).sqrt();
    DRAG_ROTATION_DEG / denom
}

pub(crate) fn run() {
    #[cfg(target_arch = "wasm32")]
    {
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document
            .get_element_by_id("svg-root")
            .expect("svg-root exists");
        let renderer_kind = app_router::load_renderer_preference();
        let core = AppCore::shared();
        core.set_renderer_kind(renderer_kind);
        if renderer_kind != RendererKind::Svg {
            return;
        }

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
                view.queue_render_snapshot(view.core.snapshot());
            }
        })));
        *view
            .subscription
            .borrow_mut() = Some(core.subscribe(Rc::new({
            let view = Rc::clone(&view);
            let core = core.clone();
            move || {
                view.queue_render_snapshot(core.snapshot());
            }
        })));
        view.install_listeners();
        view.ensure_viewport_size();
        view.queue_render_snapshot(core.snapshot());
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
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            auto_pan: RefCell::new(AutoPanState::new()),
            subscription: RefCell::new(None),
            sync_hook: RefCell::new(None),
            listeners: RefCell::new(Vec::new()),
            mask_atlas: RefCell::new(None),
            last_assets_ptr: Cell::new(0),
            last_puzzle: RefCell::new(None),
            last_z_order: RefCell::new(Vec::new()),
            last_svg_settings: RefCell::new(None),
            pending_snapshot: RefCell::new(None),
            frame_handle: RefCell::new(None),
            viewport_frame: RefCell::new(None),
            viewport_retry: Cell::new(false),
            preview: RefCell::new(None),
            debug_overlay: RefCell::new(None),
        }
    }

    fn queue_render_snapshot(self: &Rc<Self>, snapshot: AppSnapshot) {
        *self.pending_snapshot.borrow_mut() = Some(snapshot);
        if self.frame_handle.borrow().is_some() {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |_| {
            view.frame_handle.borrow_mut().take();
            let pending = view.pending_snapshot.borrow_mut().take();
            if let Some(snapshot) = pending {
                let sync_view = sync_runtime::sync_view();
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

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        let svg = self.svg.clone();
        let svg_for_down = svg.clone();
        let core = self.core.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
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
                let Some((view_x, view_y)) =
                    event_to_svg_coords(event, &svg_for_down, snapshot.view)
                else {
                    return;
                };
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                    let sync_view = sync_runtime::sync_view();
                    if piece_owned_by_other(&snapshot, &sync_view, piece_id) {
                        return;
                    }
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
                    view.update_auto_pan(
                        event.client_x() as f32,
                        event.client_y() as f32,
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
                if event.button() == 0 {
                    view.ui_credit_hovered.set(false);
                    *view.pan_state.borrow_mut() = Some(PanState {
                        last_x: event.client_x() as f32,
                        last_y: event.client_y() as f32,
                        touch_id: None,
                    });
                    view.update_svg_class(&snapshot);
                    event.prevent_default();
                }
            },
        );
        listeners.push(listener);

        let listener = EventListener::new_with_options(
            &svg,
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
        let svg_for_wheel = svg.clone();
        let listener = EventListener::new_with_options(
            &svg,
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
                if event.ctrl_key() || event.meta_key() {
                    if let Some((dx_world, dy_world)) =
                        screen_delta_to_world(delta_x, delta_y, &svg_for_wheel, snapshot.view)
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

        let core = self.core.clone();
        let svg_for_move = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new(&svg, "mousemove", move |event: &Event| {
            let Some(event) = event.dyn_ref::<MouseEvent>() else {
                return;
            };
            let snapshot = core.snapshot();
            if !snapshot.dragging_members.is_empty() {
                return;
            }
            if view.pan_state.borrow().is_some() {
                return;
            }
            let Some((view_x, view_y)) = event_to_svg_coords(event, &svg_for_move, snapshot.view)
            else {
                view.dispatch_action(CoreAction::SetHovered { hovered: None });
                return;
            };
            let (px, py) =
                workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            let hovered = view.pick_piece(px, py, &snapshot);
            if hovered.is_some() {
                if view.ui_credit_hovered.get() {
                    view.ui_credit_hovered.set(false);
                    view.update_svg_class(&snapshot);
                }
                view.dispatch_action(CoreAction::SetHovered { hovered });
                return;
            }
            let credit_hovered = view.hit_credit(view_x, view_y);
            if view.ui_credit_hovered.get() != credit_hovered {
                view.ui_credit_hovered.set(credit_hovered);
                view.update_svg_class(&snapshot);
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
        let listener = EventListener::new(&svg, "mouseleave", move |_event: &Event| {
            view.ui_credit_hovered.set(false);
            let snapshot = core_for_leave.snapshot();
            view.update_svg_class(&snapshot);
            view.dispatch_action(CoreAction::SetHovered { hovered: None });
        });
        listeners.push(listener);

        let core = self.core.clone();
        let svg_for_touch = svg.clone();
        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &svg,
            "touchstart",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<TouchEvent>() else {
                    return;
                };
                let snapshot = core.snapshot();
                if event.touches().length() >= 2 {
                    if !snapshot.dragging_members.is_empty() {
                        view.dispatch_action(CoreAction::DragEnd {
                            touch_id: snapshot.drag_touch_id,
                        });
                    }
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
                            if view.pan_state.borrow_mut().take().is_some() {
                                core.settle_view();
                            }
                            view.update_svg_class(&snapshot);
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
                let Some((view_x, view_y)) =
                    screen_to_view_coords(screen_x, screen_y, &svg_for_touch, snapshot.view)
                else {
                    return;
                };
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                    let sync_view = sync_runtime::sync_view();
                    if piece_owned_by_other(&snapshot, &sync_view, piece_id) {
                        return;
                    }
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
                    view.update_auto_pan(screen_x, screen_y, &drag_snapshot);
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
                view.update_svg_class(&snapshot);
                event.prevent_default();
            },
        );
        listeners.push(listener);

        let window = web_sys::window().expect("window available");
        let core = self.core.clone();
        let svg_for_drag = svg.clone();
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
                if !snapshot.dragging_members.is_empty() {
                    view.update_auto_pan(
                        event.client_x() as f32,
                        event.client_y() as f32,
                        &snapshot,
                    );
                    let Some((view_x, view_y)) =
                        event_to_svg_coords(event, &svg_for_drag, snapshot.view)
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
                    screen_delta_to_world(dx, dy, &svg_for_drag, snapshot.view)
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
        let svg_for_up = svg.clone();
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
                let snapshot = core.snapshot();
                view.stop_auto_pan();
                let mut pan_cleared = false;
                if view.pan_state.borrow().is_some() {
                    view.pan_state.borrow_mut().take();
                    pan_cleared = true;
                    view.update_svg_class(&snapshot);
                }
                if pan_cleared {
                    core.settle_view();
                }
                if !snapshot.dragging_members.is_empty() {
                    if let Some((view_x, view_y)) =
                        event_to_svg_coords(event, &svg_for_up, snapshot.view)
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
                let hovered = event_to_svg_coords(event, &svg_for_up, snapshot.view)
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
        let svg_for_touch_move = svg.clone();
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
                let snapshot = core.snapshot();
                if let Some(mut pinch) = view.pinch_state.borrow_mut().take() {
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
                                    &svg_for_touch_move,
                                    snapshot.view,
                                ) {
                                    core.zoom_view_at(factor, view_x, view_y);
                                }
                                pinch.last_distance = distance;
                            }
                        }
                        *view.pinch_state.borrow_mut() = Some(pinch);
                        event.prevent_default();
                        return;
                    }
                }
                if snapshot.dragging_members.is_empty() {
                    let delta = {
                        let mut pan_state = view.pan_state.borrow_mut();
                        if let Some(pan) = pan_state.as_mut() {
                            if let Some(touch) = touch_from_event(event, pan.touch_id, false) {
                                let dx = touch.client_x() as f32 - pan.last_x;
                                let dy = touch.client_y() as f32 - pan.last_y;
                                pan.last_x = touch.client_x() as f32;
                                pan.last_y = touch.client_y() as f32;
                                screen_delta_to_world(dx, dy, &svg_for_touch_move, snapshot.view)
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
                    }
                    return;
                }
                if let Some(touch_id) = snapshot.drag_touch_id {
                    let Some(touch) = touch_from_event(event, Some(touch_id), false) else {
                        return;
                    };
                    let Some((view_x, view_y)) = screen_to_view_coords(
                        touch.client_x() as f32,
                        touch.client_y() as f32,
                        &svg_for_touch_move,
                        snapshot.view,
                    ) else {
                        return;
                    };
                    view.update_auto_pan(
                        touch.client_x() as f32,
                        touch.client_y() as f32,
                        &snapshot,
                    );
                    let (px, py) =
                        workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                    view.dispatch_action(CoreAction::DragMove { x: px, y: py });
                    event.prevent_default();
                }
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let core = self.core.clone();
        let svg_for_touch_end = svg.clone();
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
                let snapshot = core.snapshot();
                view.stop_auto_pan();
                if let Some(pinch) = view.pinch_state.borrow_mut().take() {
                    let touch_a = touch_from_event(event, Some(pinch.touch_a), false);
                    let touch_b = touch_from_event(event, Some(pinch.touch_b), false);
                    if touch_a.is_some() && touch_b.is_some() {
                        *view.pinch_state.borrow_mut() = Some(pinch);
                    }
                }
                let mut pan_cleared = false;
                {
                    let mut pan_state = view.pan_state.borrow_mut();
                    if let Some(pan) = pan_state.as_ref() {
                        if touch_from_event(event, pan.touch_id, true).is_some() {
                            pan_state.take();
                            pan_cleared = true;
                        }
                    }
                }
                if pan_cleared {
                    view.update_svg_class(&snapshot);
                    core.settle_view();
                }
                if snapshot.dragging_members.is_empty() {
                    return;
                }
                if snapshot.drag_touch_id.is_some() {
                    if let Some((view_x, view_y)) =
                        touch_event_to_view_coords(event, &svg_for_touch_end, snapshot.view)
                    {
                        let (px, py) = workspace_to_puzzle_coords(
                            snapshot.layout.puzzle_scale,
                            view_x,
                            view_y,
                        );
                        view.dispatch_action(CoreAction::DragMove { x: px, y: py });
                    }
                }
                view.dispatch_action(CoreAction::DragEnd {
                    touch_id: snapshot.drag_touch_id,
                });
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
                let snapshot = core.snapshot();
                view.pan_state.borrow_mut().take();
                view.pinch_state.borrow_mut().take();
                view.stop_auto_pan();
                view.update_svg_class(&snapshot);
                core.settle_view();
                view.dispatch_action(CoreAction::DragEnd {
                    touch_id: snapshot.drag_touch_id,
                });
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

    fn dispatch_action(&self, action: CoreAction) {
        let core = self.core.clone();
        sync_runtime::dispatch_view_action(&core, action, true);
    }

    fn update_svg_class(&self, snapshot: &AppSnapshot) {
        let mut class = "puzzle-image".to_string();
        if snapshot.show_debug {
            class.push_str(" debug");
        }
        if !snapshot.svg_settings.animations {
            class.push_str(" no-anim");
        }
        if snapshot.svg_settings.fast_render {
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
        if !snapshot.show_debug {
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
        let outer_ratio = snapshot.auto_pan_outer_ratio.max(0.0);
        let inner_ratio = snapshot.auto_pan_inner_ratio.max(outer_ratio);
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
        let mut speed_ratio = snapshot.auto_pan_speed_ratio.max(0.0);
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
        let disconnected =
            matches!(sync_view.mode(), InitMode::Online) && !sync_view.connected();
        if disconnected {
            self.root.set_class_name("app svg sync-disconnected");
        } else {
            self.root.set_class_name("app svg");
        }
        if let Some(preview) = self.preview.borrow().as_ref() {
            if let Some(info) = snapshot.puzzle_info.as_ref() {
                preview.set_image_src(info.image_src.as_str());
                preview.set_visible(true);
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
        self.render_ui(snapshot, sync_view);
        self.render_pieces(snapshot, &assets, sync_view);
        self.update_z_order(snapshot);
    }

    fn clear_scene(&self) {
        self.pieces.borrow_mut().clear();
        self.mask_atlas.borrow_mut().take();
        self.last_assets_ptr.set(0);
        self.last_puzzle.borrow_mut().take();
        self.last_z_order.borrow_mut().clear();
        clear_children(&self.defs);
        clear_children(&self.puzzle_group);
        let _ = self.puzzle_group.append_child(&self.puzzle_bounds);
    }

    fn ensure_scene(&self, snapshot: &AppSnapshot, assets: Rc<PuzzleAssets>) {
        let ptr = Rc::as_ptr(&assets) as usize;
        let settings_changed = {
            let mut last = self.last_svg_settings.borrow_mut();
            if last.as_ref() != Some(&snapshot.svg_settings) {
                *last = Some(snapshot.svg_settings.clone());
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
            self.rebuild_defs(snapshot, &assets);
            self.rebuild_pieces(snapshot, &assets);
        }
    }

    fn rebuild_defs(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets) {
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
            snapshot.svg_settings.fast_filter,
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
            if let Ok(atlas) = build_mask_atlas(
                &assets.pieces,
                &assets.paths,
                assets.piece_width,
                assets.piece_height,
                assets.grid,
                assets.mask_pad,
            ) {
                *self.mask_atlas.borrow_mut() = Some(Rc::new(atlas));
            }
        }
    }

    fn rebuild_pieces(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets) {
        clear_children(&self.puzzle_group);
        let _ = self.puzzle_group.append_child(&self.puzzle_bounds);
        self.pieces.borrow_mut().clear();
        for piece in &assets.pieces {
            let node = self.build_piece_node(snapshot, assets, piece.id);
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
        let _ = image.set_attribute("href", assets.info.image_src.as_str());
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
            hitbox,
            outline_external,
            outline_internal,
            outline_simple,
            back_rect,
            image,
            debug_group,
            debug_label,
            debug_center,
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

    fn render_ui(&self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
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
            if snapshot.connections.len() == total {
                let (connected, border_connected, total_expected, border_expected) =
                    count_connections(&snapshot.connections, cols, rows);
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
        let is_dark = match snapshot.theme_mode {
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
        let _ = sync_view;
    }

    fn render_pieces(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets, sync_view: &dyn GameSyncView) {
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
            if snapshot.connections.len() == total {
                for member in collect_group(
                    &snapshot.connections,
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
                .or_else(|| drag_group_center(&snapshot.positions, &snapshot.dragging_members, assets.piece_width, assets.piece_height))
        } else {
            None
        };
        for (idx, node) in self.pieces.borrow().iter().enumerate() {
            if idx >= total {
                break;
            }
            let current = snapshot.positions.get(idx).copied().unwrap_or((
                (idx as u32 % assets.grid.cols) as f32 * assets.piece_width,
                (idx as u32 / assets.grid.cols) as f32 * assets.piece_height,
            ));
            let rotation = snapshot.rotations.get(idx).copied().unwrap_or(0.0);
            let flipped = snapshot.flips.get(idx).copied().unwrap_or(false);
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
            if piece_owned_by_other(snapshot, sync_view, idx) {
                class.push_str(" owned-other");
            }
            let _ = node.root.set_attribute("class", &class);

            if snapshot.svg_settings.emboss && !flipped {
                let _ = node.surface_group.set_attribute("filter", "url(#emboss)");
            } else {
                let _ = node.surface_group.set_attribute("filter", "none");
            }
            if snapshot.svg_settings.emboss {
                let _ = node.outline_simple.set_attribute("display", "none");
            } else {
                let _ = node.outline_simple.set_attribute("display", "");
            }
            if snapshot.show_debug {
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
            if snapshot.connections.len() == total {
                let connection = snapshot.connections.get(idx).copied().unwrap_or([false; 4]);
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
            &snapshot.positions,
            &snapshot.rotations,
            &snapshot.flips,
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

#[derive(Clone, Copy)]
enum PreviewCorner {
    BottomLeft,
    BottomRight,
    TopLeft,
    TopRight,
}

struct PreviewOverlay {
    root: Element,
    img: HtmlImageElement,
    toggle: Element,
    arrow_h: Element,
    arrow_v: Element,
    slash: Element,
    corner: Cell<PreviewCorner>,
    revealed: Cell<bool>,
    listeners: RefCell<Vec<EventListener>>,
}

impl PreviewOverlay {
    fn new(document: &Document) -> Rc<Self> {
        let root = document
            .create_element("aside")
            .expect("create preview root");
        let toggle = document
            .create_element("button")
            .expect("create preview toggle");
        toggle.set_class_name("preview-toggle");
        let _ = toggle.set_attribute("type", "button");
        let _ = toggle.set_attribute("aria-label", "Show preview");
        let _ = toggle.set_attribute("aria-pressed", "false");
        toggle.set_inner_html(
            r#"<svg class="preview-toggle-icon" viewBox="0 0 24 24" aria-hidden="true">
  <path class="preview-toggle-eye" d="M2 12c2.4-4.2 5.8-6.4 10-6.4s7.6 2.2 10 6.4c-2.4 4.2-5.8 6.4-10 6.4S4.4 16.2 2 12z"/>
  <circle class="preview-toggle-pupil" cx="12" cy="12" r="3.2"/>
  <line class="preview-toggle-slash" x1="5" y1="19" x2="19" y2="5"/>
</svg>"#,
        );
        let slash = toggle
            .query_selector(".preview-toggle-slash")
            .ok()
            .flatten()
            .expect("preview toggle slash");
        let arrow_h = document
            .create_element("button")
            .expect("create preview arrow");
        let _ = arrow_h.set_attribute("type", "button");
        let _ = arrow_h.set_attribute("aria-label", "Move preview horizontally");
        arrow_h.set_inner_html(
            r#"<svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
  <polyline points="4,2 8,6 4,10" />
</svg>"#,
        );
        let arrow_v = document
            .create_element("button")
            .expect("create preview arrow");
        let _ = arrow_v.set_attribute("type", "button");
        let _ = arrow_v.set_attribute("aria-label", "Move preview vertically");
        arrow_v.set_inner_html(
            r#"<svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
  <polyline points="4,2 8,6 4,10" />
</svg>"#,
        );
        let img = HtmlImageElement::new().expect("create preview img");
        img.set_alt("preview");
        let _ = root.append_child(&toggle);
        let _ = root.append_child(&arrow_h);
        let _ = root.append_child(&arrow_v);
        let _ = root.append_child(&img);
        let overlay = Rc::new(Self {
            root,
            img,
            toggle,
            arrow_h,
            arrow_v,
            slash,
            corner: Cell::new(PreviewCorner::BottomLeft),
            revealed: Cell::new(false),
            listeners: RefCell::new(Vec::new()),
        });
        overlay.install_listeners();
        overlay.update_classes();
        overlay
    }

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        let overlay = Rc::clone(self);
        let listener = EventListener::new(&self.toggle, "click", move |_event: &Event| {
            overlay.revealed.set(!overlay.revealed.get());
            overlay.update_classes();
        });
        listeners.push(listener);
        let overlay = Rc::clone(self);
        let listener = EventListener::new(&self.arrow_h, "click", move |_event: &Event| {
            let next = match overlay.corner.get() {
                PreviewCorner::BottomLeft => PreviewCorner::BottomRight,
                PreviewCorner::BottomRight => PreviewCorner::BottomLeft,
                PreviewCorner::TopLeft => PreviewCorner::TopRight,
                PreviewCorner::TopRight => PreviewCorner::TopLeft,
            };
            overlay.corner.set(next);
            overlay.update_classes();
        });
        listeners.push(listener);
        let overlay = Rc::clone(self);
        let listener = EventListener::new(&self.arrow_v, "click", move |_event: &Event| {
            let next = match overlay.corner.get() {
                PreviewCorner::BottomLeft => PreviewCorner::TopLeft,
                PreviewCorner::BottomRight => PreviewCorner::TopRight,
                PreviewCorner::TopLeft => PreviewCorner::BottomLeft,
                PreviewCorner::TopRight => PreviewCorner::BottomRight,
            };
            overlay.corner.set(next);
            overlay.update_classes();
        });
        listeners.push(listener);
        let overlay = Rc::clone(self);
        let listener = EventListener::new(&self.img, "click", move |_event: &Event| {
            if overlay.revealed.get() {
                overlay.revealed.set(false);
                overlay.update_classes();
            }
        });
        listeners.push(listener);
        *self.listeners.borrow_mut() = listeners;
    }

    fn set_visible(&self, visible: bool) {
        if visible {
            let _ = self.root.set_attribute("style", "");
        } else {
            let _ = self.root.set_attribute("style", "display: none;");
        }
    }

    fn set_image_src(&self, src: &str) {
        self.img.set_src(src);
    }

    fn update_classes(&self) {
        let (corner_class, arrow_h_class, arrow_v_class) = match self.corner.get() {
            PreviewCorner::BottomLeft => (
                "corner-bl",
                "preview-arrow preview-arrow-right",
                "preview-arrow preview-arrow-up",
            ),
            PreviewCorner::BottomRight => (
                "corner-br",
                "preview-arrow preview-arrow-left",
                "preview-arrow preview-arrow-up",
            ),
            PreviewCorner::TopLeft => (
                "corner-tl",
                "preview-arrow preview-arrow-right",
                "preview-arrow preview-arrow-down",
            ),
            PreviewCorner::TopRight => (
                "corner-tr",
                "preview-arrow preview-arrow-left",
                "preview-arrow preview-arrow-down",
            ),
        };
        let state_class = if self.revealed.get() {
            "preview-revealed"
        } else {
            "preview-hidden"
        };
        self.root
            .set_class_name(&format!("preview-box {} {}", corner_class, state_class));
        self.arrow_h.set_class_name(arrow_h_class);
        self.arrow_v.set_class_name(arrow_v_class);
        let (label, pressed, slash_display) = if self.revealed.get() {
            ("Hide preview", "true", "block")
        } else {
            ("Show preview", "false", "none")
        };
        let _ = self.toggle.set_attribute("aria-label", label);
        let _ = self.toggle.set_attribute("aria-pressed", pressed);
        let _ = self.slash.set_attribute("style", &format!("display: {slash_display};"));
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

fn screen_to_view_coords(
    screen_x: f32,
    screen_y: f32,
    svg: &Element,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = svg.get_bounding_client_rect();
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

fn event_to_svg_coords(
    event: &MouseEvent,
    svg: &Element,
    view: ViewRect,
) -> Option<(f32, f32)> {
    screen_to_view_coords(
        event.client_x() as f32,
        event.client_y() as f32,
        svg,
        view,
    )
}

fn touch_event_to_view_coords(
    event: &TouchEvent,
    svg: &Element,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let touch = touch_from_event(event, None, true)?;
    screen_to_view_coords(
        touch.client_x() as f32,
        touch.client_y() as f32,
        svg,
        view,
    )
}

fn screen_delta_to_world(
    dx_screen: f32,
    dy_screen: f32,
    svg: &Element,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = svg.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let scale_x = view.width / rect_width;
    let scale_y = view.height / rect_height;
    Some((-dx_screen * scale_x, -dy_screen * scale_y))
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

fn workspace_to_puzzle_coords(scale: f32, x: f32, y: f32) -> (f32, f32) {
    let scale = scale.max(1.0e-4);
    (x / scale, y / scale)
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

fn piece_owned_by_other(
    snapshot: &AppSnapshot,
    sync_view: &dyn GameSyncView,
    piece_id: usize,
) -> bool {
    if matches!(sync_view.mode(), InitMode::Local) {
        return false;
    }
    let ownership = sync_view.ownership_by_anchor();
    if ownership.is_empty() {
        return false;
    }
    let cols = snapshot.grid.cols as usize;
    let rows = snapshot.grid.rows as usize;
    if cols == 0 || rows == 0 {
        return false;
    }
    let total = cols * rows;
    if piece_id >= total {
        return false;
    }
    if snapshot.connections.len() < total {
        return false;
    }
    let mut members = collect_group(&snapshot.connections, piece_id, cols, rows);
    if members.is_empty() {
        members.push(piece_id);
    }
    let anchor_id = members.iter().copied().min().unwrap_or(piece_id);
    if let Some(owner_id) = ownership.get(&(anchor_id as u32)) {
        return Some(*owner_id) != sync_view.client_id();
    }
    false
}

fn open_credit_url() {
    if let Some(window) = web_sys::window() {
        let _ = window.open_with_url_and_target(CREDIT_URL, "_blank");
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
