use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::timers::callback::Timeout;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::closure::Closure;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    CanvasRenderingContext2d, Element, Event, HtmlCanvasElement, HtmlImageElement, PointerEvent,
    WheelEvent,
};

use crate::app_core::{AppCore, AppSnapshot, PuzzleAssets, ViewRect, now_ms_f32};
use crate::app_router;
use crate::boot;
use crate::core::*;
use crate::flip_anim::FlipAnim;
use crate::input::{
    CLICK_MAX_DURATION_MS, PointerKind, PointerPolicy, WheelIntent, WheelIntentTracker,
    screen_delta_to_world, screen_scroll_to_world, screen_slop_to_puzzle, screen_to_view_coords,
    workspace_to_puzzle_coords,
};
use crate::puzzle_image::{resolve_puzzle_credit, resolve_puzzle_image_src};
use crate::renderer::{
    EdgeVertex, FlipEdgeSpan, FlipEdges, Instance, InstanceBatch, InstanceSet, MaskAtlasData,
    UiRotationOrigin, UiSpriteSpec, UiSpriteTexture, UiTextId, UiTextSpec, WgpuRenderer,
    build_mask_atlas,
};
use crate::rotation_anim::{
    GroupAnim, SpringParams, apply_group_transform, rotate_group_succession,
};
use crate::runtime::{CoreAction, GameSyncView, GameView, ViewHooks};
use crate::sync_runtime;
use crate::view_common::Rect;
use crate::view_common::*;
use crate::view_runtime;
use crate::web::{now_ms, open_url, prefers_dark_mode};
use heddobureika_core::{ClientId, PuzzleImageRef};

/// Real-world size one pose unit represents (≈ a piece pitch). The puzzle's
/// internal "mm" pose units are actually nominal (~1 unit per piece, see
/// `topology::piece_extent_mm`), so to turn a physical millimetre count into
/// pixels we go through this assumed pitch: `px_per_mm = pose_unit_px /
/// POSE_UNIT_MM`. A standard jigsaw piece is ~2.5 cm, so 2 mm lands as a thin
/// edge that scales with the piece (zoom/piece-count independent).
const POSE_UNIT_MM: f32 = 25.0;
/// A piece counts as "mid-flip" (needs the extrusion pass) when its flip
/// progress is this far from both 0 and 1.
const FLIP_ACTIVE_EPS: f32 = 0.001;
const UI_DEBUG_FONT_RATIO: f32 = 0.02;
const OUTLINE_KIND_HOVER: f32 = 1.0;
const OUTLINE_KIND_OWNED: f32 = 2.0;
const OUTLINE_KIND_DEBUG: f32 = 3.0;
const WGPU_FPS_CAP_DEFAULT: f32 = 60.0;
const WGPU_FPS_IDLE_RESET_MS: f32 = 800.0;
const PREVIEW_PANEL_RADIUS: f32 = 12.0;
const PREVIEW_PANEL_BORDER: f32 = 1.0;
const PREVIEW_HIDDEN_BLUR_PX: f32 = 6.0;
const PREVIEW_HIDDEN_OPACITY: f32 = 0.6;
const PREVIEW_ANIM_SPEED: f32 = 12.0;
const PREVIEW_TILT_DEG: f32 = 1.1;
// Keep in sync with ViewState::fit_zoom_for_size in src/app_core.rs.
const PREVIEW_MAX_FIT_SCALE: f32 = 0.97;

struct TouchDragGate {
    pointer_id: i32,
    start_x: f32,
    start_y: f32,
    moved: bool,
}

struct PreviewColors {
    panel_bg: [f32; 4],
    panel_border: [f32; 4],
}

thread_local! {
    static WGPU_VIEW_ADAPTER: RefCell<Option<Rc<RefCell<WgpuViewAdapter>>>> = RefCell::new(None);
    static WARNED_INVALID_Z_ORDER: Cell<bool> = Cell::new(false);
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

        boot::set_phase("Initializing", "Starting GPU renderer.");
        root.set_class_name("app wgpu");

        let canvas = document
            .create_element("canvas")
            .expect("create canvas")
            .dyn_into::<HtmlCanvasElement>()
            .expect("canvas element");
        canvas.set_class_name("puzzle-canvas");
        root.append_child(&canvas).expect("append canvas");
        let render_settings = app_router::load_render_settings_with_init();
        let view = Rc::new(WgpuView::new(
            core.clone(),
            root.clone(),
            canvas,
            render_settings.wgpu.clone(),
        ));
        let debug_overlay = DebugOverlay::new(&document);
        root.append_child(&debug_overlay.root).ok();
        *view.shell.debug_overlay.borrow_mut() = Some(debug_overlay);
        *view.sync_hook.borrow_mut() = Some(sync_runtime::register_sync_view_hook(Rc::new({
            let view = view.clone();
            move || view.request_render()
        })));
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
        // Subscribers can run re-entrantly (a nested notify during render),
        // so the render subscription must not hold a RefCell borrow while
        // queueing; go through the view directly instead of the adapter.
        let core_for_render = core.clone();
        let view_for_render = view.clone();
        let subscription = core.subscribe(Rc::new(move || {
            view_for_render.queue_render_snapshot(core_for_render.snapshot());
        }));
        *view.subscription.borrow_mut() = Some(subscription);
        view_runtime::set_active_view(Some(Rc::new(view.clone())));
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
    shell: ViewShell,
    root: Element,
    canvas: HtmlCanvasElement,
    image: RefCell<Option<HtmlImageElement>>,
    renderer: RefCell<Option<WgpuRenderer>>,
    backend_label: RefCell<Option<String>>,
    creating: Cell<bool>,
    pending_instances: RefCell<Option<InstanceSet>>,
    pending_ui: RefCell<Option<Vec<UiTextSpec>>>,
    ui_credit_hovered: Cell<bool>,
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    touch_drag_gate: RefCell<Option<TouchDragGate>>,
    pointer_policy: RefCell<PointerPolicy>,
    pending_snapshot: RefCell<Option<AppSnapshot>>,
    render_timer: RefCell<Option<Timeout>>,
    idle_fps_timer: RefCell<Option<Timeout>>,
    last_render_ms: Cell<f32>,
    idle_fps_rendered: Cell<bool>,
    force_fps_fallback: Cell<bool>,
    subscription: RefCell<Option<crate::app_core::AppSubscription>>,
    listeners: RefCell<Vec<EventListener>>,
    last_view_size: Cell<(u32, u32)>,
    puzzle_epoch: Cell<u64>,
    sync_hook: RefCell<Option<sync_runtime::SyncViewHookHandle>>,
    preview_blur: Cell<f32>,
    preview_target_blur: Cell<f32>,
    preview_anim_last_ms: Cell<f32>,
    pending_ui_overlay: RefCell<Option<Vec<UiSpriteSpec>>>,
    wgpu_settings: RefCell<WgpuRenderSettings>,
    wheel_intent: RefCell<WheelIntentTracker>,
    /// Per-group spring state for the optional rotation animation, keyed by the
    /// canonical group anchor (`AppSnapshot::piece_group_anchor`). Empty when the
    /// feature is off. See `animate_rotations` and `crate::rotation_anim`.
    rotate_anim: RefCell<HashMap<u32, GroupAnim>>,
    rotate_anim_last_ms: Cell<f32>,
    /// Previous frame's `AppSnapshot::piece_group_anchor`, so `animate_rotations`
    /// can detect a merge that re-keyed a group onto a new anchor and carry its
    /// spring state across (instead of reseeding `settled`, which would jump a
    /// mid-flight group). Empty when the feature is off.
    rotate_anim_prev_anchors: RefCell<Vec<u32>>,
    /// Per-group flip spring state for the optional flip animation, keyed by
    /// group anchor. Shares the `rotate_anim` toggle and spring params. Empty
    /// when the feature is off. See `animate_flips` and `crate::flip_anim`.
    flip_anim: RefCell<HashMap<u32, FlipAnim>>,
    flip_anim_last_ms: Cell<f32>,
}

impl ViewShellHost for WgpuView {
    fn shell(&self) -> &ViewShell {
        &self.shell
    }

    fn request_render(self: &Rc<Self>) {
        self.queue_render_snapshot(self.shell.core.snapshot());
    }

    fn private_image_ready(self: &Rc<Self>, key: PuzzleKey, url: String) {
        let image_max_dim = self.shell.core.image_max_dim();
        let render_scale = self.wgpu_settings.borrow().render_scale;
        self.load_image_for_puzzle(key, url, image_max_dim, render_scale);
    }
}

impl WgpuView {
    fn new(
        core: Rc<AppCore>,
        root: Element,
        canvas: HtmlCanvasElement,
        wgpu_settings: WgpuRenderSettings,
    ) -> Self {
        Self {
            shell: ViewShell::new(
                core,
                canvas.clone().into(),
                PREVIEW_TILT_DEG,
                PREVIEW_MAX_FIT_SCALE,
            ),
            root,
            canvas,
            image: RefCell::new(None),
            renderer: RefCell::new(None),
            backend_label: RefCell::new(None),
            creating: Cell::new(false),
            pending_instances: RefCell::new(None),
            pending_ui: RefCell::new(None),
            ui_credit_hovered: Cell::new(false),
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            touch_drag_gate: RefCell::new(None),
            pointer_policy: RefCell::new(PointerPolicy::new()),
            pending_snapshot: RefCell::new(None),
            render_timer: RefCell::new(None),
            idle_fps_timer: RefCell::new(None),
            last_render_ms: Cell::new(0.0),
            idle_fps_rendered: Cell::new(false),
            force_fps_fallback: Cell::new(false),
            subscription: RefCell::new(None),
            listeners: RefCell::new(Vec::new()),
            last_view_size: Cell::new((0, 0)),
            puzzle_epoch: Cell::new(0),
            sync_hook: RefCell::new(None),
            preview_blur: Cell::new(PREVIEW_HIDDEN_BLUR_PX),
            preview_target_blur: Cell::new(PREVIEW_HIDDEN_BLUR_PX),
            preview_anim_last_ms: Cell::new(0.0),
            pending_ui_overlay: RefCell::new(None),
            wgpu_settings: RefCell::new(wgpu_settings),
            wheel_intent: RefCell::new(WheelIntentTracker::new()),
            rotate_anim: RefCell::new(HashMap::new()),
            rotate_anim_last_ms: Cell::new(0.0),
            rotate_anim_prev_anchors: RefCell::new(Vec::new()),
            flip_anim: RefCell::new(HashMap::new()),
            flip_anim_last_ms: Cell::new(0.0),
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
            self.shell.last_puzzle.borrow_mut().take();
        }
        if !settings.rotate_anim {
            // Drop spring state so the next frame snaps straight to the
            // authoritative poses/flip (identical to the un-animated path).
            self.rotate_anim.borrow_mut().clear();
            self.rotate_anim_last_ms.set(0.0);
            self.flip_anim.borrow_mut().clear();
            self.flip_anim_last_ms.set(0.0);
        }
        self.request_render();
    }

    fn reset_puzzle_cache(&self) {
        self.puzzle_epoch
            .set(self.puzzle_epoch.get().wrapping_add(1));
        self.shell.clear_private_image_cache();
        self.creating.set(false);
        self.renderer.borrow_mut().take();
        self.shell.mask_atlas.borrow_mut().take();
        self.pending_instances.borrow_mut().take();
        self.pending_ui.borrow_mut().take();
        self.pending_ui_overlay.borrow_mut().take();
        self.shell.preview_drag.borrow_mut().take();
        self.shell.preview_click.borrow_mut().take();
        self.shell.preview_seeded.set(false);
        self.shell.preview_velocity.set([0.0, 0.0]);
        self.shell.preview_motion_last_ms.set(0.0);
        self.rotate_anim.borrow_mut().clear();
        self.rotate_anim_last_ms.set(0.0);
        self.flip_anim.borrow_mut().clear();
        self.flip_anim_last_ms.set(0.0);
        *self.image.borrow_mut() = None;
    }

    fn update_viewport_size(&self) {
        let rect = self.root.get_bounding_client_rect();
        let width = rect.width() as f32;
        let height = rect.height() as f32;
        self.shell.core.set_viewport_size(width, height);
    }

    fn clear_hover_state(self: &Rc<Self>, snapshot: &AppSnapshot) {
        let mut changed = snapshot.hovered_id.is_some();
        if self.ui_credit_hovered.get() {
            self.ui_credit_hovered.set(false);
            changed = true;
        }
        if self.shell.set_preview_hover(PreviewHoverTarget::None) {
            changed = true;
        }
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered: None });
        if changed {
            self.update_canvas_class(snapshot);
            self.request_render();
        }
    }

    fn handle_pointer_down(self: &Rc<Self>, event: &PointerEvent) {
        let kind = ViewShell::pointer_kind(event);
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
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let _ = self.canvas.set_pointer_capture(pointer_id);
        let rect = self.canvas.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                self.touch_drag_gate.borrow_mut().take();
                event.prevent_default();
                if !snapshot.dragging_members.is_empty() {
                    self.shell.dispatch_action(CoreAction::DragEnd {
                        pointer_id: snapshot.drag_pointer_id,
                    });
                }
                let (id_a, a) = touch_points[0];
                let (id_b, b) = touch_points[1];
                let local_ax = a.screen_x - rect.left() as f32;
                let local_ay = a.screen_y - rect.top() as f32;
                let local_bx = b.screen_x - rect.left() as f32;
                let local_by = b.screen_y - rect.top() as f32;
                if let Some(layout) = self.shell.preview_layout(&snapshot, &rect) {
                    if self.shell.preview_point_inside(&layout, local_ax, local_ay)
                        && self.shell.preview_point_inside(&layout, local_bx, local_by)
                    {
                        self.shell.clear_preview_click();
                        if self.shell.begin_preview_pinch(
                            id_a,
                            id_b,
                            [a.screen_x, a.screen_y],
                            [b.screen_x, b.screen_y],
                            &snapshot,
                            &rect,
                        ) {
                            self.update_canvas_class(&snapshot);
                            self.request_render();
                            event.prevent_default();
                            return;
                        }
                    }
                }
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
                        self.shell.core.settle_view();
                    }
                    self.update_canvas_class(&snapshot);
                    event.prevent_default();
                }
                return;
            }
        }
        if let Some(layout) = self.shell.preview_layout(&snapshot, &rect) {
            let target = self.shell.preview_hit_test(&layout, local_x, local_y);
            if target != PreviewHoverTarget::None {
                if button == 0 {
                    let [drag_x, drag_y] =
                        self.shell.preview_local_point(&layout, local_x, local_y);
                    self.shell
                        .begin_preview_drag(target, drag_x, drag_y, Some(pointer_id));
                    if matches!(target, PreviewHoverTarget::Body) {
                        self.shell
                            .arm_preview_click(Some(pointer_id), local_x, local_y);
                    } else {
                        self.shell.clear_preview_click();
                    }
                    self.request_render();
                }
                if self.shell.set_preview_hover(target) {
                    self.request_render();
                }
                self.update_canvas_class(&snapshot);
                event.prevent_default();
                return;
            }
        }
        self.shell.clear_preview_click();
        let Some((view_x, view_y)) =
            screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
        else {
            return;
        };
        let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
        if let Some(piece_id) = self.shell.pick_piece(px, py, &snapshot) {
            self.touch_drag_gate.borrow_mut().take();
            let right_click = button == 2;
            if matches!(kind, PointerKind::Touch | PointerKind::Pen) {
                *self.touch_drag_gate.borrow_mut() = Some(TouchDragGate {
                    pointer_id,
                    start_x: px,
                    start_y: py,
                    moved: false,
                });
            }
            let click_slop = screen_slop_to_puzzle(
                TOUCH_DRAG_SLOP_PX,
                &self.canvas,
                snapshot.view,
                snapshot.layout.puzzle_scale,
            );
            self.shell.dispatch_action(CoreAction::BeginDrag {
                piece_id,
                x: px,
                y: py,
                shift_key: event.shift_key(),
                rotate_mode: event.ctrl_key(),
                right_click,
                click_slop,
                pointer_id: Some(pointer_id),
            });
            let drag_snapshot = self.shell.core.snapshot();
            self.update_auto_pan(screen_x, screen_y, &drag_snapshot);
            event.prevent_default();
            return;
        }
        if self.shell.hit_credit(view_x, view_y) {
            open_url(CREDIT_URL);
            event.prevent_default();
            return;
        }
        if let Some(url) = self.shell.hit_art_credit(view_x, view_y) {
            open_url(url);
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
            self.update_canvas_class(&snapshot);
            event.prevent_default();
        }
    }

    fn handle_pointer_move(self: &Rc<Self>, event: &PointerEvent) {
        let kind = ViewShell::pointer_kind(event);
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
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.canvas.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        self.shell
            .update_preview_click_movement(Some(pointer_id), local_x, local_y);
        if let Some(PreviewDragState::Pinch {
            touch_a,
            touch_b,
            start_distance,
            start_width,
            anchor_rel,
        }) = *self.shell.preview_drag.borrow()
        {
            let (point_a, point_b) = {
                let policy = self.pointer_policy.borrow();
                (
                    policy.pointer_sample(touch_a),
                    policy.pointer_sample(touch_b),
                )
            };
            if let (Some(a), Some(b)) = (point_a, point_b) {
                if self.shell.update_preview_pinch(
                    [a.screen_x, a.screen_y],
                    [b.screen_x, b.screen_y],
                    &snapshot,
                    &rect,
                    now_ms,
                    start_distance,
                    start_width,
                    anchor_rel,
                ) {
                    self.request_render();
                    event.prevent_default();
                    return;
                }
            }
        }
        if self.shell.update_preview_drag(
            Some(pointer_id),
            local_x,
            local_y,
            now_ms,
            &snapshot,
            &rect,
        ) {
            self.request_render();
            event.prevent_default();
            return;
        }
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
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
                            if let Some((view_x, view_y)) = screen_to_view_coords(
                                center_x,
                                center_y,
                                &self.canvas,
                                snapshot.view,
                            ) {
                                self.shell.core.zoom_view_at(factor, view_x, view_y);
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
                    screen_delta_to_world(dx, dy, &self.canvas, snapshot.view)
                }
            } else {
                None
            }
        };
        if let Some((dx_world, dy_world)) = delta {
            self.shell.core.pan_view(dx_world, dy_world);
            event.prevent_default();
            return;
        }
        if !snapshot.dragging_members.is_empty() {
            if snapshot.drag_pointer_id != Some(pointer_id) {
                return;
            }
            let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
            else {
                return;
            };
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(gate) = self.touch_drag_gate.borrow_mut().as_mut() {
                if let Some(drag_id) = snapshot.drag_pointer_id {
                    if gate.pointer_id != drag_id {
                        gate.moved = true;
                    } else if !gate.moved {
                        let click_tolerance = snapshot.min_piece_extent_px[0]
                            .min(snapshot.min_piece_extent_px[1])
                            * CLICK_MOVE_RATIO;
                        let slop = screen_slop_to_puzzle(
                            TOUCH_DRAG_SLOP_PX,
                            &self.canvas,
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
            self.shell
                .dispatch_action(CoreAction::DragMove { x: px, y: py });
            event.prevent_default();
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) || buttons != 0 {
            return;
        }
        if self.shell.preview_drag.borrow().is_some() {
            return;
        }
        if self.pan_state.borrow().is_some() {
            return;
        }
        let mut preview_target = PreviewHoverTarget::None;
        if let Some(layout) = self.shell.preview_layout(&snapshot, &rect) {
            preview_target = self.shell.preview_hit_test(&layout, local_x, local_y);
        }
        if self.shell.set_preview_hover(preview_target) {
            self.update_canvas_class(&snapshot);
            self.request_render();
        }
        if preview_target != PreviewHoverTarget::None {
            if self.ui_credit_hovered.get() {
                self.ui_credit_hovered.set(false);
                self.update_canvas_class(&snapshot);
            }
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        }
        let Some((view_x, view_y)) =
            screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
        else {
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        };
        let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
        let hovered = self.shell.pick_piece(px, py, &snapshot);
        if hovered.is_some() {
            if self.ui_credit_hovered.get() {
                self.ui_credit_hovered.set(false);
                self.update_canvas_class(&snapshot);
            }
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered });
            return;
        }
        let credit_hovered = self.shell.hit_credit(view_x, view_y)
            || self.shell.hit_art_credit(view_x, view_y).is_some();
        if self.ui_credit_hovered.get() != credit_hovered {
            self.ui_credit_hovered.set(credit_hovered);
            self.update_canvas_class(&snapshot);
        }
        if credit_hovered {
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        }
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_pointer_up(self: &Rc<Self>, event: &PointerEvent) {
        let kind = ViewShell::pointer_kind(event);
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
        let _ = self.canvas.release_pointer_capture(pointer_id);
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.canvas.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        self.shell
            .update_preview_click_movement(Some(pointer_id), local_x, local_y);
        let preview_clicked = self.shell.consume_preview_click(Some(pointer_id));
        let mut preview_dragged = false;
        let pinch_ids = {
            let drag_ref = self.shell.preview_drag.borrow();
            match drag_ref.as_ref() {
                Some(PreviewDragState::Pinch {
                    touch_a, touch_b, ..
                }) => Some((*touch_a, *touch_b)),
                _ => None,
            }
        };
        if let Some((touch_a, touch_b)) = pinch_ids {
            if pointer_id == touch_a || pointer_id == touch_b {
                preview_dragged = self.shell.end_preview_drag(None);
            }
        }
        if !preview_dragged {
            preview_dragged = self.shell.end_preview_drag(Some(pointer_id));
        }
        if preview_clicked {
            self.toggle_preview_revealed();
        }
        if preview_dragged || preview_clicked {
            self.request_render();
            event.prevent_default();
            return;
        }
        self.touch_drag_gate.borrow_mut().take();
        self.shell.stop_auto_pan();
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
            self.update_canvas_class(&snapshot);
            self.shell.core.settle_view();
        }
        if !snapshot.dragging_members.is_empty() {
            if let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
            {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                let should_send_move = snapshot
                    .drag_cursor
                    .map(|(last_x, last_y)| {
                        let dx = px - last_x;
                        let dy = py - last_y;
                        let slop = snapshot.min_piece_extent_px[0]
                            .min(snapshot.min_piece_extent_px[1])
                            * CLICK_MOVE_RATIO;
                        dx * dx + dy * dy > slop * slop
                    })
                    .unwrap_or(true);
                if should_send_move {
                    self.shell
                        .dispatch_action(CoreAction::DragMove { x: px, y: py });
                }
            }
            self.shell.dispatch_action(CoreAction::DragEnd {
                pointer_id: Some(pointer_id),
            });
        }
        let snapshot = self.shell.core.snapshot();
        if !snapshot.dragging_members.is_empty() {
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) {
            return;
        }
        let hovered = screen_to_view_coords(screen_x, screen_y, &self.canvas, snapshot.view)
            .and_then(|(view_x, view_y)| {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.shell.pick_piece(px, py, &snapshot)
            });
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_pointer_cancel(self: &Rc<Self>, event: &PointerEvent) {
        let kind = ViewShell::pointer_kind(event);
        let pointer_id = event.pointer_id();
        let now_ms = now_ms();
        {
            let mut policy = self.pointer_policy.borrow_mut();
            if !policy.accept_kind(kind, now_ms) {
                return;
            }
            policy.remove_pointer(pointer_id);
        }
        let _ = self.canvas.release_pointer_capture(pointer_id);
        self.pan_state.borrow_mut().take();
        self.pinch_state.borrow_mut().take();
        self.touch_drag_gate.borrow_mut().take();
        self.shell.preview_drag.borrow_mut().take();
        self.shell.preview_click.borrow_mut().take();
        self.shell.preview_velocity.set([0.0, 0.0]);
        self.shell.stop_auto_pan();
        let snapshot = self.shell.core.snapshot();
        self.update_canvas_class(&snapshot);
        self.shell.core.settle_view();
        self.shell.dispatch_action(CoreAction::DragEnd {
            pointer_id: snapshot.drag_pointer_id,
        });
    }

    fn handle_pointer_leave(self: &Rc<Self>, event: &PointerEvent) {
        let kind = ViewShell::pointer_kind(event);
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
        if self.shell.set_preview_hover(PreviewHoverTarget::None) {
            self.request_render();
        }
        let snapshot = self.shell.core.snapshot();
        self.update_canvas_class(&snapshot);
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered: None });
    }

    fn load_image_for_puzzle(
        self: &Rc<Self>,
        key: PuzzleKey,
        src: String,
        image_max_dim: u32,
        render_scale: f32,
    ) {
        boot::set_phase("Decoding image", "Loading puzzle artwork.");
        let img = HtmlImageElement::new().expect("create image element");
        let img_clone = img.clone();
        let view = Rc::clone(self);
        let key_for_onload = key.clone();
        let key_for_error = key.clone();
        let render_scale = render_scale.clamp(WGPU_RENDER_SCALE_MIN, WGPU_RENDER_SCALE_MAX);
        let source_cap = (IMAGE_MAX_DIMENSION_MAX as f32 * WGPU_RENDER_SCALE_MAX)
            .round()
            .max(1.0) as u32;
        let mut source_max_dim = ((image_max_dim as f32) * render_scale).round().max(1.0) as u32;
        if source_max_dim > source_cap {
            source_max_dim = source_cap;
        }
        let onload = Closure::<dyn FnMut()>::wrap(Box::new(move || {
            if !view.shell.is_current_puzzle(&key_for_onload) {
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
                    boot::fail(
                        "image-read",
                        "Failed to prepare the puzzle image.",
                        "Check the URL or CORS headers.",
                    );
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
                boot::fail(
                    "image-read",
                    "Failed to read the puzzle image.",
                    "Check the URL or CORS headers.",
                );
                return;
            }
            let data_url = match canvas.to_data_url() {
                Ok(data_url) => data_url,
                Err(_) => {
                    boot::fail(
                        "image-read",
                        "Failed to read the puzzle image.",
                        "Check the URL or CORS headers.",
                    );
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
                if !view_scaled.shell.is_current_puzzle(&key_scaled) {
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
        let view_error = Rc::clone(self);
        let onerror = Closure::<dyn FnMut()>::wrap(Box::new(move || {
            if !view_error.shell.is_current_puzzle(&key_for_error) {
                return;
            }
            boot::fail(
                "image-load",
                "Failed to load the puzzle image.",
                "Check the URL or CORS headers.",
            );
        }));
        img.set_onerror(Some(onerror.as_ref().unchecked_ref()));
        onerror.forget();
        img.set_src(&src);
        onload.forget();
    }

    fn ensure_puzzle_image(self: &Rc<Self>, snapshot: &AppSnapshot) {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            self.shell.last_puzzle.replace(None);
            self.shell.clear_private_image_cache();
            return;
        };
        let key = PuzzleKey::from_info(info);
        {
            let mut last = self.shell.last_puzzle.borrow_mut();
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
        let src = match &info.image_ref {
            PuzzleImageRef::Private { hash } => self.ensure_private_image_src(hash, key.clone()),
            _ => resolve_puzzle_image_src(&info.image_ref),
        };
        let Some(src) = src else {
            return;
        };
        let image_max_dim = self.shell.core.image_max_dim();
        let render_scale = self.wgpu_settings.borrow().render_scale;
        self.load_image_for_puzzle(key, src, image_max_dim, render_scale);
    }

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        let canvas = self.canvas.clone();

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "pointerdown",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_pointer_down(event);
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "pointermove",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_pointer_move(event);
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "pointerup",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_pointer_up(event);
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "pointercancel",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: false,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_pointer_cancel(event);
            },
        );
        listeners.push(listener);

        let view = Rc::clone(self);
        let listener = EventListener::new_with_options(
            &canvas,
            "pointerleave",
            EventListenerOptions {
                phase: EventListenerPhase::Bubble,
                passive: true,
            },
            move |event: &Event| {
                let Some(event) = event.dyn_ref::<PointerEvent>() else {
                    return;
                };
                view.handle_pointer_leave(event);
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

        let core = self.shell.core.clone();
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
                let intent = view.wheel_intent.borrow_mut().decide(event, now_ms());
                let wants_pan =
                    (event.ctrl_key() || event.meta_key()) && matches!(intent, WheelIntent::Pan);
                if wants_pan {
                    if let Some((dx_world, dy_world)) =
                        screen_scroll_to_world(delta_x, delta_y, &canvas_for_wheel, snapshot.view)
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

        let window = web_sys::window().expect("window available");
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

    fn update_canvas_class(&self, snapshot: &AppSnapshot) {
        let mut canvas_class = "puzzle-canvas".to_string();
        let is_panning = self.pan_state.borrow().is_some();
        let preview_target = self.shell.preview_hover.get();
        let preview_hovered = preview_target != PreviewHoverTarget::None
            || self.shell.preview_drag.borrow().is_some();
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
        if let Some(hovered_id) = snapshot.hovered_id {
            let sync_view = sync_runtime::sync_view();
            if foreign_ownership(&sync_view).is_some_and(|(ownership, client_id)| {
                piece_owned_by_other(snapshot, &ownership, client_id, hovered_id)
            }) {
                canvas_class.push_str(" owned-other");
            }
        }
        if self.ui_credit_hovered.get() || preview_hovered {
            canvas_class.push_str(" ui-link-hover");
        }
        let drag_target = self
            .shell
            .preview_drag
            .borrow()
            .as_ref()
            .map(|drag| match drag {
                PreviewDragState::Move { .. } => PreviewHoverTarget::Body,
                PreviewDragState::Resize { handle, .. } => PreviewHoverTarget::Resize(*handle),
                PreviewDragState::Pinch { .. } => {
                    PreviewHoverTarget::Resize(PreviewResizeHandle::BottomRight)
                }
            });
        let cursor_target = drag_target.unwrap_or(preview_target);
        if let Some(cursor_class) = preview_cursor_class(cursor_target) {
            canvas_class.push_str(" ");
            canvas_class.push_str(cursor_class);
        }
        if matches!(
            self.shell
                .preview_drag
                .borrow()
                .as_ref()
                .map(|drag| drag.kind()),
            Some(PreviewDragKind::Move)
        ) {
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

    /// Optional rotation animation (WGPU only). Returns `(positions, rotations,
    /// animating)` where the arrays are the snapshot's per-piece pixel positions
    /// and degrees with each group's rigid pose spring-smoothed toward its
    /// authoritative target. When the feature is off, settled, or a group is
    /// being dragged, the output is identical to the inputs (`animating` false).
    ///
    /// Each group is treated as one solid. A click-to-rotate is a rigid motion
    /// about a fixed pole (the clicked point); we recover that pole, spring the
    /// angle, and derive the displayed centroid from it so the pole stays put for
    /// the whole transition — see [`crate::rotation_anim`]. The displayed pose is
    /// turned into a per-member render transform `D(p) = R(φ)·(p − c_t) + c_d`
    /// applied to each piece's anchor point via [`apply_group_transform`]; at
    /// settle `φ = 0` and `c_d = c_t`, so `D` is the identity. Spring state
    /// persists per group across frames, so re-rotating mid-flight carries the
    /// current velocity into the new target/pole without a jump.
    fn animate_rotations(
        &self,
        now_ms: f32,
        snapshot: &AppSnapshot,
        geometry: &PuzzleRenderGeometry,
        positions: &[(f32, f32)],
        rotations: &[f32],
    ) -> (Vec<(f32, f32)>, Vec<f32>, bool) {
        let params = {
            let settings = self.wgpu_settings.borrow();
            if !settings.rotate_anim || snapshot.game.is_none() {
                drop(settings);
                self.rotate_anim.borrow_mut().clear();
                self.rotate_anim_prev_anchors.borrow_mut().clear();
                self.rotate_anim_last_ms.set(now_ms);
                return (positions.to_vec(), rotations.to_vec(), false);
            }
            SpringParams {
                response: settings
                    .rotate_anim_response
                    .clamp(WGPU_ROTATE_ANIM_RESPONSE_MIN, WGPU_ROTATE_ANIM_RESPONSE_MAX),
                damping: settings
                    .rotate_anim_damping
                    .clamp(WGPU_ROTATE_ANIM_DAMPING_MIN, WGPU_ROTATE_ANIM_DAMPING_MAX),
            }
        };

        let total = positions.len();
        let anchors = &snapshot.piece_group_anchor;
        // Group member ids by canonical anchor.
        let mut groups: HashMap<u32, Vec<usize>> = HashMap::new();
        for id in 0..total {
            let anchor = anchors.get(id).copied().unwrap_or(id as u32);
            groups.entry(anchor).or_default().push(id);
        }
        // Anchors of groups currently being dragged: keep them live (snap to
        // target) so the existing drag transform in `build_wgpu_instances`
        // applies cleanly without a competing spring.
        let mut dragged: Vec<u32> = Vec::new();
        for &m in &snapshot.dragging_members {
            if let Some(a) = anchors.get(m).copied() {
                if !dragged.contains(&a) {
                    dragged.push(a);
                }
            }
        }

        let last = self.rotate_anim_last_ms.get();
        let dt = if last > 0.0 {
            ((now_ms - last).max(0.0).min(50.0)) / 1000.0
        } else {
            0.0
        };
        self.rotate_anim_last_ms.set(now_ms);

        let mut new_positions = positions.to_vec();
        let mut new_rotations = rotations.to_vec();
        let mut animating = false;
        let mut map = self.rotate_anim.borrow_mut();

        // Re-key spring state across merges and splits by membership overlap, so
        // a group's animation follows its *identity* rather than its volatile
        // canonical-anchor id. `map` is still keyed by the PREVIOUS frame's
        // anchors here. Match each previous entry to the current group sharing
        // the most of its former members (its primary successor), move the entry
        // there (rebased to the successor's reference), and drop entries with no
        // winning successor. This handles a merge (the entry follows the bulk of
        // its members into the combined group) and a split (the entry follows the
        // largest fragment; smaller fragments seed fresh `settled` state) without
        // depending on `HashMap` iteration order — see `rotate_group_succession`.
        if !map.is_empty() {
            let prev = self.rotate_anim_prev_anchors.borrow();
            let dest = rotate_group_succession(total, &prev, anchors, |a| map.contains_key(&a));
            drop(prev);
            // Relocate winners (rebasing when the key actually moves); drop the
            // rest (they were absorbed into another group's entry).
            let old = std::mem::take(&mut *map);
            for (prev_a, mut anim) in old {
                let Some(&cur_a) = dest.get(&prev_a) else {
                    continue;
                };
                if prev_a != cur_a {
                    if let Some(members) = groups.get(&cur_a) {
                        if let Some(new_ref) =
                            group_reference_point(positions, cur_a, members, geometry)
                        {
                            let old_ref = anim.tracked_reference();
                            anim.rebase_reference(old_ref, new_ref);
                        }
                    }
                }
                map.insert(cur_a, anim);
            }
        }

        for (anchor, members) in &groups {
            // Use a *stable* reference point (the canonical-anchor piece's anchor)
            // rather than the membership mean: adding pieces on a merge must not
            // shift the reference, or the group jumps by the centroid delta.
            let Some(center) = group_reference_point(positions, *anchor, members, geometry) else {
                continue;
            };
            let (c_t_x, c_t_y) = center;
            // Shared rigid angle: every member's world rotation changes by the
            // same amount, so the anchor member (== canonical id) is a valid
            // reference.
            let ref_id = members
                .iter()
                .copied()
                .find(|&m| m as u32 == *anchor)
                .unwrap_or_else(|| members.iter().copied().min().unwrap_or(0));
            let target_angle = rotations.get(ref_id).copied().unwrap_or(0.0);

            let entry = map
                .entry(*anchor)
                .or_insert_with(|| GroupAnim::settled(c_t_x, c_t_y, target_angle));

            if dragged.contains(anchor) {
                entry.snap_to(c_t_x, c_t_y, target_angle);
                continue; // identity transform; leave snapshot values as-is
            }
            // Force the animation to follow the click direction for a local
            // rotate (so a clockwise click never animates counter-clockwise, even
            // when the target's shortest path is the other way), and pivot about
            // the exact click point rather than a pole inferred from a possibly
            // still-settling displayed pose. Network rotations carry no hint and
            // use the shortest path with a recovered pole.
            let (dir_hint, pivot) = match snapshot.last_local_rotation {
                Some(hint) if hint.anchor == *anchor => (hint.sign, Some(hint.pivot)),
                _ => (0.0, None),
            };
            if !entry.step(c_t_x, c_t_y, target_angle, params, dt, dir_hint, pivot) {
                continue; // settled => identity transform
            }
            animating = true;

            let (phi, c_d) = entry.offset(target_angle);
            for &id in members {
                let Some(piece) = geometry.pieces.get(id) else {
                    continue;
                };
                let pos = positions
                    .get(id)
                    .copied()
                    .unwrap_or((piece.bounds_px.x, piece.bounds_px.y));
                let anchor_px = piece.pose_anchor_px;
                // Transform the piece's anchor point, then convert back to a
                // top-left position for the instance.
                let pt = (pos.0 + anchor_px[0], pos.1 + anchor_px[1]);
                let moved = apply_group_transform(pt, center, c_d, phi);
                new_positions[id] = (moved.0 - anchor_px[0], moved.1 - anchor_px[1]);
                new_rotations[id] = rotations.get(id).copied().unwrap_or(0.0) + phi;
            }
        }

        // Drop spring state for groups that no longer exist (e.g. after a merge
        // or membership change) so stale anchors don't linger.
        if map.len() != groups.len() {
            map.retain(|anchor, _| groups.contains_key(anchor));
        }
        drop(map);

        // Remember this frame's grouping so the next frame can detect a re-keyed
        // group and carry its spring state across (see the carry-over above).
        *self.rotate_anim_prev_anchors.borrow_mut() = (0..total)
            .map(|id| anchors.get(id).copied().unwrap_or(id as u32))
            .collect();

        (new_positions, new_rotations, animating)
    }

    /// Optional flip animation (WGPU only, same gate/params as the rotation
    /// animation). Returns `(flip_progress, animating)` where `flip_progress[i]`
    /// is the per-piece flip progress in `[0,1]` fed to the renderer (0 = front,
    /// 1 = back). When the feature is off or a group's flip is settled the value
    /// is the binary target (== today's instant behavior).
    fn animate_flips(&self, now_ms: f32, snapshot: &AppSnapshot) -> (Vec<f32>, bool) {
        let total = snapshot.piece_flipped.len();
        let targets: Vec<f32> = snapshot
            .piece_flipped
            .iter()
            .map(|&f| if f { 1.0 } else { 0.0 })
            .collect();

        let params = {
            let settings = self.wgpu_settings.borrow();
            if !settings.rotate_anim || snapshot.game.is_none() {
                drop(settings);
                self.flip_anim.borrow_mut().clear();
                self.flip_anim_last_ms.set(now_ms);
                return (targets, false);
            }
            SpringParams {
                response: settings
                    .rotate_anim_response
                    .clamp(WGPU_ROTATE_ANIM_RESPONSE_MIN, WGPU_ROTATE_ANIM_RESPONSE_MAX),
                damping: settings
                    .rotate_anim_damping
                    .clamp(WGPU_ROTATE_ANIM_DAMPING_MIN, WGPU_ROTATE_ANIM_DAMPING_MAX),
            }
        };

        let anchors = &snapshot.piece_group_anchor;
        // One representative target per group (flip is uniform across a group,
        // and only single-piece groups can be flipped anyway).
        let mut groups: HashMap<u32, Vec<usize>> = HashMap::new();
        for id in 0..total {
            let anchor = anchors.get(id).copied().unwrap_or(id as u32);
            groups.entry(anchor).or_default().push(id);
        }

        let last = self.flip_anim_last_ms.get();
        let dt = if last > 0.0 {
            ((now_ms - last).max(0.0).min(50.0)) / 1000.0
        } else {
            0.0
        };
        self.flip_anim_last_ms.set(now_ms);

        let mut progress = targets.clone();
        let mut animating = false;
        let mut map = self.flip_anim.borrow_mut();

        for (anchor, members) in &groups {
            let target = members
                .first()
                .and_then(|&id| targets.get(id).copied())
                .unwrap_or(0.0);
            let entry = map
                .entry(*anchor)
                .or_insert_with(|| FlipAnim::settled(target));
            let still = entry.step(target, params, dt);
            if still {
                animating = true;
            }
            let p = entry.progress();
            for &id in members {
                if let Some(out) = progress.get_mut(id) {
                    *out = p;
                }
            }
        }

        if map.len() != groups.len() {
            map.retain(|anchor, _| groups.contains_key(anchor));
        }

        (progress, animating)
    }

    fn toggle_preview_revealed(self: &Rc<Self>) {
        let revealed = !self.shell.preview_revealed.get();
        self.shell.preview_revealed.set(revealed);
        let target_blur = if revealed {
            0.0
        } else {
            PREVIEW_HIDDEN_BLUR_PX
        };
        self.preview_target_blur.set(target_blur);
        self.preview_anim_last_ms.set(0.0);
        self.request_render();
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
        let Some(layout) = self.shell.preview_layout(snapshot, canvas_rect) else {
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
        let revealed = self.shell.preview_revealed.get();
        let base_opacity = if revealed {
            1.0
        } else {
            PREVIEW_HIDDEN_OPACITY
        };
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
        if crate::crash::is_aborted() {
            return;
        }
        if crate::crash::take_panic_in_render() {
            panic!("injected test panic (render)");
        }
        let mut snapshot = self
            .pending_snapshot
            .borrow_mut()
            .take()
            .unwrap_or_else(|| self.shell.core.snapshot());
        // A queued snapshot can predate a puzzle swap (a remote State message
        // replaces assets and game together); its piece-indexed data would
        // then live in a different index space than the live geometry.
        let live_total = self
            .shell
            .core
            .assets()
            .map(|a| a.render_geometry.pieces.len());
        let snap_total = snapshot.game.as_ref().map(|g| g.playable.piece_count());
        if live_total.is_some() && snap_total.is_some() && live_total != snap_total {
            snapshot = self.shell.core.snapshot();
        }
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
        self.shell
            .multiplayer_active
            .set(matches!(sync_view.mode(), InitMode::Online));
        self.ensure_puzzle_image(snapshot);
        let disconnected = matches!(sync_view.mode(), InitMode::Online) && !sync_view.connected();
        if disconnected {
            self.root.set_class_name("app wgpu sync-disconnected");
        } else {
            self.root.set_class_name("app wgpu");
        }
        let assets = match self.shell.core.assets() {
            Some(assets) => assets,
            None => return,
        };
        let image = match self.image.borrow().clone() {
            Some(image) => image,
            None => return,
        };
        if self.shell.mask_atlas.borrow().is_none() {
            boot::set_phase("Preparing pieces", "Building mask atlas.");
            let mask_atlas_data = match build_mask_atlas(&assets.render_geometry) {
                Ok(atlas) => Rc::new(atlas),
                Err(err) => {
                    boot::fail(
                        "mask-atlas",
                        "Failed to prepare puzzle rendering.",
                        "Reload the page or try another browser.",
                    );
                    web_sys::console::error_1(&err);
                    return;
                }
            };
            *self.shell.mask_atlas.borrow_mut() = Some(mask_atlas_data);
        }
        let layout = snapshot.layout;
        let view_rect = snapshot.view;
        self.update_canvas_class(snapshot);
        if snapshot.dragging_members.is_empty() {
            self.shell.stop_auto_pan();
        }
        self.shell.update_debug_overlay(snapshot);
        let rect = self.canvas.get_bounding_client_rect();
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return;
        }
        let animating_preview_motion = self.shell.update_preview_motion(now_ms(), snapshot, &rect);
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
        let (connections_label, border_connections_label) =
            if let Some(game) = snapshot.game.as_ref() {
                let connections_label = format_progress(
                    game.playable.logical.active_edge_count(),
                    game.playable.logical.edge_count(),
                );
                // Topology-agnostic "border connections" count: every
                // edge whose both endpoints are frame-border pieces is
                // counted as a "border edge"; we report how many of those
                // are currently active. Works for grid, triangular, and
                // any future topology that defines `is_frame_border_piece`.
                let (border_connected, border_expected) = count_border_connections(&game.playable);
                let border_connections_label = if border_expected > 0 {
                    format_progress(border_connected, border_expected)
                } else {
                    "--".to_string()
                };
                (connections_label, border_connections_label)
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
        let art_credit = resolve_puzzle_credit(&assets.info.image_ref);
        let mut measure_state = self.shell.ui_measure.borrow_mut();
        let ui_specs = build_ui_specs(
            &mut measure_state,
            layout,
            assets.info.image_width as f32,
            assets.info.image_height as f32,
            UI_TITLE_TEXT,
            art_credit.map(|credit| credit.text),
            {
                let frame = assets.render_geometry.frame_shape.bounds;
                [frame.x, frame.y, frame.width, frame.height]
            },
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
        *self.shell.ui_credit_hitbox.borrow_mut() = credit_hitbox;
        let art_credit_hitbox = art_credit.and_then(|credit| credit.url).and_then(|url| {
            ui_specs
                .iter()
                .find(|spec| matches!(spec.id, UiTextId::ArtCredit))
                .map(|spec| (ui_hitbox_for_spec(&mut measure_state, spec), url))
        });
        *self.shell.ui_art_credit_hitbox.borrow_mut() = art_credit_hitbox;
        if snapshot.puzzle_info.is_none() {
            if self.shell.set_preview_hover(PreviewHoverTarget::None) {
                self.request_render();
                self.update_canvas_class(snapshot);
            }
        }
        let preview_specs =
            self.build_preview_overlay_specs(snapshot, &assets, &image, &rect, is_dark);
        if animating_preview_motion {
            self.request_render();
        }

        let mask_atlas = self.shell.mask_atlas.borrow();
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
        // Hold rotation emphasis: animate the tilt over the press to telegraph the
        // click window. Driven by the press timestamp (performance.now epoch) so it
        // stays in lock-step with the real click decision. Suppressed entirely when
        // a click wouldn't rotate the group (big/locked groups, no-op symmetry) so
        // only the scale part of the hold effect plays for those.
        let drag_emphasis = match snapshot.drag_press_ms {
            Some(press_ms) if highlight_members.is_some() && snapshot.drag_rotation_eligible => {
                let elapsed = now_ms_f32() - press_ms;
                if drag_hold_active(elapsed, snapshot.drag_click_moved) {
                    self.request_render();
                }
                drag_hold_emphasis(elapsed, snapshot.drag_click_moved)
            }
            _ => 0.0,
        };
        let ownership_by_anchor = sync_view.ownership_by_anchor();
        let positions = snapshot.piece_positions_px();
        let rotations = snapshot.piece_rotations_deg();
        let (positions, rotations, animating_rotation) = self.animate_rotations(
            now_ms(),
            snapshot,
            &assets.render_geometry,
            &positions,
            &rotations,
        );
        if animating_rotation {
            self.request_render();
        }
        let (flip_progress, animating_flip) = self.animate_flips(now_ms(), snapshot);
        if animating_flip {
            self.request_render();
        }
        // In pose-unit (pre-`puzzle_scale`) pixels, like the instance positions;
        // the shader applies `puzzle_scale` afterward. `pose_unit_px / POSE_UNIT_MM`
        // is px-per-mm, so this is the configured physical thickness on screen.
        let flip_thickness_mm = self
            .wgpu_settings
            .borrow()
            .flip_thickness_mm
            .clamp(WGPU_FLIP_THICKNESS_MM_MIN, WGPU_FLIP_THICKNESS_MM_MAX);
        let flip_thickness_px =
            flip_thickness_mm * snapshot.pose_unit_px[0].max(0.0) / POSE_UNIT_MM;
        let mut instances = build_wgpu_instances(
            &positions,
            &rotations,
            &flip_progress,
            &snapshot.z_order,
            snapshot.hovered_id,
            snapshot.app_settings.show_debug,
            snapshot,
            &assets.render_geometry,
            mask_atlas,
            highlight_members,
            drag_origin,
            drag_dir,
            drag_emphasis,
            ownership_by_anchor.as_ref(),
            sync_view.client_id(),
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
            renderer.set_show_debug(snapshot.app_settings.show_debug);
            renderer.set_solved(snapshot.solved);
            renderer.set_flip_thickness_px(flip_thickness_px);
            renderer.set_flip_edges(std::mem::take(&mut instances.flip_edges));
            renderer.set_shadow(
                settings.shadow,
                settings
                    .shadow_distance
                    .clamp(WGPU_SHADOW_DISTANCE_MIN, WGPU_SHADOW_DISTANCE_MAX),
                settings
                    .shadow_radius
                    .clamp(WGPU_SHADOW_RADIUS_MIN, WGPU_SHADOW_RADIUS_MAX),
                settings
                    .shadow_darkness
                    .clamp(WGPU_SHADOW_DARKNESS_MIN, WGPU_SHADOW_DARKNESS_MAX),
            );
            renderer.update_instances(instances);
            renderer.set_ui_texts(&ui_specs);
            renderer.set_ui_overlay_sprites(&preview_specs);
            renderer.render();
            boot::ready();
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
        let puzzle_bounds_px = [
            assets.render_geometry.puzzle_bounds_px.width,
            assets.render_geometry.puzzle_bounds_px.height,
        ];
        // The dashed outline uses the topology's actual frame (centred,
        // possibly letterboxed sub-rect), not the full image rectangle.
        let frame = assets.render_geometry.frame_shape.bounds;
        let frame_rect_px = [frame.x, frame.y, frame.width, frame.height];
        // Full puzzle-art rectangle + padding ratio, for the debug-only
        // reference outlines (art size vs. final cropped size).
        let image_size_px = [
            assets.render_geometry.image_width as f32,
            assets.render_geometry.image_height as f32,
        ];
        let workspace_padding_ratio = snapshot.rules.workspace_padding_ratio;
        let typical_piece_extent_px = assets.render_geometry.typical_piece_extent_px;
        let view_min_x = view_rect.min_x;
        let view_min_y = view_rect.min_y;
        let view_width = view_rect.width;
        let view_height = view_rect.height;
        let workspace_min_x = layout.view_min_x;
        let workspace_min_y = layout.view_min_y;
        let workspace_width = layout.view_width;
        let workspace_height = layout.view_height;
        let puzzle_scale = layout.puzzle_scale;
        let mask_atlas_data = match self.shell.mask_atlas.borrow().clone() {
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
        boot::set_phase("Initializing", "Preparing GPU renderer.");
        spawn_local(async move {
            let mut refresh_debug = false;
            match WgpuRenderer::new(
                canvas,
                image,
                puzzle_bounds_px,
                frame_rect_px,
                image_size_px,
                workspace_padding_ratio,
                typical_piece_extent_px,
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
                    refresh_debug = view.shell.core.snapshot().app_settings.show_debug;
                    renderer.set_emboss_enabled(true);
                    renderer.set_font_bytes(UI_FONT_BYTES.to_vec());
                    renderer.set_ui_font_bytes(UI_FONT_BYTES.to_vec());
                    renderer.set_edge_aa(edge_aa);
                    renderer.set_show_fps(show_fps);
                    renderer.set_show_debug(refresh_debug);
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
                    boot::ready();
                    *view.renderer.borrow_mut() = Some(renderer);
                }
                Err(err) => {
                    let err_text = err.as_string();
                    let (message, hint) = match err_text.as_deref() {
                        Some("image_read_failed") | Some("image_draw_failed") => (
                            "Failed to read the puzzle image.",
                            "Check the URL or CORS headers.",
                        ),
                        Some(text) if text.starts_with("request_adapter failed") => (
                            "Failed to initialize the renderer.",
                            "No compatible GPU was found. Check that hardware \
                             acceleration is enabled, or try a different browser.",
                        ),
                        _ => (
                            "Failed to initialize the renderer.",
                            "Check browser support or try a different browser.",
                        ),
                    };
                    boot::fail("renderer-init", message, hint);
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
        self.view.shell.set_hooks(hooks.clone());
        self._hooks = Some(hooks);
    }

    fn render(&mut self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let _ = sync_view;
        self.view.queue_render_snapshot(snapshot.clone());
    }
}

/// Time envelope (0..1) for the hold rotation emphasis, telegraphing the click
/// window. Quick ease-out attack to full tilt by `DRAG_EMPHASIS_ATTACK_MS`, then
/// a cosine ease back to 0 exactly at `CLICK_MAX_DURATION_MS` (the click cutoff);
/// 0 once the cutoff passes or the gesture has moved past the click slop (it's a
/// drag, not a click). Multiply by the full per-group angle to get the applied
/// rotation. See [`drag_hold_active`] for the matching keep-animating predicate.
fn drag_hold_emphasis(elapsed_ms: f32, moved: bool) -> f32 {
    if moved || elapsed_ms < 0.0 || elapsed_ms >= CLICK_MAX_DURATION_MS {
        return 0.0;
    }
    if elapsed_ms <= DRAG_EMPHASIS_ATTACK_MS {
        // Ease-out quad: fast start, settling at the peak.
        let t = elapsed_ms / DRAG_EMPHASIS_ATTACK_MS;
        1.0 - (1.0 - t) * (1.0 - t)
    } else {
        // Cosine ease from 1 -> 0 across the rest of the click window.
        let u = (elapsed_ms - DRAG_EMPHASIS_ATTACK_MS)
            / (CLICK_MAX_DURATION_MS - DRAG_EMPHASIS_ATTACK_MS);
        0.5 * (1.0 + (std::f32::consts::PI * u).cos())
    }
}

/// Whether the hold emphasis is still changing and frames should keep being
/// requested (the pointer may be held perfectly still, producing no events).
fn drag_hold_active(elapsed_ms: f32, moved: bool) -> bool {
    !moved && elapsed_ms >= 0.0 && elapsed_ms < CLICK_MAX_DURATION_MS
}

/// World position of a group's *stable* reference point: the canonical-anchor
/// piece's pose-anchor point. Unlike [`drag_group_center`] (the membership mean)
/// this does not move when other pieces join the group, so a merge produces no
/// spurious centroid jump in the rotation animation. Falls back to the
/// lowest-id member when the anchor piece is missing from geometry.
fn group_reference_point(
    positions: &[(f32, f32)],
    anchor: u32,
    members: &[usize],
    geometry: &PuzzleRenderGeometry,
) -> Option<(f32, f32)> {
    let ref_id = members
        .iter()
        .copied()
        .find(|&m| m as u32 == anchor)
        .or_else(|| members.iter().copied().min())?;
    let pos = positions.get(ref_id)?;
    let piece = geometry.pieces.get(ref_id)?;
    Some((
        pos.0 + piece.pose_anchor_px[0],
        pos.1 + piece.pose_anchor_px[1],
    ))
}

fn build_wgpu_instances(
    positions: &[(f32, f32)],
    rotations: &[f32],
    flip_progress: &[f32],
    z_order: &[usize],
    hovered_id: Option<usize>,
    show_debug: bool,
    snapshot: &AppSnapshot,
    geometry: &PuzzleRenderGeometry,
    mask_atlas: &MaskAtlasData,
    highlighted_members: Option<&[usize]>,
    drag_origin: Option<(f32, f32)>,
    drag_dir: f32,
    drag_emphasis: f32,
    ownership_by_anchor: &HashMap<u32, ClientId>,
    own_client_id: Option<ClientId>,
) -> InstanceSet {
    let total = geometry.pieces.len();
    if total == 0 {
        return InstanceSet {
            instances: Vec::new(),
            batches: Vec::new(),
            groups: Vec::new(),
            flip_edges: FlipEdges::default(),
        };
    }
    let fallback_order = if crate::app_core::z_order_is_valid(z_order, total) {
        None
    } else {
        if !z_order.is_empty() {
            // Fingerprint of a desynced/malformed remote snapshot; keep it
            // visible, but only once so a bad room doesn't spam the console.
            WARNED_INVALID_Z_ORDER.with(|warned| {
                if !warned.replace(true) {
                    web_sys::console::warn_1(
                        &"invalid z-order snapshot; falling back to canonical draw order".into(),
                    );
                }
            });
        }
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
        if id < total {
            let members = snapshot.group_members_for_piece(id);
            if members.is_empty() {
                hovered_mask[id] = true;
            }
            for member in members {
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
    // "Held" gates the persistent scale-up and the anchor spread; the rotation
    // magnitude is further modulated over time by `drag_emphasis` (0..1) so it can
    // animate to zero while the piece stays held and scaled.
    let drag_active = drag_dir.abs() > f32::EPSILON && drag_mask.iter().any(|val| *val);
    let drag_count = highlighted_members
        .map(|members| members.len())
        .unwrap_or(0);
    let drag_rotation = if drag_active {
        drag_angle_for_group(drag_count) * drag_dir.signum() * drag_emphasis
    } else {
        0.0
    };
    let drag_scale = if drag_active { DRAG_SCALE } else { 1.0 };
    let drag_center = if drag_active {
        drag_origin.or_else(|| {
            highlighted_members.and_then(|members| drag_group_center(positions, members, geometry))
        })
    } else {
        None
    };
    // Pieces sharing an anchor form a group (the anchor is the group's lowest
    // piece id), so one pass over `piece_group_anchor` assigns group ids in
    // first-seen order.
    let mut owned_mask = vec![false; total];
    let mut group_id = vec![usize::MAX; total];
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut group_has_owned: Vec<bool> = Vec::new();
    let mut group_by_anchor: HashMap<u32, usize> = HashMap::new();
    for id in 0..total {
        let anchor_id = snapshot
            .piece_group_anchor
            .get(id)
            .copied()
            .unwrap_or(id as u32);
        let gid = *group_by_anchor.entry(anchor_id).or_insert_with(|| {
            groups.push(Vec::new());
            group_has_owned.push(
                ownership_by_anchor
                    .get(&anchor_id)
                    .is_some_and(|owner_id| Some(*owner_id) != own_client_id),
            );
            groups.len() - 1
        });
        group_id[id] = gid;
        groups[gid].push(id);
        owned_mask[id] = group_has_owned[gid];
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
    let mut groups: Vec<InstanceBatch> = Vec::new();
    // Thickness rims for any pieces mid-flip, indexed by their instance position.
    let mut flip_edge_verts: Vec<EdgeVertex> = Vec::new();
    let mut flip_edge_spans: Vec<FlipEdgeSpan> = Vec::new();
    for gid in group_order {
        let members = &group_members[gid];
        if members.is_empty() {
            continue;
        }
        let start = instances.len() as u32;
        for &id in members {
            let Some(piece) = geometry.pieces.get(id) else {
                continue;
            };
            let base_x = piece.bounds_px.x;
            let base_y = piece.bounds_px.y;
            let pos = positions.get(id).copied().unwrap_or((base_x, base_y));
            let render_pos = if drag_mask.get(id).copied().unwrap_or(false) {
                if let Some(center) = drag_center {
                    drag_group_position(
                        pos,
                        center,
                        drag_scale,
                        drag_rotation,
                        piece.pose_anchor_px,
                    )
                } else {
                    pos
                }
            } else {
                pos
            };
            let rotation = rotations.get(id).copied().unwrap_or(0.0);
            let flip = flip_progress.get(id).copied().unwrap_or(0.0);
            let hovered = hovered_mask.get(id).copied().unwrap_or(false);
            let owned = owned_mask.get(id).copied().unwrap_or(false);
            let mask_origin = mask_atlas.origins.get(id).copied().unwrap_or([0.0, 0.0]);
            instances.push(Instance {
                pos: [render_pos.0, render_pos.1],
                size: [piece.bounds_px.width, piece.bounds_px.height],
                rotation,
                flip_progress: flip,
                hover: if show_debug {
                    OUTLINE_KIND_DEBUG
                } else if owned {
                    OUTLINE_KIND_OWNED
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
                held: if drag_active && drag_mask.get(id).copied().unwrap_or(false) {
                    1.0
                } else {
                    0.0
                },
                piece_origin: [base_x, base_y],
                mask_origin,
                pose_anchor: piece.pose_anchor_px,
            });
            // While mid-flip, extrude this piece's outline for the thickness rim.
            if flip > FLIP_ACTIVE_EPS && flip < 1.0 - FLIP_ACTIVE_EPS {
                let v_start = flip_edge_verts.len() as u32;
                push_flip_edge_verts(&piece.outline_dense_px, &mut flip_edge_verts);
                let v_count = flip_edge_verts.len() as u32 - v_start;
                if v_count > 0 {
                    flip_edge_spans.push(FlipEdgeSpan {
                        v_start,
                        v_count,
                        instance_index: instances.len() as u32 - 1,
                    });
                }
            }
        }
        let count = (instances.len() as u32) - start;
        if count == 0 {
            continue;
        }
        let draw_outline = show_debug || group_has_hover[gid] || group_has_owned[gid];
        // Per-group span (never merged) for the interleaved shadow pass.
        groups.push(InstanceBatch {
            start,
            count,
            draw_outline,
        });
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
    InstanceSet {
        instances,
        batches,
        groups,
        flip_edges: FlipEdges {
            verts: flip_edge_verts,
            spans: flip_edge_spans,
        },
    }
}

/// Extrudes a closed outline polyline (piece-local px) into thickness-rim
/// geometry: for each segment, two triangles spanning the slab depth from
/// `z_sign = -1` (back face) to `+1` (front face). The vertex shader projects
/// these with the flip rotation. Appends 6 verts per segment to `out`.
fn push_flip_edge_verts(outline: &[[f32; 2]], out: &mut Vec<EdgeVertex>) {
    let n = outline.len();
    if n < 2 {
        return;
    }
    for i in 0..n {
        let a = outline[i];
        let b = outline[(i + 1) % n];
        let a_back = EdgeVertex {
            pos: a,
            z_sign: -1.0,
        };
        let a_front = EdgeVertex {
            pos: a,
            z_sign: 1.0,
        };
        let b_back = EdgeVertex {
            pos: b,
            z_sign: -1.0,
        };
        let b_front = EdgeVertex {
            pos: b,
            z_sign: 1.0,
        };
        out.push(a_back);
        out.push(a_front);
        out.push(b_back);
        out.push(b_back);
        out.push(a_front);
        out.push(b_front);
    }
}

fn build_ui_specs(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    title_text: &str,
    art_credit_text: Option<&str>,
    frame_rect: [f32; 4],
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

        if let Some(text) = art_credit_text {
            // Anchored just below the puzzle frame's bottom-right corner
            // rather than the workspace gutter grid, so it stays visible
            // once the puzzle is complete; apply_taffy_layout skips it.
            let art_size = min_dim * UI_ART_CREDIT_FONT_RATIO;
            let mut spec = UiTextSpec {
                id: UiTextId::ArtCredit,
                text: text.to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_ART_CREDIT_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::BottomRight,
                rotation_offset: [0.0, 0.0],
                font_size: art_size,
                line_height: art_size * 1.08,
                color: base_color,
            };
            let (text_width, text_height) = layout_text_bounds(measure, &spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let pad = art_size * UI_ART_CREDIT_PAD_RATIO;
            spec.pos = [
                frame_rect[0] + frame_rect[2] - pad - text_width * 0.5,
                frame_rect[1] + frame_rect[3] + pad + text_height * 0.5,
            ];
            specs.push(spec);
        }

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

fn rgba(r: u8, g: u8, b: u8, a: f32) -> [f32; 4] {
    [r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, a]
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
