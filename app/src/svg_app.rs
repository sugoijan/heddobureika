use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{AnimationFrame, request_animation_frame};
use wasm_bindgen::JsCast;
use web_sys::{Document, Element, Event, HtmlImageElement, PointerEvent, WheelEvent};

use crate::app_core::{AppCore, AppSnapshot, PuzzleAssets};
use crate::app_router;
use crate::boot;
use crate::core::*;
use crate::input::{
    PointerKind, PointerPolicy, WheelIntent, WheelIntentTracker, screen_delta_to_world,
    screen_scroll_to_world, screen_slop_to_puzzle, screen_to_view_coords,
    workspace_to_puzzle_coords,
};
use crate::puzzle_image::{resolve_puzzle_credit, resolve_puzzle_image_src};
use crate::renderer::{UiTextId, build_mask_atlas};
use crate::runtime::{CoreAction, GameSyncView, GameView, SyncView, ViewHooks};
use crate::sync_runtime;
use crate::view_common::*;
use crate::view_runtime;
use crate::web::{now_ms, open_url, prefers_dark_mode};
use heddobureika_core::{PuzzleImageRef, PuzzleInfo};

const PREVIEW_TILT_DEG: f32 = -1.5;
// Keep in sync with ViewState::fit_zoom_for_size in src/app_core.rs.
const PREVIEW_MAX_FIT_SCALE: f32 = 1.0;
const SVG_NS: &str = "http://www.w3.org/2000/svg";

#[derive(Clone, Copy)]
struct PointerDragGate {
    pointer_id: i32,
    start_x: f32,
    start_y: f32,
    moved: bool,
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
    shell: ViewShell,
    document: Document,
    root: Element,
    svg: Element,
    defs: Element,
    workspace_rect: Element,
    ui_group: Element,
    puzzle_group: Element,
    puzzle_bounds: Element,
    /// Debug-only reference outlines (shown when `show_debug`): the puzzle-ART
    /// frame (full image) and the workspace it would produce — so the
    /// difference vs the cropped final puzzle is visible.
    debug_art_frame: Element,
    debug_art_workspace: Element,
    pieces: RefCell<Vec<SvgPieceNodes>>,
    ui_credit_hovered: Cell<bool>,
    pointer_policy: RefCell<PointerPolicy>,
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    drag_gate: RefCell<Option<PointerDragGate>>,
    subscription: RefCell<Option<crate::app_core::AppSubscription>>,
    sync_hook: RefCell<Option<sync_runtime::SyncViewHookHandle>>,
    listeners: RefCell<Vec<EventListener>>,
    last_assets_ptr: Cell<usize>,
    last_z_order: RefCell<Vec<usize>>,
    last_svg_settings: RefCell<Option<SvgRenderSettings>>,
    svg_settings: RefCell<SvgRenderSettings>,
    pending_snapshot: RefCell<Option<(AppSnapshot, SyncView)>>,
    frame_handle: RefCell<Option<AnimationFrame>>,
    viewport_frame: RefCell<Option<AnimationFrame>>,
    viewport_retry: Cell<bool>,
    preview: RefCell<Option<Rc<PreviewOverlay>>>,
    preview_motion_frame: RefCell<Option<AnimationFrame>>,
    wheel_intent: RefCell<WheelIntentTracker>,
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
        let debug_art_frame = create_svg_element(&document, "rect");
        let _ = debug_art_frame.set_attribute(
            "style",
            "fill:none; stroke:#f438dc; stroke-width:2; stroke-dasharray:8 5; \
             pointer-events:none; display:none;",
        );
        let debug_art_workspace = create_svg_element(&document, "rect");
        let _ = debug_art_workspace.set_attribute(
            "style",
            "fill:none; stroke:#f438dc; stroke-width:2; stroke-dasharray:2 4; \
             opacity:0.7; pointer-events:none; display:none;",
        );
        let _ = svg.append_child(&defs);
        let _ = svg.append_child(&workspace_rect);
        let _ = svg.append_child(&ui_group);
        let _ = puzzle_group.append_child(&puzzle_bounds);
        let _ = puzzle_group.append_child(&debug_art_frame);
        let _ = puzzle_group.append_child(&debug_art_workspace);
        let _ = svg.append_child(&puzzle_group);
        let _ = root.append_child(&svg);

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
            debug_art_frame,
            debug_art_workspace,
            render_settings.svg.clone(),
        ));
        let preview = PreviewOverlay::new(&document);
        let _ = root.append_child(&preview.root);
        *view.preview.borrow_mut() = Some(preview);
        let debug_overlay = DebugOverlay::new(&document);
        let _ = root.append_child(&debug_overlay.root);
        *view.shell.debug_overlay.borrow_mut() = Some(debug_overlay);
        *view.sync_hook.borrow_mut() = Some(sync_runtime::register_sync_view_hook(Rc::new({
            let view = Rc::clone(&view);
            move || {
                let snapshot = view.shell.core.snapshot();
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
        // Subscribers can run re-entrantly (a nested notify during render),
        // so the render subscription must not hold a RefCell borrow while
        // queueing; go through the view directly instead of the adapter.
        *view.subscription.borrow_mut() = Some(core.subscribe(Rc::new({
            let view = Rc::clone(&view);
            let core = core.clone();
            move || {
                let snapshot = core.snapshot();
                let sync_view = sync_runtime::sync_view();
                view.queue_render_snapshot(snapshot, sync_view);
            }
        })));
        view.install_listeners();
        view.ensure_viewport_size();
        let snapshot = core.snapshot();
        let sync_view = sync_runtime::sync_view();
        adapter.borrow_mut().render(&snapshot, &sync_view);
        view_runtime::set_active_view(Some(Rc::new(view)));
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("svg renderer is only supported on wasm32 targets");
    }
}

impl ViewShellHost for SvgView {
    fn shell(&self) -> &ViewShell {
        &self.shell
    }

    fn request_render(self: &Rc<Self>) {
        self.queue_render_snapshot(self.shell.core.snapshot(), sync_runtime::sync_view());
    }

    fn private_image_ready(self: &Rc<Self>, _key: PuzzleKey, _url: String) {
        // `ensure_scene` re-resolves the image source once the asset cache is
        // invalidated.
        self.last_assets_ptr.set(0);
        self.request_render();
    }
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
        debug_art_frame: Element,
        debug_art_workspace: Element,
        svg_settings: SvgRenderSettings,
    ) -> Self {
        Self {
            shell: ViewShell::new(core, svg.clone(), PREVIEW_TILT_DEG, PREVIEW_MAX_FIT_SCALE),
            document,
            root,
            svg,
            defs,
            workspace_rect,
            ui_group,
            puzzle_group,
            puzzle_bounds,
            debug_art_frame,
            debug_art_workspace,
            pieces: RefCell::new(Vec::new()),
            ui_credit_hovered: Cell::new(false),
            pointer_policy: RefCell::new(PointerPolicy::new()),
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            drag_gate: RefCell::new(None),
            subscription: RefCell::new(None),
            sync_hook: RefCell::new(None),
            listeners: RefCell::new(Vec::new()),
            last_assets_ptr: Cell::new(0),
            last_z_order: RefCell::new(Vec::new()),
            last_svg_settings: RefCell::new(None),
            svg_settings: RefCell::new(svg_settings),
            pending_snapshot: RefCell::new(None),
            frame_handle: RefCell::new(None),
            viewport_frame: RefCell::new(None),
            viewport_retry: Cell::new(false),
            preview: RefCell::new(None),
            preview_motion_frame: RefCell::new(None),
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
        self.queue_render_snapshot(self.shell.core.snapshot(), sync_runtime::sync_view());
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
            self.shell.core.set_viewport_size(width, height);
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

    fn clear_hover_state(self: &Rc<Self>, snapshot: &AppSnapshot) {
        let mut update_preview = false;
        if self.shell.set_preview_hover(PreviewHoverTarget::None) {
            update_preview = true;
        }
        if self.ui_credit_hovered.get() {
            self.ui_credit_hovered.set(false);
            self.update_svg_class(snapshot);
        }
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered: None });
        if update_preview {
            let rect = self.root.get_bounding_client_rect();
            self.update_preview_overlay(snapshot, &rect);
        }
    }

    fn handle_preview_pointer_down(self: &Rc<Self>, event: &PointerEvent, preview_root: &Element) {
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
        let _ = preview_root.set_pointer_capture(pointer_id);
        let rect = self.root.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                let (id_a, a) = touch_points[0];
                let (id_b, b) = touch_points[1];
                if let Some(layout) = self.shell.preview_layout(&snapshot, &rect) {
                    let local_ax = a.screen_x - rect.left() as f32;
                    let local_ay = a.screen_y - rect.top() as f32;
                    let local_bx = b.screen_x - rect.left() as f32;
                    let local_by = b.screen_y - rect.top() as f32;
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
                }
                if self.shell.set_preview_hover(target) || button == 0 {
                    self.update_preview_overlay(&snapshot, &rect);
                }
                event.prevent_default();
                return;
            }
        }
        self.shell.clear_preview_click();
    }

    fn handle_preview_pointer_move(self: &Rc<Self>, event: &PointerEvent) {
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
            policy.update_pointer(pointer_id, kind, screen_x, screen_y, event.buttons());
        }
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.root.get_bounding_client_rect();
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
                    self.update_preview_overlay(&snapshot, &rect);
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
            self.update_preview_overlay(&snapshot, &rect);
            event.prevent_default();
            return;
        }
        if !matches!(kind, PointerKind::Mouse | PointerKind::Pen) || event.buttons() != 0 {
            return;
        }
        if self.shell.preview_drag.borrow().is_some() {
            return;
        }
        let mut target = PreviewHoverTarget::None;
        if let Some(layout) = self.shell.preview_layout(&snapshot, &rect) {
            target = self.shell.preview_hit_test(&layout, local_x, local_y);
        }
        if self.shell.set_preview_hover(target) {
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn handle_preview_pointer_up(self: &Rc<Self>, event: &PointerEvent, preview_root: &Element) {
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
        let _ = preview_root.release_pointer_capture(pointer_id);
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
        let rect = self.root.get_bounding_client_rect();
        let local_x = screen_x - rect.left() as f32;
        let local_y = screen_y - rect.top() as f32;
        self.shell
            .update_preview_click_movement(Some(pointer_id), local_x, local_y);
        let preview_clicked = self.shell.consume_preview_click(Some(pointer_id));
        let mut preview_dragged = false;
        if let Some(PreviewDragState::Pinch {
            touch_a, touch_b, ..
        }) = *self.shell.preview_drag.borrow()
        {
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
        let _ = preview_root.release_pointer_capture(pointer_id);
        self.shell.preview_drag.borrow_mut().take();
        self.shell.preview_click.borrow_mut().take();
        self.shell.preview_hover.set(PreviewHoverTarget::None);
        self.shell.preview_velocity.set([0.0, 0.0]);
        self.shell.preview_motion_last_ms.set(0.0);
        self.preview_motion_frame.borrow_mut().take();
        let snapshot = self.shell.core.snapshot();
        let rect = self.root.get_bounding_client_rect();
        self.update_preview_overlay(&snapshot, &rect);
    }

    fn handle_preview_pointer_leave(self: &Rc<Self>, event: &PointerEvent) {
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
        let snapshot = self.shell.core.snapshot();
        if self.shell.set_preview_hover(PreviewHoverTarget::None) {
            let rect = self.root.get_bounding_client_rect();
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn handle_svg_pointer_down(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
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
        let _ = svg.set_pointer_capture(pointer_id);
        if kind == PointerKind::Touch {
            let touch_points = { self.pointer_policy.borrow().active_touch_points() };
            if touch_points.len() >= 2 {
                if !snapshot.dragging_members.is_empty() {
                    self.shell.dispatch_action(CoreAction::DragEnd {
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
                        self.shell.core.settle_view();
                    }
                    self.update_svg_class(&snapshot);
                    event.prevent_default();
                }
                return;
            }
        }
        let Some((view_x, view_y)) = screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
        else {
            return;
        };
        let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
        if let Some(piece_id) = self.shell.pick_piece(px, py, &snapshot) {
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
            self.update_svg_class(&snapshot);
            event.prevent_default();
        }
    }

    fn handle_svg_pointer_move(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
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
            self.shell.core.pan_view(dx_world, dy_world);
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
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(gate) = self.drag_gate.borrow_mut().as_mut() {
                if let Some(drag_id) = snapshot.drag_pointer_id {
                    if gate.pointer_id != drag_id {
                        gate.moved = true;
                    } else if !gate.moved {
                        let click_tolerance = snapshot.min_piece_extent_px[0]
                            .min(snapshot.min_piece_extent_px[1])
                            * CLICK_MOVE_RATIO;
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
        let Some((view_x, view_y)) = screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
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
                self.update_svg_class(&snapshot);
            }
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered });
            return;
        }
        let credit_hovered = self.shell.hit_credit(view_x, view_y)
            || self.shell.hit_art_credit(view_x, view_y).is_some();
        if self.ui_credit_hovered.get() != credit_hovered {
            self.ui_credit_hovered.set(credit_hovered);
            self.update_svg_class(&snapshot);
        }
        if credit_hovered {
            self.shell
                .dispatch_action(CoreAction::SetHovered { hovered: None });
            return;
        }
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_svg_pointer_up(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
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
        let _ = svg.release_pointer_capture(pointer_id);
        self.drag_gate.borrow_mut().take();
        let snapshot = self.shell.core.snapshot();
        if kind_changed {
            self.clear_hover_state(&snapshot);
        }
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
            self.update_svg_class(&snapshot);
            self.shell.core.settle_view();
        }
        if !snapshot.dragging_members.is_empty() {
            if let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, svg, snapshot.view)
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
        let hovered = screen_to_view_coords(screen_x, screen_y, svg, snapshot.view).and_then(
            |(view_x, view_y)| {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.shell.pick_piece(px, py, &snapshot)
            },
        );
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered });
    }

    fn handle_svg_pointer_cancel(self: &Rc<Self>, event: &PointerEvent, svg: &Element) {
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
        let _ = svg.release_pointer_capture(pointer_id);
        self.pan_state.borrow_mut().take();
        self.pinch_state.borrow_mut().take();
        self.drag_gate.borrow_mut().take();
        self.shell.stop_auto_pan();
        let snapshot = self.shell.core.snapshot();
        self.update_svg_class(&snapshot);
        self.shell.core.settle_view();
        self.shell.dispatch_action(CoreAction::DragEnd {
            pointer_id: snapshot.drag_pointer_id,
        });
    }

    fn handle_svg_pointer_leave(self: &Rc<Self>, event: &PointerEvent) {
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
        let snapshot = self.shell.core.snapshot();
        self.update_svg_class(&snapshot);
        self.shell
            .dispatch_action(CoreAction::SetHovered { hovered: None });
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

        let core = self.shell.core.clone();
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
        if self.ui_credit_hovered.get() {
            class.push_str(" ui-link-hover");
        }
        let _ = self.svg.set_attribute("class", &class);
    }

    fn render_snapshot(self: &Rc<Self>, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        if crate::crash::is_aborted() {
            return;
        }
        if crate::crash::take_panic_in_render() {
            panic!("injected test panic (render)");
        }
        if snapshot.view.width <= 0.0 || snapshot.view.height <= 0.0 {
            self.ensure_viewport_size();
        }
        self.shell
            .multiplayer_active
            .set(matches!(sync_view.mode(), InitMode::Online));
        let disconnected = matches!(sync_view.mode(), InitMode::Online) && !sync_view.connected();
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
        let Some(assets) = self.shell.core.assets() else {
            self.clear_scene();
            return;
        };
        self.ensure_scene(snapshot, assets.clone());
        self.update_svg_class(snapshot);
        if snapshot.dragging_members.is_empty() {
            self.shell.stop_auto_pan();
        }
        self.shell.update_debug_overlay(snapshot);
        self.render_view(snapshot, &assets);
        self.render_ui(snapshot, &assets);
        self.render_pieces(snapshot, &assets, sync_view);
        self.update_z_order(snapshot);
        boot::ready();
    }

    fn reset_preview_state(&self) {
        self.shell.reset_preview_state();
        self.preview_motion_frame.borrow_mut().take();
    }

    fn resolve_image_src(self: &Rc<Self>, info: &PuzzleInfo) -> Option<String> {
        match &info.image_ref {
            PuzzleImageRef::Private { hash } => {
                self.ensure_private_image_src(hash, PuzzleKey::from_info(info))
            }
            _ => resolve_puzzle_image_src(&info.image_ref),
        }
    }

    fn update_preview_overlay(&self, snapshot: &AppSnapshot, rect: &web_sys::DomRect) {
        let preview_ref = self.preview.borrow();
        let Some(preview) = preview_ref.as_ref() else {
            return;
        };
        let Some(layout) = self.shell.preview_layout(snapshot, rect) else {
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
        if self.shell.preview_revealed.get() {
            class.push_str(" preview-revealed");
        } else {
            class.push_str(" preview-hidden");
        }
        let drag_target = self
            .shell
            .preview_drag
            .borrow()
            .as_ref()
            .and_then(|drag| match drag {
                PreviewDragState::Move { .. } => Some(PreviewHoverTarget::Body),
                PreviewDragState::Resize { handle, .. } => {
                    Some(PreviewHoverTarget::Resize(*handle))
                }
                PreviewDragState::Pinch { .. } => None,
            });
        let cursor_target = drag_target.unwrap_or(self.shell.preview_hover.get());
        if let Some(cursor_class) = preview_cursor_class(cursor_target) {
            class.push_str(" ");
            class.push_str(cursor_class);
        }
        if matches!(
            self.shell
                .preview_drag
                .borrow()
                .as_ref()
                .map(|drag| drag.kind()),
            Some(PreviewDragKind::Move)
        ) {
            class.push_str(" preview-dragging");
        }
        preview.set_class(&class);
    }

    fn toggle_preview_revealed(&self) {
        let revealed = !self.shell.preview_revealed.get();
        self.shell.preview_revealed.set(revealed);
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
        let snapshot = self.shell.core.snapshot();
        let rect = self.root.get_bounding_client_rect();
        let now = now_ms();
        if self.shell.update_preview_motion(now, &snapshot, &rect) {
            self.update_preview_overlay(&snapshot, &rect);
            self.ensure_preview_motion_frame();
        } else {
            self.update_preview_overlay(&snapshot, &rect);
        }
    }

    fn maybe_start_preview_momentum(self: &Rc<Self>) {
        let velocity = self.shell.preview_velocity.get();
        if velocity[0].abs() > PREVIEW_VELOCITY_EPS || velocity[1].abs() > PREVIEW_VELOCITY_EPS {
            self.ensure_preview_motion_frame();
        }
    }

    fn clear_scene(&self) {
        self.pieces.borrow_mut().clear();
        self.shell.mask_atlas.borrow_mut().take();
        self.last_assets_ptr.set(0);
        self.shell.last_puzzle.borrow_mut().take();
        self.shell.clear_private_image_cache();
        self.last_z_order.borrow_mut().clear();
        clear_children(&self.defs);
        clear_children(&self.puzzle_group);
        let _ = self.puzzle_group.append_child(&self.puzzle_bounds);
        let _ = self.puzzle_group.append_child(&self.debug_art_frame);
        let _ = self.puzzle_group.append_child(&self.debug_art_workspace);
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
            .shell
            .last_puzzle
            .borrow()
            .as_ref()
            .map(|key| key != &PuzzleKey::from_info(&assets.info))
            .unwrap_or(true);
        let assets_changed = self.last_assets_ptr.get() != ptr;
        if puzzle_changed || assets_changed || settings_changed {
            self.last_assets_ptr.set(ptr);
            self.shell
                .last_puzzle
                .borrow_mut()
                .replace(PuzzleKey::from_info(&assets.info));
            if puzzle_changed {
                self.reset_preview_state();
                self.shell.clear_private_image_cache();
            }
            self.rebuild_defs(snapshot, &assets);
            let image_src = self.resolve_image_src(&assets.info).unwrap_or_default();
            self.rebuild_pieces(snapshot, &assets, image_src.as_str());
        }
    }

    fn rebuild_defs(&self, _snapshot: &AppSnapshot, assets: &PuzzleAssets) {
        clear_children(&self.defs);
        self.shell.mask_atlas.borrow_mut().take();
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

        for piece in assets.render_geometry.pieces.iter() {
            let mask_x = fmt_f32(-assets.mask_pad);
            let mask_y = fmt_f32(-assets.mask_pad);
            let mask_width = fmt_f32(piece.bounds_px.width + assets.mask_pad * 2.0);
            let mask_height = fmt_f32(piece.bounds_px.height + assets.mask_pad * 2.0);
            let mask = create_svg_element(&self.document, "mask");
            let _ = mask.set_attribute("id", &format!("piece-mask-{}", piece.id.as_usize()));
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
            let _ = path.set_attribute("d", piece.outline_svg.as_str());
            let _ = path.set_attribute("fill", "white");
            let _ = mask.append_child(&rect);
            let _ = mask.append_child(&path);
            let _ = self.defs.append_child(&mask);
        }

        if self.shell.mask_atlas.borrow().is_none() {
            boot::set_phase("Preparing pieces", "Building mask atlas.");
            match build_mask_atlas(&assets.render_geometry) {
                Ok(atlas) => {
                    *self.shell.mask_atlas.borrow_mut() = Some(Rc::new(atlas));
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
        let _ = self.puzzle_group.append_child(&self.debug_art_frame);
        let _ = self.puzzle_group.append_child(&self.debug_art_workspace);
        self.pieces.borrow_mut().clear();
        for piece in &assets.render_geometry.pieces {
            let node = self.build_piece_node(snapshot, assets, piece.id.as_usize(), image_src);
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
        let _ = outline_external
            .set_attribute("class", "piece-outline piece-outline-group edge-external");
        let _ = outline_internal
            .set_attribute("class", "piece-outline piece-outline-group edge-internal");
        let _ = outline_simple.set_attribute("class", "piece-outline piece-outline-simple");
        let _ = back_rect.set_attribute("class", "piece-back");
        let _ = image.set_attribute("class", "piece-image");
        let _ = debug_group.set_attribute("class", "piece-debug");
        let _ = debug_label.set_attribute("class", "piece-debug-label");
        let _ = debug_center.set_attribute("class", "piece-debug-center");

        let Some(piece_geometry) = assets.render_geometry.pieces.get(piece_id) else {
            return SvgPieceNodes {
                root,
                outline_group,
                surface_group,
                internal_group,
                outline_external,
                outline_internal,
                outline_simple,
                debug_group,
                debug_label,
            };
        };
        let mask_ref = format!("url(#piece-mask-{piece_id})");
        let image_offset = assets
            .render_geometry
            .piece_image_offset(piece_geometry.id)
            .unwrap_or([0.0, 0.0]);
        let img_x = fmt_f32(image_offset[0]);
        let img_y = fmt_f32(image_offset[1]);
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

        let path = piece_geometry.outline_svg.clone();
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
        let center_x = fmt_f32(piece_geometry.pose_anchor_px[0]);
        let center_y = fmt_f32(piece_geometry.pose_anchor_px[1]);
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
        let _ = self.puzzle_group.set_attribute(
            "transform",
            &format!("scale({})", fmt_f32(layout.puzzle_scale)),
        );
        // The dashed outline traces the generated puzzle's actual frame
        // (`frame_shape.bounds`), which may be a centred, letterboxed
        // sub-rect of the image (e.g. triangular's equilateral frame) — not
        // the full image rectangle.
        let frame = assets.render_geometry.frame_shape.bounds;
        let bounds_inset = 1.0;
        let _ = self
            .puzzle_bounds
            .set_attribute("x", &fmt_f32(frame.x + bounds_inset));
        let _ = self
            .puzzle_bounds
            .set_attribute("y", &fmt_f32(frame.y + bounds_inset));
        let _ = self
            .puzzle_bounds
            .set_attribute("width", &fmt_f32(frame.width - 2.0 * bounds_inset));
        let _ = self
            .puzzle_bounds
            .set_attribute("height", &fmt_f32(frame.height - 2.0 * bounds_inset));
        // Dashed workspace outline shares the same `PuzzleFrameShape`
        // every topology uses for its border pieces, so a given puzzle
        // has a single corner radius regardless of topology kind.
        let radius = fmt_f32(assets.render_geometry.frame_shape.corner_radius_px);
        let _ = self.puzzle_bounds.set_attribute("rx", &radius);
        let _ = self.puzzle_bounds.set_attribute("ry", &radius);

        // Debug-only reference outlines: the puzzle-ART frame (full image) and
        // the workspace it WOULD produce, so the difference vs the cropped
        // final puzzle (drawn above) is visible. Hidden unless debug is on.
        const FRAME_STYLE: &str =
            "fill:none; stroke:#f438dc; stroke-width:2; stroke-dasharray:8 5; pointer-events:none;";
        const WORKSPACE_STYLE: &str = "fill:none; stroke:#f438dc; stroke-width:2; \
             stroke-dasharray:2 4; opacity:0.7; pointer-events:none;";
        if snapshot.app_settings.show_debug {
            let iw = assets.render_geometry.image_width as f32;
            let ih = assets.render_geometry.image_height as f32;
            let _ = self.debug_art_frame.set_attribute("x", "0");
            let _ = self.debug_art_frame.set_attribute("y", "0");
            let _ = self.debug_art_frame.set_attribute("width", &fmt_f32(iw));
            let _ = self.debug_art_frame.set_attribute("height", &fmt_f32(ih));
            let _ = self.debug_art_frame.set_attribute("style", FRAME_STYLE);

            let art =
                compute_workspace_layout(0.0, 0.0, iw, ih, snapshot.rules.workspace_padding_ratio);
            let _ = self
                .debug_art_workspace
                .set_attribute("x", &fmt_f32(art.view_min_x));
            let _ = self
                .debug_art_workspace
                .set_attribute("y", &fmt_f32(art.view_min_y));
            let _ = self
                .debug_art_workspace
                .set_attribute("width", &fmt_f32(art.view_width));
            let _ = self
                .debug_art_workspace
                .set_attribute("height", &fmt_f32(art.view_height));
            let _ = self
                .debug_art_workspace
                .set_attribute("style", WORKSPACE_STYLE);
        } else {
            let _ = self
                .debug_art_frame
                .set_attribute("style", "display:none; pointer-events:none;");
            let _ = self
                .debug_art_workspace
                .set_attribute("style", "display:none; pointer-events:none;");
        }
    }

    fn render_ui(&self, snapshot: &AppSnapshot, assets: &PuzzleAssets) {
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
        let (connections_label, border_connections_label) =
            if let Some(game) = snapshot.game.as_ref() {
                let connections_label = format_progress(
                    game.playable.logical.active_edge_count(),
                    game.playable.logical.edge_count(),
                );
                // Topology-agnostic border-connection count — see the
                // matching block in wgpu_app for the rationale.
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
        let prefers_dark = prefers_dark_mode();
        let is_dark = match snapshot.app_settings.theme_mode {
            ThemeMode::Dark => true,
            ThemeMode::Light => false,
            ThemeMode::System => prefers_dark,
        };
        let art_credit = resolve_puzzle_credit(&assets.info.image_ref);
        let mut measure_state = self.shell.ui_measure.borrow_mut();
        let ui_specs = build_ui_specs(
            &mut measure_state,
            layout,
            width,
            height,
            UI_TITLE_TEXT,
            art_credit.map(|credit| credit.text),
            {
                let frame = assets.render_geometry.frame_shape.bounds;
                [frame.x, frame.y, frame.width, frame.height]
            },
            snapshot.solved,
            connections_label.as_str(),
            border_connections_label.as_str(),
            false,
            None,
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
        if ui_specs.is_empty() {
            return;
        }
        for spec in ui_specs {
            let text_node = create_svg_element(&self.document, "text");
            let class = match spec.id {
                UiTextId::Title => "ui-text ui-title",
                UiTextId::Progress => "ui-text ui-progress",
                UiTextId::Credit => "ui-text ui-credit",
                UiTextId::ArtCredit => "ui-text ui-art-credit",
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

    fn render_pieces(
        &self,
        snapshot: &AppSnapshot,
        assets: &PuzzleAssets,
        sync_view: &dyn GameSyncView,
    ) {
        let total = assets.render_geometry.pieces.len();
        if total == 0 {
            return;
        }
        let foreign = foreign_ownership(sync_view);
        let mut hovered_mask = vec![false; total];
        if !snapshot.dragging_members.is_empty() {
            for id in &snapshot.dragging_members {
                if *id < hovered_mask.len() {
                    hovered_mask[*id] = true;
                }
            }
        } else if let Some(id) = snapshot.hovered_id {
            let members = snapshot.group_members_for_piece(id);
            if members.is_empty() && id < hovered_mask.len() {
                hovered_mask[id] = true;
            } else {
                for member in members {
                    if member < hovered_mask.len() {
                        hovered_mask[member] = true;
                    }
                }
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
        let drag_scale = if drag_dir.abs() > f32::EPSILON {
            DRAG_SCALE
        } else {
            1.0
        };
        let positions = snapshot.piece_positions_px();
        let rotations = snapshot.piece_rotations_deg();
        let drag_center = if drag_dir.abs() > f32::EPSILON {
            snapshot.drag_cursor.or_else(|| {
                drag_group_center(
                    &positions,
                    &snapshot.dragging_members,
                    &assets.render_geometry,
                )
            })
        } else {
            None
        };
        for (idx, node) in self.pieces.borrow().iter().enumerate() {
            if idx >= total {
                break;
            }
            let Some(piece_geometry) = assets.render_geometry.pieces.get(idx) else {
                continue;
            };
            let current = positions
                .get(idx)
                .copied()
                .unwrap_or((piece_geometry.bounds_px.x, piece_geometry.bounds_px.y));
            let rotation = rotations.get(idx).copied().unwrap_or(0.0);
            let flipped = snapshot.piece_flipped.get(idx).copied().unwrap_or(false);
            let is_dragging = dragging_mask.get(idx).copied().unwrap_or(false);
            let render_pos = if is_dragging {
                if let Some(center) = drag_center {
                    drag_group_position(
                        current,
                        center,
                        drag_scale,
                        drag_rotation,
                        piece_geometry.pose_anchor_px,
                    )
                } else {
                    current
                }
            } else {
                current
            };
            let center_x = piece_geometry.pose_anchor_px[0];
            let center_y = piece_geometry.pose_anchor_px[1];
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
            let _ = node
                .outline_group
                .set_attribute("transform", &inner_transform);
            let _ = node
                .surface_group
                .set_attribute("transform", &inner_transform);
            let _ = node
                .internal_group
                .set_attribute("transform", &inner_transform);

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
            if foreign.as_ref().is_some_and(|(ownership, client_id)| {
                piece_owned_by_other(snapshot, ownership, *client_id, idx)
            }) {
                class.push_str(" owned-other");
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
                update_debug_label(
                    &self.document,
                    &node.debug_label,
                    center_x,
                    center_y,
                    &label,
                );
            } else {
                let _ = node.debug_group.set_attribute("display", "none");
            }
            // Topology-agnostic "external edges" outline. For each
            // incident topology edge that is *not* currently active (i.e.
            // the piece is not joined to its neighbour across that edge),
            // include the corresponding `edge_svgs` path in the outline.
            // Frame-border edges (no topology edge) are always external.
            // This replaces the legacy grid `[bool; 4]` connection table —
            // any topology that emits `topology_edges` alongside its
            // `edge_svgs` gets the same hover/owned outline behaviour.
            let mut external_path = String::new();
            if let Some(game) = snapshot.game.as_ref() {
                let logical = &game.playable.logical;
                let svgs = &piece_geometry.edge_svgs;
                for (edge_idx, topo_edge) in piece_geometry.topology_edges.iter().enumerate() {
                    let active = topo_edge
                        .and_then(|edge| logical.is_edge_active(edge))
                        .unwrap_or(false);
                    if !active {
                        if let Some(svg) = svgs.get(edge_idx) {
                            if !external_path.is_empty() {
                                external_path.push(' ');
                            }
                            external_path.push_str(svg);
                        }
                    }
                }
            }
            // If no per-edge data is available (e.g. game not yet loaded),
            // fall back to the full piece outline so the hover indicator
            // still has *something* to draw.
            let outline_d = if external_path.is_empty() {
                piece_geometry.outline_svg.clone()
            } else {
                external_path
            };
            if flipped {
                let _ = node.outline_internal.set_attribute("d", &outline_d);
                let _ = node.outline_internal.set_attribute("display", "");
                let _ = node.outline_external.set_attribute("d", "");
                let _ = node.outline_external.set_attribute("display", "none");
            } else {
                let _ = node.outline_external.set_attribute("d", &outline_d);
                let _ = node.outline_external.set_attribute("display", "");
                let _ = node.outline_internal.set_attribute("d", "");
                let _ = node.outline_internal.set_attribute("display", "none");
            }
            let _ = node
                .outline_simple
                .set_attribute("d", &piece_geometry.outline_svg);
        }
    }

    fn update_z_order(&self, snapshot: &AppSnapshot) {
        let total = snapshot
            .game
            .as_ref()
            .map(|game| game.playable.piece_count())
            .unwrap_or(snapshot.z_order.len());
        if total == 0 {
            return;
        }
        let mut order = if crate::app_core::z_order_is_valid(&snapshot.z_order, total) {
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
        self.view.shell.set_hooks(hooks.clone());
        self._hooks = Some(hooks);
    }

    fn render(&mut self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let sync_view = snapshot_sync_view(sync_view);
        self.view.queue_render_snapshot(snapshot.clone(), sync_view);
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

        Rc::new(Self { root, img })
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
        (
            "feComponentTransfer",
            vec![("in", "SourceAlpha"), ("result", "a")],
        ),
        ("feFuncA", vec![("type", "linear"), ("slope", "1")]),
        (
            "feOffset",
            vec![
                ("in", "a"),
                ("dx", emboss_offset_neg.as_str()),
                ("dy", emboss_offset_neg.as_str()),
                ("result", "aOff"),
            ],
        ),
        (
            "feFlood",
            vec![("flood-color", "#000"), ("result", "black")],
        ),
        (
            "feComposite",
            vec![
                ("in", "black"),
                ("in2", "a"),
                ("operator", "in"),
                ("result", "blackShape"),
            ],
        ),
        (
            "feFlood",
            vec![
                ("flood-color", "#fff"),
                ("flood-opacity", "0.6"),
                ("result", "white"),
            ],
        ),
        (
            "feComposite",
            vec![
                ("in", "white"),
                ("in2", "aOff"),
                ("operator", "in"),
                ("result", "whiteShape"),
            ],
        ),
        (
            "feMorphology",
            vec![
                ("in", "whiteShape"),
                ("operator", "erode"),
                ("radius", "0.6"),
                ("result", "whiteThin"),
            ],
        ),
        (
            "feGaussianBlur",
            vec![
                ("in", "whiteThin"),
                ("stdDeviation", "0.5"),
                ("result", "whiteShapeBlur"),
            ],
        ),
        (
            "feComposite",
            vec![
                ("in", "whiteShapeBlur"),
                ("in2", "blackShape"),
                ("operator", "over"),
                ("result", "overlayFull"),
            ],
        ),
        (
            "feMorphology",
            vec![
                ("in", "a"),
                ("operator", "erode"),
                ("radius", emboss_rim_radius.as_str()),
                ("result", "aInner"),
            ],
        ),
        (
            "feComposite",
            vec![
                ("in", "a"),
                ("in2", "aInner"),
                ("operator", "arithmetic"),
                ("k1", "0"),
                ("k2", "1"),
                ("k3", "-1"),
                ("k4", "0"),
                ("result", "rim"),
            ],
        ),
        (
            "feComposite",
            vec![
                ("in", "overlayFull"),
                ("in2", "rim"),
                ("operator", "in"),
                ("result", "overlayRim"),
            ],
        ),
        (
            "feComponentTransfer",
            vec![("in", "overlayRim"), ("result", "overlayRimOpacity")],
        ),
        (
            "feFuncA",
            vec![("type", "linear"), ("slope", emboss_opacity.as_str())],
        ),
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
