use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
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

use crate::app_core::{AppCore, AppSnapshot, ViewRect};
use crate::app_router;
use crate::sync_runtime;
use crate::core::*;
use crate::renderer::{
    build_mask_atlas, Instance, InstanceBatch, InstanceSet, MaskAtlasData, UiRotationOrigin,
    UiTextId, UiTextSpec, WgpuRenderer,
};
use crate::runtime::{CoreAction, GameSyncView, GameView, ViewHooks};
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
const OUTLINE_KIND_OWNED: f32 = 2.0;
const OUTLINE_KIND_DEBUG: f32 = 3.0;
const WGPU_FPS_CAP_DEFAULT: f32 = 60.0;
const WGPU_FPS_IDLE_RESET_MS: f32 = 800.0;

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
        overlay.update_classes();
        overlay.install_listeners();
        overlay
    }

    fn install_listeners(self: &Rc<Self>) {
        let mut listeners = Vec::new();
        let overlay = Rc::clone(self);
        listeners.push(EventListener::new(&self.toggle, "click", move |_event| {
            overlay.toggle_revealed();
        }));
        let overlay = Rc::clone(self);
        listeners.push(EventListener::new(&self.arrow_h, "click", move |_event| {
            overlay.bump_horizontal();
        }));
        let overlay = Rc::clone(self);
        listeners.push(EventListener::new(&self.arrow_v, "click", move |_event| {
            overlay.bump_vertical();
        }));
        let overlay = Rc::clone(self);
        listeners.push(EventListener::new(&self.img, "click", move |_event| {
            overlay.hide_if_revealed();
        }));
        *self.listeners.borrow_mut() = listeners;
    }

    fn set_image_src(&self, src: &str) {
        self.img.set_src(src);
    }

    fn set_visible(&self, visible: bool) {
        if visible {
            let _ = self.root.remove_attribute("style");
        } else {
            let _ = self.root.set_attribute("style", "display: none;");
        }
    }

    fn toggle_revealed(&self) {
        let next = !self.revealed.get();
        self.revealed.set(next);
        self.update_classes();
    }

    fn hide_if_revealed(&self) {
        if self.revealed.get() {
            self.revealed.set(false);
            self.update_classes();
        }
    }

    fn bump_horizontal(&self) {
        let next = match self.corner.get() {
            PreviewCorner::BottomLeft => PreviewCorner::BottomRight,
            PreviewCorner::BottomRight => PreviewCorner::BottomLeft,
            PreviewCorner::TopLeft => PreviewCorner::TopRight,
            PreviewCorner::TopRight => PreviewCorner::TopLeft,
        };
        self.corner.set(next);
        self.update_classes();
    }

    fn bump_vertical(&self) {
        let next = match self.corner.get() {
            PreviewCorner::BottomLeft => PreviewCorner::TopLeft,
            PreviewCorner::BottomRight => PreviewCorner::TopRight,
            PreviewCorner::TopLeft => PreviewCorner::BottomLeft,
            PreviewCorner::TopRight => PreviewCorner::BottomRight,
        };
        self.corner.set(next);
        self.update_classes();
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
        self.root.set_class_name(&format!("preview-box {} {}", corner_class, state_class));
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

pub(crate) fn run() {
    #[cfg(target_arch = "wasm32")]
    {
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document
            .get_element_by_id("wgpu-root")
            .expect("wgpu-root exists");

        let renderer_kind = app_router::load_renderer_preference();
        let core = AppCore::shared();
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

        let view = Rc::new(WgpuView::new(core.clone(), root.clone(), canvas));
        let preview = PreviewOverlay::new(&document);
        root.append_child(&preview.root).ok();
        *view.preview.borrow_mut() = Some(preview);
        *view.sync_hook.borrow_mut() = Some(sync_runtime::register_sync_view_hook(Rc::new(
            move || {
                request_render();
            },
        )));
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
    creating: Cell<bool>,
    pending_instances: RefCell<Option<InstanceSet>>,
    pending_ui: RefCell<Option<Vec<UiTextSpec>>>,
    ui_measure: RefCell<GlyphonMeasureState>,
    ui_credit_hitbox: RefCell<Option<UiHitbox>>,
    ui_credit_hovered: Cell<bool>,
    pan_state: RefCell<Option<PanState>>,
    pinch_state: RefCell<Option<PinchState>>,
    touch_drag_gate: RefCell<Option<TouchDragGate>>,
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
    preview: RefCell<Option<Rc<PreviewOverlay>>>,
}

impl WgpuView {
    fn new(core: Rc<AppCore>, root: Element, canvas: HtmlCanvasElement) -> Self {
        Self {
            core,
            root,
            canvas,
            image: RefCell::new(None),
            renderer: RefCell::new(None),
            creating: Cell::new(false),
            pending_instances: RefCell::new(None),
            pending_ui: RefCell::new(None),
            ui_measure: RefCell::new(GlyphonMeasureState::new()),
            ui_credit_hitbox: RefCell::new(None),
            ui_credit_hovered: Cell::new(false),
            pan_state: RefCell::new(None),
            pinch_state: RefCell::new(None),
            touch_drag_gate: RefCell::new(None),
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
            preview: RefCell::new(None),
        }
    }

    fn set_image(&self, image: HtmlImageElement) {
        *self.image.borrow_mut() = Some(image);
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
        let render_scale = snapshot.wgpu_settings.render_scale;
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
            let Some((view_x, view_y)) =
                event_to_canvas_coords(event, &canvas_for_down, snapshot.view)
            else {
                return;
            };
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                let sync_view = sync_runtime::sync_view();
                if piece_owned_by_other(&snapshot, &sync_view, piece_id) {
                    return;
                }
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
                let sync_view = sync_runtime::sync_view();
                view.update_canvas_class(&snapshot, &sync_view);
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
                let sync_view = sync_runtime::sync_view();
                view.update_canvas_class(&snapshot, &sync_view);
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
            if !snapshot.dragging_members.is_empty() {
                return;
            }
            if view.pan_state.borrow().is_some() {
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
                    let sync_view = sync_runtime::sync_view();
                    view.update_canvas_class(&snapshot, &sync_view);
                }
                view.dispatch_action(CoreAction::SetHovered { hovered });
                return;
            }
            let credit_hovered = view.hit_credit(view_x, view_y);
            if view.ui_credit_hovered.get() != credit_hovered {
                view.ui_credit_hovered.set(credit_hovered);
                let sync_view = sync_runtime::sync_view();
                view.update_canvas_class(&snapshot, &sync_view);
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
            let snapshot = core_for_leave.snapshot();
            let sync_view = sync_runtime::sync_view();
            view.update_canvas_class(&snapshot, &sync_view);
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
                        let sync_view = sync_runtime::sync_view();
                        view.update_canvas_class(&snapshot, &sync_view);
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
                screen_to_view_coords(screen_x, screen_y, &canvas_for_touch, snapshot.view)
            else {
                return;
            };
            let (px, py) = workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
            if let Some(piece_id) = view.pick_piece(px, py, &snapshot) {
                let sync_view = sync_runtime::sync_view();
                if piece_owned_by_other(&snapshot, &sync_view, piece_id) {
                    return;
                }
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
            let sync_view = sync_runtime::sync_view();
            view.update_canvas_class(&snapshot, &sync_view);
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
                if !snapshot.dragging_members.is_empty() {
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
                let snapshot = core.snapshot();
                view.touch_drag_gate.borrow_mut().take();
                let mut pan_cleared = false;
                if view.pan_state.borrow().is_some() {
                    view.pan_state.borrow_mut().take();
                    pan_cleared = true;
                    let sync_view = sync_runtime::sync_view();
                    view.update_canvas_class(&snapshot, &sync_view);
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
                let snapshot = core.snapshot();
                view.touch_drag_gate.borrow_mut().take();
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
                    let sync_view = sync_runtime::sync_view();
                    view.update_canvas_class(&snapshot, &sync_view);
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
                let snapshot = core.snapshot();
                let sync_view = sync_runtime::sync_view();
                view.update_canvas_class(&snapshot, &sync_view);
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


    fn update_canvas_class(&self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView) {
        let mut canvas_class = "puzzle-canvas".to_string();
        let is_panning = self.pan_state.borrow().is_some();
        if is_panning {
            canvas_class.push_str(" panning");
        }
        if !snapshot.dragging_members.is_empty() {
            canvas_class.push_str(" dragging");
        } else if !is_panning {
            if snapshot.hovered_id.is_some() {
                canvas_class.push_str(" hover");
            } else if !self.ui_credit_hovered.get() {
                canvas_class.push_str(" pan-ready");
            }
        }
        if let Some(hovered_id) = snapshot.hovered_id {
            if piece_owned_by_other(snapshot, sync_view, hovered_id) {
                canvas_class.push_str(" owned-other");
            }
        }
        if self.ui_credit_hovered.get() {
            canvas_class.push_str(" ui-link-hover");
        }
        self.canvas.set_class_name(&canvas_class);
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
        let show_fps = snapshot.wgpu_settings.show_fps;
        let sync_view = sync_runtime::sync_view();
        self.last_render_ms.set(now_ms());
        self.force_fps_fallback.set(!schedule_idle && show_fps);
        self.render_snapshot(&snapshot, &sync_view);
        if schedule_idle {
            self.schedule_idle_fps_reset(show_fps);
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
        if let Some(preview) = self.preview.borrow().as_ref() {
            if let Some(info) = snapshot.puzzle_info.as_ref() {
                preview.set_image_src(info.image_src.as_str());
                preview.set_visible(true);
            } else {
                preview.set_visible(false);
            }
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
        self.update_canvas_class(snapshot, sync_view);
        let rect = self.canvas.get_bounding_client_rect();
        let viewport_w = rect.width() as f32;
        let viewport_h = rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return;
        }
        let dpr = web_sys::window()
            .map(|window| window.device_pixel_ratio())
            .unwrap_or(1.0) as f32;
        let render_scale = snapshot.wgpu_settings.render_scale.max(WGPU_RENDER_SCALE_MIN);
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
        let is_dark = is_dark_theme(snapshot.theme_mode);
        let (connections_label, border_connections_label) = if let Some(info) = snapshot.puzzle_info.as_ref() {
            let cols = info.cols as usize;
            let rows = info.rows as usize;
            let total = cols * rows;
            if snapshot.connections.len() == total {
                let (connected, border_connected, total_expected, border_expected) =
                    count_connections(&snapshot.connections, cols, rows);
                (
                    format!("{}/{}", connected, total_expected),
                    format!("{}/{}", border_connected, border_expected),
                )
            } else {
                ("--".to_string(), "--".to_string())
            }
        } else {
            ("--".to_string(), "--".to_string())
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
            false,
            is_dark,
        );
        let credit_hitbox = ui_specs
            .iter()
            .find(|spec| matches!(spec.id, UiTextId::Credit))
            .map(|spec| ui_hitbox_for_spec(&mut measure_state, spec));
        *self.ui_credit_hitbox.borrow_mut() = credit_hitbox;

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
            &snapshot.positions,
            &snapshot.rotations,
            &snapshot.flips,
            &snapshot.z_order,
            &snapshot.connections,
            snapshot.hovered_id,
            snapshot.show_debug,
            assets.grid.cols as usize,
            assets.grid.rows as usize,
            assets.piece_width,
            assets.piece_height,
            mask_atlas,
            highlight_members,
            drag_origin,
            drag_dir,
            sync_view.ownership_by_anchor().as_ref(),
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
            renderer.set_edge_aa(snapshot.wgpu_settings.edge_aa);
            renderer.set_show_fps(snapshot.wgpu_settings.show_fps);
            renderer.set_solved(snapshot.solved);
            renderer.update_instances(instances);
            renderer.set_ui_texts(&ui_specs);
            renderer.render();
            return;
        }
        if self.creating.get() {
            *self.pending_instances.borrow_mut() = Some(instances);
            *self.pending_ui.borrow_mut() = Some(ui_specs);
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
        let render_scale = snapshot.wgpu_settings.render_scale;
        let edge_aa = snapshot.wgpu_settings.edge_aa;
        let show_fps = snapshot.wgpu_settings.show_fps;
        let solved = snapshot.solved;
        let is_dark_theme = is_dark;
        let epoch = self.puzzle_epoch.get();
        let view = Rc::clone(self);
        spawn_local(async move {
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
                    renderer.render();
                    *view.renderer.borrow_mut() = Some(renderer);
                }
                Err(err) => {
                    web_sys::console::error_1(&err);
                }
            }
            view.creating.set(false);
        });
        *self.pending_instances.borrow_mut() = Some(instances);
        *self.pending_ui.borrow_mut() = Some(ui_specs);
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

    fn shutdown(&mut self) {}
}

fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
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
    ownership_by_anchor: &HashMap<u32, u64>,
    own_client_id: Option<u64>,
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
    let mut owned_mask = vec![false; total];
    let mut group_id = vec![usize::MAX; total];
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut group_has_owned: Vec<bool> = Vec::new();
    let mut queue = Vec::new();
    for start in 0..total {
        if group_id[start] != usize::MAX {
            continue;
        }
        let owned = ownership_by_anchor
            .get(&(start as u32))
            .map(|owner_id| Some(*owner_id) != own_client_id)
            .unwrap_or(false);
        let gid = groups.len();
        let mut members = Vec::new();
        group_id[start] = gid;
        queue.push(start);
        while let Some(id) = queue.pop() {
            members.push(id);
            if owned {
                owned_mask[id] = true;
            }
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
        group_has_owned.push(owned);
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
            let owned = owned_mask.get(id).copied().unwrap_or(false);
            let mask_origin = mask_atlas.origins.get(id).copied().unwrap_or([0.0, 0.0]);
            instances.push(Instance {
                pos: [render_pos.0, render_pos.1],
                size: [piece_width, piece_height],
                rotation,
                flip: if flipped { 1.0 } else { 0.0 },
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
                piece_origin: [base_x, base_y],
                mask_origin,
            });
        }
        let count = (instances.len() as u32) - start;
        if count == 0 {
            continue;
        }
        let draw_outline = show_debug || group_has_hover[gid] || group_has_owned[gid];
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
