use std::collections::HashMap;

use web_sys::{DomRect, Element, HtmlCanvasElement, WheelEvent};

use crate::app_core::ViewRect;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PointerKind {
    Mouse,
    Touch,
    Pen,
    Unknown,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct PointerId(pub i32);

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct InputModifiers {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub meta: bool,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub(crate) struct InputPointer {
    pub id: PointerId,
    pub kind: PointerKind,
    pub is_primary: bool,
    pub buttons: u16,
    pub pressure: f32,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum InputEventKind {
    Down,
    Move,
    Up,
    Cancel,
    Hover,
    Leave,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub(crate) struct InputEvent {
    pub kind: InputEventKind,
    pub pointer: InputPointer,
    pub view_x: f32,
    pub view_y: f32,
    pub modifiers: InputModifiers,
}

pub(crate) const INPUT_KIND_COOLDOWN_MS: f32 = 2000.0;

#[derive(Clone, Copy)]
pub(crate) struct PointerSample {
    pub screen_x: f32,
    pub screen_y: f32,
    pub kind: PointerKind,
    pub buttons: u16,
}

pub(crate) struct PointerPolicy {
    last_kind: Option<PointerKind>,
    last_ts: f32,
    active_kind: Option<PointerKind>,
    active_pointers: HashMap<i32, PointerSample>,
}

impl PointerPolicy {
    pub(crate) fn new() -> Self {
        Self {
            last_kind: None,
            last_ts: 0.0,
            active_kind: None,
            active_pointers: HashMap::new(),
        }
    }

    pub(crate) fn kind_changed(&self, kind: PointerKind) -> bool {
        self.last_kind.map(|last| last != kind).unwrap_or(false)
    }

    pub(crate) fn accept_kind(&mut self, kind: PointerKind, now_ms: f32) -> bool {
        if let Some(last) = self.last_kind {
            if last != kind && (now_ms - self.last_ts) < INPUT_KIND_COOLDOWN_MS {
                return false;
            }
        }
        self.last_kind = Some(kind);
        self.last_ts = now_ms;
        true
    }

    pub(crate) fn insert_pointer(
        &mut self,
        id: i32,
        kind: PointerKind,
        screen_x: f32,
        screen_y: f32,
        buttons: u16,
    ) {
        if let Some(active_kind) = self.active_kind {
            if active_kind != kind {
                self.active_pointers.clear();
            }
        }
        self.active_kind = Some(kind);
        self.active_pointers.insert(
            id,
            PointerSample {
                screen_x,
                screen_y,
                kind,
                buttons,
            },
        );
    }

    pub(crate) fn update_pointer(
        &mut self,
        id: i32,
        kind: PointerKind,
        screen_x: f32,
        screen_y: f32,
        buttons: u16,
    ) {
        if let Some(sample) = self.active_pointers.get_mut(&id) {
            sample.screen_x = screen_x;
            sample.screen_y = screen_y;
            sample.kind = kind;
            sample.buttons = buttons;
        }
    }

    pub(crate) fn remove_pointer(&mut self, id: i32) {
        self.active_pointers.remove(&id);
        if self.active_pointers.is_empty() {
            self.active_kind = None;
        }
    }

    pub(crate) fn clear_active(&mut self) {
        self.active_pointers.clear();
        self.active_kind = None;
    }

    pub(crate) fn pointer_sample(&self, id: i32) -> Option<PointerSample> {
        self.active_pointers.get(&id).copied()
    }

    pub(crate) fn active_touch_points(&self) -> Vec<(i32, PointerSample)> {
        let mut points: Vec<_> = self
            .active_pointers
            .iter()
            .filter_map(|(id, sample)| {
                if sample.kind == PointerKind::Touch {
                    Some((*id, *sample))
                } else {
                    None
                }
            })
            .collect();
        points.sort_by_key(|(id, _)| *id);
        points
    }
}

#[allow(dead_code)]
impl PointerKind {
    pub(crate) fn from_pointer_type(value: &str) -> Self {
        match value {
            "mouse" => PointerKind::Mouse,
            "touch" => PointerKind::Touch,
            "pen" => PointerKind::Pen,
            _ => PointerKind::Unknown,
        }
    }
}

pub(crate) const CLICK_MAX_DURATION_MS: f32 = 240.0;
pub(crate) const CLICK_QUICK_TAP_MS: f32 = 120.0;
pub(crate) const CLICK_DEFAULT_SLOP_PX: f32 = 4.0;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub(crate) struct ClickGesture {
    start: [f32; 2],
    start_ms: f32,
    max_slop: f32,
    moved: bool,
    active: bool,
}

#[allow(dead_code)]
impl ClickGesture {
    pub(crate) fn new_default() -> Self {
        Self::new_with_slop(CLICK_DEFAULT_SLOP_PX)
    }

    pub(crate) fn new_with_slop(max_slop: f32) -> Self {
        Self {
            start: [0.0, 0.0],
            start_ms: 0.0,
            max_slop,
            moved: false,
            active: false,
        }
    }

    pub(crate) fn arm(&mut self, x: f32, y: f32, now_ms: f32) {
        self.start = [x, y];
        self.start_ms = now_ms;
        self.moved = false;
        self.active = true;
    }

    pub(crate) fn update(&mut self, x: f32, y: f32) {
        if !self.active || self.moved {
            return;
        }
        let dx = x - self.start[0];
        let dy = y - self.start[1];
        if dx * dx + dy * dy > self.max_slop * self.max_slop {
            self.moved = true;
        }
    }

    pub(crate) fn is_click(&self, now_ms: f32) -> bool {
        self.is_click_with_external_moved(now_ms, false)
    }

    pub(crate) fn is_click_with_external_moved(
        &self,
        now_ms: f32,
        external_moved: bool,
    ) -> bool {
        if !self.active || external_moved {
            return false;
        }
        let elapsed = (now_ms - self.start_ms).max(0.0);
        if CLICK_QUICK_TAP_MS > 0.0 && elapsed <= CLICK_QUICK_TAP_MS {
            return true;
        }
        if self.moved {
            return false;
        }
        if CLICK_MAX_DURATION_MS <= 0.0 {
            return true;
        }
        elapsed <= CLICK_MAX_DURATION_MS
    }

    pub(crate) fn clear(&mut self) {
        self.active = false;
        self.moved = false;
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub(crate) struct DragSlopGate {
    start: [f32; 2],
    slop: f32,
    moved: bool,
}

#[allow(dead_code)]
impl DragSlopGate {
    pub(crate) fn new(start_x: f32, start_y: f32, slop: f32) -> Self {
        Self {
            start: [start_x, start_y],
            slop,
            moved: false,
        }
    }

    pub(crate) fn update(&mut self, x: f32, y: f32) -> bool {
        if self.moved {
            return true;
        }
        let dx = x - self.start[0];
        let dy = y - self.start[1];
        if dx * dx + dy * dy > self.slop * self.slop {
            self.moved = true;
        }
        self.moved
    }
}

pub(crate) trait HasClientRect {
    fn client_rect(&self) -> DomRect;
}

impl HasClientRect for HtmlCanvasElement {
    fn client_rect(&self) -> DomRect {
        self.get_bounding_client_rect()
    }
}

impl HasClientRect for Element {
    fn client_rect(&self) -> DomRect {
        self.get_bounding_client_rect()
    }
}

pub(crate) fn screen_to_view_coords(
    screen_x: f32,
    screen_y: f32,
    element: &impl HasClientRect,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = element.client_rect();
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

pub(crate) fn screen_delta_to_world(
    dx_screen: f32,
    dy_screen: f32,
    element: &impl HasClientRect,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = element.client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let scale_x = view.width / rect_width;
    let scale_y = view.height / rect_height;
    Some((-dx_screen * scale_x, -dy_screen * scale_y))
}

pub(crate) fn screen_scroll_to_world(
    dx_screen: f32,
    dy_screen: f32,
    element: &impl HasClientRect,
    view: ViewRect,
) -> Option<(f32, f32)> {
    let rect = element.client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let scale_x = view.width / rect_width;
    let scale_y = view.height / rect_height;
    Some((-dx_screen * scale_x, dy_screen * scale_y))
}

const PINCH_WHEEL_MIN_DY: f32 = 0.01;
const PINCH_WHEEL_MAX_DY: f32 = 12.0;
const PINCH_WHEEL_MAX_DX: f32 = 3.0;
const WHEEL_INTENT_STICKY_MS: f32 = 160.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum WheelIntent {
    Pan,
    Zoom,
}

pub(crate) struct WheelIntentTracker {
    last_intent: Option<WheelIntent>,
    last_ts: f32,
    last_ctrl: bool,
    last_meta: bool,
}

impl WheelIntentTracker {
    pub(crate) fn new() -> Self {
        Self {
            last_intent: None,
            last_ts: 0.0,
            last_ctrl: false,
            last_meta: false,
        }
    }

    pub(crate) fn decide(&mut self, event: &WheelEvent, now_ms: f32) -> WheelIntent {
        let ctrl = event.ctrl_key();
        let meta = event.meta_key();
        if let Some(intent) = self.last_intent {
            if ctrl == self.last_ctrl
                && meta == self.last_meta
                && (now_ms - self.last_ts) <= WHEEL_INTENT_STICKY_MS
            {
                self.last_ts = now_ms;
                return intent;
            }
        }
        let intent = if wheel_should_zoom(event) {
            WheelIntent::Zoom
        } else {
            WheelIntent::Pan
        };
        self.last_intent = Some(intent);
        self.last_ts = now_ms;
        self.last_ctrl = ctrl;
        self.last_meta = meta;
        intent
    }
}

pub(crate) fn wheel_should_zoom(event: &WheelEvent) -> bool {
    if !event.ctrl_key() {
        return false;
    }
    if event.delta_mode() != 0 {
        return false;
    }
    let dx = event.delta_x().abs() as f32;
    let dy = event.delta_y().abs() as f32;
    if dy <= PINCH_WHEEL_MIN_DY {
        return false;
    }
    dx <= PINCH_WHEEL_MAX_DX && dy <= PINCH_WHEEL_MAX_DY
}

pub(crate) fn screen_slop_to_puzzle(
    slop_px: f32,
    element: &impl HasClientRect,
    view: ViewRect,
    puzzle_scale: f32,
) -> f32 {
    let rect = element.client_rect();
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

pub(crate) fn workspace_to_puzzle_coords(scale: f32, x: f32, y: f32) -> (f32, f32) {
    let scale = scale.max(1.0e-4);
    (x / scale, y / scale)
}
