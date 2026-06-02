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

    pub(crate) fn is_click_with_external_moved(&self, now_ms: f32, external_moved: bool) -> bool {
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

    /// Press timestamp (same clock as the `now_ms` passed to [`arm`]).
    pub(crate) fn start_ms(&self) -> f32 {
        self.start_ms
    }

    /// Whether the pointer has moved beyond the click slop (no longer a click).
    pub(crate) fn moved(&self) -> bool {
        self.moved
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

/// Tunable weights and thresholds for [`ShakeDetector`]. Every sub-score is
/// normalized to `[0, 1]` and combined as a weighted product, so any single
/// disqualifying factor (too few reversals, runaway amplitude, a circular
/// path) drives the composite toward zero and vetoes the gesture. The
/// per-factor exponents let you bias how harshly each indicator contributes
/// without changing the others.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ShakeTuning {
    /// Samples older than this (relative to the newest) are discarded, so the
    /// gesture is evaluated over a short rolling window.
    pub window_ms: f32,
    /// Minimum number of direction reversals within the window before the
    /// gesture is allowed to fire (hard gate).
    pub min_reversals: u32,
    /// Back-travel must exceed this fraction of the just-completed leg's length
    /// before it counts as a reversal. Hysteresis against jitter/overshoot.
    pub reversal_hysteresis: f32,
    /// Minimum leg length, as a fraction of the reference length, for a leg to
    /// seed/close a direction. Movements below this are treated as noise.
    pub min_leg_ratio: f32,
    /// Lower edge of the amplitude band (fraction of the reference length).
    /// Legs shorter than this score below 1 (jitter is not a deliberate shake).
    pub amp_lo_ratio: f32,
    /// Upper edge of the amplitude band (fraction of the reference length).
    /// Legs longer than this score below 1 (a big sweep is a reposition, not a
    /// shake).
    pub amp_hi_ratio: f32,
    /// Lower edge of the speed band, in reference-lengths per second. Legs
    /// traversed slower than this score 0 — a slow, careful move (even with
    /// small back-and-forth corrections) is not a shake.
    pub speed_lo_ratio: f32,
    /// Upper edge of the speed band, in reference-lengths per second. Legs at or
    /// above this score 1; the score ramps linearly between the two edges.
    pub speed_hi_ratio: f32,
    /// Exponent applied to the reversal-count sub-score.
    pub w_reversal: f32,
    /// Exponent applied to the amplitude-band sub-score.
    pub w_amplitude: f32,
    /// Exponent applied to the speed-band sub-score.
    pub w_speed: f32,
    /// Exponent applied to the axis-consistency sub-score.
    pub w_axis: f32,
    /// Exponent applied to the anti-parallel sub-score.
    pub w_antiparallel: f32,
    /// Composite score at/above which the gesture fires.
    pub fire_threshold: f32,
}

impl Default for ShakeTuning {
    fn default() -> Self {
        Self {
            window_ms: 650.0,
            min_reversals: 3,
            reversal_hysteresis: 0.30,
            min_leg_ratio: 0.12,
            amp_lo_ratio: 0.22,
            amp_hi_ratio: 2.6,
            speed_lo_ratio: 2.5,
            speed_hi_ratio: 6.0,
            w_reversal: 1.0,
            w_amplitude: 1.0,
            w_speed: 1.0,
            w_axis: 1.0,
            w_antiparallel: 1.0,
            fire_threshold: 0.6,
        }
    }
}

#[derive(Clone, Copy)]
struct ShakeSample {
    x: f32,
    y: f32,
    t_ms: f32,
}

/// A completed (or in-progress) straight segment of pointer travel between two
/// turning points. `dir` is the unit direction; `len` the segment length.
#[derive(Clone, Copy)]
struct ShakeLeg {
    dir: (f32, f32),
    len: f32,
    /// Elapsed time from the leg's start (last turning point) to its far
    /// extreme, used to judge traversal speed.
    dur_ms: f32,
}

/// Detects a "shake" — rapid, roughly-collinear back-and-forth pointer motion —
/// while a drag is in progress, distinguishing it from circular swirls, large
/// sweeps, and idle jitter via a product of independent sub-scores (see
/// [`ShakeTuning`]). One-shot: once it fires it stays latched until a fresh
/// detector is created for the next drag.
#[allow(dead_code)]
#[derive(Clone)]
pub(crate) struct ShakeDetector {
    tuning: ShakeTuning,
    samples: Vec<ShakeSample>,
    fired: bool,
}

#[allow(dead_code)]
impl ShakeDetector {
    pub(crate) fn new() -> Self {
        Self::with_tuning(ShakeTuning::default())
    }

    pub(crate) fn with_tuning(tuning: ShakeTuning) -> Self {
        Self {
            tuning,
            samples: Vec::new(),
            fired: false,
        }
    }

    /// Feed a new pointer sample. `reference_len` is the length (in the same
    /// units as `x`/`y`) that amplitude is judged against — typically the
    /// smallest piece's extent. Returns `true` exactly once, on the update
    /// where the gesture first crosses the fire threshold.
    pub(crate) fn update(&mut self, x: f32, y: f32, t_ms: f32, reference_len: f32) -> bool {
        self.samples.push(ShakeSample { x, y, t_ms });
        let cutoff = t_ms - self.tuning.window_ms;
        // Keep one sample just before the cutoff so the oldest in-window leg is
        // measured from a real anchor rather than a truncated one.
        let drop_until = self
            .samples
            .iter()
            .position(|s| s.t_ms >= cutoff)
            .unwrap_or(0)
            .saturating_sub(1);
        if drop_until > 0 {
            self.samples.drain(0..drop_until);
        }
        if self.fired {
            return false;
        }
        if reference_len <= 0.0 {
            return false;
        }
        let score = self.score(reference_len);
        if score >= self.tuning.fire_threshold {
            self.fired = true;
            return true;
        }
        false
    }

    pub(crate) fn has_fired(&self) -> bool {
        self.fired
    }

    /// Current composite score in `[0, 1]`. Exposed for tuning/diagnostics.
    pub(crate) fn score(&self, reference_len: f32) -> f32 {
        if reference_len <= 0.0 {
            return 0.0;
        }
        let min_leg = self.tuning.min_leg_ratio * reference_len;
        let (legs, reversals) = self.extract_legs(min_leg);
        if reversals < self.tuning.min_reversals || legs.len() < 2 {
            return 0.0;
        }
        let reversal_score =
            (reversals as f32 / self.tuning.min_reversals.max(1) as f32).clamp(0.0, 1.0);
        let amplitude_score = self.amplitude_score(&legs, reference_len);
        let speed_score = self.speed_score(&legs, reference_len);
        let axis_score = axis_consistency_score(&legs);
        let antiparallel_score = antiparallel_score(&legs);

        powf_safe(reversal_score, self.tuning.w_reversal)
            * powf_safe(amplitude_score, self.tuning.w_amplitude)
            * powf_safe(speed_score, self.tuning.w_speed)
            * powf_safe(axis_score, self.tuning.w_axis)
            * powf_safe(antiparallel_score, self.tuning.w_antiparallel)
    }

    /// Walk the buffered samples and split them into legs at turning points. A
    /// turning point is registered when back-travel from a leg's far extreme
    /// exceeds `reversal_hysteresis` of that leg's length. Returns the legs
    /// (including the final in-progress one) and the number of reversals.
    fn extract_legs(&self, min_leg: f32) -> (Vec<ShakeLeg>, u32) {
        let mut legs = Vec::new();
        let mut reversals = 0u32;
        if self.samples.len() < 2 || min_leg <= 0.0 {
            return (legs, reversals);
        }
        let mut anchor = self.samples[0];
        let mut dir: Option<(f32, f32)> = None;
        let mut max_proj = 0.0f32;
        let mut extreme = anchor;
        for s in &self.samples[1..] {
            match dir {
                None => {
                    let v = (s.x - anchor.x, s.y - anchor.y);
                    let len = (v.0 * v.0 + v.1 * v.1).sqrt();
                    if len >= min_leg {
                        dir = Some((v.0 / len, v.1 / len));
                        max_proj = len;
                        extreme = *s;
                    }
                }
                Some(d) => {
                    let proj = (s.x - anchor.x) * d.0 + (s.y - anchor.y) * d.1;
                    if proj > max_proj {
                        max_proj = proj;
                        extreme = *s;
                    } else {
                        let back = max_proj - proj;
                        if max_proj >= min_leg && back >= self.tuning.reversal_hysteresis * max_proj
                        {
                            legs.push(ShakeLeg {
                                dir: d,
                                len: max_proj,
                                dur_ms: extreme.t_ms - anchor.t_ms,
                            });
                            reversals += 1;
                            anchor = extreme;
                            let nv = (s.x - extreme.x, s.y - extreme.y);
                            let nlen = (nv.0 * nv.0 + nv.1 * nv.1).sqrt();
                            if nlen >= min_leg {
                                dir = Some((nv.0 / nlen, nv.1 / nlen));
                                max_proj = nlen;
                                extreme = *s;
                            } else {
                                dir = None;
                                max_proj = 0.0;
                                extreme = anchor;
                            }
                        }
                    }
                }
            }
        }
        // Include the trailing in-progress leg so amplitude/axis reflect it too.
        if let Some(d) = dir {
            if max_proj >= min_leg {
                legs.push(ShakeLeg {
                    dir: d,
                    len: max_proj,
                    dur_ms: extreme.t_ms - anchor.t_ms,
                });
            }
        }
        (legs, reversals)
    }

    /// Geometric mean of a per-leg band function: 1.0 inside the amplitude
    /// band, ramping down for tiny legs and decaying for oversized ones.
    fn amplitude_score(&self, legs: &[ShakeLeg], reference_len: f32) -> f32 {
        if legs.is_empty() {
            return 0.0;
        }
        let lo = self.tuning.amp_lo_ratio.max(1.0e-4);
        let hi = self.tuning.amp_hi_ratio.max(lo);
        let mut log_sum = 0.0f32;
        for leg in legs {
            let r = leg.len / reference_len;
            let band = if r < lo {
                r / lo
            } else if r > hi {
                hi / r
            } else {
                1.0
            };
            log_sum += band.max(1.0e-4).ln();
        }
        (log_sum / legs.len() as f32).exp()
    }

    /// Geometric mean of a per-leg speed ramp (reference-lengths per second):
    /// 0 below `speed_lo_ratio`, 1 at/above `speed_hi_ratio`, linear between.
    /// A slow drag — even one that wanders back and forth — scores near 0 here,
    /// which vetoes the gesture regardless of how many reversals accrue.
    fn speed_score(&self, legs: &[ShakeLeg], reference_len: f32) -> f32 {
        if legs.is_empty() {
            return 0.0;
        }
        let lo = self.tuning.speed_lo_ratio.max(0.0);
        let hi = self.tuning.speed_hi_ratio.max(lo + 1.0e-4);
        let mut log_sum = 0.0f32;
        for leg in legs {
            // ref-lengths per second; treat a zero-duration leg as instantaneous
            // (max speed) rather than dividing by zero.
            let speed = if leg.dur_ms > 0.0 {
                (leg.len / reference_len) * 1000.0 / leg.dur_ms
            } else {
                hi
            };
            let ramp = ((speed - lo) / (hi - lo)).clamp(0.0, 1.0);
            log_sum += ramp.max(1.0e-4).ln();
        }
        (log_sum / legs.len() as f32).exp()
    }
}

/// Collinearity of the legs via a doubled-angle resultant: opposite directions
/// map to the same axis, so a clean back-and-forth yields ~1 while a circular
/// path (directions spread around the compass) yields a small magnitude.
fn axis_consistency_score(legs: &[ShakeLeg]) -> f32 {
    let mut sx = 0.0f32;
    let mut sy = 0.0f32;
    let mut total = 0.0f32;
    for leg in legs {
        let angle = leg.dir.1.atan2(leg.dir.0);
        sx += leg.len * (2.0 * angle).cos();
        sy += leg.len * (2.0 * angle).sin();
        total += leg.len;
    }
    if total <= 0.0 {
        return 0.0;
    }
    ((sx * sx + sy * sy).sqrt() / total).clamp(0.0, 1.0)
}

/// Mean of how anti-parallel consecutive legs are. A true shake reverses ~180°
/// each leg (dot ≈ -1 ⇒ score ≈ 1); curved/circular motion turns by smaller
/// angles (dot ≳ 0 ⇒ score ≈ 0).
fn antiparallel_score(legs: &[ShakeLeg]) -> f32 {
    if legs.len() < 2 {
        return 0.0;
    }
    let mut sum = 0.0f32;
    for pair in legs.windows(2) {
        let dot = pair[0].dir.0 * pair[1].dir.0 + pair[0].dir.1 * pair[1].dir.1;
        sum += (-dot).clamp(0.0, 1.0);
    }
    sum / (legs.len() - 1) as f32
}

/// `base.powf(exp)` guarded against `0^0`/NaN so a zeroed sub-score reliably
/// vetoes the product.
fn powf_safe(base: f32, exp: f32) -> f32 {
    if base <= 0.0 {
        return 0.0;
    }
    base.powf(exp)
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

#[cfg(test)]
mod shake_tests {
    use super::*;

    const REF_LEN: f32 = 100.0;
    const DT_MS: f32 = 16.0;

    /// Linearly interpolate between endpoints, `steps` segments per leg, and
    /// feed the path to a fresh detector. Returns whether it fired.
    fn run_path(points: &[(f32, f32)]) -> bool {
        run_path_with(points, 5, DT_MS)
    }

    /// As [`run_path`], but with explicit per-leg segment count and per-sample
    /// timestep so tests can control traversal speed.
    fn run_path_with(points: &[(f32, f32)], steps: usize, dt_ms: f32) -> bool {
        let mut detector = ShakeDetector::new();
        let mut t = 0.0f32;
        let mut fired = false;
        for win in points.windows(2) {
            let (ax, ay) = win[0];
            let (bx, by) = win[1];
            for i in 1..=steps {
                let f = i as f32 / steps as f32;
                let x = ax + (bx - ax) * f;
                let y = ay + (by - ay) * f;
                t += dt_ms;
                if detector.update(x, y, t, REF_LEN) {
                    fired = true;
                }
            }
        }
        fired
    }

    #[test]
    fn fires_on_clean_horizontal_shake() {
        // Back-and-forth along x: three sharp ~180° reversals at shake scale.
        let path = [
            (0.0, 0.0),
            (50.0, 0.0),
            (-50.0, 0.0),
            (50.0, 0.0),
            (-50.0, 0.0),
        ];
        assert!(run_path(&path), "a clean horizontal shake should fire");
    }

    #[test]
    fn ignores_circular_motion() {
        // A full loop: directions spread around the compass, so axis
        // consistency collapses even though projections rise and fall.
        let mut path = Vec::new();
        let r = 55.0;
        for i in 0..=32 {
            let a = std::f32::consts::TAU * i as f32 / 32.0;
            path.push((r * a.cos(), r * a.sin()));
        }
        assert!(!run_path(&path), "a circular swirl should not fire");
    }

    #[test]
    fn ignores_slow_back_and_forth() {
        // Same shake-shaped path as `fires_on_clean_horizontal_shake`, but
        // traversed slowly (careful placement with corrective wiggles). The
        // speed sub-score should veto it even though the reversals are there.
        let path = [
            (0.0, 0.0),
            (50.0, 0.0),
            (-50.0, 0.0),
            (50.0, 0.0),
            (-50.0, 0.0),
        ];
        // 10 samples per leg at 40ms => ~400ms per 100px leg ≈ 2.5 ref/s.
        assert!(
            !run_path_with(&path, 10, 40.0),
            "a slow back-and-forth should not fire"
        );
    }

    #[test]
    fn ignores_single_large_sweep() {
        // One long drag in a single direction: no reversals.
        let path = [(0.0, 0.0), (400.0, 0.0)];
        assert!(!run_path(&path), "a single sweep should not fire");
    }

    #[test]
    fn ignores_micro_jitter() {
        // Sub-piece jitter is below the noise floor and seeds no legs.
        let path = [
            (0.0, 0.0),
            (3.0, 1.0),
            (-2.0, -1.0),
            (3.0, 0.0),
            (-3.0, 1.0),
            (2.0, -1.0),
        ];
        assert!(!run_path(&path), "tiny jitter should not fire");
    }

    #[test]
    fn fires_only_once() {
        let mut detector = ShakeDetector::new();
        let mut t = 0.0f32;
        let mut fire_count = 0;
        // Keep shaking well past the first fire; it must latch.
        let endpoints = [50.0f32, -50.0, 50.0, -50.0, 50.0, -50.0, 50.0, -50.0];
        let mut prev = 0.0f32;
        for &target in &endpoints {
            for i in 1..=5 {
                let f = i as f32 / 5.0;
                let x = prev + (target - prev) * f;
                t += DT_MS;
                if detector.update(x, 0.0, t, REF_LEN) {
                    fire_count += 1;
                }
            }
            prev = target;
        }
        assert_eq!(fire_count, 1, "the gesture must fire exactly once per drag");
    }

    #[test]
    fn diagonal_shake_still_fires() {
        // Axis need not be horizontal/vertical — a collinear diagonal works.
        let path = [
            (0.0, 0.0),
            (40.0, 40.0),
            (-40.0, -40.0),
            (40.0, 40.0),
            (-40.0, -40.0),
        ];
        assert!(run_path(&path), "a diagonal shake should fire");
    }
}
