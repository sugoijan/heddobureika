use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::timers::callback::Interval;
use js_sys::Date;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt::Write;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::{
    Element, Event, HtmlImageElement, HtmlInputElement, HtmlSelectElement, InputEvent, KeyboardEvent,
    Touch, TouchEvent,
};
use yew::prelude::*;

const IMAGE_SRC: &str = "puzzles/zoe-potter.jpg";
const PUZZLE_SEED: u64 = 0x5EED_5EED_25_20;
const MAX_LINE_BEND_RATIO: f64 = 0.2;
const SNAP_DISTANCE_RATIO_DEFAULT: f64 = 0.200;
const SNAP_DISTANCE_RATIO_MIN: f64 = 0.050;
const SNAP_DISTANCE_RATIO_MAX: f64 = 0.350;
const SOLVE_TOLERANCE_RATIO: f64 = 0.080;
const ROTATION_STEP_DEG: f64 = 90.0;
const ROTATION_SNAP_TOLERANCE_DEFAULT_DEG: f64 = 5.0;
const ROTATION_SNAP_TOLERANCE_MIN_DEG: f64 = 0.5;
const ROTATION_SNAP_TOLERANCE_MAX_DEG: f64 = 12.0;
const ROTATION_SOLVE_TOLERANCE_DEG: f64 = 1.5;
const ROTATION_LOCK_THRESHOLD_DEFAULT: usize = 4;
const ROTATION_LOCK_THRESHOLD_MIN: usize = 1;
const ROTATION_NOISE_MIN: f64 = 0.0;
const ROTATION_NOISE_MAX: f64 = 6.0;
const ROTATION_NOISE_DEFAULT: f64 = 0.6;
const CLICK_MOVE_RATIO: f64 = 0.08;
const CLICK_MAX_DURATION_MS: f64 = 240.0;
const SNAP_ANIMATION_MS: f64 = 160.0;
const FLIP_CHANCE: f64 = 0.2;
const RUBBER_BAND_RATIO: f64 = 0.35;
const WORKSPACE_SCALE_MIN: f64 = 0.4;
const WORKSPACE_SCALE_MAX: f64 = 2.5;
const WORKSPACE_SCALE_DEFAULT: f64 = 1.7;
const FRAME_SNAP_MIN: f64 = 0.4;
const FRAME_SNAP_MAX: f64 = 3.0;
const FRAME_SNAP_DEFAULT: f64 = 1.0;
const TAB_WIDTH_MIN: f64 = 0.2;
const TAB_WIDTH_MAX: f64 = 0.72;
const TAB_WIDTH_RANGE: f64 = 0.16;
const TAB_DEPTH_MIN: f64 = 0.2;
const TAB_DEPTH_MAX: f64 = 1.1;
const TAB_DEPTH_RANGE: f64 = 0.35;
const TAB_SIZE_SCALE_MIN: f64 = 0.1;
const TAB_SIZE_SCALE_MAX: f64 = 0.5;
const TAB_SIZE_MIN_LIMIT: f64 = 0.02;
const TAB_SIZE_MAX_LIMIT: f64 = 0.24;
const JITTER_STRENGTH_MIN: f64 = 0.0;
const JITTER_STRENGTH_MAX: f64 = 0.3;
const JITTER_LEN_BIAS_MIN: f64 = 0.0;
const JITTER_LEN_BIAS_MAX: f64 = 1.0;
const TAB_DEPTH_CAP_MIN: f64 = 0.2;
const TAB_DEPTH_CAP_MAX: f64 = 0.45;
const CURVE_DETAIL_MIN: f64 = 0.5;
const CURVE_DETAIL_MAX: f64 = 3.0;
const SKEW_RANGE_MAX: f64 = 0.2;
const VARIATION_MIN: f64 = 0.0;
const VARIATION_MAX: f64 = 1.0;
const LINE_BEND_MIN: f64 = 0.0;
const EDGE_STEP_DIV: f64 = 6.0;
const EDGE_STEP_MIN: f64 = 6.0;
const CORNER_RADIUS_RATIO: f64 = 0.05;
const STORAGE_KEY: &str = "heddobureika.board.v1";
const STORAGE_VERSION: u32 = 1;
const DIR_UP: usize = 0;
const DIR_RIGHT: usize = 1;
const DIR_DOWN: usize = 2;
const DIR_LEFT: usize = 3;

#[derive(Clone, Copy, Debug)]
struct Piece {
    id: usize,
    row: u32,
    col: u32,
}

#[derive(Clone, Copy, Debug)]
struct EdgeParams {
    tab_size: f64,
    tab_depth: f64,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
    e: f64,
}

#[derive(Clone, Copy, Debug)]
enum TabSide {
    Tab,
    Blank,
}

impl TabSide {
    fn sign(self) -> i8 {
        match self {
            TabSide::Tab => 1,
            TabSide::Blank => -1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Edge {
    tab_side: TabSide,
    params: EdgeParams,
}

#[derive(Clone, Copy)]
enum EdgeOrientation {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Clone, Copy)]
enum Segment {
    LineTo { x: f64, y: f64 },
}

#[derive(Clone, Copy)]
struct LineWave {
    amplitude: f64,
    skew: f64,
}

struct WarpField<'a> {
    width: f64,
    height: f64,
    horizontal: &'a [LineWave],
    vertical: &'a [LineWave],
}

#[derive(Clone, Debug)]
struct DragState {
    start_x: f64,
    start_y: f64,
    start_time: f64,
    primary_id: usize,
    touch_id: Option<i32>,
    rotate_mode: bool,
    pivot_x: f64,
    pivot_y: f64,
    start_angle: f64,
    members: Vec<usize>,
    start_positions: Vec<(f64, f64)>,
    start_rotations: Vec<f64>,
}

#[derive(Clone, Debug)]
enum AnimationKind {
    Pivot {
        pivot_x: f64,
        pivot_y: f64,
        delta: f64,
    },
    Anchor {
        anchor_id: usize,
        start_center: (f64, f64),
        target_center: (f64, f64),
        start_rot: f64,
        target_rot: f64,
    },
}

#[derive(Clone, Debug)]
struct RotationAnimation {
    start_time: f64,
    duration: f64,
    members: Vec<usize>,
    start_positions: Vec<(f64, f64)>,
    start_rotations: Vec<f64>,
    kind: AnimationKind,
}

#[derive(Clone, Debug)]
struct QueuedRotation {
    members: Vec<usize>,
    pivot_x: f64,
    pivot_y: f64,
    noise: f64,
}

#[derive(Clone, Copy, PartialEq)]
enum PreviewCorner {
    BottomLeft,
    BottomRight,
    TopLeft,
    TopRight,
}

#[derive(Default)]
struct DragHandlers {
    on_move: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_release: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_touch_move: Option<Rc<dyn Fn(&TouchEvent)>>,
    on_touch_release: Option<Rc<dyn Fn(&TouchEvent)>>,
}

#[derive(Clone, Serialize, Deserialize)]
struct SavedBoard {
    version: u32,
    cols: u32,
    rows: u32,
    image_width: u32,
    image_height: u32,
    positions: Vec<(f64, f64)>,
    rotations: Vec<f64>,
    flips: Vec<bool>,
    connections: Vec<[bool; 4]>,
    z_order: Vec<usize>,
    scramble_nonce: u64,
}

#[derive(Clone, PartialEq)]
struct ShapeSettings {
    tab_width: f64,
    tab_depth: f64,
    tab_size_scale: f64,
    tab_size_min: f64,
    tab_size_max: f64,
    jitter_strength: f64,
    jitter_len_bias: f64,
    tab_depth_cap: f64,
    curve_detail: f64,
    skew_range: f64,
    variation: f64,
    line_bend_ratio: f64,
}

impl Default for ShapeSettings {
    fn default() -> Self {
        Self {
            tab_width: 0.43,
            tab_depth: 0.98,
            tab_size_scale: 0.25,
            tab_size_min: 0.04,
            tab_size_max: 0.16,
            jitter_strength: 0.13,
            jitter_len_bias: 0.4,
            tab_depth_cap: 0.32,
            curve_detail: 1.4,
            skew_range: 0.18,
            variation: 0.16,
            line_bend_ratio: 0.06,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct GridChoice {
    target_count: u32,
    cols: u32,
    rows: u32,
    actual_count: u32,
}

const TARGET_PIECE_COUNTS: [u32; 11] = [
    50, 100, 150, 300, 500, 750, 1000, 1500, 2000, 3000, 5000,
];
const DEFAULT_TARGET_COUNT: u32 = 100;
const GRID_REL_COUNT_TOL: f64 = 0.05;
const GRID_PIECE_RATIO_MAX: f64 = 1.42;
const GRID_ROW_MIN: u32 = 2;
const GRID_ROW_WIDEN: f64 = 1.5;
const GRID_NEIGHBOR_COLS: i32 = 3;
const GRID_SCORE_COUNT: f64 = 1.0;
const GRID_SCORE_GRID: f64 = 1.0;
const GRID_SCORE_PIECE: f64 = 0.5;
const SOLVE_TIME_FACTOR: f64 = 4.1;
const SOLVE_TIME_EXPONENT: f64 = 1.3;

const FALLBACK_GRID: GridChoice = GridChoice {
    target_count: 80,
    cols: 10,
    rows: 8,
    actual_count: 80,
};

fn grid_choice_index(choices: &[GridChoice], cols: u32, rows: u32) -> Option<usize> {
    choices
        .iter()
        .position(|choice| choice.cols == cols && choice.rows == rows)
}

fn grid_choice_label(choice: &GridChoice) -> String {
    if choice.actual_count == choice.target_count {
        format!(
            "{} pieces ({}x{})",
            choice.target_count, choice.cols, choice.rows
        )
    } else {
        format!(
            "{} pieces ({}x{}, actual {})",
            choice.target_count, choice.cols, choice.rows, choice.actual_count
        )
    }
}

fn best_grid_for_count(width: u32, height: u32, target: u32) -> Option<GridChoice> {
    if target == 0 || width == 0 || height == 0 {
        return None;
    }
    let aspect = width as f64 / height as f64;
    let piece_ratio_max = GRID_PIECE_RATIO_MAX.max(1.0);
    let piece_ratio_min = 1.0 / piece_ratio_max;
    let base = (target as f64).sqrt().ceil() as u32;
    let r_hi = ((base as f64) * GRID_ROW_WIDEN).ceil() as u32;
    let r_hi = r_hi.max(GRID_ROW_MIN);
    let mut best: Option<(GridChoice, f64)> = None;
    for r in GRID_ROW_MIN..=r_hi {
        let c_star = target as f64 / r as f64;
        let c0 = c_star.round() as i32;
        for dc in -GRID_NEIGHBOR_COLS..=GRID_NEIGHBOR_COLS {
            let c = c0 + dc;
            if c < 2 {
                continue;
            }
            let actual = r as i32 * c;
            let actual_u = actual as u32;
            let rel_err = ((actual_u as f64) - (target as f64)).abs() / target as f64;
            if rel_err > GRID_REL_COUNT_TOL {
                continue;
            }
            let grid_ratio = c as f64 / r as f64;
            let piece_ratio = aspect / grid_ratio;
            if piece_ratio < piece_ratio_min || piece_ratio > piece_ratio_max {
                continue;
            }
            let eps = 1e-12;
            let count_term = rel_err.powi(2);
            let grid_term = ((grid_ratio + eps) / (aspect + eps)).ln().powi(2);
            let piece_term = (piece_ratio + eps).ln().powi(2);
            let score =
                GRID_SCORE_COUNT * count_term + GRID_SCORE_GRID * grid_term + GRID_SCORE_PIECE * piece_term;
            let choice = GridChoice {
                target_count: target,
                cols: c as u32,
                rows: r,
                actual_count: actual_u,
            };
            match &best {
                Some((_best_choice, best_score)) if score >= *best_score => {}
                _ => {
                    best = Some((choice, score));
                }
            }
        }
    }
    best.map(|(choice, _)| choice)
}

fn build_grid_choices(width: u32, height: u32) -> Vec<GridChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| best_grid_for_count(width, height, *target))
        .collect()
}

fn validate_saved_board(state: &SavedBoard, total: usize) -> bool {
    if state.positions.len() != total
        || state.rotations.len() != total
        || state.flips.len() != total
        || state.connections.len() != total
        || state.z_order.len() != total
    {
        return false;
    }
    if state.positions.iter().any(|(x, y)| !x.is_finite() || !y.is_finite()) {
        return false;
    }
    if state.rotations.iter().any(|rot| !rot.is_finite()) {
        return false;
    }
    let mut seen = vec![false; total];
    for id in &state.z_order {
        if *id >= total || seen[*id] {
            return false;
        }
        seen[*id] = true;
    }
    true
}

fn load_saved_board() -> Option<SavedBoard> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(STORAGE_KEY).ok()??;
    let state: SavedBoard = serde_json::from_str(&raw).ok()?;
    if state.version != STORAGE_VERSION {
        return None;
    }
    Some(state)
}

fn save_board_state(state: &SavedBoard) {
    let Ok(raw) = serde_json::to_string(state) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(STORAGE_KEY, &raw);
}

fn lerp(a: f64, b: f64, t: f64) -> f64 {
    a + (b - a) * t
}

fn sample_line_wave(lines: &[LineWave], t: f64) -> LineWave {
    let last = lines.len().saturating_sub(1) as f64;
    if last <= 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }
    let clamped = t.clamp(0.0, 1.0) * last;
    let idx = clamped.floor() as usize;
    let next = (idx + 1).min(lines.len().saturating_sub(1));
    let frac = clamped - idx as f64;
    let current = lines[idx];
    let following = lines[next];
    LineWave {
        amplitude: lerp(current.amplitude, following.amplitude, frac),
        skew: lerp(current.skew, following.skew, frac),
    }
}

fn warp_point(x: f64, y: f64, warp: &WarpField<'_>) -> (f64, f64) {
    let u = (x / warp.width).clamp(0.0, 1.0);
    let v = (y / warp.height).clamp(0.0, 1.0);
    let h_wave = sample_line_wave(warp.horizontal, v);
    let v_wave = sample_line_wave(warp.vertical, u);
    let u_skew = (u + h_wave.skew * u * (1.0 - u)).clamp(0.0, 1.0);
    let v_skew = (v + v_wave.skew * v * (1.0 - v)).clamp(0.0, 1.0);
    let dy = h_wave.amplitude * (std::f64::consts::PI * u_skew).sin();
    let dx = v_wave.amplitude * (std::f64::consts::PI * v_skew).sin();
    (x + dx, y + dy)
}

fn build_pieces(rows: u32, cols: u32) -> Vec<Piece> {
    let mut pieces = Vec::with_capacity((rows * cols) as usize);
    for row in 0..rows {
        for col in 0..cols {
            let id = (row * cols + col) as usize;
            pieces.push(Piece { id, row, col });
        }
    }
    pieces
}

fn fmt_f64(value: f64) -> String {
    format!("{:.3}", value)
}

fn format_time_unit(value: u64, unit: &str) -> String {
    if value == 1 {
        format!("~{} {}", value, unit)
    } else {
        format!("~{} {}s", value, unit)
    }
}

fn format_duration(seconds: f64) -> String {
    if !seconds.is_finite() || seconds <= 0.0 {
        return "~0 seconds".to_string();
    }
    if seconds < 90.0 {
        return format_time_unit(seconds.round().max(1.0) as u64, "second");
    }
    let minutes = seconds / 60.0;
    if minutes < 90.0 {
        return format_time_unit(minutes.round().max(1.0) as u64, "minute");
    }
    let hours = minutes / 60.0;
    if hours < 36.0 {
        return format_time_unit(hours.round().max(1.0) as u64, "hour");
    }
    let days = hours / 24.0;
    format_time_unit(days.round().max(1.0) as u64, "day")
}

fn jitter_value(
    seed: u64,
    salt: u64,
    base: f64,
    range: f64,
    min: f64,
    max: f64,
    variation: f64,
) -> f64 {
    let jitter = rand_unit(seed, salt) * 2.0 - 1.0;
    let value = base + jitter * range * variation;
    value.clamp(min, max)
}

fn event_to_svg_coords(
    event: &MouseEvent,
    svg_ref: &NodeRef,
    view_min_x: f64,
    view_min_y: f64,
    view_width: f64,
    view_height: f64,
) -> Option<(f64, f64)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    if rect.width() <= 0.0 || rect.height() <= 0.0 {
        return None;
    }
    let x = view_min_x + (event.client_x() as f64 - rect.left()) * view_width / rect.width();
    let y = view_min_y + (event.client_y() as f64 - rect.top()) * view_height / rect.height();
    Some((x, y))
}

fn touch_from_event(event: &TouchEvent, touch_id: Option<i32>, use_changed: bool) -> Option<Touch> {
    let list = if use_changed {
        event.changed_touches()
    } else {
        event.touches()
    };
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

fn touch_event_to_svg_coords(
    event: &TouchEvent,
    svg_ref: &NodeRef,
    view_min_x: f64,
    view_min_y: f64,
    view_width: f64,
    view_height: f64,
    touch_id: Option<i32>,
    use_changed: bool,
) -> Option<(f64, f64)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    if rect.width() <= 0.0 || rect.height() <= 0.0 {
        return None;
    }
    let touch = touch_from_event(event, touch_id, use_changed)?;
    let x = view_min_x + (touch.client_x() as f64 - rect.left()) * view_width / rect.width();
    let y = view_min_y + (touch.client_y() as f64 - rect.top()) * view_height / rect.height();
    Some((x, y))
}

fn on_setting_change<F>(settings: UseStateHandle<ShapeSettings>, updater: F) -> Callback<InputEvent>
where
    F: Fn(&mut ShapeSettings, f64) + 'static,
{
    Callback::from(move |event: InputEvent| {
        let input: HtmlInputElement = event.target_unchecked_into();
        if let Ok(value) = input.value().parse::<f64>() {
            let mut next = (*settings).clone();
            updater(&mut next, value);
            settings.set(next);
        }
    })
}

fn neighbor_id(id: usize, cols: usize, rows: usize, dir: usize) -> Option<usize> {
    let col = id % cols;
    let row = id / cols;
    match dir {
        DIR_UP if row > 0 => Some(id - cols),
        DIR_RIGHT if col + 1 < cols => Some(id + 1),
        DIR_DOWN if row + 1 < rows => Some(id + cols),
        DIR_LEFT if col > 0 => Some(id - 1),
        _ => None,
    }
}

fn opposite_dir(dir: usize) -> usize {
    match dir {
        DIR_UP => DIR_DOWN,
        DIR_RIGHT => DIR_LEFT,
        DIR_DOWN => DIR_UP,
        DIR_LEFT => DIR_RIGHT,
        _ => DIR_UP,
    }
}

fn set_connection(
    connections: &mut Vec<[bool; 4]>,
    id: usize,
    dir: usize,
    value: bool,
    cols: usize,
    rows: usize,
) {
    if let Some(neighbor) = neighbor_id(id, cols, rows, dir) {
        if let Some(edges) = connections.get_mut(id) {
            edges[dir] = value;
        }
        let opposite = opposite_dir(dir);
        if let Some(edges) = connections.get_mut(neighbor) {
            edges[opposite] = value;
        }
    }
}

fn clear_piece_connections(
    connections: &mut Vec<[bool; 4]>,
    id: usize,
    cols: usize,
    rows: usize,
) {
    for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
        set_connection(connections, id, dir, false, cols, rows);
    }
}

fn collect_group(connections: &[[bool; 4]], start: usize, cols: usize, rows: usize) -> Vec<usize> {
    let total = cols * rows;
    if start >= total {
        return Vec::new();
    }
    let mut visited = vec![false; total];
    let mut queue = VecDeque::new();
    let mut group = Vec::new();
    visited[start] = true;
    queue.push_back(start);

    while let Some(id) = queue.pop_front() {
        group.push(id);
        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
            if connections
                .get(id)
                .map(|edges| edges[dir])
                .unwrap_or(false)
            {
                if let Some(neighbor) = neighbor_id(id, cols, rows, dir) {
                    if !visited[neighbor] {
                        visited[neighbor] = true;
                        queue.push_back(neighbor);
                    }
                }
            }
        }
    }
    group
}

fn is_fully_connected(connections: &[[bool; 4]], cols: usize, rows: usize) -> bool {
    let total = cols * rows;
    if total == 0 || connections.len() != total {
        return false;
    }
    collect_group(connections, 0, cols, rows).len() == total
}

fn is_solved(
    positions: &[(f64, f64)],
    rotations: &[f64],
    flips: &[bool],
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
    piece_width: f64,
    piece_height: f64,
    rotation_enabled: bool,
) -> bool {
    let total = cols * rows;
    if positions.len() != total {
        return false;
    }
    if flips.len() != total {
        return false;
    }
    if flips.iter().any(|flip| *flip) {
        return false;
    }
    if is_fully_connected(connections, cols, rows) {
        return true;
    }
    if rotation_enabled {
        if rotations.len() != total {
            return false;
        }
        for rotation in rotations {
            if angle_delta(0.0, *rotation).abs() > ROTATION_SOLVE_TOLERANCE_DEG {
                return false;
            }
        }
    }
    let tolerance = piece_width.min(piece_height) * SOLVE_TOLERANCE_RATIO;
    let tolerance_sq = tolerance * tolerance;
    for row in 0..rows {
        for col in 0..cols {
            let id = row * cols + col;
            if let Some(pos) = positions.get(id) {
                let target_x = col as f64 * piece_width;
                let target_y = row as f64 * piece_height;
                let dx = pos.0 - target_x;
                let dy = pos.1 - target_y;
                if dx * dx + dy * dy > tolerance_sq {
                    return false;
                }
            } else {
                return false;
            }
        }
    }
    true
}

fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e3779b97f4a7c15);
    let mut z = value;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^ (z >> 31)
}

fn rand_unit(seed: u64, salt: u64) -> f64 {
    let mixed = splitmix64(seed ^ salt);
    let top = mixed >> 11;
    top as f64 / ((1u64 << 53) as f64)
}

fn rand_range(seed: u64, salt: u64, min: f64, max: f64) -> f64 {
    min + (max - min) * rand_unit(seed, salt)
}

fn rubber_band_distance(delta: f64, limit: f64) -> f64 {
    if limit <= 0.0 {
        return 0.0;
    }
    let abs = delta.abs();
    let sign = delta.signum();
    sign * (limit * abs / (limit + abs))
}

fn rubber_band_clamp(value: f64, min: f64, max: f64, limit: f64) -> f64 {
    if value < min {
        min + rubber_band_distance(value - min, limit)
    } else if value > max {
        max + rubber_band_distance(value - max, limit)
    } else {
        value
    }
}

fn normalize_angle(mut angle: f64) -> f64 {
    angle = angle % 360.0;
    if angle < 0.0 {
        angle += 360.0;
    }
    angle
}

fn angle_delta(target: f64, current: f64) -> f64 {
    let mut diff = normalize_angle(target - current);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

fn angle_matches(a: f64, b: f64, tolerance: f64) -> bool {
    angle_delta(a, b).abs() <= tolerance
}

fn next_snap_rotation(angle: f64) -> f64 {
    let next = (angle / ROTATION_STEP_DEG).floor() + 1.0;
    normalize_angle(next * ROTATION_STEP_DEG)
}

fn click_rotation_delta(
    current_angle: f64,
    noise: f64,
    noise_range: f64,
    snap_tolerance: f64,
) -> f64 {
    let mut target = next_snap_rotation(current_angle + noise);
    target = normalize_angle(target + noise);
    let min_step = if noise_range > 0.0 {
        noise_range.max(snap_tolerance)
    } else {
        0.0
    };
    if min_step > 0.0 && angle_delta(target, current_angle).abs() <= min_step {
        target = normalize_angle(target + ROTATION_STEP_DEG);
    }
    angle_delta(target, current_angle)
}

fn rotate_vec(x: f64, y: f64, angle_deg: f64) -> (f64, f64) {
    let radians = angle_deg.to_radians();
    let cos = radians.cos();
    let sin = radians.sin();
    (x * cos - y * sin, x * sin + y * cos)
}

fn rotate_point(x: f64, y: f64, origin_x: f64, origin_y: f64, angle_deg: f64) -> (f64, f64) {
    let (dx, dy) = (x - origin_x, y - origin_y);
    let (rx, ry) = rotate_vec(dx, dy, angle_deg);
    (origin_x + rx, origin_y + ry)
}

fn aligned_center_from_anchor(
    anchor_row: i32,
    anchor_col: i32,
    anchor_center: (f64, f64),
    member_id: usize,
    cols: usize,
    piece_width: f64,
    piece_height: f64,
    rotation: f64,
) -> (f64, f64) {
    let row = (member_id / cols) as i32;
    let col = (member_id % cols) as i32;
    let dx = (col - anchor_col) as f64 * piece_width;
    let dy = (row - anchor_row) as f64 * piece_height;
    let (rx, ry) = rotate_vec(dx, dy, rotation);
    (anchor_center.0 + rx, anchor_center.1 + ry)
}

fn align_group_to_anchor(
    positions: &mut Vec<(f64, f64)>,
    rotations: &mut Vec<f64>,
    members: &[usize],
    anchor_id: usize,
    anchor_center: (f64, f64),
    target_rot: f64,
    cols: usize,
    piece_width: f64,
    piece_height: f64,
) {
    if members.is_empty() || cols == 0 {
        return;
    }
    let target_rot = normalize_angle(target_rot);
    let anchor_row = (anchor_id / cols) as i32;
    let anchor_col = (anchor_id % cols) as i32;
    for id in members {
        let center = aligned_center_from_anchor(
            anchor_row,
            anchor_col,
            anchor_center,
            *id,
            cols,
            piece_width,
            piece_height,
            target_rot,
        );
        if let Some(pos) = positions.get_mut(*id) {
            *pos = (center.0 - piece_width * 0.5, center.1 - piece_height * 0.5);
        }
        if let Some(rot) = rotations.get_mut(*id) {
            *rot = target_rot;
        }
    }
}

fn connect_aligned_neighbors(
    members: &[usize],
    positions: &[(f64, f64)],
    rotations: &[f64],
    flips: &[bool],
    connections: &mut Vec<[bool; 4]>,
    cols: usize,
    rows: usize,
    piece_width: f64,
    piece_height: f64,
    snap_distance: f64,
    rotation_snap_tolerance: f64,
    rotation_enabled: bool,
) {
    let group_rot = members
        .first()
        .and_then(|id| rotations.get(*id))
        .copied()
        .unwrap_or(0.0);
    for member in members {
        if *member >= positions.len() {
            continue;
        }
        if flips.get(*member).copied().unwrap_or(false) {
            continue;
        }
        let current = positions[*member];
        let center_a = (
            current.0 + piece_width * 0.5,
            current.1 + piece_height * 0.5,
        );
        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
            if let Some(neighbor) = neighbor_id(*member, cols, rows, dir) {
                if flips.get(neighbor).copied().unwrap_or(false) {
                    continue;
                }
                let rot_b = rotations.get(neighbor).copied().unwrap_or(0.0);
                if rotation_enabled
                    && !angle_matches(group_rot, rot_b, rotation_snap_tolerance)
                {
                    continue;
                }
                let base = match dir {
                    DIR_LEFT => (-piece_width, 0.0),
                    DIR_RIGHT => (piece_width, 0.0),
                    DIR_UP => (0.0, -piece_height),
                    DIR_DOWN => (0.0, piece_height),
                    _ => (0.0, 0.0),
                };
                let expected_rot = if rotation_enabled { group_rot } else { 0.0 };
                let (vx, vy) = rotate_vec(base.0, base.1, expected_rot);
                let neighbor_pos = positions[neighbor];
                let center_b = (
                    neighbor_pos.0 + piece_width * 0.5,
                    neighbor_pos.1 + piece_height * 0.5,
                );
                let actual = (center_b.0 - center_a.0, center_b.1 - center_a.1);
                let dx = actual.0 - vx;
                let dy = actual.1 - vy;
                let dist = (dx * dx + dy * dy).sqrt();
                if dist <= snap_distance {
                    set_connection(connections, *member, dir, true, cols, rows);
                }
            }
        }
    }
}

fn frame_center_bounds_for_rotation(
    frame_width: f64,
    frame_height: f64,
    piece_width: f64,
    piece_height: f64,
    rotation: f64,
) -> (f64, f64, f64, f64) {
    let rotation = normalize_angle(rotation);
    let swap = ((rotation / ROTATION_STEP_DEG).round() as i32) % 2 != 0;
    let (rot_w, rot_h) = if swap {
        (piece_height, piece_width)
    } else {
        (piece_width, piece_height)
    };
    let min_x = rot_w * 0.5;
    let min_y = rot_h * 0.5;
    let mut max_x = frame_width - rot_w * 0.5;
    let mut max_y = frame_height - rot_h * 0.5;
    if max_x < min_x {
        max_x = min_x;
    }
    if max_y < min_y {
        max_y = min_y;
    }
    (min_x, max_x, min_y, max_y)
}

fn apply_snaps_for_group(
    members: &[usize],
    positions: &mut Vec<(f64, f64)>,
    rotations: &mut Vec<f64>,
    flips: &[bool],
    connections: &mut Vec<[bool; 4]>,
    cols: usize,
    rows: usize,
    piece_width: f64,
    piece_height: f64,
    snap_distance: f64,
    frame_snap_ratio: f64,
    center_min_x: f64,
    center_max_x: f64,
    center_min_y: f64,
    center_max_y: f64,
    view_min_x: f64,
    view_min_y: f64,
    view_width: f64,
    view_height: f64,
    rotation_snap_tolerance: f64,
    rotation_enabled: bool,
) -> Vec<usize> {
    let total = cols * rows;
    if members.is_empty() || total == 0 {
        return Vec::new();
    }
    let mut in_group = vec![false; total];
    for member in members {
        if *member < in_group.len() {
            in_group[*member] = true;
        }
    }

    #[derive(Clone, Copy)]
    struct SnapCandidate {
        member: usize,
        dir: usize,
        center_b: (f64, f64),
        rot_b: f64,
        base: (f64, f64),
        dist: f64,
    }

    let mut candidates = Vec::new();
    for member in members {
        if *member >= positions.len() {
            continue;
        }
        if flips.get(*member).copied().unwrap_or(false) {
            continue;
        }
        let current = positions[*member];
        let center_a = (
            current.0 + piece_width * 0.5,
            current.1 + piece_height * 0.5,
        );
        let rot_a = rotations.get(*member).copied().unwrap_or(0.0);
        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
            if let Some(neighbor) = neighbor_id(*member, cols, rows, dir) {
                if in_group[neighbor] {
                    continue;
                }
                if flips.get(neighbor).copied().unwrap_or(false) {
                    continue;
                }
                let rot_b = rotations.get(neighbor).copied().unwrap_or(0.0);
                if !angle_matches(rot_a, rot_b, rotation_snap_tolerance) {
                    continue;
                }
                let base = match dir {
                    DIR_LEFT => (-piece_width, 0.0),
                    DIR_RIGHT => (piece_width, 0.0),
                    DIR_UP => (0.0, -piece_height),
                    DIR_DOWN => (0.0, piece_height),
                    _ => (0.0, 0.0),
                };
                let expected_rot = if rotation_enabled { rot_b } else { rot_a };
                let (vx, vy) = rotate_vec(base.0, base.1, expected_rot);
                let neighbor_pos = positions[neighbor];
                let center_b = (
                    neighbor_pos.0 + piece_width * 0.5,
                    neighbor_pos.1 + piece_height * 0.5,
                );
                let actual = (center_b.0 - center_a.0, center_b.1 - center_a.1);
                let dx = actual.0 - vx;
                let dy = actual.1 - vy;
                let dist = (dx * dx + dy * dy).sqrt();
                if dist <= snap_distance {
                    candidates.push(SnapCandidate {
                        member: *member,
                        dir,
                        center_b,
                        rot_b,
                        base,
                        dist,
                    });
                }
            }
        }
    }

    let mut snap_anchor: Option<(usize, (f64, f64), f64)> = None;
    if !candidates.is_empty() {
        let mut best_index = None;
        let mut best_count = 0usize;
        let mut best_error = f64::INFINITY;
        let mut best_dist = f64::INFINITY;
        let mut best_anchor_id = 0usize;
        let mut best_anchor_center = (0.0, 0.0);
        let mut best_target_rot = 0.0;
        let mut best_candidate = None;

        for (idx, candidate) in candidates.iter().enumerate() {
            let target_rot = if rotation_enabled {
                normalize_angle(candidate.rot_b)
            } else {
                0.0
            };
            let anchor_id = candidate.member;
            let anchor_row = (anchor_id / cols) as i32;
            let anchor_col = (anchor_id % cols) as i32;
            let expected = rotate_vec(candidate.base.0, candidate.base.1, target_rot);
            let anchor_center = (
                candidate.center_b.0 - expected.0,
                candidate.center_b.1 - expected.1,
            );

            let mut min_cx = f64::INFINITY;
            let mut max_cx = f64::NEG_INFINITY;
            let mut min_cy = f64::INFINITY;
            let mut max_cy = f64::NEG_INFINITY;
            for id in members {
                let transformed = aligned_center_from_anchor(
                    anchor_row,
                    anchor_col,
                    anchor_center,
                    *id,
                    cols,
                    piece_width,
                    piece_height,
                    target_rot,
                );
                min_cx = min_cx.min(transformed.0);
                max_cx = max_cx.max(transformed.0);
                min_cy = min_cy.min(transformed.1);
                max_cy = max_cy.max(transformed.1);
            }
            let can_snap = min_cx.is_finite()
                && min_cy.is_finite()
                && min_cx >= center_min_x
                && max_cx <= center_max_x
                && min_cy >= center_min_y
                && max_cy <= center_max_y;
            if !can_snap {
                continue;
            }

            let mut count = 0usize;
            let mut error_sum = 0.0;
            for other in &candidates {
                if rotation_enabled
                    && !angle_matches(target_rot, other.rot_b, rotation_snap_tolerance)
                {
                    continue;
                }
                let new_center = aligned_center_from_anchor(
                    anchor_row,
                    anchor_col,
                    anchor_center,
                    other.member,
                    cols,
                    piece_width,
                    piece_height,
                    target_rot,
                );
                let expected = rotate_vec(other.base.0, other.base.1, target_rot);
                let actual = (
                    other.center_b.0 - new_center.0,
                    other.center_b.1 - new_center.1,
                );
                let dx = actual.0 - expected.0;
                let dy = actual.1 - expected.1;
                let dist = (dx * dx + dy * dy).sqrt();
                if dist <= snap_distance {
                    count += 1;
                    error_sum += dist;
                }
            }

            if count > best_count
                || (count == best_count
                    && (error_sum < best_error
                        || (error_sum == best_error && candidate.dist < best_dist)))
            {
                best_index = Some(idx);
                best_count = count;
                best_error = error_sum;
                best_dist = candidate.dist;
                best_anchor_id = anchor_id;
                best_anchor_center = anchor_center;
                best_target_rot = target_rot;
                best_candidate = Some(*candidate);
            }
        }

        if let Some(_best_idx) = best_index {
            align_group_to_anchor(
                positions,
                rotations,
                members,
                best_anchor_id,
                best_anchor_center,
                best_target_rot,
                cols,
                piece_width,
                piece_height,
            );

            if let Some(candidate) = best_candidate {
                set_connection(
                    connections,
                    candidate.member,
                    candidate.dir,
                    true,
                    cols,
                    rows,
                );
            }

            for candidate in &candidates {
                if rotation_enabled
                    && !angle_matches(best_target_rot, candidate.rot_b, rotation_snap_tolerance)
                {
                    continue;
                }
                let new_center = aligned_center_from_anchor(
                    (best_anchor_id / cols) as i32,
                    (best_anchor_id % cols) as i32,
                    best_anchor_center,
                    candidate.member,
                    cols,
                    piece_width,
                    piece_height,
                    best_target_rot,
                );
                let expected = rotate_vec(candidate.base.0, candidate.base.1, best_target_rot);
                let actual = (
                    candidate.center_b.0 - new_center.0,
                    candidate.center_b.1 - new_center.1,
                );
                let dx = actual.0 - expected.0;
                let dy = actual.1 - expected.1;
                let dist = (dx * dx + dy * dy).sqrt();
                if dist <= snap_distance {
                    set_connection(
                        connections,
                        candidate.member,
                        candidate.dir,
                        true,
                        cols,
                        rows,
                    );
                }
            }

            snap_anchor = Some((best_anchor_id, best_anchor_center, best_target_rot));
        }
    }

    let mut group_after = members
        .first()
        .map(|id| collect_group(connections, *id, cols, rows))
        .unwrap_or_default();
    if rotation_enabled {
        if let Some((anchor_id, anchor_center, target_rot)) = snap_anchor {
            if !group_after.is_empty() {
                align_group_to_anchor(
                    positions,
                    rotations,
                    &group_after,
                    anchor_id,
                    anchor_center,
                    target_rot,
                    cols,
                    piece_width,
                    piece_height,
                );
            }
        }
    }
    if !group_after.is_empty() {
        let frame_width = cols as f64 * piece_width;
        let frame_height = rows as f64 * piece_height;
        let corner_snap_distance = snap_distance * frame_snap_ratio;
        let group_rot = group_after
            .first()
            .and_then(|id| rotations.get(*id))
            .copied()
            .unwrap_or(0.0);
        let mut corner_snapped = false;
        if rotation_enabled && group_after.len() < total && corner_snap_distance > 0.0 {
            let mut best_corner = None;
            let target_center_for = |corner: usize, rotation: f64| {
                let rotation = normalize_angle(rotation);
                let swap = ((rotation / ROTATION_STEP_DEG).round() as i32) % 2 != 0;
                let (offset_x, offset_y) = if swap {
                    (piece_height * 0.5, piece_width * 0.5)
                } else {
                    (piece_width * 0.5, piece_height * 0.5)
                };
                match corner {
                    0 => (offset_x, offset_y),
                    1 => (frame_width - offset_x, offset_y),
                    2 => (frame_width - offset_x, frame_height - offset_y),
                    3 => (offset_x, frame_height - offset_y),
                    _ => (offset_x, offset_y),
                }
            };
            for id in &group_after {
                if *id >= positions.len() {
                    continue;
                }
                if flips.get(*id).copied().unwrap_or(false) {
                    continue;
                }
                let row = *id / cols;
                let col = *id % cols;
                let piece_corner = if row == 0 && col == 0 {
                    Some(0usize)
                } else if row == 0 && col + 1 == cols {
                    Some(1usize)
                } else if row + 1 == rows && col + 1 == cols {
                    Some(2usize)
                } else if row + 1 == rows && col == 0 {
                    Some(3usize)
                } else {
                    None
                };
                let piece_corner = match piece_corner {
                    Some(value) => value,
                    None => continue,
                };
                let current = positions[*id];
                let current_center = (
                    current.0 + piece_width * 0.5,
                    current.1 + piece_height * 0.5,
                );
                for target_corner in 0..4usize {
                    let steps = (target_corner + 4 - piece_corner) % 4;
                    let target_rot = normalize_angle(steps as f64 * ROTATION_STEP_DEG);
                    if rotation_enabled
                        && !angle_matches(group_rot, target_rot, rotation_snap_tolerance)
                    {
                        continue;
                    }
                    let target_center = target_center_for(target_corner, target_rot);
                    let dx = current_center.0 - target_center.0;
                    let dy = current_center.1 - target_center.1;
                    let dist = (dx * dx + dy * dy).sqrt();
                    if dist > corner_snap_distance {
                        continue;
                    }
                    let anchor_row = (*id / cols) as i32;
                    let anchor_col = (*id % cols) as i32;
                    let (frame_min_x, frame_max_x, frame_min_y, frame_max_y) =
                        frame_center_bounds_for_rotation(
                            frame_width,
                            frame_height,
                            piece_width,
                            piece_height,
                            target_rot,
                        );
                    let mut min_cx = f64::INFINITY;
                    let mut max_cx = f64::NEG_INFINITY;
                    let mut min_cy = f64::INFINITY;
                    let mut max_cy = f64::NEG_INFINITY;
                    for member in &group_after {
                        let center = aligned_center_from_anchor(
                            anchor_row,
                            anchor_col,
                            target_center,
                            *member,
                            cols,
                            piece_width,
                            piece_height,
                            target_rot,
                        );
                        min_cx = min_cx.min(center.0);
                        max_cx = max_cx.max(center.0);
                        min_cy = min_cy.min(center.1);
                        max_cy = max_cy.max(center.1);
                    }
                    let can_snap = min_cx.is_finite()
                        && min_cy.is_finite()
                        && min_cx >= frame_min_x
                        && max_cx <= frame_max_x
                        && min_cy >= frame_min_y
                        && max_cy <= frame_max_y;
                    if !can_snap {
                        continue;
                    }
                    let should_replace = match best_corner {
                        None => true,
                        Some((_best_id, _best_center, _best_rot, best_dist)) => dist < best_dist,
                    };
                    if should_replace {
                        best_corner = Some((*id, target_center, target_rot, dist));
                    }
                }
            }
            if let Some((anchor_id, anchor_center, target_rot, _)) = best_corner {
                align_group_to_anchor(
                    positions,
                    rotations,
                    &group_after,
                    anchor_id,
                    anchor_center,
                    target_rot,
                    cols,
                    piece_width,
                    piece_height,
                );
                corner_snapped = true;
            }
        }
        if rotation_enabled
            && !corner_snapped
            && group_after.len() < total
            && corner_snap_distance > 0.0
        {
            let mut best_edge = None;
            for id in &group_after {
                if *id >= positions.len() {
                    continue;
                }
                if flips.get(*id).copied().unwrap_or(false) {
                    continue;
                }
                let row = *id / cols;
                let col = *id % cols;
                let is_corner = (row == 0 || row + 1 == rows) && (col == 0 || col + 1 == cols);
                if is_corner {
                    continue;
                }
                let edge_side = if row == 0 {
                    Some(DIR_UP)
                } else if row + 1 == rows {
                    Some(DIR_DOWN)
                } else if col == 0 {
                    Some(DIR_LEFT)
                } else if col + 1 == cols {
                    Some(DIR_RIGHT)
                } else {
                    None
                };
                let edge_side = match edge_side {
                    Some(value) => value,
                    None => continue,
                };
                let current = positions[*id];
                let current_center = (
                    current.0 + piece_width * 0.5,
                    current.1 + piece_height * 0.5,
                );
                for target_edge in 0..4usize {
                    let steps = (target_edge + 4 - edge_side) % 4;
                    let target_rot = normalize_angle(steps as f64 * ROTATION_STEP_DEG);
                    if rotation_enabled
                        && !angle_matches(group_rot, target_rot, rotation_snap_tolerance)
                    {
                        continue;
                    }
                    let swap = ((target_rot / ROTATION_STEP_DEG).round() as i32) % 2 != 0;
                    let (rot_w, rot_h) = if swap {
                        (piece_height, piece_width)
                    } else {
                        (piece_width, piece_height)
                    };
                    let target_center = match target_edge {
                        DIR_UP => (current_center.0, rot_h * 0.5),
                        DIR_RIGHT => (frame_width - rot_w * 0.5, current_center.1),
                        DIR_DOWN => (current_center.0, frame_height - rot_h * 0.5),
                        DIR_LEFT => (rot_w * 0.5, current_center.1),
                        _ => (current_center.0, current_center.1),
                    };
                    let dist = if target_edge == DIR_UP || target_edge == DIR_DOWN {
                        (current_center.1 - target_center.1).abs()
                    } else {
                        (current_center.0 - target_center.0).abs()
                    };
                    if dist > corner_snap_distance {
                        continue;
                    }
                    let anchor_row = (*id / cols) as i32;
                    let anchor_col = (*id % cols) as i32;
                    let (frame_min_x, frame_max_x, frame_min_y, frame_max_y) =
                        frame_center_bounds_for_rotation(
                            frame_width,
                            frame_height,
                            piece_width,
                            piece_height,
                            target_rot,
                        );
                    let mut min_cx = f64::INFINITY;
                    let mut max_cx = f64::NEG_INFINITY;
                    let mut min_cy = f64::INFINITY;
                    let mut max_cy = f64::NEG_INFINITY;
                    for member in &group_after {
                        let center = aligned_center_from_anchor(
                            anchor_row,
                            anchor_col,
                            target_center,
                            *member,
                            cols,
                            piece_width,
                            piece_height,
                            target_rot,
                        );
                        min_cx = min_cx.min(center.0);
                        max_cx = max_cx.max(center.0);
                        min_cy = min_cy.min(center.1);
                        max_cy = max_cy.max(center.1);
                    }
                    let can_snap = min_cx.is_finite()
                        && min_cy.is_finite()
                        && min_cx >= frame_min_x
                        && max_cx <= frame_max_x
                        && min_cy >= frame_min_y
                        && max_cy <= frame_max_y;
                    if !can_snap {
                        continue;
                    }
                    let should_replace = match best_edge {
                        None => true,
                        Some((_best_id, _best_center, _best_rot, best_dist)) => dist < best_dist,
                    };
                    if should_replace {
                        best_edge = Some((*id, target_center, target_rot, dist));
                    }
                }
            }
            if let Some((anchor_id, anchor_center, target_rot, _)) = best_edge {
                align_group_to_anchor(
                    positions,
                    rotations,
                    &group_after,
                    anchor_id,
                    anchor_center,
                    target_rot,
                    cols,
                    piece_width,
                    piece_height,
                );
            }
        }

        if group_after.len() == total {
            let stage_center_x = view_min_x + view_width * 0.5;
            let stage_center_y = view_min_y + view_height * 0.5;
            let target_center = (
                stage_center_x - frame_width * 0.5 + piece_width * 0.5,
                stage_center_y - frame_height * 0.5 + piece_height * 0.5,
            );
            align_group_to_anchor(
                positions,
                rotations,
                &group_after,
                0,
                target_center,
                0.0,
                cols,
                piece_width,
                piece_height,
            );
        } else {
            let mut in_group = vec![false; total];
            for id in &group_after {
                if *id < in_group.len() {
                    in_group[*id] = true;
                }
            }
            let mut has_borders = true;
            'border_check: for row in 0..rows {
                for col in 0..cols {
                    if row == 0 || row + 1 == rows || col == 0 || col + 1 == cols {
                        let id = row * cols + col;
                        if !in_group[id] {
                            has_borders = false;
                            break 'border_check;
                        }
                    }
                }
            }
            if has_borders {
                let frame_snap_distance = snap_distance * frame_snap_ratio;
                let anchor_id = 0usize;
                if let Some(anchor_pos) = positions.get(anchor_id) {
                    let current_center = (
                        anchor_pos.0 + piece_width * 0.5,
                        anchor_pos.1 + piece_height * 0.5,
                    );
                    let target_center = (piece_width * 0.5, piece_height * 0.5);
                    let dx = current_center.0 - target_center.0;
                    let dy = current_center.1 - target_center.1;
                    let dist = (dx * dx + dy * dy).sqrt();
                    let rotation_ok =
                        !rotation_enabled || angle_matches(group_rot, 0.0, rotation_snap_tolerance);
                    if dist <= frame_snap_distance && rotation_ok {
                        align_group_to_anchor(
                            positions,
                            rotations,
                            &group_after,
                            anchor_id,
                            target_center,
                            0.0,
                            cols,
                            piece_width,
                            piece_height,
                        );
                    }
                }
            }
        }
    }

    let clamp_ids: &[usize] = if group_after.is_empty() {
        members
    } else {
        &group_after
    };
    if !clamp_ids.is_empty() {
        let mut min_cx = f64::INFINITY;
        let mut max_cx = f64::NEG_INFINITY;
        let mut min_cy = f64::INFINITY;
        let mut max_cy = f64::NEG_INFINITY;
        for id in clamp_ids {
            if let Some(pos) = positions.get(*id) {
                let center_x = pos.0 + piece_width * 0.5;
                let center_y = pos.1 + piece_height * 0.5;
                min_cx = min_cx.min(center_x);
                max_cx = max_cx.max(center_x);
                min_cy = min_cy.min(center_y);
                max_cy = max_cy.max(center_y);
            }
        }
        if min_cx.is_finite() && min_cy.is_finite() {
            let mut shift_x = 0.0;
            let mut shift_y = 0.0;
            if min_cx < center_min_x {
                shift_x = center_min_x - min_cx;
            } else if max_cx > center_max_x {
                shift_x = center_max_x - max_cx;
            }
            if min_cy < center_min_y {
                shift_y = center_min_y - min_cy;
            } else if max_cy > center_max_y {
                shift_y = center_max_y - max_cy;
            }
            if shift_x != 0.0 || shift_y != 0.0 {
                for id in clamp_ids {
                    if let Some(pos) = positions.get_mut(*id) {
                        *pos = (pos.0 + shift_x, pos.1 + shift_y);
                    }
                }
            }
        }
    }

    if !clamp_ids.is_empty() {
        connect_aligned_neighbors(
            clamp_ids,
            positions,
            rotations,
            flips,
            connections,
            cols,
            rows,
            piece_width,
            piece_height,
            snap_distance,
            rotation_snap_tolerance,
            rotation_enabled,
        );
        if let Some(anchor_id) = clamp_ids.first() {
            group_after = collect_group(connections, *anchor_id, cols, rows);
        }
    }

    group_after
}

fn time_nonce(previous: u64) -> u64 {
    let now = Date::now() as u64;
    splitmix64(now ^ previous.wrapping_add(0x9e3779b97f4a7c15))
}

fn build_full_connections(cols: usize, rows: usize) -> Vec<[bool; 4]> {
    let total = cols * rows;
    let mut connections = vec![[false; 4]; total];
    for row in 0..rows {
        for col in 0..cols {
            let id = row * cols + col;
            if col + 1 < cols {
                set_connection(&mut connections, id, DIR_RIGHT, true, cols, rows);
            }
            if row + 1 < rows {
                set_connection(&mut connections, id, DIR_DOWN, true, cols, rows);
            }
        }
    }
    connections
}

fn scramble_seed(base: u64, nonce: u64, cols: usize, rows: usize) -> u64 {
    base ^ nonce.wrapping_mul(0x9e3779b97f4a7c15) ^ ((cols as u64) << 32) ^ rows as u64 ^ 0x5CA7_7EED
}

fn scramble_layout(
    seed: u64,
    cols: usize,
    rows: usize,
    piece_width: f64,
    piece_height: f64,
    view_min_x: f64,
    view_min_y: f64,
    view_width: f64,
    view_height: f64,
    margin: f64,
) -> (Vec<(f64, f64)>, Vec<usize>) {
    let total = cols * rows;
    let min_x = view_min_x + margin;
    let mut max_x = view_min_x + view_width - piece_width - margin;
    let min_y = view_min_y + margin;
    let mut max_y = view_min_y + view_height - piece_height - margin;
    if max_x < min_x {
        max_x = min_x;
    }
    if max_y < min_y {
        max_y = min_y;
    }

    let mut positions = Vec::with_capacity(total);
    for id in 0..total {
        let salt = (id as u64) << 1;
        let x = rand_range(seed, salt, min_x, max_x);
        let y = rand_range(seed, salt + 1, min_y, max_y);
        positions.push((x, y));
    }

    let mut order: Vec<usize> = (0..total).collect();
    for i in (1..order.len()).rev() {
        let salt = 0xC0DE_u64 + i as u64;
        let j = (rand_unit(seed, salt) * (i as f64 + 1.0)) as usize;
        order.swap(i, j);
    }
    (positions, order)
}

fn scramble_rotations(seed: u64, total: usize, enabled: bool) -> Vec<f64> {
    if !enabled {
        return vec![0.0; total];
    }
    let mut rotations = Vec::with_capacity(total);
    for id in 0..total {
        let salt = 0xC001_u64 + id as u64;
        rotations.push(rand_range(seed, salt, 0.0, 360.0));
    }
    rotations
}

fn scramble_flips(seed: u64, total: usize, chance: f64) -> Vec<bool> {
    let threshold = chance.clamp(0.0, 1.0);
    let mut flips = Vec::with_capacity(total);
    for id in 0..total {
        let salt = 0xF11F_5EED_u64 + id as u64;
        flips.push(rand_unit(seed, salt) < threshold);
    }
    flips
}

fn edge_seed(base: u64, orientation: u64, row: u32, col: u32) -> u64 {
    base ^ (orientation << 56) ^ ((row as u64) << 28) ^ (col as u64)
}

fn edge_from_seed(seed: u64, settings: &ShapeSettings) -> Edge {
    let variation = settings
        .variation
        .clamp(VARIATION_MIN, VARIATION_MAX);
    let tab_size_raw = jitter_value(
        seed,
        0,
        settings.tab_width,
        TAB_WIDTH_RANGE,
        TAB_WIDTH_MIN,
        TAB_WIDTH_MAX,
        variation,
    );
    let tab_depth_raw = jitter_value(
        seed,
        2,
        settings.tab_depth,
        TAB_DEPTH_RANGE,
        TAB_DEPTH_MIN,
        TAB_DEPTH_MAX,
        variation,
    );
    let tab_size_scale = settings
        .tab_size_scale
        .clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
    let tab_size_min = settings
        .tab_size_min
        .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
    let tab_size_max = settings
        .tab_size_max
        .clamp(tab_size_min, TAB_SIZE_MAX_LIMIT);
    let tab_size = (tab_size_raw * tab_size_scale).clamp(tab_size_min, tab_size_max);
    let tab_depth = tab_depth_raw.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
    let jitter_strength = settings
        .jitter_strength
        .clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
    let jitter_base = (variation * jitter_strength).clamp(0.0, jitter_strength);
    let skew_ratio = (settings.skew_range / SKEW_RANGE_MAX).clamp(0.0, 1.0);
    let jitter_len_bias = settings
        .jitter_len_bias
        .clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);
    let jitter_len = jitter_base * (jitter_len_bias + (1.0 - jitter_len_bias) * skew_ratio);
    let jitter_depth = jitter_base * tab_depth;
    let a = rand_range(seed, 3, -jitter_depth, jitter_depth);
    let b = rand_range(seed, 4, -jitter_len, jitter_len);
    let c = rand_range(seed, 5, -jitter_depth, jitter_depth);
    let d = rand_range(seed, 6, -jitter_len, jitter_len);
    let e = rand_range(seed, 7, -jitter_depth, jitter_depth);
    let tab_side = if rand_unit(seed, 8) < 0.5 {
        TabSide::Tab
    } else {
        TabSide::Blank
    };

    Edge {
        tab_side,
        params: EdgeParams {
            tab_size,
            tab_depth,
            a,
            b,
            c,
            d,
            e,
        },
    }
}

fn build_edge_maps(
    rows: u32,
    cols: u32,
    seed: u64,
    settings: &ShapeSettings,
) -> (Vec<Vec<Option<Edge>>>, Vec<Vec<Option<Edge>>>) {
    let mut horizontal = vec![vec![None; cols as usize]; (rows + 1) as usize];
    for row in 1..rows {
        for col in 0..cols {
            let seed = edge_seed(seed, 0, row, col);
            horizontal[row as usize][col as usize] = Some(edge_from_seed(seed, settings));
        }
    }

    let mut vertical = vec![vec![None; (cols + 1) as usize]; rows as usize];
    for row in 0..rows {
        for col in 1..cols {
            let seed = edge_seed(seed, 1, row, col);
            vertical[row as usize][col as usize] = Some(edge_from_seed(seed, settings));
        }
    }

    (horizontal, vertical)
}

fn line_wave(seed: u64, axis: u64, index: u32, max_amp: f64) -> LineWave {
    if max_amp == 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }
    let salt = (axis << 32) | index as u64;
    let amplitude = rand_range(seed ^ 0xB1EB_01DE, salt, -max_amp, max_amp);
    let skew = rand_range(seed ^ 0xA11C_E0DE, salt, -0.6, 0.6);
    LineWave { amplitude, skew }
}

fn build_line_waves(
    rows: u32,
    cols: u32,
    seed: u64,
    piece_width: f64,
    piece_height: f64,
    line_bend_ratio: f64,
) -> (Vec<LineWave>, Vec<LineWave>) {
    let bend_ratio = line_bend_ratio.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
    let max_h = piece_height * bend_ratio;
    let max_v = piece_width * bend_ratio;

    let mut horizontal = Vec::with_capacity((rows + 1) as usize);
    for row in 0..=rows {
        let wave = if row == 0 || row == rows {
            LineWave {
                amplitude: 0.0,
                skew: 0.0,
            }
        } else {
            line_wave(seed, 2, row, max_h)
        };
        horizontal.push(wave);
    }

    let mut vertical = Vec::with_capacity((cols + 1) as usize);
    for col in 0..=cols {
        let wave = if col == 0 || col == cols {
            LineWave {
                amplitude: 0.0,
                skew: 0.0,
            }
        } else {
            line_wave(seed, 3, col, max_v)
        };
        vertical.push(wave);
    }

    (horizontal, vertical)
}

fn cubic_point(
    p0: (f64, f64),
    p1: (f64, f64),
    p2: (f64, f64),
    p3: (f64, f64),
    t: f64,
) -> (f64, f64) {
    let u = 1.0 - t;
    let tt = t * t;
    let uu = u * u;
    let uuu = uu * u;
    let ttt = tt * t;
    (
        uuu * p0.0 + 3.0 * uu * t * p1.0 + 3.0 * u * tt * p2.0 + ttt * p3.0,
        uuu * p0.1 + 3.0 * uu * t * p1.1 + 3.0 * u * tt * p2.1 + ttt * p3.1,
    )
}

fn cubic_segments(
    p0: (f64, f64),
    p1: (f64, f64),
    p2: (f64, f64),
    p3: (f64, f64),
    steps: usize,
) -> Vec<Segment> {
    let mut segments = Vec::with_capacity(steps.max(1));
    let steps = steps.max(1);
    for step in 1..=steps {
        let t = step as f64 / steps as f64;
        let (x, y) = cubic_point(p0, p1, p2, p3, t);
        segments.push(Segment::LineTo { x, y });
    }
    segments
}

fn edge_segments(
    len: f64,
    depth_base: f64,
    edge: Option<&Edge>,
    tab_sign: i8,
    depth_limit: f64,
    curve_detail: f64,
) -> Vec<Segment> {
    let edge = match edge {
        Some(edge) if tab_sign != 0 => edge,
        _ => return vec![Segment::LineTo { x: len, y: 0.0 }],
    };

    let params = edge.params;
    let depth_limit = depth_limit.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    let t_len = params.tab_size;
    let mut t_depth = t_len * params.tab_depth;
    let max_t_depth = depth_limit / 3.0;
    if t_depth > max_t_depth {
        t_depth = max_t_depth;
    }
    let max_jitter_depth = (depth_limit - 3.0 * t_depth).max(0.0);
    let mut a = params.a.clamp(-depth_limit, depth_limit);
    let mut c = params.c.clamp(-max_jitter_depth, max_jitter_depth);
    let mut e = params.e.clamp(-depth_limit, depth_limit);
    let b = params.b;
    let d = params.d;
    if max_jitter_depth == 0.0 {
        a = 0.0;
        c = 0.0;
        e = 0.0;
    }

    let sign = tab_sign as f64;
    let l = |v: f64| len * v;
    let w = |v: f64| depth_base * v * sign;

    let p0 = (l(0.0), w(0.0));
    let p1 = (l(0.2), w(a));
    let p2 = (l(0.5 + b + d), w(-t_depth + c));
    let p3 = (l(0.5 - t_len + b), w(t_depth + c));
    let p4 = (l(0.5 - 2.0 * t_len + b - d), w(3.0 * t_depth + c));
    let p5 = (l(0.5 + 2.0 * t_len + b - d), w(3.0 * t_depth + c));
    let p6 = (l(0.5 + t_len + b), w(t_depth + c));
    let p7 = (l(0.5 + b + d), w(-t_depth + c));
    let p8 = (l(0.8), w(e));
    let p9 = (l(1.0), w(0.0));

    let base_steps = (len / EDGE_STEP_MIN).ceil() as usize;
    let detail = curve_detail.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    let steps = ((base_steps as f64) * detail).round() as usize;
    let steps = steps.max(8);
    let mut segments = Vec::with_capacity(steps * 3);
    segments.extend(cubic_segments(p0, p1, p2, p3, steps));
    segments.extend(cubic_segments(p3, p4, p5, p6, steps));
    segments.extend(cubic_segments(p6, p7, p8, p9, steps));
    segments
}

fn reverse_segments(segments: &[Segment]) -> Vec<Segment> {
    let mut states = Vec::with_capacity(segments.len());
    let mut current = (0.0, 0.0);
    for segment in segments {
        let end = match *segment {
            Segment::LineTo { x, y } => (x, y),
        };
        states.push((current, *segment, end));
        current = end;
    }

    let mut reversed = Vec::with_capacity(segments.len());
    for (start, segment, _end) in states.into_iter().rev() {
        match segment {
            Segment::LineTo { .. } => reversed.push(Segment::LineTo {
                x: start.0,
                y: start.1,
            }),
        }
    }
    reversed
}

fn map_point(
    orientation: EdgeOrientation,
    origin: (f64, f64),
    offset: (f64, f64),
    warp: &WarpField<'_>,
    x: f64,
    y: f64,
) -> (f64, f64) {
    let (ox, oy) = origin;
    let (dx, dy) = offset;
    let (gx, gy) = match orientation {
        EdgeOrientation::Top => (ox + x, oy - y),
        EdgeOrientation::Right => (ox + y, oy + x),
        EdgeOrientation::Bottom => (ox + x, oy + y),
        EdgeOrientation::Left => (ox - y, oy + x),
    };
    let (wx, wy) = warp_point(gx + dx, gy + dy, warp);
    (wx - dx, wy - dy)
}

fn map_local_point(
    offset: (f64, f64),
    warp: &WarpField<'_>,
    x: f64,
    y: f64,
) -> (f64, f64) {
    let (wx, wy) = warp_point(offset.0 + x, offset.1 + y, warp);
    (wx - offset.0, wy - offset.1)
}

fn append_local_points(
    path: &mut String,
    offset: (f64, f64),
    warp: &WarpField<'_>,
    points: &[(f64, f64)],
) {
    for &(x, y) in points {
        let (gx, gy) = map_local_point(offset, warp, x, y);
        let _ = write!(path, " L {} {}", fmt_f64(gx), fmt_f64(gy));
    }
}

fn build_local_path(
    offset: (f64, f64),
    warp: &WarpField<'_>,
    points: &[(f64, f64)],
) -> String {
    if points.is_empty() {
        return String::new();
    }
    let (sx, sy) = map_local_point(offset, warp, points[0].0, points[0].1);
    let mut path = String::new();
    let _ = write!(path, "M {} {}", fmt_f64(sx), fmt_f64(sy));
    append_local_points(&mut path, offset, warp, &points[1..]);
    path
}

fn corner_arc_points(
    cx: f64,
    cy: f64,
    radius: f64,
    start_angle: f64,
    end_angle: f64,
    steps: usize,
) -> Vec<(f64, f64)> {
    let steps = steps.max(1);
    let mut end = end_angle;
    if end < start_angle {
        end += 2.0 * std::f64::consts::PI;
    }
    let span = end - start_angle;
    let mut points = Vec::with_capacity(steps + 1);
    for step in 0..=steps {
        let t = step as f64 / steps as f64;
        let angle = start_angle + span * t;
        points.push((cx + radius * angle.cos(), cy + radius * angle.sin()));
    }
    points
}

fn append_segments(
    path: &mut String,
    segments: &[Segment],
    orientation: EdgeOrientation,
    origin: (f64, f64),
    offset: (f64, f64),
    warp: &WarpField<'_>,
    start: (f64, f64),
    max_segment_len: f64,
) {
    let mut current = start;
    let max_len = if max_segment_len > 0.0 {
        max_segment_len
    } else {
        f64::INFINITY
    };
    for segment in segments {
        match *segment {
            Segment::LineTo { x, y } => {
                let dx = x - current.0;
                let dy = y - current.1;
                let dist = (dx * dx + dy * dy).sqrt();
                let steps = if dist <= max_len {
                    1
                } else {
                    (dist / max_len).ceil() as usize
                }
                .max(1);
                for step in 1..=steps {
                    let t = step as f64 / steps as f64;
                    let px = current.0 + dx * t;
                    let py = current.1 + dy * t;
                    let (gx, gy) = map_point(orientation, origin, offset, warp, px, py);
                    let _ = write!(path, " L {} {}", fmt_f64(gx), fmt_f64(gy));
                }
                current = (x, y);
            }
        }
    }
}

struct PiecePaths {
    outline: String,
    edges: [String; 4],
}

fn build_edge_path(
    segments: &[Segment],
    orientation: EdgeOrientation,
    origin: (f64, f64),
    offset: (f64, f64),
    warp: &WarpField<'_>,
    start: (f64, f64),
    max_segment_len: f64,
) -> String {
    let (sx, sy) = map_point(orientation, origin, offset, warp, start.0, start.1);
    let mut path = String::new();
    let _ = write!(path, "M {} {}", fmt_f64(sx), fmt_f64(sy));
    append_segments(
        &mut path,
        segments,
        orientation,
        origin,
        offset,
        warp,
        start,
        max_segment_len,
    );
    path
}

fn build_piece_path(
    piece: &Piece,
    piece_width: f64,
    piece_height: f64,
    horizontal: &[Vec<Option<Edge>>],
    vertical: &[Vec<Option<Edge>>],
    warp: &WarpField<'_>,
    tab_depth_cap: f64,
    curve_detail: f64,
) -> PiecePaths {
    let row = piece.row as usize;
    let col = piece.col as usize;
    let piece_x = piece.col as f64 * piece_width;
    let piece_y = piece.row as f64 * piece_height;

    let top_edge = horizontal[row][col].as_ref();
    let bottom_edge = horizontal[row + 1][col].as_ref();
    let left_edge = vertical[row][col].as_ref();
    let right_edge = vertical[row][col + 1].as_ref();

    let top_sign = top_edge.map(|edge| -edge.tab_side.sign()).unwrap_or(0);
    let right_sign = right_edge.map(|edge| edge.tab_side.sign()).unwrap_or(0);
    let bottom_sign = bottom_edge.map(|edge| edge.tab_side.sign()).unwrap_or(0);
    let left_sign = left_edge.map(|edge| -edge.tab_side.sign()).unwrap_or(0);

    let rows = horizontal.len().saturating_sub(1);
    let cols = horizontal.first().map(|row| row.len()).unwrap_or(0);
    let is_top = row == 0;
    let is_left = col == 0;
    let is_bottom = row + 1 == rows;
    let is_right = col + 1 == cols;
    let mut corner_radius = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
    let max_corner = piece_width.min(piece_height) * 0.45;
    if corner_radius > max_corner {
        corner_radius = max_corner;
    }
    let round_tl = is_top && is_left && corner_radius > 0.0;
    let round_tr = is_top && is_right && corner_radius > 0.0;
    let round_br = is_bottom && is_right && corner_radius > 0.0;
    let round_bl = is_bottom && is_left && corner_radius > 0.0;

    let top_start_trim = if round_tl { corner_radius } else { 0.0 };
    let top_end_trim = if round_tr { corner_radius } else { 0.0 };
    let right_start_trim = if round_tr { corner_radius } else { 0.0 };
    let right_end_trim = if round_br { corner_radius } else { 0.0 };
    let bottom_start_trim = if round_br { corner_radius } else { 0.0 };
    let bottom_end_trim = if round_bl { corner_radius } else { 0.0 };
    let left_start_trim = if round_bl { corner_radius } else { 0.0 };
    let left_end_trim = if round_tl { corner_radius } else { 0.0 };

    let top_is_boundary = top_edge.is_none();
    let right_is_boundary = right_edge.is_none();
    let bottom_is_boundary = bottom_edge.is_none();
    let left_is_boundary = left_edge.is_none();

    let top_start = if top_is_boundary {
        (top_start_trim, 0.0)
    } else {
        (0.0, 0.0)
    };
    let right_start = if right_is_boundary {
        (right_start_trim, 0.0)
    } else {
        (0.0, 0.0)
    };
    let bottom_start = if bottom_is_boundary {
        (piece_width - bottom_start_trim, 0.0)
    } else {
        (piece_width, 0.0)
    };
    let left_start = if left_is_boundary {
        (piece_height - left_start_trim, 0.0)
    } else {
        (piece_height, 0.0)
    };

    let top_segments = if top_is_boundary {
        vec![Segment::LineTo {
            x: piece_width - top_end_trim,
            y: 0.0,
        }]
    } else {
        edge_segments(
            piece_width,
            piece_height,
            top_edge,
            top_sign,
            tab_depth_cap,
            curve_detail,
        )
    };
    let right_segments = if right_is_boundary {
        vec![Segment::LineTo {
            x: piece_height - right_end_trim,
            y: 0.0,
        }]
    } else {
        edge_segments(
            piece_height,
            piece_width,
            right_edge,
            right_sign,
            tab_depth_cap,
            curve_detail,
        )
    };
    let bottom_segments = if bottom_is_boundary {
        vec![Segment::LineTo {
            x: bottom_end_trim,
            y: 0.0,
        }]
    } else {
        reverse_segments(&edge_segments(
            piece_width,
            piece_height,
            bottom_edge,
            bottom_sign,
            tab_depth_cap,
            curve_detail,
        ))
    };
    let left_segments = if left_is_boundary {
        vec![Segment::LineTo {
            x: left_end_trim,
            y: 0.0,
        }]
    } else {
        reverse_segments(&edge_segments(
            piece_height,
            piece_width,
            left_edge,
            left_sign,
            tab_depth_cap,
            curve_detail,
        ))
    };

    let offset = (piece_x, piece_y);
    let top_step = (piece_width / EDGE_STEP_DIV).max(EDGE_STEP_MIN);
    let side_step = (piece_height / EDGE_STEP_DIV).max(EDGE_STEP_MIN);
    let arc_steps = ((curve_detail * 6.0).round() as usize).clamp(4, 24);
    let pi = std::f64::consts::PI;

    let mut path = String::new();
    let (start_x, start_y) = map_local_point(offset, warp, top_start.0, top_start.1);
    let _ = write!(path, "M {} {}", fmt_f64(start_x), fmt_f64(start_y));
    append_segments(
        &mut path,
        &top_segments,
        EdgeOrientation::Top,
        (0.0, 0.0),
        offset,
        warp,
        top_start,
        top_step,
    );
    if round_tr {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            corner_radius,
            corner_radius,
            1.5 * pi,
            2.0 * pi,
            arc_steps,
        );
        append_local_points(&mut path, offset, warp, &arc[1..]);
    }
    append_segments(
        &mut path,
        &right_segments,
        EdgeOrientation::Right,
        (piece_width, 0.0),
        offset,
        warp,
        right_start,
        side_step,
    );
    if round_br {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.0,
            0.5 * pi,
            arc_steps,
        );
        append_local_points(&mut path, offset, warp, &arc[1..]);
    }
    append_segments(
        &mut path,
        &bottom_segments,
        EdgeOrientation::Bottom,
        (0.0, piece_height),
        offset,
        warp,
        bottom_start,
        top_step,
    );
    if round_bl {
        let arc = corner_arc_points(
            corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.5 * pi,
            pi,
            arc_steps,
        );
        append_local_points(&mut path, offset, warp, &arc[1..]);
    }
    append_segments(
        &mut path,
        &left_segments,
        EdgeOrientation::Left,
        (0.0, 0.0),
        offset,
        warp,
        left_start,
        side_step,
    );
    if round_tl {
        let arc = corner_arc_points(
            corner_radius,
            corner_radius,
            corner_radius,
            pi,
            1.5 * pi,
            arc_steps,
        );
        append_local_points(&mut path, offset, warp, &arc[1..]);
    }
    path.push_str(" Z");

    let mut top_path = build_edge_path(
        &top_segments,
        EdgeOrientation::Top,
        (0.0, 0.0),
        offset,
        warp,
        top_start,
        top_step,
    );
    let mut right_path = build_edge_path(
        &right_segments,
        EdgeOrientation::Right,
        (piece_width, 0.0),
        offset,
        warp,
        right_start,
        side_step,
    );
    let mut bottom_path = build_edge_path(
        &bottom_segments,
        EdgeOrientation::Bottom,
        (0.0, piece_height),
        offset,
        warp,
        bottom_start,
        top_step,
    );
    let mut left_path = build_edge_path(
        &left_segments,
        EdgeOrientation::Left,
        (0.0, 0.0),
        offset,
        warp,
        left_start,
        side_step,
    );

    if round_tr {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            corner_radius,
            corner_radius,
            1.5 * pi,
            2.0 * pi,
            arc_steps,
        );
        let arc_path = build_local_path(offset, warp, &arc);
        if !arc_path.is_empty() {
            if !top_path.is_empty() {
                top_path.push(' ');
            }
            top_path.push_str(&arc_path);
        }
    }
    if round_br {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.0,
            0.5 * pi,
            arc_steps,
        );
        let arc_path = build_local_path(offset, warp, &arc);
        if !arc_path.is_empty() {
            if !right_path.is_empty() {
                right_path.push(' ');
            }
            right_path.push_str(&arc_path);
        }
    }
    if round_bl {
        let arc = corner_arc_points(
            corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.5 * pi,
            pi,
            arc_steps,
        );
        let arc_path = build_local_path(offset, warp, &arc);
        if !arc_path.is_empty() {
            if !bottom_path.is_empty() {
                bottom_path.push(' ');
            }
            bottom_path.push_str(&arc_path);
        }
    }
    if round_tl {
        let arc = corner_arc_points(
            corner_radius,
            corner_radius,
            corner_radius,
            pi,
            1.5 * pi,
            arc_steps,
        );
        let arc_path = build_local_path(offset, warp, &arc);
        if !arc_path.is_empty() {
            if !left_path.is_empty() {
                left_path.push(' ');
            }
            left_path.push_str(&arc_path);
        }
    }

    PiecePaths {
        outline: path,
        edges: [top_path, right_path, bottom_path, left_path],
    }
}

#[function_component(App)]
fn app() -> Html {
    let image_size = use_state(|| None::<(u32, u32)>);
    let image_size_value = *image_size;
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let depth_cap = settings_value
        .tab_depth_cap
        .clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    let curve_detail = settings_value
        .curve_detail
        .clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    let mut grid_choices = if let Some((width, height)) = image_size_value {
        build_grid_choices(width, height)
    } else {
        Vec::new()
    };
    if image_size_value.is_some() && grid_choices.is_empty() {
        grid_choices.push(FALLBACK_GRID);
    }
    let grid_default_index = grid_choices
        .iter()
        .position(|choice| choice.target_count == DEFAULT_TARGET_COUNT)
        .unwrap_or(0);
    let grid_index = use_state(|| grid_default_index);
    let grid_index_value = *grid_index;
    let grid = grid_choices
        .get(grid_index_value)
        .copied()
        .or_else(|| grid_choices.first().copied())
        .unwrap_or(FALLBACK_GRID);
    let total = (grid.cols * grid.rows) as usize;
    let grid_label = if image_size_value.is_some() && !grid_choices.is_empty() {
        grid_choice_label(&grid)
    } else {
        "--".to_string()
    };
    let solve_time_label = if image_size_value.is_some() {
        let pieces = grid.actual_count as f64;
        format_duration(SOLVE_TIME_FACTOR * pieces.powf(SOLVE_TIME_EXPONENT))
    } else {
        "--".to_string()
    };
    let grid_options: Html = grid_choices
        .iter()
        .enumerate()
        .map(|(index, choice)| {
            let label = grid_choice_label(choice);
            html! {
                <option value={index.to_string()} selected={index == grid_index_value}>
                    {label}
                </option>
            }
        })
        .collect();

    let tab_width_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_width = value.clamp(TAB_WIDTH_MIN, TAB_WIDTH_MAX);
    });
    let tab_depth_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_depth = value.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
    });
    let tab_size_scale_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_size_scale = value.clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
    });
    let tab_size_min_input = on_setting_change(settings.clone(), |settings, value| {
        let max_allowed = settings
            .tab_size_max
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_min = value.clamp(TAB_SIZE_MIN_LIMIT, max_allowed);
    });
    let tab_size_max_input = on_setting_change(settings.clone(), |settings, value| {
        let min_allowed = settings
            .tab_size_min
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_max = value.clamp(min_allowed, TAB_SIZE_MAX_LIMIT);
    });
    let skew_input = on_setting_change(settings.clone(), |settings, value| {
        settings.skew_range = value.clamp(0.0, SKEW_RANGE_MAX);
    });
    let jitter_strength_input = on_setting_change(settings.clone(), |settings, value| {
        settings.jitter_strength = value.clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
    });
    let jitter_len_bias_input = on_setting_change(settings.clone(), |settings, value| {
        settings.jitter_len_bias = value.clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);
    });
    let tab_depth_cap_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_depth_cap = value.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    });
    let curve_detail_input = on_setting_change(settings.clone(), |settings, value| {
        settings.curve_detail = value.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    });
    let variation_input = on_setting_change(settings.clone(), |settings, value| {
        settings.variation = value.clamp(VARIATION_MIN, VARIATION_MAX);
    });
    let line_bend_input = on_setting_change(settings.clone(), |settings, value| {
        settings.line_bend_ratio = value.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
    });
    let positions = use_state(Vec::<(f64, f64)>::new);
    let active_id = use_state(|| None::<usize>);
    let dragging_members = use_state(Vec::<usize>::new);
    let animating_members = use_state(Vec::<usize>::new);
    let drag_state = use_mut_ref(|| None::<DragState>);
    let rotation_anim = use_mut_ref(|| None::<RotationAnimation>);
    let rotation_anim_handle = use_mut_ref(|| None::<Interval>);
    let rotation_queue = use_mut_ref(|| VecDeque::<QueuedRotation>::new());
    let preview_corner = use_state(|| PreviewCorner::BottomLeft);
    let drag_handlers = use_mut_ref(DragHandlers::default);
    let restore_state = use_mut_ref(|| None::<SavedBoard>);
    let restore_attempted = use_mut_ref(|| false);
    let grid_initialized = use_mut_ref(|| false);
    let svg_ref = use_node_ref();
    let workspace_scale = use_state(|| WORKSPACE_SCALE_DEFAULT);
    let workspace_scale_value = *workspace_scale;
    let z_order = use_state(Vec::<usize>::new);
    let connections = use_state(Vec::<[bool; 4]>::new);
    let rotations = use_state(Vec::<f64>::new);
    let flips = use_state(Vec::<bool>::new);
    let rotation_enabled = use_state(|| true);
    let rotation_enabled_value = *rotation_enabled;
    let animations_enabled = use_state(|| false);
    let animations_enabled_value = *animations_enabled;
    let hovered_id = use_state(|| None::<usize>);
    let rotation_noise = use_state(|| ROTATION_NOISE_DEFAULT);
    let rotation_noise_value = *rotation_noise;
    let rotation_snap_tolerance = use_state(|| ROTATION_SNAP_TOLERANCE_DEFAULT_DEG);
    let rotation_snap_tolerance_value = *rotation_snap_tolerance;
    let rotation_lock_threshold = use_state(|| ROTATION_LOCK_THRESHOLD_DEFAULT);
    let rotation_lock_threshold_value = *rotation_lock_threshold;
    let snap_distance_ratio = use_state(|| SNAP_DISTANCE_RATIO_DEFAULT);
    let snap_distance_ratio_value = *snap_distance_ratio;
    let scramble_nonce = use_state(|| 0u64);
    let scramble_nonce_value = *scramble_nonce;
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(|| false);
    let show_controls_value = *show_controls;
    let show_debug = use_state(|| false);
    let show_debug_value = *show_debug;
    let dragging_members_value = (*dragging_members).clone();
    let animating_members_value = (*animating_members).clone();
    let hovered_id_value = *hovered_id;
    let status_label = if solved_value { "Solved" } else { "In progress" };
    let status_class = if solved_value {
        "status status-solved"
    } else {
        "status"
    };
    let solved_banner = if solved_value {
        html! { <div class="solved-banner">{ "Solved!" }</div> }
    } else {
        html! {}
    };

    let seed_label = if image_size_value.is_some() {
        let cols = grid.cols as usize;
        let rows = grid.rows as usize;
        format!(
            "{:#x}",
            scramble_seed(PUZZLE_SEED, scramble_nonce_value, cols, rows)
        )
    } else {
        "--".to_string()
    };
    {
        let positions = positions.clone();
        let active_id = active_id.clone();
        let drag_state = drag_state.clone();
        let dragging_members = dragging_members.clone();
        let animating_members = animating_members.clone();
        let rotation_anim = rotation_anim.clone();
        let rotation_anim_handle = rotation_anim_handle.clone();
        let rotation_queue = rotation_queue.clone();
        let z_order = z_order.clone();
        let connections = connections.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let solved = solved.clone();
        let scramble_nonce = scramble_nonce.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let grid_default_index = grid_default_index;
        let restore_state = restore_state.clone();
        let restore_attempted = restore_attempted.clone();
        let grid_initialized = grid_initialized.clone();
        use_effect_with(
            (grid_index_value, image_size_value),
            move |(grid_index_value, image_size_value)| {
                if let Some((width, height)) = *image_size_value {
                    let mut allow_scramble = true;
                    let mut skip_scramble = false;
                    if grid_choices.is_empty() {
                        allow_scramble = false;
                    } else if *grid_index_value >= grid_choices.len() {
                        grid_index.set(grid_default_index.min(grid_choices.len() - 1));
                        *grid_initialized.borrow_mut() = true;
                        allow_scramble = false;
                    }
                    if allow_scramble {
                        let saved_state = {
                            let mut attempted = restore_attempted.borrow_mut();
                            if !*attempted {
                                *attempted = true;
                                *restore_state.borrow_mut() = load_saved_board();
                            }
                            restore_state.borrow_mut().take()
                        };
                        if let Some(state) = saved_state {
                            if state.image_width == width && state.image_height == height {
                                if let Some(saved_index) =
                                    grid_choice_index(&grid_choices, state.cols, state.rows)
                                {
                                    if saved_index != *grid_index_value {
                                        *restore_state.borrow_mut() = Some(state);
                                        grid_index.set(saved_index);
                                        *grid_initialized.borrow_mut() = true;
                                        skip_scramble = true;
                                    } else {
                                        let cols = state.cols as usize;
                                        let rows = state.rows as usize;
                                        let total = cols * rows;
                                        if validate_saved_board(&state, total) {
                                            let piece_width = width as f64 / state.cols as f64;
                                            let piece_height = height as f64 / state.rows as f64;
                                            positions.set(state.positions.clone());
                                            rotations.set(state.rotations.clone());
                                            flips.set(state.flips.clone());
                                            connections.set(state.connections.clone());
                                            z_order.set(state.z_order.clone());
                                            scramble_nonce.set(state.scramble_nonce);
                                            active_id.set(None);
                                            dragging_members.set(Vec::new());
                                            animating_members.set(Vec::new());
                                            *rotation_anim.borrow_mut() = None;
                                            rotation_anim_handle.borrow_mut().take();
                                            rotation_queue.borrow_mut().clear();
                                            *drag_state.borrow_mut() = None;
                                            let solved_now = is_solved(
                                                &state.positions,
                                                &state.rotations,
                                                &state.flips,
                                                &state.connections,
                                                cols,
                                                rows,
                                                piece_width,
                                                piece_height,
                                                rotation_enabled_value,
                                            );
                                            solved.set(solved_now);
                                            *grid_initialized.borrow_mut() = true;
                                            skip_scramble = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if allow_scramble && !skip_scramble {
                        let mut initialized = grid_initialized.borrow_mut();
                        if !*initialized {
                            if *grid_index_value != grid_default_index {
                                grid_index.set(grid_default_index);
                                *initialized = true;
                                allow_scramble = false;
                            } else {
                                *initialized = true;
                            }
                        }
                    }
                    if allow_scramble && !skip_scramble {
                        let grid = grid_choices
                            .get(*grid_index_value)
                            .copied()
                            .unwrap_or(FALLBACK_GRID);
                        let cols = grid.cols as usize;
                        let rows = grid.rows as usize;
                        let piece_width = width as f64 / grid.cols as f64;
                        let piece_height = height as f64 / grid.rows as f64;
                        let view_width = width as f64 * workspace_scale_value;
                        let view_height = height as f64 * workspace_scale_value;
                        let view_min_x = (width as f64 - view_width) * 0.5;
                        let view_min_y = (height as f64 - view_height) * 0.5;
                        let margin =
                            piece_width.max(piece_height) * (depth_cap + MAX_LINE_BEND_RATIO);
                        let nonce = time_nonce(*scramble_nonce);
                        let seed = scramble_seed(PUZZLE_SEED, nonce, cols, rows);
                        let rotation_seed = splitmix64(seed ^ 0xC0DE_F00D);
                        let flip_seed = splitmix64(seed ^ 0xF11F_5EED);
                        let (next_positions, order) = scramble_layout(
                            seed,
                            cols,
                            rows,
                            piece_width,
                            piece_height,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            margin,
                        );
                        positions.set(next_positions);
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        animating_members.set(Vec::new());
                        *rotation_anim.borrow_mut() = None;
                        rotation_anim_handle.borrow_mut().take();
                        rotation_queue.borrow_mut().clear();
                        *drag_state.borrow_mut() = None;
                        z_order.set(order);
                        connections.set(vec![[false; 4]; cols * rows]);
                        rotations.set(scramble_rotations(
                            rotation_seed,
                            cols * rows,
                            rotation_enabled_value,
                        ));
                        flips.set(scramble_flips(flip_seed, cols * rows, FLIP_CHANCE));
                        solved.set(false);
                        scramble_nonce.set(nonce);
                    }
                }
                || ()
            },
        );
    }

    let on_grid_change = {
        let grid_index = grid_index.clone();
        let grid_choices_len = grid_choices.len();
        Callback::from(move |event: Event| {
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < grid_choices_len {
                    grid_index.set(value);
                }
            }
        })
    };
    let on_workspace_scale = {
        let workspace_scale = workspace_scale.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                workspace_scale.set(value.clamp(WORKSPACE_SCALE_MIN, WORKSPACE_SCALE_MAX));
            }
        })
    };
    let on_frame_snap = {
        let frame_snap_ratio = frame_snap_ratio.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                frame_snap_ratio.set(value.clamp(FRAME_SNAP_MIN, FRAME_SNAP_MAX));
            }
        })
    };
    let on_snap_distance = {
        let snap_distance_ratio = snap_distance_ratio.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                snap_distance_ratio.set(value.clamp(
                    SNAP_DISTANCE_RATIO_MIN,
                    SNAP_DISTANCE_RATIO_MAX,
                ));
            }
        })
    };
    let on_rotation_toggle = {
        let rotation_enabled = rotation_enabled.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let positions = positions.clone();
        let connections = connections.clone();
        let image_size = image_size.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let solved = solved.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            rotation_enabled.set(enabled);
            let total = positions.len();
            let rotations_snapshot = if enabled {
                (*rotations).clone()
            } else {
                let zeroed = vec![0.0; total];
                rotations.set(zeroed.clone());
                zeroed
            };
            if let Some((width, height)) = *image_size {
                let grid = grid_choices
                    .get(*grid_index)
                    .copied()
                    .unwrap_or(FALLBACK_GRID);
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let piece_width = width as f64 / grid.cols as f64;
                let piece_height = height as f64 / grid.rows as f64;
                let positions_snapshot = (*positions).clone();
                let flips_snapshot = (*flips).clone();
                let connections_snapshot = (*connections).clone();
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    enabled,
                );
                solved.set(solved_now);
            }
        })
    };
    let on_rotation_noise = {
        let rotation_noise = rotation_noise.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                rotation_noise.set(value.clamp(ROTATION_NOISE_MIN, ROTATION_NOISE_MAX));
            }
        })
    };
    let on_rotation_snap_tolerance = {
        let rotation_snap_tolerance = rotation_snap_tolerance.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                rotation_snap_tolerance.set(value.clamp(
                    ROTATION_SNAP_TOLERANCE_MIN_DEG,
                    ROTATION_SNAP_TOLERANCE_MAX_DEG,
                ));
            }
        })
    };
    let on_rotation_lock_threshold = {
        let rotation_lock_threshold = rotation_lock_threshold.clone();
        let positions = positions.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f64>() {
                let max_value = positions.len().max(ROTATION_LOCK_THRESHOLD_MIN);
                let rounded = value.round() as usize;
                let clamped = rounded
                    .max(ROTATION_LOCK_THRESHOLD_MIN)
                    .min(max_value);
                rotation_lock_threshold.set(clamped);
            }
        })
    };
    let on_animations_toggle = {
        let animations_enabled = animations_enabled.clone();
        let animating_members = animating_members.clone();
        let rotation_anim = rotation_anim.clone();
        let rotation_anim_handle = rotation_anim_handle.clone();
        let rotation_queue = rotation_queue.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            animations_enabled.set(enabled);
            if !enabled {
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
            }
        })
    };
    let on_debug_toggle = {
        let show_debug = show_debug.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            show_debug.set(input.checked());
        })
    };

    {
        let positions = positions.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let connections = connections.clone();
        let z_order = z_order.clone();
        let image_size = image_size.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let scramble_nonce = scramble_nonce.clone();
        let active_id = active_id.clone();
        let animating_members = animating_members.clone();
        use_effect_with(
            (
                (*positions).clone(),
                (*rotations).clone(),
                (*flips).clone(),
                (*connections).clone(),
                (*z_order).clone(),
                *grid_index,
                *scramble_nonce,
                *image_size,
                *active_id,
                (*animating_members).clone(),
            ),
            move |(
                positions_value,
                rotations_value,
                flips_value,
                connections_value,
                z_order_value,
                grid_index_value,
                scramble_nonce_value,
                image_size_value,
                active_id_value,
                animating_members_value,
            )| {
                if active_id_value.is_none() && animating_members_value.is_empty() {
                    if let Some((width, height)) = *image_size_value {
                        let grid = grid_choices
                            .get(*grid_index_value)
                            .copied()
                            .unwrap_or(FALLBACK_GRID);
                        let total = (grid.cols * grid.rows) as usize;
                        if positions_value.len() == total
                            && rotations_value.len() == total
                            && flips_value.len() == total
                            && connections_value.len() == total
                            && z_order_value.len() == total
                            && z_order_value.iter().all(|id| *id < total)
                        {
                            let state = SavedBoard {
                                version: STORAGE_VERSION,
                                cols: grid.cols,
                                rows: grid.rows,
                                image_width: width,
                                image_height: height,
                                positions: positions_value.clone(),
                                rotations: rotations_value.clone(),
                                flips: flips_value.clone(),
                                connections: connections_value.clone(),
                                z_order: z_order_value.clone(),
                                scramble_nonce: *scramble_nonce_value,
                            };
                            save_board_state(&state);
                        }
                    }
                }
                || ()
            },
        );
    }

    {
        let show_controls = show_controls.clone();
        use_effect_with(
            show_controls_value,
            move |show_controls_value| {
                let current = *show_controls_value;
                let window = web_sys::window().expect("window available");
                let options = EventListenerOptions {
                    phase: EventListenerPhase::Capture,
                    passive: false,
                };
                let listener = EventListener::new_with_options(
                    &window,
                    "keydown",
                    options,
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<KeyboardEvent>() {
                            if event.repeat() {
                                return;
                            }
                            let key = event.key();
                            let code = event.code();
                            let toggle = matches!(key.as_str(), "?" | "d" | "D")
                                || matches!(code.as_str(), "KeyD" | "Slash");
                            if toggle {
                                let next = !current;
                                gloo::console::log!(
                                    "controls",
                                    format!("{} -> {}", current, next),
                                    key,
                                    code
                                );
                                show_controls.set(next);
                                event.prevent_default();
                            }
                        }
                    },
                );
                || drop(listener)
            },
        );
    }

    {
        let drag_handlers = drag_handlers.clone();
        use_effect_with(
            (),
            move |_| {
                let window = web_sys::window().expect("window available");
                let move_handlers = drag_handlers.clone();
                let move_listener = EventListener::new_with_options(
                    &window,
                    "mousemove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = move_handlers.borrow().on_move.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_move_handlers = drag_handlers.clone();
                let touch_move_listener = EventListener::new_with_options(
                    &window,
                    "touchmove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) = touch_move_handlers.borrow().on_touch_move.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let up_handlers = drag_handlers.clone();
                let up_listener = EventListener::new_with_options(
                    &window,
                    "mouseup",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = up_handlers.borrow().on_release.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_end_handlers = drag_handlers.clone();
                let touch_end_listener = EventListener::new_with_options(
                    &window,
                    "touchend",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_end_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_cancel_handlers = drag_handlers.clone();
                let touch_cancel_listener = EventListener::new_with_options(
                    &window,
                    "touchcancel",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_cancel_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                || {
                    drop(move_listener);
                    drop(touch_move_listener);
                    drop(up_listener);
                    drop(touch_end_listener);
                    drop(touch_cancel_listener);
                }
            },
        );
    }

    {
        let image_size = image_size.clone();
        use_effect_with(
            (),
            move |_| {
                let img = HtmlImageElement::new().expect("create image element");
                let img_clone = img.clone();
                let onload = Closure::<dyn FnMut()>::wrap(Box::new(move || {
                    let width = img_clone.natural_width();
                    let height = img_clone.natural_height();
                    image_size.set(Some((width, height)));
                }));
                img.set_onload(Some(onload.as_ref().unchecked_ref()));
                img.set_src(IMAGE_SRC);
                onload.forget();
                || ()
            },
        );
    }

    let (content, on_scramble, on_solve, on_solve_rotation, on_unflip, scramble_disabled) =
        if let Some((width, height)) = *image_size {
        let width_f = width as f64;
        let height_f = height as f64;
        let view_width = width_f * workspace_scale_value;
        let view_height = height_f * workspace_scale_value;
        let view_min_x = (width_f - view_width) * 0.5;
        let view_min_y = (height_f - view_height) * 0.5;
        let view_box = format!(
            "{} {} {} {}",
            fmt_f64(view_min_x),
            fmt_f64(view_min_y),
            fmt_f64(view_width),
            fmt_f64(view_height)
        );
        let piece_width = width_f / grid.cols as f64;
        let piece_height = height_f / grid.rows as f64;
        let max_depth = piece_width.max(piece_height) * depth_cap;
        let pieces = build_pieces(grid.rows, grid.cols);

        let (horizontal, vertical) =
            build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &settings_value);
        let (horizontal_waves, vertical_waves) = build_line_waves(
            grid.rows,
            grid.cols,
            PUZZLE_SEED,
            piece_width,
            piece_height,
            settings_value.line_bend_ratio,
        );
        let warp_field = WarpField {
            width: width_f,
            height: height_f,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };
        let max_bend = horizontal_waves
            .iter()
            .chain(vertical_waves.iter())
            .fold(0.0_f64, |acc, wave| acc.max(wave.amplitude.abs()));
        let mask_pad = max_depth + max_bend;
        let mut frame_corner_radius = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
        let max_corner_radius = piece_width.min(piece_height) * 0.45;
        if frame_corner_radius > max_corner_radius {
            frame_corner_radius = max_corner_radius;
        }
        let frame_corner_radius = fmt_f64(frame_corner_radius);
        let mask_x = fmt_f64(-mask_pad);
        let mask_y = fmt_f64(-mask_pad);
        let mask_width = fmt_f64(piece_width + mask_pad * 2.0);
        let mask_height = fmt_f64(piece_height + mask_pad * 2.0);
        let center_min_x = view_min_x + piece_width * 0.5;
        let center_min_y = view_min_y + piece_height * 0.5;
        let mut center_max_x = view_min_x + view_width - piece_width * 0.5;
        let mut center_max_y = view_min_y + view_height - piece_height * 0.5;
        if center_max_x < center_min_x {
            center_max_x = center_min_x;
        }
        if center_max_y < center_min_y {
            center_max_y = center_min_y;
        }
        let piece_shapes: Vec<(Piece, PiecePaths)> = pieces
            .iter()
            .map(|piece| {
                let paths = build_piece_path(
                    piece,
                    piece_width,
                    piece_height,
                    &horizontal,
                    &vertical,
                    &warp_field,
                    depth_cap,
                    curve_detail,
                );
                (*piece, paths)
            })
            .collect();

        let positions_value = (*positions).clone();
        let rotations_value = (*rotations).clone();
        let flips_value = (*flips).clone();
        let connections_value = (*connections).clone();
        let hovered_group = if let Some(id) = hovered_id_value {
            if id < connections_value.len() {
                collect_group(&connections_value, id, grid.cols as usize, grid.rows as usize)
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        let mut hovered_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &hovered_group {
            if *id < hovered_mask.len() {
                hovered_mask[*id] = true;
            }
        }
        let mut dragging_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &dragging_members_value {
            if *id < dragging_mask.len() {
                dragging_mask[*id] = true;
            }
        }
        let mut animating_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &animating_members_value {
            if *id < animating_mask.len() {
                animating_mask[*id] = true;
            }
        }
        let z_order_value = (*z_order).clone();
        let drag_move_common: Rc<dyn Fn(f64, f64) -> bool> = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let drag_state = drag_state.clone();
            Rc::new(move |x: f64, y: f64| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    if drag.rotate_mode && rotation_enabled_value {
                        let current_angle = (y - drag.pivot_y).atan2(x - drag.pivot_x);
                        let delta_deg = (current_angle - drag.start_angle).to_degrees();
                        let mut next = (*positions).clone();
                        let mut next_rotations = (*rotations).clone();
                        let flips_snapshot = &*flips;
                        for (index, member) in drag.members.iter().enumerate() {
                            if let Some(start) = drag.start_positions.get(index) {
                                let center_x = start.0 + piece_width * 0.5;
                                let center_y = start.1 + piece_height * 0.5;
                                let (rx, ry) = rotate_point(
                                    center_x,
                                    center_y,
                                    drag.pivot_x,
                                    drag.pivot_y,
                                    delta_deg,
                                );
                                if let Some(pos) = next.get_mut(*member) {
                                    *pos = (
                                        rx - piece_width * 0.5,
                                        ry - piece_height * 0.5,
                                    );
                                }
                                if let Some(rot) = next_rotations.get_mut(*member) {
                                    let start_rot =
                                        drag.start_rotations.get(index).copied().unwrap_or(0.0);
                                    let flipped = flips_snapshot.get(*member).copied().unwrap_or(false);
                                    let signed_delta = if flipped { -delta_deg } else { delta_deg };
                                    *rot = normalize_angle(start_rot + signed_delta);
                                }
                            }
                        }
                        positions.set(next);
                        rotations.set(next_rotations);
                        true
                    } else {
                        let mut dx = x - drag.start_x;
                        let mut dy = y - drag.start_y;
                        if !drag.start_positions.is_empty() {
                            let mut min_start_x = f64::INFINITY;
                            let mut max_start_x = f64::NEG_INFINITY;
                            let mut min_start_y = f64::INFINITY;
                            let mut max_start_y = f64::NEG_INFINITY;
                            for start in &drag.start_positions {
                                let center_x = start.0 + piece_width * 0.5;
                                let center_y = start.1 + piece_height * 0.5;
                                min_start_x = min_start_x.min(center_x);
                                max_start_x = max_start_x.max(center_x);
                                min_start_y = min_start_y.min(center_y);
                                max_start_y = max_start_y.max(center_y);
                            }
                            let min_dx = center_min_x - min_start_x;
                            let max_dx = center_max_x - max_start_x;
                            let min_dy = center_min_y - min_start_y;
                            let max_dy = center_max_y - max_start_y;
                            let rubber_limit = piece_width.min(piece_height) * RUBBER_BAND_RATIO;
                            if min_dx <= max_dx {
                                dx = rubber_band_clamp(dx, min_dx, max_dx, rubber_limit);
                            }
                            if min_dy <= max_dy {
                                dy = rubber_band_clamp(dy, min_dy, max_dy, rubber_limit);
                            }
                        }
                        let mut next = (*positions).clone();
                        for (index, member) in drag.members.iter().enumerate() {
                            if let Some(pos) = next.get_mut(*member) {
                                let start = drag.start_positions[index];
                                *pos = (start.0 + dx, start.1 + dy);
                            }
                        }
                        positions.set(next);
                        true
                    }
                } else {
                    false
                }
            })
        };
        let drag_move = {
            let svg_ref = svg_ref.clone();
            let drag_move_common = drag_move_common.clone();
            move |event: &MouseEvent| {
                if let Some((x, y)) = event_to_svg_coords(
                    event,
                    &svg_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) {
                    if drag_move_common(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_move_touch = {
            let svg_ref = svg_ref.clone();
            let drag_state = drag_state.clone();
            let active_id = active_id.clone();
            let dragging_members = dragging_members.clone();
            let drag_move_common = drag_move_common.clone();
            move |event: &TouchEvent| {
                if event.touches().length() > 1 {
                    if drag_state.borrow().is_some() {
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                    }
                    return;
                }
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                if let Some((x, y)) = touch_event_to_svg_coords(
                    event,
                    &svg_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    false,
                ) {
                    if drag_move_common(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_release_common: Rc<dyn Fn(Option<(f64, f64)>) -> bool> = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            Rc::new(move |coords: Option<(f64, f64)>| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    let ctrl_flip = drag.rotate_mode;
                    let mut next = (*positions).clone();
                    let mut next_rotations = (*rotations).clone();
                    let mut next_flips = (*flips).clone();
                    let mut next_connections = (*connections).clone();
                    let start_positions_all = next.clone();
                    let start_rotations_all = next_rotations.clone();
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let snap_distance = piece_width.min(piece_height) * snap_distance_ratio_value;
                    let click_tolerance = piece_width.min(piece_height) * CLICK_MOVE_RATIO;
                    let start_group_animation = {
                        let positions = positions.clone();
                        let rotations = rotations.clone();
                        let flips = flips.clone();
                        let connections = connections.clone();
                        let solved = solved.clone();
                        let rotation_anim = rotation_anim.clone();
                        let rotation_anim_handle = rotation_anim_handle.clone();
                        let animating_members = animating_members.clone();
                        let rotation_queue = rotation_queue.clone();
                        let rotation_enabled_value = rotation_enabled_value;
                        let rotation_noise_value = rotation_noise_value;
                        let animations_enabled_value = animations_enabled_value;
                        let rotation_snap_tolerance_value = rotation_snap_tolerance_value;
                        let cols = cols;
                        let rows = rows;
                        let piece_width = piece_width;
                        let piece_height = piece_height;
                        let frame_snap_ratio_value = frame_snap_ratio_value;
                        let snap_distance_ratio_value = snap_distance_ratio_value;
                        let center_min_x = center_min_x;
                        let center_max_x = center_max_x;
                        let center_min_y = center_min_y;
                        let center_max_y = center_max_y;
                        let view_min_x = view_min_x;
                        let view_min_y = view_min_y;
                        let view_width = view_width;
                        let view_height = view_height;
                        move |anim: RotationAnimation,
                              connections_override: Option<Vec<[bool; 4]>>| {
                            let connections_override = connections_override.map(Rc::new);
                            if !animations_enabled_value {
                                rotation_queue.borrow_mut().clear();
                                let mut next_positions = (*positions).clone();
                                let mut next_rotations = (*rotations).clone();
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) =
                                                anim.start_positions.get(index)
                                            {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    *delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + *delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        target_center,
                                        target_rot,
                                        ..
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let rot = normalize_angle(*target_rot);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                *target_center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                let should_snap = matches!(&anim.kind, AnimationKind::Pivot { .. });
                                let mut connections_snapshot = connections_override
                                    .as_ref()
                                    .map(|snapshot| (**snapshot).clone())
                                    .unwrap_or_else(|| (*connections).clone());
                                let flips_snapshot = (*flips).clone();
                                if should_snap {
                                    let snap_distance = piece_width.min(piece_height)
                                        * snap_distance_ratio_value;
                                    apply_snaps_for_group(
                                        &anim.members,
                                        &mut next_positions,
                                        &mut next_rotations,
                                        &flips_snapshot,
                                        &mut connections_snapshot,
                                        cols,
                                        rows,
                                        piece_width,
                                        piece_height,
                                        snap_distance,
                                        frame_snap_ratio_value,
                                        center_min_x,
                                        center_max_x,
                                        center_min_y,
                                        center_max_y,
                                        view_min_x,
                                        view_min_y,
                                        view_width,
                                        view_height,
                                        rotation_snap_tolerance_value,
                                        rotation_enabled_value,
                                    );
                                }
                                positions.set(next_positions.clone());
                                rotations.set(next_rotations.clone());
                                if should_snap || connections_override.is_some() {
                                    connections.set(connections_snapshot.clone());
                                }
                                animating_members.set(Vec::new());
                                *rotation_anim.borrow_mut() = None;
                                rotation_anim_handle.borrow_mut().take();
                                let solved_now = is_solved(
                                    &next_positions,
                                    &next_rotations,
                                    &flips_snapshot,
                                    &connections_snapshot,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                solved.set(solved_now);
                                return;
                            }
                            let members = anim.members.clone();
                            *rotation_anim.borrow_mut() = Some(anim);
                            animating_members.set(members);
                            rotation_anim_handle.borrow_mut().take();
                            let positions = positions.clone();
                            let rotations = rotations.clone();
                            let flips = flips.clone();
                            let connections = connections.clone();
                            let solved = solved.clone();
                            let rotation_anim = rotation_anim.clone();
                            let rotation_anim_handle = rotation_anim_handle.clone();
                            let animating_members = animating_members.clone();
                            let rotation_anim_handle_for_tick = rotation_anim_handle.clone();
                            let rotation_queue = rotation_queue.clone();
                            let connections_override = connections_override.clone();
                            let interval = Interval::new(16, move || {
                                let now = Date::now();
                                let anim = match rotation_anim.borrow().clone() {
                                    Some(value) => value,
                                    None => {
                                        rotation_anim_handle_for_tick.borrow_mut().take();
                                        animating_members.set(Vec::new());
                                        return;
                                    }
                                };
                                let mut t = (now - anim.start_time) / anim.duration;
                                if t < 0.0 {
                                    t = 0.0;
                                } else if t > 1.0 {
                                    t = 1.0;
                                }
                                let eased = t * t * (3.0 - 2.0 * t);
                                let mut next_positions = (*positions).clone();
                                let mut next_rotations = (*rotations).clone();
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        let current_delta = *delta * eased;
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) = anim.start_positions.get(index) {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    current_delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + current_delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let center = (
                                            start_center.0
                                                + (target_center.0 - start_center.0) * eased,
                                            start_center.1
                                                + (target_center.1 - start_center.1) * eased,
                                        );
                                        let delta = angle_delta(*target_rot, *start_rot);
                                        let rot = normalize_angle(*start_rot + delta * eased);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                positions.set(next_positions.clone());
                                rotations.set(next_rotations.clone());
                                if t >= 1.0 {
                                    let queued = rotation_queue.borrow_mut().pop_front();
                                    if let Some(next_step) = queued {
                                        let members = next_step.members.clone();
                                        let mut start_positions =
                                            Vec::with_capacity(members.len());
                                        let mut start_rotations =
                                            Vec::with_capacity(members.len());
                                        for member in &members {
                                            if let Some(pos) =
                                                next_positions.get(*member)
                                            {
                                                start_positions.push(*pos);
                                            } else {
                                                start_positions.push((0.0, 0.0));
                                            }
                                            let rot = next_rotations
                                                .get(*member)
                                                .copied()
                                                .unwrap_or(0.0);
                                            start_rotations.push(rot);
                                        }
                                        let current_angle = members
                                            .first()
                                            .and_then(|id| next_rotations.get(*id))
                                            .copied()
                                            .unwrap_or(0.0);
                                        let delta = click_rotation_delta(
                                            current_angle,
                                            next_step.noise,
                                            rotation_noise_value,
                                            rotation_snap_tolerance_value,
                                        );
                                        *rotation_anim.borrow_mut() = Some(RotationAnimation {
                                            start_time: Date::now(),
                                            duration: SNAP_ANIMATION_MS,
                                            members: members.clone(),
                                            start_positions,
                                            start_rotations,
                                            kind: AnimationKind::Pivot {
                                                pivot_x: next_step.pivot_x,
                                                pivot_y: next_step.pivot_y,
                                                delta,
                                            },
                                        });
                                        animating_members.set(members);
                                        return;
                                    }
                                    *rotation_anim.borrow_mut() = None;
                                    animating_members.set(Vec::new());
                                    let should_snap =
                                        matches!(&anim.kind, AnimationKind::Pivot { .. });
                                    let mut snapped_positions = next_positions.clone();
                                    let mut snapped_rotations = next_rotations.clone();
                                    let mut connections_snapshot = connections_override
                                        .as_ref()
                                        .map(|snapshot| (**snapshot).clone())
                                        .unwrap_or_else(|| (*connections).clone());
                                    let flips_snapshot = (*flips).clone();
                                    if should_snap {
                                        let snap_distance = piece_width.min(piece_height)
                                            * snap_distance_ratio_value;
                                        apply_snaps_for_group(
                                            &anim.members,
                                            &mut snapped_positions,
                                            &mut snapped_rotations,
                                            &flips_snapshot,
                                            &mut connections_snapshot,
                                            cols,
                                            rows,
                                            piece_width,
                                            piece_height,
                                            snap_distance,
                                            frame_snap_ratio_value,
                                            center_min_x,
                                            center_max_x,
                                            center_min_y,
                                            center_max_y,
                                            view_min_x,
                                            view_min_y,
                                            view_width,
                                            view_height,
                                            rotation_snap_tolerance_value,
                                            rotation_enabled_value,
                                        );
                                    }
                                    positions.set(snapped_positions.clone());
                                    rotations.set(snapped_rotations.clone());
                                    if should_snap || connections_override.is_some() {
                                        connections.set(connections_snapshot.clone());
                                    }
                                    let solved_now = is_solved(
                                        &snapped_positions,
                                        &snapped_rotations,
                                        &flips_snapshot,
                                        &connections_snapshot,
                                        cols,
                                        rows,
                                        piece_width,
                                        piece_height,
                                        rotation_enabled_value,
                                    );
                                    solved.set(solved_now);
                                    rotation_anim_handle_for_tick.borrow_mut().take();
                                }
                            });
                            *rotation_anim_handle.borrow_mut() = Some(interval);
                        }
                    };
                    if let Some((x, y)) = coords {
                        let dx = x - drag.start_x;
                        let dy = y - drag.start_y;
                        let dist = (dx * dx + dy * dy).sqrt();
                        let elapsed = Date::now() - drag.start_time;
                        if dist <= click_tolerance && elapsed <= CLICK_MAX_DURATION_MS {
                            let click_id = drag.primary_id;
                            let was_flipped = next_flips.get(click_id).copied().unwrap_or(false);
                            if ctrl_flip {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = !*flip;
                                }
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
                                let solved_now = is_solved(
                                    &next,
                                    &next_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                positions.set(next);
                                connections.set(next_connections);
                                rotations.set(next_rotations);
                                flips.set(next_flips);
                                solved.set(solved_now);
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                return true;
                            }
                            if was_flipped {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = false;
                                }
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
                                let solved_now = is_solved(
                                    &next,
                                    &next_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                positions.set(next);
                                connections.set(next_connections);
                                rotations.set(next_rotations);
                                flips.set(next_flips);
                                solved.set(solved_now);
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                return true;
                            }
                            if rotation_enabled_value && !drag.members.is_empty() {
                                let pivot_x = drag.start_x;
                                let pivot_y = drag.start_y;
                                let members = drag.members.clone();
                                let mut noise = 0.0;
                                if rotation_noise_value > 0.0 {
                                    let noise_seed = splitmix64(
                                        (drag.start_time as u64)
                                            ^ (drag.primary_id as u64)
                                                .wrapping_mul(0x9e3779b97f4a7c15),
                                    );
                                    noise = rand_range(
                                        noise_seed,
                                        members.len() as u64,
                                        -rotation_noise_value,
                                        rotation_noise_value,
                                    );
                                }
                                if animations_enabled_value {
                                    if let Some(active_anim) = rotation_anim.borrow().clone() {
                                        if active_anim.members.contains(&click_id) {
                                            rotation_queue
                                                .borrow_mut()
                                                .push_back(QueuedRotation {
                                                    members: active_anim.members,
                                                    pivot_x,
                                                    pivot_y,
                                                    noise,
                                                });
                                            dragging_members.set(Vec::new());
                                            active_id.set(None);
                                            *drag_state.borrow_mut() = None;
                                            return true;
                                        }
                                    }
                                }
                                let current_angle = members
                                    .first()
                                    .and_then(|id| next_rotations.get(*id))
                                    .copied()
                                    .unwrap_or(0.0);
                                let group_size = members.len();
                                let total = cols * rows;
                                let rotation_locked =
                                    group_size == total || group_size > rotation_lock_threshold_value;
                                if group_size > 1
                                    && rotation_locked
                                    && angle_matches(
                                        current_angle,
                                        0.0,
                                        rotation_snap_tolerance_value,
                                    )
                                {
                                    active_id.set(None);
                                    dragging_members.set(Vec::new());
                                    *drag_state.borrow_mut() = None;
                                    return true;
                                }
                                let delta = click_rotation_delta(
                                    current_angle,
                                    noise,
                                    rotation_noise_value,
                                    rotation_snap_tolerance_value,
                                );
                                let mut start_positions = Vec::with_capacity(members.len());
                                let mut start_rotations = Vec::with_capacity(members.len());
                                for member in &members {
                                    if let Some(pos) = next.get(*member) {
                                        start_positions.push(*pos);
                                    } else {
                                        start_positions.push((0.0, 0.0));
                                    }
                                    let rot = next_rotations.get(*member).copied().unwrap_or(0.0);
                                    start_rotations.push(rot);
                                }
                                rotation_queue.borrow_mut().clear();
                                start_group_animation(
                                    RotationAnimation {
                                        start_time: Date::now(),
                                        duration: SNAP_ANIMATION_MS,
                                        members: members.clone(),
                                        start_positions,
                                        start_rotations,
                                        kind: AnimationKind::Pivot {
                                            pivot_x,
                                            pivot_y,
                                            delta,
                                        },
                                    },
                                    None,
                                );
                                dragging_members.set(Vec::new());
                                active_id.set(None);
                                *drag_state.borrow_mut() = None;
                                return true;
                            }
                        }
                    }
                    let group_after = apply_snaps_for_group(
                        &drag.members,
                        &mut next,
                        &mut next_rotations,
                        &next_flips,
                        &mut next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        snap_distance,
                        frame_snap_ratio_value,
                        center_min_x,
                        center_max_x,
                        center_min_y,
                        center_max_y,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                        rotation_snap_tolerance_value,
                        rotation_enabled_value,
                    );

                    let mut pending_animation = None;
                    if rotation_enabled_value && !group_after.is_empty() {
                        let anchor_id = if group_after.contains(&drag.primary_id) {
                            drag.primary_id
                        } else {
                            group_after[0]
                        };
                        if anchor_id < next.len() {
                            let start_rot = start_rotations_all
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(0.0);
                            let target_rot = next_rotations
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(start_rot);
                            if angle_delta(target_rot, start_rot).abs() > 0.01 {
                                let start_pos = start_positions_all
                                    .get(anchor_id)
                                    .copied()
                                    .unwrap_or(next[anchor_id]);
                                let target_pos = next[anchor_id];
                                let start_center = (
                                    start_pos.0 + piece_width * 0.5,
                                    start_pos.1 + piece_height * 0.5,
                                );
                                let target_center = (
                                    target_pos.0 + piece_width * 0.5,
                                    target_pos.1 + piece_height * 0.5,
                                );
                                let mut member_positions = Vec::with_capacity(group_after.len());
                                let mut member_rotations = Vec::with_capacity(group_after.len());
                                for member in &group_after {
                                    if let Some(pos) = start_positions_all.get(*member) {
                                        member_positions.push(*pos);
                                    } else {
                                        member_positions.push((0.0, 0.0));
                                    }
                                    let rot = start_rotations_all
                                        .get(*member)
                                        .copied()
                                        .unwrap_or(0.0);
                                    member_rotations.push(rot);
                                }
                                pending_animation = Some(RotationAnimation {
                                    start_time: Date::now(),
                                    duration: SNAP_ANIMATION_MS,
                                    members: group_after.clone(),
                                    start_positions: member_positions,
                                    start_rotations: member_rotations,
                                    kind: AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    },
                                });
                            }
                        }
                    }
                    if let Some(anim) = pending_animation {
                        let connections_snapshot = next_connections.clone();
                        connections.set(next_connections);
                        flips.set(next_flips);
                        start_group_animation(anim, Some(connections_snapshot));
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                        return true;
                    }

                    let solved_now = is_solved(
                        &next,
                        &next_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        rotation_enabled_value,
                    );
                    positions.set(next);
                    connections.set(next_connections);
                    rotations.set(next_rotations);
                    flips.set(next_flips);
                    solved.set(solved_now);
                    active_id.set(None);
                    dragging_members.set(Vec::new());
                    *drag_state.borrow_mut() = None;
                    return true;
                }
                false
            })
        };
        let drag_release = {
            let svg_ref = svg_ref.clone();
            let drag_release_common = drag_release_common.clone();
            move |event: &MouseEvent| {
                let coords = event_to_svg_coords(
                    event,
                    &svg_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_release_touch = {
            let svg_ref = svg_ref.clone();
            let drag_state = drag_state.clone();
            let drag_release_common = drag_release_common.clone();
            move |event: &TouchEvent| {
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                let coords = touch_event_to_svg_coords(
                    event,
                    &svg_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    true,
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_move = Rc::new(drag_move);
        let drag_move_touch = Rc::new(drag_move_touch);
        let drag_release = Rc::new(drag_release);
        let drag_release_touch = Rc::new(drag_release_touch);
        {
            let mut handlers = drag_handlers.borrow_mut();
            handlers.on_move = Some(drag_move.clone());
            handlers.on_release = Some(drag_release.clone());
            handlers.on_touch_move = Some(drag_move_touch.clone());
            handlers.on_touch_release = Some(drag_release_touch.clone());
        }

        let on_scramble = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let scramble_nonce = scramble_nonce.clone();
            let solved = solved.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let next_nonce = time_nonce(*scramble_nonce);
                scramble_nonce.set(next_nonce);
                let seed = scramble_seed(PUZZLE_SEED, next_nonce, cols, rows);
                let rotation_seed = splitmix64(seed ^ 0xC0DE_F00D);
                let flip_seed = splitmix64(seed ^ 0xF11F_5EED);
                let (next_positions, order) = scramble_layout(
                    seed,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    mask_pad,
                );
                positions.set(next_positions);
                connections.set(vec![[false; 4]; total]);
                z_order.set(order);
                rotations.set(scramble_rotations(
                    rotation_seed,
                    total,
                    rotation_enabled_value,
                ));
                flips.set(scramble_flips(flip_seed, total, FLIP_CHANCE));
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(false);
            })
        };
        let on_solve = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let mut next_positions = Vec::with_capacity(total);
                for row in 0..rows {
                    for col in 0..cols {
                        next_positions.push((
                            col as f64 * piece_width,
                            row as f64 * piece_height,
                        ));
                    }
                }
                let order: Vec<usize> = (0..total).collect();
                positions.set(next_positions);
                connections.set(build_full_connections(cols, rows));
                z_order.set(order);
                rotations.set(vec![0.0; total]);
                flips.set(vec![false; total]);
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(true);
            })
        };
        let on_solve_rotation = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let positions_snapshot = (*positions).clone();
                let flips_snapshot = (*flips).clone();
                let connections_snapshot = (*connections).clone();
                let zeroed = vec![0.0; total];
                rotations.set(zeroed.clone());
                let solved_now = is_solved(
                    &positions_snapshot,
                    &zeroed,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                solved.set(solved_now);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
            })
        };
        let on_unflip = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let positions_snapshot = (*positions).clone();
                let rotations_snapshot = (*rotations).clone();
                let connections_snapshot = (*connections).clone();
                let cleared = vec![false; total];
                flips.set(cleared.clone());
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &cleared,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                solved.set(solved_now);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
            })
        };

        let color_pattern_bg = "#8f5b32";
        let color_pattern_fg1 = "#734423";
        let color_pattern_fg2 = "#3a2418";
        let back_pattern = html! {
            <pattern
                id="piece-back-pattern"
                patternUnits="userSpaceOnUse"
                width="28"
                height="28"
            >
                <rect width="28" height="28" fill={color_pattern_bg} />
                <circle cx="7" cy="7" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="21" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="7" r="1.8" fill={color_pattern_fg2} />
                <circle cx="7" cy="21" r="1.8" fill={color_pattern_fg2} />
            </pattern>
        };

        let mask_defs: Html = piece_shapes
            .iter()
            .map(|(piece, paths)| {
                let mask_id = format!("piece-mask-{}", piece.id);
                html! {
                    <mask
                        id={mask_id}
                        maskUnits="userSpaceOnUse"
                        maskContentUnits="userSpaceOnUse"
                        x={mask_x.clone()}
                        y={mask_y.clone()}
                        width={mask_width.clone()}
                        height={mask_height.clone()}
                        mask-type="luminance"
                    >
                        <rect
                            x={mask_x.clone()}
                            y={mask_y.clone()}
                            width={mask_width.clone()}
                            height={mask_height.clone()}
                            fill="black"
                        />
                        <path d={paths.outline.clone()} fill="white" />
                    </mask>
                }
            })
            .collect();

        let mut nodes = Vec::with_capacity(piece_shapes.len());
        for (piece, paths) in piece_shapes.iter() {
            let piece_x = piece.col as f64 * piece_width;
            let piece_y = piece.row as f64 * piece_height;
            let current =
                positions_value.get(piece.id).copied().unwrap_or((piece_x, piece_y));
            let rotation = rotations_value.get(piece.id).copied().unwrap_or(0.0);
            let flipped = flips_value.get(piece.id).copied().unwrap_or(false);
            let center_x = piece_width * 0.5;
            let center_y = piece_height * 0.5;
            let flip_transform = if flipped {
                format!(
                    " translate({} {}) scale(-1 1) translate(-{} -{})",
                    fmt_f64(center_x),
                    fmt_f64(center_y),
                    fmt_f64(center_x),
                    fmt_f64(center_y)
                )
            } else {
                String::new()
            };
            let transform = format!(
                "translate({} {}){} rotate({} {} {})",
                fmt_f64(current.0),
                fmt_f64(current.1),
                flip_transform,
                fmt_f64(rotation),
                fmt_f64(center_x),
                fmt_f64(center_y)
            );
            let mask_ref = format!("url(#piece-mask-{})", piece.id);
            let img_x = fmt_f64(-piece_x);
            let img_y = fmt_f64(-piece_y);
            let is_dragging = dragging_mask.get(piece.id).copied().unwrap_or(false);
            let is_animating = animating_mask.get(piece.id).copied().unwrap_or(false);
            let is_hovered = hovered_mask.get(piece.id).copied().unwrap_or(false);
            let mut class = if is_dragging {
                if flipped {
                    "piece dragging flipped".to_string()
                } else {
                    "piece dragging".to_string()
                }
            } else if is_animating {
                if flipped {
                    "piece animating flipped".to_string()
                } else {
                    "piece animating".to_string()
                }
            } else if flipped {
                "piece flipped".to_string()
            } else {
                "piece".to_string()
            };
            if is_hovered {
                class.push_str(" hovered");
            }
            let connection = connections_value
                .get(piece.id)
                .copied()
                .unwrap_or([false; 4]);
            let mut external_path = String::new();
            let mut connected_path = String::new();
            for (dir, edge_path) in [
                (DIR_UP, &paths.edges[DIR_UP]),
                (DIR_RIGHT, &paths.edges[DIR_RIGHT]),
                (DIR_DOWN, &paths.edges[DIR_DOWN]),
                (DIR_LEFT, &paths.edges[DIR_LEFT]),
            ] {
                let connected = connection.get(dir).copied().unwrap_or(false);
                let target = if connected {
                    &mut connected_path
                } else {
                    &mut external_path
                };
                if !target.is_empty() {
                    target.push(' ');
                }
                target.push_str(edge_path);
            }
            let external_outline = if external_path.is_empty() {
                html! {}
            } else {
                html! {
                    <g class="piece-outline-group edge-external">
                        <path
                            class="piece-outline piece-outline-light edge-external"
                            d={external_path.clone()}
                        />
                        <path
                            class="piece-outline piece-outline-dark edge-external"
                            d={external_path}
                        />
                    </g>
                }
            };
            let connected_outline = if connected_path.is_empty() {
                html! {}
            } else {
                html! {
                    <g
                        class="piece-outline-group edge-connected"
                        mask={mask_ref.clone()}
                    >
                        <path
                            class="piece-outline piece-outline-light edge-connected"
                            d={connected_path.clone()}
                        />
                        <path
                            class="piece-outline piece-outline-dark edge-connected"
                            d={connected_path}
                        />
                    </g>
                }
            };
            let start_drag: Rc<dyn Fn(f64, f64, bool, bool, Option<i32>)> = {
                let positions = positions.clone();
                let rotations = rotations.clone();
                let drag_state = drag_state.clone();
                let active_id = active_id.clone();
                let dragging_members = dragging_members.clone();
                let animating_members = animating_members.clone();
                let rotation_anim = rotation_anim.clone();
                let rotation_anim_handle = rotation_anim_handle.clone();
                let rotation_queue = rotation_queue.clone();
                let z_order = z_order.clone();
                let connections = connections.clone();
                let piece_id = piece.id;
                let current_pos = current;
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                Rc::new(move |x, y, shift_key, rotate_mode, touch_id| {
                    let positions_snapshot = (*positions).clone();
                    let mut connections_snapshot = (*connections).clone();
                    let mut members = if shift_key {
                        clear_piece_connections(&mut connections_snapshot, piece_id, cols, rows);
                        connections.set(connections_snapshot);
                        vec![piece_id]
                    } else {
                        collect_group(&connections_snapshot, piece_id, cols, rows)
                    };
                    if members.is_empty() {
                        members.push(piece_id);
                    }
                    *rotation_anim.borrow_mut() = None;
                    rotation_anim_handle.borrow_mut().take();
                    rotation_queue.borrow_mut().clear();
                    animating_members.set(Vec::new());
                    dragging_members.set(members.clone());
                    let pos = positions_snapshot
                        .get(piece_id)
                        .copied()
                        .unwrap_or(current_pos);
                    let pivot_x = pos.0 + piece_width * 0.5;
                    let pivot_y = pos.1 + piece_height * 0.5;
                    let start_angle = (y - pivot_y).atan2(x - pivot_x);
                    let mut order = (*z_order).clone();
                    let mut in_group = vec![false; cols * rows];
                    for id in &members {
                        if *id < in_group.len() {
                            in_group[*id] = true;
                        }
                    }
                    let mut group_order = Vec::new();
                    for id in &order {
                        if *id < in_group.len() && in_group[*id] {
                            group_order.push(*id);
                        }
                    }
                    order.retain(|id| !in_group.get(*id).copied().unwrap_or(false));
                    order.extend(group_order);
                    z_order.set(order);
                    let mut start_positions = Vec::with_capacity(members.len());
                    for id in &members {
                        if let Some(start) = positions_snapshot.get(*id) {
                            start_positions.push(*start);
                        } else {
                            start_positions.push(pos);
                        }
                    }
                    let rotations_snapshot = (*rotations).clone();
                    let mut start_rotations = Vec::with_capacity(members.len());
                    for id in &members {
                        let rot = rotations_snapshot.get(*id).copied().unwrap_or(0.0);
                        start_rotations.push(rot);
                    }
                    *drag_state.borrow_mut() = Some(DragState {
                        start_x: x,
                        start_y: y,
                        start_time: Date::now(),
                        primary_id: piece_id,
                        touch_id,
                        rotate_mode,
                        pivot_x,
                        pivot_y,
                        start_angle,
                        members,
                        start_positions,
                        start_rotations,
                    });
                    active_id.set(Some(piece_id));
                })
            };
            let on_piece_down = {
                let svg_ref = svg_ref.clone();
                let start_drag = start_drag.clone();
                Callback::from(move |event: MouseEvent| {
                    if let Some((x, y)) = event_to_svg_coords(
                        &event,
                        &svg_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ) {
                        start_drag(x, y, event.shift_key(), event.ctrl_key(), None);
                    }
                    event.prevent_default();
                })
            };
            let on_piece_touch = {
                let svg_ref = svg_ref.clone();
                let start_drag = start_drag.clone();
                Callback::from(move |event: TouchEvent| {
                    if event.touches().length() > 1 {
                        return;
                    }
                    if let Some(touch) = touch_from_event(&event, None, true) {
                        let touch_id = Some(touch.identifier());
                        if let Some((x, y)) = touch_event_to_svg_coords(
                            &event,
                            &svg_ref,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            touch_id,
                            true,
                        ) {
                            start_drag(x, y, false, false, touch_id);
                        }
                    }
                })
            };
            let on_piece_enter = {
                let hovered_id = hovered_id.clone();
                let piece_id = piece.id;
                Callback::from(move |_| {
                    hovered_id.set(Some(piece_id));
                })
            };
            let on_piece_leave = {
                let hovered_id = hovered_id.clone();
                Callback::from(move |_| {
                    hovered_id.set(None);
                })
            };
            let debug_overlay = if show_debug_value {
                let label = format!(
                    "#{}\nx:{}\ny:{}\nr:{}",
                    piece.id,
                    fmt_f64(current.0),
                    fmt_f64(current.1),
                    fmt_f64(rotation)
                );
                html! {
                    <>
                        <circle
                            class="piece-debug-center"
                            cx={fmt_f64(center_x)}
                            cy={fmt_f64(center_y)}
                            r="3"
                        />
                        <text
                            class="piece-debug-label"
                            x={fmt_f64(center_x)}
                            y={fmt_f64(center_y - 12.0)}
                        >
                            {label
                                .lines()
                                .enumerate()
                                .map(|(idx, line)| {
                                    html! {
                                        <tspan
                                            x={fmt_f64(center_x)}
                                            dy={if idx == 0 { "-20" } else { "20" }}
                                        >
                                            {line}
                                        </tspan>
                                    }
                                })
                                .collect::<Html>()}
                        </text>
                    </>
                }
            } else {
                html! {}
            };
            let node = html! {
                <g
                    key={piece.id.to_string()}
                    class={class}
                    transform={transform}
                    onmousedown={on_piece_down}
                    ontouchstart={on_piece_touch}
                    onmouseenter={on_piece_enter}
                    onmouseleave={on_piece_leave}
                >
                    {external_outline}
                    <rect
                        class="piece-back"
                        x={img_x.clone()}
                        y={img_y.clone()}
                        width={width.to_string()}
                        height={height.to_string()}
                        fill="url(#piece-back-pattern)"
                        mask={mask_ref.clone()}
                    />
                    <image
                        class="piece-image"
                        href={IMAGE_SRC}
                        x={img_x}
                        y={img_y}
                        width={width.to_string()}
                        height={height.to_string()}
                        preserveAspectRatio="xMidYMid meet"
                        mask={mask_ref}
                    />
                    <path class="piece-hitbox" d={paths.outline.clone()} />
                    {connected_outline}
                    {debug_overlay}
                </g>
            };
            nodes.push(node);
        }
        let piece_nodes: Html = if z_order_value.len() == nodes.len() {
            z_order_value
                .iter()
                .filter_map(|id| nodes.get(*id))
                .cloned()
                .collect()
        } else {
            nodes.into_iter().collect()
        };
        let on_context_menu = Callback::from(|event: MouseEvent| {
            event.prevent_default();
        });
        let bounds = html! {
            <>
                <rect
                    class="workspace-bounds"
                    x={fmt_f64(view_min_x)}
                    y={fmt_f64(view_min_y)}
                    width={fmt_f64(view_width)}
                    height={fmt_f64(view_height)}
                />
                <rect
                    class="puzzle-bounds"
                    x="0"
                    y="0"
                    width={width.to_string()}
                    height={height.to_string()}
                    rx={frame_corner_radius.clone()}
                    ry={frame_corner_radius.clone()}
                />
            </>
        };

        let mut svg_class = if show_debug_value {
            "puzzle-image debug".to_string()
        } else {
            "puzzle-image".to_string()
        };
        if !animations_enabled_value {
            svg_class.push_str(" no-anim");
        }
        (
            html! {
                <svg
                    class={svg_class}
                    viewBox={view_box}
                    width={fmt_f64(view_width)}
                    height={fmt_f64(view_height)}
                    preserveAspectRatio="xMidYMid meet"
                    ref={svg_ref}
                    oncontextmenu={on_context_menu}
                >
                    <defs>
                        {back_pattern}
                        {mask_defs}
                    </defs>
                    {bounds}
                    {piece_nodes}
                </svg>
            },
            on_scramble,
            on_solve,
            on_solve_rotation,
            on_unflip,
            false,
        )
    } else {
        (
            html! { <p>{ "Loading puzzle image..." }</p> },
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            true,
        )
    };

    let controls_hint = html! {
        <a
            class="controls-hint"
            href="https://github.com/sugoijan/heddobureika"
            target="_blank"
            rel="noopener noreferrer"
        >
            { "source" }
        </a>
    };
    let preview_class = match *preview_corner {
        PreviewCorner::BottomLeft => "preview-box corner-bl",
        PreviewCorner::BottomRight => "preview-box corner-br",
        PreviewCorner::TopLeft => "preview-box corner-tl",
        PreviewCorner::TopRight => "preview-box corner-tr",
    };
    let on_preview_horizontal = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::BottomRight,
                PreviewCorner::BottomRight => PreviewCorner::BottomLeft,
                PreviewCorner::TopLeft => PreviewCorner::TopRight,
                PreviewCorner::TopRight => PreviewCorner::TopLeft,
            };
            preview_corner.set(next);
        })
    };
    let on_preview_vertical = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::TopLeft,
                PreviewCorner::BottomRight => PreviewCorner::TopRight,
                PreviewCorner::TopLeft => PreviewCorner::BottomLeft,
                PreviewCorner::TopRight => PreviewCorner::BottomRight,
            };
            preview_corner.set(next);
        })
    };
    let preview_arrow_horizontal = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::TopLeft => "preview-arrow preview-arrow-right",
        PreviewCorner::BottomRight | PreviewCorner::TopRight => "preview-arrow preview-arrow-left",
    };
    let preview_arrow_vertical = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::BottomRight => "preview-arrow preview-arrow-up",
        PreviewCorner::TopLeft | PreviewCorner::TopRight => "preview-arrow preview-arrow-down",
    };
    let preview_box = html! {
        <aside class={preview_class}>
            <button
                class={preview_arrow_horizontal}
                type="button"
                aria-label="Move preview horizontally"
                onclick={on_preview_horizontal}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <button
                class={preview_arrow_vertical}
                type="button"
                aria-label="Move preview vertically"
                onclick={on_preview_vertical}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <img src={IMAGE_SRC} alt="preview" />
        </aside>
    };
    let controls_panel = if show_controls_value {
        html! {
            <aside class="controls">
                <h2>{ "Dev Panel" }</h2>
                <p class={status_class}>{ status_label }</p>
                <div class="control">
                    <label>
                        { "Seed" }
                        <span class="control-value">{ seed_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label>
                        { "Expected solve time" }
                        <span class="control-value">{ solve_time_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label for="grid-select">
                        { "Grid" }
                        <span class="control-value">{ grid_label }</span>
                    </label>
                    <select
                        id="grid-select"
                        onchange={on_grid_change}
                    >
                        {grid_options}
                    </select>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_scramble}
                        disabled={scramble_disabled}
                    >
                        { "Scramble" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_solve}
                        disabled={scramble_disabled}
                    >
                        { "Solve" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_solve_rotation}
                        disabled={scramble_disabled}
                    >
                        { "Solve rotation" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_unflip}
                        disabled={scramble_disabled}
                    >
                        { "Unflip all" }
                    </button>
                </div>
                <div class="control">
                    <label for="workspace-scale">
                        { "Workspace scale" }
                        <span class="control-value">{ fmt_f64(workspace_scale_value) }</span>
                    </label>
                    <input
                        id="workspace-scale"
                        type="range"
                        min={WORKSPACE_SCALE_MIN.to_string()}
                        max={WORKSPACE_SCALE_MAX.to_string()}
                        step="0.05"
                        value={workspace_scale_value.to_string()}
                        oninput={on_workspace_scale}
                    />
                </div>
                <div class="control">
                    <label for="animations-enabled">
                        { "Animations: " } { if animations_enabled_value { "On" } else { "Off" } }
                        <input
                            id="animations-enabled"
                            type="checkbox"
                            checked={animations_enabled_value}
                            onchange={on_animations_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="frame-snap">
                        { "Frame snap" }
                        <span class="control-value">{ fmt_f64(frame_snap_ratio_value) }</span>
                    </label>
                    <input
                        id="frame-snap"
                        type="range"
                        min={FRAME_SNAP_MIN.to_string()}
                        max={FRAME_SNAP_MAX.to_string()}
                        step="0.05"
                        value={frame_snap_ratio_value.to_string()}
                        oninput={on_frame_snap}
                    />
                </div>
                <div class="control">
                    <label for="snap-distance">
                        { "Snap distance tol" }
                        <span class="control-value">{ fmt_f64(snap_distance_ratio_value) }</span>
                    </label>
                    <input
                        id="snap-distance"
                        type="range"
                        min={SNAP_DISTANCE_RATIO_MIN.to_string()}
                        max={SNAP_DISTANCE_RATIO_MAX.to_string()}
                        step="0.01"
                        value={snap_distance_ratio_value.to_string()}
                        oninput={on_snap_distance}
                    />
                </div>
                <div class="control">
                    <label for="rotation-snap-tolerance">
                        { "Snap angle tol (deg)" }
                        <span class="control-value">{ fmt_f64(rotation_snap_tolerance_value) }</span>
                    </label>
                    <input
                        id="rotation-snap-tolerance"
                        type="range"
                        min={ROTATION_SNAP_TOLERANCE_MIN_DEG.to_string()}
                        max={ROTATION_SNAP_TOLERANCE_MAX_DEG.to_string()}
                        step="0.5"
                        value={rotation_snap_tolerance_value.to_string()}
                        oninput={on_rotation_snap_tolerance}
                    />
                </div>
                <div class="control">
                    <label for="rotation-lock-threshold">
                        { "Aligned rotate <= " }
                        <span class="control-value">{ rotation_lock_threshold_value }</span>
                    </label>
                    <input
                        id="rotation-lock-threshold"
                        type="range"
                        min={ROTATION_LOCK_THRESHOLD_MIN.to_string()}
                        max={total.max(ROTATION_LOCK_THRESHOLD_MIN).to_string()}
                        step="1"
                        value={rotation_lock_threshold_value.to_string()}
                        oninput={on_rotation_lock_threshold}
                    />
                </div>
                <div class="control">
                    <label for="rotation-enabled">
                        { "Rotation: " } { if rotation_enabled_value { "On" } else { "Off" } }
                        <input
                            id="rotation-enabled"
                            type="checkbox"
                            checked={rotation_enabled_value}
                            onchange={on_rotation_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="rotation-noise">
                        { "Rotation noise" }
                        <span class="control-value">{ fmt_f64(rotation_noise_value) }</span>
                    </label>
                    <input
                        id="rotation-noise"
                        type="range"
                        min={ROTATION_NOISE_MIN.to_string()}
                        max={ROTATION_NOISE_MAX.to_string()}
                        step="0.1"
                        value={rotation_noise_value.to_string()}
                        oninput={on_rotation_noise}
                    />
                </div>
                <div class="control">
                    <label for="debug-enabled">
                        { "Debug overlay: " } { if show_debug_value { "On" } else { "Off" } }
                        <input
                            id="debug-enabled"
                            type="checkbox"
                            checked={show_debug_value}
                            onchange={on_debug_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="tab-width">
                        { "Tab size" }
                        <span class="control-value">{ fmt_f64(settings_value.tab_width) }</span>
                    </label>
                    <input
                        id="tab-width"
                        type="range"
                        min={TAB_WIDTH_MIN.to_string()}
                        max={TAB_WIDTH_MAX.to_string()}
                        step="0.005"
                        value={settings_value.tab_width.to_string()}
                        oninput={tab_width_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-depth">
                        { "Tab depth" }
                        <span class="control-value">{ fmt_f64(settings_value.tab_depth) }</span>
                    </label>
                    <input
                        id="tab-depth"
                        type="range"
                        min={TAB_DEPTH_MIN.to_string()}
                        max={TAB_DEPTH_MAX.to_string()}
                        step="0.01"
                        value={settings_value.tab_depth.to_string()}
                        oninput={tab_depth_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-scale">
                        { "Tab size scale" }
                        <span class="control-value">
                            { fmt_f64(settings_value.tab_size_scale) }
                        </span>
                    </label>
                    <input
                        id="tab-size-scale"
                        type="range"
                        min={TAB_SIZE_SCALE_MIN.to_string()}
                        max={TAB_SIZE_SCALE_MAX.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_scale.to_string()}
                        oninput={tab_size_scale_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-min">
                        { "Tab size min" }
                        <span class="control-value">{ fmt_f64(settings_value.tab_size_min) }</span>
                    </label>
                    <input
                        id="tab-size-min"
                        type="range"
                        min={TAB_SIZE_MIN_LIMIT.to_string()}
                        max={settings_value.tab_size_max.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_min.to_string()}
                        oninput={tab_size_min_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-max">
                        { "Tab size max" }
                        <span class="control-value">{ fmt_f64(settings_value.tab_size_max) }</span>
                    </label>
                    <input
                        id="tab-size-max"
                        type="range"
                        min={settings_value.tab_size_min.to_string()}
                        max={TAB_SIZE_MAX_LIMIT.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_max.to_string()}
                        oninput={tab_size_max_input}
                    />
                </div>
                <div class="control">
                    <label for="skew-range">
                        { "Center skew" }
                        <span class="control-value">{ fmt_f64(settings_value.skew_range) }</span>
                    </label>
                    <input
                        id="skew-range"
                        type="range"
                        min="0.0"
                        max={SKEW_RANGE_MAX.to_string()}
                        step="0.005"
                        value={settings_value.skew_range.to_string()}
                        oninput={skew_input}
                    />
                </div>
                <div class="control">
                    <label for="variation">
                        { "Variation" }
                        <span class="control-value">{ fmt_f64(settings_value.variation) }</span>
                    </label>
                    <input
                        id="variation"
                        type="range"
                        min={VARIATION_MIN.to_string()}
                        max={VARIATION_MAX.to_string()}
                        step="0.01"
                        value={settings_value.variation.to_string()}
                        oninput={variation_input}
                    />
                </div>
                <div class="control">
                    <label for="jitter-strength">
                        { "Jitter strength" }
                        <span class="control-value">
                            { fmt_f64(settings_value.jitter_strength) }
                        </span>
                    </label>
                    <input
                        id="jitter-strength"
                        type="range"
                        min={JITTER_STRENGTH_MIN.to_string()}
                        max={JITTER_STRENGTH_MAX.to_string()}
                        step="0.005"
                        value={settings_value.jitter_strength.to_string()}
                        oninput={jitter_strength_input}
                    />
                </div>
                <div class="control">
                    <label for="jitter-len-bias">
                        { "Length jitter bias" }
                        <span class="control-value">
                            { fmt_f64(settings_value.jitter_len_bias) }
                        </span>
                    </label>
                    <input
                        id="jitter-len-bias"
                        type="range"
                        min={JITTER_LEN_BIAS_MIN.to_string()}
                        max={JITTER_LEN_BIAS_MAX.to_string()}
                        step="0.01"
                        value={settings_value.jitter_len_bias.to_string()}
                        oninput={jitter_len_bias_input}
                    />
                </div>
                <div class="control">
                    <label for="line-bend">
                        { "Grid bend" }
                        <span class="control-value">{ fmt_f64(settings_value.line_bend_ratio) }</span>
                    </label>
                    <input
                        id="line-bend"
                        type="range"
                        min={LINE_BEND_MIN.to_string()}
                        max={MAX_LINE_BEND_RATIO.to_string()}
                        step="0.01"
                        value={settings_value.line_bend_ratio.to_string()}
                        oninput={line_bend_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-depth-cap">
                        { "Tab depth cap" }
                        <span class="control-value">
                            { fmt_f64(settings_value.tab_depth_cap) }
                        </span>
                    </label>
                    <input
                        id="tab-depth-cap"
                        type="range"
                        min={TAB_DEPTH_CAP_MIN.to_string()}
                        max={TAB_DEPTH_CAP_MAX.to_string()}
                        step="0.01"
                        value={settings_value.tab_depth_cap.to_string()}
                        oninput={tab_depth_cap_input}
                    />
                </div>
                <div class="control">
                    <label for="curve-detail">
                        { "Curve detail" }
                        <span class="control-value">
                            { fmt_f64(settings_value.curve_detail) }
                        </span>
                    </label>
                    <input
                        id="curve-detail"
                        type="range"
                        min={CURVE_DETAIL_MIN.to_string()}
                        max={CURVE_DETAIL_MAX.to_string()}
                        step="0.05"
                        value={settings_value.curve_detail.to_string()}
                        oninput={curve_detail_input}
                    />
                </div>
            </aside>
        }
    } else {
        html! {}
    };
    html! {
        <main class="app">
            {content}
            {solved_banner}
            {controls_hint}
            {preview_box}
            {controls_panel}
        </main>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    fn assert_close(actual: f64, expected: f64) {
        let delta = (actual - expected).abs();
        assert!(
            delta <= 1e-6,
            "expected {:.6} got {:.6} (delta {:.6})",
            expected,
            actual,
            delta
        );
    }

    #[wasm_bindgen_test]
    fn align_group_sets_uniform_rotation() {
        let cols = 3usize;
        let piece_width = 100.0;
        let piece_height = 100.0;
        let mut positions = vec![(0.0, 0.0); 4];
        let mut rotations = vec![0.0; 4];
        rotations[0] = 11.027;
        rotations[1] = 359.504;
        positions[0] = (12.0, -44.0);
        positions[1] = (180.0, 36.0);
        let members = vec![0usize, 1usize];
        let anchor_id = 0usize;
        let anchor_center = (50.0, 50.0);
        let target_rot = 11.027;

        align_group_to_anchor(
            &mut positions,
            &mut rotations,
            &members,
            anchor_id,
            anchor_center,
            target_rot,
            cols,
            piece_width,
            piece_height,
        );

        for id in &members {
            assert_close(rotations[*id], normalize_angle(target_rot));
        }

        let base_rotation = rotations[members[0]];
        for id in &members[1..] {
            assert_close(rotations[*id], base_rotation);
        }

        for id in 0..rotations.len() {
            if !members.contains(&id) {
                assert_close(rotations[id], 0.0);
            }
        }

        let (dx, dy) = rotate_vec(piece_width, 0.0, normalize_angle(target_rot));
        let expected_center = (anchor_center.0 + dx, anchor_center.1 + dy);
        let pos = positions[1];
        let center = (pos.0 + piece_width * 0.5, pos.1 + piece_height * 0.5);
        assert_close(center.0, expected_center.0);
        assert_close(center.1, expected_center.1);
    }
}
