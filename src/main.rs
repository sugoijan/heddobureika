use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use js_sys::Date;
use std::collections::VecDeque;
use std::fmt::Write;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::{
    Element, Event, HtmlImageElement, HtmlInputElement, HtmlSelectElement, InputEvent, KeyboardEvent,
};
use yew::prelude::*;

const IMAGE_SRC: &str = "puzzles/zoe-potter.jpg";
const PUZZLE_SEED: u64 = 0x5EED_5EED_25_20;
const MAX_TAB_DEPTH_RATIO: f64 = 0.32;
const MAX_LINE_BEND_RATIO: f64 = 0.2;
const SNAP_DISTANCE_RATIO: f64 = 0.18;
const SOLVE_TOLERANCE_RATIO: f64 = 0.08;
const ROTATION_STEP_DEG: f64 = 90.0;
const ROTATION_SNAP_TOLERANCE_DEG: f64 = 3.0;
const ROTATION_SOLVE_TOLERANCE_DEG: f64 = 1.5;
const CLICK_MOVE_RATIO: f64 = 0.08;
const CLICK_MAX_DURATION_MS: f64 = 240.0;
const RUBBER_BAND_RATIO: f64 = 0.35;
const WORKSPACE_SCALE_MIN: f64 = 1.0;
const WORKSPACE_SCALE_MAX: f64 = 2.5;
const WORKSPACE_SCALE_DEFAULT: f64 = 1.6;
const FRAME_SNAP_MIN: f64 = 0.4;
const FRAME_SNAP_MAX: f64 = 3.0;
const FRAME_SNAP_DEFAULT: f64 = 1.0;
const TAB_WIDTH_MIN: f64 = 0.2;
const TAB_WIDTH_MAX: f64 = 0.72;
const TAB_WIDTH_RANGE: f64 = 0.16;
const TAB_DEPTH_MIN: f64 = 0.2;
const TAB_DEPTH_MAX: f64 = 1.1;
const TAB_DEPTH_RANGE: f64 = 0.35;
const SHOULDER_SCALE: f64 = 0.78;
const DP_RATIO_MIN: f64 = 0.1;
const DP_RATIO_MAX: f64 = 0.7;
const DP_RATIO_RANGE: f64 = 0.16;
const NECK_WEIGHT_MIN: f64 = 6.0;
const NECK_WEIGHT_MAX: f64 = 26.0;
const NECK_WEIGHT_RANGE: f64 = 8.0;
const MID_WEIGHT_MIN: f64 = 6.0;
const MID_WEIGHT_MAX: f64 = 28.0;
const MID_WEIGHT_RANGE: f64 = 8.0;
const KNOB_WEIGHT_MIN: f64 = 16.0;
const KNOB_WEIGHT_MAX: f64 = 70.0;
const KNOB_WEIGHT_RANGE: f64 = 18.0;
const SKEW_RANGE_MAX: f64 = 0.2;
const VARIATION_MIN: f64 = 0.0;
const VARIATION_MAX: f64 = 1.0;
const LINE_BEND_MIN: f64 = 0.0;
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
    tab_width: f64,
    tab_depth: f64,
    shoulder: f64,
    skew: f64,
    dp_ratio: f64,
    neck_weight: f64,
    mid_weight: f64,
    knob_weight: f64,
}

#[derive(Clone, Copy, Debug)]
struct Edge {
    tab_dir: i8,
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
    members: Vec<usize>,
    start_positions: Vec<(f64, f64)>,
}

#[derive(Default)]
struct DragHandlers {
    on_move: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_release: Option<Rc<dyn Fn(&MouseEvent)>>,
}

#[derive(Clone, PartialEq)]
struct ShapeSettings {
    tab_width: f64,
    tab_depth: f64,
    dp_ratio: f64,
    skew_range: f64,
    neck_weight: f64,
    mid_weight: f64,
    knob_weight: f64,
    variation: f64,
    line_bend_ratio: f64,
}

impl Default for ShapeSettings {
    fn default() -> Self {
        Self {
            tab_width: 0.6,
            tab_depth: 0.43,
            dp_ratio: 0.5,
            skew_range: 0.05,
            neck_weight: 8.0,
            mid_weight: 8.5,
            knob_weight: 42.0,
            variation: 0.05,
            line_bend_ratio: 0.06,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct GridPreset {
    label: &'static str,
    cols: u32,
    rows: u32,
}

const GRID_PRESETS: [GridPreset; 5] = [
    GridPreset {
        label: "10 x 8",
        cols: 10,
        rows: 8,
    },
    GridPreset {
        label: "15 x 12",
        cols: 15,
        rows: 12,
    },
    GridPreset {
        label: "20 x 16",
        cols: 20,
        rows: 16,
    },
    GridPreset {
        label: "25 x 20",
        cols: 25,
        rows: 20,
    },
    GridPreset {
        label: "30 x 24",
        cols: 30,
        rows: 24,
    },
];

const DEFAULT_GRID_INDEX: usize = 0;

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

fn is_solved(
    positions: &[(f64, f64)],
    rotations: &[f64],
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

fn angle_matches(a: f64, b: f64) -> bool {
    angle_delta(a, b).abs() <= ROTATION_SNAP_TOLERANCE_DEG
}

fn next_snap_rotation(angle: f64) -> f64 {
    let next = (angle / ROTATION_STEP_DEG).floor() + 1.0;
    normalize_angle(next * ROTATION_STEP_DEG)
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

fn edge_seed(base: u64, orientation: u64, row: u32, col: u32) -> u64 {
    base ^ (orientation << 56) ^ ((row as u64) << 28) ^ (col as u64)
}

fn edge_from_seed(seed: u64, settings: &ShapeSettings) -> Edge {
    let variation = settings
        .variation
        .clamp(VARIATION_MIN, VARIATION_MAX);
    let tab_width = jitter_value(
        seed,
        0,
        settings.tab_width,
        TAB_WIDTH_RANGE,
        TAB_WIDTH_MIN,
        TAB_WIDTH_MAX,
        variation,
    );
    let tab_depth = jitter_value(
        seed,
        2,
        settings.tab_depth,
        TAB_DEPTH_RANGE,
        TAB_DEPTH_MIN,
        TAB_DEPTH_MAX,
        variation,
    );
    let dp_ratio = jitter_value(
        seed,
        3,
        settings.dp_ratio,
        DP_RATIO_RANGE,
        DP_RATIO_MIN,
        DP_RATIO_MAX,
        variation,
    );
    let neck_weight = jitter_value(
        seed,
        4,
        settings.neck_weight,
        NECK_WEIGHT_RANGE,
        NECK_WEIGHT_MIN,
        NECK_WEIGHT_MAX,
        variation,
    );
    let mid_weight = jitter_value(
        seed,
        5,
        settings.mid_weight,
        MID_WEIGHT_RANGE,
        MID_WEIGHT_MIN,
        MID_WEIGHT_MAX,
        variation,
    );
    let knob_weight = jitter_value(
        seed,
        6,
        settings.knob_weight,
        KNOB_WEIGHT_RANGE,
        KNOB_WEIGHT_MIN,
        KNOB_WEIGHT_MAX,
        variation,
    );
    let skew_range = settings.skew_range.clamp(0.0, SKEW_RANGE_MAX);
    let skew = (rand_unit(seed, 7) * 2.0 - 1.0) * skew_range;
    let tab_dir = if rand_unit(seed, 8) < 0.5 { 1 } else { -1 };

    let shoulder = ((1.0 - tab_width) * 0.5) * SHOULDER_SCALE;

    Edge {
        tab_dir,
        params: EdgeParams {
            tab_width,
            tab_depth,
            shoulder,
            skew,
            dp_ratio,
            neck_weight,
            mid_weight,
            knob_weight,
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

fn edge_segments(
    len: f64,
    depth_base: f64,
    edge: Option<&Edge>,
    sign: i8,
) -> Vec<Segment> {
    let edge = match edge {
        Some(edge) if sign != 0 => edge,
        _ => return vec![Segment::LineTo { x: len, y: 0.0 }],
    };

    let params = edge.params;
    let tab_width = len * params.tab_width;
    let depth_limit = depth_base * MAX_TAB_DEPTH_RATIO;
    let mut depth = tab_width * params.tab_depth * sign as f64;
    if depth > depth_limit {
        depth = depth_limit;
    }
    if depth < -depth_limit {
        depth = -depth_limit;
    }
    let shoulder = len * params.shoulder;

    let mut center = len * 0.5 + len * params.skew;
    let min_center = shoulder + tab_width / 2.0;
    let max_center = len - shoulder - tab_width / 2.0;
    if center < min_center {
        center = min_center;
    }
    if center > max_center {
        center = max_center;
    }

    let left = center - tab_width / 2.0;
    let right = center + tab_width / 2.0;
    let pm = center;
    let dp = tab_width * params.dp_ratio;

    let control_points = [
        (left, 0.0),
        (pm, 0.0),
        (pm - dp, depth),
        (pm, depth * 1.15),
        (pm + dp, depth),
        (pm, 0.0),
        (right, 0.0),
    ];
    let weights = [
        1.0,
        params.neck_weight,
        params.mid_weight,
        params.knob_weight,
        params.mid_weight,
        params.neck_weight,
        1.0,
    ];
    let spline_points = bspline_rational_points(&control_points, &weights, 3, 24);

    let mut segments = Vec::with_capacity(2 + spline_points.len());
    if left > 0.0 {
        segments.push(Segment::LineTo { x: left, y: 0.0 });
    }
    for (x, y) in spline_points.into_iter().skip(1) {
        segments.push(Segment::LineTo { x, y });
    }
    if right < len {
        segments.push(Segment::LineTo { x: len, y: 0.0 });
    }
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

fn build_knot_vector(control_len: usize, degree: usize) -> Vec<f64> {
    let n = control_len.saturating_sub(1);
    let p = degree;
    let mut knots = vec![0.0; n + p + 2];
    let max_value = (n - p + 1) as f64;
    for i in 0..knots.len() {
        if i <= p {
            knots[i] = 0.0;
        } else if i >= n + 1 {
            knots[i] = max_value;
        } else {
            knots[i] = (i - p) as f64;
        }
    }
    knots
}

fn find_span(u: f64, degree: usize, knots: &[f64], control_len: usize) -> usize {
    let n = control_len.saturating_sub(1);
    if u >= knots[n + 1] {
        return n;
    }
    if u <= knots[degree] {
        return degree;
    }
    let mut low = degree;
    let mut high = n + 1;
    let mut mid = (low + high) / 2;
    while u < knots[mid] || u >= knots[mid + 1] {
        if u < knots[mid] {
            high = mid;
        } else {
            low = mid;
        }
        mid = (low + high) / 2;
    }
    mid
}

fn basis_funs(span: usize, u: f64, degree: usize, knots: &[f64]) -> Vec<f64> {
    let mut left = vec![0.0; degree + 1];
    let mut right = vec![0.0; degree + 1];
    let mut n = vec![0.0; degree + 1];
    n[0] = 1.0;
    for j in 1..=degree {
        left[j] = u - knots[span + 1 - j];
        right[j] = knots[span + j] - u;
        let mut saved = 0.0;
        for r in 0..j {
            let denom = right[r + 1] + left[j - r];
            let temp = if denom == 0.0 { 0.0 } else { n[r] / denom };
            n[r] = saved + right[r + 1] * temp;
            saved = left[j - r] * temp;
        }
        n[j] = saved;
    }
    n
}

fn bspline_rational_points(
    control_points: &[(f64, f64)],
    weights: &[f64],
    degree: usize,
    samples: usize,
) -> Vec<(f64, f64)> {
    let control_len = control_points.len();
    let knots = build_knot_vector(control_len, degree);
    let max_u = knots[control_len];
    let mut points = Vec::with_capacity(samples + 1);

    for i in 0..=samples {
        let u = max_u * (i as f64) / (samples as f64);
        let span = find_span(u, degree, &knots, control_len);
        let basis = basis_funs(span, u, degree, &knots);
        let mut denom = 0.0;
        let mut x = 0.0;
        let mut y = 0.0;
        for (j, coeff) in basis.iter().enumerate() {
            let idx = span - degree + j;
            let weight = weights[idx];
            let influence = coeff * weight;
            denom += influence;
            x += influence * control_points[idx].0;
            y += influence * control_points[idx].1;
        }
        if denom != 0.0 {
            x /= denom;
            y /= denom;
        }
        points.push((x, y));
    }
    points
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

fn build_piece_path(
    piece: &Piece,
    piece_width: f64,
    piece_height: f64,
    horizontal: &[Vec<Option<Edge>>],
    vertical: &[Vec<Option<Edge>>],
    warp: &WarpField<'_>,
) -> String {
    let row = piece.row as usize;
    let col = piece.col as usize;
    let piece_x = piece.col as f64 * piece_width;
    let piece_y = piece.row as f64 * piece_height;

    let top_edge = horizontal[row][col].as_ref();
    let bottom_edge = horizontal[row + 1][col].as_ref();
    let left_edge = vertical[row][col].as_ref();
    let right_edge = vertical[row][col + 1].as_ref();

    let top_sign = top_edge.map(|edge| -edge.tab_dir).unwrap_or(0);
    let right_sign = right_edge.map(|edge| edge.tab_dir).unwrap_or(0);
    let bottom_sign = bottom_edge.map(|edge| edge.tab_dir).unwrap_or(0);
    let left_sign = left_edge.map(|edge| -edge.tab_dir).unwrap_or(0);

    let top_segments = edge_segments(piece_width, piece_height, top_edge, top_sign);
    let right_segments = edge_segments(piece_height, piece_width, right_edge, right_sign);
    let bottom_segments =
        reverse_segments(&edge_segments(piece_width, piece_height, bottom_edge, bottom_sign));
    let left_segments =
        reverse_segments(&edge_segments(piece_height, piece_width, left_edge, left_sign));

    let offset = (piece_x, piece_y);
    let top_step = (piece_width / 10.0).max(4.0);
    let side_step = (piece_height / 10.0).max(4.0);

    let mut path = String::new();
    let (start_x, start_y) = warp_point(piece_x, piece_y, warp);
    let _ = write!(
        path,
        "M {} {}",
        fmt_f64(start_x - piece_x),
        fmt_f64(start_y - piece_y)
    );
    append_segments(
        &mut path,
        &top_segments,
        EdgeOrientation::Top,
        (0.0, 0.0),
        offset,
        warp,
        (0.0, 0.0),
        top_step,
    );
    append_segments(
        &mut path,
        &right_segments,
        EdgeOrientation::Right,
        (piece_width, 0.0),
        offset,
        warp,
        (0.0, 0.0),
        side_step,
    );
    append_segments(
        &mut path,
        &bottom_segments,
        EdgeOrientation::Bottom,
        (0.0, piece_height),
        offset,
        warp,
        (piece_width, 0.0),
        top_step,
    );
    append_segments(
        &mut path,
        &left_segments,
        EdgeOrientation::Left,
        (0.0, 0.0),
        offset,
        warp,
        (piece_height, 0.0),
        side_step,
    );
    path.push_str(" Z");
    path
}

#[function_component(App)]
fn app() -> Html {
    let image_size = use_state(|| None::<(u32, u32)>);
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let grid_index = use_state(|| DEFAULT_GRID_INDEX);
    let grid_index_value = *grid_index;
    let grid = GRID_PRESETS[grid_index_value];
    let grid_label = format!(
        "{} ({} pieces)",
        grid.label,
        grid.cols * grid.rows
    );
    let grid_options: Html = GRID_PRESETS
        .iter()
        .enumerate()
        .map(|(index, preset)| {
            let label = format!("{} ({} pieces)", preset.label, preset.cols * preset.rows);
            html! { <option value={index.to_string()}>{label}</option> }
        })
        .collect();

    let tab_width_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_width = value.clamp(TAB_WIDTH_MIN, TAB_WIDTH_MAX);
    });
    let tab_depth_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_depth = value.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
    });
    let dp_ratio_input = on_setting_change(settings.clone(), |settings, value| {
        settings.dp_ratio = value.clamp(DP_RATIO_MIN, DP_RATIO_MAX);
    });
    let skew_input = on_setting_change(settings.clone(), |settings, value| {
        settings.skew_range = value.clamp(0.0, SKEW_RANGE_MAX);
    });
    let neck_input = on_setting_change(settings.clone(), |settings, value| {
        settings.neck_weight = value.clamp(NECK_WEIGHT_MIN, NECK_WEIGHT_MAX);
    });
    let mid_input = on_setting_change(settings.clone(), |settings, value| {
        settings.mid_weight = value.clamp(MID_WEIGHT_MIN, MID_WEIGHT_MAX);
    });
    let knob_input = on_setting_change(settings.clone(), |settings, value| {
        settings.knob_weight = value.clamp(KNOB_WEIGHT_MIN, KNOB_WEIGHT_MAX);
    });
    let variation_input = on_setting_change(settings.clone(), |settings, value| {
        settings.variation = value.clamp(VARIATION_MIN, VARIATION_MAX);
    });
    let line_bend_input = on_setting_change(settings.clone(), |settings, value| {
        settings.line_bend_ratio = value.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
    });
    let positions = use_state(Vec::<(f64, f64)>::new);
    let active_id = use_state(|| None::<usize>);
    let drag_state = use_mut_ref(|| None::<DragState>);
    let drag_handlers = use_mut_ref(DragHandlers::default);
    let svg_ref = use_node_ref();
    let workspace_scale = use_state(|| WORKSPACE_SCALE_DEFAULT);
    let workspace_scale_value = *workspace_scale;
    let z_order = use_state(Vec::<usize>::new);
    let connections = use_state(Vec::<[bool; 4]>::new);
    let rotations = use_state(Vec::<f64>::new);
    let rotation_enabled = use_state(|| true);
    let rotation_enabled_value = *rotation_enabled;
    let scramble_nonce = use_state(|| 0u64);
    let scramble_nonce_value = *scramble_nonce;
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(|| false);
    let show_controls_value = *show_controls;
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

    let image_size_value = *image_size;
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
        let z_order = z_order.clone();
        let connections = connections.clone();
        let rotations = rotations.clone();
        let solved = solved.clone();
        let scramble_nonce = scramble_nonce.clone();
        use_effect_with(
            (grid_index_value, image_size_value),
            move |(grid_index_value, image_size_value)| {
                if let Some((width, height)) = *image_size_value {
                    let grid = GRID_PRESETS[*grid_index_value];
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let piece_width = width as f64 / grid.cols as f64;
                    let piece_height = height as f64 / grid.rows as f64;
                    let view_width = width as f64 * workspace_scale_value;
                    let view_height = height as f64 * workspace_scale_value;
                    let view_min_x = (width as f64 - view_width) * 0.5;
                    let view_min_y = (height as f64 - view_height) * 0.5;
                    let margin =
                        piece_width.max(piece_height) * (MAX_TAB_DEPTH_RATIO + MAX_LINE_BEND_RATIO);
                    let nonce = time_nonce(*scramble_nonce);
                    let seed = scramble_seed(PUZZLE_SEED, nonce, cols, rows);
                    let rotation_seed = splitmix64(seed ^ 0xC0DE_F00D);
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
                    *drag_state.borrow_mut() = None;
                    z_order.set(order);
                    connections.set(vec![[false; 4]; cols * rows]);
                    rotations.set(scramble_rotations(
                        rotation_seed,
                        cols * rows,
                        rotation_enabled_value,
                    ));
                    solved.set(false);
                    scramble_nonce.set(nonce);
                }
                || ()
            },
        );
    }

    let on_grid_change = {
        let grid_index = grid_index.clone();
        Callback::from(move |event: Event| {
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < GRID_PRESETS.len() {
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
    let on_rotation_toggle = {
        let rotation_enabled = rotation_enabled.clone();
        let rotations = rotations.clone();
        let positions = positions.clone();
        let image_size = image_size.clone();
        let grid_index = grid_index.clone();
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
                let grid = GRID_PRESETS[*grid_index];
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let piece_width = width as f64 / grid.cols as f64;
                let piece_height = height as f64 / grid.rows as f64;
                let positions_snapshot = (*positions).clone();
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
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
                || {
                    drop(move_listener);
                    drop(up_listener);
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

    let (content, on_scramble, on_solve, on_solve_rotation, scramble_disabled) =
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
        let max_depth = piece_width.max(piece_height) * MAX_TAB_DEPTH_RATIO;
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
        let piece_shapes: Vec<(Piece, String)> = pieces
            .iter()
            .map(|piece| {
                let path_d =
                    build_piece_path(piece, piece_width, piece_height, &horizontal, &vertical, &warp_field);
                (*piece, path_d)
            })
            .collect();

        let positions_value = (*positions).clone();
        let rotations_value = (*rotations).clone();
        let active_id_value = *active_id;
        let z_order_value = (*z_order).clone();
        let drag_move = {
            let positions = positions.clone();
            let drag_state = drag_state.clone();
            let svg_ref = svg_ref.clone();
            move |event: &MouseEvent| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    if let Some((x, y)) = event_to_svg_coords(
                        event,
                        &svg_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ) {
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
                            let rubber_limit =
                                piece_width.min(piece_height) * RUBBER_BAND_RATIO;
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
                    }
                    event.prevent_default();
                }
            }
        };
        let drag_release = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let connections = connections.clone();
            let solved = solved.clone();
            let svg_ref = svg_ref.clone();
            move |event: &MouseEvent| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    let mut next = (*positions).clone();
                    let mut next_rotations = (*rotations).clone();
                    let mut next_connections = (*connections).clone();
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let snap_distance = piece_width.min(piece_height) * SNAP_DISTANCE_RATIO;
                    let click_tolerance =
                        piece_width.min(piece_height) * CLICK_MOVE_RATIO;
                    if rotation_enabled_value {
                        if let Some((x, y)) = event_to_svg_coords(
                            event,
                            &svg_ref,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                        ) {
                            let dx = x - drag.start_x;
                            let dy = y - drag.start_y;
                            let dist = (dx * dx + dy * dy).sqrt();
                            let elapsed = Date::now() - drag.start_time;
                            if dist <= click_tolerance && elapsed <= CLICK_MAX_DURATION_MS {
                                if !drag.members.is_empty() {
                                    let pivot_x = drag.start_x;
                                    let pivot_y = drag.start_y;
                                    let current_positions = next.clone();
                                    let current_angle = drag
                                        .members
                                        .first()
                                        .and_then(|id| next_rotations.get(*id))
                                        .copied()
                                        .unwrap_or(0.0);
                                    let target_angle = next_snap_rotation(current_angle);
                                    let delta = angle_delta(target_angle, current_angle);
                                    for member in &drag.members {
                                        if let Some(pos) = current_positions.get(*member) {
                                            let center_x = pos.0 + piece_width * 0.5;
                                            let center_y = pos.1 + piece_height * 0.5;
                                            let (rx, ry) = rotate_point(
                                                center_x,
                                                center_y,
                                                pivot_x,
                                                pivot_y,
                                                delta,
                                            );
                                            if let Some(pos) = next.get_mut(*member) {
                                                *pos = (
                                                    rx - piece_width * 0.5,
                                                    ry - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot) = next_rotations.get_mut(*member) {
                                                *rot = normalize_angle(*rot + delta);
                                            }
                                        }
                                    }
                                    let solved_now = is_solved(
                                        &next,
                                        &next_rotations,
                                        cols,
                                            rows,
                                            piece_width,
                                            piece_height,
                                            rotation_enabled_value,
                                        );
                                        positions.set(next);
                                        rotations.set(next_rotations);
                                        solved.set(solved_now);
                                        active_id.set(None);
                                        *drag_state.borrow_mut() = None;
                                        event.prevent_default();
                                        return;
                                }
                            }
                        }
                    }
                    let mut in_group = vec![false; cols * rows];
                    for member in &drag.members {
                        if *member < in_group.len() {
                            in_group[*member] = true;
                        }
                    }

                    let mut best: Option<(f64, f64, f64, usize, usize)> = None;
                    for member in &drag.members {
                        if *member >= next.len() {
                            continue;
                        }
                        let current = next[*member];
                        let center_a = (
                            current.0 + piece_width * 0.5,
                            current.1 + piece_height * 0.5,
                        );
                        let rot_a = next_rotations.get(*member).copied().unwrap_or(0.0);
                        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
                            if let Some(neighbor) = neighbor_id(*member, cols, rows, dir) {
                                if in_group[neighbor] {
                                    continue;
                                }
                                let rot_b = next_rotations.get(neighbor).copied().unwrap_or(0.0);
                                if !angle_matches(rot_a, rot_b) {
                                    continue;
                                }
                                let base = match dir {
                                    DIR_LEFT => (-piece_width, 0.0),
                                    DIR_RIGHT => (piece_width, 0.0),
                                    DIR_UP => (0.0, -piece_height),
                                    DIR_DOWN => (0.0, piece_height),
                                    _ => (0.0, 0.0),
                                };
                                let (vx, vy) = rotate_vec(base.0, base.1, rot_a);
                                let neighbor_pos = next[neighbor];
                                let center_b = (
                                    neighbor_pos.0 + piece_width * 0.5,
                                    neighbor_pos.1 + piece_height * 0.5,
                                );
                                let actual = (center_b.0 - center_a.0, center_b.1 - center_a.1);
                                let dx = actual.0 - vx;
                                let dy = actual.1 - vy;
                                let dist = (dx * dx + dy * dy).sqrt();
                                if dist <= snap_distance {
                                    match best {
                                        Some((best_dist, _, _, _, _)) if dist >= best_dist => {}
                                        _ => best = Some((dist, dx, dy, *member, dir)),
                                    }
                                }
                            }
                        }
                    }

                    if let Some((_dist, dx, dy, member, dir)) = best {
                        let mut min_x = f64::INFINITY;
                        let mut max_x = f64::NEG_INFINITY;
                        let mut min_y = f64::INFINITY;
                        let mut max_y = f64::NEG_INFINITY;
                        for id in &drag.members {
                            if let Some(pos) = next.get(*id) {
                                let center_x = pos.0 + piece_width * 0.5;
                                let center_y = pos.1 + piece_height * 0.5;
                                min_x = min_x.min(center_x);
                                max_x = max_x.max(center_x);
                                min_y = min_y.min(center_y);
                                max_y = max_y.max(center_y);
                            }
                        }
                        let can_snap = min_x.is_finite()
                            && min_y.is_finite()
                            && min_x + dx >= center_min_x
                            && max_x + dx <= center_max_x
                            && min_y + dy >= center_min_y
                            && max_y + dy <= center_max_y;
                        if can_snap {
                            for id in &drag.members {
                                if let Some(pos) = next.get_mut(*id) {
                                    *pos = (pos.0 + dx, pos.1 + dy);
                                }
                            }
                            set_connection(
                                &mut next_connections,
                                member,
                                dir,
                                true,
                                cols,
                                rows,
                            );
                        }
                    }

                    for member in &drag.members {
                        if *member >= next.len() {
                            continue;
                        }
                        let current = next[*member];
                        let center_a = (
                            current.0 + piece_width * 0.5,
                            current.1 + piece_height * 0.5,
                        );
                        let rot_a = next_rotations.get(*member).copied().unwrap_or(0.0);
                        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
                            if let Some(neighbor) = neighbor_id(*member, cols, rows, dir) {
                                if in_group[neighbor] {
                                    continue;
                                }
                                let rot_b = next_rotations.get(neighbor).copied().unwrap_or(0.0);
                                if !angle_matches(rot_a, rot_b) {
                                    continue;
                                }
                                let base = match dir {
                                    DIR_LEFT => (-piece_width, 0.0),
                                    DIR_RIGHT => (piece_width, 0.0),
                                    DIR_UP => (0.0, -piece_height),
                                    DIR_DOWN => (0.0, piece_height),
                                    _ => (0.0, 0.0),
                                };
                                let (vx, vy) = rotate_vec(base.0, base.1, rot_a);
                                let neighbor_pos = next[neighbor];
                                let center_b = (
                                    neighbor_pos.0 + piece_width * 0.5,
                                    neighbor_pos.1 + piece_height * 0.5,
                                );
                                let actual = (center_b.0 - center_a.0, center_b.1 - center_a.1);
                                let dx = actual.0 - vx;
                                let dy = actual.1 - vy;
                                let dist = (dx * dx + dy * dy).sqrt();
                                if dist <= snap_distance {
                                    set_connection(
                                        &mut next_connections,
                                        *member,
                                        dir,
                                        true,
                                        cols,
                                        rows,
                                    );
                                }
                            }
                        }
                    }

                    let group_after = drag
                        .members
                        .first()
                        .map(|id| collect_group(&next_connections, *id, cols, rows))
                        .unwrap_or_default();
                    if !group_after.is_empty() {
                        let mut min_x = f64::INFINITY;
                        let mut min_y = f64::INFINITY;
                        let mut max_x = f64::NEG_INFINITY;
                        let mut max_y = f64::NEG_INFINITY;
                        for id in &group_after {
                            if let Some(pos) = next.get(*id) {
                                min_x = min_x.min(pos.0);
                                min_y = min_y.min(pos.1);
                                max_x = max_x.max(pos.0 + piece_width);
                                max_y = max_y.max(pos.1 + piece_height);
                            }
                        }

                        if group_after.len() == cols * rows {
                            let center_x = (min_x + max_x) * 0.5;
                            let center_y = (min_y + max_y) * 0.5;
                            let stage_center_x = view_min_x + view_width * 0.5;
                            let stage_center_y = view_min_y + view_height * 0.5;
                            let dx = stage_center_x - center_x;
                            let dy = stage_center_y - center_y;
                            for id in &group_after {
                                if let Some(pos) = next.get_mut(*id) {
                                    *pos = (pos.0 + dx, pos.1 + dy);
                                }
                            }
                        } else {
                            let mut in_group = vec![false; cols * rows];
                            for id in &group_after {
                                if *id < in_group.len() {
                                    in_group[*id] = true;
                                }
                            }
                            let mut has_borders = true;
                            'border_check: for row in 0..rows {
                                for col in 0..cols {
                                    if row == 0
                                        || row + 1 == rows
                                        || col == 0
                                        || col + 1 == cols
                                    {
                                        let id = row * cols + col;
                                        if !in_group[id] {
                                            has_borders = false;
                                            break 'border_check;
                                        }
                                    }
                                }
                            }
                            let mut rotation_ok = true;
                            if rotation_enabled_value {
                                for id in &group_after {
                                    if let Some(rot) = next_rotations.get(*id) {
                                        if angle_delta(0.0, *rot).abs()
                                            > ROTATION_SNAP_TOLERANCE_DEG
                                        {
                                            rotation_ok = false;
                                            break;
                                        }
                                    }
                                }
                            }
                            if has_borders && rotation_ok {
                                let dx = -min_x;
                                let dy = -min_y;
                                let dist = (dx * dx + dy * dy).sqrt();
                                let frame_snap_distance = snap_distance * frame_snap_ratio_value;
                                if dist <= frame_snap_distance {
                                    for id in &group_after {
                                        if let Some(pos) = next.get_mut(*id) {
                                            *pos = (pos.0 + dx, pos.1 + dy);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    let clamp_ids: &[usize] = if group_after.is_empty() {
                        &drag.members
                    } else {
                        &group_after
                    };
                    if !clamp_ids.is_empty() {
                        let mut min_cx = f64::INFINITY;
                        let mut max_cx = f64::NEG_INFINITY;
                        let mut min_cy = f64::INFINITY;
                        let mut max_cy = f64::NEG_INFINITY;
                        for id in clamp_ids {
                            if let Some(pos) = next.get(*id) {
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
                                    if let Some(pos) = next.get_mut(*id) {
                                        *pos = (pos.0 + shift_x, pos.1 + shift_y);
                                    }
                                }
                            }
                        }
                    }

                    let solved_now = is_solved(
                        &next,
                        &next_rotations,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        rotation_enabled_value,
                    );
                    positions.set(next);
                    connections.set(next_connections);
                    solved.set(solved_now);
                    active_id.set(None);
                    *drag_state.borrow_mut() = None;
                    event.prevent_default();
                }
            }
        };
        let drag_move = Rc::new(drag_move);
        let drag_release = Rc::new(drag_release);
        {
            let mut handlers = drag_handlers.borrow_mut();
            handlers.on_move = Some(drag_move.clone());
            handlers.on_release = Some(drag_release.clone());
        }

        let on_scramble = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
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
                active_id.set(None);
                *drag_state.borrow_mut() = None;
                solved.set(false);
            })
        };
        let on_solve = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
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
                active_id.set(None);
                *drag_state.borrow_mut() = None;
                solved.set(true);
            })
        };
        let on_solve_rotation = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let solved = solved.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let positions_snapshot = (*positions).clone();
                let zeroed = vec![0.0; total];
                rotations.set(zeroed.clone());
                let solved_now = is_solved(
                    &positions_snapshot,
                    &zeroed,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                solved.set(solved_now);
            })
        };

        let mask_defs: Html = piece_shapes
            .iter()
            .map(|(piece, path_d)| {
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
                        <path d={path_d.clone()} fill="white" />
                    </mask>
                }
            })
            .collect();

        let mut nodes = Vec::with_capacity(piece_shapes.len());
        for (piece, path_d) in piece_shapes.iter() {
            let piece_x = piece.col as f64 * piece_width;
            let piece_y = piece.row as f64 * piece_height;
            let current =
                positions_value.get(piece.id).copied().unwrap_or((piece_x, piece_y));
            let rotation = rotations_value.get(piece.id).copied().unwrap_or(0.0);
            let transform = format!(
                "translate({} {}) rotate({} {} {})",
                fmt_f64(current.0),
                fmt_f64(current.1),
                fmt_f64(rotation),
                fmt_f64(piece_width * 0.5),
                fmt_f64(piece_height * 0.5)
            );
            let mask_ref = format!("url(#piece-mask-{})", piece.id);
            let img_x = fmt_f64(-piece_x);
            let img_y = fmt_f64(-piece_y);
            let is_dragging = active_id_value == Some(piece.id);
            let class = if is_dragging { "piece dragging" } else { "piece" };
            let on_piece_down = {
                let positions = positions.clone();
                let drag_state = drag_state.clone();
                let active_id = active_id.clone();
                let svg_ref = svg_ref.clone();
                let z_order = z_order.clone();
                let connections = connections.clone();
                let piece_id = piece.id;
                let current_pos = current;
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                Callback::from(move |event: MouseEvent| {
                    if let Some((x, y)) = event_to_svg_coords(
                        &event,
                        &svg_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ) {
                        let positions_snapshot = (*positions).clone();
                        let mut connections_snapshot = (*connections).clone();
                        let mut members = if event.shift_key() {
                            clear_piece_connections(
                                &mut connections_snapshot,
                                piece_id,
                                cols,
                                rows,
                            );
                            connections.set(connections_snapshot);
                            vec![piece_id]
                        } else {
                            collect_group(&connections_snapshot, piece_id, cols, rows)
                        };
                        if members.is_empty() {
                            members.push(piece_id);
                        }
                        let pos = positions_snapshot
                            .get(piece_id)
                            .copied()
                            .unwrap_or(current_pos);
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
                        *drag_state.borrow_mut() = Some(DragState {
                            start_x: x,
                            start_y: y,
                            start_time: Date::now(),
                            members,
                            start_positions,
                        });
                        active_id.set(Some(piece_id));
                    }
                    event.prevent_default();
                })
            };
            let node = html! {
                <g
                    key={piece.id.to_string()}
                    class={class}
                    transform={transform}
                    onmousedown={on_piece_down}
                >
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
                    <path class="piece-hitbox" d={path_d.clone()} />
                    <path
                        class="piece-outline piece-outline-light"
                        d={path_d.clone()}
                    />
                    <path
                        class="piece-outline piece-outline-dark"
                        d={path_d.clone()}
                    />
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
                />
            </>
        };

        (
            html! {
                <svg
                    class="puzzle-image"
                    viewBox={view_box}
                    width={fmt_f64(view_width)}
                    height={fmt_f64(view_height)}
                    preserveAspectRatio="xMidYMid meet"
                    ref={svg_ref}
                >
                    <defs>
                        {mask_defs}
                    </defs>
                    {bounds}
                    {piece_nodes}
                </svg>
            },
            on_scramble,
            on_solve,
            on_solve_rotation,
            false,
        )
    } else {
        (
            html! { <p>{ "Loading puzzle image..." }</p> },
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
    let controls_panel = if show_controls_value {
        html! {
            <aside class="controls">
                <h2>{ "Puzzle Controls" }</h2>
                <p class={status_class}>{ status_label }</p>
                <div class="control">
                    <label>
                        { "Seed" }
                        <span class="control-value">{ seed_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label for="grid-select">
                        { "Grid" }
                        <span class="control-value">{ grid_label }</span>
                    </label>
                    <select
                        id="grid-select"
                        value={grid_index_value.to_string()}
                        onchange={on_grid_change}
                    >
                        {grid_options}
                    </select>
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
                    <label for="rotation-enabled">
                        { "Rotation" }
                        <span class="control-value">
                            { if rotation_enabled_value { "On" } else { "Off" } }
                        </span>
                    </label>
                    <input
                        id="rotation-enabled"
                        type="checkbox"
                        checked={rotation_enabled_value}
                        onchange={on_rotation_toggle}
                    />
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
                    <label for="dp-ratio">
                        { "Neck width" }
                        <span class="control-value">{ fmt_f64(settings_value.dp_ratio) }</span>
                    </label>
                    <input
                        id="dp-ratio"
                        type="range"
                        min={DP_RATIO_MIN.to_string()}
                        max={DP_RATIO_MAX.to_string()}
                        step="0.005"
                        value={settings_value.dp_ratio.to_string()}
                        oninput={dp_ratio_input}
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
                    <label for="neck-weight">
                        { "Neck weight" }
                        <span class="control-value">{ fmt_f64(settings_value.neck_weight) }</span>
                    </label>
                    <input
                        id="neck-weight"
                        type="range"
                        min={NECK_WEIGHT_MIN.to_string()}
                        max={NECK_WEIGHT_MAX.to_string()}
                        step="0.5"
                        value={settings_value.neck_weight.to_string()}
                        oninput={neck_input}
                    />
                </div>
                <div class="control">
                    <label for="mid-weight">
                        { "Mid weight" }
                        <span class="control-value">{ fmt_f64(settings_value.mid_weight) }</span>
                    </label>
                    <input
                        id="mid-weight"
                        type="range"
                        min={MID_WEIGHT_MIN.to_string()}
                        max={MID_WEIGHT_MAX.to_string()}
                        step="0.5"
                        value={settings_value.mid_weight.to_string()}
                        oninput={mid_input}
                    />
                </div>
                <div class="control">
                    <label for="knob-weight">
                        { "Knob weight" }
                        <span class="control-value">{ fmt_f64(settings_value.knob_weight) }</span>
                    </label>
                    <input
                        id="knob-weight"
                        type="range"
                        min={KNOB_WEIGHT_MIN.to_string()}
                        max={KNOB_WEIGHT_MAX.to_string()}
                        step="1"
                        value={settings_value.knob_weight.to_string()}
                        oninput={knob_input}
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
            {controls_panel}
        </main>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
