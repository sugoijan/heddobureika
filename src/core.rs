use serde::{Deserialize, Serialize};
use std::fmt::Write;
pub use heddobureika_core::game::*;
pub(crate) use heddobureika_core::{
    best_grid_for_count, build_grid_choices, grid_choice_index, grid_choice_label, GridChoice,
    DEFAULT_TARGET_COUNT, FALLBACK_GRID, SOLVE_TIME_EXPONENT, SOLVE_TIME_FACTOR,
};
pub(crate) const ROTATION_LOCK_THRESHOLD_DEFAULT: usize = 4;
pub(crate) const ROTATION_LOCK_THRESHOLD_MIN: usize = 1;
pub(crate) const ROTATION_NOISE_MIN: f32 = 0.0;
pub(crate) const ROTATION_NOISE_MAX: f32 = 6.0;
pub(crate) const ROTATION_NOISE_DEFAULT: f32 = 0.6;
pub(crate) const EMBOSS_OFFSET: f32 = 2.0;
pub(crate) const EMBOSS_RIM: f32 = 1.0;
pub(crate) const EMBOSS_OPACITY: f32 = 0.25;
pub(crate) const WGPU_EDGE_AA_MIN: f32 = 0.02;
pub(crate) const WGPU_EDGE_AA_MAX: f32 = 2.0;
pub(crate) const WGPU_EDGE_AA_DEFAULT: f32 = 1.0;
pub(crate) const WGPU_RENDER_SCALE_MIN: f32 = 0.5;
pub(crate) const WGPU_RENDER_SCALE_MAX: f32 = 2.0;
pub(crate) const WGPU_RENDER_SCALE_DEFAULT: f32 = 1.0;
pub(crate) const WGPU_CANVAS_MAX_PX: u32 = 8192;
pub(crate) const AUTO_PAN_OUTER_RATIO_MIN: f32 = 0.0;
pub(crate) const AUTO_PAN_OUTER_RATIO_MAX: f32 = 0.2;
pub(crate) const AUTO_PAN_OUTER_RATIO_DEFAULT: f32 = 0.03;
pub(crate) const AUTO_PAN_INNER_RATIO_MIN: f32 = 0.02;
pub(crate) const AUTO_PAN_INNER_RATIO_MAX: f32 = 0.3;
pub(crate) const AUTO_PAN_INNER_RATIO_DEFAULT: f32 = 0.06;
pub(crate) const AUTO_PAN_SPEED_RATIO_MIN: f32 = 0.1;
pub(crate) const AUTO_PAN_SPEED_RATIO_MAX: f32 = 2.0;
pub(crate) const AUTO_PAN_SPEED_RATIO_DEFAULT: f32 = 1.0;
pub(crate) const CLICK_MOVE_RATIO: f32 = 0.01;
pub(crate) const CLICK_MAX_DURATION_MS: f32 = 240.0;
pub(crate) const CLICK_QUICK_TAP_MS: f32 = 120.0;
pub(crate) const TOUCH_DRAG_SLOP_PX: f32 = 4.0;
pub(crate) const SNAP_ANIMATION_MS: f32 = 160.0;
pub(crate) const RUBBER_BAND_RATIO: f32 = 0.35;
pub(crate) const TAB_WIDTH_MIN: f32 = 0.2;
pub(crate) const TAB_WIDTH_MAX: f32 = 0.72;
pub(crate) const TAB_WIDTH_RANGE: f32 = 0.16;
pub(crate) const TAB_DEPTH_MIN: f32 = 0.2;
pub(crate) const TAB_DEPTH_MAX: f32 = 1.1;
pub(crate) const TAB_DEPTH_RANGE: f32 = 0.35;
pub(crate) const TAB_SIZE_SCALE_MIN: f32 = 0.1;
pub(crate) const TAB_SIZE_SCALE_MAX: f32 = 0.5;
pub(crate) const TAB_SIZE_MIN_LIMIT: f32 = 0.02;
pub(crate) const TAB_SIZE_MAX_LIMIT: f32 = 0.24;
pub(crate) const JITTER_STRENGTH_MIN: f32 = 0.0;
pub(crate) const JITTER_STRENGTH_MAX: f32 = 0.3;
pub(crate) const JITTER_LEN_BIAS_MIN: f32 = 0.0;
pub(crate) const JITTER_LEN_BIAS_MAX: f32 = 1.0;
pub(crate) const TAB_DEPTH_CAP_MIN: f32 = 0.2;
pub(crate) const TAB_DEPTH_CAP_MAX: f32 = 0.45;
pub(crate) const CURVE_DETAIL_MIN: f32 = 0.5;
pub(crate) const CURVE_DETAIL_MAX: f32 = 3.0;
pub(crate) const SKEW_RANGE_MAX: f32 = 0.2;
pub(crate) const VARIATION_MIN: f32 = 0.0;
pub(crate) const VARIATION_MAX: f32 = 1.0;
pub(crate) const LINE_BEND_MIN: f32 = 0.0;
pub(crate) const EDGE_STEP_DIV: f32 = 6.0;
pub(crate) const EDGE_STEP_MIN: f32 = 6.0;
pub(crate) const CORNER_RADIUS_RATIO: f32 = 0.05;
pub(crate) const LOCAL_GAME_KEY: &str = "heddobureika.game.v1";
pub(crate) const PUZZLE_SELECTION_KEY: &str = "heddobureika.puzzle.v1";
pub(crate) const RENDER_SETTINGS_KEY: &str = "heddobureika.render.v1";
pub(crate) const THEME_MODE_KEY: &str = "heddobureika.theme.v1";
pub(crate) const INIT_SETTINGS_KEY: &str = "heddobureika.init.v1";
pub(crate) const ADMIN_TOKEN_KEY: &str = "heddobureika.admin.v1";
pub(crate) const PUZZLE_SELECTION_VERSION: u32 = 1;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Piece {
    pub(crate) id: usize,
    pub(crate) row: u32,
    pub(crate) col: u32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EdgeParams {
    pub(crate) tab_size: f32,
    pub(crate) tab_depth: f32,
    pub(crate) a: f32,
    pub(crate) b: f32,
    pub(crate) c: f32,
    pub(crate) d: f32,
    pub(crate) e: f32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum TabSide {
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
pub(crate) struct Edge {
    pub(crate) tab_side: TabSide,
    pub(crate) params: EdgeParams,
}

#[derive(Clone, Copy)]
pub(crate) enum EdgeOrientation {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Clone, Copy)]
pub(crate) enum Segment {
    LineTo { x: f32, y: f32 },
}

#[derive(Clone, Copy)]
pub(crate) struct LineWave {
    pub(crate) amplitude: f32,
    pub(crate) skew: f32,
}

pub(crate) struct WarpField<'a> {
    pub(crate) width: f32,
    pub(crate) height: f32,
    pub(crate) horizontal: &'a [LineWave],
    pub(crate) vertical: &'a [LineWave],
}

#[derive(Clone, Debug)]
pub(crate) struct DragState {
    pub(crate) start_x: f32,
    pub(crate) start_y: f32,
    pub(crate) start_time: f32,
    pub(crate) primary_id: usize,
    pub(crate) anchor_id: usize,
    pub(crate) anchor_pos: (f32, f32),
    pub(crate) anchor_rot: f32,
    pub(crate) touch_id: Option<i32>,
    pub(crate) rotate_mode: bool,
    pub(crate) right_click: bool,
    pub(crate) cursor_x: f32,
    pub(crate) cursor_y: f32,
    pub(crate) pivot_x: f32,
    pub(crate) pivot_y: f32,
    pub(crate) start_angle: f32,
    pub(crate) members: Vec<usize>,
    pub(crate) start_positions: Vec<(f32, f32)>,
}

#[derive(Clone, Debug)]
pub(crate) enum AnimationKind {
    Pivot {
        pivot_x: f32,
        pivot_y: f32,
        delta: f32,
    },
    Anchor {
        anchor_id: usize,
        start_center: (f32, f32),
        target_center: (f32, f32),
        start_rot: f32,
        target_rot: f32,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct RotationAnimation {
    pub(crate) start_time: f32,
    pub(crate) duration: f32,
    pub(crate) members: Vec<usize>,
    pub(crate) start_positions: Vec<(f32, f32)>,
    pub(crate) start_rotations: Vec<f32>,
    pub(crate) kind: AnimationKind,
}

#[derive(Clone, Debug)]
pub(crate) struct QueuedRotation {
    pub(crate) members: Vec<usize>,
    pub(crate) pivot_x: f32,
    pub(crate) pivot_y: f32,
    pub(crate) noise: f32,
    pub(crate) reverse: bool,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum PreviewCorner {
    BottomLeft,
    BottomRight,
    TopLeft,
    TopRight,
}

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum ThemeMode {
    System,
    Light,
    Dark,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum InitMode {
    Local,
    Online,
}

impl Default for InitMode {
    fn default() -> Self {
        InitMode::Local
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum RendererKind {
    Svg,
    Wgpu,
}

impl Default for RendererKind {
    fn default() -> Self {
        RendererKind::Wgpu
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct SvgRenderSettings {
    pub(crate) animations: bool,
    pub(crate) emboss: bool,
    pub(crate) fast_render: bool,
    pub(crate) fast_filter: bool,
}

impl Default for SvgRenderSettings {
    fn default() -> Self {
        Self {
            animations: false,
            emboss: true,
            fast_render: true,
            fast_filter: true,
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct WgpuRenderSettings {
    #[serde(default)]
    pub(crate) show_fps: bool,
    #[serde(default = "default_wgpu_edge_aa")]
    pub(crate) edge_aa: f32,
    #[serde(default = "default_wgpu_render_scale")]
    pub(crate) render_scale: f32,
}

impl Default for WgpuRenderSettings {
    fn default() -> Self {
        Self {
            show_fps: false,
            edge_aa: WGPU_EDGE_AA_DEFAULT,
            render_scale: WGPU_RENDER_SCALE_DEFAULT,
        }
    }
}

fn default_wgpu_edge_aa() -> f32 {
    WGPU_EDGE_AA_DEFAULT
}

fn default_wgpu_render_scale() -> f32 {
    WGPU_RENDER_SCALE_DEFAULT
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct RenderSettings {
    #[serde(default = "default_image_max_dim")]
    pub(crate) image_max_dim: u32,
    pub(crate) renderer: RendererKind,
    pub(crate) svg: SvgRenderSettings,
    pub(crate) wgpu: WgpuRenderSettings,
}

impl Default for RenderSettings {
    fn default() -> Self {
        Self {
            image_max_dim: IMAGE_MAX_DIMENSION_DEFAULT,
            renderer: RendererKind::Wgpu,
            svg: SvgRenderSettings::default(),
            wgpu: WgpuRenderSettings::default(),
        }
    }
}

fn default_image_max_dim() -> u32 {
    IMAGE_MAX_DIMENSION_DEFAULT
}

#[derive(Clone, PartialEq)]
pub(crate) struct ShapeSettings {
    pub(crate) tab_width: f32,
    pub(crate) tab_depth: f32,
    pub(crate) tab_size_scale: f32,
    pub(crate) tab_size_min: f32,
    pub(crate) tab_size_max: f32,
    pub(crate) jitter_strength: f32,
    pub(crate) jitter_len_bias: f32,
    pub(crate) tab_depth_cap: f32,
    pub(crate) curve_detail: f32,
    pub(crate) skew_range: f32,
    pub(crate) variation: f32,
    pub(crate) line_bend_ratio: f32,
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

pub(crate) fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t
}

pub(crate) fn sample_line_wave(lines: &[LineWave], t: f32) -> LineWave {
    let last = lines.len().saturating_sub(1) as f32;
    if last <= 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }
    let clamped = t.clamp(0.0, 1.0) * last;
    let idx = clamped.floor() as usize;
    let next = (idx + 1).min(lines.len().saturating_sub(1));
    let frac = clamped - idx as f32;
    let current = lines[idx];
    let following = lines[next];
    LineWave {
        amplitude: lerp(current.amplitude, following.amplitude, frac),
        skew: lerp(current.skew, following.skew, frac),
    }
}

pub(crate) fn warp_point(x: f32, y: f32, warp: &WarpField<'_>) -> (f32, f32) {
    let u = (x / warp.width).clamp(0.0, 1.0);
    let v = (y / warp.height).clamp(0.0, 1.0);
    let h_wave = sample_line_wave(warp.horizontal, v);
    let v_wave = sample_line_wave(warp.vertical, u);
    let u_skew = (u + h_wave.skew * u * (1.0 - u)).clamp(0.0, 1.0);
    let v_skew = (v + v_wave.skew * v * (1.0 - v)).clamp(0.0, 1.0);
    let dy = h_wave.amplitude * (std::f32::consts::PI * u_skew).sin();
    let dx = v_wave.amplitude * (std::f32::consts::PI * v_skew).sin();
    (x + dx, y + dy)
}

pub(crate) fn build_pieces(rows: u32, cols: u32) -> Vec<Piece> {
    let mut pieces = Vec::with_capacity((rows * cols) as usize);
    for row in 0..rows {
        for col in 0..cols {
            let id = (row * cols + col) as usize;
            pieces.push(Piece { id, row, col });
        }
    }
    pieces
}

pub(crate) fn is_border_piece(row: usize, col: usize, rows: usize, cols: usize) -> bool {
    row == 0 || row + 1 == rows || col == 0 || col + 1 == cols
}

pub(crate) fn count_connections(
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
) -> (usize, usize, usize, usize) {
    if cols == 0 || rows == 0 {
        return (0, 0, 0, 0);
    }
    let mut connected = 0;
    let mut border_connected = 0;
    let mut total_expected = 0;
    let mut border_expected = 0;
    for row in 0..rows {
        for col in 0..cols {
            let id = row * cols + col;
            let is_border = is_border_piece(row, col, rows, cols);
            if col + 1 < cols {
                total_expected += 1;
                let neighbor_border = is_border_piece(row, col + 1, rows, cols);
                if is_border && neighbor_border {
                    border_expected += 1;
                }
                if id < connections.len() && connections[id][DIR_RIGHT] {
                    connected += 1;
                    if is_border && neighbor_border {
                        border_connected += 1;
                    }
                }
            }
            if row + 1 < rows {
                total_expected += 1;
                let neighbor_border = is_border_piece(row + 1, col, rows, cols);
                if is_border && neighbor_border {
                    border_expected += 1;
                }
                if id < connections.len() && connections[id][DIR_DOWN] {
                    connected += 1;
                    if is_border && neighbor_border {
                        border_connected += 1;
                    }
                }
            }
        }
    }
    (connected, border_connected, total_expected, border_expected)
}

pub(crate) fn fmt_f32(value: f32) -> String {
    format!("{:.3}", value)
}

pub(crate) fn format_progress(count: usize, total: usize) -> String {
    if total == 0 {
        return "--".to_string();
    }
    let pct = (count as f32 / total as f32) * 100.0;
    //format!("{}/{} ({:.0}%)", count, total, pct)
    format!("{:.0}%", pct)
}

pub(crate) fn format_time_unit(value: u32, unit: &str) -> String {
    if value == 1 {
        format!("~{} {}", value, unit)
    } else {
        format!("~{} {}s", value, unit)
    }
}

pub(crate) fn format_duration(seconds: f32) -> String {
    if !seconds.is_finite() || seconds <= 0.0 {
        return "~0 seconds".to_string();
    }
    if seconds < 90.0 {
        return format_time_unit(seconds.round().max(1.0) as u32, "second");
    }
    let minutes = seconds / 60.0;
    if minutes < 90.0 {
        return format_time_unit(minutes.round().max(1.0) as u32, "minute");
    }
    let hours = minutes / 60.0;
    if hours < 36.0 {
        return format_time_unit(hours.round().max(1.0) as u32, "hour");
    }
    let days = hours / 24.0;
    format_time_unit(days.round().max(1.0) as u32, "day")
}

pub(crate) fn jitter_value(
    seed: u32,
    salt: u32,
    base: f32,
    range: f32,
    min: f32,
    max: f32,
    variation: f32,
) -> f32 {
    let jitter = rand_unit(seed, salt) * 2.0 - 1.0;
    let value = base + jitter * range * variation;
    value.clamp(min, max)
}

pub(crate) fn is_solved(
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
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
                let target_x = col as f32 * piece_width;
                let target_y = row as f32 * piece_height;
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


pub(crate) fn rubber_band_distance(delta: f32, limit: f32) -> f32 {
    if limit <= 0.0 {
        return 0.0;
    }
    let abs = delta.abs();
    let sign = delta.signum();
    sign * (limit * abs / (limit + abs))
}

pub(crate) fn rubber_band_clamp(value: f32, min: f32, max: f32, limit: f32) -> f32 {
    if value < min {
        min + rubber_band_distance(value - min, limit)
    } else if value > max {
        max + rubber_band_distance(value - max, limit)
    } else {
        value
    }
}


pub(crate) fn next_snap_rotation(angle: f32) -> f32 {
    let next = (angle / ROTATION_STEP_DEG).floor() + 1.0;
    normalize_angle(next * ROTATION_STEP_DEG)
}

pub(crate) fn click_rotation_delta(
    current_angle: f32,
    noise: f32,
    noise_range: f32,
    snap_tolerance: f32,
) -> f32 {
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


pub(crate) fn rotate_point(x: f32, y: f32, origin_x: f32, origin_y: f32, angle_deg: f32) -> (f32, f32) {
    let (dx, dy) = (x - origin_x, y - origin_y);
    let (rx, ry) = rotate_vec(dx, dy, angle_deg);
    (origin_x + rx, origin_y + ry)
}

pub(crate) fn edge_seed(base: u32, orientation: u32, row: u32, col: u32) -> u32 {
    base ^ orientation.wrapping_mul(0x9E37_79B9)
        ^ row.wrapping_mul(0x85EB_CA6B)
        ^ col.wrapping_mul(0xC2B2_AE35)
}

pub(crate) fn edge_from_seed(seed: u32, settings: &ShapeSettings) -> Edge {
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

pub(crate) fn build_edge_maps(
    rows: u32,
    cols: u32,
    seed: u32,
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

pub(crate) fn line_wave(seed: u32, axis: u32, index: u32, max_amp: f32) -> LineWave {
    if max_amp == 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }
    let salt = axis.wrapping_mul(0x9E37_79B9) ^ index;
    let amplitude = rand_range(seed ^ 0xB1EB_01DE, salt, -max_amp, max_amp);
    let skew = rand_range(seed ^ 0xA11C_E0DE, salt, -0.6, 0.6);
    LineWave { amplitude, skew }
}

pub(crate) fn build_line_waves(
    rows: u32,
    cols: u32,
    seed: u32,
    piece_width: f32,
    piece_height: f32,
    line_bend_ratio: f32,
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

pub(crate) fn cubic_point(
    p0: (f32, f32),
    p1: (f32, f32),
    p2: (f32, f32),
    p3: (f32, f32),
    t: f32,
) -> (f32, f32) {
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

pub(crate) fn cubic_segments(
    p0: (f32, f32),
    p1: (f32, f32),
    p2: (f32, f32),
    p3: (f32, f32),
    steps: usize,
) -> Vec<Segment> {
    let mut segments = Vec::with_capacity(steps.max(1));
    let steps = steps.max(1);
    for step in 1..=steps {
        let t = step as f32 / steps as f32;
        let (x, y) = cubic_point(p0, p1, p2, p3, t);
        segments.push(Segment::LineTo { x, y });
    }
    segments
}

pub(crate) fn edge_segments(
    len: f32,
    depth_base: f32,
    edge: Option<&Edge>,
    tab_sign: i8,
    depth_limit: f32,
    curve_detail: f32,
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

    let sign = tab_sign as f32;
    let l = |v: f32| len * v;
    let w = |v: f32| depth_base * v * sign;

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
    let steps = ((base_steps as f32) * detail).round() as usize;
    let steps = steps.max(8);
    let mut segments = Vec::with_capacity(steps * 3);
    segments.extend(cubic_segments(p0, p1, p2, p3, steps));
    segments.extend(cubic_segments(p3, p4, p5, p6, steps));
    segments.extend(cubic_segments(p6, p7, p8, p9, steps));
    segments
}

pub(crate) fn reverse_segments(segments: &[Segment]) -> Vec<Segment> {
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

pub(crate) fn map_point(
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
    warp: &WarpField<'_>,
    x: f32,
    y: f32,
) -> (f32, f32) {
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

pub(crate) fn map_local_point(
    offset: (f32, f32),
    warp: &WarpField<'_>,
    x: f32,
    y: f32,
) -> (f32, f32) {
    let (wx, wy) = warp_point(offset.0 + x, offset.1 + y, warp);
    (wx - offset.0, wy - offset.1)
}

pub(crate) fn append_local_points(
    path: &mut String,
    offset: (f32, f32),
    warp: &WarpField<'_>,
    points: &[(f32, f32)],
) {
    for &(x, y) in points {
        let (gx, gy) = map_local_point(offset, warp, x, y);
        let _ = write!(path, " L {} {}", fmt_f32(gx), fmt_f32(gy));
    }
}

pub(crate) fn build_local_path(
    offset: (f32, f32),
    warp: &WarpField<'_>,
    points: &[(f32, f32)],
) -> String {
    if points.is_empty() {
        return String::new();
    }
    let (sx, sy) = map_local_point(offset, warp, points[0].0, points[0].1);
    let mut path = String::new();
    let _ = write!(path, "M {} {}", fmt_f32(sx), fmt_f32(sy));
    append_local_points(&mut path, offset, warp, &points[1..]);
    path
}

pub(crate) fn corner_arc_points(
    cx: f32,
    cy: f32,
    radius: f32,
    start_angle: f32,
    end_angle: f32,
    steps: usize,
) -> Vec<(f32, f32)> {
    let steps = steps.max(1);
    let mut end = end_angle;
    if end < start_angle {
        end += 2.0 * std::f32::consts::PI;
    }
    let span = end - start_angle;
    let mut points = Vec::with_capacity(steps + 1);
    for step in 0..=steps {
        let t = step as f32 / steps as f32;
        let angle = start_angle + span * t;
        points.push((cx + radius * angle.cos(), cy + radius * angle.sin()));
    }
    points
}

pub(crate) fn append_segments(
    path: &mut String,
    segments: &[Segment],
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
    warp: &WarpField<'_>,
    start: (f32, f32),
    max_segment_len: f32,
) {
    let mut current = start;
    let max_len = if max_segment_len > 0.0 {
        max_segment_len
    } else {
        f32::INFINITY
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
                    let t = step as f32 / steps as f32;
                    let px = current.0 + dx * t;
                    let py = current.1 + dy * t;
                    let (gx, gy) = map_point(orientation, origin, offset, warp, px, py);
                    let _ = write!(path, " L {} {}", fmt_f32(gx), fmt_f32(gy));
                }
                current = (x, y);
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct PiecePaths {
    pub(crate) outline: String,
    pub(crate) edges: [String; 4],
}

pub(crate) fn build_edge_path(
    segments: &[Segment],
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
    warp: &WarpField<'_>,
    start: (f32, f32),
    max_segment_len: f32,
) -> String {
    let (sx, sy) = map_point(orientation, origin, offset, warp, start.0, start.1);
    let mut path = String::new();
    let _ = write!(path, "M {} {}", fmt_f32(sx), fmt_f32(sy));
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

pub(crate) fn build_piece_path(
    piece: &Piece,
    piece_width: f32,
    piece_height: f32,
    horizontal: &[Vec<Option<Edge>>],
    vertical: &[Vec<Option<Edge>>],
    warp: &WarpField<'_>,
    tab_depth_cap: f32,
    curve_detail: f32,
) -> PiecePaths {
    let row = piece.row as usize;
    let col = piece.col as usize;
    let piece_x = piece.col as f32 * piece_width;
    let piece_y = piece.row as f32 * piece_height;

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
    let pi = std::f32::consts::PI;

    let mut path = String::new();
    let (start_x, start_y) = map_local_point(offset, warp, top_start.0, top_start.1);
    let _ = write!(path, "M {} {}", fmt_f32(start_x), fmt_f32(start_y));
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
