use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt::Write;
pub(crate) const PUZZLE_SEED: u32 = 0x5EED_2520;
pub(crate) const MAX_LINE_BEND_RATIO: f32 = 0.2;
pub(crate) const SNAP_DISTANCE_RATIO_DEFAULT: f32 = 0.200;
pub(crate) const SNAP_DISTANCE_RATIO_MIN: f32 = 0.050;
pub(crate) const SNAP_DISTANCE_RATIO_MAX: f32 = 0.350;
pub(crate) const SOLVE_TOLERANCE_RATIO: f32 = 0.080;
pub(crate) const ROTATION_STEP_DEG: f32 = 90.0;
pub(crate) const ROTATION_SNAP_TOLERANCE_DEFAULT_DEG: f32 = 5.0;
pub(crate) const ROTATION_SNAP_TOLERANCE_MIN_DEG: f32 = 0.5;
pub(crate) const ROTATION_SNAP_TOLERANCE_MAX_DEG: f32 = 12.0;
pub(crate) const ROTATION_SOLVE_TOLERANCE_DEG: f32 = 1.5;
pub(crate) const ROTATION_LOCK_THRESHOLD_DEFAULT: usize = 4;
pub(crate) const ROTATION_LOCK_THRESHOLD_MIN: usize = 1;
pub(crate) const ROTATION_NOISE_MIN: f32 = 0.0;
pub(crate) const ROTATION_NOISE_MAX: f32 = 6.0;
pub(crate) const ROTATION_NOISE_DEFAULT: f32 = 0.6;
pub(crate) const EMBOSS_OFFSET: f32 = 2.0;
pub(crate) const EMBOSS_RIM: f32 = 1.0;
pub(crate) const EMBOSS_OPACITY: f32 = 0.2;
pub(crate) const WGPU_EDGE_AA_MIN: f32 = 0.02;
pub(crate) const WGPU_EDGE_AA_MAX: f32 = 2.0;
pub(crate) const WGPU_EDGE_AA_DEFAULT: f32 = 1.0;
pub(crate) const WGPU_RENDER_SCALE_MIN: f32 = 0.5;
pub(crate) const WGPU_RENDER_SCALE_MAX: f32 = 2.0;
pub(crate) const WGPU_RENDER_SCALE_DEFAULT: f32 = 2.0;
pub(crate) const IMAGE_MAX_DIMENSION_MIN: u32 = 512;
pub(crate) const IMAGE_MAX_DIMENSION_MAX: u32 = 4096;
pub(crate) const IMAGE_MAX_DIMENSION_DEFAULT: u32 = 1280;
pub(crate) const CLICK_MOVE_RATIO: f32 = 0.01;
pub(crate) const CLICK_MAX_DURATION_MS: f32 = 240.0;
pub(crate) const CLICK_QUICK_TAP_MS: f32 = 120.0;
pub(crate) const SNAP_ANIMATION_MS: f32 = 160.0;
pub(crate) const FLIP_CHANCE: f32 = 0.2;
pub(crate) const RUBBER_BAND_RATIO: f32 = 0.35;
pub(crate) const WORKSPACE_SCALE_MIN: f32 = 0.4;
pub(crate) const WORKSPACE_SCALE_MAX: f32 = 2.5;
pub(crate) const WORKSPACE_SCALE_DEFAULT: f32 = 1.7;
pub(crate) const FRAME_SNAP_MIN: f32 = 0.4;
pub(crate) const FRAME_SNAP_MAX: f32 = 3.0;
pub(crate) const FRAME_SNAP_DEFAULT: f32 = 1.0;
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
pub(crate) const STORAGE_KEY: &str = "heddobureika.board.v2";
pub(crate) const RENDER_SETTINGS_KEY: &str = "heddobureika.render.v1";
pub(crate) const THEME_MODE_KEY: &str = "heddobureika.theme.v1";
pub(crate) const STORAGE_VERSION: u32 = 2;
pub(crate) const DIR_UP: usize = 0;
pub(crate) const DIR_RIGHT: usize = 1;
pub(crate) const DIR_DOWN: usize = 2;
pub(crate) const DIR_LEFT: usize = 3;

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

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct SavedBoard {
    pub(crate) version: u32,
    pub(crate) cols: u32,
    pub(crate) rows: u32,
    pub(crate) image_width: u32,
    pub(crate) image_height: u32,
    pub(crate) positions: Vec<(f32, f32)>,
    pub(crate) rotations: Vec<f32>,
    pub(crate) flips: Vec<bool>,
    pub(crate) connections: Vec<[bool; 4]>,
    pub(crate) z_order: Vec<usize>,
    pub(crate) scramble_nonce: u32,
}

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct GridChoice {
    pub(crate) target_count: u32,
    pub(crate) cols: u32,
    pub(crate) rows: u32,
    pub(crate) actual_count: u32,
}

pub(crate) const TARGET_PIECE_COUNTS: [u32; 11] = [
    50, 100, 150, 300, 500, 750, 1000, 1500, 2000, 3000, 5000,
];
pub(crate) const DEFAULT_TARGET_COUNT: u32 = 100;
pub(crate) const GRID_REL_COUNT_TOL: f32 = 0.05;
pub(crate) const GRID_PIECE_RATIO_MAX: f32 = 1.42;
pub(crate) const GRID_ROW_MIN: u32 = 2;
pub(crate) const GRID_ROW_WIDEN: f32 = 1.5;
pub(crate) const GRID_NEIGHBOR_COLS: i32 = 3;
pub(crate) const GRID_SCORE_COUNT: f32 = 1.0;
pub(crate) const GRID_SCORE_GRID: f32 = 1.0;
pub(crate) const GRID_SCORE_PIECE: f32 = 0.5;
pub(crate) const SOLVE_TIME_FACTOR: f32 = 4.1;
pub(crate) const SOLVE_TIME_EXPONENT: f32 = 1.3;

pub(crate) const FALLBACK_GRID: GridChoice = GridChoice {
    target_count: 80,
    cols: 10,
    rows: 8,
    actual_count: 80,
};

pub(crate) fn grid_choice_index(choices: &[GridChoice], cols: u32, rows: u32) -> Option<usize> {
    choices
        .iter()
        .position(|choice| choice.cols == cols && choice.rows == rows)
}

pub(crate) fn grid_choice_label(choice: &GridChoice) -> String {
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

pub(crate) fn best_grid_for_count(width: u32, height: u32, target: u32) -> Option<GridChoice> {
    if target == 0 || width == 0 || height == 0 {
        return None;
    }
    let aspect = width as f32 / height as f32;
    let piece_ratio_max = GRID_PIECE_RATIO_MAX.max(1.0);
    let piece_ratio_min = 1.0 / piece_ratio_max;
    let base = (target as f32).sqrt().ceil() as u32;
    let r_hi = ((base as f32) * GRID_ROW_WIDEN).ceil() as u32;
    let r_hi = r_hi.max(GRID_ROW_MIN);
    let mut best: Option<(GridChoice, f32)> = None;
    for r in GRID_ROW_MIN..=r_hi {
        let c_star = target as f32 / r as f32;
        let c0 = c_star.round() as i32;
        for dc in -GRID_NEIGHBOR_COLS..=GRID_NEIGHBOR_COLS {
            let c = c0 + dc;
            if c < 2 {
                continue;
            }
            let actual = r as i32 * c;
            let actual_u = actual as u32;
            let rel_err = ((actual_u as f32) - (target as f32)).abs() / target as f32;
            if rel_err > GRID_REL_COUNT_TOL {
                continue;
            }
            let grid_ratio = c as f32 / r as f32;
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

pub(crate) fn build_grid_choices(width: u32, height: u32) -> Vec<GridChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| best_grid_for_count(width, height, *target))
        .collect()
}

pub(crate) fn validate_saved_board(state: &SavedBoard, total: usize) -> bool {
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
    format!("{}/{} ({:.0}%)", count, total, pct)
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

pub(crate) fn neighbor_id(id: usize, cols: usize, rows: usize, dir: usize) -> Option<usize> {
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

pub(crate) fn opposite_dir(dir: usize) -> usize {
    match dir {
        DIR_UP => DIR_DOWN,
        DIR_RIGHT => DIR_LEFT,
        DIR_DOWN => DIR_UP,
        DIR_LEFT => DIR_RIGHT,
        _ => DIR_UP,
    }
}

pub(crate) fn set_connection(
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

pub(crate) fn clear_piece_connections(
    connections: &mut Vec<[bool; 4]>,
    id: usize,
    cols: usize,
    rows: usize,
) {
    for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
        set_connection(connections, id, dir, false, cols, rows);
    }
}

pub(crate) fn collect_group(connections: &[[bool; 4]], start: usize, cols: usize, rows: usize) -> Vec<usize> {
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

pub(crate) fn groups_from_connections(
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
) -> Vec<Vec<usize>> {
    let total = cols * rows;
    if total == 0 {
        return Vec::new();
    }
    let mut visited = vec![false; total];
    let mut groups = Vec::new();
    let mut queue = VecDeque::new();
    for start in 0..total {
        if visited[start] {
            continue;
        }
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
        group.sort_unstable();
        groups.push(group);
    }
    groups
}

pub(crate) fn is_fully_connected(connections: &[[bool; 4]], cols: usize, rows: usize) -> bool {
    let total = cols * rows;
    if total == 0 || connections.len() != total {
        return false;
    }
    collect_group(connections, 0, cols, rows).len() == total
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

pub(crate) fn splitmix32(mut value: u32) -> u32 {
    value = value.wrapping_add(0x9E37_79B9);
    let mut z = value;
    z = (z ^ (z >> 16)).wrapping_mul(0x85EB_CA6B);
    z = (z ^ (z >> 13)).wrapping_mul(0xC2B2_AE35);
    z ^ (z >> 16)
}

pub(crate) fn rand_unit(seed: u32, salt: u32) -> f32 {
    let mixed = splitmix32(seed ^ salt);
    let top = mixed >> 8;
    top as f32 / ((1u32 << 24) as f32)
}

pub(crate) fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
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

pub(crate) fn normalize_angle(mut angle: f32) -> f32 {
    angle = angle % 360.0;
    if angle < 0.0 {
        angle += 360.0;
    }
    angle
}

pub(crate) fn angle_delta(target: f32, current: f32) -> f32 {
    let mut diff = normalize_angle(target - current);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

pub(crate) fn angle_matches(a: f32, b: f32, tolerance: f32) -> bool {
    angle_delta(a, b).abs() <= tolerance
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

pub(crate) fn rotate_vec(x: f32, y: f32, angle_deg: f32) -> (f32, f32) {
    let radians = angle_deg.to_radians();
    let cos = radians.cos();
    let sin = radians.sin();
    (x * cos - y * sin, x * sin + y * cos)
}

pub(crate) fn rotate_point(x: f32, y: f32, origin_x: f32, origin_y: f32, angle_deg: f32) -> (f32, f32) {
    let (dx, dy) = (x - origin_x, y - origin_y);
    let (rx, ry) = rotate_vec(dx, dy, angle_deg);
    (origin_x + rx, origin_y + ry)
}

pub(crate) fn aligned_center_from_anchor(
    anchor_row: i32,
    anchor_col: i32,
    anchor_center: (f32, f32),
    member_id: usize,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    rotation: f32,
) -> (f32, f32) {
    let row = (member_id / cols) as i32;
    let col = (member_id % cols) as i32;
    let dx = (col - anchor_col) as f32 * piece_width;
    let dy = (row - anchor_row) as f32 * piece_height;
    let (rx, ry) = rotate_vec(dx, dy, rotation);
    (anchor_center.0 + rx, anchor_center.1 + ry)
}

pub(crate) fn align_group_to_anchor(
    positions: &mut Vec<(f32, f32)>,
    rotations: &mut Vec<f32>,
    members: &[usize],
    anchor_id: usize,
    anchor_center: (f32, f32),
    target_rot: f32,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
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

pub(crate) fn connect_aligned_neighbors(
    members: &[usize],
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    connections: &mut Vec<[bool; 4]>,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    snap_distance: f32,
    rotation_snap_tolerance: f32,
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

pub(crate) fn frame_center_bounds_for_rotation(
    frame_width: f32,
    frame_height: f32,
    piece_width: f32,
    piece_height: f32,
    rotation: f32,
) -> (f32, f32, f32, f32) {
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

pub(crate) fn apply_snaps_for_group(
    members: &[usize],
    positions: &mut Vec<(f32, f32)>,
    rotations: &mut Vec<f32>,
    flips: &[bool],
    connections: &mut Vec<[bool; 4]>,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    snap_distance: f32,
    frame_snap_ratio: f32,
    center_min_x: f32,
    center_max_x: f32,
    center_min_y: f32,
    center_max_y: f32,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    rotation_snap_tolerance: f32,
    rotation_enabled: bool,
) -> Vec<usize> {
    let total = cols * rows;
    if members.is_empty() || total == 0 {
        return Vec::new();
    }
    let (bounds_min_x, bounds_max_x, bounds_min_y, bounds_max_y) = if members.len() > 1 {
        let mut min_x = center_min_x + piece_width;
        let mut max_x = center_max_x - piece_width;
        let mut min_y = center_min_y + piece_height;
        let mut max_y = center_max_y - piece_height;
        if max_x < min_x {
            let mid = (center_min_x + center_max_x) * 0.5;
            min_x = mid;
            max_x = mid;
        }
        if max_y < min_y {
            let mid = (center_min_y + center_max_y) * 0.5;
            min_y = mid;
            max_y = mid;
        }
        (min_x, max_x, min_y, max_y)
    } else {
        (center_min_x, center_max_x, center_min_y, center_max_y)
    };
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
        center_b: (f32, f32),
        rot_b: f32,
        base: (f32, f32),
        dist: f32,
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

    let mut snap_anchor: Option<(usize, (f32, f32), f32)> = None;
    if !candidates.is_empty() {
        let mut best_index = None;
        let mut best_count = 0usize;
        let mut best_error = f32::INFINITY;
        let mut best_dist = f32::INFINITY;
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

            let mut min_cx = f32::INFINITY;
            let mut max_cx = f32::NEG_INFINITY;
            let mut min_cy = f32::INFINITY;
            let mut max_cy = f32::NEG_INFINITY;
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
                && min_cx >= bounds_min_x
                && max_cx <= bounds_max_x
                && min_cy >= bounds_min_y
                && max_cy <= bounds_max_y;
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
        let frame_width = cols as f32 * piece_width;
        let frame_height = rows as f32 * piece_height;
        let corner_snap_distance = snap_distance * frame_snap_ratio;
        let frame_snap_slop = corner_snap_distance * 0.25;
        let group_rot = group_after
            .first()
            .and_then(|id| rotations.get(*id))
            .copied()
            .unwrap_or(0.0);
        let mut corner_snapped = false;
        if group_after.len() < total && corner_snap_distance > 0.0 {
            let mut best_corner = None;
            let target_center_for = |corner: usize, rotation: f32| {
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
                    let target_rot = normalize_angle(steps as f32 * ROTATION_STEP_DEG);
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
                    let mut min_cx = f32::INFINITY;
                    let mut max_cx = f32::NEG_INFINITY;
                    let mut min_cy = f32::INFINITY;
                    let mut max_cy = f32::NEG_INFINITY;
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
                        && min_cx >= frame_min_x - frame_snap_slop
                        && max_cx <= frame_max_x + frame_snap_slop
                        && min_cy >= frame_min_y - frame_snap_slop
                        && max_cy <= frame_max_y + frame_snap_slop;
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
        if !corner_snapped && group_after.len() < total && corner_snap_distance > 0.0 {
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
                    let target_rot = normalize_angle(steps as f32 * ROTATION_STEP_DEG);
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
                    let mut min_cx = f32::INFINITY;
                    let mut max_cx = f32::NEG_INFINITY;
                    let mut min_cy = f32::INFINITY;
                    let mut max_cy = f32::NEG_INFINITY;
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
                        && min_cx >= frame_min_x - frame_snap_slop
                        && max_cx <= frame_max_x + frame_snap_slop
                        && min_cy >= frame_min_y - frame_snap_slop
                        && max_cy <= frame_max_y + frame_snap_slop;
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
        let mut min_cx = f32::INFINITY;
        let mut max_cx = f32::NEG_INFINITY;
        let mut min_cy = f32::INFINITY;
        let mut max_cy = f32::NEG_INFINITY;
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
            let mut any_inside = false;
            for id in clamp_ids {
                if let Some(pos) = positions.get(*id) {
                    let center_x = pos.0 + piece_width * 0.5;
                    let center_y = pos.1 + piece_height * 0.5;
                    if center_x >= bounds_min_x
                        && center_x <= bounds_max_x
                        && center_y >= bounds_min_y
                        && center_y <= bounds_max_y
                    {
                        any_inside = true;
                        break;
                    }
                }
            }
            if !any_inside {
                let mut best_shift = (0.0, 0.0);
                let mut best_dist = f32::INFINITY;
                for id in clamp_ids {
                    if let Some(pos) = positions.get(*id) {
                        let center_x = pos.0 + piece_width * 0.5;
                        let center_y = pos.1 + piece_height * 0.5;
                        let mut shift_x = 0.0;
                        let mut shift_y = 0.0;
                        if center_x < bounds_min_x {
                            shift_x = bounds_min_x - center_x;
                        } else if center_x > bounds_max_x {
                            shift_x = bounds_max_x - center_x;
                        }
                        if center_y < bounds_min_y {
                            shift_y = bounds_min_y - center_y;
                        } else if center_y > bounds_max_y {
                            shift_y = bounds_max_y - center_y;
                        }
                        let dist = shift_x * shift_x + shift_y * shift_y;
                        if dist < best_dist {
                            best_dist = dist;
                            best_shift = (shift_x, shift_y);
                        }
                    }
                }
                if best_dist.is_finite()
                    && (best_shift.0 != 0.0 || best_shift.1 != 0.0)
                {
                    for id in clamp_ids {
                        if let Some(pos) = positions.get_mut(*id) {
                            *pos = (pos.0 + best_shift.0, pos.1 + best_shift.1);
                        }
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

pub(crate) fn build_full_connections(cols: usize, rows: usize) -> Vec<[bool; 4]> {
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

pub(crate) fn scramble_seed(base: u32, nonce: u32, cols: usize, rows: usize) -> u32 {
    let grid = ((cols as u32) << 16) ^ (rows as u32);
    base ^ nonce.wrapping_mul(0x9E37_79B9) ^ grid ^ 0x5CA7_7EED
}

pub(crate) fn scramble_layout(
    seed: u32,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    margin: f32,
) -> (Vec<(f32, f32)>, Vec<usize>) {
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
        let salt = (id as u32) << 1;
        let x = rand_range(seed, salt, min_x, max_x);
        let y = rand_range(seed, salt + 1, min_y, max_y);
        positions.push((x, y));
    }

    let mut order: Vec<usize> = (0..total).collect();
    for i in (1..order.len()).rev() {
        let salt = 0xC0DE_u32 + i as u32;
        let j = (rand_unit(seed, salt) * (i as f32 + 1.0)) as usize;
        order.swap(i, j);
    }
    (positions, order)
}

pub(crate) fn scramble_rotations(seed: u32, total: usize, enabled: bool) -> Vec<f32> {
    if !enabled {
        return vec![0.0; total];
    }
    let mut rotations = Vec::with_capacity(total);
    for id in 0..total {
        let salt = 0xC001_u32 + id as u32;
        rotations.push(rand_range(seed, salt, 0.0, 360.0));
    }
    rotations
}

pub(crate) fn scramble_flips(seed: u32, total: usize, chance: f32) -> Vec<bool> {
    let threshold = chance.clamp(0.0, 1.0);
    let mut flips = Vec::with_capacity(total);
    for id in 0..total {
        let salt = 0xF11F_5EED_u32 + id as u32;
        flips.push(rand_unit(seed, salt) < threshold);
    }
    flips
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
