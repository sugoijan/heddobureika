pub use heddobureika_core::game::*;
#[allow(unused_imports)]
pub(crate) use heddobureika_core::{
    build_grid_choices, clamp_custom_piece_count, grid_choice_index, grid_choice_label,
    nearest_valid_grid, GridChoice, CUSTOM_PIECE_COUNT_MAX, CUSTOM_PIECE_COUNT_MIN,
    DEFAULT_TARGET_COUNT, FALLBACK_GRID, SOLVE_TIME_EXPONENT, SOLVE_TIME_FACTOR,
};
use serde::{Deserialize, Serialize};
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
pub(crate) const TOUCH_DRAG_SLOP_PX: f32 = 4.0;
pub(crate) const RUBBER_BAND_RATIO: f32 = 0.35;
pub(crate) const TAB_WIDTH_MIN: f32 = 0.2;
pub(crate) const TAB_WIDTH_MAX: f32 = 0.72;
pub(crate) const TAB_DEPTH_MIN: f32 = 0.2;
pub(crate) const TAB_DEPTH_MAX: f32 = 1.1;
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
pub(crate) const CORNER_RADIUS_RATIO: f32 = 0.05;

#[derive(
    Clone,
    Copy,
    PartialEq,
    Serialize,
    Deserialize,
    rkyv::Archive,
    rkyv::Serialize,
    rkyv::Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub(crate) enum ThemeMode {
    System,
    Light,
    Dark,
}

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Serialize,
    Deserialize,
    rkyv::Archive,
    rkyv::Serialize,
    rkyv::Deserialize,
)]
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

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Serialize,
    Deserialize,
    rkyv::Archive,
    rkyv::Serialize,
    rkyv::Deserialize,
)]
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

#[derive(
    Clone, PartialEq, Serialize, Deserialize, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize,
)]
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

#[derive(
    Clone, PartialEq, Serialize, Deserialize, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize,
)]
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

#[derive(
    Clone, PartialEq, Serialize, Deserialize, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize,
)]
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

pub(crate) use heddobureika_core::GridShapeSettings as ShapeSettings;
pub(crate) use heddobureika_core::PuzzleRenderGeometry;

pub(crate) fn is_border_piece(row: usize, col: usize, rows: usize, cols: usize) -> bool {
    row == 0 || row + 1 == rows || col == 0 || col + 1 == cols
}

/// Builds the legacy grid `[UP, RIGHT, DOWN, LEFT]` connection table from a
/// `PlayableState` whose topology is known to be `cols × rows` grid. Used
/// exclusively by the debug HUD and the legacy yew preview — not by the
/// renderer pipeline, which works in topology-agnostic terms.
pub(crate) fn grid_piece_connections_from_playable<T>(
    playable: &heddobureika_core::PlayableState<T>,
    cols: usize,
    rows: usize,
) -> Vec<[bool; 4]>
where
    T: heddobureika_core::PuzzleTopology,
{
    let total = cols.saturating_mul(rows);
    if total == 0 || playable.piece_count() != total {
        return Vec::new();
    }
    let mut out = vec![[false; 4]; total];
    for edge_idx in 0..playable.logical.edge_count() {
        let edge = heddobureika_core::EdgeId(edge_idx as u32);
        if playable.logical.is_edge_active(edge) != Some(true) {
            continue;
        }
        let (a, b) = playable.logical.topology.edge_endpoints(edge);
        let a_idx = a.as_usize();
        let b_idx = b.as_usize();
        if a_idx >= total || b_idx >= total {
            continue;
        }
        if b_idx == a_idx + 1 && a_idx / cols == b_idx / cols {
            out[a_idx][DIR_RIGHT] = true;
            out[b_idx][DIR_LEFT] = true;
        } else if a_idx == b_idx + 1 && a_idx / cols == b_idx / cols {
            out[a_idx][DIR_LEFT] = true;
            out[b_idx][DIR_RIGHT] = true;
        } else if b_idx == a_idx + cols {
            out[a_idx][DIR_DOWN] = true;
            out[b_idx][DIR_UP] = true;
        } else if a_idx == b_idx + cols {
            out[a_idx][DIR_UP] = true;
            out[b_idx][DIR_DOWN] = true;
        }
    }
    out
}

/// Topology-agnostic border-connection counter. Counts every topology
/// edge whose two endpoints both lie on the puzzle's outer frame and
/// reports `(active_count, total_count)`. Works for any topology that
/// defines `is_frame_border_piece` — grid, triangular, future Voronoi.
pub(crate) fn count_border_connections<T>(
    playable: &heddobureika_core::PlayableState<T>,
) -> (usize, usize)
where
    T: heddobureika_core::PuzzleTopology,
{
    let edge_count = playable.logical.edge_count();
    let mut total = 0;
    let mut active = 0;
    for idx in 0..edge_count {
        let edge = heddobureika_core::EdgeId(idx as u32);
        let (a, b) = playable.logical.topology.edge_endpoints(edge);
        if playable.logical.topology.is_frame_border_piece(a)
            && playable.logical.topology.is_frame_border_piece(b)
        {
            total += 1;
            if playable.logical.is_edge_active(edge) == Some(true) {
                active += 1;
            }
        }
    }
    (active, total)
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

pub(crate) fn rotate_point(
    x: f32,
    y: f32,
    origin_x: f32,
    origin_y: f32,
    angle_deg: f32,
) -> (f32, f32) {
    let (dx, dy) = (x - origin_x, y - origin_y);
    let (rx, ry) = rotate_vec(dx, dy, angle_deg);
    (origin_x + rx, origin_y + ry)
}
