use std::cell::RefCell;
use std::rc::Rc;

#[cfg(target_arch = "wasm32")]
use js_sys::{Date, Function, Reflect};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsCast;

use crate::core::*;
use crate::input::ClickGesture;
use crate::runtime::CoreAction;
use heddobureika_core::{CoreSnapshot, CoreState, GameRules, PuzzleInfo};

pub(crate) type AppSubscriber = Rc<dyn Fn()>;

const VIEW_ZOOM_MAX: f32 = 4.0;
const VIEW_ZOOM_MIN: f32 = 0.2;
const VIEW_ZOOM_MIN_FACTOR: f32 = 0.5;
const VIEW_PAN_RUBBER_RATIO: f32 = 0.5;
const VIEW_FIT_PADDING_RATIO: f32 = 0.02;

pub(crate) struct AppCore {
    state: RefCell<AppState>,
    snapshots: RefCell<SnapshotBuffer>,
    subscribers: Rc<RefCell<Vec<AppSubscriber>>>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ViewRect {
    pub min_x: f32,
    pub min_y: f32,
    pub width: f32,
    pub height: f32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ViewMode {
    Fit,
    Manual,
}

#[derive(Clone, Copy, Debug)]
struct ViewState {
    viewport_w: f32,
    viewport_h: f32,
    zoom: f32,
    center_x: f32,
    center_y: f32,
    mode: ViewMode,
}

#[derive(Clone)]
pub(crate) struct AppSnapshot {
    pub(crate) puzzle_info: Option<PuzzleInfo>,
    pub(crate) rules: GameRules,
    pub(crate) core: CoreSnapshot,
    pub(crate) grid: GridChoice,
    pub(crate) piece_width: f32,
    pub(crate) piece_height: f32,
    pub(crate) z_order: Vec<usize>,
    pub(crate) hovered_id: Option<usize>,
    pub(crate) active_id: Option<usize>,
    pub(crate) dragging_members: Vec<usize>,
    pub(crate) drag_cursor: Option<(f32, f32)>,
    pub(crate) drag_pointer_id: Option<i32>,
    pub(crate) drag_rotate_mode: bool,
    pub(crate) drag_right_click: bool,
    pub(crate) drag_primary_id: Option<usize>,
    pub(crate) solved: bool,
    pub(crate) layout: WorkspaceLayout,
    pub(crate) view: ViewRect,
    pub(crate) app_settings: AppSettings,
    pub(crate) view_settings: ViewSettings,
}

#[derive(Clone, Copy)]
pub(crate) struct AppSettings {
    pub(crate) theme_mode: ThemeMode,
    pub(crate) show_debug: bool,
}

impl Default for AppSettings {
    fn default() -> Self {
        Self {
            theme_mode: ThemeMode::System,
            show_debug: false,
        }
    }
}

#[derive(Clone)]
pub(crate) struct ViewSettings {
    pub(crate) auto_pan_outer_ratio: f32,
    pub(crate) auto_pan_inner_ratio: f32,
    pub(crate) auto_pan_speed_ratio: f32,
    pub(crate) shape: ShapeSettings,
}

impl Default for ViewSettings {
    fn default() -> Self {
        Self {
            auto_pan_outer_ratio: AUTO_PAN_OUTER_RATIO_DEFAULT,
            auto_pan_inner_ratio: AUTO_PAN_INNER_RATIO_DEFAULT,
            auto_pan_speed_ratio: AUTO_PAN_SPEED_RATIO_DEFAULT,
            shape: ShapeSettings::default(),
        }
    }
}


struct SnapshotBuffer {
    front: AppSnapshot,
    back: AppSnapshot,
}

impl SnapshotBuffer {
    fn new(state: &AppState) -> Self {
        let snapshot = build_snapshot_from_state(state);
        Self {
            front: snapshot.clone(),
            back: snapshot,
        }
    }

    fn refresh_from_state(&mut self, state: &AppState) {
        fill_snapshot_from_state(state, &mut self.back);
        std::mem::swap(&mut self.front, &mut self.back);
    }

    fn mutate_from_front<F>(&mut self, mutator: F)
    where
        F: FnOnce(&mut AppSnapshot),
    {
        self.back.clone_from(&self.front);
        mutator(&mut self.back);
        std::mem::swap(&mut self.front, &mut self.back);
    }
}

#[derive(Clone)]
pub(crate) struct PuzzleAssets {
    pub(crate) info: PuzzleInfo,
    pub(crate) grid: GridChoice,
    pub(crate) pieces: Vec<Piece>,
    pub(crate) paths: Vec<PiecePaths>,
    pub(crate) piece_width: f32,
    pub(crate) piece_height: f32,
    pub(crate) mask_pad: f32,
}

#[derive(Clone)]
struct DragState {
    start_x: f32,
    start_y: f32,
    cursor_x: f32,
    cursor_y: f32,
    click_gesture: ClickGesture,
    primary_id: usize,
    members: Vec<usize>,
    start_positions: Vec<(f32, f32)>,
    start_rotations: Vec<f32>,
    rotate_mode: bool,
    right_click: bool,
    pivot_x: f32,
    pivot_y: f32,
    start_angle: f32,
    pointer_id: Option<i32>,
}

struct AppState {
    core: CoreState,
    assets: Option<Rc<PuzzleAssets>>,
    z_order: Vec<usize>,
    hovered_id: Option<usize>,
    active_id: Option<usize>,
    dragging_members: Vec<usize>,
    drag_state: Option<DragState>,
    view: ViewState,
    app_settings: AppSettings,
    view_settings: ViewSettings,
    renderer_kind: RendererKind,
}

impl AppCore {
    pub(crate) fn new() -> Rc<Self> {
        let state = AppState::new();
        let snapshots = SnapshotBuffer::new(&state);
        Rc::new(Self {
            state: RefCell::new(state),
            snapshots: RefCell::new(snapshots),
            subscribers: Rc::new(RefCell::new(Vec::new())),
        })
    }

    pub(crate) fn subscribe(&self, subscriber: AppSubscriber) -> AppSubscription {
        self.subscribers.borrow_mut().push(subscriber.clone());
        AppSubscription {
            subscriber,
            subscribers: Rc::clone(&self.subscribers),
        }
    }

    fn notify(&self) {
        self.refresh_snapshot_from_state();
        self.notify_subscribers();
    }

    fn notify_subscribers(&self) {
        let subscribers = self.subscribers.borrow().clone();
        for subscriber in subscribers {
            (subscriber)();
        }
    }

    fn notify_snapshot_only(&self) {
        self.notify_subscribers();
    }

    fn refresh_snapshot_from_state(&self) {
        let state = self.state.borrow();
        let mut snapshots = self.snapshots.borrow_mut();
        snapshots.refresh_from_state(&state);
    }

    pub(crate) fn snapshot(&self) -> AppSnapshot {
        self.snapshots.borrow().front.clone()
    }

    pub(crate) fn mutate_snapshot<F>(&self, mutator: F)
    where
        F: FnOnce(&mut AppSnapshot),
    {
        {
            let mut snapshots = self.snapshots.borrow_mut();
            snapshots.mutate_from_front(mutator);
        }
        self.notify_snapshot_only();
    }

    pub(crate) fn assets(&self) -> Option<Rc<PuzzleAssets>> {
        self.state.borrow().assets.clone()
    }

    pub(crate) fn set_puzzle_with_grid(
        &self,
        label: String,
        src: String,
        dims: (u32, u32),
        grid_override: Option<GridChoice>,
    ) {
        self.set_puzzle_with_grid_with_nonce(label, src, dims, grid_override, None);
    }

    pub(crate) fn set_puzzle_with_grid_with_nonce(
        &self,
        label: String,
        src: String,
        dims: (u32, u32),
        grid_override: Option<GridChoice>,
        scramble_nonce: Option<u32>,
    ) {
        let (width, height) = dims;
        if width == 0 || height == 0 {
            return;
        }
        let mut state = self.state.borrow_mut();
        let grid = match grid_override {
            Some(grid) if grid.cols > 0 && grid.rows > 0 => grid,
            Some(_) => return,
            None => select_grid(width, height),
        };
        let info = PuzzleInfo {
            label,
            image_src: src,
            rows: grid.rows,
            cols: grid.cols,
            shape_seed: PUZZLE_SEED,
            image_width: width,
            image_height: height,
        };
        let piece_width = width as f32 / grid.cols as f32;
        let piece_height = height as f32 / grid.rows as f32;
        let depth_cap = state.view_settings.shape.tab_depth_cap.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
        let curve_detail = state.view_settings.shape.curve_detail.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
        let pieces = build_pieces(grid.rows, grid.cols);
        let (horizontal, vertical) = build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &state.view_settings.shape);
        let (horizontal_waves, vertical_waves) = build_line_waves(
            grid.rows,
            grid.cols,
            PUZZLE_SEED,
            piece_width,
            piece_height,
            state.view_settings.shape.line_bend_ratio,
        );
        let warp_field = WarpField {
            width: width as f32,
            height: height as f32,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };
        let mut paths = Vec::with_capacity(pieces.len());
        for piece in &pieces {
            paths.push(build_piece_path(
                piece,
                piece_width,
                piece_height,
                &horizontal,
                &vertical,
                &warp_field,
                depth_cap,
                curve_detail,
            ));
        }
        let max_depth = piece_width.max(piece_height) * depth_cap;
        let max_bend = horizontal_waves
            .iter()
            .chain(vertical_waves.iter())
            .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
        let mask_pad = (max_depth + max_bend).ceil();
        let assets = Rc::new(PuzzleAssets {
            info: info.clone(),
            grid,
            pieces,
            paths,
            piece_width,
            piece_height,
            mask_pad,
        });
        state.core.puzzle_info = Some(info);
        state.assets = Some(assets);
        state.core.grid = grid;
        state.core.piece_width = piece_width;
        state.core.piece_height = piece_height;
        state.core.layout = compute_workspace_layout(
            width as f32,
            height as f32,
            state.core.rules.workspace_padding_ratio,
        );
        let layout = state.core.layout;
        state.view.reset_to_fit(layout);
        let cols = grid.cols as usize;
        let rows = grid.rows as usize;
        let total = cols * rows;
        let view_width = state.core.layout.view_width;
        let view_height = state.core.layout.view_height;
        let view_min_x = state.core.layout.view_min_x;
        let view_min_y = state.core.layout.view_min_y;
        let puzzle_scale = state.core.layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = view_min_x / puzzle_scale;
        let puzzle_view_min_y = view_min_y / puzzle_scale;
        let puzzle_view_width = view_width / puzzle_scale;
        let puzzle_view_height = view_height / puzzle_scale;
        let margin = piece_width.max(piece_height) * (depth_cap + MAX_LINE_BEND_RATIO);
        let nonce = scramble_nonce.unwrap_or_else(|| time_nonce(state.core.scramble_nonce));
        let seed = scramble_seed(PUZZLE_SEED, nonce, cols, rows);
        let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
        let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
        let (positions, order) = scramble_layout(
            seed,
            cols,
            rows,
            piece_width,
            piece_height,
            puzzle_view_min_x,
            puzzle_view_min_y,
            puzzle_view_width,
            puzzle_view_height,
            margin,
        );
        let rotations = scramble_rotations(rotation_seed, total, state.core.rules.rotation_enabled);
        let flips = scramble_flips(flip_seed, total, 0.0);
        let connections = vec![[false; 4]; total];
        let (
            _anchor_of,
            _group_positions,
            _group_rotations,
            _group_order,
            derived_positions,
            derived_rotations,
            piece_order,
        ) = rebuild_group_state(
            &positions,
            &rotations,
            &connections,
            cols,
            rows,
            piece_width,
            piece_height,
            Some(&order),
        );
        state.core.scramble_nonce = nonce;
        state.core.positions = derived_positions;
        state.core.rotations = derived_rotations;
        state.core.flips = flips;
        state.core.connections = connections;
        state.z_order = piece_order;
        state.hovered_id = None;
        state.active_id = None;
        state.dragging_members.clear();
        state.drag_state = None;
        state.core.solved = false;
        drop(state);
        self.notify();
    }

    pub(crate) fn begin_drag(
        &self,
        piece_id: usize,
        x: f32,
        y: f32,
        shift_key: bool,
        rotate_mode: bool,
        right_click: bool,
        pointer_id: Option<i32>,
    ) {
        let mut state = self.state.borrow_mut();
        let total = (state.core.grid.cols as usize) * (state.core.grid.rows as usize);
        if total == 0 || piece_id >= total {
            return;
        }
        let cols = state.core.grid.cols as usize;
        let rows = state.core.grid.rows as usize;
        if shift_key {
            clear_piece_connections(&mut state.core.connections, piece_id, cols, rows);
        }
        let mut members = collect_group(
            &state.core.connections,
            piece_id,
            cols,
            rows,
        );
        if members.is_empty() {
            members.push(piece_id);
        }
        members.sort_unstable();
        let mut start_positions = Vec::with_capacity(members.len());
        let mut start_rotations = Vec::with_capacity(members.len());
        for id in &members {
            if let Some(pos) = state.core.positions.get(*id).copied() {
                start_positions.push(pos);
            } else {
                start_positions.push((0.0, 0.0));
            }
            let rot = state.core.rotations.get(*id).copied().unwrap_or(0.0);
            start_rotations.push(rot);
        }
        let piece_width = state.core.piece_width;
        let piece_height = state.core.piece_height;
        let click_tolerance = piece_width.min(piece_height) * CLICK_MOVE_RATIO;
        let now_ms = now_ms_f32();
        let mut click_gesture = ClickGesture::new_with_slop(click_tolerance);
        click_gesture.arm(x, y, now_ms);
        let base_pos = state
            .core
            .positions
            .get(piece_id)
            .copied()
            .unwrap_or((
                (piece_id % state.core.grid.cols as usize) as f32 * piece_width,
                (piece_id / state.core.grid.cols as usize) as f32 * piece_height,
            ));
        let pivot_x = base_pos.0 + piece_width * 0.5;
        let pivot_y = base_pos.1 + piece_height * 0.5;
        let start_angle = (y - pivot_y).atan2(x - pivot_x);
        state.drag_state = Some(DragState {
            start_x: x,
            start_y: y,
            cursor_x: x,
            cursor_y: y,
            click_gesture,
            primary_id: piece_id,
            members: members.clone(),
            start_positions,
            start_rotations,
            rotate_mode,
            right_click,
            pivot_x,
            pivot_y,
            start_angle,
            pointer_id,
        });
        state.dragging_members = members.clone();
        state.active_id = Some(piece_id);
        state.hovered_id = None;
        bring_members_to_front(&mut state.z_order, &members);
        drop(state);
        self.notify();
    }

    pub(crate) fn drag_move(&self, x: f32, y: f32) {
        let mut state = self.state.borrow_mut();
        let Some(mut drag) = state.drag_state.take() else {
            return;
        };
        drag.cursor_x = x;
        drag.cursor_y = y;
        drag.click_gesture.update(x, y);
        if drag.rotate_mode {
            let piece_width = state.core.piece_width;
            let piece_height = state.core.piece_height;
            let pivot_x = drag.pivot_x;
            let pivot_y = drag.pivot_y;
            let current_angle = (y - pivot_y).atan2(x - pivot_x);
            let delta_deg = (current_angle - drag.start_angle).to_degrees();
            let anchor_id = drag.members.first().copied().unwrap_or(0);
            let flipped = state.core.flips.get(anchor_id).copied().unwrap_or(false);
            let rotation_delta = if flipped { -delta_deg } else { delta_deg };
            for (idx, id) in drag.members.iter().enumerate() {
                let start_pos = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
                let center_x = start_pos.0 + piece_width * 0.5;
                let center_y = start_pos.1 + piece_height * 0.5;
                let (rx, ry) = rotate_point(center_x, center_y, pivot_x, pivot_y, delta_deg);
                if let Some(pos) = state.core.positions.get_mut(*id) {
                    *pos = (rx - piece_width * 0.5, ry - piece_height * 0.5);
                }
                if let Some(rot) = state.core.rotations.get_mut(*id) {
                    let start_rot = drag.start_rotations.get(idx).copied().unwrap_or(*rot);
                    *rot = normalize_angle(start_rot + rotation_delta);
                }
            }
        } else {
            let dx = x - drag.start_x;
            let dy = y - drag.start_y;
            let mut dx = dx;
            let mut dy = dy;
            if !drag.start_positions.is_empty() {
                let piece_width = state.core.piece_width;
                let piece_height = state.core.piece_height;
                let layout = state.core.layout;
                let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
                let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
                let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
                let puzzle_view_width = layout.view_width / puzzle_scale;
                let puzzle_view_height = layout.view_height / puzzle_scale;
                let center_min_x = puzzle_view_min_x + piece_width * 0.5;
                let center_min_y = puzzle_view_min_y + piece_height * 0.5;
                let mut center_max_x = puzzle_view_min_x + puzzle_view_width - piece_width * 0.5;
                let mut center_max_y = puzzle_view_min_y + puzzle_view_height - piece_height * 0.5;
                if center_max_x < center_min_x {
                    center_max_x = center_min_x;
                }
                if center_max_y < center_min_y {
                    center_max_y = center_min_y;
                }
                let rubber_limit = piece_width.min(piece_height) * RUBBER_BAND_RATIO;
                let (bounds_min_x, bounds_max_x, bounds_min_y, bounds_max_y) =
                    if drag.members.len() > 1 {
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
                        (
                            center_min_x,
                            center_max_x,
                            center_min_y,
                            center_max_y,
                        )
                    };
                let mut in_bounds = false;
                let mut best_dx = dx;
                let mut best_dy = dy;
                let mut best_dist = f32::INFINITY;
                for start in &drag.start_positions {
                    let center_x = start.0 + piece_width * 0.5;
                    let center_y = start.1 + piece_height * 0.5;
                    let min_dx = bounds_min_x - center_x;
                    let max_dx = bounds_max_x - center_x;
                    let min_dy = bounds_min_y - center_y;
                    let max_dy = bounds_max_y - center_y;
                    if dx >= min_dx && dx <= max_dx && dy >= min_dy && dy <= max_dy {
                        in_bounds = true;
                        break;
                    }
                    let cand_dx = rubber_band_clamp(dx, min_dx, max_dx, rubber_limit);
                    let cand_dy = rubber_band_clamp(dy, min_dy, max_dy, rubber_limit);
                    let delta_dx = cand_dx - dx;
                    let delta_dy = cand_dy - dy;
                    let dist = delta_dx * delta_dx + delta_dy * delta_dy;
                    if dist < best_dist {
                        best_dist = dist;
                        best_dx = cand_dx;
                        best_dy = cand_dy;
                    }
                }
                if !in_bounds {
                    dx = best_dx;
                    dy = best_dy;
                }
            }
            for (idx, id) in drag.members.iter().enumerate() {
                if let Some(pos) = state.core.positions.get_mut(*id) {
                    let start = drag.start_positions.get(idx).copied().unwrap_or(*pos);
                    *pos = (start.0 + dx, start.1 + dy);
                }
            }
        }
        state.drag_state = Some(drag);
        drop(state);
        self.notify();
    }

    pub(crate) fn drag_end(&self, pointer_id: Option<i32>) {
        let mut state = self.state.borrow_mut();
        let Some(drag) = state.drag_state.take() else {
            return;
        };
        if drag.pointer_id.is_some() && pointer_id.is_none() {
            state.drag_state = Some(drag);
            return;
        }
        if pointer_id.is_some() && drag.pointer_id != pointer_id {
            state.drag_state = Some(drag);
            return;
        }
        let cols = state.core.grid.cols as usize;
        let rows = state.core.grid.rows as usize;
        let total = cols * rows;
        if total == 0 {
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        }
        let piece_width = state.core.piece_width;
        let piece_height = state.core.piece_height;
        let layout = state.core.layout;
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
        let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
        let puzzle_view_width = layout.view_width / puzzle_scale;
        let puzzle_view_height = layout.view_height / puzzle_scale;
        let center_min_x = puzzle_view_min_x + piece_width * 0.5;
        let center_min_y = puzzle_view_min_y + piece_height * 0.5;
        let mut center_max_x = puzzle_view_min_x + puzzle_view_width - piece_width * 0.5;
        let mut center_max_y = puzzle_view_min_y + puzzle_view_height - piece_height * 0.5;
        if center_max_x < center_min_x {
            center_max_x = center_min_x;
        }
        if center_max_y < center_min_y {
            center_max_y = center_min_y;
        }
        let snap_distance = piece_width.min(piece_height) * state.core.rules.snap_distance_ratio;
        let complete_snap = !state.core.solved;
        let flips = state.core.flips.clone();
        let frame_snap_ratio = state.core.rules.frame_snap_ratio;
        let rotation_snap_tolerance = state.core.rules.rotation_snap_tolerance_deg;
        let rotation_enabled = state.core.rules.rotation_enabled;
        let click_tolerance = piece_width.min(piece_height) * CLICK_MOVE_RATIO;
        let click_tolerance_sq = click_tolerance * click_tolerance;
        let moved = drag
            .members
            .iter()
            .enumerate()
            .any(|(idx, id)| {
                let start = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
                let current = state.core.positions.get(*id).copied().unwrap_or(start);
                let dx = current.0 - start.0;
                let dy = current.1 - start.1;
                dx * dx + dy * dy > click_tolerance_sq
            });
        let is_click = drag
            .click_gesture
            .is_click_with_external_moved(now_ms_f32(), moved);
        let click_id = drag.primary_id;
        let was_flipped = state.core.flips.get(click_id).copied().unwrap_or(false);
        if is_click && drag.rotate_mode {
            if let Some(flip) = state.core.flips.get_mut(click_id) {
                *flip = !*flip;
            }
            clear_piece_connections(&mut state.core.connections, click_id, cols, rows);
            let order_snapshot = if state.z_order.len() == total {
                Some(state.z_order.as_slice())
            } else {
                None
            };
            let (
                _anchor_of,
                _group_positions,
                _group_rotations,
                _group_order,
                derived_positions,
                derived_rotations,
                piece_order,
            ) = rebuild_group_state(
                &state.core.positions,
                &state.core.rotations,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                order_snapshot,
            );
            state.core.positions = derived_positions;
            state.core.rotations = derived_rotations;
            state.z_order = piece_order;
            state.core.solved = is_solved(
                &state.core.positions,
                &state.core.rotations,
                &state.core.flips,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                state.core.rules.rotation_enabled,
            );
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        }
        if is_click && was_flipped {
            if let Some(flip) = state.core.flips.get_mut(click_id) {
                *flip = false;
            }
            clear_piece_connections(&mut state.core.connections, click_id, cols, rows);
            let order_snapshot = if state.z_order.len() == total {
                Some(state.z_order.as_slice())
            } else {
                None
            };
            let (
                _anchor_of,
                _group_positions,
                _group_rotations,
                _group_order,
                derived_positions,
                derived_rotations,
                piece_order,
            ) = rebuild_group_state(
                &state.core.positions,
                &state.core.rotations,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                order_snapshot,
            );
            state.core.positions = derived_positions;
            state.core.rotations = derived_rotations;
            state.z_order = piece_order;
            state.core.solved = is_solved(
                &state.core.positions,
                &state.core.rotations,
                &state.core.flips,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                state.core.rules.rotation_enabled,
            );
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        }
        if is_click && rotation_enabled && !drag.rotate_mode && !drag.members.is_empty() {
            let members = drag.members.clone();
            let group_size = members.len();
            let rotation_locked = group_size == total || group_size > ROTATION_LOCK_THRESHOLD_DEFAULT;
            let anchor_id = members[0];
            let current_angle = state.core.rotations.get(anchor_id).copied().unwrap_or(0.0);
            if group_size > 1
                && rotation_locked
                && angle_matches(current_angle, 0.0, rotation_snap_tolerance)
            {
                state.drag_state = None;
                state.dragging_members.clear();
                state.active_id = None;
                drop(state);
                self.notify();
                return;
            }
            let mut delta = click_rotation_delta(current_angle, 0.0, 0.0, rotation_snap_tolerance);
            if drag.right_click {
                delta = -delta;
            }
            for (idx, id) in members.iter().enumerate() {
                let start_pos = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
                let center_x = start_pos.0 + piece_width * 0.5;
                let center_y = start_pos.1 + piece_height * 0.5;
                let (rx, ry) = rotate_point(center_x, center_y, drag.start_x, drag.start_y, delta);
                if let Some(pos) = state.core.positions.get_mut(*id) {
                    *pos = (rx - piece_width * 0.5, ry - piece_height * 0.5);
                }
                if let Some(rot) = state.core.rotations.get_mut(*id) {
                    let base = drag.start_rotations.get(idx).copied().unwrap_or(*rot);
                    *rot = normalize_angle(base + delta);
                }
            }
            {
                let (positions, rotations, connections) = {
                    let state = &mut *state;
                    (&mut state.core.positions, &mut state.core.rotations, &mut state.core.connections)
                };
                apply_snaps_for_group(
                    &members,
                    positions,
                    rotations,
                    &flips,
                    connections,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    snap_distance,
                    frame_snap_ratio,
                    complete_snap,
                    center_min_x,
                    center_max_x,
                    center_min_y,
                    center_max_y,
                    layout.view_min_x,
                    layout.view_min_y,
                    layout.view_width,
                    layout.view_height,
                    rotation_snap_tolerance,
                    rotation_enabled,
                );
            }
            let order_snapshot = if state.z_order.len() == total {
                Some(state.z_order.as_slice())
            } else {
                None
            };
            let (
                _anchor_of,
                _group_positions,
                _group_rotations,
                _group_order,
                derived_positions,
                derived_rotations,
                piece_order,
            ) = rebuild_group_state(
                &state.core.positions,
                &state.core.rotations,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                order_snapshot,
            );
            state.core.positions = derived_positions;
            state.core.rotations = derived_rotations;
            state.z_order = piece_order;
            state.core.solved = is_solved(
                &state.core.positions,
                &state.core.rotations,
                &state.core.flips,
                &state.core.connections,
                cols,
                rows,
                piece_width,
                piece_height,
                state.core.rules.rotation_enabled,
            );
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        }
        {
            let (positions, rotations, connections) = {
                let state = &mut *state;
                (&mut state.core.positions, &mut state.core.rotations, &mut state.core.connections)
            };
            apply_snaps_for_group(
                &drag.members,
                positions,
                rotations,
                &flips,
                connections,
                cols,
                rows,
                piece_width,
                piece_height,
                snap_distance,
                frame_snap_ratio,
                complete_snap,
                center_min_x,
                center_max_x,
                center_min_y,
                center_max_y,
                layout.view_min_x,
                layout.view_min_y,
                layout.view_width,
                layout.view_height,
                rotation_snap_tolerance,
                rotation_enabled,
            );
        }
        let order_snapshot = if state.z_order.len() == total {
            Some(state.z_order.as_slice())
        } else {
            None
        };
        let (
            _anchor_of,
            _group_positions,
            _group_rotations,
            _group_order,
            derived_positions,
            derived_rotations,
            piece_order,
        ) = rebuild_group_state(
            &state.core.positions,
            &state.core.rotations,
            &state.core.connections,
            cols,
            rows,
            piece_width,
            piece_height,
            order_snapshot,
        );
        state.core.positions = derived_positions;
        state.core.rotations = derived_rotations;
        state.z_order = piece_order;
        state.core.solved = is_solved(
            &state.core.positions,
            &state.core.rotations,
            &state.core.flips,
            &state.core.connections,
            cols,
            rows,
            piece_width,
            piece_height,
            state.core.rules.rotation_enabled,
        );
        state.drag_state = None;
        state.dragging_members.clear();
        state.active_id = None;
        drop(state);
        self.notify();
    }

    pub(crate) fn cancel_drag(&self) {
        let mut state = self.state.borrow_mut();
        if state.drag_state.is_none() {
            return;
        }
        state.drag_state = None;
        state.dragging_members.clear();
        state.active_id = None;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_hovered(&self, hovered: Option<usize>) {
        let mut state = self.state.borrow_mut();
        if state.hovered_id == hovered {
            return;
        }
        state.hovered_id = hovered;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_workspace_padding_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(WORKSPACE_PADDING_RATIO_MIN, WORKSPACE_PADDING_RATIO_MAX);
        if (state.core.rules.workspace_padding_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.core.rules.workspace_padding_ratio = value;
        let (width, height) = state
            .core
            .puzzle_info
            .as_ref()
            .map(|info| (info.image_width as f32, info.image_height as f32))
            .unwrap_or((1.0, 1.0));
        state.core.layout = compute_workspace_layout(width, height, value);
        let layout = state.core.layout;
        state.view.reset_to_fit(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn set_image_max_dim(&self, value: u32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
        if state.core.rules.image_max_dimension == value {
            return;
        }
        state.core.rules.image_max_dimension = value;
        let (width, height) = state
            .core
            .puzzle_info
            .as_ref()
            .map(|info| (info.image_width as f32, info.image_height as f32))
            .unwrap_or((1.0, 1.0));
        state.core.layout = compute_workspace_layout(width, height, state.core.rules.workspace_padding_ratio);
        let layout = state.core.layout;
        state.view.reset_to_fit(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn set_viewport_size(&self, width: f32, height: f32) {
        if width <= 0.0 || height <= 0.0 {
            return;
        }
        let mut state = self.state.borrow_mut();
        let width = width.max(1.0);
        let height = height.max(1.0);
        if (state.view.viewport_w - width).abs() <= f32::EPSILON
            && (state.view.viewport_h - height).abs() <= f32::EPSILON
        {
            return;
        }
        state.view.viewport_w = width;
        state.view.viewport_h = height;
        let layout = state.core.layout;
        match state.view.mode {
            ViewMode::Fit => state.view.reset_to_fit(layout),
            ViewMode::Manual => {
                state.view.zoom = state.view.clamp_zoom(state.view.zoom, layout);
                state.view.clamp_to_layout(layout);
            }
        }
        drop(state);
        self.notify();
    }

    pub(crate) fn pan_view(&self, dx_world: f32, dy_world: f32) {
        if dx_world == 0.0 && dy_world == 0.0 {
            return;
        }
        let mut state = self.state.borrow_mut();
        state.view.mode = ViewMode::Manual;
        state.view.center_x += dx_world;
        state.view.center_y += dy_world;
        let layout = state.core.layout;
        state.view.zoom = state.view.clamp_zoom(state.view.zoom, layout);
        state.view.clamp_to_layout_elastic(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn zoom_view_at(&self, factor: f32, anchor_world_x: f32, anchor_world_y: f32) {
        if factor <= 0.0 {
            return;
        }
        let mut state = self.state.borrow_mut();
        let old_zoom = state.view.zoom.max(1.0e-4);
        let layout = state.core.layout;
        let new_zoom = state.view.clamp_zoom(old_zoom * factor, layout);
        if (new_zoom - old_zoom).abs() <= f32::EPSILON {
            return;
        }
        let ratio = old_zoom / new_zoom;
        state.view.center_x =
            anchor_world_x - (anchor_world_x - state.view.center_x) * ratio;
        state.view.center_y =
            anchor_world_y - (anchor_world_y - state.view.center_y) * ratio;
        state.view.zoom = new_zoom;
        state.view.mode = ViewMode::Manual;
        state.view.clamp_to_layout(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn reset_view_to_fit(&self) {
        let mut state = self.state.borrow_mut();
        let layout = state.core.layout;
        state.view.reset_to_fit(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn fit_view_to_frame(&self) {
        let mut state = self.state.borrow_mut();
        let Some(info) = state.core.puzzle_info.as_ref() else {
            return;
        };
        let layout = state.core.layout;
        let frame_width = info.image_width as f32 * layout.puzzle_scale.max(1.0e-4);
        let frame_height = info.image_height as f32 * layout.puzzle_scale.max(1.0e-4);
        let fit_zoom = state.view.fit_zoom_for_size(frame_width, frame_height);
        state.view.zoom = state.view.clamp_zoom(fit_zoom, layout);
        state.view.center_x = frame_width * 0.5;
        state.view.center_y = frame_height * 0.5;
        state.view.mode = ViewMode::Manual;
        state.view.clamp_to_layout(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn settle_view(&self) {
        let mut state = self.state.borrow_mut();
        let layout = state.core.layout;
        state.view.clamp_to_layout(layout);
        drop(state);
        self.notify();
    }

    pub(crate) fn image_max_dim(&self) -> u32 {
        self.state.borrow().core.rules.image_max_dimension
    }

    pub(crate) fn set_renderer_kind(&self, kind: RendererKind) {
        let mut state = self.state.borrow_mut();
        if state.renderer_kind == kind {
            return;
        }
        state.renderer_kind = kind;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_theme_mode(&self, mode: ThemeMode) {
        let mut state = self.state.borrow_mut();
        if state.app_settings.theme_mode == mode {
            return;
        }
        state.app_settings.theme_mode = mode;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_show_debug(&self, enabled: bool) {
        let mut state = self.state.borrow_mut();
        if state.app_settings.show_debug == enabled {
            return;
        }
        state.app_settings.show_debug = enabled;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_auto_pan_outer_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(AUTO_PAN_OUTER_RATIO_MIN, AUTO_PAN_OUTER_RATIO_MAX);
        if (state.view_settings.auto_pan_outer_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.view_settings.auto_pan_outer_ratio = value;
        if state.view_settings.auto_pan_inner_ratio < value {
            state.view_settings.auto_pan_inner_ratio = value;
        }
        drop(state);
        self.notify();
    }

    pub(crate) fn set_auto_pan_inner_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let mut value = value.clamp(AUTO_PAN_INNER_RATIO_MIN, AUTO_PAN_INNER_RATIO_MAX);
        if value < state.view_settings.auto_pan_outer_ratio {
            value = state.view_settings.auto_pan_outer_ratio;
        }
        if (state.view_settings.auto_pan_inner_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.view_settings.auto_pan_inner_ratio = value;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_auto_pan_speed_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(AUTO_PAN_SPEED_RATIO_MIN, AUTO_PAN_SPEED_RATIO_MAX);
        if (state.view_settings.auto_pan_speed_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.view_settings.auto_pan_speed_ratio = value;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_rotation_enabled(&self, enabled: bool) {
        let mut state = self.state.borrow_mut();
        if state.core.rules.rotation_enabled == enabled {
            return;
        }
        state.core.rules.rotation_enabled = enabled;
        drop(state);
        self.notify();
    }

    pub(crate) fn rotation_enabled(&self) -> bool {
        self.state.borrow().core.rules.rotation_enabled
    }

    pub(crate) fn set_rotation_snap_tolerance(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(ROTATION_SNAP_TOLERANCE_MIN_DEG, ROTATION_SNAP_TOLERANCE_MAX_DEG);
        if (state.core.rules.rotation_snap_tolerance_deg - value).abs() <= f32::EPSILON {
            return;
        }
        state.core.rules.rotation_snap_tolerance_deg = value;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_snap_distance_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(SNAP_DISTANCE_RATIO_MIN, SNAP_DISTANCE_RATIO_MAX);
        if (state.core.rules.snap_distance_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.core.rules.snap_distance_ratio = value;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_frame_snap_ratio(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(FRAME_SNAP_MIN, FRAME_SNAP_MAX);
        if (state.core.rules.frame_snap_ratio - value).abs() <= f32::EPSILON {
            return;
        }
        state.core.rules.frame_snap_ratio = value;
        drop(state);
        self.notify();
    }

    pub(crate) fn set_shape_settings(&self, settings: ShapeSettings) {
        let mut state = self.state.borrow_mut();
        if state.view_settings.shape == settings {
            return;
        }
        state.view_settings.shape = settings;
        let Some(info) = state.core.puzzle_info.clone() else {
            drop(state);
            self.notify();
            return;
        };
        let grid = state.core.grid;
        let piece_width = info.image_width as f32 / grid.cols as f32;
        let piece_height = info.image_height as f32 / grid.rows as f32;
        let depth_cap = state.view_settings.shape.tab_depth_cap.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
        let curve_detail = state.view_settings.shape.curve_detail.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
        let pieces = build_pieces(grid.rows, grid.cols);
        let (horizontal, vertical) = build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &state.view_settings.shape);
        let (horizontal_waves, vertical_waves) = build_line_waves(
            grid.rows,
            grid.cols,
            PUZZLE_SEED,
            piece_width,
            piece_height,
            state.view_settings.shape.line_bend_ratio,
        );
        let warp_field = WarpField {
            width: info.image_width as f32,
            height: info.image_height as f32,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };
        let mut paths = Vec::with_capacity(pieces.len());
        for piece in &pieces {
            paths.push(build_piece_path(
                piece,
                piece_width,
                piece_height,
                &horizontal,
                &vertical,
                &warp_field,
                depth_cap,
                curve_detail,
            ));
        }
        let max_depth = piece_width.max(piece_height) * depth_cap;
        let max_bend = horizontal_waves
            .iter()
            .chain(vertical_waves.iter())
            .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
        let mask_pad = (max_depth + max_bend).ceil();
        state.assets = Some(Rc::new(PuzzleAssets {
            info,
            grid,
            pieces,
            paths,
            piece_width,
            piece_height,
            mask_pad,
        }));
        state.core.piece_width = piece_width;
        state.core.piece_height = piece_height;
        drop(state);
        self.notify();
    }

    pub(crate) fn apply_snapshot(
        &self,
        positions: Vec<(f32, f32)>,
        rotations: Vec<f32>,
        flips: Vec<bool>,
        connections: Vec<[bool; 4]>,
        z_order: Vec<usize>,
        scramble_nonce: u32,
    ) {
        self.apply_snapshot_with_drag(
            positions,
            rotations,
            flips,
            connections,
            z_order,
            scramble_nonce,
            false,
        );
    }

    pub(crate) fn apply_snapshot_with_drag(
        &self,
        positions: Vec<(f32, f32)>,
        rotations: Vec<f32>,
        flips: Vec<bool>,
        connections: Vec<[bool; 4]>,
        z_order: Vec<usize>,
        scramble_nonce: u32,
        preserve_drag: bool,
    ) {
        let mut state = self.state.borrow_mut();
        state.core.positions = positions;
        state.core.rotations = rotations;
        state.core.flips = flips;
        state.core.connections = connections;
        state.z_order = z_order;
        state.core.scramble_nonce = scramble_nonce;
        let cols = state.core.grid.cols as usize;
        let rows = state.core.grid.rows as usize;
        if cols > 0 && rows > 0 {
            state.core.solved = is_solved(
                &state.core.positions,
                &state.core.rotations,
                &state.core.flips,
                &state.core.connections,
                cols,
                rows,
                state.core.piece_width,
                state.core.piece_height,
                state.core.rules.rotation_enabled,
            );
        } else {
            state.core.solved = false;
        }
        if !preserve_drag {
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
        }
        drop(state);
        self.notify();
    }

    pub(crate) fn apply_action(&self, action: CoreAction) {
        match action {
            CoreAction::BeginDrag {
                piece_id,
                x,
                y,
                shift_key,
                rotate_mode,
                right_click,
                pointer_id,
            } => self.begin_drag(
                piece_id,
                x,
                y,
                shift_key,
                rotate_mode,
                right_click,
                pointer_id,
            ),
            CoreAction::DragMove { x, y } => self.drag_move(x, y),
            CoreAction::DragEnd { pointer_id } => self.drag_end(pointer_id),
            CoreAction::SetHovered { hovered } => self.set_hovered(hovered),
            CoreAction::Sync(_) => {}
        }
    }

}

fn build_snapshot_from_state(state: &AppState) -> AppSnapshot {
    let mut snapshot = AppSnapshot {
        puzzle_info: None,
        rules: state.core.rules,
        core: CoreSnapshot {
            positions: Vec::new(),
            rotations: Vec::new(),
            flips: Vec::new(),
            connections: Vec::new(),
            group_order: Vec::new(),
            scramble_nonce: 0,
        },
        grid: state.core.grid,
        piece_width: state.core.piece_width,
        piece_height: state.core.piece_height,
        z_order: Vec::new(),
        hovered_id: None,
        active_id: None,
        dragging_members: Vec::new(),
        drag_cursor: None,
        drag_pointer_id: None,
        drag_rotate_mode: false,
        drag_right_click: false,
        drag_primary_id: None,
        solved: false,
        layout: state.core.layout,
        view: state.view.view_rect(),
        app_settings: state.app_settings,
        view_settings: state.view_settings.clone(),
    };
    fill_snapshot_from_state(state, &mut snapshot);
    snapshot
}

fn fill_snapshot_from_state(state: &AppState, snapshot: &mut AppSnapshot) {
    snapshot.puzzle_info = state.core.puzzle_info.clone();
    snapshot.rules = state.core.rules;
    fill_core_snapshot(state, &mut snapshot.core);
    snapshot.grid = state.core.grid;
    snapshot.piece_width = state.core.piece_width;
    snapshot.piece_height = state.core.piece_height;
    snapshot.z_order.clone_from(&state.z_order);
    snapshot.hovered_id = state.hovered_id;
    snapshot.active_id = state.active_id;
    snapshot
        .dragging_members
        .clone_from(&state.dragging_members);
    snapshot.drag_cursor = state
        .drag_state
        .as_ref()
        .map(|drag| (drag.cursor_x, drag.cursor_y));
    snapshot.drag_pointer_id = state.drag_state.as_ref().and_then(|drag| drag.pointer_id);
    snapshot.drag_rotate_mode = state
        .drag_state
        .as_ref()
        .map(|drag| drag.rotate_mode)
        .unwrap_or(false);
    snapshot.drag_right_click = state
        .drag_state
        .as_ref()
        .map(|drag| drag.right_click)
        .unwrap_or(false);
    snapshot.drag_primary_id = state.drag_state.as_ref().map(|drag| drag.primary_id);
    snapshot.solved = state.core.solved;
    snapshot.layout = state.core.layout;
    snapshot.view = state.view.view_rect();
    snapshot.app_settings = state.app_settings;
    snapshot.view_settings = state.view_settings.clone();
}

fn fill_core_snapshot(state: &AppState, snapshot: &mut CoreSnapshot) {
    snapshot.positions.clone_from(&state.core.positions);
    snapshot.rotations.clone_from(&state.core.rotations);
    snapshot.flips.clone_from(&state.core.flips);
    snapshot.connections.clone_from(&state.core.connections);
    snapshot.scramble_nonce = state.core.scramble_nonce;
    snapshot.group_order.clear();
    if let Some(info) = state.core.puzzle_info.as_ref() {
        let cols = info.cols as usize;
        let rows = info.rows as usize;
        let total = cols.saturating_mul(rows);
        if total > 0
            && state.core.positions.len() == total
            && state.core.rotations.len() == total
            && state.core.flips.len() == total
            && state.core.connections.len() == total
        {
            let piece_order = if state.z_order.len() == total {
                state.z_order.clone()
            } else {
                (0..total).collect()
            };
            let anchor_of = anchor_of_from_connections(&state.core.connections, cols, rows);
            snapshot.group_order.extend(
                build_group_order_from_piece_order(&piece_order, &anchor_of)
                    .into_iter()
                    .filter_map(|id| u32::try_from(id).ok()),
            );
        }
    }
}

fn anchor_of_from_connections(connections: &[[bool; 4]], cols: usize, rows: usize) -> Vec<usize> {
    let total = cols.saturating_mul(rows);
    let mut anchor_of = vec![0usize; total];
    let groups = groups_from_connections(connections, cols, rows);
    for group in groups {
        if group.is_empty() {
            continue;
        }
        let anchor = group[0];
        for id in group {
            if id < total {
                anchor_of[id] = anchor;
            }
        }
    }
    anchor_of
}

impl ViewState {
    fn new(layout: WorkspaceLayout) -> Self {
        let mut state = Self {
            viewport_w: layout.view_width.max(1.0),
            viewport_h: layout.view_height.max(1.0),
            zoom: 1.0,
            center_x: 0.0,
            center_y: 0.0,
            mode: ViewMode::Fit,
        };
        state.reset_to_fit(layout);
        state
    }

    fn view_rect(&self) -> ViewRect {
        let zoom = self.zoom.max(1.0e-4);
        let width = (self.viewport_w / zoom).max(1.0e-3);
        let height = (self.viewport_h / zoom).max(1.0e-3);
        ViewRect {
            min_x: self.center_x - width * 0.5,
            min_y: self.center_y - height * 0.5,
            width,
            height,
        }
    }

    fn fit_zoom(&self, layout: WorkspaceLayout) -> f32 {
        self.fit_zoom_for_size(layout.view_width, layout.view_height)
    }

    fn fit_zoom_for_size(&self, width: f32, height: f32) -> f32 {
        let viewport_w = self.viewport_w.max(1.0);
        let viewport_h = self.viewport_h.max(1.0);
        let target_w = width.max(1.0) * (1.0 + VIEW_FIT_PADDING_RATIO);
        let target_h = height.max(1.0) * (1.0 + VIEW_FIT_PADDING_RATIO);
        (viewport_w / target_w).min(viewport_h / target_h)
    }

    fn clamp_zoom(&self, zoom: f32, layout: WorkspaceLayout) -> f32 {
        let fit_zoom = self.fit_zoom(layout);
        let min_zoom = (fit_zoom * VIEW_ZOOM_MIN_FACTOR)
            .max(VIEW_ZOOM_MIN)
            .min(VIEW_ZOOM_MAX);
        let max_zoom = VIEW_ZOOM_MAX;
        zoom.clamp(min_zoom, max_zoom)
    }

    fn reset_to_fit(&mut self, layout: WorkspaceLayout) {
        let fit_zoom = self.fit_zoom(layout);
        self.zoom = self.clamp_zoom(fit_zoom, layout);
        self.center_x = layout.view_min_x + layout.view_width * 0.5;
        self.center_y = layout.view_min_y + layout.view_height * 0.5;
        self.mode = ViewMode::Fit;
        self.clamp_to_layout(layout);
    }

    fn pan_bounds(&self, layout: WorkspaceLayout) -> (f32, f32, f32, f32) {
        let view = self.view_rect();
        let min_x = layout.view_min_x;
        let min_y = layout.view_min_y;
        let max_x = min_x + layout.view_width;
        let max_y = min_y + layout.view_height;
        let (min_cx, max_cx) = (min_x - view.width * 0.5, max_x + view.width * 0.5);
        let (min_cy, max_cy) = (min_y - view.height * 0.5, max_y + view.height * 0.5);
        (min_cx, max_cx, min_cy, max_cy)
    }

    fn clamp_to_layout(&mut self, layout: WorkspaceLayout) {
        let (min_cx, max_cx, min_cy, max_cy) = self.pan_bounds(layout);
        self.center_x = self.center_x.clamp(min_cx, max_cx);
        self.center_y = self.center_y.clamp(min_cy, max_cy);
    }

    fn clamp_to_layout_elastic(&mut self, layout: WorkspaceLayout) {
        let view = self.view_rect();
        let (min_cx, max_cx, min_cy, max_cy) = self.pan_bounds(layout);
        let min_dim = view.width.min(view.height).max(1.0);
        let rubber_limit = min_dim * VIEW_PAN_RUBBER_RATIO;
        let clamp_axis = |value: f32, min: f32, max: f32| {
            let range = (max - min).max(0.0);
            let axis_limit = rubber_limit.min(range * 0.5);
            if axis_limit <= 0.0 {
                return value.clamp(min, max);
            }
            let elastic_min = min + axis_limit;
            let elastic_max = max - axis_limit;
            rubber_band_clamp(value, elastic_min, elastic_max, axis_limit)
        };

        self.center_x = clamp_axis(self.center_x, min_cx, max_cx);
        self.center_y = clamp_axis(self.center_y, min_cy, max_cy);
    }
}

pub(crate) struct AppSubscription {
    subscriber: AppSubscriber,
    subscribers: Rc<RefCell<Vec<AppSubscriber>>>,
}

impl Drop for AppSubscription {
    fn drop(&mut self) {
        let mut subscribers = self.subscribers.borrow_mut();
        subscribers.retain(|item| !Rc::ptr_eq(item, &self.subscriber));
    }
}

impl AppState {
    fn new() -> Self {
        let core = CoreState::new();
        let view = ViewState::new(core.layout);
        Self {
            core,
            assets: None,
            z_order: Vec::new(),
            hovered_id: None,
            active_id: None,
            dragging_members: Vec::new(),
            drag_state: None,
            view,
            app_settings: AppSettings::default(),
            view_settings: ViewSettings::default(),
            renderer_kind: RendererKind::Wgpu,
        }
    }
}

fn bring_members_to_front(order: &mut Vec<usize>, members: &[usize]) {
    if order.is_empty() || members.is_empty() {
        return;
    }
    let mut keep = Vec::with_capacity(order.len());
    for id in order.iter().copied() {
        if !members.contains(&id) {
            keep.push(id);
        }
    }
    keep.extend_from_slice(members);
    *order = keep;
}

fn select_grid(width: u32, height: u32) -> GridChoice {
    let choices = build_grid_choices(width, height);
    if choices.is_empty() {
        return FALLBACK_GRID;
    }
    choices
        .iter()
        .find(|choice| choice.target_count == DEFAULT_TARGET_COUNT)
        .copied()
        .unwrap_or_else(|| choices[0])
}

fn time_nonce(previous: u32) -> u32 {
    #[cfg(target_arch = "wasm32")]
    {
        let now = Date::now() as u32;
        return splitmix32(now ^ previous.wrapping_add(0x9E37_79B9));
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|duration| duration.as_millis() as u32)
            .unwrap_or(0);
        return splitmix32(now ^ previous.wrapping_add(0x9E37_79B9));
    }
}

fn now_ms_f32() -> f32 {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(window) = web_sys::window() {
            if let Ok(perf) = Reflect::get(&window, &"performance".into()) {
                if let Ok(now_fn) =
                    Reflect::get(&perf, &"now".into()).and_then(|value| value.dyn_into::<Function>())
                {
                    if let Ok(value) = now_fn.call0(&perf) {
                        if let Some(ms) = value.as_f64() {
                            return ms as f32;
                        }
                    }
                }
            }
        }
        return (Date::now() % 1_000_000.0) as f32;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        return std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|duration| duration.as_millis() as f32)
            .unwrap_or(0.0);
    }
}
