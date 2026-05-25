use std::cell::RefCell;
use std::rc::Rc;

#[cfg(target_arch = "wasm32")]
use js_sys::{Date, Function, Reflect};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsCast;

use crate::core::*;
use crate::game_state::AppGameState;
use crate::input::ClickGesture;
use crate::runtime::CoreAction;
use heddobureika_core::{
    angle_delta, build_topology_from_spec, rand_range, safety_corrections_after_detach,
    scramble_flips, validate_image_ref, AngleDeg, CoreState, FlipState, GameRules, MergePolicy,
    PieceId, PlayableAction, Pose2, Position2, PuzzleImageRef, PuzzleInfo, PuzzleTopology,
    RestrictedPlayableAction, TopologySpec, DEFAULT_TAB_DEPTH_CAP, FLIP_CHANCE,
    MAX_LINE_BEND_RATIO,
};

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

#[cfg(target_arch = "wasm32")]
fn log_puzzle_dimensions(
    label: &str,
    image_ref: &PuzzleImageRef,
    width: u32,
    height: u32,
    grid: GridChoice,
) {
    gloo::console::log!(
        "puzzle load",
        label,
        format!("image_ref={:?}", image_ref),
        format!("dims={}x{}", width, height),
        format!("grid={}x{}", grid.cols, grid.rows)
    );
}

#[cfg(not(target_arch = "wasm32"))]
fn log_puzzle_dimensions(
    _label: &str,
    _image_ref: &PuzzleImageRef,
    _width: u32,
    _height: u32,
    _grid: GridChoice,
) {
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
    /// Full authoritative game state. Cloned from `AppState.game` into each
    /// snapshot so renderers and bridges can read from a `PlayableState` /
    /// `VisualState` directly.
    pub(crate) game: Option<AppGameState>,
    /// Piece world poses in mm (center-of-piece coords), derived from
    /// `state.game.visual.piece_visual_pose`.
    pub(crate) piece_world_poses: Vec<Pose2>,
    pub(crate) piece_top_left_px: Vec<(f32, f32)>,
    /// Per-piece flip state, derived from the piece's group's flip in
    /// `state.game.playable.group_flip`.
    pub(crate) piece_flipped: Vec<bool>,
    /// Per-piece group anchor (the canonical group id = min piece in group).
    pub(crate) piece_group_anchor: Vec<u32>,
    pub(crate) scramble_nonce: u32,
    /// Pose-mm → pixel scale per axis. Use this (not `piece_width`/`piece_height`)
    /// for any mm↔px conversion. Equals `render_geometry.pose_unit_px` when
    /// a puzzle is loaded; `[1.0, 1.0]` otherwise.
    pub(crate) pose_unit_px: [f32; 2],
    /// Pose-mm origin in pixel coords. Equals `render_geometry.pose_origin_px`
    /// when loaded; `[0.0, 0.0]` otherwise.
    pub(crate) pose_origin_px: [f32; 2],
    /// Per-axis minimum piece bounding box across all pieces (px). Use this
    /// for UX tolerances that should react to the smallest piece in the
    /// puzzle.
    pub(crate) min_piece_extent_px: [f32; 2],
    /// Per-axis median piece bounding box across all pieces (px). Use this
    /// as a "typical piece size" for scramble margins and tolerances that
    /// should reflect the puzzle's overall scale.
    pub(crate) typical_piece_extent_px: [f32; 2],
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

impl AppSnapshot {
    /// Materializes piece pixel-coord top-left positions as a Vec.
    pub(crate) fn piece_positions_px(&self) -> Vec<(f32, f32)> {
        self.piece_top_left_px.clone()
    }

    /// Materializes piece rotations in degrees as a Vec.
    pub(crate) fn piece_rotations_deg(&self) -> Vec<f32> {
        self.piece_world_poses
            .iter()
            .map(|pose| pose.rotation_degrees())
            .collect()
    }

    pub(crate) fn group_members_for_piece(&self, piece_id: usize) -> Vec<usize> {
        if piece_id >= self.piece_group_anchor.len() {
            return Vec::new();
        }
        let anchor = self.piece_group_anchor[piece_id];
        self.piece_group_anchor
            .iter()
            .enumerate()
            .filter_map(|(idx, value)| (*value == anchor).then_some(idx))
            .collect()
    }
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
}

#[derive(Clone)]
pub(crate) struct PuzzleAssets {
    pub(crate) info: PuzzleInfo,
    pub(crate) grid: GridChoice,
    pub(crate) topology: TopologySpec,
    pub(crate) render_geometry: PuzzleRenderGeometry,
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
    click_slop: f32,
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
    /// Authoritative game state. Populated whenever a puzzle is loaded;
    /// `None` only on the empty default state.
    game: Option<AppGameState>,
    assets: Option<Rc<PuzzleAssets>>,
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

    /// Reads a piece's pose from `state.game.visual` and converts it to the
    /// legacy `((x_px, y_px), rot_deg)` format used by drag/snap math.
    fn piece_pixel_pose(state: &AppState, piece_id: usize) -> ((f32, f32), f32) {
        let Some(game) = state.game.as_ref() else {
            return ((0.0, 0.0), 0.0);
        };
        let pose = game
            .visual
            .piece_visual_pose()
            .get(piece_id)
            .copied()
            .unwrap_or_default();
        let top_left = state
            .assets
            .as_ref()
            .and_then(|assets| {
                assets
                    .render_geometry
                    .pose_to_piece_top_left(PieceId(piece_id as u32), pose)
            })
            .unwrap_or((
                pose.x_mm() * state.core.piece_width - state.core.piece_width * 0.5,
                pose.y_mm() * state.core.piece_height - state.core.piece_height * 0.5,
            ));
        (top_left, pose.rotation_degrees())
    }

    /// Reads a piece's flip state from `state.game.playable.group_flip`.
    fn piece_is_flipped(state: &AppState, piece_id: usize) -> bool {
        let Some(game) = state.game.as_ref() else {
            return false;
        };
        let piece = PieceId(piece_id as u32);
        match game.playable.logical.group_of(piece) {
            Some(group) => game.playable.flip_of(group) == Some(FlipState::Flipped),
            None => false,
        }
    }

    /// Writes a piece's pose into `state.game.visual.piece_visual_pose`,
    /// converting the legacy `((x_px, y_px), rot_deg)` format to mm/degree
    /// `Pose2`.
    fn set_piece_pixel_pose(state: &mut AppState, piece_id: usize, px: (f32, f32), rot_deg: f32) {
        let piece_width = state.core.piece_width;
        let piece_height = state.core.piece_height;
        if piece_width <= 0.0 || piece_height <= 0.0 {
            return;
        }
        let pose = state
            .assets
            .as_ref()
            .and_then(|assets| {
                assets
                    .render_geometry
                    .pixel_to_pose(PieceId(piece_id as u32), px, rot_deg)
            })
            .or_else(|| {
                let mm_x = (px.0 + piece_width * 0.5) / piece_width;
                let mm_y = (px.1 + piece_height * 0.5) / piece_height;
                Pose2::try_from_mm_degrees(mm_x, mm_y, rot_deg)
            });
        let Some(game) = state.game.as_mut() else {
            return;
        };
        if let Some(pose) = pose {
            game.visual
                .set_piece_visual_pose(PieceId(piece_id as u32), pose);
        }
    }

    fn refresh_snapshot_from_state(&self) {
        let state = self.state.borrow();
        let mut snapshots = self.snapshots.borrow_mut();
        snapshots.refresh_from_state(&state);
    }

    pub(crate) fn snapshot(&self) -> AppSnapshot {
        self.snapshots.borrow().front.clone()
    }

    pub(crate) fn assets(&self) -> Option<Rc<PuzzleAssets>> {
        self.state.borrow().assets.clone()
    }

    pub(crate) fn set_puzzle_with_topology(
        &self,
        label: String,
        image_ref: PuzzleImageRef,
        dims: (u32, u32),
        descriptor: TopologySpec,
        scramble_nonce: Option<u32>,
    ) {
        self.set_puzzle_with_topology_seeded(
            label,
            image_ref,
            dims,
            descriptor,
            scramble_nonce,
            random_shape_seed(),
        );
    }

    pub(crate) fn set_puzzle_with_topology_seeded(
        &self,
        label: String,
        image_ref: PuzzleImageRef,
        dims: (u32, u32),
        descriptor: TopologySpec,
        scramble_nonce: Option<u32>,
        shape_seed: u32,
    ) {
        let (width, height) = dims;
        if width == 0 || height == 0 {
            return;
        }
        if validate_image_ref(&image_ref).is_err() {
            return;
        }
        let mut state = self.state.borrow_mut();
        let topology = match build_topology_from_spec(&descriptor) {
            Some(topology) => topology,
            None => return,
        };
        let grid =
            descriptor_grid_choice(&descriptor, topology.piece_count()).unwrap_or_else(|| {
                GridChoice {
                    target_count: topology.piece_count(),
                    cols: topology.piece_count().max(1),
                    rows: 1,
                    actual_count: topology.piece_count(),
                }
            });
        log_puzzle_dimensions(&label, &image_ref, width, height, grid);
        let info = PuzzleInfo {
            label,
            image_ref,
            topology: descriptor.clone().into(),
            shape_seed,
            image_width: width,
            image_height: height,
        };
        let Some((render_geometry, piece_width, piece_height, mask_pad)) =
            build_assets_for_topology(&info, &descriptor, grid, &state.view_settings)
        else {
            return;
        };
        let assets = Rc::new(PuzzleAssets {
            info: info.clone(),
            grid,
            topology: descriptor.clone(),
            render_geometry,
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
        let total = topology.piece_count() as usize;
        let view_width = state.core.layout.view_width;
        let view_height = state.core.layout.view_height;
        let view_min_x = state.core.layout.view_min_x;
        let view_min_y = state.core.layout.view_min_y;
        let puzzle_scale = state.core.layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = view_min_x / puzzle_scale;
        let puzzle_view_min_y = view_min_y / puzzle_scale;
        let puzzle_view_width = view_width / puzzle_scale;
        let puzzle_view_height = view_height / puzzle_scale;
        let margin = piece_width
            .max(piece_height)
            .mul_add(DEFAULT_TAB_DEPTH_CAP + MAX_LINE_BEND_RATIO, mask_pad);
        let nonce = scramble_nonce.unwrap_or_else(|| time_nonce(state.core.scramble_nonce));
        let seed = scramble_seed_from_topology(PUZZLE_SEED, nonce, &descriptor);
        let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
        let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
        let (positions, order) = scramble_layout_for_geometry(
            seed,
            state
                .assets
                .as_ref()
                .map(|assets| &assets.render_geometry)
                .expect("assets installed before scramble"),
            puzzle_view_min_x,
            puzzle_view_min_y,
            puzzle_view_width,
            puzzle_view_height,
            margin,
        );
        let rotations = scramble_rotations(rotation_seed, total, state.core.rules.rotation_enabled);
        let flips = scramble_flips(flip_seed, total, FLIP_CHANCE);
        state.core.scramble_nonce = nonce;
        if let Some(info) = state.core.puzzle_info.clone() {
            state.game = AppGameState::scrambled(
                info,
                state.core.rules,
                descriptor.clone(),
                &state
                    .assets
                    .as_ref()
                    .expect("assets installed before game")
                    .render_geometry,
                nonce,
                &positions,
                &rotations,
                &flips,
                &order,
            )
            .ok();
        } else {
            state.game = None;
        }
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
        click_slop: f32,
        pointer_id: Option<i32>,
    ) {
        let mut state = self.state.borrow_mut();
        let total = state
            .game
            .as_ref()
            .map(|game| game.playable.piece_count())
            .unwrap_or(0);
        if total == 0 || piece_id >= total {
            return;
        }
        // Apply shift-key split as a `DetachPieceAsGroup` mutation on
        // state.game so the split persists through drag_move/drag_end.
        // Multi-piece groups can sit outside the tight "safe area" because
        // drag_move uses the same loose bound for groups as for singles; when
        // a Shift-detach breaks such a group up, force-move each resulting
        // group back to its applicable safety bound (loose for singletons,
        // tight for multi-piece). This is the optimistic local apply — the
        // server runs the exact same `safety_corrections_after_detach` logic
        // server-side as part of its Detach handler, so the wire echo will
        // confirm the same corrected poses.
        if shift_key {
            let rules = state.core.rules;
            let pose_unit_px = state.assets.as_ref().map(|assets| {
                (
                    assets.render_geometry.pose_unit_px[0],
                    assets.render_geometry.pose_unit_px[1],
                )
            });
            if let Some(game) = state.game.as_mut() {
                let piece = PieceId(piece_id as u32);
                let pose = game
                    .visual
                    .piece_visual_pose()
                    .get(piece_id)
                    .copied()
                    .unwrap_or_default();
                let flip = game
                    .playable
                    .logical
                    .group_of(piece)
                    .and_then(|g| game.playable.flip_of(g))
                    .unwrap_or(FlipState::Normal);
                let original_members: Vec<PieceId> = game
                    .playable
                    .logical
                    .group_of(piece)
                    .map(|g| game.playable.logical.members_of(g).collect())
                    .unwrap_or_else(|| vec![piece]);
                let _ = game.playable.apply_restricted_action_batch(
                    RestrictedPlayableAction::DetachPieceAsGroup {
                        piece,
                        target_pose: pose,
                        target_flip: flip,
                    },
                    None,
                );
                let puzzle_info = game.puzzle.clone();
                // Pose units default to topology-derived scale when no
                // render geometry is loaded yet. Works for grid,
                // triangular, and any future topology because it goes
                // through `image_extent_in_pose_units`.
                let pose_unit_px = pose_unit_px.unwrap_or_else(|| {
                    build_topology_from_spec(&puzzle_info.to_spec())
                        .map(|t| {
                            let (ex, ey) = t.image_extent_in_pose_units();
                            (
                                puzzle_info.image_width as f32 / ex.max(1.0),
                                puzzle_info.image_height as f32 / ey.max(1.0),
                            )
                        })
                        .unwrap_or((1.0, 1.0))
                });
                let corrections = safety_corrections_after_detach(
                    &game.playable,
                    &original_members,
                    &puzzle_info,
                    &rules,
                    pose_unit_px,
                );
                for (group, drop_pos) in corrections {
                    let _ = game.playable.apply_action_only(
                        PlayableAction::TranslateGroup { group, drop_pos },
                        None,
                    );
                }
                game.rebuild_visual();
            }
        }
        // Walk the group containing piece_id directly on state.game.
        let mut members: Vec<usize> = if let Some(game) = state.game.as_ref() {
            let piece = PieceId(piece_id as u32);
            match game.playable.logical.group_of(piece) {
                Some(group) => game
                    .playable
                    .logical
                    .members_of(group)
                    .map(|p| p.as_usize())
                    .collect(),
                None => Vec::new(),
            }
        } else {
            Vec::new()
        };
        if members.is_empty() {
            members.push(piece_id);
        }
        members.sort_unstable();
        let mut start_positions = Vec::with_capacity(members.len());
        let mut start_rotations = Vec::with_capacity(members.len());
        for id in &members {
            let ((px, py), rot) = Self::piece_pixel_pose(&state, *id);
            start_positions.push((px, py));
            start_rotations.push(rot);
        }
        let piece_width = state.core.piece_width;
        let piece_height = state.core.piece_height;
        let min_piece_extent = state
            .assets
            .as_ref()
            .map(|assets| {
                let [w, h] = assets.render_geometry.min_piece_extent_px;
                w.min(h)
            })
            .unwrap_or_else(|| piece_width.min(piece_height));
        let click_tolerance = min_piece_extent * CLICK_MOVE_RATIO;
        let click_slop = click_slop.max(click_tolerance);
        let now_ms = now_ms_f32();
        let mut click_gesture = ClickGesture::new_with_slop(click_slop);
        click_gesture.arm(x, y, now_ms);
        let base_pos = Self::piece_pixel_pose(&state, piece_id).0;
        let anchor = state
            .assets
            .as_ref()
            .and_then(|assets| {
                assets
                    .render_geometry
                    .piece(PieceId(piece_id as u32))
                    .map(|piece| piece.pose_anchor_px)
            })
            .unwrap_or([piece_width * 0.5, piece_height * 0.5]);
        let pivot_x = base_pos.0 + anchor[0];
        let pivot_y = base_pos.1 + anchor[1];
        let start_angle = (y - pivot_y).atan2(x - pivot_x);
        state.drag_state = Some(DragState {
            start_x: x,
            start_y: y,
            cursor_x: x,
            cursor_y: y,
            click_gesture,
            click_slop,
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
        // Bring the dragged group to the top of the z-stack. The anchor
        // piece id is `members[0]` (members is sorted ascending). On
        // PlayableState that's a `SetGroupOrder` restricted action which
        // promotes the named anchors to the back of `z_order` (= top of
        // the render stack).
        if let Some(game) = state.game.as_mut() {
            let anchors = vec![members[0] as u32];
            game.playable.set_z_order_by_anchors(&anchors);
            game.rebuild_visual();
        }
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
            let flipped = Self::piece_is_flipped(&state, anchor_id);
            let rotation_delta = if flipped { -delta_deg } else { delta_deg };
            for (idx, id) in drag.members.iter().enumerate() {
                let start_pos = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
                let anchor = state
                    .assets
                    .as_ref()
                    .and_then(|assets| {
                        assets
                            .render_geometry
                            .piece(PieceId(*id as u32))
                            .map(|piece| piece.pose_anchor_px)
                    })
                    .unwrap_or([piece_width * 0.5, piece_height * 0.5]);
                let center_x = start_pos.0 + anchor[0];
                let center_y = start_pos.1 + anchor[1];
                let (rx, ry) = rotate_point(center_x, center_y, pivot_x, pivot_y, delta_deg);
                let new_px = (rx - anchor[0], ry - anchor[1]);
                let start_rot = drag.start_rotations.get(idx).copied().unwrap_or(0.0);
                let new_rot = normalize_angle(start_rot + rotation_delta);
                Self::set_piece_pixel_pose(&mut state, *id, new_px, new_rot);
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
                let min_extent = state
                    .assets
                    .as_ref()
                    .map(|assets| {
                        let [w, h] = assets.render_geometry.min_piece_extent_px;
                        w.min(h)
                    })
                    .unwrap_or_else(|| piece_width.min(piece_height));
                let rubber_limit = min_extent * RUBBER_BAND_RATIO;
                // Single bound for both single pieces and multi-piece groups —
                // the tighter group-only inset was removed because it was
                // disproportionately restrictive for puzzles with few/large
                // pieces. Groups that get broken up by Shift-drag now have
                // their resulting sub-groups force-moved back into the safe
                // area at detach time (see `enforce_workspace_safety_after_detach`).
                let (bounds_min_x, bounds_max_x, bounds_min_y, bounds_max_y) =
                    (center_min_x, center_max_x, center_min_y, center_max_y);
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
                let start = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
                let new_px = (start.0 + dx, start.1 + dy);
                let rot = drag.start_rotations.get(idx).copied().unwrap_or(0.0);
                Self::set_piece_pixel_pose(&mut state, *id, new_px, rot);
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
        let total = state
            .game
            .as_ref()
            .map(|game| game.playable.piece_count())
            .unwrap_or(0);
        let piece_width = state.core.piece_width;
        let piece_height = state.core.piece_height;
        let rotation_snap_tolerance = state.core.rules.rotation_snap_tolerance_deg;
        let rotation_enabled = state.core.rules.rotation_enabled;
        let click_tolerance = drag.click_slop;
        let click_tolerance_sq = click_tolerance * click_tolerance;
        let click_id = drag.primary_id;
        let primary_piece = PieceId(click_id as u32);
        let render_geometry = state
            .assets
            .as_ref()
            .map(|assets| assets.render_geometry.clone());
        // World-space pivot for a click-driven flip/unflip: the press point,
        // converted from puzzle-pixel coords to pose-mm units. Pinning this
        // point keeps it under the cursor across the toggle so the user can
        // immediately grab the (un)flipped piece without it sliding away.
        let flip_pivot = render_geometry.as_ref().and_then(|geom| {
            let [ox, oy] = geom.pose_origin_px;
            let [ux, uy] = geom.pose_unit_px;
            if ux > 0.0 && uy > 0.0 {
                Position2::try_from_mm((drag.start_x - ox) / ux, (drag.start_y - oy) / uy)
            } else {
                None
            }
        });
        let Some(game) = state.game.as_mut() else {
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        };
        if total == 0 || game.playable.piece_count() != total {
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        }
        let Some(primary_group) = game.playable.logical.group_of(primary_piece) else {
            state.drag_state = None;
            state.dragging_members.clear();
            state.active_id = None;
            drop(state);
            self.notify();
            return;
        };
        // Check whether the drag moved beyond the click threshold by sampling
        // the current visual pose of each member against its start position.
        // For Ctrl-drag rotations the piece can rotate around its click point
        // without its center translating, so also treat any rotation delta
        // beyond the snap tolerance as movement — otherwise a held Ctrl+drag
        // would be misinterpreted as a click-flip and lose the rotation.
        let visual_poses = game.visual.piece_visual_pose();
        let moved = drag.members.iter().enumerate().any(|(idx, id)| {
            let start = drag.start_positions.get(idx).copied().unwrap_or((0.0, 0.0));
            let start_rot = drag.start_rotations.get(idx).copied().unwrap_or(0.0);
            let pose = visual_poses.get(*id).copied().unwrap_or_default();
            let piece = PieceId(*id as u32);
            let top_left = render_geometry
                .as_ref()
                .and_then(|geom| geom.pose_to_piece_top_left(piece, pose))
                .unwrap_or((
                    pose.x_mm() * piece_width - piece_width * 0.5,
                    pose.y_mm() * piece_height - piece_height * 0.5,
                ));
            let dx = top_left.0 - start.0;
            let dy = top_left.1 - start.1;
            if dx * dx + dy * dy > click_tolerance_sq {
                return true;
            }
            let rot_delta = angle_delta(pose.rotation_degrees(), start_rot).abs();
            rot_delta > rotation_snap_tolerance
        });
        let is_click = drag
            .click_gesture
            .is_click_with_external_moved(now_ms_f32(), moved);
        let was_flipped = game.playable.flip_of(primary_group) == Some(FlipState::Flipped);
        // Click while in rotate-mode toggles flip.
        if is_click && drag.rotate_mode {
            game.playable.apply_restricted_action_batch(
                RestrictedPlayableAction::FlipGroup {
                    group: primary_group,
                    pivot: flip_pivot,
                },
                None,
            );
            game.rebuild_visual();
            self.finalize_drag(&mut state);
            drop(state);
            self.notify();
            return;
        }
        // Click on a flipped piece un-flips it.
        if is_click && was_flipped {
            game.playable.apply_action_with_snap(
                PlayableAction::UnflipGroup {
                    group: primary_group,
                    pivot: flip_pivot,
                },
                None,
                MergePolicy::KeepFixedGroup,
            );
            game.rebuild_visual();
            self.finalize_drag(&mut state);
            drop(state);
            self.notify();
            return;
        }
        // Click with rotation enabled (and not rotate-mode): snap the group's
        // current rotation toward the nearest cardinal angle.
        if is_click && rotation_enabled && !drag.rotate_mode && !drag.members.is_empty() {
            let group_size = drag.members.len();
            let rotation_locked =
                group_size == total || group_size > ROTATION_LOCK_THRESHOLD_DEFAULT;
            let current_pose = game.playable.pose_of(primary_group).unwrap_or_default();
            let current_angle = current_pose.rotation_degrees();
            if group_size > 1
                && rotation_locked
                && angle_matches(current_angle, 0.0, rotation_snap_tolerance)
            {
                self.finalize_drag(&mut state);
                drop(state);
                self.notify();
                return;
            }
            // Topology-aware step rotation: ask the playable state for the
            // next discrete rotation toward the relevant symmetry angles
            // (cardinal 90° for grid, 60°/120° for triangular pieces, etc.).
            // Falls back to the legacy cardinal-step delta when no step is
            // available.
            let clockwise = !drag.right_click;
            let delta = match game
                .playable
                .next_step_rotation(primary_group, clockwise)
                .map(|angle| angle.as_degrees_f32())
            {
                Some(target) => angle_delta(target, current_angle),
                None => {
                    let mut d =
                        click_rotation_delta(current_angle, 0.0, 0.0, rotation_snap_tolerance);
                    if drag.right_click {
                        d = -d;
                    }
                    d
                }
            };
            // Compute the new anchor pose: rotate the anchor's current world
            // position around the click point by delta, and add delta to its
            // rotation. The mm↔px conversion uses the topology's pose units
            // (which can differ from piece_width/piece_height — e.g. for
            // triangular tessellation where the row height divides the image
            // by piece_rows rather than rows).
            let (pose_unit_x, pose_unit_y, pose_origin_x, pose_origin_y) = render_geometry
                .as_ref()
                .map(|geom| {
                    (
                        geom.pose_unit_px[0],
                        geom.pose_unit_px[1],
                        geom.pose_origin_px[0],
                        geom.pose_origin_px[1],
                    )
                })
                .unwrap_or((piece_width, piece_height, 0.0, 0.0));
            let anchor_x_px = pose_origin_x + current_pose.x_mm() * pose_unit_x;
            let anchor_y_px = pose_origin_y + current_pose.y_mm() * pose_unit_y;
            let (rx_px, ry_px) =
                rotate_point(anchor_x_px, anchor_y_px, drag.start_x, drag.start_y, delta);
            let new_x_mm = (rx_px - pose_origin_x) / pose_unit_x;
            let new_y_mm = (ry_px - pose_origin_y) / pose_unit_y;
            let new_rot = normalize_angle(current_angle + delta);
            let Some(drop_pos) = Position2::try_from_mm(new_x_mm, new_y_mm) else {
                self.finalize_drag(&mut state);
                drop(state);
                self.notify();
                return;
            };
            let Some(drop_rotation) = AngleDeg::try_new(new_rot) else {
                self.finalize_drag(&mut state);
                drop(state);
                self.notify();
                return;
            };
            game.playable.apply_action_with_snap(
                PlayableAction::TransformGroupTo {
                    group: primary_group,
                    drop_pos,
                    drop_rotation,
                },
                None,
                MergePolicy::KeepFixedGroup,
            );
            game.rebuild_visual();
            self.finalize_drag(&mut state);
            drop(state);
            self.notify();
            return;
        }
        // Regular drag end: commit the current visual pose of the dragged
        // group, then let `apply_action_with_snap` run the snap algorithm.
        let anchor_piece = game
            .playable
            .anchor_piece_of_group(primary_group)
            .unwrap_or(primary_piece);
        let visual_pose = game
            .visual
            .piece_visual_pose()
            .get(anchor_piece.as_usize())
            .copied()
            .unwrap_or_default();
        let Some(drop_pos) = Position2::try_from_mm(visual_pose.x_mm(), visual_pose.y_mm()) else {
            self.finalize_drag(&mut state);
            drop(state);
            self.notify();
            return;
        };
        if rotation_enabled {
            let Some(drop_rotation) = AngleDeg::try_new(visual_pose.rotation_degrees()) else {
                self.finalize_drag(&mut state);
                drop(state);
                self.notify();
                return;
            };
            game.playable.apply_action_with_snap(
                PlayableAction::TransformGroupTo {
                    group: primary_group,
                    drop_pos,
                    drop_rotation,
                },
                None,
                MergePolicy::KeepFixedGroup,
            );
        } else {
            game.playable.apply_action_with_snap(
                PlayableAction::TranslateGroup {
                    group: primary_group,
                    drop_pos,
                },
                None,
                MergePolicy::KeepFixedGroup,
            );
        }
        game.rebuild_visual();
        self.finalize_drag(&mut state);
        drop(state);
        self.notify();
    }

    /// Updates derived bookkeeping after a drag finalizes (whether snap
    /// happened or not). Reads back from the now-authoritative `state.game`.
    fn finalize_drag(&self, state: &mut AppState) {
        if let Some(game) = state.game.as_ref() {
            state.core.solved = game.playable.is_solved();
        }
        state.drag_state = None;
        state.dragging_members.clear();
        state.active_id = None;
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
        state.core.layout =
            compute_workspace_layout(width, height, state.core.rules.workspace_padding_ratio);
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
        state.view.center_x = anchor_world_x - (anchor_world_x - state.view.center_x) * ratio;
        state.view.center_y = anchor_world_y - (anchor_world_y - state.view.center_y) * ratio;
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

    /// Zeros the rotation of every group's pose. Used by the dev panel's
    /// "solve rotation" button and as a side effect when rotation is
    /// disabled.
    pub(crate) fn clear_all_group_rotations(&self) {
        let mut state = self.state.borrow_mut();
        let Some(game) = state.game.as_mut() else {
            return;
        };
        for pose in game.playable.group_pose.iter_mut() {
            if let Some(zeroed) = Pose2::try_from_mm_degrees(pose.x_mm(), pose.y_mm(), 0.0) {
                *pose = zeroed;
            }
        }
        game.rebuild_visual();
        drop(state);
        self.notify();
    }

    /// Sets every group's flip state to `Normal`. Used by the dev panel's
    /// "unflip" button.
    pub(crate) fn clear_all_group_flips(&self) {
        let mut state = self.state.borrow_mut();
        let Some(game) = state.game.as_mut() else {
            return;
        };
        for flip in game.playable.group_flip.iter_mut() {
            *flip = FlipState::Normal;
        }
        game.rebuild_visual();
        drop(state);
        self.notify();
    }

    /// Rescrambles the current puzzle in place: regenerates per-piece
    /// positions/rotations/flips with a new nonce and installs the result.
    /// Dev-panel "scramble" button.
    pub(crate) fn rescramble(&self) {
        let (
            info,
            rules,
            layout,
            current_nonce,
            piece_width,
            piece_height,
            mask_pad,
            descriptor,
            geometry,
        ) = {
            let state = self.state.borrow();
            let Some(info) = state.core.puzzle_info.clone() else {
                return;
            };
            let Some(assets) = state.assets.as_ref() else {
                return;
            };
            let piece_width = state.core.piece_width;
            let piece_height = state.core.piece_height;
            if piece_width <= 0.0 || piece_height <= 0.0 {
                return;
            }
            (
                info,
                state.core.rules,
                state.core.layout,
                state.core.scramble_nonce,
                piece_width,
                piece_height,
                assets.mask_pad,
                assets.topology.clone(),
                assets.render_geometry.clone(),
            )
        };
        let total = geometry.pieces.len();
        if total == 0 {
            return;
        }
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
        let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
        let puzzle_view_width = layout.view_width / puzzle_scale;
        let puzzle_view_height = layout.view_height / puzzle_scale;
        let margin = piece_width
            .max(piece_height)
            .mul_add(DEFAULT_TAB_DEPTH_CAP + MAX_LINE_BEND_RATIO, mask_pad);
        let nonce = time_nonce(current_nonce);
        let seed = scramble_seed_from_topology(PUZZLE_SEED, nonce, &info.to_spec());
        let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
        let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
        let (positions, order) = scramble_layout_for_geometry(
            seed,
            &geometry,
            puzzle_view_min_x,
            puzzle_view_min_y,
            puzzle_view_width,
            puzzle_view_height,
            margin,
        );
        let rotations = scramble_rotations(rotation_seed, total, rules.rotation_enabled);
        let flips = scramble_flips(flip_seed, total, FLIP_CHANCE);
        let Ok(game) = AppGameState::scrambled(
            info, rules, descriptor, &geometry, nonce, &positions, &rotations, &flips, &order,
        ) else {
            return;
        };
        self.install_game(game, false);
    }

    /// Replaces the live state with a fully-solved puzzle: every edge
    /// active, the single resulting group anchored at the origin with no
    /// rotation or flip. Dev-panel "solve" button.
    pub(crate) fn solve_puzzle(&self) {
        let (info, rules, descriptor) = {
            let state = self.state.borrow();
            (
                state.core.puzzle_info.clone(),
                state.core.rules,
                state.assets.as_ref().map(|assets| assets.topology.clone()),
            )
        };
        let Some(info) = info else {
            return;
        };
        let descriptor = descriptor.unwrap_or_else(|| info.to_spec());
        let Ok(game) = AppGameState::solved_with_topology(info, rules, descriptor) else {
            return;
        };
        self.install_game(game, false);
    }

    pub(crate) fn set_rotation_snap_tolerance(&self, value: f32) {
        let mut state = self.state.borrow_mut();
        let value = value.clamp(
            ROTATION_SNAP_TOLERANCE_MIN_DEG,
            ROTATION_SNAP_TOLERANCE_MAX_DEG,
        );
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
        let topology = state
            .assets
            .as_ref()
            .map(|assets| assets.topology.clone())
            .unwrap_or_else(|| info.to_spec());
        let grid = state.core.grid;
        let view_settings = state.view_settings.clone();
        // Topology-agnostic rebuild: delegate to the same asset builder
        // `set_puzzle_with_topology` uses. Topologies that don't consume
        // shape settings (e.g. triangular today) return effectively the
        // same geometry and the call is a no-op refresh.
        let Some((render_geometry, piece_width, piece_height, mask_pad)) =
            build_assets_for_topology(&info, &topology, grid, &view_settings)
        else {
            drop(state);
            self.notify();
            return;
        };
        state.assets = Some(Rc::new(PuzzleAssets {
            info,
            grid,
            topology,
            render_geometry,
            piece_width,
            piece_height,
            mask_pad,
        }));
        state.core.piece_width = piece_width;
        state.core.piece_height = piece_height;
        drop(state);
        self.notify();
    }

    /// Installs a fully-built `AppGameState` as the authoritative app state.
    /// Used by callers that already have an `AppGameState` (multiplayer
    /// bridge, local snapshot restore) so we don't round-trip through the
    /// rectangular array projection.
    pub(crate) fn install_game(&self, game: AppGameState, preserve_drag: bool) {
        let mut state = self.state.borrow_mut();
        state.core.scramble_nonce = game.scramble_nonce;
        state.core.solved = game.playable.is_solved();
        // When preserving an active drag, `drag_move` owns the dragged
        // pieces' visual poses. Capture them off the outgoing `state.game`
        // before replacing it so the incoming authoritative-or-predicted
        // visual doesn't briefly clobber the in-flight cursor pose (which
        // would otherwise show as a one-frame flicker every server echo).
        let preserved_visual: Vec<(PieceId, Pose2)> = if preserve_drag {
            match state.game.as_ref() {
                Some(old_game) => {
                    let old_visual = old_game.visual.piece_visual_pose();
                    state
                        .dragging_members
                        .iter()
                        .filter_map(|&id| {
                            old_visual
                                .get(id)
                                .copied()
                                .map(|pose| (PieceId(id as u32), pose))
                        })
                        .collect()
                }
                None => Vec::new(),
            }
        } else {
            Vec::new()
        };
        state.game = Some(game);
        if !preserved_visual.is_empty() {
            if let Some(new_game) = state.game.as_mut() {
                for (piece, pose) in preserved_visual {
                    new_game.visual.set_piece_visual_pose(piece, pose);
                }
            }
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
                click_slop,
                pointer_id,
            } => self.begin_drag(
                piece_id,
                x,
                y,
                shift_key,
                rotate_mode,
                right_click,
                click_slop,
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
        game: None,
        piece_world_poses: Vec::new(),
        piece_top_left_px: Vec::new(),
        piece_flipped: Vec::new(),
        piece_group_anchor: Vec::new(),
        scramble_nonce: 0,
        pose_unit_px: [1.0, 1.0],
        pose_origin_px: [0.0, 0.0],
        min_piece_extent_px: [0.0, 0.0],
        typical_piece_extent_px: [0.0, 0.0],
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
    snapshot.scramble_nonce = state.core.scramble_nonce;
    fill_game_state_view(state, snapshot);
    if let Some(assets) = state.assets.as_ref() {
        snapshot.pose_unit_px = assets.render_geometry.pose_unit_px;
        snapshot.pose_origin_px = assets.render_geometry.pose_origin_px;
        snapshot.min_piece_extent_px = assets.render_geometry.min_piece_extent_px;
        snapshot.typical_piece_extent_px = assets.render_geometry.typical_piece_extent_px;
    } else {
        snapshot.pose_unit_px = [1.0, 1.0];
        snapshot.pose_origin_px = [0.0, 0.0];
        snapshot.min_piece_extent_px = [0.0, 0.0];
        snapshot.typical_piece_extent_px = [0.0, 0.0];
    }
    // Derive the per-piece render order from `state.game.playable.z_order`
    // (groups, back-to-front), expanded with each group's member piece
    // ids. Renderers iterate this to draw back-to-front.
    snapshot.z_order.clear();
    if let Some(game) = state.game.as_ref() {
        snapshot.z_order.reserve(game.playable.piece_count());
        for group in game.playable.iter_z_asc() {
            let mut members: Vec<PieceId> = game.playable.logical.members_of(group).collect();
            members.sort_unstable_by_key(|p| p.as_usize());
            for piece in members {
                snapshot.z_order.push(piece.as_usize());
            }
        }
    }
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

/// Populates the renderer-facing accessors on `AppSnapshot` from
/// `state.game`. When no game state is loaded, leaves the fields empty
/// (legacy `core` fields take over for the no-puzzle case).
fn fill_game_state_view(state: &AppState, snapshot: &mut AppSnapshot) {
    let Some(game) = state.game.as_ref() else {
        snapshot.game = None;
        snapshot.piece_world_poses.clear();
        snapshot.piece_top_left_px.clear();
        snapshot.piece_flipped.clear();
        snapshot.piece_group_anchor.clear();
        return;
    };
    snapshot.game = Some(game.clone());
    let total = game.playable.piece_count();
    snapshot.piece_world_poses.clear();
    snapshot.piece_world_poses.reserve(total);
    snapshot.piece_top_left_px.clear();
    snapshot.piece_top_left_px.reserve(total);
    snapshot.piece_flipped.clear();
    snapshot.piece_flipped.reserve(total);
    snapshot.piece_group_anchor.clear();
    snapshot.piece_group_anchor.reserve(total);

    let visual_poses = game.visual.piece_visual_pose();
    for idx in 0..total {
        let piece = heddobureika_core::PieceId(idx as u32);
        let pose = visual_poses.get(idx).copied().unwrap_or_default();
        snapshot.piece_world_poses.push(pose);
        let top_left = state
            .assets
            .as_ref()
            .and_then(|assets| assets.render_geometry.pose_to_piece_top_left(piece, pose))
            .unwrap_or((
                pose.x_mm() * state.core.piece_width - state.core.piece_width * 0.5,
                pose.y_mm() * state.core.piece_height - state.core.piece_height * 0.5,
            ));
        snapshot.piece_top_left_px.push(top_left);
        let group = game.playable.logical.group_of(piece);
        let flipped = group
            .and_then(|g| game.playable.flip_of(g))
            .map(|f| f == heddobureika_core::FlipState::Flipped)
            .unwrap_or(false);
        snapshot.piece_flipped.push(flipped);
        snapshot
            .piece_group_anchor
            .push(group.map(|g| g.as_u32()).unwrap_or(idx as u32));
    }
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
            game: None,
            assets: None,
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

fn descriptor_grid_choice(descriptor: &TopologySpec, piece_count: u32) -> Option<GridChoice> {
    let (cols, rows) = build_topology_from_spec(descriptor)?.dims_hint()?;
    if cols == 0 || rows == 0 || piece_count == 0 {
        return None;
    }
    Some(GridChoice {
        target_count: piece_count,
        cols,
        rows,
        actual_count: piece_count,
    })
}

fn build_assets_for_topology(
    info: &PuzzleInfo,
    descriptor: &TopologySpec,
    _grid: GridChoice,
    view_settings: &ViewSettings,
) -> Option<(PuzzleRenderGeometry, f32, f32, f32)> {
    let topology = build_topology_from_spec(descriptor)?;
    let render_geometry = topology.build_render_geometry(
        info.image_width,
        info.image_height,
        info.shape_seed,
        &view_settings.shape,
    )?;
    let pose_unit = render_geometry.pose_unit_px;
    let mask_pad = render_geometry.mask_pad_px;
    Some((render_geometry, pose_unit[0], pose_unit[1], mask_pad))
}

fn scramble_layout_for_geometry(
    seed: u32,
    geometry: &PuzzleRenderGeometry,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    margin: f32,
) -> (Vec<(f32, f32)>, Vec<usize>) {
    let mut positions = Vec::with_capacity(geometry.pieces.len());
    for piece in &geometry.pieces {
        let min_x = view_min_x + margin;
        let mut max_x = view_min_x + view_width - piece.bounds_px.width - margin;
        let min_y = view_min_y + margin;
        let mut max_y = view_min_y + view_height - piece.bounds_px.height - margin;
        if max_x < min_x {
            max_x = min_x;
        }
        if max_y < min_y {
            max_y = min_y;
        }
        let salt = piece.id.as_u32() << 1;
        positions.push((
            rand_range(seed, salt, min_x, max_x),
            rand_range(seed, salt + 1, min_y, max_y),
        ));
    }

    let mut order: Vec<usize> = (0..geometry.pieces.len()).collect();
    for i in (1..order.len()).rev() {
        let salt = 0xC0DE_u32 + i as u32;
        let j = (heddobureika_core::rand_unit(seed, salt) * (i as f32 + 1.0)) as usize;
        order.swap(i, j);
    }
    (positions, order)
}

/// Generates a fresh, non-zero shape seed for a brand-new puzzle. Used
/// when no saved snapshot pins a specific seed (first boot, picking a
/// new catalog puzzle, etc.) — every fresh puzzle gets a unique tab/
/// blank pattern.
pub(crate) fn random_shape_seed() -> u32 {
    let mut seed = time_nonce(0);
    if seed == 0 {
        seed = 0x9E37_79B9;
    }
    seed
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
                if let Ok(now_fn) = Reflect::get(&perf, &"now".into())
                    .and_then(|value| value.dyn_into::<Function>())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn triangular_in_bounds_pose_survives_safety_correction() {
        // Build a solved triangular puzzle and verify that the per-piece
        // safety correction doesn't drag pieces vertically. Before the fix,
        // pieces at piece_row > 0 would get force-moved upward because the
        // mm→px conversion used the wrong y unit for triangular topology.
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "tri".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "tri".to_string(),
            },
            (600, 400),
            TopologySpec::triangular_tessellation(3, 3),
            Some(2),
        );
        let snapshot_before = core.snapshot();
        let game = snapshot_before.game.as_ref().expect("game");
        let pose_unit_px = core.assets().expect("assets").render_geometry.pose_unit_px;
        // Pick the piece nearest the lattice centre — solidly in-bounds, so a
        // correct safety helper produces no correction. (The old y-unit bug
        // would still drag such an interior piece.)
        let topo = heddobureika_core::TriangularTessellationTopology::try_new(3, 3).expect("topo");
        let (ex, ey) = topo.pose_extent();
        let (cx, cy) = (ex * 0.5, ey * 0.5);
        let piece_id = (0..topo.piece_count())
            .min_by(|&a, &b| {
                let da = topo
                    .canonical_position_in_pose_units(heddobureika_core::PieceId(a))
                    .map(|(x, y)| (x - cx).powi(2) + (y - cy).powi(2))
                    .unwrap_or(f32::INFINITY);
                let db = topo
                    .canonical_position_in_pose_units(heddobureika_core::PieceId(b))
                    .map(|(x, y)| (x - cx).powi(2) + (y - cy).powi(2))
                    .unwrap_or(f32::INFINITY);
                da.total_cmp(&db)
            })
            .expect("a piece") as usize;
        let original_pose = game
            .playable
            .pose_of(heddobureika_core::GroupId(piece_id as u32))
            .expect("pose");
        let puzzle_info = game.puzzle.clone();
        let rules = snapshot_before.rules;
        let corrections = heddobureika_core::safety_corrections_after_detach(
            &game.playable,
            &[heddobureika_core::PieceId(piece_id as u32)],
            &puzzle_info,
            &rules,
            (pose_unit_px[0], pose_unit_px[1]),
        );
        // The piece's canonical position sits inside the workspace, so the
        // safety helper should produce no correction.
        assert!(
            corrections.is_empty(),
            "expected no safety correction for in-bounds triangular piece, got {corrections:?} (original pose: {original_pose:?})",
        );
    }

    #[test]
    fn set_puzzle_with_grid_wrapper_builds_grid_game() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "grid".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "grid".to_string(),
            },
            (300, 200),
            TopologySpec::grid(3, 2),
            None,
        );
        let snapshot = core.snapshot();
        let game = snapshot.game.expect("grid game");
        assert_eq!(game.playable.piece_count(), 6);
        assert_eq!(
            game.playable.logical.topology.to_spec(),
            TopologySpec::grid(3, 2)
        );
    }

    #[test]
    fn scrambled_puzzle_flips_some_pieces() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "flips".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "flips".to_string(),
            },
            (800, 600),
            TopologySpec::grid(8, 5),
            None,
        );
        let snapshot = core.snapshot();
        let flipped_count = snapshot.piece_flipped.iter().filter(|f| **f).count();
        // With FLIP_CHANCE = 0.2 over 40 pieces, expect a non-trivial number
        // of flips. Allow some randomness — just assert >= 1.
        assert!(
            flipped_count >= 1,
            "expected some pieces to be scrambled flipped, got {flipped_count}"
        );
    }

    #[test]
    fn ctrl_click_sets_flipped() {
        let core = AppCore::new();
        core.clear_all_group_flips();
        core.set_puzzle_with_topology(
            "flip".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "flip".to_string(),
            },
            (300, 200),
            TopologySpec::grid(3, 2),
            None,
        );
        // Force-reset every piece's flip to Normal so the test is deterministic
        // regardless of scramble seed.
        core.clear_all_group_flips();
        let snapshot = core.snapshot();
        assert!(!snapshot.piece_flipped[0]);
        let start_top_left = snapshot.piece_top_left_px[0];
        let piece_w = snapshot.pose_unit_px[0];
        let piece_h = snapshot.pose_unit_px[1];
        let pivot_x = start_top_left.0 + piece_w * 0.5;
        let pivot_y = start_top_left.1 + piece_h * 0.5;
        core.begin_drag(0, pivot_x, pivot_y, false, true, false, 4.0, Some(1));
        core.drag_end(Some(1));
        let snapshot = core.snapshot();
        assert!(snapshot.piece_flipped[0], "Ctrl-click should flip piece");
    }

    #[test]
    fn ctrl_drag_rotates_piece_visual_pose() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "rot".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "rot".to_string(),
            },
            (300, 200),
            TopologySpec::grid(3, 2),
            None,
        );
        // Unflip every piece so the rotation direction in the test is
        // independent of the random scramble.
        core.clear_all_group_flips();
        let snapshot = core.snapshot();
        let start_top_left = snapshot.piece_top_left_px[0];
        let start_rot = snapshot.piece_world_poses[0].rotation_degrees();
        let piece_w = snapshot.pose_unit_px[0];
        let piece_h = snapshot.pose_unit_px[1];
        let pivot_x = start_top_left.0 + piece_w * 0.5;
        let pivot_y = start_top_left.1 + piece_h * 0.5;
        core.begin_drag(0, pivot_x + 20.0, pivot_y, false, true, false, 4.0, Some(1));
        // Drag perpendicular — should rotate by 90 deg.
        core.drag_move(pivot_x, pivot_y + 20.0);
        let snapshot = core.snapshot();
        let new_rot = snapshot.piece_world_poses[0].rotation_degrees();
        assert!(
            (new_rot - (start_rot + 90.0)).abs() < 0.5
                || (new_rot - (start_rot - 270.0)).abs() < 0.5,
            "rotation should change from {start_rot} to ~{}, got {new_rot}",
            start_rot + 90.0
        );
        // After drag end, the rotation should persist (be committed to playable).
        core.drag_end(Some(1));
        let snapshot = core.snapshot();
        let end_rot = snapshot.piece_world_poses[0].rotation_degrees();
        assert!(
            (end_rot - new_rot).abs() < 0.5,
            "rotation should persist after drag end: drag rotation {new_rot}, post-end {end_rot}"
        );
    }

    #[test]
    fn click_rotates_triangular_piece_by_topology_step() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "triangular".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "triangular".to_string(),
            },
            (300, 200),
            TopologySpec::triangular_tessellation(3, 3),
            Some(1),
        );
        core.clear_all_group_flips();
        // Force every group to a canonical rotation so the next click step
        // is deterministic. clear_all_group_rotations zeros every group pose,
        // then we rebuild the visual cache.
        core.clear_all_group_rotations();
        let snapshot = core.snapshot();
        // Pick any regular (interior, equilateral) triangular piece — those
        // have 60° rotational symmetry.
        let topo = heddobureika_core::TriangularTessellationTopology::try_new(3, 3).expect("topo");
        let regular_id = (0..topo.piece_count())
            .find(|&p| !topo.is_frame_border_piece(heddobureika_core::PieceId(p)))
            .expect("an interior piece") as usize;
        let start_top_left = snapshot.piece_top_left_px[regular_id];
        let regular_assets = core.assets().expect("assets");
        let regular_anchor = regular_assets
            .render_geometry
            .pieces
            .get(regular_id)
            .map(|p| p.pose_anchor_px)
            .expect("anchor");
        let pivot_x = start_top_left.0 + regular_anchor[0];
        let pivot_y = start_top_left.1 + regular_anchor[1];
        let start_rot = snapshot.piece_world_poses[regular_id].rotation_degrees();
        core.begin_drag(
            regular_id,
            pivot_x,
            pivot_y,
            false,
            false,
            false,
            4.0,
            Some(2),
        );
        core.drag_end(Some(2));
        let snapshot = core.snapshot();
        let new_rot = snapshot.piece_world_poses[regular_id].rotation_degrees();
        let delta = (new_rot - start_rot).rem_euclid(360.0);
        // Regular-triangle pieces have 60° rotational symmetry, so the
        // click step should be 60°, not 90°.
        assert!(
            (delta - 60.0).abs() < 1.0,
            "regular-triangle click rotation should step by 60°, got {delta}°"
        );
    }

    #[test]
    fn click_at_canonical_anchor_keeps_triangular_piece_in_place() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "triangular".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "triangular".to_string(),
            },
            (300, 200),
            TopologySpec::triangular_tessellation(3, 3),
            Some(1),
        );
        core.clear_all_group_flips();
        core.clear_all_group_rotations();
        let snapshot = core.snapshot();
        let regular_id = 3usize;
        let assets = core.assets().expect("assets");
        let geom = &assets.render_geometry;
        // Click exactly at the piece's canonical position (its pose anchor).
        // A rotation around this point must leave pose.x_mm and pose.y_mm
        // unchanged — the piece pivots in place.
        let pose_before = snapshot.piece_world_poses[regular_id];
        let pivot_px = (
            geom.pose_origin_px[0] + pose_before.x_mm() * geom.pose_unit_px[0],
            geom.pose_origin_px[1] + pose_before.y_mm() * geom.pose_unit_px[1],
        );
        core.begin_drag(
            regular_id,
            pivot_px.0,
            pivot_px.1,
            false,
            false,
            false,
            4.0,
            Some(7),
        );
        core.drag_end(Some(7));
        let snapshot = core.snapshot();
        let pose_after = snapshot.piece_world_poses[regular_id];
        assert!(
            (pose_after.x_mm() - pose_before.x_mm()).abs() < 0.01,
            "x_mm should not move when click pivot == canonical anchor (before {} after {})",
            pose_before.x_mm(),
            pose_after.x_mm()
        );
        assert!(
            (pose_after.y_mm() - pose_before.y_mm()).abs() < 0.01,
            "y_mm should not move when click pivot == canonical anchor (before {} after {})",
            pose_before.y_mm(),
            pose_after.y_mm()
        );
    }

    #[test]
    fn set_puzzle_with_topology_builds_triangular_game() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "triangular".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "triangular".to_string(),
            },
            (300, 200),
            TopologySpec::triangular_tessellation(3, 3),
            Some(1),
        );
        let snapshot = core.snapshot();
        let game = snapshot.game.as_ref().expect("triangular game");
        let count = game.playable.piece_count();
        assert!(count > 0, "triangular game should have pieces");
        assert_eq!(snapshot.piece_positions_px().len(), count);
        let assets = core.assets().expect("assets");
        assert_eq!(assets.render_geometry.pieces.len(), count);
        // puzzle_bounds is the full image; the lattice frame is centred inside.
        assert_eq!(assets.render_geometry.puzzle_bounds_px.height, 200.0);
        // Pose units are square (uniform scale), so x and y units match.
        let [ux, uy] = assets.render_geometry.pose_unit_px;
        assert!(
            (ux - uy).abs() / ux.max(uy) < 1.0e-3,
            "pose units not square"
        );
    }
}
