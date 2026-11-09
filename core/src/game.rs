use std::collections::VecDeque;

use heddobureika_game::{
    GroupId, ImagePlacement, PieceId, PlayableState, Position2, PuzzleTopology,
};

use crate::snapshot::{GameRules, PuzzleInfo};

pub const PUZZLE_SEED: u32 = 0x5EED_2520;
pub const MAX_LINE_BEND_RATIO: f32 = 0.2;

pub const SNAP_DISTANCE_RATIO_DEFAULT: f32 = 0.200;
pub const SNAP_DISTANCE_RATIO_MIN: f32 = 0.050;
pub const SNAP_DISTANCE_RATIO_MAX: f32 = 0.350;

pub const ROTATION_STEP_DEG: f32 = 90.0;
pub const ROTATION_SNAP_TOLERANCE_DEFAULT_DEG: f32 = 5.0;
pub const ROTATION_SNAP_TOLERANCE_MIN_DEG: f32 = 0.5;
pub const ROTATION_SNAP_TOLERANCE_MAX_DEG: f32 = 12.0;

pub const FLIP_CHANCE: f32 = 0.2;

pub const WORKSPACE_PADDING_RATIO_MIN: f32 = 0.5;
pub const WORKSPACE_PADDING_RATIO_MAX: f32 = 2.0;
pub const WORKSPACE_PADDING_RATIO_DEFAULT: f32 = 0.8;

pub const FRAME_SNAP_MIN: f32 = 0.4;
pub const FRAME_SNAP_MAX: f32 = 3.0;
pub const FRAME_SNAP_DEFAULT: f32 = 1.0;

pub const IMAGE_MAX_DIMENSION_MIN: u32 = 512;
pub const IMAGE_MAX_DIMENSION_MAX: u32 = 4096;
pub const IMAGE_MAX_DIMENSION_DEFAULT: u32 = 1280;

pub const DIR_UP: usize = 0;
pub const DIR_RIGHT: usize = 1;
pub const DIR_DOWN: usize = 2;
pub const DIR_LEFT: usize = 3;

pub const DEFAULT_TAB_DEPTH_CAP: f32 = 0.32;

#[derive(Clone, Copy, Debug)]
pub struct WorkspaceLayout {
    pub view_min_x: f32,
    pub view_min_y: f32,
    pub view_width: f32,
    pub view_height: f32,
    pub puzzle_scale: f32,
}

/// Builds the workspace (play area) around the puzzle's actual frame rect, in
/// the same pixel space the pieces live in. The frame may be a letterboxed
/// sub-rect of the image (e.g. triangular's centred equilateral frame), so the
/// padded play area hugs the cropped puzzle rather than the full image. Callers
/// that have no distinct frame pass `frame_x = frame_y = 0` with the image
/// dimensions, which reproduces the historical image-sized workspace exactly.
pub fn compute_workspace_layout(
    frame_x: f32,
    frame_y: f32,
    frame_width: f32,
    frame_height: f32,
    padding_ratio: f32,
) -> WorkspaceLayout {
    let safe_width = frame_width.max(1.0);
    let safe_height = frame_height.max(1.0);
    let min_dim = safe_width.min(safe_height).max(1.0);
    let padding_ratio =
        padding_ratio.clamp(WORKSPACE_PADDING_RATIO_MIN, WORKSPACE_PADDING_RATIO_MAX);
    let padding = (min_dim * padding_ratio).max(0.0);
    let workspace_width = safe_width + padding;
    let workspace_height = safe_height + padding;
    let puzzle_scale = (workspace_width / safe_width)
        .min(workspace_height / safe_height)
        .min(1.0);
    // Centre the padding around the frame's actual position.
    let puzzle_offset_x = (workspace_width - safe_width) * 0.5;
    let puzzle_offset_y = (workspace_height - safe_height) * 0.5;
    WorkspaceLayout {
        view_min_x: frame_x - puzzle_offset_x,
        view_min_y: frame_y - puzzle_offset_y,
        view_width: workspace_width,
        view_height: workspace_height,
        puzzle_scale,
    }
}

pub use heddobureika_game::{
    angle_matches, normalize_angle, rand_range, rand_unit, rotate_point, rotate_vec,
    shortest_angle_delta, splitmix32,
};

/// Signed rotation from `current` to `target` in `(-180, 180]`. Unlike
/// [`shortest_angle_delta`], an exact half turn always comes out positive so
/// click-to-rotate animates in a consistent direction.
pub fn angle_delta(target: f32, current: f32) -> f32 {
    let mut diff = normalize_angle(target - current);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

pub fn neighbor_id(id: usize, cols: usize, rows: usize, dir: usize) -> Option<usize> {
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

pub fn groups_from_connections(
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
                if connections.get(id).map(|edges| edges[dir]).unwrap_or(false) {
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

pub fn scramble_seed(base: u32, nonce: u32, cols: usize, rows: usize) -> u32 {
    let grid = ((cols as u32) << 16) ^ (rows as u32);
    base ^ nonce.wrapping_mul(0x9E37_79B9) ^ grid ^ 0x5CA7_7EED
}

pub fn scramble_nonce_from_seed(base: u32, seed: u32, cols: usize, rows: usize) -> u32 {
    let grid = ((cols as u32) << 16) ^ (rows as u32);
    let mixed = seed ^ base ^ grid ^ 0x5CA7_7EED;
    mixed.wrapping_mul(0x144C_BC89)
}

/// Topology-agnostic scramble seed derivation. Hashes the topology kind +
/// numeric params into a `u32` salt so different topologies (or the same
/// topology with different params) yield distinct scrambles for the same
/// nonce.
pub fn scramble_seed_from_topology(
    base: u32,
    nonce: u32,
    topology: &heddobureika_game::TopologySpec,
) -> u32 {
    let mut hash: u32 = 0x5CA7_7EED;
    // Hash both the tag and the opaque payload — together they uniquely
    // identify the topology instance. We don't care about the payload's
    // internal structure; the FNV-style mix is enough to avoid trivial
    // collisions between topologies.
    for byte in topology.tag.as_bytes() {
        hash = hash.wrapping_mul(0x0100_0193) ^ (*byte as u32);
    }
    for byte in &topology.payload {
        hash = hash.wrapping_mul(0x0100_0193) ^ (*byte as u32);
    }
    base ^ nonce.wrapping_mul(0x9E37_79B9) ^ hash
}

/// Per-piece bounding box used by the topology-agnostic scramble layout.
#[derive(Clone, Copy, Debug)]
pub struct PieceBoundsPx {
    pub width: f32,
    pub height: f32,
}

/// Topology-agnostic scramble layout. Each piece is placed at a random
/// position inside the workspace, with a per-piece `margin` inset based
/// on its own bounding box. The returned `(Vec<(f32, f32)>, Vec<usize>)`
/// pair is `(positions, order)`, where positions are piece top-left
/// pixels and order is a random z-order over the same piece ids.
pub fn scramble_layout_for_pieces(
    seed: u32,
    piece_bounds: &[PieceBoundsPx],
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    margin: f32,
) -> (Vec<(f32, f32)>, Vec<usize>) {
    let total = piece_bounds.len();
    let mut positions = Vec::with_capacity(total);
    for (idx, bounds) in piece_bounds.iter().enumerate() {
        let min_x = view_min_x + margin;
        let mut max_x = view_min_x + view_width - bounds.width - margin;
        let min_y = view_min_y + margin;
        let mut max_y = view_min_y + view_height - bounds.height - margin;
        if max_x < min_x {
            max_x = min_x;
        }
        if max_y < min_y {
            max_y = min_y;
        }
        let salt = (idx as u32) << 1;
        positions.push((
            rand_range(seed, salt, min_x, max_x),
            rand_range(seed, salt + 1, min_y, max_y),
        ));
    }

    let mut order: Vec<usize> = (0..total).collect();
    for i in (1..order.len()).rev() {
        let salt = 0xC0DE_u32 + i as u32;
        let j = (rand_unit(seed, salt) * (i as f32 + 1.0)) as usize;
        order.swap(i, j);
    }
    (positions, order)
}

/// Per-piece scatter bounding boxes derived purely from the topology — the
/// piece's pose-unit extent scaled into pixels by the shared [`ImagePlacement`].
/// No shaped render geometry involved, so the client and server agree exactly.
pub fn piece_bounds_px<T: PuzzleTopology>(
    topology: &T,
    placement: ImagePlacement,
) -> Vec<PieceBoundsPx> {
    let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
    (0..topology.piece_count())
        .map(|idx| {
            let (ex, ey) = topology.piece_extent_mm(PieceId(idx));
            PieceBoundsPx {
                width: ex.as_mm_f32() * pose_unit_x,
                height: ey.as_mm_f32() * pose_unit_y,
            }
        })
        .collect()
}

/// Converts a scattered top-left pixel position into a piece pose, shared by the
/// client and server so both place pieces identically. The piece's geometric
/// centre (its bounding-box centre, derived from the topology's pose-unit
/// extent — NOT shaped render geometry) is mapped back through the shared
/// [`ImagePlacement`]: `pose = (centre_px - origin) / pose_unit`.
pub fn scramble_pose<T: PuzzleTopology>(
    topology: &T,
    placement: ImagePlacement,
    piece: PieceId,
    top_left_px: (f32, f32),
    rotation_deg: f32,
) -> Option<heddobureika_game::Pose2> {
    let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
    let [origin_x, origin_y] = placement.origin_px;
    if pose_unit_x <= 0.0 || pose_unit_y <= 0.0 {
        return None;
    }
    let (ex, ey) = topology.piece_extent_mm(piece);
    let center_x = top_left_px.0 + ex.as_mm_f32() * pose_unit_x * 0.5;
    let center_y = top_left_px.1 + ey.as_mm_f32() * pose_unit_y * 0.5;
    heddobureika_game::Pose2::try_from_mm_degrees(
        (center_x - origin_x) / pose_unit_x,
        (center_y - origin_y) / pose_unit_y,
        rotation_deg,
    )
}

pub fn scramble_rotations(seed: u32, total: usize, enabled: bool) -> Vec<f32> {
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

pub fn scramble_flips(seed: u32, total: usize, chance: f32) -> Vec<bool> {
    let threshold = chance.clamp(0.0, 1.0);
    let mut flips = Vec::with_capacity(total);
    for id in 0..total {
        let salt = 0xF11F_5EED_u32 + id as u32;
        flips.push(rand_unit(seed, salt) < threshold);
    }
    flips
}

/// Computes any safety-bound corrections that need to be applied to groups
/// resulting from a just-applied detach. A group is considered safe — and so
/// reported with no correction — as long as at least one of its pieces' centers
/// sits inside the loose workspace box, which is exactly the predicate
/// `drag_move` enforces while dragging. A group that ends up entirely outside
/// that box is translated by the smallest delta that brings its nearest piece
/// back to the edge of the box, leaving it at the same limit a drag would stop
/// at (rather than yanking the whole group deep inside the workspace).
///
/// Each reported correction is `(group_id, new_anchor_pos)`. The helper is pure
/// — it does not mutate `playable`. The caller applies each correction via
/// `apply_action_only(PlayableAction::TranslateGroup { group, drop_pos })`.
///
/// `affected_pieces` should contain every piece that belonged to the original
/// group before the detach, so the helper visits every component the detach
/// could have produced.
pub fn safety_corrections_after_detach<T: PuzzleTopology>(
    playable: &PlayableState<T>,
    affected_pieces: &[PieceId],
    puzzle: &PuzzleInfo,
    rules: &GameRules,
    placement: ImagePlacement,
) -> Vec<(GroupId, Position2)> {
    if puzzle.image_width == 0 || puzzle.image_height == 0 {
        return Vec::new();
    }
    // `placement` is the shared pose→pixel mapping (`pixel = origin + pose *
    // pose_unit`) used by the renderer AND the worker, so the clamp matches
    // exactly on both sides. For the box we assume a typical piece spans one
    // pose unit per axis — exact for grid, a safe over-approximation otherwise.
    let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
    let [origin_x, origin_y] = placement.origin_px;
    let [frame_w, frame_h] = placement.frame_px;
    let piece_width = pose_unit_x.max(0.0);
    let piece_height = pose_unit_y.max(0.0);
    if piece_width <= 0.0 || piece_height <= 0.0 {
        return Vec::new();
    }
    let layout = compute_workspace_layout(
        origin_x,
        origin_y,
        frame_w,
        frame_h,
        rules.workspace_padding_ratio,
    );
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

    let mut corrections = Vec::new();
    let mut seen_groups: Vec<GroupId> = Vec::new();
    for piece in affected_pieces {
        let Some(group) = playable.logical.group_of(*piece) else {
            continue;
        };
        if seen_groups.contains(&group) {
            continue;
        }
        seen_groups.push(group);

        let Some(pose) = playable.pose_of(group) else {
            continue;
        };

        // A group is safe as long as ANY one of its pieces' centers lands in
        // the loose box (the same per-piece predicate `drag_move` uses). Walk
        // every member; if one is already inside, leave the group alone.
        // Otherwise record the smallest pixel translation that pulls the
        // nearest member back to the box edge — this is the minimal nudge that
        // keeps the group draggable instead of burying it deep inside.
        let mut in_bounds = false;
        let mut best: Option<(f32, f32, f32)> = None; // (dist², dx_px, dy_px)
        for member in playable.logical.members_of(group) {
            let Some(member_pose) = playable.piece_world_pose(member) else {
                continue;
            };
            let cx = origin_x + member_pose.x_mm() * pose_unit_x;
            let cy = origin_y + member_pose.y_mm() * pose_unit_y;
            let dx = cx.clamp(center_min_x, center_max_x) - cx;
            let dy = cy.clamp(center_min_y, center_max_y) - cy;
            if dx.abs() < 1.0e-3 && dy.abs() < 1.0e-3 {
                in_bounds = true;
                break;
            }
            let dist = dx * dx + dy * dy;
            if best.is_none_or(|(best_dist, _, _)| dist < best_dist) {
                best = Some((dist, dx, dy));
            }
        }
        if in_bounds {
            continue;
        }
        let Some((_, dx, dy)) = best else {
            continue;
        };

        // The translation is the same for every member, so apply it to the
        // group anchor pose and convert the pixel delta back to pose units.
        let new_x_mm = pose.x_mm() + dx / pose_unit_x;
        let new_y_mm = pose.y_mm() + dy / pose_unit_y;
        let Some(new_pos) = Position2::try_from_mm(new_x_mm, new_y_mm) else {
            continue;
        };
        corrections.push((group, new_pos));
    }
    corrections
}

#[cfg(test)]
mod safety_tests {
    use super::*;
    use crate::snapshot::PuzzleImageRef;
    use heddobureika_game::{
        GridTopology, LogicalState, PieceId, PlayRules, Pose2, RestrictedPlayableAction,
        TopologySpec, build_topology_from_spec,
    };

    /// The shared scatter contract: a scattered top-left maps to a pose whose
    /// bounding-box CENTRE, mapped back through the placement, lands at the
    /// intended pixel centre. This is the single mapping the client and worker
    /// both use, so verifying it locks their scatter behaviour together. Uses a
    /// letterboxed triangular placement so the non-zero origin is exercised.
    #[test]
    fn scramble_pose_round_trips_bbox_center_through_placement() {
        let topology =
            build_topology_from_spec(&TopologySpec::triangular_tessellation(4, 5)).expect("topo");
        let placement = topology.image_placement(1200, 700);
        let [ux, uy] = placement.pose_unit_px;
        let [ox, oy] = placement.origin_px;
        assert!(
            ox > 0.0 || oy > 0.0,
            "expected a letterboxed (offset) frame"
        );

        let piece = PieceId(7);
        let (ex, ey) = topology.piece_extent_mm(piece);
        let top_left = (250.0_f32, 130.0_f32);
        let pose = scramble_pose(&topology, placement, piece, top_left, 0.0).expect("pose");

        let center_back_x = ox + pose.x_mm() * ux;
        let center_back_y = oy + pose.y_mm() * uy;
        assert!(
            (center_back_x - (top_left.0 + ex.as_mm_f32() * ux * 0.5)).abs() < 1.0e-3,
            "x centre did not round-trip"
        );
        assert!(
            (center_back_y - (top_left.1 + ey.as_mm_f32() * uy * 0.5)).abs() < 1.0e-3,
            "y centre did not round-trip"
        );
    }

    fn puzzle_3x1() -> PuzzleInfo {
        PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            topology: heddobureika_game::TopologySpec::grid(3, 1).into(),
            shape_seed: 0,
            image_width: 300,
            image_height: 100,
        }
    }

    /// Build a 3x1 puzzle in the "all connected" state with the anchor at the
    /// given mm-space pose. Useful for setting up unsafe placements.
    fn solved_3x1_at(anchor_pose: Pose2) -> PlayableState<GridTopology> {
        let topology = GridTopology::try_new(3, 1).expect("3x1 topology");
        let mut logical = LogicalState::new(topology);
        logical.activate_all_edges();
        let mut playable = PlayableState::new(logical, PlayRules::default());
        for slot in playable.group_pose.iter_mut() {
            *slot = anchor_pose;
        }
        playable
    }

    #[test]
    fn detach_endpiece_nudges_out_of_bounds_group_to_the_drag_limit() {
        // 3x1 puzzle: workspace center bounds are roughly x in [10..290],
        // y in [10..90]. Place the anchor (piece 0) at x = -1.0 so the three
        // piece centers sit at -100, 0, 100 px. Detach the END piece (id 2)
        // at 100 px — it stays a singleton inside the loose bound, so no
        // correction. The remaining {0, 1} group has both centers (-100, 0)
        // outside the loose x bound, so it must be nudged: the NEAREST member
        // (piece 1 at 0 px) only needs +10 px to reach the left edge (10 px),
        // not the full pull to bury the group inside.
        let anchor_pose = Pose2::try_from_mm_degrees(-1.0, 0.5, 0.0).expect("finite pose");
        let mut playable = solved_3x1_at(anchor_pose);

        // Capture original members BEFORE the detach.
        let original_group = playable
            .logical
            .group_of(PieceId(0))
            .expect("piece 0 grouped");
        let original_members: Vec<PieceId> = playable.logical.members_of(original_group).collect();

        let _ = playable.apply_restricted_action_batch(
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece: PieceId(2),
                target_pose: playable.piece_world_pose(PieceId(2)).expect("piece 2 pose"),
                target_flip: heddobureika_game::FlipState::Normal,
            },
            None,
        );

        let puzzle = puzzle_3x1();
        let rules = GameRules::default();
        let corrections = safety_corrections_after_detach(
            &playable,
            &original_members,
            &puzzle,
            &rules,
            heddobureika_game::build_topology_from_spec(&puzzle.to_spec())
                .map(|t| t.image_placement(puzzle.image_width, puzzle.image_height))
                .unwrap_or(ImagePlacement {
                    pose_unit_px: [1.0, 1.0],
                    origin_px: [0.0, 0.0],
                    frame_px: [puzzle.image_width as f32, puzzle.image_height as f32],
                }),
        );

        // The remaining {0, 1} group should be nudged. The singleton {2}
        // sits at (100, 50) in pixel-center coords — inside the loose bound
        // x in [10..290] — so no correction is reported for it.
        assert_eq!(
            corrections.len(),
            1,
            "expected one correction (for the remaining {{0, 1}} group)"
        );
        let (group, new_pos) = corrections[0];
        assert_eq!(
            playable.anchor_piece_of_group(group),
            Some(PieceId(0)),
            "correction should target the remaining group whose anchor is piece 0"
        );
        // Minimal nudge: piece 1 (center 0 px) reaches the left edge (10 px)
        // with +10 px == +0.1 pose units, so the anchor moves -1.0 -> -0.9.
        assert!(
            (new_pos.x_mm() - (-0.9)).abs() < 1.0e-3,
            "anchor x should nudge to the drag limit: got {}",
            new_pos.x_mm()
        );
        // y is already in bounds (50 px), so it stays put (0.5 mm).
        assert!(
            (new_pos.y_mm() - 0.5).abs() < 1.0e-3,
            "anchor y should be unchanged: got {}",
            new_pos.y_mm()
        );
    }

    #[test]
    fn detach_produces_no_corrections_when_resulting_singletons_are_in_bounds() {
        // 2x1 puzzle. After a detach, both resulting groups are singletons —
        // they use the loose bound only. With anchor at (0.5, 0.5) the two
        // pieces sit at pixel centers (50, 50) and (150, 50), both well
        // inside the loose bound, so no corrections should fire.
        let puzzle = PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            topology: heddobureika_game::TopologySpec::grid(2, 1).into(),
            shape_seed: 0,
            image_width: 200,
            image_height: 100,
        };
        let topology = GridTopology::try_new(2, 1).expect("2x1 topology");
        let mut logical = LogicalState::new(topology);
        logical.activate_all_edges();
        let mut playable = PlayableState::new(logical, PlayRules::default());
        let anchor_pose = Pose2::try_from_mm_degrees(0.5, 0.5, 0.0).expect("finite pose");
        for slot in playable.group_pose.iter_mut() {
            *slot = anchor_pose;
        }

        let original_group = playable
            .logical
            .group_of(PieceId(0))
            .expect("piece 0 grouped");
        let original_members: Vec<PieceId> = playable.logical.members_of(original_group).collect();

        let _ = playable.apply_restricted_action_batch(
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece: PieceId(1),
                target_pose: playable.piece_world_pose(PieceId(1)).expect("piece 1 pose"),
                target_flip: heddobureika_game::FlipState::Normal,
            },
            None,
        );

        let rules = GameRules::default();
        let corrections = safety_corrections_after_detach(
            &playable,
            &original_members,
            &puzzle,
            &rules,
            heddobureika_game::build_topology_from_spec(&puzzle.to_spec())
                .map(|t| t.image_placement(puzzle.image_width, puzzle.image_height))
                .unwrap_or(ImagePlacement {
                    pose_unit_px: [1.0, 1.0],
                    origin_px: [0.0, 0.0],
                    frame_px: [puzzle.image_width as f32, puzzle.image_height as f32],
                }),
        );

        assert!(
            corrections.is_empty(),
            "no correction expected when everything is already in bounds, got {:?}",
            corrections
        );
    }
}
