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

pub const SOLVE_TOLERANCE_RATIO: f32 = 0.080;

pub const ROTATION_STEP_DEG: f32 = 90.0;
pub const ROTATION_SNAP_TOLERANCE_DEFAULT_DEG: f32 = 5.0;
pub const ROTATION_SNAP_TOLERANCE_MIN_DEG: f32 = 0.5;
pub const ROTATION_SNAP_TOLERANCE_MAX_DEG: f32 = 12.0;
pub const ROTATION_SOLVE_TOLERANCE_DEG: f32 = 1.5;

pub const FLIP_CHANCE: f32 = 0.2;

pub const WORKSPACE_PADDING_RATIO_MIN: f32 = 0.5;
pub const WORKSPACE_PADDING_RATIO_MAX: f32 = 2.0;
pub const WORKSPACE_PADDING_RATIO_DEFAULT: f32 = 0.8;

pub const FRAME_SNAP_MIN: f32 = 0.4;
pub const FRAME_SNAP_MAX: f32 = 3.0;
pub const FRAME_SNAP_DEFAULT: f32 = 1.0;
pub const COMPLETE_SNAP_MULTIPLIER: f32 = 2.0;

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

pub fn splitmix32(mut value: u32) -> u32 {
    value = value.wrapping_add(0x9E37_79B9);
    let mut z = value;
    z = (z ^ (z >> 16)).wrapping_mul(0x85EB_CA6B);
    z = (z ^ (z >> 13)).wrapping_mul(0xC2B2_AE35);
    z ^ (z >> 16)
}

pub fn rand_unit(seed: u32, salt: u32) -> f32 {
    let mixed = splitmix32(seed ^ salt);
    let top = mixed >> 8;
    top as f32 / ((1u32 << 24) as f32)
}

pub fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
}

pub fn normalize_angle(mut angle: f32) -> f32 {
    angle = angle % 360.0;
    if angle < 0.0 {
        angle += 360.0;
    }
    angle
}

pub fn angle_delta(target: f32, current: f32) -> f32 {
    let mut diff = normalize_angle(target - current);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

pub fn angle_matches(a: f32, b: f32, tolerance: f32) -> bool {
    angle_delta(a, b).abs() <= tolerance
}

pub fn rotate_vec(x: f32, y: f32, angle_deg: f32) -> (f32, f32) {
    let theta = angle_deg.to_radians();
    let (sin, cos) = theta.sin_cos();
    (x * cos - y * sin, x * sin + y * cos)
}

pub fn piece_local_offset(
    id: usize,
    anchor: usize,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
) -> (f32, f32) {
    let col = (id % cols) as f32;
    let row = (id / cols) as f32;
    let anchor_col = (anchor % cols) as f32;
    let anchor_row = (anchor / cols) as f32;
    (
        (col - anchor_col) * piece_width,
        (row - anchor_row) * piece_height,
    )
}

pub fn derive_piece_state(
    anchor_of: &[usize],
    group_pos: &[(f32, f32)],
    group_rot: &[f32],
    cols: usize,
    piece_width: f32,
    piece_height: f32,
) -> (Vec<(f32, f32)>, Vec<f32>) {
    let total = anchor_of.len();
    let mut positions = vec![(0.0, 0.0); total];
    let mut rotations = vec![0.0; total];
    for id in 0..total {
        let anchor = anchor_of[id];
        if anchor >= group_pos.len() || anchor >= group_rot.len() {
            continue;
        }
        let base = group_pos[anchor];
        let rot = group_rot[anchor];
        let (dx, dy) = piece_local_offset(id, anchor, cols, piece_width, piece_height);
        let (rx, ry) = rotate_vec(dx, dy, rot);
        positions[id] = (base.0 + rx, base.1 + ry);
        rotations[id] = rot;
    }
    (positions, rotations)
}

pub fn update_group_members_state(
    members: &[usize],
    anchor_id: usize,
    group_pos: &[(f32, f32)],
    group_rot: &[f32],
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    positions: &mut [(f32, f32)],
    rotations: &mut [f32],
) {
    if anchor_id >= group_pos.len() || anchor_id >= group_rot.len() {
        return;
    }
    let base = group_pos[anchor_id];
    let rot = group_rot[anchor_id];
    for &id in members {
        if id >= positions.len() || id >= rotations.len() {
            continue;
        }
        let (dx, dy) = piece_local_offset(id, anchor_id, cols, piece_width, piece_height);
        let (rx, ry) = rotate_vec(dx, dy, rot);
        positions[id] = (base.0 + rx, base.1 + ry);
        rotations[id] = rot;
    }
}

pub fn build_group_order_from_piece_order(
    piece_order: &[usize],
    anchor_of: &[usize],
) -> Vec<usize> {
    let total = anchor_of.len();
    let mut seen = vec![false; total];
    let mut group_order = Vec::new();
    for &id in piece_order {
        if id >= total {
            continue;
        }
        let anchor = anchor_of[id];
        if anchor < total && !seen[anchor] {
            seen[anchor] = true;
            group_order.push(anchor);
        }
    }
    for anchor in 0..total {
        if anchor_of[anchor] == anchor && !seen[anchor] {
            group_order.push(anchor);
        }
    }
    group_order
}

pub fn build_piece_order_from_groups(group_order: &[usize], anchor_of: &[usize]) -> Vec<usize> {
    let total = anchor_of.len();
    let mut members: Vec<Vec<usize>> = vec![Vec::new(); total];
    for id in 0..total {
        let anchor = anchor_of[id];
        if anchor < total {
            members[anchor].push(id);
        }
    }
    for group in &mut members {
        if group.len() > 1 {
            group.sort_unstable();
        }
    }
    let mut group_seen = vec![false; total];
    for &anchor in group_order {
        if anchor < total {
            group_seen[anchor] = true;
        }
    }
    let mut order = Vec::with_capacity(total);
    for &anchor in group_order {
        if anchor < total {
            order.extend_from_slice(&members[anchor]);
        }
    }
    for anchor in 0..total {
        if anchor_of[anchor] == anchor && !group_seen[anchor] {
            order.extend_from_slice(&members[anchor]);
        }
    }
    if order.len() < total {
        let mut seen = vec![false; total];
        for &id in &order {
            if id < total {
                seen[id] = true;
            }
        }
        for id in 0..total {
            if !seen[id] {
                order.push(id);
            }
        }
    }
    order
}

pub fn rebuild_groups_from_piece_state(
    positions: &[(f32, f32)],
    rotations: &[f32],
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
    piece_order: Option<&[usize]>,
) -> (Vec<usize>, Vec<(f32, f32)>, Vec<f32>, Vec<usize>) {
    let total = cols * rows;
    let mut anchor_of = vec![0usize; total];
    let mut group_pos = vec![(0.0, 0.0); total];
    let mut group_rot = vec![0.0; total];
    let groups = groups_from_connections(connections, cols, rows);
    for group in &groups {
        if group.is_empty() {
            continue;
        }
        let anchor = group[0];
        for &id in group {
            if id < total {
                anchor_of[id] = anchor;
            }
        }
        if anchor < positions.len() {
            group_pos[anchor] = positions[anchor];
        }
        if anchor < rotations.len() {
            group_rot[anchor] = rotations[anchor];
        }
    }
    let group_order = if let Some(order) = piece_order {
        build_group_order_from_piece_order(order, &anchor_of)
    } else {
        let mut order = Vec::new();
        for id in 0..total {
            if anchor_of[id] == id {
                order.push(id);
            }
        }
        order
    };
    (anchor_of, group_pos, group_rot, group_order)
}

pub fn rebuild_group_state(
    positions: &[(f32, f32)],
    rotations: &[f32],
    connections: &[[bool; 4]],
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    piece_order: Option<&[usize]>,
) -> (
    Vec<usize>,
    Vec<(f32, f32)>,
    Vec<f32>,
    Vec<usize>,
    Vec<(f32, f32)>,
    Vec<f32>,
    Vec<usize>,
) {
    let (anchor_of, group_pos, group_rot, group_order) =
        rebuild_groups_from_piece_state(positions, rotations, connections, cols, rows, piece_order);
    let (derived_positions, derived_rotations) = derive_piece_state(
        &anchor_of,
        &group_pos,
        &group_rot,
        cols,
        piece_width,
        piece_height,
    );
    let piece_order = build_piece_order_from_groups(&group_order, &anchor_of);
    (
        anchor_of,
        group_pos,
        group_rot,
        group_order,
        derived_positions,
        derived_rotations,
        piece_order,
    )
}

pub fn group_transforms_from_anchor(
    anchor_of: &[usize],
    positions: &[(f32, f32)],
    rotations: &[f32],
) -> (Vec<(f32, f32)>, Vec<f32>) {
    let total = anchor_of.len();
    let mut group_pos = vec![(0.0, 0.0); total];
    let mut group_rot = vec![0.0; total];
    for (id, anchor) in anchor_of.iter().copied().enumerate() {
        if anchor == id {
            if let Some(pos) = positions.get(id) {
                group_pos[id] = *pos;
            }
            if let Some(rot) = rotations.get(id) {
                group_rot[id] = *rot;
            }
        }
    }
    (group_pos, group_rot)
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

pub fn opposite_dir(dir: usize) -> usize {
    match dir {
        DIR_UP => DIR_DOWN,
        DIR_RIGHT => DIR_LEFT,
        DIR_DOWN => DIR_UP,
        DIR_LEFT => DIR_RIGHT,
        _ => DIR_UP,
    }
}

pub fn set_connection(
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

pub fn clear_piece_connections(
    connections: &mut Vec<[bool; 4]>,
    id: usize,
    cols: usize,
    rows: usize,
) {
    for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
        set_connection(connections, id, dir, false, cols, rows);
    }
}

pub fn collect_group(
    connections: &[[bool; 4]],
    start: usize,
    cols: usize,
    rows: usize,
) -> Vec<usize> {
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
    group
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

pub fn is_fully_connected(connections: &[[bool; 4]], cols: usize, rows: usize) -> bool {
    let total = cols * rows;
    if total == 0 || connections.len() != total {
        return false;
    }
    collect_group(connections, 0, cols, rows).len() == total
}

pub fn aligned_center_from_anchor(
    anchor_row: i32,
    anchor_col: i32,
    anchor_center: (f32, f32),
    id: usize,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    rotation: f32,
) -> (f32, f32) {
    let row = (id / cols) as i32;
    let col = (id % cols) as i32;
    let dx = (col - anchor_col) as f32 * piece_width;
    let dy = (row - anchor_row) as f32 * piece_height;
    let (rx, ry) = rotate_vec(dx, dy, rotation);
    (anchor_center.0 + rx, anchor_center.1 + ry)
}

pub fn align_group_to_anchor(
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

pub fn build_full_connections(cols: usize, rows: usize) -> Vec<[bool; 4]> {
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

/// Inverse of `scramble_seed_from_topology` — recover the nonce from a
/// seed and topology. Used by code that wants to round-trip a "share-this-
/// scramble" seed back into a `scramble_nonce`.
pub fn scramble_nonce_from_topology_seed(
    base: u32,
    seed: u32,
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
    let mixed = seed ^ base ^ hash;
    mixed.wrapping_mul(0x144C_BC89)
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

pub fn scramble_layout(
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
/// resulting from a just-applied detach. For each affected group:
///   * singletons must keep their anchor within the loose single-piece bound,
///   * multi-piece groups must keep their anchor within the tight one-piece
///     inset of that bound.
///
/// A group whose anchor is already inside its applicable bound is reported as
/// no correction (omitted from the result). Otherwise the anchor center is
/// clamped to the nearest in-bounds position and returned as `(group_id,
/// new_anchor_pos)`. The helper is pure — it does not mutate `playable`. The
/// caller applies each correction via
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
    // exactly on both sides. For the inset we assume a typical piece spans one
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
    let mut tight_min_x = center_min_x + piece_width;
    let mut tight_max_x = center_max_x - piece_width;
    let mut tight_min_y = center_min_y + piece_height;
    let mut tight_max_y = center_max_y - piece_height;
    if tight_max_x < tight_min_x {
        let mid = (center_min_x + center_max_x) * 0.5;
        tight_min_x = mid;
        tight_max_x = mid;
    }
    if tight_max_y < tight_min_y {
        let mid = (center_min_y + center_max_y) * 0.5;
        tight_min_y = mid;
        tight_max_y = mid;
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

        let is_singleton = playable.logical.members_of(group).nth(1).is_none();
        let (min_x, max_x, min_y, max_y) = if is_singleton {
            (center_min_x, center_max_x, center_min_y, center_max_y)
        } else {
            (tight_min_x, tight_max_x, tight_min_y, tight_max_y)
        };

        let cx = origin_x + pose.x_mm() * pose_unit_x;
        let cy = origin_y + pose.y_mm() * pose_unit_y;
        let new_cx = cx.clamp(min_x, max_x);
        let new_cy = cy.clamp(min_y, max_y);
        if (new_cx - cx).abs() < 1.0e-3 && (new_cy - cy).abs() < 1.0e-3 {
            continue;
        }

        let new_x_mm = (new_cx - origin_x) / pose_unit_x;
        let new_y_mm = (new_cy - origin_y) / pose_unit_y;
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
        build_topology_from_spec, GridTopology, LogicalState, PieceId, PlayRules, Pose2,
        RestrictedPlayableAction, TopologySpec,
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
    fn detach_endpiece_force_moves_remaining_multi_piece_group_to_tight_bound() {
        // 3x1 puzzle: workspace center bounds are roughly x in [10..290],
        // y in [10..90]. Tight (multi-piece) bound for x is [110..190]; for
        // y the inset collapses to the midpoint 50 (because the workspace is
        // narrower than 3 piece-heights). Place the anchor at center (50,
        // 50) — well outside the tight x bound. Detach the END piece (id 2)
        // so the remaining group {0, 1} is still multi-piece.
        let anchor_pose = Pose2::try_from_mm_degrees(0.5, 0.5, 0.0).expect("finite pose");
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

        // The remaining {0, 1} group should be force-moved. The singleton {2}
        // sits at (250, 50) in pixel-center coords — inside the loose bound
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
        // New anchor center x should be clamped to the tight-min x (110) —
        // i.e. 110 / piece_width = 110/100 = 1.1.
        assert!(
            (new_pos.x_mm() - 1.1).abs() < 1.0e-3,
            "anchor x should clamp to tight_min_x: got {}",
            new_pos.x_mm()
        );
        // y stays at the tight midpoint (50px -> 0.5mm).
        assert!(
            (new_pos.y_mm() - 0.5).abs() < 1.0e-3,
            "anchor y should sit at tight midpoint: got {}",
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
