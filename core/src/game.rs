use std::collections::VecDeque;

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

pub const WORKSPACE_SCALE_MIN: f32 = 0.5;
pub const WORKSPACE_SCALE_MAX: f32 = 2.0;
pub const WORKSPACE_SCALE_DEFAULT: f32 = 0.8;

pub const FRAME_SNAP_MIN: f32 = 0.4;
pub const FRAME_SNAP_MAX: f32 = 3.0;
pub const FRAME_SNAP_DEFAULT: f32 = 1.0;
pub const COMPLETE_SNAP_MULTIPLIER: f32 = 2.0;

pub const IMAGE_MAX_DIMENSION_MIN: u32 = 512;
pub const IMAGE_MAX_DIMENSION_MAX: u32 = 4096;
pub const IMAGE_MAX_DIMENSION_DEFAULT: u32 = 1280;

pub const WORKSPACE_ASPECT_RATIO: f32 = 1.618034;
pub const WORKSPACE_SLOT_WIDTH_FRAC: f32 = 0.76;
pub const WORKSPACE_SLOT_HEIGHT_FRAC: f32 = 0.82;
pub const WORKSPACE_SLOT_LEFT_FRAC: f32 = (1.0 - WORKSPACE_SLOT_WIDTH_FRAC) * 0.5;
pub const WORKSPACE_SLOT_TOP_FRAC: f32 = (1.0 - WORKSPACE_SLOT_HEIGHT_FRAC) * 0.5;
pub const WORKSPACE_SLOT_MARGIN_FRAC: f32 = 0.03;
pub const WORKSPACE_SLOT_MARGIN_MIN: f32 = 6.0;

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

pub fn compute_workspace_layout(width: f32, height: f32, scale: f32, max_dim: f32) -> WorkspaceLayout {
    let max_dim = max_dim.max(1.0);
    let safe_width = width.max(1.0);
    let safe_height = height.max(1.0);
    let min_height_for_slot = max_dim / WORKSPACE_SLOT_HEIGHT_FRAC;
    let min_height_for_width = max_dim / (WORKSPACE_SLOT_WIDTH_FRAC * WORKSPACE_ASPECT_RATIO);
    let base_height = min_height_for_slot.max(min_height_for_width);
    let base_width = base_height * WORKSPACE_ASPECT_RATIO;
    let workspace_width = base_width * scale;
    let workspace_height = base_height * scale;
    let slot_width = workspace_width * WORKSPACE_SLOT_WIDTH_FRAC;
    let slot_height = workspace_height * WORKSPACE_SLOT_HEIGHT_FRAC;
    let slot_origin_x = workspace_width * WORKSPACE_SLOT_LEFT_FRAC;
    let slot_origin_y = workspace_height * WORKSPACE_SLOT_TOP_FRAC;
    let slot_min = slot_width.min(slot_height).max(1.0);
    let margin = (slot_min * WORKSPACE_SLOT_MARGIN_FRAC).max(WORKSPACE_SLOT_MARGIN_MIN);
    let fit_width = (slot_width - margin * 2.0).max(1.0);
    let fit_height = (slot_height - margin * 2.0).max(1.0);
    let puzzle_scale = (fit_width / safe_width)
        .min(fit_height / safe_height)
        .min(1.0);
    let scaled_width = safe_width * puzzle_scale;
    let scaled_height = safe_height * puzzle_scale;
    let puzzle_offset_x = slot_origin_x + (slot_width - scaled_width) * 0.5;
    let puzzle_offset_y = slot_origin_y + (slot_height - scaled_height) * 0.5;
    WorkspaceLayout {
        view_min_x: -puzzle_offset_x,
        view_min_y: -puzzle_offset_y,
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

pub fn build_group_order_from_piece_order(piece_order: &[usize], anchor_of: &[usize]) -> Vec<usize> {
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
    let (anchor_of, group_pos, group_rot, group_order) = rebuild_groups_from_piece_state(
        positions,
        rotations,
        connections,
        cols,
        rows,
        piece_order,
    );
    let (derived_positions, derived_rotations) =
        derive_piece_state(&anchor_of, &group_pos, &group_rot, cols, piece_width, piece_height);
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

pub fn collect_group(connections: &[[bool; 4]], start: usize, cols: usize, rows: usize) -> Vec<usize> {
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

pub fn connect_aligned_neighbors(
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
                if rotation_enabled && !angle_matches(group_rot, rot_b, rotation_snap_tolerance) {
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

pub fn frame_center_bounds_for_rotation(
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

pub fn apply_snaps_for_group(
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
    complete_snap: bool,
    center_min_x: f32,
    center_max_x: f32,
    center_min_y: f32,
    center_max_y: f32,
    _view_min_x: f32,
    _view_min_y: f32,
    _view_width: f32,
    _view_height: f32,
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
        for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
            if let Some(neighbor) = neighbor_id(*member, cols, rows, dir) {
                if in_group.get(neighbor).copied().unwrap_or(false) {
                    continue;
                }
                if flips.get(neighbor).copied().unwrap_or(false) {
                    continue;
                }
                let neighbor_pos = positions.get(neighbor).copied().unwrap_or((0.0, 0.0));
                let center_b = (
                    neighbor_pos.0 + piece_width * 0.5,
                    neighbor_pos.1 + piece_height * 0.5,
                );
                let base = match dir {
                    DIR_LEFT => (-piece_width, 0.0),
                    DIR_RIGHT => (piece_width, 0.0),
                    DIR_UP => (0.0, -piece_height),
                    DIR_DOWN => (0.0, piece_height),
                    _ => (0.0, 0.0),
                };
                let rot_b = rotations.get(neighbor).copied().unwrap_or(0.0);
                let actual = (center_b.0 - center_a.0, center_b.1 - center_a.1);
                let expected_rot = if rotation_enabled {
                    rotations.get(*member).copied().unwrap_or(0.0)
                } else {
                    0.0
                };
                let (vx, vy) = rotate_vec(base.0, base.1, expected_rot);
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
        let mut best_candidate: Option<SnapCandidate> = None;
        let mut best_dist = f32::INFINITY;
        for candidate in candidates {
            if rotation_enabled
                && !angle_matches(
                    rotations.get(candidate.member).copied().unwrap_or(0.0),
                    candidate.rot_b,
                    rotation_snap_tolerance,
                )
            {
                continue;
            }
            let expected_rot = if rotation_enabled {
                rotations.get(candidate.member).copied().unwrap_or(0.0)
            } else {
                0.0
            };
            let (vx, vy) = rotate_vec(candidate.base.0, candidate.base.1, expected_rot);
            let dx = candidate.center_b.0 - vx;
            let dy = candidate.center_b.1 - vy;
            let expected_center = (dx, dy);
            let mut min_cx = f32::INFINITY;
            let mut max_cx = f32::NEG_INFINITY;
            let mut min_cy = f32::INFINITY;
            let mut max_cy = f32::NEG_INFINITY;
            let anchor_row = (candidate.member / cols) as i32;
            let anchor_col = (candidate.member % cols) as i32;
            for member in members {
                let center = aligned_center_from_anchor(
                    anchor_row,
                    anchor_col,
                    expected_center,
                    *member,
                    cols,
                    piece_width,
                    piece_height,
                    expected_rot,
                );
                min_cx = min_cx.min(center.0);
                max_cx = max_cx.max(center.0);
                min_cy = min_cy.min(center.1);
                max_cy = max_cy.max(center.1);
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
            if candidate.dist < best_dist {
                best_dist = candidate.dist;
                best_candidate = Some(candidate);
            }
        }
        if let Some(candidate) = best_candidate {
            let expected_rot = if rotation_enabled {
                rotations.get(candidate.member).copied().unwrap_or(0.0)
            } else {
                0.0
            };
            let (vx, vy) = rotate_vec(candidate.base.0, candidate.base.1, expected_rot);
            let dx = candidate.center_b.0 - vx;
            let dy = candidate.center_b.1 - vy;
            let expected_center = (dx, dy);
            align_group_to_anchor(
                positions,
                rotations,
                members,
                candidate.member,
                expected_center,
                expected_rot,
                cols,
                piece_width,
                piece_height,
            );
            set_connection(
                connections,
                candidate.member,
                candidate.dir,
                true,
                cols,
                rows,
            );
            snap_anchor = Some((candidate.member, expected_center, expected_rot));
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
            let target_center = (piece_width * 0.5, piece_height * 0.5);
            let anchor_id = 0usize;
            if let Some(anchor_pos) = positions.get(anchor_id) {
                let current_center = (
                    anchor_pos.0 + piece_width * 0.5,
                    anchor_pos.1 + piece_height * 0.5,
                );
                let dx = current_center.0 - target_center.0;
                let dy = current_center.1 - target_center.1;
                let dist = (dx * dx + dy * dy).sqrt();
                let rotation_ok =
                    !rotation_enabled || angle_matches(group_rot, 0.0, rotation_snap_tolerance);
                let frame_snap_distance =
                    snap_distance * frame_snap_ratio * COMPLETE_SNAP_MULTIPLIER;
                let should_snap = complete_snap
                    || (frame_snap_distance > 0.0 && rotation_ok && dist <= frame_snap_distance);
                if should_snap {
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

    let clamp_ids: &[usize] = if group_after.is_empty() { members } else { &group_after };
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
                if best_dist.is_finite() && (best_shift.0 != 0.0 || best_shift.1 != 0.0) {
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
