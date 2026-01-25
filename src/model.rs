use serde::{Deserialize, Serialize};

use crate::core::{groups_from_connections, rotate_vec};

pub(crate) type PieceId = usize;
pub(crate) type GroupId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) struct Transform2d {
    pub(crate) pos: [f32; 2],
    pub(crate) rot_deg: f32,
}

impl Transform2d {
    pub(crate) fn new(x: f32, y: f32, rot_deg: f32) -> Self {
        Self {
            pos: [x, y],
            rot_deg,
        }
    }

    pub(crate) fn apply_to(&self, local: [f32; 2]) -> [f32; 2] {
        let (rx, ry) = rotate_vec(local[0], local[1], self.rot_deg);
        [self.pos[0] + rx, self.pos[1] + ry]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum PieceState {
    Solo { group: GroupId },
    InGroup { group: GroupId },
}

impl PieceState {
    pub(crate) fn group(self) -> GroupId {
        match self {
            PieceState::Solo { group } => group,
            PieceState::InGroup { group } => group,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) struct GroupState {
    pub(crate) id: GroupId,
    pub(crate) anchor: PieceId,
    pub(crate) transform: Transform2d,
    pub(crate) members: Vec<PieceId>,
}

impl GroupState {
    pub(crate) fn offset_for(
        &self,
        piece: PieceId,
        cols: usize,
        piece_width: f32,
        piece_height: f32,
    ) -> [f32; 2] {
        let col = (piece % cols) as f32;
        let row = (piece / cols) as f32;
        let anchor_col = (self.anchor % cols) as f32;
        let anchor_row = (self.anchor / cols) as f32;
        [
            (col - anchor_col) * piece_width,
            (row - anchor_row) * piece_height,
        ]
    }

    pub(crate) fn piece_transform(
        &self,
        piece: PieceId,
        cols: usize,
        piece_width: f32,
        piece_height: f32,
    ) -> Transform2d {
        let offset = self.offset_for(piece, cols, piece_width, piece_height);
        let pos = self.transform.apply_to(offset);
        Transform2d {
            pos,
            rot_deg: self.transform.rot_deg,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) struct PuzzleInstance {
    pub(crate) label: String,
    pub(crate) image_src: String,
    pub(crate) rows: u32,
    pub(crate) cols: u32,
    pub(crate) shape_seed: u32,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) struct PuzzleState {
    pub(crate) pieces: Vec<PieceState>,
    pub(crate) groups: Vec<Option<GroupState>>,
    pub(crate) group_order: Vec<GroupId>,
    pub(crate) connections: Vec<[bool; 4]>,
    pub(crate) flips: Vec<bool>,
    pub(crate) scramble_nonce: u32,
}

impl PuzzleState {
    pub(crate) fn empty() -> Self {
        Self {
            pieces: Vec::new(),
            groups: Vec::new(),
            group_order: Vec::new(),
            connections: Vec::new(),
            flips: Vec::new(),
            scramble_nonce: 0,
        }
    }

    pub(crate) fn total_pieces(&self) -> usize {
        self.pieces.len()
    }

    pub(crate) fn group(&self, group: GroupId) -> Option<&GroupState> {
        self.groups.get(group).and_then(|entry| entry.as_ref())
    }

    pub(crate) fn anchor_of(&self, piece: PieceId) -> Option<GroupId> {
        self.pieces.get(piece).map(|state| state.group())
    }

    pub(crate) fn piece_transform(
        &self,
        piece: PieceId,
        cols: usize,
        piece_width: f32,
        piece_height: f32,
    ) -> Option<Transform2d> {
        let group_id = self.anchor_of(piece)?;
        let group = self.group(group_id)?;
        Some(group.piece_transform(piece, cols, piece_width, piece_height))
    }

    pub(crate) fn derive_piece_transforms(
        &self,
        cols: usize,
        piece_width: f32,
        piece_height: f32,
    ) -> (Vec<(f32, f32)>, Vec<f32>) {
        let total = self.total_pieces();
        let mut positions = vec![(0.0, 0.0); total];
        let mut rotations = vec![0.0; total];
        for piece in 0..total {
            if let Some(transform) =
                self.piece_transform(piece, cols, piece_width, piece_height)
            {
                positions[piece] = (transform.pos[0], transform.pos[1]);
                rotations[piece] = transform.rot_deg;
            }
        }
        (positions, rotations)
    }

    pub(crate) fn build_piece_order(&self) -> Vec<PieceId> {
        build_piece_order_from_groups(&self.group_order, &self.groups, self.total_pieces())
    }

    pub(crate) fn rebuild_from_piece_state(
        positions: &[(f32, f32)],
        rotations: &[f32],
        flips: &[bool],
        connections: &[[bool; 4]],
        cols: usize,
        rows: usize,
        piece_order: Option<&[PieceId]>,
        scramble_nonce: u32,
    ) -> Self {
        let total = cols * rows;
        let mut pieces = vec![PieceState::Solo { group: 0 }; total];
        let mut groups: Vec<Option<GroupState>> = vec![None; total];
        let group_lists = groups_from_connections(connections, cols, rows);
        for group in group_lists {
            if group.is_empty() {
                continue;
            }
            let anchor = group[0];
            let transform = if anchor < positions.len() {
                let pos = positions[anchor];
                let rot = rotations.get(anchor).copied().unwrap_or(0.0);
                Transform2d::new(pos.0, pos.1, rot)
            } else {
                Transform2d::new(0.0, 0.0, 0.0)
            };
            let is_solo = group.len() == 1;
            for &id in &group {
                if id < total {
                    pieces[id] = if is_solo {
                        PieceState::Solo { group: anchor }
                    } else {
                        PieceState::InGroup { group: anchor }
                    };
                }
            }
            groups[anchor] = Some(GroupState {
                id: anchor,
                anchor,
                transform,
                members: group,
            });
        }
        let group_order = if let Some(order) = piece_order {
            build_group_order_from_piece_order(order, &pieces, &groups, total)
        } else {
            let mut order = Vec::new();
            for id in 0..total {
                if let Some(group) = groups.get(id).and_then(|entry| entry.as_ref()) {
                    if group.anchor == id {
                        order.push(id);
                    }
                }
            }
            order
        };
        Self {
            pieces,
            groups,
            group_order,
            connections: connections.to_vec(),
            flips: flips.to_vec(),
            scramble_nonce,
        }
    }
}

pub(crate) fn build_group_order_from_piece_order(
    piece_order: &[PieceId],
    pieces: &[PieceState],
    groups: &[Option<GroupState>],
    total: usize,
) -> Vec<GroupId> {
    let mut seen = vec![false; total];
    let mut group_order = Vec::new();
    for &id in piece_order {
        if id >= total {
            continue;
        }
        let group = pieces[id].group();
        if group < total && !seen[group] {
            seen[group] = true;
            group_order.push(group);
        }
    }
    for group_id in 0..total {
        if seen[group_id] {
            continue;
        }
        if groups
            .get(group_id)
            .and_then(|entry| entry.as_ref())
            .is_some()
        {
            group_order.push(group_id);
        }
    }
    group_order
}

pub(crate) fn build_piece_order_from_groups(
    group_order: &[GroupId],
    groups: &[Option<GroupState>],
    total: usize,
) -> Vec<PieceId> {
    let mut order = Vec::with_capacity(total);
    let mut group_seen = vec![false; total];
    for &group_id in group_order {
        if group_id >= total {
            continue;
        }
        if let Some(group) = groups.get(group_id).and_then(|entry| entry.as_ref()) {
            group_seen[group_id] = true;
            order.extend_from_slice(&group.members);
        }
    }
    for group_id in 0..total {
        if group_seen[group_id] {
            continue;
        }
        if let Some(group) = groups.get(group_id).and_then(|entry| entry.as_ref()) {
            order.extend_from_slice(&group.members);
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
