//! Logical connectivity state with no world-space placement.

use crate::ids::{EdgeId, GroupId, PieceId};
use crate::topology::PuzzleTopology;

/// Summary of logical puzzle progress.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LogicalStateSummary {
    pub piece_count: u32,
    pub edge_count: u32,
    pub active_edges: u32,
    pub group_count: u32,
    pub solved: bool,
}

/// Per-piece logical slot.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PieceSlot {
    pub group: GroupId,
}

/// Per-group logical slot.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct GroupSlot {
    pub alive: bool,
    pub size: u32,
}

/// Logical puzzle progress model.
///
/// This state is authoritative for piece-group membership and active topology
/// edges. It intentionally excludes render-space transforms.
pub struct LogicalState<T: PuzzleTopology> {
    pub topology: T,
    pub edge_active: Box<[bool]>,
    pub pieces: Box<[PieceSlot]>,
    pub groups: Box<[GroupSlot]>,
    pub scratch_queue: Vec<PieceId>,
    pub scratch_marks: Vec<bool>,
}

impl<T: PuzzleTopology> LogicalState<T> {
    pub fn new(topology: T) -> Self {
        let piece_count = topology.piece_count() as usize;
        let edge_count = topology.edge_count() as usize;

        let pieces = (0..piece_count)
            .map(|i| PieceSlot {
                group: GroupId(i as u32),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let groups = vec![
            GroupSlot {
                alive: true,
                size: 1,
            };
            piece_count
        ]
        .into_boxed_slice();

        Self {
            topology,
            edge_active: vec![false; edge_count].into_boxed_slice(),
            pieces,
            groups,
            scratch_queue: Vec::with_capacity(piece_count),
            scratch_marks: vec![false; piece_count],
        }
    }

    /// Constructs a shuffled baseline logical state (no active joins).
    pub fn shuffled(topology: T) -> Self {
        Self::new(topology)
    }

    /// Constructs a solved logical state by activating all topology edges.
    pub fn solved(topology: T) -> Self {
        let mut state = Self::new(topology);
        state.activate_all_edges();
        state
    }

    pub fn piece_count(&self) -> usize {
        self.pieces.len()
    }

    pub fn edge_count(&self) -> usize {
        self.edge_active.len()
    }

    pub fn active_edge_count(&self) -> usize {
        self.edge_active.iter().filter(|active| **active).count()
    }

    pub fn group_count(&self) -> usize {
        self.groups
            .iter()
            .filter(|slot| slot.alive && slot.size > 0)
            .count()
    }

    pub fn edge_active_slice(&self) -> &[bool] {
        &self.edge_active
    }

    pub fn piece_slots(&self) -> &[PieceSlot] {
        &self.pieces
    }

    pub fn group_slots(&self) -> &[GroupSlot] {
        &self.groups
    }

    pub fn piece_groups(&self) -> impl Iterator<Item = GroupId> + '_ {
        self.pieces.iter().map(|slot| slot.group)
    }

    pub fn group_of(&self, piece: PieceId) -> Option<GroupId> {
        self.pieces.get(piece.as_usize()).map(|slot| slot.group)
    }

    pub fn is_edge_active(&self, edge: EdgeId) -> Option<bool> {
        self.edge_active.get(edge.as_usize()).copied()
    }

    /// Activates one topology edge and merges endpoint groups if needed.
    ///
    /// Returns `true` when the edge activation changed the state.
    pub fn activate_edge(&mut self, edge: EdgeId) -> bool {
        let edge_idx = edge.as_usize();
        if edge_idx >= self.edge_active.len() || self.edge_active[edge_idx] {
            return false;
        }

        let (a, b) = self.topology.edge_endpoints(edge);
        let a_idx = a.as_usize();
        let b_idx = b.as_usize();
        if a_idx >= self.pieces.len() || b_idx >= self.pieces.len() {
            return false;
        }

        self.edge_active[edge_idx] = true;
        let ga = self.pieces[a_idx].group;
        let gb = self.pieces[b_idx].group;
        if ga != gb {
            self.merge_groups(ga, gb);
        }
        true
    }

    pub fn activate_all_edges(&mut self) {
        let edge_count = self.edge_count();
        for idx in 0..edge_count {
            let _ = self.activate_edge(EdgeId(idx as u32));
        }
    }

    pub fn is_solved(&self) -> bool {
        if self.piece_count() == 0 {
            return true;
        }
        self.edge_active.iter().all(|active| *active) && self.group_count() == 1
    }

    pub fn summary(&self) -> LogicalStateSummary {
        LogicalStateSummary {
            piece_count: self.piece_count() as u32,
            edge_count: self.edge_count() as u32,
            active_edges: self.active_edge_count() as u32,
            group_count: self.group_count() as u32,
            solved: self.is_solved(),
        }
    }

    pub fn active_group_ids(&self) -> impl Iterator<Item = GroupId> + '_ {
        self.groups
            .iter()
            .enumerate()
            .filter_map(|(idx, slot)| slot.alive.then_some(GroupId(idx as u32)))
    }

    pub fn members_of(&self, group: GroupId) -> impl Iterator<Item = PieceId> + '_ {
        self.pieces
            .iter()
            .enumerate()
            .filter_map(move |(idx, slot)| (slot.group == group).then_some(PieceId(idx as u32)))
    }

    fn merge_groups(&mut self, lhs: GroupId, rhs: GroupId) {
        if lhs == rhs {
            return;
        }
        let (keep, absorb) = if lhs.as_u32() <= rhs.as_u32() {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };

        let absorb_size = self
            .groups
            .get(absorb.as_usize())
            .map(|slot| slot.size)
            .unwrap_or(0);
        if absorb_size == 0 {
            return;
        }

        for slot in &mut self.pieces {
            if slot.group == absorb {
                slot.group = keep;
            }
        }

        if let Some(keep_slot) = self.groups.get_mut(keep.as_usize()) {
            keep_slot.alive = true;
            keep_slot.size = keep_slot.size.saturating_add(absorb_size);
        }
        if let Some(absorb_slot) = self.groups.get_mut(absorb.as_usize()) {
            absorb_slot.alive = false;
            absorb_slot.size = 0;
        }
    }
}
