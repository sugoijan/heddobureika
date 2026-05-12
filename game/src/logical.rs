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

/// Logical connectivity invariant violation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LogicalInvariantError {
    EdgeEndpointOutOfBounds {
        edge: EdgeId,
        a: PieceId,
        b: PieceId,
    },
    ActiveEdgeAcrossGroups {
        edge: EdgeId,
        a: PieceId,
        b: PieceId,
        group_a: GroupId,
        group_b: GroupId,
    },
    PieceGroupOutOfBounds {
        piece: PieceId,
        group: GroupId,
    },
    PieceInDeadGroup {
        piece: PieceId,
        group: GroupId,
    },
    GroupSizeMismatch {
        group: GroupId,
        expected: u32,
        actual: u32,
    },
    EmptyAliveGroup {
        group: GroupId,
    },
    NonEmptyDeadGroup {
        group: GroupId,
        size: u32,
    },
}

/// Result of an edge activation that can also merge two groups.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LogicalMerge {
    pub edge: EdgeId,
    pub keep: GroupId,
    pub absorbed: Option<GroupId>,
    pub edge_changed: bool,
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

impl<T: PuzzleTopology + Clone> Clone for LogicalState<T> {
    fn clone(&self) -> Self {
        Self {
            topology: self.topology.clone(),
            edge_active: self.edge_active.clone(),
            pieces: self.pieces.clone(),
            groups: self.groups.clone(),
            scratch_queue: self.scratch_queue.clone(),
            scratch_marks: self.scratch_marks.clone(),
        }
    }
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

    /// Validates internal connectivity/group invariants.
    pub fn validate(&self) -> Result<(), LogicalInvariantError> {
        let piece_count = self.pieces.len();
        let group_count = self.groups.len();
        let mut actual_sizes = vec![0u32; group_count];

        for (piece_idx, piece_slot) in self.pieces.iter().copied().enumerate() {
            let group_idx = piece_slot.group.as_usize();
            if group_idx >= group_count {
                return Err(LogicalInvariantError::PieceGroupOutOfBounds {
                    piece: PieceId(piece_idx as u32),
                    group: piece_slot.group,
                });
            }
            let group_slot = self.groups[group_idx];
            if !group_slot.alive {
                return Err(LogicalInvariantError::PieceInDeadGroup {
                    piece: PieceId(piece_idx as u32),
                    group: piece_slot.group,
                });
            }
            actual_sizes[group_idx] = actual_sizes[group_idx].saturating_add(1);
        }

        for (edge_idx, active) in self.edge_active.iter().copied().enumerate() {
            let edge = EdgeId(edge_idx as u32);
            let (a, b) = self.topology.edge_endpoints(edge);
            if a.as_usize() >= piece_count || b.as_usize() >= piece_count {
                return Err(LogicalInvariantError::EdgeEndpointOutOfBounds { edge, a, b });
            }
            if active {
                let group_a = self.pieces[a.as_usize()].group;
                let group_b = self.pieces[b.as_usize()].group;
                if group_a != group_b {
                    return Err(LogicalInvariantError::ActiveEdgeAcrossGroups {
                        edge,
                        a,
                        b,
                        group_a,
                        group_b,
                    });
                }
            }
        }

        for (group_idx, group_slot) in self.groups.iter().copied().enumerate() {
            let group = GroupId(group_idx as u32);
            let actual = actual_sizes[group_idx];
            if group_slot.alive && group_slot.size == 0 {
                return Err(LogicalInvariantError::EmptyAliveGroup { group });
            }
            if !group_slot.alive && group_slot.size != 0 {
                return Err(LogicalInvariantError::NonEmptyDeadGroup {
                    group,
                    size: group_slot.size,
                });
            }
            if group_slot.size != actual {
                return Err(LogicalInvariantError::GroupSizeMismatch {
                    group,
                    expected: group_slot.size,
                    actual,
                });
            }
        }

        Ok(())
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

    /// Activates an edge while preserving a requested endpoint group when a
    /// merge is required.
    pub fn activate_edge_prefer_group(
        &mut self,
        edge: EdgeId,
        preferred_keep: GroupId,
    ) -> Option<LogicalMerge> {
        let edge_idx = edge.as_usize();
        if edge_idx >= self.edge_active.len() {
            return None;
        }

        let (a, b) = self.topology.edge_endpoints(edge);
        let a_idx = a.as_usize();
        let b_idx = b.as_usize();
        if a_idx >= self.pieces.len() || b_idx >= self.pieces.len() {
            return None;
        }

        let edge_changed = !self.edge_active[edge_idx];
        self.edge_active[edge_idx] = true;

        let ga = self.pieces[a_idx].group;
        let gb = self.pieces[b_idx].group;
        let absorbed = if ga == gb {
            None
        } else {
            let keep = if preferred_keep == ga || preferred_keep == gb {
                preferred_keep
            } else if ga.as_u32() <= gb.as_u32() {
                ga
            } else {
                gb
            };
            let absorb = if keep == ga { gb } else { ga };
            self.merge_groups_keep(keep, absorb);
            Some(absorb)
        };

        let keep = match absorbed {
            Some(absorb) if absorb == ga => gb,
            Some(_) => ga,
            None => ga,
        };

        Some(LogicalMerge {
            edge,
            keep,
            absorbed,
            edge_changed,
        })
    }

    /// Deactivates one topology edge and rebuilds connected components from
    /// remaining active edges.
    pub fn deactivate_edge(&mut self, edge: EdgeId) -> bool {
        let edge_idx = edge.as_usize();
        if edge_idx >= self.edge_active.len() || !self.edge_active[edge_idx] {
            return false;
        }

        self.edge_active[edge_idx] = false;
        self.rebuild_groups_from_active_edges();
        true
    }

    /// Detaches a piece by deactivating all active incident edges and
    /// rebuilding components. Returns true when connectivity changed.
    pub fn detach_piece(&mut self, piece: PieceId) -> bool {
        if piece.as_usize() >= self.pieces.len() {
            return false;
        }

        let mut changed = false;
        for edge_idx in 0..self.edge_active.len() {
            if !self.edge_active[edge_idx] {
                continue;
            }
            let edge = EdgeId(edge_idx as u32);
            let (a, b) = self.topology.edge_endpoints(edge);
            if a == piece || b == piece {
                self.edge_active[edge_idx] = false;
                changed = true;
            }
        }

        if changed {
            self.rebuild_groups_from_active_edges();
        }
        changed
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
        self.merge_groups_keep(keep, absorb);
    }

    fn merge_groups_keep(&mut self, keep: GroupId, absorb: GroupId) {
        if keep == absorb {
            return;
        }
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

    fn rebuild_groups_from_active_edges(&mut self) {
        let piece_count = self.pieces.len();
        for (idx, piece) in self.pieces.iter_mut().enumerate() {
            piece.group = GroupId(idx as u32);
        }
        for slot in &mut self.groups {
            slot.alive = false;
            slot.size = 0;
        }

        self.scratch_queue.clear();
        self.scratch_marks.clear();
        self.scratch_marks.resize(piece_count, false);

        let mut adjacency = vec![Vec::<PieceId>::new(); piece_count];
        for edge_idx in 0..self.edge_active.len() {
            if !self.edge_active[edge_idx] {
                continue;
            }
            let (a, b) = self.topology.edge_endpoints(EdgeId(edge_idx as u32));
            if a.as_usize() >= piece_count || b.as_usize() >= piece_count {
                continue;
            }
            adjacency[a.as_usize()].push(b);
            adjacency[b.as_usize()].push(a);
        }

        for start_idx in 0..piece_count {
            if self.scratch_marks[start_idx] {
                continue;
            }

            self.scratch_queue.clear();
            self.scratch_queue.push(PieceId(start_idx as u32));
            self.scratch_marks[start_idx] = true;
            let mut component = Vec::new();
            let mut keep = PieceId(start_idx as u32);
            let mut cursor = 0;

            while cursor < self.scratch_queue.len() {
                let piece = self.scratch_queue[cursor];
                cursor += 1;
                keep = keep.min(piece);
                component.push(piece);

                for neighbor in adjacency[piece.as_usize()].iter().copied() {
                    let idx = neighbor.as_usize();
                    if !self.scratch_marks[idx] {
                        self.scratch_marks[idx] = true;
                        self.scratch_queue.push(neighbor);
                    }
                }
            }

            let group = GroupId(keep.as_u32());
            for piece in component.iter().copied() {
                self.pieces[piece.as_usize()].group = group;
            }
            if let Some(slot) = self.groups.get_mut(group.as_usize()) {
                slot.alive = true;
                slot.size = component.len() as u32;
            }
        }
    }
}
