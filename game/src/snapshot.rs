//! Canonical playable snapshot DTOs.
//!
//! Compatibility adapters to the legacy core snapshot format are intentionally
//! not included in this iteration.

use crate::ids::{GroupId, PieceId};
use crate::logical::{GroupSlot, LogicalInvariantError, LogicalState, PieceSlot};
use crate::playable::{FlipState, PlayableInvariantError, PlayableState, Pose2};
use crate::rules::PlayRules;
use crate::topology::{build_topology_from_spec, GenericPlayableState, TopologySpec};
use crate::traits::topology::PuzzleTopology;

pub const PLAYABLE_SNAPSHOT_VERSION: u32 = 2;

/// Snapshot validation/restore failure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlayableSnapshotError {
    PieceCountMismatch {
        expected: usize,
        actual: usize,
    },
    EdgeCountMismatch {
        expected: usize,
        actual: usize,
    },
    TopologySpecMismatch {
        snapshot: TopologySpec,
        expected: TopologySpec,
    },
    UnknownTopologySpec {
        topology: TopologySpec,
    },
    PieceGroupLenMismatch {
        expected: usize,
        actual: usize,
    },
    PieceLocalPoseLenMismatch {
        expected: usize,
        actual: usize,
    },
    GroupPoseLenMismatch {
        expected: usize,
        actual: usize,
    },
    GroupFlipLenMismatch {
        expected: usize,
        actual: usize,
    },
    PieceLocalPoseMismatch {
        piece: PieceId,
    },
    PieceGroupOutOfBounds {
        piece: PieceId,
        group: GroupId,
    },
    ZOrderGroupOutOfBounds {
        group: GroupId,
    },
    ZOrderDeadGroup {
        group: GroupId,
    },
    ZOrderDuplicateGroup {
        group: GroupId,
    },
    ZOrderMissingAliveGroup {
        group: GroupId,
    },
    FocusedPieceOutOfBounds {
        piece: PieceId,
    },
    Logical(LogicalInvariantError),
    Playable(PlayableInvariantError),
}

impl From<LogicalInvariantError> for PlayableSnapshotError {
    fn from(value: LogicalInvariantError) -> Self {
        Self::Logical(value)
    }
}

impl From<PlayableInvariantError> for PlayableSnapshotError {
    fn from(value: PlayableInvariantError) -> Self {
        Self::Playable(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlayableSnapshot {
    pub revision: u64,
    pub topology: TopologySpec,
    pub topology_piece_count: u32,
    pub topology_edge_count: u32,
    pub rules: PlayRules,
    pub edge_active: Vec<bool>,
    pub piece_group: Vec<GroupId>,
    pub piece_local_pose: Vec<Pose2>,
    pub group_pose: Vec<Pose2>,
    pub group_flip: Vec<FlipState>,
    pub z_order: Vec<GroupId>,
    pub focused_piece: Option<PieceId>,
}

impl PlayableSnapshot {
    pub fn from_playable<T: PuzzleTopology>(
        playable: &PlayableState<T>,
        focused_piece: Option<PieceId>,
    ) -> Self {
        Self {
            revision: playable.revision,
            topology: playable.logical.topology.to_spec(),
            topology_piece_count: playable.logical.piece_count() as u32,
            topology_edge_count: playable.logical.edge_count() as u32,
            rules: playable.rules,
            edge_active: playable.logical.edge_active_slice().to_vec(),
            piece_group: playable.logical.piece_groups().collect(),
            piece_local_pose: playable.piece_local_pose.to_vec(),
            group_pose: playable.group_pose.to_vec(),
            group_flip: playable.group_flip.to_vec(),
            z_order: playable.z_order.clone(),
            focused_piece,
        }
    }

    pub fn envelope(self) -> SnapshotEnvelope {
        SnapshotEnvelope {
            version: PLAYABLE_SNAPSHOT_VERSION,
            state: self,
        }
    }

    pub fn validate_for_topology<T: PuzzleTopology>(
        &self,
        topology: &T,
    ) -> Result<(), PlayableSnapshotError> {
        let piece_count = topology.piece_count() as usize;
        let edge_count = topology.edge_count() as usize;
        self.validate_topology_descriptor(topology.to_spec())?;
        self.validate_lengths(piece_count, edge_count)?;

        let canonical = PlayableState::new(LogicalState::new(topology), self.rules);
        for (idx, (snapshot_pose, canonical_pose)) in self
            .piece_local_pose
            .iter()
            .zip(canonical.piece_local_pose.iter())
            .enumerate()
        {
            if snapshot_pose != canonical_pose {
                return Err(PlayableSnapshotError::PieceLocalPoseMismatch {
                    piece: PieceId(idx as u32),
                });
            }
        }

        let logical = self.logical_state_for_topology_ref(topology)?;
        logical.validate()?;
        self.validate_z_order(&logical)?;
        Ok(())
    }

    pub fn restore<T: PuzzleTopology>(
        &self,
        topology: T,
    ) -> Result<PlayableState<T>, PlayableSnapshotError> {
        self.validate_for_topology(&topology)?;
        let logical = self.logical_state_for_topology(topology)?;
        let mut playable = PlayableState::new(logical, self.rules);
        playable.revision = self.revision;
        playable.piece_local_pose = self.piece_local_pose.clone().into_boxed_slice();
        playable.group_pose = self.group_pose.clone().into_boxed_slice();
        playable.group_flip = self.group_flip.clone().into_boxed_slice();
        playable.z_order = self.z_order.clone();
        playable.rebuild_z_indices_from_snapshot();
        playable.validate()?;
        Ok(playable)
    }

    pub fn restore_from_descriptor(&self) -> Result<GenericPlayableState, PlayableSnapshotError> {
        let topology = build_topology_from_spec(&self.topology).ok_or_else(|| {
            PlayableSnapshotError::UnknownTopologySpec {
                topology: self.topology.clone(),
            }
        })?;
        self.restore(topology)
    }

    pub fn restore_from_spec(&self) -> Result<GenericPlayableState, PlayableSnapshotError> {
        self.restore_from_descriptor()
    }

    fn validate_topology_descriptor(
        &self,
        expected: TopologySpec,
    ) -> Result<(), PlayableSnapshotError> {
        if self.topology != expected {
            return Err(PlayableSnapshotError::TopologySpecMismatch {
                snapshot: self.topology.clone(),
                expected,
            });
        }
        // The snapshot carries piece/edge counts redundantly with the
        // topology spec; rebuild the topology to confirm they line up.
        if let Some(topology) = build_topology_from_spec(&self.topology) {
            if topology.piece_count() != self.topology_piece_count
                || topology.edge_count() != self.topology_edge_count
            {
                return Err(PlayableSnapshotError::TopologySpecMismatch {
                    snapshot: self.topology.clone(),
                    expected,
                });
            }
        }
        Ok(())
    }

    fn validate_lengths(
        &self,
        piece_count: usize,
        edge_count: usize,
    ) -> Result<(), PlayableSnapshotError> {
        if self.topology_piece_count as usize != piece_count {
            return Err(PlayableSnapshotError::PieceCountMismatch {
                expected: piece_count,
                actual: self.topology_piece_count as usize,
            });
        }
        if self.topology_edge_count as usize != edge_count {
            return Err(PlayableSnapshotError::EdgeCountMismatch {
                expected: edge_count,
                actual: self.topology_edge_count as usize,
            });
        }
        if self.edge_active.len() != edge_count {
            return Err(PlayableSnapshotError::EdgeCountMismatch {
                expected: edge_count,
                actual: self.edge_active.len(),
            });
        }
        if self.piece_group.len() != piece_count {
            return Err(PlayableSnapshotError::PieceGroupLenMismatch {
                expected: piece_count,
                actual: self.piece_group.len(),
            });
        }
        if self.piece_local_pose.len() != piece_count {
            return Err(PlayableSnapshotError::PieceLocalPoseLenMismatch {
                expected: piece_count,
                actual: self.piece_local_pose.len(),
            });
        }
        if self.group_pose.len() != piece_count {
            return Err(PlayableSnapshotError::GroupPoseLenMismatch {
                expected: piece_count,
                actual: self.group_pose.len(),
            });
        }
        if self.group_flip.len() != piece_count {
            return Err(PlayableSnapshotError::GroupFlipLenMismatch {
                expected: piece_count,
                actual: self.group_flip.len(),
            });
        }
        if let Some(piece) = self.focused_piece {
            if piece.as_usize() >= piece_count {
                return Err(PlayableSnapshotError::FocusedPieceOutOfBounds { piece });
            }
        }
        Ok(())
    }

    fn logical_state_for_topology_ref<'a, T: PuzzleTopology>(
        &self,
        topology: &'a T,
    ) -> Result<LogicalState<&'a T>, PlayableSnapshotError> {
        build_logical_state(topology, self)
    }

    fn logical_state_for_topology<T: PuzzleTopology>(
        &self,
        topology: T,
    ) -> Result<LogicalState<T>, PlayableSnapshotError> {
        build_logical_state(topology, self)
    }

    fn validate_z_order<T: PuzzleTopology>(
        &self,
        logical: &LogicalState<T>,
    ) -> Result<(), PlayableSnapshotError> {
        let mut seen = vec![false; logical.group_slots().len()];
        for group in self.z_order.iter().copied() {
            let idx = group.as_usize();
            if idx >= seen.len() {
                return Err(PlayableSnapshotError::ZOrderGroupOutOfBounds { group });
            }
            if !logical.group_slots()[idx].alive {
                return Err(PlayableSnapshotError::ZOrderDeadGroup { group });
            }
            if seen[idx] {
                return Err(PlayableSnapshotError::ZOrderDuplicateGroup { group });
            }
            seen[idx] = true;
        }

        for (idx, slot) in logical.group_slots().iter().copied().enumerate() {
            if slot.alive && !seen[idx] {
                return Err(PlayableSnapshotError::ZOrderMissingAliveGroup {
                    group: GroupId(idx as u32),
                });
            }
        }

        Ok(())
    }
}

pub type RestoredPlayableState = GenericPlayableState;

#[derive(Clone, Debug, PartialEq)]
pub struct SnapshotEnvelope {
    pub version: u32,
    pub state: PlayableSnapshot,
}

impl SnapshotEnvelope {
    pub fn from_playable<T: PuzzleTopology>(
        playable: &PlayableState<T>,
        focused_piece: Option<PieceId>,
    ) -> Self {
        PlayableSnapshot::from_playable(playable, focused_piece).envelope()
    }

    pub fn validate_version(&self) -> Result<(), u32> {
        if self.version == PLAYABLE_SNAPSHOT_VERSION {
            Ok(())
        } else {
            Err(self.version)
        }
    }
}

fn build_logical_state<T: PuzzleTopology>(
    topology: T,
    snapshot: &PlayableSnapshot,
) -> Result<LogicalState<T>, PlayableSnapshotError> {
    let piece_count = topology.piece_count() as usize;
    let edge_count = topology.edge_count() as usize;
    snapshot.validate_lengths(piece_count, edge_count)?;

    let mut groups = vec![
        GroupSlot {
            alive: false,
            size: 0,
        };
        piece_count
    ];
    let mut pieces = Vec::with_capacity(piece_count);

    for (idx, group) in snapshot.piece_group.iter().copied().enumerate() {
        let group_idx = group.as_usize();
        if group_idx >= piece_count {
            return Err(PlayableSnapshotError::PieceGroupOutOfBounds {
                piece: PieceId(idx as u32),
                group,
            });
        }
        groups[group_idx].alive = true;
        groups[group_idx].size = groups[group_idx].size.saturating_add(1);
        pieces.push(PieceSlot { group });
    }

    Ok(LogicalState {
        topology,
        edge_active: snapshot.edge_active.clone().into_boxed_slice(),
        pieces: pieces.into_boxed_slice(),
        groups: groups.into_boxed_slice(),
        scratch_queue: Vec::with_capacity(piece_count),
        scratch_marks: vec![false; piece_count],
    })
}
