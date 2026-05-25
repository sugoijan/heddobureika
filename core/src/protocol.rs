use std::fmt;

use rkyv::{Archive, Deserialize, Serialize};

use crate::snapshot::{
    PlayableGameSnapshot, PlayableGameStateSnapshot, PlayablePoseSnapshot,
    PlayablePositionSnapshot, PlayableTopologySnapshot, PuzzleImageRef,
};
use heddobureika_game::{
    EdgeId, FlipState, GroupId, GroupMergeUpdate, PlayableState, PlayableUpdateBatch, Pose2,
    ProposalApplyRejection, ProposalApplyStatus, PuzzleTopology, SnapRejectionReason,
};

pub const PRIVATE_UPLOAD_MAX_BYTES: u32 = 10 * 1024 * 1024;
pub const PRIVATE_ASSET_MAX_BYTES: u32 = 3 * 1024 * 1024;
pub const ASSET_CHUNK_BYTES: usize = 32 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RoomPersistence {
    Durable,
    BestEffort,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct PuzzleSpec {
    pub image_ref: PuzzleImageRef,
    /// Target piece count. Only consulted when `topology` is `None`, where
    /// it drives the back-compat grid fallback (CLI, older clients).
    pub pieces: Option<u32>,
    /// Scramble seed — controls how pieces are scattered. `None` randomises.
    pub seed: Option<u32>,
    /// Fully resolved topology to build. When `Some`, the worker re-fits it
    /// to the room's image dimensions (so aspect-dependent topologies stay
    /// correct) and builds it directly. When `None`, falls back to a grid
    /// derived from `pieces`.
    pub topology: Option<PlayableTopologySnapshot>,
    /// Tab/blank edge-direction seed (`PuzzleInfo::shape_seed`). `None` uses
    /// the worker default; the UI "Regenerate" action bumps it so the layout
    /// re-rolls.
    pub shape_seed: Option<u32>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Archive, Serialize, Deserialize)]
#[repr(transparent)]
pub struct ClientId(pub u64);

impl ClientId {
    pub fn as_u64(self) -> u64 {
        self.0
    }
}

impl From<u64> for ClientId {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl From<ClientId> for u64 {
    fn from(value: ClientId) -> Self {
        value.0
    }
}

impl fmt::Display for ClientId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for ClientId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = self.0;
        write!(
            f,
            "cid:{:04x}_{:04x}_{:04x}_{:04x}",
            (value >> 48) as u16,
            (value >> 32) as u16,
            (value >> 16) as u16,
            value as u16
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum OwnershipReason {
    Granted,
    Released,
    Timeout,
    AutoRelease,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RecordedCommandKind {
    Init,
    AssetRequest,
    Select,
    Move,
    Transform,
    Rotate,
    Place,
    Flip,
    Release,
    Ping,
    Detach,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RecordedCommandOutcome {
    Applied,
    AcceptedNoStateChange,
    Ignored,
    Rejected,
    HandlerError,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct RecordedCommand {
    pub id: u64,
    pub ts_ms: i64,
    pub client_id: ClientId,
    pub kind: RecordedCommandKind,
    pub piece_id: Option<u32>,
    pub anchor_id: Option<u32>,
    pub pos: Option<(f32, f32)>,
    pub rot_deg: Option<f32>,
    pub client_seq: Option<u64>,
    pub room_seq: Option<u64>,
    pub outcome: RecordedCommandOutcome,
    pub reason: Option<String>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum RoomControlUpdate {
    Ownership {
        /// Canonical anchor piece id for the group (`min` of group members).
        /// Stable across merges as long as the lowest-id member remains in
        /// the group.
        group_anchor: u32,
        owner: Option<ClientId>,
        reason: OwnershipReason,
    },
    GroupOrder {
        order: Vec<u32>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub struct PlayableDeltaSnapshot {
    pub revision: u64,
    pub dirty_groups: Vec<u32>,
    pub dirty_pieces: Vec<u32>,
    pub dirty_edges: Vec<u32>,
    pub z_order_changed: bool,
    pub membership_changed: bool,
    pub solved_changed: bool,
}

impl From<&heddobureika_game::PlayableDelta> for PlayableDeltaSnapshot {
    fn from(value: &heddobureika_game::PlayableDelta) -> Self {
        Self {
            revision: value.revision,
            dirty_groups: value
                .dirty_groups
                .iter()
                .map(|group| group.as_u32())
                .collect(),
            dirty_pieces: value
                .dirty_pieces
                .iter()
                .map(|piece| piece.as_u32())
                .collect(),
            dirty_edges: value.dirty_edges.iter().map(|edge| edge.as_u32()).collect(),
            z_order_changed: value.z_order_changed,
            membership_changed: value.membership_changed,
            solved_changed: value.solved_changed,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum PlayableRoomUpdateKind {
    ActionOnly,
    Snap,
    RestrictedAction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum ProposalApplyStatusSnapshot {
    ActionOnly,
    Accepted,
    Rebased,
    Noop,
    Rejected,
}

impl From<ProposalApplyStatus> for ProposalApplyStatusSnapshot {
    fn from(value: ProposalApplyStatus) -> Self {
        match value {
            ProposalApplyStatus::ActionOnly => Self::ActionOnly,
            ProposalApplyStatus::Accepted => Self::Accepted,
            ProposalApplyStatus::Rebased => Self::Rebased,
            ProposalApplyStatus::Noop => Self::Noop,
            ProposalApplyStatus::Rejected => Self::Rejected,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum SnapRejectionReasonSnapshot {
    MoverGroupMissing,
    SameGroup,
    FlippedGroup,
    RotationMismatch,
    OutsideSnapDistance,
    InvalidTopology,
}

impl From<SnapRejectionReason> for SnapRejectionReasonSnapshot {
    fn from(value: SnapRejectionReason) -> Self {
        match value {
            SnapRejectionReason::MoverGroupMissing => Self::MoverGroupMissing,
            SnapRejectionReason::SameGroup => Self::SameGroup,
            SnapRejectionReason::FlippedGroup => Self::FlippedGroup,
            SnapRejectionReason::RotationMismatch => Self::RotationMismatch,
            SnapRejectionReason::OutsideSnapDistance => Self::OutsideSnapDistance,
            SnapRejectionReason::InvalidTopology => Self::InvalidTopology,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub enum ProposalApplyRejectionSnapshot {
    MoverGroupMissing,
    NoCandidate,
    CandidateRejected(SnapRejectionReasonSnapshot),
}

impl From<ProposalApplyRejection> for ProposalApplyRejectionSnapshot {
    fn from(value: ProposalApplyRejection) -> Self {
        match value {
            ProposalApplyRejection::MoverGroupMissing => Self::MoverGroupMissing,
            ProposalApplyRejection::NoCandidate => Self::NoCandidate,
            ProposalApplyRejection::CandidateRejected(reason) => {
                Self::CandidateRejected(reason.into())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub struct GroupMergeSnapshot {
    pub keep: u32,
    pub absorbed: u32,
}

impl From<GroupMergeUpdate> for GroupMergeSnapshot {
    fn from(value: GroupMergeUpdate) -> Self {
        Self {
            keep: value.keep.as_u32(),
            absorbed: value.absorbed.as_u32(),
        }
    }
}

/// Post-update authoritative state for one dirty group.
#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct GroupChangeSnapshot {
    pub group: u32,
    pub pose: PlayablePoseSnapshot,
    pub flipped: bool,
    pub alive: bool,
}

/// Post-update authoritative membership for one dirty piece.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub struct PieceChangeSnapshot {
    pub piece: u32,
    pub group: u32,
}

#[derive(Debug, Clone, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayableRoomUpdate {
    pub kind: PlayableRoomUpdateKind,
    pub action_id: Option<u64>,
    pub status: ProposalApplyStatusSnapshot,
    pub rejection: Option<ProposalApplyRejectionSnapshot>,
    pub rebased: bool,
    pub base_revision: u64,
    pub revision_before: u64,
    pub revision_after: u64,
    pub mover_group: u32,
    pub fixed_group: Option<u32>,
    pub activated_edges: Vec<u32>,
    /// Edges that flipped from active to inactive (e.g. by a detach). Clients
    /// apply these directly to `logical.edge_active` so the bitmap stays
    /// consistent with the piece→group map — without them, any later
    /// `logical.detach_piece` would resurrect stale active edges and
    /// reconnect previously-detached pieces.
    pub deactivated_edges: Vec<u32>,
    pub merged_groups: Vec<GroupMergeSnapshot>,
    pub group_changes: Vec<GroupChangeSnapshot>,
    pub piece_changes: Vec<PieceChangeSnapshot>,
    pub z_order_changed: bool,
    pub membership_changed: bool,
    pub solved_changed: bool,
}

impl PlayableRoomUpdate {
    /// Builds a wire update from a freshly applied `PlayableUpdateBatch` and
    /// the post-apply state snapshot. The batch's delta drives which
    /// group/piece states are read into the update.
    ///
    /// Until the worker holds a live `PlayableState`, this is the cheap path:
    /// it reads directly from the snapshot arrays without re-deriving state.
    pub fn from_batch_and_state(
        kind: PlayableRoomUpdateKind,
        batch: &PlayableUpdateBatch,
        state: &PlayableGameStateSnapshot,
    ) -> Self {
        let applied = &batch.proposal;
        let group_changes = batch
            .delta
            .dirty_groups
            .iter()
            .copied()
            .filter_map(|group| {
                let idx = group.as_usize();
                let pose = state.group_pose.get(idx).copied()?;
                let flipped = state.group_flip.get(idx).copied().unwrap_or(false);
                Some(GroupChangeSnapshot {
                    group: group.as_u32(),
                    pose,
                    flipped,
                    alive: state.z_order.iter().any(|g| *g == group.as_u32())
                        || flipped
                        || state.piece_group.iter().any(|g| *g == group.as_u32()),
                })
            })
            .collect::<Vec<_>>();
        let piece_changes = batch
            .delta
            .dirty_pieces
            .iter()
            .copied()
            .filter_map(|piece| {
                let group = state.piece_group.get(piece.as_usize()).copied()?;
                Some(PieceChangeSnapshot {
                    piece: piece.as_u32(),
                    group,
                })
            })
            .collect::<Vec<_>>();
        Self {
            kind,
            action_id: applied.action_id.map(|id| id.0),
            status: applied.status.into(),
            rejection: applied.rejection.map(ProposalApplyRejectionSnapshot::from),
            rebased: applied.rebased,
            base_revision: applied.base_revision,
            revision_before: batch.revision_before,
            revision_after: batch.revision_after,
            mover_group: applied.mover_group.as_u32(),
            fixed_group: applied.fixed_group.map(|group| group.as_u32()),
            activated_edges: applied
                .activated_edges
                .iter()
                .map(|edge| edge.as_u32())
                .collect(),
            deactivated_edges: applied
                .deactivated_edges
                .iter()
                .map(|edge| edge.as_u32())
                .collect(),
            merged_groups: applied
                .merged_groups
                .iter()
                .copied()
                .map(GroupMergeSnapshot::from)
                .collect(),
            group_changes,
            piece_changes,
            z_order_changed: batch.delta.z_order_changed,
            membership_changed: batch.delta.membership_changed,
            solved_changed: batch.delta.solved_changed,
        }
    }

    /// Builds a wire update from a freshly applied `PlayableUpdateBatch` and
    /// the authoritative `PlayableState` after the apply. The batch's delta
    /// drives which group/piece states are read into the update.
    pub fn from_batch_and_playable<T: PuzzleTopology>(
        kind: PlayableRoomUpdateKind,
        batch: &PlayableUpdateBatch,
        playable: &PlayableState<T>,
    ) -> Self {
        let applied = &batch.proposal;
        let group_changes = batch
            .delta
            .dirty_groups
            .iter()
            .copied()
            .map(|group| GroupChangeSnapshot {
                group: group.as_u32(),
                pose: PlayablePoseSnapshot::from_pose(
                    playable.pose_of(group).unwrap_or(Pose2::default()),
                ),
                flipped: playable.flip_of(group) == Some(FlipState::Flipped),
                alive: playable
                    .logical
                    .groups
                    .get(group.as_usize())
                    .map(|slot| slot.alive)
                    .unwrap_or(false),
            })
            .collect::<Vec<_>>();
        let piece_changes = batch
            .delta
            .dirty_pieces
            .iter()
            .copied()
            .filter_map(|piece| {
                let group = playable.logical.group_of(piece)?;
                Some(PieceChangeSnapshot {
                    piece: piece.as_u32(),
                    group: group.as_u32(),
                })
            })
            .collect::<Vec<_>>();
        Self {
            kind,
            action_id: applied.action_id.map(|id| id.0),
            status: applied.status.into(),
            rejection: applied.rejection.map(ProposalApplyRejectionSnapshot::from),
            rebased: applied.rebased,
            base_revision: applied.base_revision,
            revision_before: batch.revision_before,
            revision_after: batch.revision_after,
            mover_group: applied.mover_group.as_u32(),
            fixed_group: applied.fixed_group.map(|group| group.as_u32()),
            activated_edges: applied
                .activated_edges
                .iter()
                .map(|edge| edge.as_u32())
                .collect(),
            deactivated_edges: applied
                .deactivated_edges
                .iter()
                .map(|edge| edge.as_u32())
                .collect(),
            merged_groups: applied
                .merged_groups
                .iter()
                .copied()
                .map(GroupMergeSnapshot::from)
                .collect(),
            group_changes,
            piece_changes,
            z_order_changed: batch.delta.z_order_changed,
            membership_changed: batch.delta.membership_changed,
            solved_changed: batch.delta.solved_changed,
        }
    }

    /// Applies the wire update to a local `PlayableState` shadow. Returns
    /// `false` when the update could not be applied because `revision_before`
    /// disagrees with the state's current revision.
    pub fn apply_to_playable<T: PuzzleTopology>(&self, playable: &mut PlayableState<T>) -> bool {
        if playable.revision != self.revision_before {
            return false;
        }
        for edge in &self.activated_edges {
            let _ = playable.logical.activate_edge(EdgeId(*edge));
        }
        // Deactivate edges by direct write — `logical.deactivate_edge()` would
        // trigger `rebuild_groups_from_active_edges()`, which we don't want:
        // the authoritative piece→group map is delivered via `piece_changes`
        // below, and rebuilding from edge_active before applying that map
        // would just clobber it.
        for edge in &self.deactivated_edges {
            let edge_idx = *edge as usize;
            if let Some(slot) = playable.logical.edge_active.get_mut(edge_idx) {
                *slot = false;
            }
        }
        for merge in &self.merged_groups {
            if let Some(absorbed) = playable.logical.groups.get_mut(merge.absorbed as usize) {
                absorbed.alive = false;
                absorbed.size = 0;
            }
        }
        for change in &self.piece_changes {
            let piece_idx = change.piece as usize;
            if let Some(slot) = playable.logical.pieces.get_mut(piece_idx) {
                let prev_group = slot.group;
                let new_group = GroupId(change.group);
                if prev_group != new_group {
                    if let Some(prev_slot) = playable.logical.groups.get_mut(prev_group.as_usize())
                    {
                        if prev_slot.size > 0 {
                            prev_slot.size -= 1;
                        }
                        if prev_slot.size == 0 {
                            prev_slot.alive = false;
                        }
                    }
                    slot.group = new_group;
                    if let Some(new_slot) = playable.logical.groups.get_mut(change.group as usize) {
                        new_slot.alive = true;
                        new_slot.size = new_slot.size.saturating_add(1);
                    }
                }
            }
        }
        for change in &self.group_changes {
            let group_idx = change.group as usize;
            if let Some(pose) = change.pose.to_pose().ok() {
                if let Some(slot_pose) = playable.group_pose.get_mut(group_idx) {
                    *slot_pose = pose;
                }
            }
            if let Some(slot_flip) = playable.group_flip.get_mut(group_idx) {
                *slot_flip = if change.flipped {
                    FlipState::Flipped
                } else {
                    FlipState::Normal
                };
            }
            if let Some(group_slot) = playable.logical.groups.get_mut(group_idx) {
                group_slot.alive = change.alive;
                if !change.alive {
                    group_slot.size = 0;
                }
            }
        }
        if self.z_order_changed || self.membership_changed {
            let alive_groups = playable.logical.active_group_ids().collect::<Vec<_>>();
            playable
                .z_order
                .retain(|group| alive_groups.contains(group));
            for group in alive_groups {
                if !playable.z_order.contains(&group) {
                    playable.z_order.push(group);
                }
            }
            playable.rebuild_z_indices_from_snapshot();
        }
        // Invariant safety net: edge_active[e] must equal (group_of(a) ==
        // group_of(b)) for every edge. Holds automatically when the wire
        // update carries the right activated/deactivated_edges, but a future
        // action that deactivates edges without populating
        // `AppliedProposal::deactivated_edges` would otherwise leave stale
        // active edges that resurrect on the next local `detach_piece`. The
        // sweep is O(edge_count) and only runs when membership shifted.
        if self.membership_changed
            || !self.activated_edges.is_empty()
            || !self.deactivated_edges.is_empty()
        {
            let edge_count = playable.logical.edge_count();
            for edge_idx in 0..edge_count {
                let edge = EdgeId(edge_idx as u32);
                let (a, b) = playable.logical.topology.edge_endpoints(edge);
                let same_group = match (playable.logical.group_of(a), playable.logical.group_of(b))
                {
                    (Some(ga), Some(gb)) => ga == gb,
                    _ => false,
                };
                if let Some(slot) = playable.logical.edge_active.get_mut(edge_idx) {
                    if *slot != same_group {
                        *slot = same_group;
                    }
                }
            }
        }
        playable.revision = self.revision_after;
        true
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum AdminMsg {
    Create {
        persistence: RoomPersistence,
        puzzle: PuzzleSpec,
    },
    ChangePuzzle {
        puzzle: PuzzleSpec,
    },
    UploadPrivateBegin {
        mime: String,
        size: u32,
    },
    UploadPrivateChunk {
        bytes: Vec<u8>,
    },
    UploadPrivateEnd {
        pieces: Option<u32>,
        seed: Option<u32>,
        topology: Option<PlayableTopologySnapshot>,
        shape_seed: Option<u32>,
    },
    Scramble {
        seed: Option<u32>,
    },
    /// Place every piece in its solved position (one connected group),
    /// authoritatively, and broadcast the result to the room.
    Solve,
    RecordingSet {
        enabled: bool,
        max_events: Option<u32>,
    },
    RecordingStatus,
    RecordingExport {
        after_id: Option<u64>,
        limit: u32,
    },
    RecordingClear,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ClientMsg {
    Init {
        snapshot: PlayableGameSnapshot,
    },
    AssetRequest {
        hash: String,
    },
    Select {
        piece_id: u32,
    },
    Move {
        piece_id: u32,
        drop_pos: PlayablePositionSnapshot,
        client_seq: u64,
        base_revision: u64,
    },
    Transform {
        piece_id: u32,
        drop_pose: PlayablePoseSnapshot,
        client_seq: u64,
        base_revision: u64,
    },
    Rotate {
        piece_id: u32,
        drop_rotation_deg: f32,
        base_revision: u64,
    },
    Place {
        piece_id: u32,
        drop_pose: PlayablePoseSnapshot,
        client_seq: u64,
        base_revision: u64,
    },
    Flip {
        piece_id: u32,
        flipped: bool,
        /// Post-flip world pose of the (singleton) piece. The server applies
        /// it as the target pose so the click-pivot adjustment computed by
        /// the originating client is reproduced authoritatively.
        drop_pose: PlayablePoseSnapshot,
        base_revision: u64,
    },
    Detach {
        piece_id: u32,
        base_revision: u64,
    },
    Release {
        piece_id: u32,
    },
    Ping {
        nonce: Option<u64>,
    },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ServerMsg {
    Welcome {
        room_id: String,
        persistence: RoomPersistence,
        initialized: bool,
        client_id: Option<ClientId>,
    },
    AdminAck {
        room_id: String,
        persistence: RoomPersistence,
    },
    UploadAck {
        hash: String,
    },
    RecordingStatus {
        enabled: bool,
        capped: bool,
        max_events: u32,
        event_count: u64,
        dropped_events: u64,
    },
    RecordingRows {
        rows: Vec<RecordedCommand>,
        next_after_id: Option<u64>,
    },
    RecordingCleared,
    NeedInit,
    Warning {
        minutes_idle: u32,
    },
    AssetBegin {
        hash: String,
        mime: String,
        width: u32,
        height: u32,
        size: u32,
    },
    AssetChunk {
        hash: String,
        index: u32,
        bytes: Vec<u8>,
    },
    AssetEnd {
        hash: String,
    },
    State {
        seq: u64,
        snapshot: PlayableGameSnapshot,
    },
    ControlUpdate {
        seq: u64,
        update: RoomControlUpdate,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    },
    PlayableUpdate {
        seq: u64,
        update: PlayableRoomUpdate,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    },
    Pong {
        nonce: Option<u64>,
    },
    Error {
        code: String,
        message: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use heddobureika_game::{
        GridTopology, LogicalState, PieceId, PlayRules, PlayableState, RestrictedPlayableAction,
    };

    /// Regression: a server-confirmed detach must leave the client's
    /// `edge_active` bitmap consistent with the new piece→group map, so that
    /// a SUBSEQUENT local detach (e.g. the client's optimistic shift-drag)
    /// does not resurrect the previously-detached piece via
    /// `rebuild_groups_from_active_edges`.
    ///
    /// Before the fix, the wire update carried only `activated_edges` and
    /// `piece_changes` — `edge_active` was never cleared for the detached
    /// piece, so on the next local `detach_piece` the rebuild reconnected
    /// every previously-detached piece back into the main group.
    #[test]
    fn server_detach_keeps_edges_consistent_for_subsequent_local_detach() {
        let topology = GridTopology::try_new(3, 3).expect("3x3 topology");
        let mut logical = LogicalState::new(topology);
        logical.activate_all_edges();
        let server = PlayableState::new(logical, PlayRules::default());
        let mut client = server.clone();

        // Server detaches piece 0.
        let mut server = server;
        let batch = server.apply_restricted_action_batch(
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece: PieceId(0),
                target_pose: Pose2::default(),
                target_flip: FlipState::Normal,
            },
            None,
        );
        let update = PlayableRoomUpdate::from_batch_and_playable(
            PlayableRoomUpdateKind::ActionOnly,
            &batch,
            &server,
        );

        // Ship the wire update to the client.
        assert!(update.apply_to_playable(&mut client));

        let piece0_group_after_first = client
            .logical
            .group_of(PieceId(0))
            .expect("piece 0 grouped");
        let members_after_first: Vec<_> = client
            .logical
            .members_of(piece0_group_after_first)
            .collect();
        assert_eq!(
            members_after_first,
            vec![PieceId(0)],
            "piece 0 should be a singleton on the client after the server detach"
        );

        // Now the client optimistically detaches piece 1 (simulating
        // `begin_drag` with shift-key on a fresh piece). The bug: this would
        // rebuild groups from `edge_active`, and since piece 0's edges were
        // never deactivated on the client, piece 0 would get reconnected to
        // the main group.
        let _ = client.apply_restricted_action_batch(
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece: PieceId(1),
                target_pose: Pose2::default(),
                target_flip: FlipState::Normal,
            },
            None,
        );

        let piece0_group = client
            .logical
            .group_of(PieceId(0))
            .expect("piece 0 still grouped");
        let members: Vec<_> = client.logical.members_of(piece0_group).collect();
        assert_eq!(
            members,
            vec![PieceId(0)],
            "piece 0 must remain a singleton after the second local detach (otherwise the puzzle flickers back together)"
        );
    }

    #[test]
    fn puzzle_spec_round_trips_topology_and_shape_seed() {
        use crate::codec::{decode, encode};

        let topology: crate::PlayableTopologySnapshot =
            heddobureika_game::TopologySpec::voronoi(80, 3, 1.5).into();
        let msg = AdminMsg::Create {
            persistence: RoomPersistence::Durable,
            puzzle: PuzzleSpec {
                image_ref: crate::PuzzleImageRef::BuiltIn {
                    slug: "demo".to_string(),
                },
                pieces: Some(80),
                seed: Some(42),
                topology: Some(topology.clone()),
                shape_seed: Some(7),
            },
        };
        let bytes = encode(&msg).expect("encode");
        let decoded: AdminMsg = decode(&bytes).expect("decode");
        match decoded {
            AdminMsg::Create { puzzle, .. } => {
                assert_eq!(puzzle.seed, Some(42));
                assert_eq!(puzzle.shape_seed, Some(7));
                assert_eq!(puzzle.topology, Some(topology));
            }
            other => panic!("unexpected variant: {other:?}"),
        }
    }

    #[test]
    fn upload_private_end_round_trips_topology_and_shape_seed() {
        use crate::codec::{decode, encode};

        let topology: crate::PlayableTopologySnapshot =
            heddobureika_game::TopologySpec::grid(4, 3).into();
        let msg = AdminMsg::UploadPrivateEnd {
            pieces: Some(12),
            seed: None,
            topology: Some(topology.clone()),
            shape_seed: Some(99),
        };
        let bytes = encode(&msg).expect("encode");
        let decoded: AdminMsg = decode(&bytes).expect("decode");
        match decoded {
            AdminMsg::UploadPrivateEnd {
                topology: decoded_topology,
                shape_seed,
                ..
            } => {
                assert_eq!(shape_seed, Some(99));
                assert_eq!(decoded_topology, Some(topology));
            }
            other => panic!("unexpected variant: {other:?}"),
        }
    }
}
