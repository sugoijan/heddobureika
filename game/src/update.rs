//! Ordered authoritative update batches for multiplayer/app sync.

use crate::delta::PlayableDelta;
use crate::ids::{EdgeId, GroupId};
use crate::playable::Pose2;
use crate::snap::{ActionId, SnapRejectionReason};

/// Outcome of applying a snap proposal to the current authoritative state.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProposalApplyStatus {
    /// A gameplay action changed state but did not activate a join.
    ActionOnly,
    /// The proposal matched current state and activated at least one edge.
    Accepted,
    /// The proposal was stale but rebased successfully before activation.
    Rebased,
    /// The proposal did not activate any edge and did not change state.
    Noop,
    /// The proposal could not be applied to the current state.
    Rejected,
}

/// Reason a proposal did not apply.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProposalApplyRejection {
    MoverGroupMissing,
    NoCandidate,
    CandidateRejected(SnapRejectionReason),
}

/// Group merge summary included in ordered update batches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GroupMergeUpdate {
    pub keep: GroupId,
    pub absorbed: GroupId,
}

/// Final pose for a group touched by an ordered update batch.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GroupPoseUpdate {
    pub group: GroupId,
    pub pose: Pose2,
}

/// Proposal result plus pull-style dirty delta.
#[derive(Clone, Debug, PartialEq)]
pub struct AppliedProposal {
    pub action_id: Option<ActionId>,
    pub base_revision: u64,
    pub applied_revision: u64,
    pub status: ProposalApplyStatus,
    pub rejection: Option<ProposalApplyRejection>,
    pub rebased: bool,
    pub mover_group: GroupId,
    pub fixed_group: Option<GroupId>,
    pub activated_edges: Vec<EdgeId>,
    /// Edges that flipped from active to inactive as part of this proposal.
    /// Populated by detach actions; empty for merge/snap activations and
    /// pose-only mutations.
    pub deactivated_edges: Vec<EdgeId>,
    pub merged_groups: Vec<GroupMergeUpdate>,
    pub final_group_poses: Vec<GroupPoseUpdate>,
}

/// Ordered authoritative update unit emitted by `PlayableState`.
#[derive(Clone, Debug, PartialEq)]
pub struct PlayableUpdateBatch {
    pub revision_before: u64,
    pub revision_after: u64,
    pub delta: PlayableDelta,
    pub proposal: AppliedProposal,
}
