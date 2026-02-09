//! Placeholder snapshot DTOs for playable canonical state.
//!
//! Compatibility adapters to the legacy core snapshot format are intentionally
//! not included in this first skeleton iteration.

use crate::ids::{GroupId, PieceId};
use crate::playable::{FlipState, Pose2};

pub const PLAYABLE_SNAPSHOT_VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq)]
pub struct PlayableSnapshot {
    pub revision: u64,
    pub edge_active: Vec<bool>,
    pub piece_group: Vec<GroupId>,
    pub group_pose: Vec<Pose2>,
    pub group_flip: Vec<FlipState>,
    pub z_order: Vec<GroupId>,
    pub focused_piece: Option<PieceId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SnapshotEnvelope {
    pub version: u32,
    pub state: PlayableSnapshot,
}
