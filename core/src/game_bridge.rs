//! Re-exports the `heddobureika_game` core types onto `heddobureika_core`
//! and defines the small error enum used by the worker's action helpers
//! (`apply_bridge_live_move`, `apply_bridge_live_transform`,
//! `apply_bridge_finalize`, `apply_bridge_flip`).
//!
//! All the rectangular-array bridge (`playable_from_grid_arrays`,
//! `core_snapshot_from_playable_game_snapshot`, `GridProjection`,
//! `GridArrayError`) lived here historically. With drag/snap, scramble,
//! multiplayer prediction, and the dev panel all migrated onto
//! `PlayableState` directly, that bridge no longer has any callers and
//! has been removed.

pub use heddobureika_game::{
    ActionId, AngleDeg, EdgeId, FlipState, GridTopology, GroupId, LengthMm, LogicalState,
    MergePolicy, PieceId, PlayRules, PlayableAction, PlayableDelta, PlayableState,
    PlayableUpdateBatch, Pose2, Position2, ProjectionScratch, PuzzleTopology,
    RestoredPlayableState, RestrictedPlayableAction, VisualState,
};

use crate::snapshot::PlayableGameSnapshotError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GameBridgeError {
    InvalidPieceId { piece_id: u32 },
    InvalidActionPose,
    PlayableGameSnapshot(PlayableGameSnapshotError),
}

impl From<PlayableGameSnapshotError> for GameBridgeError {
    fn from(value: PlayableGameSnapshotError) -> Self {
        Self::PlayableGameSnapshot(value)
    }
}
