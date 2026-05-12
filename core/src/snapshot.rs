use rkyv::{Archive, Deserialize, Serialize};

use crate::game::{
    FRAME_SNAP_DEFAULT, IMAGE_MAX_DIMENSION_DEFAULT, ROTATION_SNAP_TOLERANCE_DEFAULT_DEG,
    SNAP_DISTANCE_RATIO_DEFAULT, WORKSPACE_PADDING_RATIO_DEFAULT,
};

pub const PLAYABLE_GAME_SNAPSHOT_VERSION: u32 = 4;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Archive, Serialize, Deserialize)]
pub enum PuzzleImageRef {
    BuiltIn { slug: String },
    Private { hash: String },
}

pub fn validate_image_ref(image_ref: &PuzzleImageRef) -> Result<(), String> {
    match image_ref {
        PuzzleImageRef::BuiltIn { slug } => {
            if slug.trim().is_empty() {
                return Err("missing puzzle slug".to_string());
            }
            Ok(())
        }
        PuzzleImageRef::Private { hash } => {
            if hash.trim().is_empty() {
                return Err("missing image hash".to_string());
            }
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct GameRules {
    pub workspace_padding_ratio: f32,
    pub image_max_dimension: u32,
    pub snap_distance_ratio: f32,
    pub rotation_snap_tolerance_deg: f32,
    pub frame_snap_ratio: f32,
    pub rotation_enabled: bool,
}

impl Default for GameRules {
    fn default() -> Self {
        Self {
            workspace_padding_ratio: WORKSPACE_PADDING_RATIO_DEFAULT,
            image_max_dimension: IMAGE_MAX_DIMENSION_DEFAULT,
            snap_distance_ratio: SNAP_DISTANCE_RATIO_DEFAULT,
            rotation_snap_tolerance_deg: ROTATION_SNAP_TOLERANCE_DEFAULT_DEG,
            frame_snap_ratio: FRAME_SNAP_DEFAULT,
            rotation_enabled: true,
        }
    }
}

impl GameRules {
    pub fn to_play_rules(self) -> Result<heddobureika_game::PlayRules, PlayableGameSnapshotError> {
        if !self.snap_distance_ratio.is_finite() || !self.frame_snap_ratio.is_finite() {
            return Err(PlayableGameSnapshotError::InvalidRules);
        }
        let rotation_snap_tolerance =
            heddobureika_game::AngleDeg::try_new(self.rotation_snap_tolerance_deg)
                .ok_or(PlayableGameSnapshotError::InvalidRules)?;

        Ok(heddobureika_game::PlayRules {
            snap_distance_ratio: self.snap_distance_ratio,
            rotation_snap_tolerance,
            frame_snap_ratio: self.frame_snap_ratio,
            rotation_enabled: self.rotation_enabled,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Archive, Serialize, Deserialize)]
pub struct PuzzleInfo {
    pub label: String,
    pub image_ref: PuzzleImageRef,
    pub rows: u32,
    pub cols: u32,
    pub shape_seed: u32,
    pub image_width: u32,
    pub image_height: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub enum PlayableTopologySnapshot {
    Unknown { piece_count: u32, edge_count: u32 },
    Grid { cols: u32, rows: u32 },
    TriangularTessellation { cols: u32, rows: u32 },
}

impl From<heddobureika_game::TopologySpec> for PlayableTopologySnapshot {
    fn from(value: heddobureika_game::TopologySpec) -> Self {
        match value {
            heddobureika_game::TopologySpec::Unknown {
                piece_count,
                edge_count,
            } => Self::Unknown {
                piece_count,
                edge_count,
            },
            heddobureika_game::TopologySpec::Grid { cols, rows } => Self::Grid { cols, rows },
            heddobureika_game::TopologySpec::TriangularTessellation { cols, rows } => {
                Self::TriangularTessellation { cols, rows }
            }
        }
    }
}

impl From<PlayableTopologySnapshot> for heddobureika_game::TopologySpec {
    fn from(value: PlayableTopologySnapshot) -> Self {
        match value {
            PlayableTopologySnapshot::Unknown {
                piece_count,
                edge_count,
            } => Self::Unknown {
                piece_count,
                edge_count,
            },
            PlayableTopologySnapshot::Grid { cols, rows } => Self::Grid { cols, rows },
            PlayableTopologySnapshot::TriangularTessellation { cols, rows } => {
                Self::TriangularTessellation { cols, rows }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayablePositionSnapshot {
    pub x_mm: f32,
    pub y_mm: f32,
}

impl PlayablePositionSnapshot {
    pub fn from_position(value: heddobureika_game::Position2) -> Self {
        Self {
            x_mm: value.x_mm(),
            y_mm: value.y_mm(),
        }
    }

    pub fn to_position(self) -> Result<heddobureika_game::Position2, PlayableGameSnapshotError> {
        heddobureika_game::Position2::try_from_mm(self.x_mm, self.y_mm)
            .ok_or(PlayableGameSnapshotError::InvalidPose)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayablePoseSnapshot {
    pub x_mm: f32,
    pub y_mm: f32,
    pub rotation_deg: f32,
}

impl PlayablePoseSnapshot {
    pub fn from_pose(value: heddobureika_game::Pose2) -> Self {
        Self {
            x_mm: value.x_mm(),
            y_mm: value.y_mm(),
            rotation_deg: value.rotation_degrees(),
        }
    }

    pub fn to_pose(self) -> Result<heddobureika_game::Pose2, PlayableGameSnapshotError> {
        heddobureika_game::Pose2::try_from_mm_degrees(self.x_mm, self.y_mm, self.rotation_deg)
            .ok_or(PlayableGameSnapshotError::InvalidPose)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayableRulesSnapshot {
    pub snap_distance_ratio: f32,
    pub rotation_snap_tolerance_deg: f32,
    pub frame_snap_ratio: f32,
    pub rotation_enabled: bool,
}

impl PlayableRulesSnapshot {
    pub fn from_rules(value: heddobureika_game::PlayRules) -> Self {
        Self {
            snap_distance_ratio: value.snap_distance_ratio,
            rotation_snap_tolerance_deg: value.rotation_snap_tolerance.as_degrees_f32(),
            frame_snap_ratio: value.frame_snap_ratio,
            rotation_enabled: value.rotation_enabled,
        }
    }

    pub fn to_rules(self) -> Result<heddobureika_game::PlayRules, PlayableGameSnapshotError> {
        if !self.snap_distance_ratio.is_finite() || !self.frame_snap_ratio.is_finite() {
            return Err(PlayableGameSnapshotError::InvalidRules);
        }
        let rotation_snap_tolerance =
            heddobureika_game::AngleDeg::try_new(self.rotation_snap_tolerance_deg)
                .ok_or(PlayableGameSnapshotError::InvalidRules)?;
        Ok(heddobureika_game::PlayRules {
            snap_distance_ratio: self.snap_distance_ratio,
            rotation_snap_tolerance,
            frame_snap_ratio: self.frame_snap_ratio,
            rotation_enabled: self.rotation_enabled,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayableGameStateSnapshot {
    pub revision: u64,
    pub topology: PlayableTopologySnapshot,
    pub topology_piece_count: u32,
    pub topology_edge_count: u32,
    pub rules: PlayableRulesSnapshot,
    pub edge_active: Vec<bool>,
    pub piece_group: Vec<u32>,
    pub piece_local_pose: Vec<PlayablePoseSnapshot>,
    pub group_pose: Vec<PlayablePoseSnapshot>,
    pub group_flip: Vec<bool>,
    pub z_order: Vec<u32>,
    pub focused_piece: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlayableGameSnapshotError {
    InvalidPose,
    InvalidRules,
    Game(heddobureika_game::PlayableSnapshotError),
}

impl From<heddobureika_game::PlayableSnapshotError> for PlayableGameSnapshotError {
    fn from(value: heddobureika_game::PlayableSnapshotError) -> Self {
        Self::Game(value)
    }
}

impl PlayableGameStateSnapshot {
    pub fn from_playable<T: heddobureika_game::PuzzleTopology>(
        playable: &heddobureika_game::PlayableState<T>,
        focused_piece: Option<u32>,
    ) -> Self {
        let snapshot = heddobureika_game::PlayableSnapshot::from_playable(
            playable,
            focused_piece.map(heddobureika_game::PieceId),
        );
        Self::from_game_snapshot(&snapshot)
    }

    pub fn from_game_snapshot(value: &heddobureika_game::PlayableSnapshot) -> Self {
        Self {
            revision: value.revision,
            topology: value.topology.into(),
            topology_piece_count: value.topology_piece_count,
            topology_edge_count: value.topology_edge_count,
            rules: PlayableRulesSnapshot::from_rules(value.rules),
            edge_active: value.edge_active.clone(),
            piece_group: value
                .piece_group
                .iter()
                .map(|group| group.as_u32())
                .collect(),
            piece_local_pose: value
                .piece_local_pose
                .iter()
                .copied()
                .map(PlayablePoseSnapshot::from_pose)
                .collect(),
            group_pose: value
                .group_pose
                .iter()
                .copied()
                .map(PlayablePoseSnapshot::from_pose)
                .collect(),
            group_flip: value
                .group_flip
                .iter()
                .map(|flip| *flip == heddobureika_game::FlipState::Flipped)
                .collect(),
            z_order: value.z_order.iter().map(|group| group.as_u32()).collect(),
            focused_piece: value.focused_piece.map(|piece| piece.as_u32()),
        }
    }

    pub fn to_game_snapshot(
        &self,
    ) -> Result<heddobureika_game::PlayableSnapshot, PlayableGameSnapshotError> {
        Ok(heddobureika_game::PlayableSnapshot {
            revision: self.revision,
            topology: self.topology.into(),
            topology_piece_count: self.topology_piece_count,
            topology_edge_count: self.topology_edge_count,
            rules: self.rules.to_rules()?,
            edge_active: self.edge_active.clone(),
            piece_group: self
                .piece_group
                .iter()
                .copied()
                .map(heddobureika_game::GroupId)
                .collect(),
            piece_local_pose: self
                .piece_local_pose
                .iter()
                .copied()
                .map(PlayablePoseSnapshot::to_pose)
                .collect::<Result<Vec<_>, _>>()?,
            group_pose: self
                .group_pose
                .iter()
                .copied()
                .map(PlayablePoseSnapshot::to_pose)
                .collect::<Result<Vec<_>, _>>()?,
            group_flip: self
                .group_flip
                .iter()
                .map(|flipped| {
                    if *flipped {
                        heddobureika_game::FlipState::Flipped
                    } else {
                        heddobureika_game::FlipState::Normal
                    }
                })
                .collect(),
            z_order: self
                .z_order
                .iter()
                .copied()
                .map(heddobureika_game::GroupId)
                .collect(),
            focused_piece: self.focused_piece.map(heddobureika_game::PieceId),
        })
    }

    pub fn restore_from_spec(
        &self,
    ) -> Result<heddobureika_game::RestoredPlayableState, PlayableGameSnapshotError> {
        Ok(self.to_game_snapshot()?.restore_from_spec()?)
    }
}

#[derive(Debug, Clone, PartialEq, Archive, Serialize, Deserialize)]
pub struct PlayableGameSnapshot {
    pub version: u32,
    pub seq: u64,
    pub puzzle: PuzzleInfo,
    pub rules: GameRules,
    pub scramble_nonce: u32,
    pub state: PlayableGameStateSnapshot,
}

impl PlayableGameSnapshot {
    pub fn from_playable<T: heddobureika_game::PuzzleTopology>(
        puzzle: PuzzleInfo,
        rules: GameRules,
        scramble_nonce: u32,
        playable: &heddobureika_game::PlayableState<T>,
        focused_piece: Option<u32>,
    ) -> Self {
        let state = PlayableGameStateSnapshot::from_playable(playable, focused_piece);
        Self {
            version: PLAYABLE_GAME_SNAPSHOT_VERSION,
            seq: state.revision,
            puzzle,
            rules,
            scramble_nonce,
            state,
        }
    }

    pub fn restore_playable_from_spec(
        &self,
    ) -> Result<heddobureika_game::RestoredPlayableState, PlayableGameSnapshotError> {
        self.state.restore_from_spec()
    }

    pub fn apply_action_with_snap(
        &mut self,
        action: heddobureika_game::PlayableAction,
        action_id: Option<heddobureika_game::ActionId>,
        policy: heddobureika_game::MergePolicy,
    ) -> Result<heddobureika_game::PlayableUpdateBatch, PlayableGameSnapshotError> {
        match self.restore_playable_from_spec()? {
            heddobureika_game::RestoredPlayableState::Grid(mut playable) => {
                let batch = playable.apply_action_with_snap(action, action_id, policy);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
            heddobureika_game::RestoredPlayableState::TriangularTessellation(mut playable) => {
                let batch = playable.apply_action_with_snap(action, action_id, policy);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
        }
    }

    pub fn apply_action_only(
        &mut self,
        action: heddobureika_game::PlayableAction,
        action_id: Option<heddobureika_game::ActionId>,
    ) -> Result<heddobureika_game::PlayableUpdateBatch, PlayableGameSnapshotError> {
        match self.restore_playable_from_spec()? {
            heddobureika_game::RestoredPlayableState::Grid(mut playable) => {
                let batch = playable.apply_action_only(action, action_id);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
            heddobureika_game::RestoredPlayableState::TriangularTessellation(mut playable) => {
                let batch = playable.apply_action_only(action, action_id);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
        }
    }

    pub fn apply_restricted_action(
        &mut self,
        action: heddobureika_game::RestrictedPlayableAction,
        action_id: Option<heddobureika_game::ActionId>,
    ) -> Result<heddobureika_game::PlayableUpdateBatch, PlayableGameSnapshotError> {
        match self.restore_playable_from_spec()? {
            heddobureika_game::RestoredPlayableState::Grid(mut playable) => {
                let batch = playable.apply_restricted_action_batch(action, action_id);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
            heddobureika_game::RestoredPlayableState::TriangularTessellation(mut playable) => {
                let batch = playable.apply_restricted_action_batch(action, action_id);
                self.replace_state_from_playable(&playable);
                Ok(batch)
            }
        }
    }

    fn replace_state_from_playable<T: heddobureika_game::PuzzleTopology>(
        &mut self,
        playable: &heddobureika_game::PlayableState<T>,
    ) {
        let focused_piece = self.state.focused_piece;
        self.state = PlayableGameStateSnapshot::from_playable(playable, focused_piece);
        self.seq = self.state.revision;
    }
}
