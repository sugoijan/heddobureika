use rkyv::{Archive, Deserialize, Serialize};

use crate::game::{
    FRAME_SNAP_DEFAULT, IMAGE_MAX_DIMENSION_DEFAULT, ROTATION_SNAP_TOLERANCE_DEFAULT_DEG,
    SNAP_DISTANCE_RATIO_DEFAULT, WORKSPACE_PADDING_RATIO_DEFAULT,
};

pub const GAME_SNAPSHOT_VERSION: u32 = 1;

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

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct PuzzleInfo {
    pub label: String,
    pub image_src: String,
    pub rows: u32,
    pub cols: u32,
    pub shape_seed: u32,
    pub image_width: u32,
    pub image_height: u32,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct PuzzleStateSnapshot {
    pub positions: Vec<(f32, f32)>,
    pub rotations: Vec<f32>,
    pub flips: Vec<bool>,
    pub connections: Vec<[bool; 4]>,
    pub group_order: Vec<u32>,
    pub scramble_nonce: u32,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct GameSnapshot {
    pub version: u32,
    pub seq: u64,
    pub rules: GameRules,
    pub puzzle: PuzzleInfo,
    pub state: PuzzleStateSnapshot,
}
