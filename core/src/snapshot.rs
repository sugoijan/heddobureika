use rkyv::{Archive, Deserialize, Serialize};

use crate::game::{
    FRAME_SNAP_DEFAULT, IMAGE_MAX_DIMENSION_DEFAULT, ROTATION_SNAP_TOLERANCE_DEFAULT_DEG,
    SNAP_DISTANCE_RATIO_DEFAULT, WORKSPACE_PADDING_RATIO_DEFAULT,
};

pub const GAME_SNAPSHOT_VERSION: u32 = 3;

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

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct PuzzleInfo {
    pub label: String,
    pub image_ref: PuzzleImageRef,
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

pub type CoreSnapshot = PuzzleStateSnapshot;

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct GameSnapshot {
    pub version: u32,
    pub seq: u64,
    pub rules: GameRules,
    pub puzzle: PuzzleInfo,
    pub state: PuzzleStateSnapshot,
}
