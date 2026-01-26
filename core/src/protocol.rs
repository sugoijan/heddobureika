use rkyv::{Archive, Deserialize, Serialize};

use crate::game::{
    FRAME_SNAP_DEFAULT, IMAGE_MAX_DIMENSION_DEFAULT, ROTATION_SNAP_TOLERANCE_DEFAULT_DEG,
    SNAP_DISTANCE_RATIO_DEFAULT, WORKSPACE_SCALE_DEFAULT,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RoomPersistence {
    Durable,
    BestEffort,
}

#[derive(Debug, Clone, Copy, PartialEq, Archive, Serialize, Deserialize)]
pub struct RoomRules {
    pub workspace_scale: f32,
    pub image_max_dimension: u32,
    pub snap_distance_ratio: f32,
    pub rotation_snap_tolerance_deg: f32,
    pub frame_snap_ratio: f32,
    pub rotation_enabled: bool,
}

impl Default for RoomRules {
    fn default() -> Self {
        Self {
            workspace_scale: WORKSPACE_SCALE_DEFAULT,
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
pub struct RoomSnapshot {
    pub version: u32,
    pub seq: u64,
    pub rules: RoomRules,
    pub puzzle: PuzzleInfo,
    pub state: PuzzleStateSnapshot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum OwnershipReason {
    Granted,
    Released,
    Timeout,
    AutoRelease,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum RoomUpdate {
    Ownership {
        anchor_id: u32,
        owner: Option<u64>,
        reason: OwnershipReason,
    },
    GroupTransform {
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: f32,
    },
    Flip {
        piece_id: u32,
        flipped: bool,
    },
    GroupOrder { order: Vec<u32> },
    Connections { connections: Vec<[bool; 4]> },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum AdminMsg {
    Create {
        persistence: RoomPersistence,
        puzzle: String,
        pieces: Option<u32>,
        seed: Option<u32>,
    },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ClientMsg {
    Init {
        puzzle: PuzzleInfo,
        rules: Option<RoomRules>,
        state: Option<PuzzleStateSnapshot>,
    },
    Select { piece_id: u32 },
    Move { anchor_id: u32, pos: (f32, f32) },
    Rotate { anchor_id: u32, rot_deg: f32 },
    Place { anchor_id: u32, pos: (f32, f32), rot_deg: f32 },
    Flip { piece_id: u32, flipped: bool },
    Release { anchor_id: u32 },
    Ping { nonce: Option<u64> },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ServerMsg {
    Welcome {
        room_id: String,
        persistence: RoomPersistence,
        initialized: bool,
        client_id: Option<u64>,
    },
    AdminAck { room_id: String, persistence: RoomPersistence },
    NeedInit,
    Warning { minutes_idle: u32 },
    State { seq: u64, snapshot: RoomSnapshot },
    Update {
        seq: u64,
        update: RoomUpdate,
        source: Option<u64>,
    },
    Pong { nonce: Option<u64> },
    Error { code: String, message: String },
}
