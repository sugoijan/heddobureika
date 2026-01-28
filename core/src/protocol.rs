use rkyv::{Archive, Deserialize, Serialize};

use crate::snapshot::{GameRules, GameSnapshot, PuzzleInfo, PuzzleStateSnapshot};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RoomPersistence {
    Durable,
    BestEffort,
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
        rules: Option<GameRules>,
        state: Option<PuzzleStateSnapshot>,
    },
    Select { piece_id: u32 },
    Move { anchor_id: u32, pos: (f32, f32) },
    Transform { anchor_id: u32, pos: (f32, f32), rot_deg: f32 },
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
    State { seq: u64, snapshot: GameSnapshot },
    Update {
        seq: u64,
        update: RoomUpdate,
        source: Option<u64>,
    },
    Pong { nonce: Option<u64> },
    Error { code: String, message: String },
}
