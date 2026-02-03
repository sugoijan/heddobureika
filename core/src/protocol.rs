use std::fmt;

use rkyv::{Archive, Deserialize, Serialize};

use crate::snapshot::{GameRules, GameSnapshot, PuzzleImageRef, PuzzleInfo, PuzzleStateSnapshot};

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
    pub pieces: Option<u32>,
    pub seed: Option<u32>,
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

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum RoomUpdate {
    Ownership {
        anchor_id: u32,
        owner: Option<ClientId>,
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
    },
    Scramble {
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
    AssetRequest { hash: String },
    Select { piece_id: u32 },
    Move {
        anchor_id: u32,
        pos: (f32, f32),
        client_seq: u64,
    },
    Transform {
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: f32,
        client_seq: u64,
    },
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
        client_id: Option<ClientId>,
    },
    AdminAck { room_id: String, persistence: RoomPersistence },
    UploadAck { hash: String },
    NeedInit,
    Warning { minutes_idle: u32 },
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
    State { seq: u64, snapshot: GameSnapshot },
    Update {
        seq: u64,
        update: RoomUpdate,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    },
    Pong { nonce: Option<u64> },
    Error { code: String, message: String },
}
