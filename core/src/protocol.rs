use rkyv::{Archive, Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[repr(u8)]
pub enum RoomPersistence {
    Durable,
    BestEffort,
}

#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
pub struct RoomConfig {
    pub persistence: RoomPersistence,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum AdminMsg {
    Create { persistence: RoomPersistence },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ClientMsg {
    Command { action: String, payload: Vec<u8> },
    Ping { nonce: Option<u64> },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub enum ServerMsg {
    Welcome { room_id: String, persistence: RoomPersistence },
    AdminAck { room_id: String, persistence: RoomPersistence },
    Warning { minutes_idle: u32 },
    State { payload: Vec<u8> },
    Event { event: String, payload: Vec<u8> },
    Error { code: String, message: String },
}
