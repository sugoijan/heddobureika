use std::fmt;

pub const ROOM_ID_LEN: usize = 10;
pub const ROOM_ID_ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

pub fn is_valid_room_id(value: &str) -> bool {
    if value.len() != ROOM_ID_LEN {
        return false;
    }
    value.chars().all(|ch| ROOM_ID_ALPHABET.contains(ch))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RoomId(String);

impl RoomId {
    pub fn parse(value: &str) -> Result<Self, RoomIdError> {
        if value.len() != ROOM_ID_LEN {
            return Err(RoomIdError::InvalidLength {
                expected: ROOM_ID_LEN,
                found: value.len(),
            });
        }
        for (idx, ch) in value.chars().enumerate() {
            if !ROOM_ID_ALPHABET.contains(ch) {
                return Err(RoomIdError::InvalidCharacter { ch, index: idx });
            }
        }
        Ok(Self(value.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RoomId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl std::str::FromStr for RoomId {
    type Err = RoomIdError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::parse(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RoomIdError {
    InvalidLength { expected: usize, found: usize },
    InvalidCharacter { ch: char, index: usize },
}

impl fmt::Display for RoomIdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RoomIdError::InvalidLength { expected, found } => {
                write!(f, "room id must be {expected} chars, got {found}")
            }
            RoomIdError::InvalidCharacter { ch, index } => {
                write!(f, "invalid character '{ch}' at position {index}")
            }
        }
    }
}

impl std::error::Error for RoomIdError {}
