//! The WebSocket auth handshake: a signed JSON payload carried as the
//! `Sec-WebSocket-Protocol` value `heddo-auth-v1.<base64url(json)>`. Every
//! client (browser, CLI) builds it and the Worker parses it, so the wire
//! format and the client-id derivation live here.

use base64::Engine as _;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use serde::{Deserialize, Serialize};
use sha2::Digest;

use crate::protocol::ClientId;

pub const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
pub const AUTH_CONTEXT: &str = "heddobureika-auth-v1";
/// Payload format version carried in [`AuthPayload::v`].
pub const AUTH_PAYLOAD_VERSION: u8 = 1;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthPayload {
    pub v: u8,
    /// Decimal [`ClientId`], which must match the one derived from `pubkey`.
    pub client_id: String,
    /// Client wall-clock milliseconds since the Unix epoch.
    pub ts: i64,
    pub nonce: String,
    /// Base64url (no padding) SEC1 uncompressed P-256 public key.
    pub pubkey: String,
    /// Base64url (no padding) ECDSA signature over [`auth_message`].
    pub sig: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub admin_token: Option<String>,
}

impl AuthPayload {
    /// Assembles a version-1 payload from the raw key and signature bytes.
    pub fn new(
        ts: i64,
        nonce: String,
        public_key_sec1: &[u8],
        signature: &[u8],
        admin_token: Option<String>,
    ) -> Self {
        Self {
            v: AUTH_PAYLOAD_VERSION,
            client_id: client_id_from_public_key(public_key_sec1).to_string(),
            ts,
            nonce,
            pubkey: URL_SAFE_NO_PAD.encode(public_key_sec1),
            sig: URL_SAFE_NO_PAD.encode(signature),
            admin_token,
        }
    }
}

/// The bytes a client signs: room id, timestamp and nonce bound to a fixed
/// context string so the signature cannot be replayed elsewhere.
pub fn auth_message(room_id: &str, ts: i64, nonce: &str) -> Vec<u8> {
    format!("{room_id}\n{ts}\n{nonce}\n{AUTH_CONTEXT}").into_bytes()
}

/// Client id derived from a SEC1 public key: the first 8 bytes of its SHA-256
/// digest, big endian.
pub fn client_id_from_public_key(public_key_sec1: &[u8]) -> ClientId {
    let digest = sha2::Sha256::digest(public_key_sec1);
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&digest[..8]);
    ClientId::from(u64::from_be_bytes(bytes))
}

/// Encodes `payload` as the subprotocol value sent during the handshake.
pub fn encode_auth_protocol(payload: &AuthPayload) -> Result<String, serde_json::Error> {
    let payload_bytes = serde_json::to_vec(payload)?;
    Ok(format!(
        "{AUTH_PROTOCOL_PREFIX}{}",
        URL_SAFE_NO_PAD.encode(payload_bytes)
    ))
}

/// Parses a subprotocol value produced by [`encode_auth_protocol`]. Returns
/// `None` for anything that is not a well-formed payload of the current
/// version; signature and freshness checks are the caller's job.
pub fn decode_auth_protocol(protocol: &str) -> Option<AuthPayload> {
    let payload_b64 = protocol.strip_prefix(AUTH_PROTOCOL_PREFIX)?;
    let payload_bytes = URL_SAFE_NO_PAD.decode(payload_b64).ok()?;
    let payload: AuthPayload = serde_json::from_slice(&payload_bytes).ok()?;
    (payload.v == AUTH_PAYLOAD_VERSION).then_some(payload)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn protocol_round_trips() {
        let payload = AuthPayload::new(1234, "n0nce".into(), &[4, 1, 2, 3], &[9, 9], None);
        let encoded = encode_auth_protocol(&payload).expect("encode");
        assert!(encoded.starts_with(AUTH_PROTOCOL_PREFIX));
        let decoded = decode_auth_protocol(&encoded).expect("decode");
        assert_eq!(decoded.client_id, payload.client_id);
        assert_eq!(decoded.ts, 1234);
        assert_eq!(decoded.nonce, "n0nce");
        assert_eq!(decoded.admin_token, None);
        assert!(decode_auth_protocol("other.abc").is_none());
    }
}
