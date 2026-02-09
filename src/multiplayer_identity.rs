use std::cell::RefCell;

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use js_sys::Date;
use p256::ecdsa::{signature::Signer, Signature, SigningKey};
use p256::elliptic_curve::rand_core::{OsRng, RngCore};
use p256::SecretKey;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use wasm_bindgen::JsValue;
use web_sys::IdbDatabase;
use heddobureika_core::ClientId;

use crate::idb;

const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
const AUTH_CONTEXT: &str = "heddobureika-auth-v1";
const NONCE_BYTES: usize = 12;

const IDB_KEY: &str = "client-keypair-v1";

#[derive(Clone)]
pub(crate) struct ClientIdentity {
    pub(crate) client_id: ClientId,
    pub(crate) public_key_sec1: Vec<u8>,
    pub(crate) private_key: Vec<u8>,
}

#[derive(Serialize)]
struct AuthPayload {
    v: u8,
    client_id: String,
    ts: i64,
    nonce: String,
    pubkey: String,
    sig: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    admin_token: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct StoredIdentity {
    v: u8,
    private_key: String,
}

thread_local! {
    static IDENTITY_CACHE: RefCell<Option<ClientIdentity>> = RefCell::new(None);
}

pub(crate) async fn build_auth_protocol(
    room_id: &str,
    admin_token: Option<&str>,
) -> Result<String, String> {
    let identity = load_or_create_identity().await?;
    let ts = now_ms();
    let nonce_bytes = random_bytes(NONCE_BYTES);
    let nonce = URL_SAFE_NO_PAD.encode(&nonce_bytes);
    let message = auth_message(room_id, ts, &nonce);
    let sig_bytes = sign_message(&identity.private_key, &message)?;
    let sig = URL_SAFE_NO_PAD.encode(sig_bytes);
    let pubkey = URL_SAFE_NO_PAD.encode(&identity.public_key_sec1);
    let payload = AuthPayload {
        v: 1,
        client_id: identity.client_id.to_string(),
        ts,
        nonce,
        pubkey,
        sig,
        admin_token: admin_token
            .map(str::trim)
            .filter(|token| !token.is_empty())
            .map(|token| token.to_string()),
    };
    let payload_bytes = serde_json::to_vec(&payload).map_err(|err| err.to_string())?;
    let encoded = URL_SAFE_NO_PAD.encode(payload_bytes);
    Ok(format!("{AUTH_PROTOCOL_PREFIX}{encoded}"))
}

pub(crate) async fn reset_identity() -> Result<(), String> {
    IDENTITY_CACHE.with(|cache| cache.borrow_mut().take());
    let db = idb::open_db().await.map_err(idb::js_err)?;
    idb::idb_delete_key(&db, idb::IDB_STORE_IDENTITY, IDB_KEY)
        .await
        .map_err(idb::js_err)?;
    Ok(())
}

async fn load_or_create_identity() -> Result<ClientIdentity, String> {
    if let Some(identity) = IDENTITY_CACHE.with(|cache| cache.borrow().clone()) {
        return Ok(identity);
    }

    let db = idb::open_db().await.map_err(idb::js_err)?;
    if let Ok(Some(record)) = load_identity_record(&db).await {
        if let Ok(identity) = import_identity(&record) {
            IDENTITY_CACHE.with(|cache| cache.borrow_mut().replace(identity.clone()));
            return Ok(identity);
        }
        let _ = idb::idb_delete_key(&db, idb::IDB_STORE_IDENTITY, IDB_KEY).await;
    }

    let identity = generate_identity().await?;
    IDENTITY_CACHE.with(|cache| cache.borrow_mut().replace(identity.clone()));
    Ok(identity)
}

async fn generate_identity() -> Result<ClientIdentity, String> {
    let signing_key = SigningKey::random(&mut OsRng);
    let verifying_key = signing_key.verifying_key();
    let public_key_sec1 = verifying_key.to_encoded_point(false).as_bytes().to_vec();
    let client_id = derive_client_id(&public_key_sec1);

    let private_key_bytes = signing_key.to_bytes().to_vec();
    let record = StoredIdentity {
        v: 1,
        private_key: URL_SAFE_NO_PAD.encode(&private_key_bytes),
    };

    let db = idb::open_db().await.map_err(idb::js_err)?;
    store_identity_record(&db, &record).await.map_err(idb::js_err)?;

    Ok(ClientIdentity {
        client_id,
        public_key_sec1,
        private_key: private_key_bytes,
    })
}

fn import_identity(record: &StoredIdentity) -> Result<ClientIdentity, String> {
    if record.v != 1 {
        return Err("unsupported identity version".to_string());
    }
    let private_key_bytes = URL_SAFE_NO_PAD
        .decode(record.private_key.as_bytes())
        .map_err(|_| "invalid private key".to_string())?;
    let signing_key = signing_key_from_bytes(&private_key_bytes)?;
    let verifying_key = signing_key.verifying_key();
    let public_key_sec1 = verifying_key.to_encoded_point(false).as_bytes().to_vec();
    let client_id = derive_client_id(&public_key_sec1);
    Ok(ClientIdentity {
        client_id,
        public_key_sec1,
        private_key: private_key_bytes,
    })
}

fn signing_key_from_bytes(private_key_bytes: &[u8]) -> Result<SigningKey, String> {
    let secret = SecretKey::from_slice(private_key_bytes)
        .map_err(|_| "invalid private key".to_string())?;
    Ok(SigningKey::from(secret))
}

fn sign_message(private_key_bytes: &[u8], message: &[u8]) -> Result<Vec<u8>, String> {
    let signing_key = signing_key_from_bytes(private_key_bytes)?;
    let signature: Signature = signing_key.sign(message);
    Ok(signature.to_bytes().to_vec())
}

fn derive_client_id(public_key_sec1: &[u8]) -> ClientId {
    let digest = Sha256::digest(public_key_sec1);
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&digest[..8]);
    ClientId::from(u64::from_be_bytes(bytes))
}

fn random_bytes(len: usize) -> Vec<u8> {
    let mut bytes = vec![0u8; len];
    OsRng.fill_bytes(&mut bytes);
    bytes
}

fn now_ms() -> i64 {
    Date::now() as i64
}

fn auth_message(room_id: &str, ts: i64, nonce: &str) -> Vec<u8> {
    format!("{room_id}\n{ts}\n{nonce}\n{AUTH_CONTEXT}").into_bytes()
}

async fn load_identity_record(db: &IdbDatabase) -> Result<Option<StoredIdentity>, JsValue> {
    let Some(raw) = idb::idb_get_string(db, idb::IDB_STORE_IDENTITY, IDB_KEY).await? else {
        return Ok(None);
    };
    Ok(serde_json::from_str(&raw).ok())
}

async fn store_identity_record(db: &IdbDatabase, record: &StoredIdentity) -> Result<(), JsValue> {
    let raw = serde_json::to_string(record)
        .map_err(|err| JsValue::from_str(&err.to_string()))?;
    idb::idb_put_string(db, idb::IDB_STORE_IDENTITY, IDB_KEY, &raw).await
}
