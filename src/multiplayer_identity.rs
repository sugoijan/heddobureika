use std::cell::RefCell;

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use js_sys::{Date, JSON};
use p256::ecdsa::{signature::Signer, Signature, SigningKey};
use p256::SecretKey;
use rand_core::{OsRng, RngCore};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Event, IdbDatabase, IdbFactory, IdbOpenDbRequest, IdbRequest, IdbTransactionMode};
use heddobureika_core::ClientId;

const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
const AUTH_CONTEXT: &str = "heddobureika-auth-v1";
const NONCE_BYTES: usize = 12;

const IDB_NAME: &str = "heddobureika";
const IDB_VERSION: u32 = 1;
const IDB_STORE: &str = "identity";
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
    let db = open_db().await.map_err(js_err)?;
    idb_delete_string(&db, IDB_KEY).await.map_err(js_err)?;
    Ok(())
}

async fn load_or_create_identity() -> Result<ClientIdentity, String> {
    if let Some(identity) = IDENTITY_CACHE.with(|cache| cache.borrow().clone()) {
        return Ok(identity);
    }

    let db = open_db().await.map_err(js_err)?;
    if let Ok(Some(record)) = load_identity_record(&db).await {
        if let Ok(identity) = import_identity(&record) {
            IDENTITY_CACHE.with(|cache| cache.borrow_mut().replace(identity.clone()));
            return Ok(identity);
        }
        let _ = idb_delete_string(&db, IDB_KEY).await;
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

    let db = open_db().await.map_err(js_err)?;
    store_identity_record(&db, &record).await.map_err(js_err)?;

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

async fn open_db() -> Result<IdbDatabase, JsValue> {
    let factory = idb_factory()?;
    let request = factory.open_with_u32(IDB_NAME, IDB_VERSION)?;
    attach_upgrade_handler(&request);
    let db_value = wasm_bindgen_futures::JsFuture::from(request_to_promise(request.into())).await?;
    db_value.dyn_into::<IdbDatabase>()
}

fn idb_factory() -> Result<IdbFactory, JsValue> {
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("missing window"))?;
    window
        .indexed_db()?
        .ok_or_else(|| JsValue::from_str("indexeddb unavailable"))
}

fn attach_upgrade_handler(request: &IdbOpenDbRequest) {
    let request_for_cb = request.clone();
    let on_upgrade = wasm_bindgen::closure::Closure::once(move |_event: Event| {
        let Ok(result) = request_for_cb.result() else {
            return;
        };
        let Ok(db) = result.dyn_into::<IdbDatabase>() else {
            return;
        };
        let _ = db.create_object_store(IDB_STORE);
    });
    request.set_onupgradeneeded(Some(on_upgrade.as_ref().unchecked_ref()));
    on_upgrade.forget();
}

async fn load_identity_record(db: &IdbDatabase) -> Result<Option<StoredIdentity>, JsValue> {
    let tx = db.transaction_with_str_and_mode(IDB_STORE, IdbTransactionMode::Readonly)?;
    let store = tx.object_store(IDB_STORE)?;
    let request = store.get(&JsValue::from_str(IDB_KEY))?;
    let value = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    if value.is_undefined() || value.is_null() {
        return Ok(None);
    }
    let raw = value.as_string().unwrap_or_default();
    if raw.trim().is_empty() {
        return Ok(None);
    }
    Ok(serde_json::from_str(&raw).ok())
}

async fn store_identity_record(db: &IdbDatabase, record: &StoredIdentity) -> Result<(), JsValue> {
    let raw = serde_json::to_string(record)
        .map_err(|err| JsValue::from_str(&err.to_string()))?;
    idb_put_string(db, IDB_KEY, &raw).await
}

async fn idb_put_string(db: &IdbDatabase, key: &str, value: &str) -> Result<(), JsValue> {
    let tx = db.transaction_with_str_and_mode(IDB_STORE, IdbTransactionMode::Readwrite)?;
    let store = tx.object_store(IDB_STORE)?;
    let request = store.put_with_key(&JsValue::from_str(value), &JsValue::from_str(key))?;
    let _ = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    Ok(())
}

async fn idb_delete_string(db: &IdbDatabase, key: &str) -> Result<(), JsValue> {
    let tx = db.transaction_with_str_and_mode(IDB_STORE, IdbTransactionMode::Readwrite)?;
    let store = tx.object_store(IDB_STORE)?;
    let request = store.delete(&JsValue::from_str(key))?;
    let _ = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    Ok(())
}

fn request_to_promise(request: IdbRequest) -> js_sys::Promise {
    js_sys::Promise::new(&mut |resolve, reject| {
        let success_request = request.clone();
        let on_success = wasm_bindgen::closure::Closure::once(move |_event: Event| {
            let result = success_request.result().unwrap_or(JsValue::UNDEFINED);
            let _ = resolve.call1(&JsValue::NULL, &result);
        });
        let on_error = wasm_bindgen::closure::Closure::once(move |_event: Event| {
            let _ = reject.call1(&JsValue::NULL, &JsValue::from_str("indexeddb request failed"));
        });
        request.set_onsuccess(Some(on_success.as_ref().unchecked_ref()));
        request.set_onerror(Some(on_error.as_ref().unchecked_ref()));
        on_success.forget();
        on_error.forget();
    })
}

fn js_err(error: JsValue) -> String {
    if let Some(value) = error.as_string() {
        return value;
    }
    if let Ok(json) = JSON::stringify(&error) {
        if let Some(value) = json.as_string() {
            return value;
        }
    }
    "js error".to_string()
}
