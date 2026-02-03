use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Cursor;

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use image::codecs::avif::AvifEncoder;
use image::{ExtendedColorType, ImageEncoder, ImageReader};
use imagesize::{Compression, ImageType};
use heddobureika_core::codec::{decode, encode};
use heddobureika_core::game::{
    apply_snaps_for_group, clear_piece_connections, collect_group, compute_workspace_layout,
    groups_from_connections, piece_local_offset, rotate_vec, scramble_flips, scramble_layout,
    scramble_nonce_from_seed, scramble_rotations, scramble_seed, splitmix32, DEFAULT_TAB_DEPTH_CAP,
    FLIP_CHANCE, MAX_LINE_BEND_RATIO, PUZZLE_SEED,
};
use heddobureika_core::{
    validate_image_ref, AdminMsg, ClientId, ClientMsg, GameRules, GameSnapshot, OwnershipReason,
    PuzzleImageRef, PuzzleInfo, PuzzleSpec, PuzzleStateSnapshot, RoomPersistence, RoomUpdate,
    ServerMsg, ASSET_CHUNK_BYTES, GAME_SNAPSHOT_VERSION, PRIVATE_ASSET_MAX_BYTES,
    PRIVATE_UPLOAD_MAX_BYTES,
};
use heddobureika_core::{
    best_grid_for_count, logical_image_size, puzzle_by_slug, DEFAULT_TARGET_COUNT, FALLBACK_GRID,
};
use heddobureika_core::room_id::{is_valid_room_id, ROOM_ID_LEN};
use js_sys::Date;
use p256::ecdsa::{signature::Verifier, Signature, VerifyingKey};
use rkyv::{Archive, Deserialize, Serialize};
use serde::Deserialize as SerdeDeserialize;
use sha2::{Digest, Sha256};
use worker::*;

const DEFAULT_ROOM_PATH_PREFIX: &str = "/ws/";
const META_KEY: &str = "room_meta";
const SNAPSHOT_KEY: &str = "room_snapshot";
const ASSET_INDEX_KEY: &str = "room_assets";
const ASSET_META_PREFIX: &str = "asset_meta:";
const ASSET_CHUNK_PREFIX: &str = "asset_chunk:";
const ASSET_STORAGE_CHUNK_BYTES: usize = 256 * 1024;

const INACTIVITY_WARNING_MS: i64 = 10 * 60 * 1000;
const INACTIVITY_EXPIRE_MS: i64 = 60 * 60 * 1000;
const FULL_STATE_INTERVAL_MS: i64 = 30 * 1000;
const OWNERSHIP_TIMEOUT_MS: i64 = 30 * 1000;
const SNAPSHOT_VERSION: u32 = GAME_SNAPSHOT_VERSION;
const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
const AUTH_CONTEXT: &str = "heddobureika-auth-v1";
const AUTH_WINDOW_MS: i64 = 5 * 60 * 1000;
const DISCONNECT_GRACE_MS: i64 = 1000;

#[event(fetch)]
pub async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    let path = req.path();
    let prefix = room_path_prefix(&env);
    let room_id = match extract_room_id(&path, &prefix) {
        Some(room_id) => room_id,
        None => return Response::error("not found", 404),
    };

    if !is_valid_room_id(room_id) {
        return Response::error("invalid room id", 400);
    }

    let namespace = env.durable_object("ROOMS")?;
    let stub = namespace.get_by_name(room_id)?;

    stub.fetch_with_request(req).await
}

fn room_path_prefix(env: &Env) -> String {
    let raw = env
        .var("ROOM_PATH_PREFIX")
        .ok()
        .map(|value| value.to_string())
        .unwrap_or_else(|| DEFAULT_ROOM_PATH_PREFIX.to_string());
    normalize_room_path_prefix(&raw)
}

fn normalize_room_path_prefix(raw: &str) -> String {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return DEFAULT_ROOM_PATH_PREFIX.to_string();
    }
    let mut value = trimmed.to_string();
    if !value.starts_with('/') {
        value.insert(0, '/');
    }
    if !value.ends_with('/') {
        value.push('/');
    }
    value
}

fn extract_room_id<'a>(path: &'a str, prefix: &str) -> Option<&'a str> {
    let room = path.strip_prefix(prefix)?;
    if room.is_empty() || room.contains('/') {
        return None;
    }
    if room.len() != ROOM_ID_LEN {
        return None;
    }
    Some(room)
}

fn now_ms() -> i64 {
    Date::now() as i64
}

fn asset_meta_key(hash: &str) -> String {
    format!("{ASSET_META_PREFIX}{hash}")
}

fn asset_chunk_key(hash: &str, index: u32) -> String {
    format!("{ASSET_CHUNK_PREFIX}{hash}:{index}")
}

fn auth_protocol_from_request(req: &Request) -> Result<Option<String>> {
    let Some(raw) = req.headers().get("Sec-WebSocket-Protocol")? else {
        return Ok(None);
    };
    for part in raw.split(',') {
        let trimmed = part.trim();
        if trimmed.starts_with(AUTH_PROTOCOL_PREFIX) {
            return Ok(Some(trimmed.to_string()));
        }
    }
    Ok(None)
}

fn error_response(message: &str, status: u16) -> Response {
    Response::error(message, status).unwrap_or_else(|_| {
        Response::error("server error", 500).unwrap_or_else(|_| Response::error("error", 500).unwrap())
    })
}

fn decode_base64_url(value: &str) -> Result<Vec<u8>, ()> {
    URL_SAFE_NO_PAD.decode(value).map_err(|_| ())
}

fn auth_message(room_id: &str, ts: i64, nonce: &str) -> Vec<u8> {
    format!("{room_id}\n{ts}\n{nonce}\n{AUTH_CONTEXT}").into_bytes()
}

fn derive_client_id(pubkey_spki: &[u8]) -> Result<ClientId, ()> {
    let digest = Sha256::digest(pubkey_spki);
    if digest.len() < 8 {
        return Err(());
    }
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&digest[..8]);
    Ok(ClientId::from(u64::from_be_bytes(bytes)))
}

fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write;
        let _ = write!(out, "{:02x}", byte);
    }
    out
}

fn transcode_to_avif(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let reader = ImageReader::new(Cursor::new(bytes))
        .with_guessed_format()
        .map_err(|err| err.to_string())?;
    let image = reader.decode().map_err(|err| err.to_string())?;
    let rgba = image.to_rgba8();
    let mut out = Vec::new();
    let encoder = AvifEncoder::new_with_speed_quality(&mut out, 6, 80)
        .with_num_threads(Some(1));
    encoder
        .write_image(
            &rgba,
            image.width(),
            image.height(),
            ExtendedColorType::Rgba8,
        )
        .map_err(|err| err.to_string())?;
    Ok(out)
}

fn detect_image_info(bytes: &[u8]) -> Result<(u32, u32, ImageType), String> {
    let image_type = imagesize::image_type(bytes).map_err(|err| err.to_string())?;
    let size = imagesize::blob_size(bytes).map_err(|err| err.to_string())?;
    let width = u32::try_from(size.width).map_err(|_| "image width too large".to_string())?;
    let height = u32::try_from(size.height).map_err(|_| "image height too large".to_string())?;
    if width == 0 || height == 0 {
        return Err("image dimensions are zero".to_string());
    }
    Ok((width, height, image_type))
}

fn is_avif(image_type: ImageType) -> bool {
    matches!(image_type, ImageType::Heif(Compression::Av1))
}

fn verify_signature(pubkey_spki: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, ()> {
    let verifying_key = VerifyingKey::from_sec1_bytes(pubkey_spki).map_err(|_| ())?;
    let signature = Signature::from_slice(signature).map_err(|_| ())?;
    Ok(verifying_key.verify(message, &signature).is_ok())
}

fn client_id_from_tags(tags: &[String]) -> Option<ClientId> {
    for tag in tags {
        if let Some(rest) = tag.strip_prefix("client:") {
            if let Ok(id) = rest.parse::<u64>() {
                return Some(ClientId::from(id));
            }
        }
    }
    None
}

fn is_admin_from_tags(tags: &[String]) -> bool {
    tags.iter().any(|tag| tag == "admin")
}

#[derive(Debug, SerdeDeserialize)]
struct AuthPayload {
    v: u8,
    client_id: String,
    ts: i64,
    nonce: String,
    pubkey: String,
    sig: String,
    #[serde(default)]
    admin_token: Option<String>,
}

struct AuthContext {
    client_id: ClientId,
    is_admin: bool,
    protocol: String,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct RoomMeta {
    activated: bool,
    persistence: RoomPersistence,
    last_command_at: Option<i64>,
    last_warning_at: Option<i64>,
    last_full_state_at: Option<i64>,
}

impl Default for RoomMeta {
    fn default() -> Self {
        Self {
            activated: false,
            persistence: RoomPersistence::Durable,
            last_command_at: None,
            last_warning_at: None,
            last_full_state_at: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Ownership {
    owner_id: ClientId,
    anchor_id: u32,
    since_ms: i64,
}

struct RoomRuntime {
    loaded: bool,
    meta: RoomMeta,
    snapshot: Option<GameSnapshot>,
    owners_by_anchor: HashMap<u32, Ownership>,
    owner_by_client: HashMap<ClientId, u32>,
    recent_nonces: HashMap<String, i64>,
    pending_releases: HashMap<ClientId, i64>,
    assets: HashMap<String, StoredAsset>,
    asset_index: Vec<String>,
    pending_uploads: HashMap<ClientId, PendingUpload>,
}

impl RoomRuntime {
    fn new() -> Self {
        Self {
            loaded: false,
            meta: RoomMeta::default(),
            snapshot: None,
            owners_by_anchor: HashMap::new(),
            owner_by_client: HashMap::new(),
            recent_nonces: HashMap::new(),
            pending_releases: HashMap::new(),
            assets: HashMap::new(),
            asset_index: Vec::new(),
            pending_uploads: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct StoredAssetMeta {
    mime: String,
    width: u32,
    height: u32,
    size: u32,
    created_at: i64,
    chunks: u32,
}

#[derive(Debug, Clone)]
struct StoredAsset {
    meta: StoredAssetMeta,
    bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
struct PendingUpload {
    size: u32,
    received: u32,
    bytes: Vec<u8>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct AssetIndex {
    hashes: Vec<String>,
}

#[durable_object]
pub struct Room {
    state: State,
    env: Env,
    inner: RefCell<RoomRuntime>,
}

impl DurableObject for Room {
    fn new(state: State, env: Env) -> Self {
        Self {
            state,
            env,
            inner: RefCell::new(RoomRuntime::new()),
        }
    }

    async fn fetch(&self, req: Request) -> Result<Response> {
        let is_websocket = req
            .headers()
            .get("Upgrade")?
            .map(|h| h.to_ascii_lowercase() == "websocket")
            .unwrap_or(false);
        if !is_websocket {
            return Response::error("expected websocket", 400);
        }

        let path = req.path();
        let prefix = room_path_prefix(&self.env);
        let room_id = extract_room_id(&path, &prefix).unwrap_or("unknown");

        self.ensure_loaded().await?;
        let auth = match self.authenticate_request(&req, room_id).await {
            Ok(auth) => auth,
            Err(response) => return Ok(response),
        };
        let is_admin = auth.is_admin;
        let client_id = auth.client_id;

        let activated = { self.inner.borrow().meta.activated };
        if !activated && !is_admin {
            return Response::error("room not activated", 403);
        }

        if !is_admin && self.has_active_client(client_id, false) {
            return Response::error("client already connected", 409);
        }

        if !is_admin {
            self.clear_pending_release(client_id);
        }
        let pair = WebSocketPair::new()?;
        let server = pair.server;
        let client_tag = format!("client:{client_id}");
        if is_admin {
            let tags = ["admin", client_tag.as_str()];
            self.state.accept_websocket_with_tags(&server, &tags);
        } else {
            let tags = [client_tag.as_str()];
            self.state.accept_websocket_with_tags(&server, &tags);
        }

        if activated {
            let (persistence, initialized) = {
                let inner = self.inner.borrow();
                (inner.meta.persistence, inner.snapshot.is_some())
            };
            let welcome = ServerMsg::Welcome {
                room_id: room_id.to_string(),
                persistence,
                initialized,
                client_id: Some(client_id),
            };
            let _ = self.send_server_msg(&server, &welcome);

            if initialized {
                if let Some(snapshot) = self.inner.borrow().snapshot.clone() {
                    let msg = ServerMsg::State {
                        seq: snapshot.seq,
                        snapshot,
                    };
                    let _ = self.send_server_msg(&server, &msg);
                }
            } else if !is_admin {
                let _ = self.send_server_msg(&server, &ServerMsg::NeedInit);
            }
        }

        let headers = Headers::new();
        let _ = headers.set("Sec-WebSocket-Protocol", &auth.protocol);
        Ok(Response::builder()
            .with_websocket(pair.client)
            .with_status(101)
            .with_headers(headers)
            .empty())
    }

    async fn websocket_message(
        &self,
        ws: WebSocket,
        message: WebSocketIncomingMessage,
    ) -> Result<()> {
        self.ensure_loaded().await?;

        let bytes = match message {
            WebSocketIncomingMessage::Binary(bytes) => bytes,
            WebSocketIncomingMessage::String(_) => return Ok(()),
        };

        let tags = self.state.get_tags(&ws);
        let is_admin = is_admin_from_tags(&tags);
        if is_admin {
            if let Some(msg) = decode::<AdminMsg>(&bytes) {
                match msg {
                    AdminMsg::Create { persistence, puzzle } => {
                        return self.handle_admin_create(ws, persistence, puzzle).await;
                    }
                    AdminMsg::ChangePuzzle { puzzle } => {
                        return self.handle_admin_change_puzzle(ws, puzzle).await;
                    }
                    AdminMsg::UploadPrivateBegin { mime, size } => {
                        return self.handle_admin_upload_begin(ws, mime, size).await;
                    }
                    AdminMsg::UploadPrivateChunk { bytes } => {
                        return self.handle_admin_upload_chunk(ws, bytes).await;
                    }
                    AdminMsg::UploadPrivateEnd { pieces, seed } => {
                        return self
                            .handle_admin_upload_end(ws, pieces, seed)
                            .await;
                    }
                    AdminMsg::Scramble { seed } => {
                        return self.handle_admin_scramble(ws, seed).await;
                    }
                }
            }
            return Ok(());
        }

        let Some(client_id) = client_id_from_tags(&tags) else {
            return Ok(());
        };

        let Some(msg) = decode::<ClientMsg>(&bytes) else {
            return Ok(());
        };

        match msg {
            ClientMsg::Init { puzzle, rules, state } => {
                self.handle_init(ws, puzzle, rules, state).await?;
            }
            ClientMsg::AssetRequest { hash } => {
                self.handle_asset_request(ws, hash).await?;
            }
            ClientMsg::Select { piece_id } => {
                self.handle_select(client_id, piece_id).await?;
            }
            ClientMsg::Move {
                anchor_id,
                pos,
                client_seq,
            } => {
                self.handle_move(client_id, anchor_id, pos, client_seq)
                    .await?;
            }
            ClientMsg::Transform {
                anchor_id,
                pos,
                rot_deg,
                client_seq,
            } => {
                self.handle_transform(client_id, anchor_id, pos, rot_deg, client_seq)
                    .await?;
            }
            ClientMsg::Rotate { anchor_id, rot_deg } => {
                self.handle_rotate(client_id, anchor_id, rot_deg).await?;
            }
            ClientMsg::Place { anchor_id, pos, rot_deg } => {
                self.handle_place(client_id, anchor_id, pos, rot_deg).await?;
            }
            ClientMsg::Flip { piece_id, flipped } => {
                self.handle_flip(client_id, piece_id, flipped).await?;
            }
            ClientMsg::Release { anchor_id } => {
                self.handle_release(client_id, anchor_id).await?;
            }
            ClientMsg::Ping { nonce } => {
                let response = ServerMsg::Pong { nonce };
                let _ = self.send_server_msg(&ws, &response);
            }
        }

        Ok(())
    }

    async fn websocket_close(
        &self,
        ws: WebSocket,
        _code: usize,
        _reason: String,
        _was_clean: bool,
    ) -> Result<()> {
        let tags = self.state.get_tags(&ws);
        let is_admin = is_admin_from_tags(&tags);
        if let Some(client_id) = client_id_from_tags(&tags) {
            if !is_admin {
                self.schedule_disconnect_release(client_id, now_ms());
                self.schedule_alarm().await?;
            }
        }
        Ok(())
    }

    async fn websocket_error(&self, _ws: WebSocket, _error: Error) -> Result<()> {
        Ok(())
    }

    async fn alarm(&self) -> Result<Response> {
        self.ensure_loaded().await?;

        let has_clients = !self.state.get_websockets().is_empty();
        let (activated, last_command_at, last_warning_at, last_full_state_at, has_snapshot) = {
            let inner = self.inner.borrow();
            (
                inner.meta.activated,
                inner.meta.last_command_at,
                inner.meta.last_warning_at,
                inner.meta.last_full_state_at,
                inner.snapshot.is_some(),
            )
        };
        if !activated {
            return Response::ok("inactive");
        }

        let now = now_ms();

        if let Some(last_command_at) = last_command_at {
            let expire_at = last_command_at + INACTIVITY_EXPIRE_MS;
            if now >= expire_at {
                self.expire_room().await?;
                return Response::ok("expired");
            }
            let warn_at = last_command_at + INACTIVITY_WARNING_MS;
            if now >= warn_at && last_warning_at.map(|t| t < warn_at).unwrap_or(true) {
                self.broadcast(&ServerMsg::Warning { minutes_idle: 10 })?;
                self.update_last_warning_at(now).await?;
            }
        }

        self.release_pending_disconnects(now)?;
        self.release_timeouts(now)?;

        if has_snapshot && has_clients {
            if let Some(last_full) = last_full_state_at {
                if now >= last_full + FULL_STATE_INTERVAL_MS {
                    self.broadcast_full_state().await?;
                }
            } else {
                self.broadcast_full_state().await?;
            }
        }

        self.schedule_alarm().await?;
        Response::ok("scheduled")
    }
}

struct RoomGeometry {
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    center_min_x: f32,
    center_max_x: f32,
    center_min_y: f32,
    center_max_y: f32,
    snap_distance: f32,
    frame_snap_ratio: f32,
    rotation_snap_tolerance: f32,
    rotation_enabled: bool,
}

impl Room {
    fn admin_token(&self) -> Result<String> {
        Ok(self.env.var("ADMIN_TOKEN")?.to_string())
    }

    async fn ensure_loaded(&self) -> Result<()> {
        let loaded = { self.inner.borrow().loaded };
        if loaded {
            return Ok(());
        }

        let storage = self.state.storage();
        let meta_bytes: Option<Vec<u8>> = storage.get(META_KEY).await?;
        let snapshot_bytes: Option<Vec<u8>> = storage.get(SNAPSHOT_KEY).await?;
        let asset_index_bytes: Option<Vec<u8>> = storage.get(ASSET_INDEX_KEY).await?;

        let mut inner = self.inner.borrow_mut();
        inner.loaded = true;
        if let Some(bytes) = meta_bytes {
            if let Some(meta) = decode::<RoomMeta>(&bytes) {
                inner.meta = meta;
            }
        }
        if let Some(bytes) = snapshot_bytes {
            if let Some(snapshot) = decode::<GameSnapshot>(&bytes) {
                inner.snapshot = Some(snapshot);
            }
        }
        if let Some(bytes) = asset_index_bytes {
            if let Some(index) = decode::<AssetIndex>(&bytes) {
                inner.asset_index = index.hashes;
            }
        }

        Ok(())
    }

    async fn authenticate_request(
        &self,
        req: &Request,
        room_id: &str,
    ) -> Result<AuthContext, Response> {
        let protocol = match auth_protocol_from_request(req) {
            Ok(Some(protocol)) => protocol,
            Ok(None) => return Err(error_response("missing auth", 401)),
            Err(_) => return Err(error_response("invalid auth", 401)),
        };
        let Some(payload_b64) = protocol.strip_prefix(AUTH_PROTOCOL_PREFIX) else {
            return Err(error_response("invalid auth", 401));
        };
        let payload_bytes =
            decode_base64_url(payload_b64).map_err(|_| error_response("invalid auth", 401))?;
        let payload: AuthPayload =
            serde_json::from_slice(&payload_bytes).map_err(|_| error_response("invalid auth", 401))?;
        if payload.v != 1 {
            return Err(error_response("invalid auth", 401));
        }
        let client_id = ClientId::from(
            payload
                .client_id
                .parse::<u64>()
                .map_err(|_| error_response("invalid client id", 401))?,
        );
        let now = now_ms();
        let drift = if now >= payload.ts {
            now - payload.ts
        } else {
            payload.ts - now
        };
        if drift > AUTH_WINDOW_MS {
            return Err(error_response("auth expired", 401));
        }
        let pubkey_bytes =
            decode_base64_url(&payload.pubkey).map_err(|_| error_response("invalid auth", 401))?;
        let sig_bytes =
            decode_base64_url(&payload.sig).map_err(|_| error_response("invalid auth", 401))?;
        let derived_id =
            derive_client_id(&pubkey_bytes).map_err(|_| error_response("invalid auth", 401))?;
        if derived_id != client_id {
            return Err(error_response("invalid auth", 401));
        }
        let message = auth_message(room_id, payload.ts, &payload.nonce);
        let valid =
            verify_signature(&pubkey_bytes, &message, &sig_bytes)
                .map_err(|_| error_response("invalid auth", 401))?;
        if !valid {
            return Err(error_response("invalid auth", 401));
        }
        let nonce_key = format!("{client_id}:{}", payload.nonce);
        if !self.record_nonce(&nonce_key, now) {
            return Err(error_response("replay detected", 401));
        }

        let mut is_admin = false;
        if let Some(token) = payload
            .admin_token
            .as_ref()
            .map(|token| token.trim())
            .filter(|token| !token.is_empty())
        {
            let expected = self
                .admin_token()
                .map_err(|_| error_response("invalid admin token", 403))?;
            if token == expected {
                is_admin = true;
            } else {
                return Err(error_response("invalid admin token", 403));
            }
        }

        Ok(AuthContext {
            client_id,
            is_admin,
            protocol,
        })
    }

    fn record_nonce(&self, key: &str, now: i64) -> bool {
        let mut inner = self.inner.borrow_mut();
        inner
            .recent_nonces
            .retain(|_, ts| now.saturating_sub(*ts) <= AUTH_WINDOW_MS);
        if inner.recent_nonces.contains_key(key) {
            return false;
        }
        inner.recent_nonces.insert(key.to_string(), now);
        true
    }

    fn has_active_client(&self, client_id: ClientId, include_admin: bool) -> bool {
        let tag = format!("client:{client_id}");
        for socket in self.state.get_websockets() {
            let tags = self.state.get_tags(&socket);
            if tags.iter().any(|value| value == &tag) {
                if include_admin || !is_admin_from_tags(&tags) {
                    return true;
                }
            }
        }
        false
    }

    fn clear_pending_release(&self, client_id: ClientId) {
        let mut inner = self.inner.borrow_mut();
        inner.pending_releases.remove(&client_id);
    }

    fn schedule_disconnect_release(&self, client_id: ClientId, now: i64) {
        let mut inner = self.inner.borrow_mut();
        inner
            .pending_releases
            .insert(client_id, now + DISCONNECT_GRACE_MS);
    }

    fn release_pending_disconnects(&self, now: i64) -> Result<()> {
        let due_clients = {
            let mut inner = self.inner.borrow_mut();
            let mut due = Vec::new();
            inner.pending_releases.retain(|client_id, due_at| {
                if now >= *due_at {
                    due.push(*client_id);
                    false
                } else {
                    true
                }
            });
            due
        };
        for client_id in due_clients {
            if !self.has_active_client(client_id, false) {
                self.release_by_client(client_id, OwnershipReason::Released)?;
            }
        }
        Ok(())
    }

    fn geometry_for_snapshot(snapshot: &GameSnapshot) -> Option<RoomGeometry> {
        let cols = snapshot.puzzle.cols as usize;
        let rows = snapshot.puzzle.rows as usize;
        if cols == 0 || rows == 0 {
            return None;
        }
        let image_width = snapshot.puzzle.image_width as f32;
        let image_height = snapshot.puzzle.image_height as f32;
        if image_width <= 0.0 || image_height <= 0.0 {
            return None;
        }
        let piece_width = image_width / cols as f32;
        let piece_height = image_height / rows as f32;
        let layout = compute_workspace_layout(
            image_width,
            image_height,
            snapshot.rules.workspace_padding_ratio,
        );
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
        let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
        let puzzle_view_width = layout.view_width / puzzle_scale;
        let puzzle_view_height = layout.view_height / puzzle_scale;
        let center_min_x = puzzle_view_min_x + piece_width * 0.5;
        let center_min_y = puzzle_view_min_y + piece_height * 0.5;
        let mut center_max_x = puzzle_view_min_x + puzzle_view_width - piece_width * 0.5;
        let mut center_max_y = puzzle_view_min_y + puzzle_view_height - piece_height * 0.5;
        if center_max_x < center_min_x {
            center_max_x = center_min_x;
        }
        if center_max_y < center_min_y {
            center_max_y = center_min_y;
        }
        let snap_distance = piece_width.min(piece_height) * snapshot.rules.snap_distance_ratio;
        Some(RoomGeometry {
            cols,
            rows,
            piece_width,
            piece_height,
            view_min_x: puzzle_view_min_x,
            view_min_y: puzzle_view_min_y,
            view_width: puzzle_view_width,
            view_height: puzzle_view_height,
            center_min_x,
            center_max_x,
            center_min_y,
            center_max_y,
            snap_distance,
            frame_snap_ratio: snapshot.rules.frame_snap_ratio,
            rotation_snap_tolerance: snapshot.rules.rotation_snap_tolerance_deg,
            rotation_enabled: snapshot.rules.rotation_enabled,
        })
    }

    fn build_puzzle_from_spec(
        &self,
        spec: PuzzleSpec,
        rules: &GameRules,
    ) -> Result<(PuzzleInfo, Option<u32>), String> {
        validate_image_ref(&spec.image_ref)?;
        let (label, image_ref, image_width, image_height) = match &spec.image_ref {
            PuzzleImageRef::BuiltIn { slug } => {
                let entry = puzzle_by_slug(slug).ok_or_else(|| {
                    format!("unknown puzzle: {slug}")
                })?;
                let (width, height) = logical_image_size(
                    entry.width,
                    entry.height,
                    rules.image_max_dimension,
                );
                (
                    entry.label.to_string(),
                    PuzzleImageRef::BuiltIn {
                        slug: entry.slug.to_string(),
                    },
                    width,
                    height,
                )
            }
            PuzzleImageRef::Private { hash } => {
                let _ = hash;
                return Err("private puzzles must be uploaded".to_string());
            }
        };
        let target = spec.pieces.unwrap_or(DEFAULT_TARGET_COUNT);
        let grid = best_grid_for_count(image_width, image_height, target).unwrap_or(FALLBACK_GRID);
        let scramble_override = spec.seed.map(|seed| {
            scramble_nonce_from_seed(
                PUZZLE_SEED,
                seed,
                grid.cols as usize,
                grid.rows as usize,
            )
        });
        let puzzle = PuzzleInfo {
            label,
            image_ref,
            rows: grid.rows,
            cols: grid.cols,
            shape_seed: PUZZLE_SEED,
            image_width,
            image_height,
        };
        Ok((puzzle, scramble_override))
    }

    async fn handle_admin_create(
        &self,
        ws: WebSocket,
        persistence: RoomPersistence,
        puzzle: PuzzleSpec,
    ) -> Result<()> {
        let now = now_ms();
        let rules = GameRules::default();
        let (puzzle, scramble_override) = match self.build_puzzle_from_spec(puzzle, &rules) {
            Ok(result) => result,
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_puzzle".to_string(),
                        message,
                    },
                );
                return Ok(());
            }
        };
        let snapshot = match self.build_initial_snapshot(puzzle, rules, None, scramble_override) {
            Some(snapshot) => snapshot,
            None => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_init".to_string(),
                        message: "failed to initialize room".to_string(),
                    },
                );
                return Ok(());
            }
        };
        {
            let mut inner = self.inner.borrow_mut();
            if inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "already_active".to_string(),
                        message: "room already active".to_string(),
                    },
                );
                return Ok(());
            }
            inner.meta.activated = true;
            inner.meta.persistence = persistence;
            inner.meta.last_command_at = Some(now);
            inner.meta.last_warning_at = None;
            inner.meta.last_full_state_at = None;
            inner.snapshot = Some(snapshot);
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.persist_meta().await?;
        self.persist_snapshot_if_needed().await?;
        self.schedule_alarm().await?;

        let _ = self.send_server_msg(
            &ws,
            &ServerMsg::AdminAck {
                room_id: "unknown".to_string(),
                persistence,
            },
        );
        Ok(())
    }

    async fn handle_admin_change_puzzle(
        &self,
        ws: WebSocket,
        puzzle: PuzzleSpec,
    ) -> Result<()> {
        let now = now_ms();
        let rules = {
            let inner = self.inner.borrow();
            if !inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "inactive".to_string(),
                        message: "room not activated".to_string(),
                    },
                );
                return Ok(());
            }
            inner
                .snapshot
                .as_ref()
                .map(|snapshot| snapshot.rules.clone())
                .unwrap_or_default()
        };
        let (puzzle, scramble_override) = match self.build_puzzle_from_spec(puzzle, &rules) {
            Ok(result) => result,
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_puzzle".to_string(),
                        message,
                    },
                );
                return Ok(());
            }
        };
        let mut snapshot =
            match self.build_initial_snapshot(puzzle, rules, None, scramble_override) {
                Some(snapshot) => snapshot,
                None => {
                    let _ = self.send_server_msg(
                        &ws,
                        &ServerMsg::Error {
                            code: "invalid_init".to_string(),
                            message: "failed to initialize room".to_string(),
                        },
                    );
                    return Ok(());
                }
            };
        {
            let mut inner = self.inner.borrow_mut();
            let next_seq = inner
                .snapshot
                .as_ref()
                .map(|snap| snap.seq.saturating_add(1))
                .unwrap_or(0);
            snapshot.seq = next_seq;
            inner.snapshot = Some(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&ServerMsg::State {
            seq: snapshot.seq,
            snapshot,
        })?;
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_admin_upload_begin(
        &self,
        ws: WebSocket,
        _mime: String,
        size: u32,
    ) -> Result<()> {
        if size == 0 || size > PRIVATE_UPLOAD_MAX_BYTES {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "upload_too_large".to_string(),
                    message: format!(
                        "upload exceeds limit (max {} bytes)",
                        PRIVATE_UPLOAD_MAX_BYTES
                    ),
                },
            );
            return Ok(());
        }
        let tags = self.state.get_tags(&ws);
        let Some(client_id) = client_id_from_tags(&tags) else {
            return Ok(());
        };
        {
            let inner = self.inner.borrow();
            if !inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "inactive".to_string(),
                        message: "room not activated".to_string(),
                    },
                );
                return Ok(());
            }
        }
        let mut inner = self.inner.borrow_mut();
        inner.pending_uploads.insert(
            client_id,
            PendingUpload {
                size,
                received: 0,
                bytes: Vec::with_capacity(size as usize),
            },
        );
        Ok(())
    }

    async fn handle_admin_upload_chunk(
        &self,
        ws: WebSocket,
        bytes: Vec<u8>,
    ) -> Result<()> {
        let tags = self.state.get_tags(&ws);
        let Some(client_id) = client_id_from_tags(&tags) else {
            return Ok(());
        };
        let mut inner = self.inner.borrow_mut();
        let Some(pending) = inner.pending_uploads.get_mut(&client_id) else {
            return Ok(());
        };
        let next = pending
            .received
            .saturating_add(bytes.len().min(u32::MAX as usize) as u32);
        if next > pending.size || next > PRIVATE_UPLOAD_MAX_BYTES {
            inner.pending_uploads.remove(&client_id);
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "upload_too_large".to_string(),
                    message: format!(
                        "upload exceeds limit (max {} bytes)",
                        PRIVATE_UPLOAD_MAX_BYTES
                    ),
                },
            );
            return Ok(());
        }
        pending.bytes.extend_from_slice(&bytes);
        pending.received = next;
        Ok(())
    }

    async fn handle_admin_upload_end(
        &self,
        ws: WebSocket,
        pieces: Option<u32>,
        seed: Option<u32>,
    ) -> Result<()> {
        let now = now_ms();
        let tags = self.state.get_tags(&ws);
        let Some(client_id) = client_id_from_tags(&tags) else {
            return Ok(());
        };
        let pending = {
            let mut inner = self.inner.borrow_mut();
            inner.pending_uploads.remove(&client_id)
        };
        let Some(pending) = pending else {
            return Ok(());
        };
        if pending.received != pending.size {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "upload_incomplete".to_string(),
                    message: "upload incomplete".to_string(),
                },
            );
            return Ok(());
        }
        let (raw_width, raw_height, image_type) = match detect_image_info(&pending.bytes) {
            Ok(info) => info,
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_image".to_string(),
                        message,
                    },
                );
                return Ok(());
            }
        };
        if matches!(image_type, ImageType::Heif(_)) && !is_avif(image_type) {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "invalid_image".to_string(),
                    message: "unsupported HEIF compression".to_string(),
                },
            );
            return Ok(());
        }
        let stored_bytes = if is_avif(image_type) {
            pending.bytes
        } else {
            match transcode_to_avif(&pending.bytes) {
                Ok(image) => image,
                Err(message) => {
                    let _ = self.send_server_msg(
                        &ws,
                        &ServerMsg::Error {
                            code: "invalid_image".to_string(),
                            message,
                        },
                    );
                    return Ok(());
                }
            }
        };
        let stored_mime = "image/avif".to_string();
        if stored_bytes.len() > PRIVATE_ASSET_MAX_BYTES as usize {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "asset_too_large".to_string(),
                    message: format!(
                        "stored asset exceeds limit (max {} bytes)",
                        PRIVATE_ASSET_MAX_BYTES
                    ),
                },
            );
            return Ok(());
        }
        let rules = {
            let inner = self.inner.borrow();
            if !inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "inactive".to_string(),
                        message: "room not activated".to_string(),
                    },
                );
                return Ok(());
            }
            inner
                .snapshot
                .as_ref()
                .map(|snapshot| snapshot.rules.clone())
                .unwrap_or_default()
        };
        let (image_width, image_height) =
            logical_image_size(raw_width, raw_height, rules.image_max_dimension);
        let hash = sha256_hex(&stored_bytes);
        let size = stored_bytes.len() as u32;
        let chunks = ((size as usize + ASSET_STORAGE_CHUNK_BYTES - 1) / ASSET_STORAGE_CHUNK_BYTES)
            as u32;
        let asset = StoredAsset {
            meta: StoredAssetMeta {
                mime: stored_mime.clone(),
                width: image_width,
                height: image_height,
                size,
                created_at: now,
                chunks,
            },
            bytes: stored_bytes,
        };
        if let Err(message) = self.store_asset(&hash, asset).await {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "asset_store_failed".to_string(),
                    message,
                },
            );
            return Ok(());
        }
        let target = pieces.unwrap_or(DEFAULT_TARGET_COUNT);
        let grid = best_grid_for_count(image_width, image_height, target).unwrap_or(FALLBACK_GRID);
        let scramble_override = seed.map(|seed| {
            scramble_nonce_from_seed(
                PUZZLE_SEED,
                seed,
                grid.cols as usize,
                grid.rows as usize,
            )
        });
        let puzzle = PuzzleInfo {
            label: String::new(),
            image_ref: PuzzleImageRef::Private { hash: hash.clone() },
            rows: grid.rows,
            cols: grid.cols,
            shape_seed: PUZZLE_SEED,
            image_width,
            image_height,
        };
        let mut snapshot =
            match self.build_initial_snapshot(puzzle, rules, None, scramble_override) {
                Some(snapshot) => snapshot,
                None => {
                    let _ = self.send_server_msg(
                        &ws,
                        &ServerMsg::Error {
                            code: "invalid_init".to_string(),
                            message: "failed to initialize room".to_string(),
                        },
                    );
                    return Ok(());
                }
            };
        {
            let mut inner = self.inner.borrow_mut();
            let next_seq = inner
                .snapshot
                .as_ref()
                .map(|snap| snap.seq.saturating_add(1))
                .unwrap_or(0);
            snapshot.seq = next_seq;
            inner.snapshot = Some(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        let _ = self.send_server_msg(&ws, &ServerMsg::UploadAck { hash: hash.clone() });
        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&ServerMsg::State {
            seq: snapshot.seq,
            snapshot,
        })?;
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_admin_scramble(&self, ws: WebSocket, seed: Option<u32>) -> Result<()> {
        let now = now_ms();
        let (puzzle, rules) = {
            let inner = self.inner.borrow();
            if !inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "inactive".to_string(),
                        message: "room not activated".to_string(),
                    },
                );
                return Ok(());
            }
            let Some(snapshot) = inner.snapshot.as_ref() else {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "uninitialized".to_string(),
                        message: "room not initialized".to_string(),
                    },
                );
                return Ok(());
            };
            (snapshot.puzzle.clone(), snapshot.rules.clone())
        };
        let cols = puzzle.cols as usize;
        let rows = puzzle.rows as usize;
        let scramble_override =
            seed.map(|seed| scramble_nonce_from_seed(PUZZLE_SEED, seed, cols, rows));
        let mut snapshot =
            match self.build_initial_snapshot(puzzle, rules, None, scramble_override) {
                Some(snapshot) => snapshot,
                None => {
                    let _ = self.send_server_msg(
                        &ws,
                        &ServerMsg::Error {
                            code: "invalid_init".to_string(),
                            message: "failed to scramble room".to_string(),
                        },
                    );
                    return Ok(());
                }
            };
        {
            let mut inner = self.inner.borrow_mut();
            let next_seq = inner
                .snapshot
                .as_ref()
                .map(|snap| snap.seq.saturating_add(1))
                .unwrap_or(0);
            snapshot.seq = next_seq;
            inner.snapshot = Some(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&ServerMsg::State {
            seq: snapshot.seq,
            snapshot,
        })?;
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_asset_request(&self, ws: WebSocket, hash: String) -> Result<()> {
        let asset = match self.load_asset(&hash).await {
            Ok(Some(asset)) => asset,
            Ok(None) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "asset_missing".to_string(),
                        message: "private image not found".to_string(),
                    },
                );
                return Ok(());
            }
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "asset_missing".to_string(),
                        message,
                    },
                );
                return Ok(());
            }
        };
        if let Err(message) = self.send_asset(&ws, &hash, &asset) {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "asset_send_failed".to_string(),
                    message,
                },
            );
        }
        Ok(())
    }

    fn send_asset(
        &self,
        ws: &WebSocket,
        hash: &str,
        asset: &StoredAsset,
    ) -> std::result::Result<(), String> {
        self.send_server_msg(
            ws,
            &ServerMsg::AssetBegin {
                hash: hash.to_string(),
                mime: asset.meta.mime.clone(),
                width: asset.meta.width,
                height: asset.meta.height,
                size: asset.meta.size,
            },
        )
        .map_err(|err| err.to_string())?;
        let mut index = 0u32;
        for chunk in asset.bytes.chunks(ASSET_CHUNK_BYTES) {
            self.send_server_msg(
                ws,
                &ServerMsg::AssetChunk {
                    hash: hash.to_string(),
                    index,
                    bytes: chunk.to_vec(),
                },
            )
            .map_err(|err| err.to_string())?;
            index = index.saturating_add(1);
        }
        self.send_server_msg(
            ws,
            &ServerMsg::AssetEnd {
                hash: hash.to_string(),
            },
        )
        .map_err(|err| err.to_string())?;
        Ok(())
    }

    async fn store_asset(
        &self,
        hash: &str,
        asset: StoredAsset,
    ) -> std::result::Result<(), String> {
        let (should_persist, hashes) = {
            let mut inner = self.inner.borrow_mut();
            inner.assets.insert(hash.to_string(), asset.clone());
            if !inner.asset_index.iter().any(|value| value == hash) {
                inner.asset_index.push(hash.to_string());
            }
            let hashes = inner.asset_index.clone();
            let should_persist = matches!(inner.meta.persistence, RoomPersistence::Durable);
            (should_persist, hashes)
        };
        if should_persist {
            self.persist_asset(hash, &asset).await?;
            self.persist_asset_index(hashes).await?;
        }
        Ok(())
    }

    async fn load_asset(
        &self,
        hash: &str,
    ) -> std::result::Result<Option<StoredAsset>, String> {
        if let Some(asset) = self.inner.borrow().assets.get(hash).cloned() {
            return Ok(Some(asset));
        }
        let persistence = { self.inner.borrow().meta.persistence };
        if !matches!(persistence, RoomPersistence::Durable) {
            return Ok(None);
        }
        let storage = self.state.storage();
        let meta_bytes: Option<Vec<u8>> = storage
            .get(&asset_meta_key(hash))
            .await
            .map_err(|err| err.to_string())?;
        let Some(bytes) = meta_bytes else {
            return Ok(None);
        };
        let Some(meta) = decode::<StoredAssetMeta>(&bytes) else {
            return Ok(None);
        };
        let mut bytes = Vec::with_capacity(meta.size as usize);
        for index in 0..meta.chunks {
            let chunk_key = asset_chunk_key(hash, index);
            let chunk: Option<Vec<u8>> = storage
                .get(&chunk_key)
                .await
                .map_err(|err| err.to_string())?;
            let Some(chunk) = chunk else {
                return Ok(None);
            };
            bytes.extend_from_slice(&chunk);
        }
        if bytes.len() != meta.size as usize {
            return Ok(None);
        }
        let asset = StoredAsset { meta, bytes };
        self.inner
            .borrow_mut()
            .assets
            .insert(hash.to_string(), asset.clone());
        Ok(Some(asset))
    }

    async fn persist_asset(
        &self,
        hash: &str,
        asset: &StoredAsset,
    ) -> std::result::Result<(), String> {
        let Some(meta_bytes) = encode(&asset.meta) else {
            return Ok(());
        };
        let storage = self.state.storage();
        storage
            .put(&asset_meta_key(hash), meta_bytes)
            .await
            .map_err(|err| err.to_string())?;
        let mut index = 0u32;
        for chunk in asset.bytes.chunks(ASSET_STORAGE_CHUNK_BYTES) {
            storage
                .put(&asset_chunk_key(hash, index), chunk.to_vec())
                .await
                .map_err(|err| err.to_string())?;
            index = index.saturating_add(1);
        }
        Ok(())
    }

    async fn persist_asset_index(
        &self,
        hashes: Vec<String>,
    ) -> std::result::Result<(), String> {
        let Some(bytes) = encode(&AssetIndex { hashes }) else {
            return Ok(());
        };
        let storage = self.state.storage();
        storage
            .put(ASSET_INDEX_KEY, bytes)
            .await
            .map_err(|err| err.to_string())?;
        Ok(())
    }

    async fn clear_assets(
        &self,
        hashes: Vec<String>,
    ) -> std::result::Result<(), String> {
        let storage = self.state.storage();
        let _ = storage.delete(ASSET_INDEX_KEY).await;
        for hash in hashes {
            let meta_key = asset_meta_key(&hash);
            let meta_bytes: Option<Vec<u8>> = storage
                .get(&meta_key)
                .await
                .map_err(|err| err.to_string())?;
            let mut chunks = None;
            if let Some(bytes) = meta_bytes {
                if let Some(meta) = decode::<StoredAssetMeta>(&bytes) {
                    chunks = Some(meta.chunks);
                }
            }
            let _ = storage.delete(&meta_key).await;
            if let Some(count) = chunks {
                for index in 0..count {
                    let _ = storage.delete(&asset_chunk_key(&hash, index)).await;
                }
            }
        }
        Ok(())
    }

    async fn handle_init(
        &self,
        ws: WebSocket,
        puzzle: PuzzleInfo,
        rules: Option<GameRules>,
        state: Option<PuzzleStateSnapshot>,
    ) -> Result<()> {
        let now = now_ms();
        let _ = rules;
        {
            let inner = self.inner.borrow();
            if !inner.meta.activated {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "inactive".to_string(),
                        message: "room not activated".to_string(),
                    },
                );
                return Ok(());
            }
            if inner.snapshot.is_some() {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "already_initialized".to_string(),
                        message: "room already initialized".to_string(),
                    },
                );
                return Ok(());
            }
        }

        if let Err(message) = validate_image_ref(&puzzle.image_ref) {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "invalid_puzzle".to_string(),
                    message,
                },
            );
            return Ok(());
        }

        let rules = GameRules::default();
        let snapshot = match self.build_initial_snapshot(puzzle, rules, state, None) {
            Some(snapshot) => snapshot,
            None => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_init".to_string(),
                        message: "invalid puzzle info".to_string(),
                    },
                );
                return Ok(());
            }
        };

        {
            let mut inner = self.inner.borrow_mut();
            inner.snapshot = Some(snapshot.clone());
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&ServerMsg::State {
            seq: snapshot.seq,
            snapshot,
        })?;
        self.schedule_alarm().await?;
        Ok(())
    }

    fn build_initial_snapshot(
        &self,
        puzzle: PuzzleInfo,
        rules: GameRules,
        state: Option<PuzzleStateSnapshot>,
        scramble_override: Option<u32>,
    ) -> Option<GameSnapshot> {
        let cols = puzzle.cols as usize;
        let rows = puzzle.rows as usize;
        if cols == 0 || rows == 0 {
            return None;
        }
        let image_width = puzzle.image_width as f32;
        let image_height = puzzle.image_height as f32;
        if image_width <= 0.0 || image_height <= 0.0 {
            return None;
        }

        let total = cols * rows;
        let piece_width = image_width / cols as f32;
        let piece_height = image_height / rows as f32;
        let layout = compute_workspace_layout(
            image_width,
            image_height,
            rules.workspace_padding_ratio,
        );
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
        let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
        let puzzle_view_width = layout.view_width / puzzle_scale;
        let puzzle_view_height = layout.view_height / puzzle_scale;
        let margin = piece_width.max(piece_height) * (DEFAULT_TAB_DEPTH_CAP + MAX_LINE_BEND_RATIO);

        let (positions, rotations, flips, connections, mut group_order, scramble_nonce) =
            if let Some(state) = state {
                if state.positions.len() != total
                    || state.rotations.len() != total
                    || state.flips.len() != total
                    || state.connections.len() != total
                {
                    return None;
                }
                (
                    state.positions,
                    state.rotations,
                    state.flips,
                    state.connections,
                    state.group_order,
                    state.scramble_nonce,
                )
            } else {
                let scramble_nonce = match scramble_override {
                    Some(value) => value,
                    None => {
                        let now_seed = splitmix32(now_ms() as u32 ^ splitmix32(total as u32));
                        splitmix32(now_seed ^ 0xA5A5_55AA)
                    }
                };
                let seed = scramble_seed(PUZZLE_SEED, scramble_nonce, cols, rows);
                let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
                let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
                let (positions, order) = scramble_layout(
                    seed,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    puzzle_view_min_x,
                    puzzle_view_min_y,
                    puzzle_view_width,
                    puzzle_view_height,
                    margin,
                );
                let rotations = scramble_rotations(rotation_seed, total, rules.rotation_enabled);
                let flips = scramble_flips(flip_seed, total, FLIP_CHANCE);
                let connections = vec![[false; 4]; total];
                let group_order = order.into_iter().map(|id| id as u32).collect();
                (positions, rotations, flips, connections, group_order, scramble_nonce)
            };
        let anchor_of = anchor_of_from_connections(&connections, cols, rows);
        group_order = reconcile_group_order(&group_order, &anchor_of);

        Some(GameSnapshot {
            version: SNAPSHOT_VERSION,
            seq: 0,
            rules,
            puzzle,
            state: PuzzleStateSnapshot {
                positions,
                rotations,
                flips,
                connections,
                group_order,
                scramble_nonce,
            },
        })
    }

    async fn handle_select(&self, client_id: ClientId, piece_id: u32) -> Result<()> {
        let now = now_ms();
        let (pending_updates, update_msg, group_order_update) = {
            let mut inner = self.inner.borrow_mut();
            let mut snapshot = match inner.snapshot.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let geometry = match Self::geometry_for_snapshot(&snapshot) {
                Some(geometry) => geometry,
                None => {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            };
            let total = geometry.cols * geometry.rows;
            if piece_id as usize >= total {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let members = collect_group(
                &snapshot.state.connections,
                piece_id as usize,
                geometry.cols,
                geometry.rows,
            );
            let mut members = members;
            if members.is_empty() {
                members.push(piece_id as usize);
            }
            members.sort_unstable();
            let anchor_id = members[0] as u32;

            if let Some(existing) = inner.owners_by_anchor.get(&anchor_id) {
                if existing.owner_id != client_id {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            }

            let mut pending_updates = Vec::new();
            if let Some(prev_anchor) = inner.owner_by_client.get(&client_id).copied() {
                if prev_anchor != anchor_id {
                    inner.owner_by_client.remove(&client_id);
                    inner.owners_by_anchor.remove(&prev_anchor);
                    console_log!(
                        "auto-release ownership: client={} anchor={}",
                        client_id,
                        prev_anchor
                    );
                    let seq = self.bump_seq(&mut snapshot);
                    pending_updates.push(ServerMsg::Update {
                        seq,
                        update: RoomUpdate::Ownership {
                            anchor_id: prev_anchor,
                            owner: None,
                            reason: OwnershipReason::AutoRelease,
                        },
                        source: Some(client_id),
                        client_seq: None,
                    });
                }
            }

            inner.owners_by_anchor.insert(
                anchor_id,
                Ownership {
                    owner_id: client_id,
                    anchor_id,
                    since_ms: now,
                },
            );
            inner.owner_by_client.insert(client_id, anchor_id);

            let mut group_order = snapshot.state.group_order.clone();
            group_order.retain(|id| *id != anchor_id);
            group_order.push(anchor_id);
            snapshot.state.group_order = group_order.clone();

            let seq = self.bump_seq(&mut snapshot);
            let update_msg = ServerMsg::Update {
                seq,
                update: RoomUpdate::Ownership {
                    anchor_id,
                    owner: Some(client_id),
                    reason: OwnershipReason::Granted,
                },
                source: Some(client_id),
                client_seq: None,
            };
            let group_order_update = ServerMsg::Update {
                seq,
                update: RoomUpdate::GroupOrder { order: group_order },
                source: Some(client_id),
                client_seq: None,
            };

            inner.snapshot = Some(snapshot);
            (pending_updates, update_msg, group_order_update)
        };

        self.touch_command(now, false).await?;
        self.persist_snapshot_if_needed().await?;
        for msg in pending_updates {
            let _ = self.broadcast(&msg);
        }
        let _ = self.broadcast(&update_msg);
        let _ = self.broadcast(&group_order_update);
        self.schedule_alarm().await?;

        Ok(())
    }

    async fn handle_move(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        pos: (f32, f32),
        client_seq: u64,
    ) -> Result<()> {
        let now = now_ms();
        let update = {
            let mut inner = self.inner.borrow_mut();
            let mut snapshot = match inner.snapshot.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                console_log!(
                    "move ignored: not owner (client={} anchor={})",
                    client_id,
                    anchor_id
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let geometry = match Self::geometry_for_snapshot(&snapshot) {
                Some(geometry) => geometry,
                None => {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            };
            let total = geometry.cols * geometry.rows;
            if anchor_id as usize >= total {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let members = collect_group(
                &snapshot.state.connections,
                anchor_id as usize,
                geometry.cols,
                geometry.rows,
            );
            let mut members = members;
            if members.is_empty() {
                members.push(anchor_id as usize);
            }
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                console_log!(
                    "move ignored: anchor mismatch (client={} anchor={} canonical={})",
                    client_id,
                    anchor_id,
                    anchor_id_usize
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let anchor_rot = snapshot.state.rotations.get(anchor_id_usize).copied().unwrap_or(0.0);

            let mut next_positions = Vec::with_capacity(members.len());
            for &id in &members {
                let (dx, dy) = piece_local_offset(
                    id,
                    anchor_id_usize,
                    geometry.cols,
                    geometry.piece_width,
                    geometry.piece_height,
                );
                let (rx, ry) = rotate_vec(dx, dy, anchor_rot);
                next_positions.push((id, (pos.0 + rx, pos.1 + ry)));
            }

            if !self.group_in_bounds(&members, &next_positions, &geometry) {
                console_log!(
                    "move ignored: out of bounds (client={} anchor={})",
                    client_id,
                    anchor_id
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            for (id, next_pos) in next_positions {
                if let Some(slot) = snapshot.state.positions.get_mut(id) {
                    *slot = next_pos;
                }
                if let Some(rot) = snapshot.state.rotations.get_mut(id) {
                    *rot = anchor_rot;
                }
            }

            let seq = self.bump_seq(&mut snapshot);
            let update = Some(ServerMsg::Update {
                seq,
                update: RoomUpdate::GroupTransform {
                    anchor_id,
                    pos,
                    rot_deg: anchor_rot,
                },
                source: Some(client_id),
                client_seq: Some(client_seq).filter(|value| *value != 0),
            });
            inner.snapshot = Some(snapshot);
            update
        };

        self.touch_command(now, false).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some(update) = update {
            console_log!(
                "move accepted: client={} anchor={} seq={}",
                client_id,
                anchor_id,
                match &update {
                    ServerMsg::Update { seq, .. } => *seq,
                    _ => 0,
                }
            );
            let _ = self.broadcast(&update);
        }
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_transform(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: f32,
        client_seq: u64,
    ) -> Result<()> {
        let now = now_ms();
        let update = {
            let mut inner = self.inner.borrow_mut();
            let mut snapshot = match inner.snapshot.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let geometry = match Self::geometry_for_snapshot(&snapshot) {
                Some(geometry) => geometry,
                None => {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            };
            let total = geometry.cols * geometry.rows;
            if anchor_id as usize >= total {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let members = collect_group(
                &snapshot.state.connections,
                anchor_id as usize,
                geometry.cols,
                geometry.rows,
            );
            let mut members = members;
            if members.is_empty() {
                members.push(anchor_id as usize);
            }
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                console_log!(
                    "transform ignored: anchor mismatch (client={} anchor={} canonical={})",
                    client_id,
                    anchor_id,
                    anchor_id_usize
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let mut next_positions = Vec::with_capacity(members.len());
            for &id in &members {
                let (dx, dy) = piece_local_offset(
                    id,
                    anchor_id_usize,
                    geometry.cols,
                    geometry.piece_width,
                    geometry.piece_height,
                );
                let (rx, ry) = rotate_vec(dx, dy, rot_deg);
                next_positions.push((id, (pos.0 + rx, pos.1 + ry)));
            }

            if !self.group_in_bounds(&members, &next_positions, &geometry) {
                console_log!(
                    "transform ignored: out of bounds (client={} anchor={})",
                    client_id,
                    anchor_id
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            for (id, next_pos) in next_positions {
                if let Some(slot) = snapshot.state.positions.get_mut(id) {
                    *slot = next_pos;
                }
                if let Some(rot) = snapshot.state.rotations.get_mut(id) {
                    *rot = rot_deg;
                }
            }

            let seq = self.bump_seq(&mut snapshot);
            let update = Some(ServerMsg::Update {
                seq,
                update: RoomUpdate::GroupTransform {
                    anchor_id,
                    pos,
                    rot_deg,
                },
                source: Some(client_id),
                client_seq: Some(client_seq).filter(|value| *value != 0),
            });
            inner.snapshot = Some(snapshot);
            update
        };

        self.touch_command(now, false).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some(update) = update {
            console_log!(
                "transform accepted: client={} anchor={} seq={}",
                client_id,
                anchor_id,
                match &update {
                    ServerMsg::Update { seq, .. } => *seq,
                    _ => 0,
                }
            );
            let _ = self.broadcast(&update);
        }
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_flip(&self, client_id: ClientId, piece_id: u32, flipped: bool) -> Result<()> {
        let now = now_ms();
        let updates = {
            let mut inner = self.inner.borrow_mut();
            let mut snapshot = match inner.snapshot.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let geometry = match Self::geometry_for_snapshot(&snapshot) {
                Some(geometry) => geometry,
                None => {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            };
            let total = geometry.cols * geometry.rows;
            if piece_id as usize >= total {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let mut members = collect_group(
                &snapshot.state.connections,
                piece_id as usize,
                geometry.cols,
                geometry.rows,
            );
            if members.is_empty() {
                members.push(piece_id as usize);
            }
            members.sort_unstable();
            let anchor_id = members[0] as u32;
            if members.len() != 1 || anchor_id != piece_id {
                console_log!(
                    "flip rejected: piece in group (client={} piece={} anchor={} size={})",
                    client_id,
                    piece_id,
                    anchor_id,
                    members.len()
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                console_log!(
                    "flip rejected: not owner (client={} piece={} anchor={})",
                    client_id,
                    piece_id,
                    anchor_id
                );
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            if let Some(slot) = snapshot.state.flips.get_mut(piece_id as usize) {
                *slot = flipped;
            } else {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            clear_piece_connections(
                &mut snapshot.state.connections,
                piece_id as usize,
                geometry.cols,
                geometry.rows,
            );
            let anchor_of = anchor_of_from_connections(
                &snapshot.state.connections,
                geometry.cols,
                geometry.rows,
            );
            snapshot.state.group_order =
                reconcile_group_order(&snapshot.state.group_order, &anchor_of);

            inner.owner_by_client.remove(&client_id);
            inner.owners_by_anchor.remove(&anchor_id);

            let seq = self.bump_seq(&mut snapshot);
            let flip_update = ServerMsg::Update {
                seq,
                update: RoomUpdate::Flip { piece_id, flipped },
                source: Some(client_id),
                client_seq: None,
            };
            let ownership_update = ServerMsg::Update {
                seq,
                update: RoomUpdate::Ownership {
                    anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                source: Some(client_id),
                client_seq: None,
            };
            inner.snapshot = Some(snapshot);
            Some((flip_update, ownership_update))
        };

        self.touch_command(now, false).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some((flip_update, ownership_update)) = updates {
            console_log!(
                "flip accepted: client={} piece={} flipped={}",
                client_id,
                piece_id,
                flipped
            );
            let _ = self.broadcast(&flip_update);
            let _ = self.broadcast(&ownership_update);
        }
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_rotate(&self, client_id: ClientId, anchor_id: u32, rot_deg: f32) -> Result<()> {
        self.handle_finalize(client_id, anchor_id, None, Some(rot_deg))
            .await
    }

    async fn handle_place(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: f32,
    ) -> Result<()> {
        self.handle_finalize(client_id, anchor_id, Some(pos), Some(rot_deg))
            .await
    }

    async fn handle_finalize(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        pos: Option<(f32, f32)>,
        rot_deg: Option<f32>,
    ) -> Result<()> {
        let now = now_ms();
        let (snapshot, released_anchor) = {
            let mut inner = self.inner.borrow_mut();
            let mut snapshot = match inner.snapshot.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let geometry = match Self::geometry_for_snapshot(&snapshot) {
                Some(geometry) => geometry,
                None => {
                    inner.snapshot = Some(snapshot);
                    return Ok(());
                }
            };
            let total = geometry.cols * geometry.rows;
            if anchor_id as usize >= total {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }
            let members = collect_group(
                &snapshot.state.connections,
                anchor_id as usize,
                geometry.cols,
                geometry.rows,
            );
            let mut members = members;
            if members.is_empty() {
                members.push(anchor_id as usize);
            }
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                inner.snapshot = Some(snapshot);
                return Ok(());
            }

            let anchor_rot = rot_deg.unwrap_or_else(|| {
                snapshot
                    .state
                    .rotations
                    .get(anchor_id_usize)
                    .copied()
                    .unwrap_or(0.0)
            });
            let anchor_pos = pos.unwrap_or_else(|| {
                snapshot
                    .state
                    .positions
                    .get(anchor_id_usize)
                    .copied()
                    .unwrap_or((0.0, 0.0))
            });

            for &id in &members {
                let (dx, dy) = piece_local_offset(
                    id,
                    anchor_id_usize,
                    geometry.cols,
                    geometry.piece_width,
                    geometry.piece_height,
                );
                let (rx, ry) = rotate_vec(dx, dy, anchor_rot);
                if let Some(slot) = snapshot.state.positions.get_mut(id) {
                    *slot = (anchor_pos.0 + rx, anchor_pos.1 + ry);
                }
                if let Some(slot) = snapshot.state.rotations.get_mut(id) {
                    *slot = anchor_rot;
                }
            }

            let mut positions = snapshot.state.positions.clone();
            let mut rotations = snapshot.state.rotations.clone();
            let mut connections = snapshot.state.connections.clone();
            let group_after = apply_snaps_for_group(
                &members,
                &mut positions,
                &mut rotations,
                &snapshot.state.flips,
                &mut connections,
                geometry.cols,
                geometry.rows,
                geometry.piece_width,
                geometry.piece_height,
                geometry.snap_distance,
                geometry.frame_snap_ratio,
                true,
                geometry.center_min_x,
                geometry.center_max_x,
                geometry.center_min_y,
                geometry.center_max_y,
                geometry.view_min_x,
                geometry.view_min_y,
                geometry.view_width,
                geometry.view_height,
                geometry.rotation_snap_tolerance,
                geometry.rotation_enabled,
            );
            if group_after.is_empty() {
                let fallback_seed =
                    scramble_seed(PUZZLE_SEED, splitmix32(now as u32 ^ anchor_id_usize as u32), geometry.cols, geometry.rows);
                let margin =
                    geometry.piece_width.max(geometry.piece_height) * (DEFAULT_TAB_DEPTH_CAP + MAX_LINE_BEND_RATIO);
                let (fallback_positions, _) = scramble_layout(
                    fallback_seed,
                    geometry.cols,
                    geometry.rows,
                    geometry.piece_width,
                    geometry.piece_height,
                    geometry.view_min_x,
                    geometry.view_min_y,
                    geometry.view_width,
                    geometry.view_height,
                    margin,
                );
                if let Some(fallback_pos) = fallback_positions.get(anchor_id_usize) {
                    if let Some(slot) = positions.get_mut(anchor_id_usize) {
                        *slot = *fallback_pos;
                    }
                    if let Some(slot) = rotations.get_mut(anchor_id_usize) {
                        *slot = anchor_rot;
                    }
                }
                clear_piece_connections(&mut connections, anchor_id_usize, geometry.cols, geometry.rows);
            }
            snapshot.state.positions = positions;
            snapshot.state.rotations = rotations;
            snapshot.state.connections = connections;

            let anchor_of = anchor_of_from_connections(
                &snapshot.state.connections,
                geometry.cols,
                geometry.rows,
            );
            snapshot.state.group_order =
                reconcile_group_order(&snapshot.state.group_order, &anchor_of);

            let released_anchor = inner.owner_by_client.remove(&client_id);
            if let Some(released_anchor) = released_anchor {
                inner.owners_by_anchor.remove(&released_anchor);
            }
            self.bump_seq(&mut snapshot);
            let snapshot_clone = snapshot.clone();
            inner.snapshot = Some(snapshot);
            (snapshot_clone, released_anchor)
        };

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some(anchor_id) = released_anchor {
            let update = ServerMsg::Update {
                seq: snapshot.seq,
                update: RoomUpdate::Ownership {
                    anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                source: Some(client_id),
                client_seq: None,
            };
            let _ = self.broadcast(&update);
        }
        let seq = snapshot.seq;
        self.broadcast(&ServerMsg::State { seq, snapshot })?;
        console_log!(
            "finalize accepted: client={} anchor={} seq={}",
            client_id,
            anchor_id,
            seq
        );
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_release(&self, client_id: ClientId, anchor_id: u32) -> Result<()> {
        let now = now_ms();
        let released = {
            let inner = self.inner.borrow();
            inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false)
        };
        if released {
            self.release_by_client(client_id, OwnershipReason::Released)?;
            self.touch_command(now, false).await?;
            self.persist_snapshot_if_needed().await?;
            self.schedule_alarm().await?;
        }
        Ok(())
    }

    fn group_in_bounds(
        &self,
        members: &[usize],
        positions: &[(usize, (f32, f32))],
        geometry: &RoomGeometry,
    ) -> bool {
        let (bounds_min_x, bounds_max_x, bounds_min_y, bounds_max_y) = if members.len() > 1 {
            let mut min_x = geometry.center_min_x + geometry.piece_width;
            let mut max_x = geometry.center_max_x - geometry.piece_width;
            let mut min_y = geometry.center_min_y + geometry.piece_height;
            let mut max_y = geometry.center_max_y - geometry.piece_height;
            if max_x < min_x {
                let mid = (geometry.center_min_x + geometry.center_max_x) * 0.5;
                min_x = mid;
                max_x = mid;
            }
            if max_y < min_y {
                let mid = (geometry.center_min_y + geometry.center_max_y) * 0.5;
                min_y = mid;
                max_y = mid;
            }
            (min_x, max_x, min_y, max_y)
        } else {
            (
                geometry.center_min_x,
                geometry.center_max_x,
                geometry.center_min_y,
                geometry.center_max_y,
            )
        };
        for &(_id, pos) in positions {
            let center_x = pos.0 + geometry.piece_width * 0.5;
            let center_y = pos.1 + geometry.piece_height * 0.5;
            if center_x >= bounds_min_x
                && center_x <= bounds_max_x
                && center_y >= bounds_min_y
                && center_y <= bounds_max_y
            {
                return true;
            }
        }
        false
    }

    fn release_by_client(&self, client_id: ClientId, reason: OwnershipReason) -> Result<()> {
        if let Some(anchor_id) = self.clear_ownership_for_client(client_id) {
            let seq = self.bump_seq_for_update();
            let msg = ServerMsg::Update {
                seq,
                update: RoomUpdate::Ownership {
                    anchor_id,
                    owner: None,
                    reason,
                },
                source: Some(client_id),
                client_seq: None,
            };
            let _ = self.broadcast(&msg);
        }
        Ok(())
    }

    fn clear_ownership_for_client(&self, client_id: ClientId) -> Option<u32> {
        let mut inner = self.inner.borrow_mut();
        let anchor_id = inner.owner_by_client.remove(&client_id);
        if let Some(anchor_id) = anchor_id {
            inner.owners_by_anchor.remove(&anchor_id);
        }
        anchor_id
    }

    fn release_timeouts(&self, now: i64) -> Result<()> {
        let expired: Vec<Ownership> = {
            let inner = self.inner.borrow();
            inner
                .owners_by_anchor
                .values()
                .copied()
                .filter(|owner| now.saturating_sub(owner.since_ms) >= OWNERSHIP_TIMEOUT_MS)
                .collect()
        };
        for owner in expired {
            {
                let mut inner = self.inner.borrow_mut();
                inner.owners_by_anchor.remove(&owner.anchor_id);
                inner.owner_by_client.remove(&owner.owner_id);
            }
            console_log!(
                "ownership timeout: client={} anchor={}",
                owner.owner_id,
                owner.anchor_id
            );
            let update = RoomUpdate::Ownership {
                anchor_id: owner.anchor_id,
                owner: None,
                reason: OwnershipReason::Timeout,
            };
            let seq = self.bump_seq_for_update();
            let msg = ServerMsg::Update {
                seq,
                update,
                source: None,
                client_seq: None,
            };
            let _ = self.broadcast(&msg);
        }
        Ok(())
    }

    fn bump_seq(&self, snapshot: &mut GameSnapshot) -> u64 {
        snapshot.seq = snapshot.seq.saturating_add(1);
        snapshot.seq
    }

    fn bump_seq_for_update(&self) -> u64 {
        let mut inner = self.inner.borrow_mut();
        if let Some(snapshot) = inner.snapshot.as_mut() {
            snapshot.seq = snapshot.seq.saturating_add(1);
            snapshot.seq
        } else {
            0
        }
    }

    async fn touch_command(&self, now: i64, full_state: bool) -> Result<()> {
        {
            let mut inner = self.inner.borrow_mut();
            inner.meta.last_command_at = Some(now);
            inner.meta.last_warning_at = None;
            if full_state {
                inner.meta.last_full_state_at = Some(now);
            }
        }
        self.persist_meta().await
    }

    async fn persist_meta(&self) -> Result<()> {
        let meta = { self.inner.borrow().meta.clone() };
        if let Some(bytes) = encode(&meta) {
            self.state.storage().put(META_KEY, bytes).await?;
        }
        Ok(())
    }

    async fn persist_snapshot_if_needed(&self) -> Result<()> {
        let (persistence, snapshot) = {
            let inner = self.inner.borrow();
            (inner.meta.persistence, inner.snapshot.clone())
        };

        if matches!(persistence, RoomPersistence::Durable) {
            if let Some(snapshot) = snapshot {
                if let Some(bytes) = encode(&snapshot) {
                    self.state.storage().put(SNAPSHOT_KEY, bytes).await?;
                }
            }
        }
        Ok(())
    }

    async fn update_last_warning_at(&self, when: i64) -> Result<()> {
        {
            let mut inner = self.inner.borrow_mut();
            inner.meta.last_warning_at = Some(when);
        }
        self.persist_meta().await
    }

    async fn schedule_alarm(&self) -> Result<()> {
        let now = now_ms();
        let has_clients = !self.state.get_websockets().is_empty();
        let (last_command_at, last_full_state_at, has_snapshot, ownerships, pending_releases) = {
            let inner = self.inner.borrow();
            (
                inner.meta.last_command_at,
                inner.meta.last_full_state_at,
                inner.snapshot.is_some(),
                inner.owners_by_anchor.values().copied().collect::<Vec<_>>(),
                inner.pending_releases.values().copied().collect::<Vec<_>>(),
            )
        };

        let mut next_at: Option<i64> = None;

        if let Some(last_command_at) = last_command_at {
            let warn_at = last_command_at + INACTIVITY_WARNING_MS;
            let expire_at = last_command_at + INACTIVITY_EXPIRE_MS;
            next_at = Some(match next_at {
                Some(current) => current.min(warn_at),
                None => warn_at,
            });
            next_at = Some(match next_at {
                Some(current) => current.min(expire_at),
                None => expire_at,
            });
        }

        if has_snapshot && has_clients {
            let base = last_full_state_at.unwrap_or(now);
            let full_at = base + FULL_STATE_INTERVAL_MS;
            next_at = Some(match next_at {
                Some(current) => current.min(full_at),
                None => full_at,
            });
        }

        for owner in ownerships {
            let expires_at = owner.since_ms + OWNERSHIP_TIMEOUT_MS;
            next_at = Some(match next_at {
                Some(current) => current.min(expires_at),
                None => expires_at,
            });
        }

        for release_at in pending_releases {
            next_at = Some(match next_at {
                Some(current) => current.min(release_at),
                None => release_at,
            });
        }

        if let Some(next_at) = next_at {
            let offset = (next_at - now).max(0);
            self.state.storage().set_alarm(offset).await?;
        }

        Ok(())
    }

    async fn expire_room(&self) -> Result<()> {
        let (hashes, persistence) = {
            let mut inner = self.inner.borrow_mut();
            let hashes = inner.asset_index.clone();
            inner.meta.activated = false;
            inner.meta.last_command_at = None;
            inner.meta.last_warning_at = None;
            inner.meta.last_full_state_at = None;
            inner.snapshot = None;
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
            inner.pending_releases.clear();
            inner.recent_nonces.clear();
            inner.assets.clear();
            inner.asset_index.clear();
            inner.pending_uploads.clear();
            (hashes, inner.meta.persistence)
        };

        self.persist_meta().await?;
        let _ = self.state.storage().delete(SNAPSHOT_KEY).await;
        if matches!(persistence, RoomPersistence::Durable) && !hashes.is_empty() {
            let _ = self.clear_assets(hashes).await;
        }

        let msg = ServerMsg::Error {
            code: "room_expired".to_string(),
            message: "room expired due to inactivity".to_string(),
        };
        self.broadcast(&msg)?;

        for socket in self.state.get_websockets() {
            let _ = socket.close(None, Some("room expired"));
        }

        Ok(())
    }

    async fn broadcast_full_state(&self) -> Result<()> {
        let snapshot = {
            let mut inner = self.inner.borrow_mut();
            let Some(snapshot) = inner.snapshot.clone() else {
                return Ok(());
            };
            inner.meta.last_full_state_at = Some(now_ms());
            snapshot
        };
        self.persist_meta().await?;
        let seq = snapshot.seq;
        let result = self.broadcast(&ServerMsg::State { seq, snapshot });
        console_log!("full state broadcast seq={}", seq);
        result
    }

    fn broadcast(&self, msg: &ServerMsg) -> Result<()> {
        let Some(bytes) = encode(msg) else {
            return Ok(());
        };
        for socket in self.state.get_websockets() {
            let _ = socket.send_with_bytes(bytes.as_slice());
        }
        Ok(())
    }

    fn send_server_msg(&self, ws: &WebSocket, msg: &ServerMsg) -> Result<()> {
        let Some(bytes) = encode(msg) else {
            return Ok(());
        };
        ws.send_with_bytes(bytes)?;
        Ok(())
    }
}

fn anchor_of_from_connections(connections: &[[bool; 4]], cols: usize, rows: usize) -> Vec<usize> {
    let total = cols * rows;
    let mut anchor_of = vec![0usize; total];
    for group in groups_from_connections(connections, cols, rows) {
        if group.is_empty() {
            continue;
        }
        let anchor = group[0];
        for id in group {
            if id < total {
                anchor_of[id] = anchor;
            }
        }
    }
    anchor_of
}

fn reconcile_group_order(prev: &[u32], anchor_of: &[usize]) -> Vec<u32> {
    let total = anchor_of.len();
    let mut seen = vec![false; total];
    let mut order = Vec::new();
    for &anchor in prev {
        let anchor_idx = anchor as usize;
        if anchor_idx >= total {
            continue;
        }
        let mapped = anchor_of[anchor_idx];
        if mapped < total && !seen[mapped] {
            seen[mapped] = true;
            order.push(mapped as u32);
        }
    }
    for anchor in 0..total {
        if anchor_of[anchor] == anchor && !seen[anchor] {
            order.push(anchor as u32);
        }
    }
    order
}
