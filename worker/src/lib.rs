use std::cell::RefCell;
use std::collections::HashMap;
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use image_pipeline::{AlphaMode, PipelineConfig};
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
    PuzzleImageRef, PuzzleInfo, PuzzleSpec, PuzzleStateSnapshot, RecordedCommand,
    RecordedCommandKind, RecordedCommandOutcome, RoomPersistence, RoomUpdate, ServerMsg,
    ASSET_CHUNK_BYTES, DIR_DOWN, DIR_RIGHT, GAME_SNAPSHOT_VERSION, PRIVATE_ASSET_MAX_BYTES,
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
const ROOM_ID_KEY: &str = "room_id";
const ASSET_STORAGE_CHUNK_BYTES: usize = 256 * 1024;
const DEFAULT_RECORDING_MAX_EVENTS: u32 = 200_000;

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

fn transcode_to_avif(bytes: &[u8]) -> Result<image_pipeline::TranscodeResult, String> {
    let mut config = PipelineConfig::default();
    config.alpha_mode = AlphaMode::Preserve;
    image_pipeline::transcode_to_avif(bytes, config).map_err(|err| err.to_string())
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
    room_id: Option<String>,
    meta: RoomMeta,
    snapshot: Option<GameSnapshot>,
    owners_by_anchor: HashMap<u32, Ownership>,
    owner_by_client: HashMap<ClientId, u32>,
    recent_nonces: HashMap<String, i64>,
    pending_releases: HashMap<ClientId, i64>,
    assets: HashMap<String, StoredAsset>,
    pending_uploads: HashMap<ClientId, PendingUpload>,
}

impl RoomRuntime {
    fn new() -> Self {
        Self {
            loaded: false,
            room_id: None,
            meta: RoomMeta::default(),
            snapshot: None,
            owners_by_anchor: HashMap::new(),
            owner_by_client: HashMap::new(),
            recent_nonces: HashMap::new(),
            pending_releases: HashMap::new(),
            assets: HashMap::new(),
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

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
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

#[derive(Debug, Clone)]
struct ClientCommandRecord {
    kind: RecordedCommandKind,
    piece_id: Option<u32>,
    anchor_id: Option<u32>,
    pos: Option<(f32, f32)>,
    rot_deg: Option<f32>,
    client_seq: Option<u64>,
}

#[derive(Debug, Clone)]
struct CommandHandlingResult {
    outcome: RecordedCommandOutcome,
    reason: Option<String>,
    room_seq: Option<u64>,
}

#[derive(Debug, Clone, Copy)]
struct SnapshotProgress {
    groups: u32,
    largest_group: u32,
    connected_edges: u32,
    total_edges: u32,
    border_done: bool,
    solved: bool,
}

impl CommandHandlingResult {
    fn applied(room_seq: Option<u64>) -> Self {
        Self {
            outcome: RecordedCommandOutcome::Applied,
            reason: None,
            room_seq,
        }
    }

    fn accepted_no_state_change(room_seq: Option<u64>) -> Self {
        Self {
            outcome: RecordedCommandOutcome::AcceptedNoStateChange,
            reason: None,
            room_seq,
        }
    }

    fn ignored(reason: impl Into<String>, room_seq: Option<u64>) -> Self {
        Self {
            outcome: RecordedCommandOutcome::Ignored,
            reason: Some(reason.into()),
            room_seq,
        }
    }

    fn rejected(reason: impl Into<String>, room_seq: Option<u64>) -> Self {
        Self {
            outcome: RecordedCommandOutcome::Rejected,
            reason: Some(reason.into()),
            room_seq,
        }
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreConfig {
    enabled: bool,
    capped: bool,
    max_events: u32,
    dropped_events: u64,
}

impl Default for CommandStoreConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            capped: false,
            max_events: DEFAULT_RECORDING_MAX_EVENTS,
            dropped_events: 0,
        }
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreStatus {
    enabled: bool,
    capped: bool,
    max_events: u32,
    event_count: u64,
    dropped_events: u64,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreSetRequest {
    enabled: bool,
    max_events: Option<u32>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreAppendRequest {
    ts_ms: i64,
    client_id: ClientId,
    kind: RecordedCommandKind,
    piece_id: Option<u32>,
    anchor_id: Option<u32>,
    pos: Option<(f32, f32)>,
    rot_deg: Option<f32>,
    client_seq: Option<u64>,
    room_seq: Option<u64>,
    outcome: RecordedCommandOutcome,
    reason: Option<String>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreAppendResponse {
    accepted: bool,
    capped: bool,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreExportRequest {
    after_id: Option<u64>,
    limit: u32,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct CommandStoreExportResponse {
    rows: Vec<RecordedCommand>,
    next_after_id: Option<u64>,
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
        self.persist_room_id(room_id).await?;
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
                    AdminMsg::RecordingSet { enabled, max_events } => {
                        return self
                            .handle_admin_recording_set(ws, enabled, max_events)
                            .await;
                    }
                    AdminMsg::RecordingStatus => {
                        return self.handle_admin_recording_status(ws).await;
                    }
                    AdminMsg::RecordingExport { after_id, limit } => {
                        return self
                            .handle_admin_recording_export(ws, after_id, limit)
                            .await;
                    }
                    AdminMsg::RecordingClear => {
                        return self.handle_admin_recording_clear(ws).await;
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

        let classify_msg = msg.clone();
        let record = Self::record_from_client_msg(&msg);
        let pre_seq = self.current_snapshot_seq();
        let pre_progress = self.current_snapshot_progress();
        let dispatch = match msg {
            ClientMsg::Init { puzzle, rules, state } => {
                self.handle_init(ws.clone(), puzzle, rules, state).await
            }
            ClientMsg::AssetRequest { hash } => self.handle_asset_request(ws.clone(), hash).await,
            ClientMsg::Select { piece_id } => self.handle_select(client_id, piece_id).await,
            ClientMsg::Move {
                anchor_id,
                pos,
                client_seq,
            } => self.handle_move(client_id, anchor_id, pos, client_seq).await,
            ClientMsg::Transform {
                anchor_id,
                pos,
                rot_deg,
                client_seq,
            } => {
                self.handle_transform(client_id, anchor_id, pos, rot_deg, client_seq)
                    .await
            }
            ClientMsg::Rotate { anchor_id, rot_deg } => {
                self.handle_rotate(client_id, anchor_id, rot_deg).await
            }
            ClientMsg::Place { anchor_id, pos, rot_deg } => {
                self.handle_place(client_id, anchor_id, pos, rot_deg).await
            }
            ClientMsg::Flip { piece_id, flipped } => {
                self.handle_flip(client_id, piece_id, flipped).await
            }
            ClientMsg::Release { anchor_id } => self.handle_release(client_id, anchor_id).await,
            ClientMsg::Ping { nonce } => {
                let response = ServerMsg::Pong { nonce };
                let _ = self.send_server_msg(&ws, &response);
                Ok(())
            }
        };
        let post_seq = self.current_snapshot_seq();
        let post_progress = self.current_snapshot_progress();
        let outcome = match &dispatch {
            Ok(()) => Self::classify_command_outcome(&classify_msg, pre_seq, post_seq),
            Err(err) => CommandHandlingResult {
                outcome: RecordedCommandOutcome::HandlerError,
                reason: Some(err.to_string()),
                room_seq: post_seq.or(pre_seq),
            },
        };
        self.record_client_command(client_id, &record, outcome, pre_progress, post_progress)
            .await?;
        dispatch?;

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

    async fn persist_room_id(&self, room_id: &str) -> Result<()> {
        let should_persist = {
            let mut inner = self.inner.borrow_mut();
            match inner.room_id.as_deref() {
                Some(existing) if existing == room_id => false,
                _ => {
                    inner.room_id = Some(room_id.to_string());
                    true
                }
            }
        };
        if should_persist {
            self.state.storage().put(ROOM_ID_KEY, room_id).await?;
        }
        Ok(())
    }

    async fn ensure_loaded(&self) -> Result<()> {
        let loaded = { self.inner.borrow().loaded };
        if loaded {
            return Ok(());
        }

        let storage = self.state.storage();
        let meta_bytes: Option<Vec<u8>> = storage.get(META_KEY).await?;
        let snapshot_bytes: Option<Vec<u8>> = storage.get(SNAPSHOT_KEY).await?;
        let room_id: Option<String> = storage.get(ROOM_ID_KEY).await?;

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
        if let Some(room_id) = room_id {
            inner.room_id = Some(room_id);
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

    fn current_snapshot_seq(&self) -> Option<u64> {
        self.inner.borrow().snapshot.as_ref().map(|snapshot| snapshot.seq)
    }

    fn current_snapshot_progress(&self) -> Option<SnapshotProgress> {
        let inner = self.inner.borrow();
        let snapshot = inner.snapshot.as_ref()?;
        Self::progress_for_snapshot(snapshot)
    }

    fn progress_for_snapshot(snapshot: &GameSnapshot) -> Option<SnapshotProgress> {
        let cols = snapshot.puzzle.cols as usize;
        let rows = snapshot.puzzle.rows as usize;
        if cols == 0 || rows == 0 {
            return None;
        }
        let total = cols.saturating_mul(rows);
        if snapshot.state.connections.len() != total {
            return None;
        }
        let groups = groups_from_connections(&snapshot.state.connections, cols, rows);
        let group_count = groups.len().max(1) as u32;
        let largest_group = groups
            .iter()
            .map(|group| group.len())
            .max()
            .unwrap_or(0) as u32;
        let total_edges =
            (cols.saturating_sub(1).saturating_mul(rows) + rows.saturating_sub(1).saturating_mul(cols))
                as u32;
        let mut connected_edges = 0u32;
        for row in 0..rows {
            for col in 0..cols {
                let idx = row * cols + col;
                if col + 1 < cols
                    && snapshot
                        .state
                        .connections
                        .get(idx)
                        .map(|conn| conn[DIR_RIGHT])
                        .unwrap_or(false)
                {
                    connected_edges = connected_edges.saturating_add(1);
                }
                if row + 1 < rows
                    && snapshot
                        .state
                        .connections
                        .get(idx)
                        .map(|conn| conn[DIR_DOWN])
                        .unwrap_or(false)
                {
                    connected_edges = connected_edges.saturating_add(1);
                }
            }
        }
        let anchor_of = anchor_of_from_connections(&snapshot.state.connections, cols, rows);
        let mut border_anchor = None::<usize>;
        let mut border_seen = false;
        let mut border_done = true;
        for row in 0..rows {
            for col in 0..cols {
                if row != 0 && col != 0 && row + 1 != rows && col + 1 != cols {
                    continue;
                }
                border_seen = true;
                let idx = row * cols + col;
                let anchor = anchor_of.get(idx).copied().unwrap_or(idx);
                if let Some(existing) = border_anchor {
                    if existing != anchor {
                        border_done = false;
                        break;
                    }
                } else {
                    border_anchor = Some(anchor);
                }
            }
            if !border_done {
                break;
            }
        }
        if !border_seen {
            border_done = false;
        }
        Some(SnapshotProgress {
            groups: group_count,
            largest_group,
            connected_edges,
            total_edges,
            border_done,
            solved: group_count <= 1 && total > 0,
        })
    }

    fn compose_record_reason(
        base_reason: Option<String>,
        pre: Option<SnapshotProgress>,
        post: Option<SnapshotProgress>,
    ) -> Option<String> {
        if pre.is_none() && post.is_none() {
            return base_reason;
        }
        let fallback = pre.or(post)?;
        let before = pre.unwrap_or(fallback);
        let after = post.unwrap_or(fallback);
        let payload = serde_json::json!({
            "reason": base_reason,
            "groups_before": before.groups,
            "groups_after": after.groups,
            "largest_group_before": before.largest_group,
            "largest_group_after": after.largest_group,
            "connected_edges_before": before.connected_edges,
            "connected_edges_after": after.connected_edges,
            "total_edges": before.total_edges.max(after.total_edges),
            "border_done_before": before.border_done,
            "border_done_after": after.border_done,
            "solved_before": before.solved,
            "solved_after": after.solved
        });
        Some(payload.to_string())
    }

    fn record_from_client_msg(msg: &ClientMsg) -> ClientCommandRecord {
        match msg {
            ClientMsg::Init { .. } => ClientCommandRecord {
                kind: RecordedCommandKind::Init,
                piece_id: None,
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::AssetRequest { .. } => ClientCommandRecord {
                kind: RecordedCommandKind::AssetRequest,
                piece_id: None,
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Select { piece_id } => ClientCommandRecord {
                kind: RecordedCommandKind::Select,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Move {
                anchor_id,
                pos,
                client_seq,
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Move,
                piece_id: None,
                anchor_id: Some(*anchor_id),
                pos: Some(*pos),
                rot_deg: None,
                client_seq: Some(*client_seq),
            },
            ClientMsg::Transform {
                anchor_id,
                pos,
                rot_deg,
                client_seq,
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Transform,
                piece_id: None,
                anchor_id: Some(*anchor_id),
                pos: Some(*pos),
                rot_deg: Some(*rot_deg),
                client_seq: Some(*client_seq),
            },
            ClientMsg::Rotate { anchor_id, rot_deg } => ClientCommandRecord {
                kind: RecordedCommandKind::Rotate,
                piece_id: None,
                anchor_id: Some(*anchor_id),
                pos: None,
                rot_deg: Some(*rot_deg),
                client_seq: None,
            },
            ClientMsg::Place {
                anchor_id,
                pos,
                rot_deg,
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Place,
                piece_id: None,
                anchor_id: Some(*anchor_id),
                pos: Some(*pos),
                rot_deg: Some(*rot_deg),
                client_seq: None,
            },
            ClientMsg::Flip { piece_id, .. } => ClientCommandRecord {
                kind: RecordedCommandKind::Flip,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Release { anchor_id } => ClientCommandRecord {
                kind: RecordedCommandKind::Release,
                piece_id: None,
                anchor_id: Some(*anchor_id),
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Ping { .. } => ClientCommandRecord {
                kind: RecordedCommandKind::Ping,
                piece_id: None,
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
        }
    }

    fn classify_command_outcome(
        msg: &ClientMsg,
        pre_seq: Option<u64>,
        post_seq: Option<u64>,
    ) -> CommandHandlingResult {
        let applied = match (pre_seq, post_seq) {
            (Some(before), Some(after)) => after > before,
            (None, Some(_)) => true,
            _ => false,
        };
        if applied {
            return CommandHandlingResult::applied(post_seq);
        }
        let seq = post_seq.or(pre_seq);
        match msg {
            ClientMsg::Ping { .. } | ClientMsg::AssetRequest { .. } => {
                CommandHandlingResult::accepted_no_state_change(seq)
            }
            ClientMsg::Init { .. } => CommandHandlingResult::rejected("init_not_applied", seq),
            ClientMsg::Select { .. }
            | ClientMsg::Move { .. }
            | ClientMsg::Transform { .. }
            | ClientMsg::Rotate { .. }
            | ClientMsg::Place { .. }
            | ClientMsg::Flip { .. }
            | ClientMsg::Release { .. } => {
                CommandHandlingResult::ignored("ignored_or_conflict", seq)
            }
        }
    }

    async fn record_client_command(
        &self,
        client_id: ClientId,
        record: &ClientCommandRecord,
        outcome: CommandHandlingResult,
        pre_progress: Option<SnapshotProgress>,
        post_progress: Option<SnapshotProgress>,
    ) -> Result<()> {
        let reason = Self::compose_record_reason(outcome.reason, pre_progress, post_progress);
        let request = CommandStoreAppendRequest {
            ts_ms: now_ms(),
            client_id,
            kind: record.kind,
            piece_id: record.piece_id,
            anchor_id: record.anchor_id,
            pos: record.pos,
            rot_deg: record.rot_deg,
            client_seq: record.client_seq,
            room_seq: outcome.room_seq,
            outcome: outcome.outcome,
            reason,
        };
        if let Err(message) = self.command_store_append(&request).await {
            console_log!(
                "recording append failed for client {} kind {:?}: {}",
                client_id,
                record.kind,
                message
            );
        }
        Ok(())
    }

    async fn handle_admin_recording_set(
        &self,
        ws: WebSocket,
        enabled: bool,
        max_events: Option<u32>,
    ) -> Result<()> {
        match self.command_store_set(enabled, max_events).await {
            Ok(status) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::RecordingStatus {
                        enabled: status.enabled,
                        capped: status.capped,
                        max_events: status.max_events,
                        event_count: status.event_count,
                        dropped_events: status.dropped_events,
                    },
                );
            }
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "recording_set_failed".to_string(),
                        message,
                    },
                );
            }
        }
        Ok(())
    }

    async fn handle_admin_recording_status(&self, ws: WebSocket) -> Result<()> {
        match self.command_store_status().await {
            Ok(status) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::RecordingStatus {
                        enabled: status.enabled,
                        capped: status.capped,
                        max_events: status.max_events,
                        event_count: status.event_count,
                        dropped_events: status.dropped_events,
                    },
                );
            }
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "recording_status_failed".to_string(),
                        message,
                    },
                );
            }
        }
        Ok(())
    }

    async fn handle_admin_recording_export(
        &self,
        ws: WebSocket,
        after_id: Option<u64>,
        limit: u32,
    ) -> Result<()> {
        match self.command_store_export(after_id, limit).await {
            Ok(response) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::RecordingRows {
                        rows: response.rows,
                        next_after_id: response.next_after_id,
                    },
                );
            }
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "recording_export_failed".to_string(),
                        message,
                    },
                );
            }
        }
        Ok(())
    }

    async fn handle_admin_recording_clear(&self, ws: WebSocket) -> Result<()> {
        match self.command_store_clear().await {
            Ok(()) => {
                let _ = self.send_server_msg(&ws, &ServerMsg::RecordingCleared);
            }
            Err(message) => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "recording_clear_failed".to_string(),
                        message,
                    },
                );
            }
        }
        Ok(())
    }

    fn command_store_stub(&self) -> std::result::Result<Stub, String> {
        let room_id = self
            .inner
            .borrow()
            .room_id
            .clone()
            .ok_or_else(|| "missing room id".to_string())?;
        let namespace = self
            .env
            .durable_object("COMMANDS")
            .map_err(|err| err.to_string())?;
        namespace
            .get_by_name(&room_id)
            .map_err(|err| err.to_string())
    }

    async fn command_store_post(
        &self,
        path: &str,
        body: Option<Vec<u8>>,
    ) -> std::result::Result<Vec<u8>, String> {
        let stub = self.command_store_stub()?;
        let mut init = RequestInit::new();
        init.with_method(Method::Post);
        if let Some(body) = body {
            init.with_body(Some(js_sys::Uint8Array::from(body.as_slice()).into()));
        }
        let req = Request::new_with_init(path, &init).map_err(|err| err.to_string())?;
        let mut resp = stub.fetch_with_request(req).await.map_err(|err| err.to_string())?;
        if !(200..300).contains(&resp.status_code()) {
            let message = resp
                .text()
                .await
                .unwrap_or_else(|_| "command store failed".to_string());
            return Err(message);
        }
        resp.bytes().await.map_err(|err| err.to_string())
    }

    async fn command_store_set(
        &self,
        enabled: bool,
        max_events: Option<u32>,
    ) -> std::result::Result<CommandStoreStatus, String> {
        let req = CommandStoreSetRequest {
            enabled,
            max_events,
        };
        let Some(bytes) = encode(&req) else {
            return Err("failed to encode recording set request".to_string());
        };
        let body = self
            .command_store_post("https://command/config/set", Some(bytes))
            .await?;
        decode::<CommandStoreStatus>(&body)
            .ok_or_else(|| "failed to decode recording set response".to_string())
    }

    async fn command_store_status(&self) -> std::result::Result<CommandStoreStatus, String> {
        let body = self
            .command_store_post("https://command/config/status", None)
            .await?;
        decode::<CommandStoreStatus>(&body)
            .ok_or_else(|| "failed to decode recording status response".to_string())
    }

    async fn command_store_append(
        &self,
        request: &CommandStoreAppendRequest,
    ) -> std::result::Result<CommandStoreAppendResponse, String> {
        let Some(bytes) = encode(request) else {
            return Err("failed to encode command append request".to_string());
        };
        let body = self
            .command_store_post("https://command/events/append", Some(bytes))
            .await?;
        decode::<CommandStoreAppendResponse>(&body)
            .ok_or_else(|| "failed to decode command append response".to_string())
    }

    async fn command_store_export(
        &self,
        after_id: Option<u64>,
        limit: u32,
    ) -> std::result::Result<CommandStoreExportResponse, String> {
        let req = CommandStoreExportRequest {
            after_id,
            limit,
        };
        let Some(bytes) = encode(&req) else {
            return Err("failed to encode command export request".to_string());
        };
        let body = self
            .command_store_post("https://command/events/export", Some(bytes))
            .await?;
        decode::<CommandStoreExportResponse>(&body)
            .ok_or_else(|| "failed to decode command export response".to_string())
    }

    async fn command_store_clear(&self) -> std::result::Result<(), String> {
        let _ = self
            .command_store_post("https://command/events/clear", None)
            .await?;
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
        let mut stored_width = raw_width;
        let mut stored_height = raw_height;
        let stored_bytes = if is_avif(image_type) {
            pending.bytes
        } else {
            match transcode_to_avif(&pending.bytes) {
                Ok(result) => {
                    stored_width = result.width;
                    stored_height = result.height;
                    result.bytes
                }
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
            logical_image_size(stored_width, stored_height, rules.image_max_dimension);
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
        let should_persist = {
            let mut inner = self.inner.borrow_mut();
            inner.assets.insert(hash.to_string(), asset.clone());
            matches!(inner.meta.persistence, RoomPersistence::Durable)
        };
        if should_persist {
            self.asset_store_put(hash, &asset).await?;
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
        let asset = self.asset_store_get(hash).await?;
        if let Some(asset) = asset.as_ref() {
            self.inner
                .borrow_mut()
                .assets
                .insert(hash.to_string(), asset.clone());
            return Ok(Some(asset.clone()));
        }
        Ok(asset)
    }

    fn asset_store_stub(&self) -> std::result::Result<Stub, String> {
        let room_id = self
            .inner
            .borrow()
            .room_id
            .clone()
            .ok_or_else(|| "missing room id".to_string())?;
        let namespace = self
            .env
            .durable_object("ASSETS")
            .map_err(|err| err.to_string())?;
        namespace
            .get_by_name(&room_id)
            .map_err(|err| err.to_string())
    }

    async fn asset_store_put(
        &self,
        hash: &str,
        asset: &StoredAsset,
    ) -> std::result::Result<(), String> {
        let Some(body) = encode(asset) else {
            return Err("failed to encode asset".to_string());
        };
        let stub = self.asset_store_stub()?;
        let mut init = RequestInit::new();
        init.with_method(Method::Post);
        init.with_body(Some(js_sys::Uint8Array::from(body.as_slice()).into()));
        let req = Request::new_with_init(&format!("https://asset/asset/{hash}"), &init)
            .map_err(|err| err.to_string())?;
        let mut resp = stub.fetch_with_request(req).await.map_err(|err| err.to_string())?;
        if !(200..300).contains(&resp.status_code()) {
            let message = resp
                .text()
                .await
                .unwrap_or_else(|_| "asset store failed".to_string());
            return Err(message);
        }
        Ok(())
    }

    async fn asset_store_get(
        &self,
        hash: &str,
    ) -> std::result::Result<Option<StoredAsset>, String> {
        let stub = self.asset_store_stub()?;
        let mut init = RequestInit::new();
        init.with_method(Method::Get);
        let req = Request::new_with_init(&format!("https://asset/asset/{hash}"), &init)
            .map_err(|err| err.to_string())?;
        let mut resp = stub.fetch_with_request(req).await.map_err(|err| err.to_string())?;
        if resp.status_code() == 404 {
            return Ok(None);
        }
        if !(200..300).contains(&resp.status_code()) {
            let message = resp
                .text()
                .await
                .unwrap_or_else(|_| "asset store failed".to_string());
            return Err(message);
        }
        let bytes = resp.bytes().await.map_err(|err| err.to_string())?;
        let Some(asset) = decode::<StoredAsset>(&bytes) else {
            return Err("failed to decode stored asset".to_string());
        };
        Ok(Some(asset))
    }

    async fn clear_assets(&self) -> std::result::Result<(), String> {
        let persistence = { self.inner.borrow().meta.persistence };
        if !matches!(persistence, RoomPersistence::Durable) {
            return Ok(());
        }
        let stub = self.asset_store_stub()?;
        let mut init = RequestInit::new();
        init.with_method(Method::Post);
        let req = Request::new_with_init("https://asset/clear", &init)
            .map_err(|err| err.to_string())?;
        let mut resp = stub.fetch_with_request(req).await.map_err(|err| err.to_string())?;
        if !(200..300).contains(&resp.status_code()) {
            let message = resp
                .text()
                .await
                .unwrap_or_else(|_| "asset store failed".to_string());
            return Err(message);
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
        let persistence = {
            let mut inner = self.inner.borrow_mut();
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
            inner.pending_uploads.clear();
            inner.meta.persistence
        };

        self.persist_meta().await?;
        let _ = self.state.storage().delete(SNAPSHOT_KEY).await;
        if matches!(persistence, RoomPersistence::Durable) {
            let _ = self.clear_assets().await;
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

struct AssetStoreRuntime {
    schema_ready: bool,
}

impl AssetStoreRuntime {
    fn new() -> Self {
        Self { schema_ready: false }
    }
}

enum AssetStoreRoute {
    Asset { hash: String },
    Clear,
}

impl AssetStoreRoute {
    fn from_path(path: &str) -> Option<Self> {
        let trimmed = path.trim_start_matches('/');
        if trimmed == "clear" {
            return Some(Self::Clear);
        }
        let hash = trimmed.strip_prefix("asset/")?;
        if hash.is_empty() {
            return None;
        }
        Some(Self::Asset {
            hash: hash.to_string(),
        })
    }
}

#[durable_object]
pub struct AssetStore {
    state: State,
    inner: RefCell<AssetStoreRuntime>,
}

impl DurableObject for AssetStore {
    fn new(state: State, _env: Env) -> Self {
        Self {
            state,
            inner: RefCell::new(AssetStoreRuntime::new()),
        }
    }

    async fn fetch(&self, mut req: Request) -> Result<Response> {
        self.ensure_schema()?;
        let Some(route) = AssetStoreRoute::from_path(&req.path()) else {
            return Response::error("not found", 404);
        };
        match (req.method(), route) {
            (Method::Post, AssetStoreRoute::Asset { hash }) => {
                let bytes = req.bytes().await?;
                let Some(asset) = decode::<StoredAsset>(&bytes) else {
                    return Response::error("invalid asset payload", 400);
                };
                match self.store_asset_sql(&hash, asset) {
                    Ok(()) => Response::ok("ok"),
                    Err(message) => Response::error(&message, 500),
                }
            }
            (Method::Get, AssetStoreRoute::Asset { hash }) => {
                match self.load_asset_sql(&hash) {
                    Ok(Some(asset)) => {
                        let Some(body) = encode(&asset) else {
                            return Response::error("failed to encode asset", 500);
                        };
                        Response::from_bytes(body)
                    }
                    Ok(None) => Response::error("not found", 404),
                    Err(message) => Response::error(&message, 500),
                }
            }
            (Method::Delete, AssetStoreRoute::Asset { hash }) => {
                match self.delete_asset_sql(&hash) {
                    Ok(()) => Response::ok("ok"),
                    Err(message) => Response::error(&message, 500),
                }
            }
            (Method::Post, AssetStoreRoute::Clear) => match self.clear_assets_sql() {
                Ok(()) => Response::ok("ok"),
                Err(message) => Response::error(&message, 500),
            },
            _ => Response::error("not found", 404),
        }
    }
}

impl AssetStore {
    fn ensure_schema(&self) -> Result<()> {
        let mut inner = self.inner.borrow_mut();
        if inner.schema_ready {
            return Ok(());
        }
        let sql = self.state.storage().sql();
        sql.exec(
            "CREATE TABLE IF NOT EXISTS asset_meta (
                hash TEXT PRIMARY KEY,
                mime TEXT NOT NULL,
                width INTEGER NOT NULL,
                height INTEGER NOT NULL,
                size INTEGER NOT NULL,
                created_at INTEGER NOT NULL,
                chunks INTEGER NOT NULL
            )",
            None,
        )?;
        sql.exec(
            "CREATE TABLE IF NOT EXISTS asset_chunks (
                hash TEXT NOT NULL,
                idx INTEGER NOT NULL,
                bytes BLOB NOT NULL,
                PRIMARY KEY (hash, idx)
            )",
            None,
        )?;
        inner.schema_ready = true;
        Ok(())
    }

    fn store_asset_sql(&self, hash: &str, asset: StoredAsset) -> std::result::Result<(), String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let size = asset.bytes.len() as u32;
        if size != asset.meta.size {
            return Err("asset size mismatch".to_string());
        }
        let expected_chunks = ((size as usize + ASSET_STORAGE_CHUNK_BYTES - 1)
            / ASSET_STORAGE_CHUNK_BYTES) as u32;
        if expected_chunks != asset.meta.chunks {
            return Err("asset chunk count mismatch".to_string());
        }

        let sql = self.state.storage().sql();
        sql.exec(
            "INSERT OR REPLACE INTO asset_meta
                (hash, mime, width, height, size, created_at, chunks)
                VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            Some(vec![
                hash.into(),
                asset.meta.mime.clone().into(),
                (asset.meta.width as i64).into(),
                (asset.meta.height as i64).into(),
                (asset.meta.size as i64).into(),
                asset.meta.created_at.into(),
                (asset.meta.chunks as i64).into(),
            ]),
        )
        .map_err(|err| err.to_string())?;
        sql.exec(
            "DELETE FROM asset_chunks WHERE hash = ?1",
            Some(vec![hash.into()]),
        )
        .map_err(|err| err.to_string())?;
        let mut index = 0u32;
        for chunk in asset.bytes.chunks(ASSET_STORAGE_CHUNK_BYTES) {
            sql.exec(
                "INSERT INTO asset_chunks (hash, idx, bytes) VALUES (?1, ?2, ?3)",
                Some(vec![
                    hash.into(),
                    (index as i64).into(),
                    chunk.to_vec().into(),
                ]),
            )
            .map_err(|err| err.to_string())?;
            index = index.saturating_add(1);
        }
        Ok(())
    }

    fn load_asset_sql(
        &self,
        hash: &str,
    ) -> std::result::Result<Option<StoredAsset>, String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        let mut meta_rows = sql
            .exec(
                "SELECT mime, width, height, size, created_at, chunks
                    FROM asset_meta
                    WHERE hash = ?1",
                Some(vec![hash.into()]),
            )
            .map_err(|err| err.to_string())?
            .raw();
        let Some(meta_row) = meta_rows.next() else {
            return Ok(None);
        };
        let meta_row = meta_row.map_err(|err| err.to_string())?;
        let meta = Self::decode_meta_row(meta_row)?;

        let mut bytes = Vec::with_capacity(meta.size as usize);
        let mut expected_idx = 0u32;
        let chunks = sql
            .exec(
                "SELECT idx, bytes FROM asset_chunks WHERE hash = ?1 ORDER BY idx",
                Some(vec![hash.into()]),
            )
            .map_err(|err| err.to_string())?;
        for row in chunks.raw() {
            let row = row.map_err(|err| err.to_string())?;
            if row.len() != 2 {
                return Err("invalid asset chunk row".to_string());
            }
            let idx = Self::sql_value_u32(&row[0], "chunk index")?;
            if idx != expected_idx {
                return Err("missing asset chunk".to_string());
            }
            let chunk = Self::sql_value_blob(&row[1], "chunk bytes")?;
            bytes.extend_from_slice(&chunk);
            expected_idx = expected_idx.saturating_add(1);
        }
        if expected_idx != meta.chunks {
            return Err("asset chunk count mismatch".to_string());
        }
        if bytes.len() != meta.size as usize {
            return Err("asset size mismatch".to_string());
        }
        Ok(Some(StoredAsset { meta, bytes }))
    }

    fn delete_asset_sql(&self, hash: &str) -> std::result::Result<(), String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        sql.exec(
            "DELETE FROM asset_chunks WHERE hash = ?1",
            Some(vec![hash.into()]),
        )
        .map_err(|err| err.to_string())?;
        sql.exec(
            "DELETE FROM asset_meta WHERE hash = ?1",
            Some(vec![hash.into()]),
        )
        .map_err(|err| err.to_string())?;
        Ok(())
    }

    fn clear_assets_sql(&self) -> std::result::Result<(), String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        sql.exec("DELETE FROM asset_chunks", None)
            .map_err(|err| err.to_string())?;
        sql.exec("DELETE FROM asset_meta", None)
            .map_err(|err| err.to_string())?;
        Ok(())
    }

    fn decode_meta_row(row: Vec<SqlStorageValue>) -> std::result::Result<StoredAssetMeta, String> {
        if row.len() != 6 {
            return Err("invalid asset meta row".to_string());
        }
        let mime = Self::sql_value_string(&row[0], "mime")?;
        let width = Self::sql_value_u32(&row[1], "width")?;
        let height = Self::sql_value_u32(&row[2], "height")?;
        let size = Self::sql_value_u32(&row[3], "size")?;
        let created_at = Self::sql_value_i64(&row[4], "created_at")?;
        let chunks = Self::sql_value_u32(&row[5], "chunks")?;
        Ok(StoredAssetMeta {
            mime,
            width,
            height,
            size,
            created_at,
            chunks,
        })
    }

    fn sql_value_string(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<String, String> {
        match value {
            SqlStorageValue::String(value) => Ok(value.clone()),
            _ => Err(format!("invalid asset {field}")),
        }
    }

    fn sql_value_i64(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<i64, String> {
        match value {
            SqlStorageValue::Integer(value) => Ok(*value),
            SqlStorageValue::Float(value) => Ok(*value as i64),
            _ => Err(format!("invalid asset {field}")),
        }
    }

    fn sql_value_u32(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<u32, String> {
        let raw = Self::sql_value_i64(value, field)?;
        u32::try_from(raw).map_err(|_| format!("invalid asset {field}"))
    }

    fn sql_value_blob(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<Vec<u8>, String> {
        match value {
            SqlStorageValue::Blob(value) => Ok(value.clone()),
            _ => Err(format!("invalid asset {field}")),
        }
    }
}

struct CommandStoreRuntime {
    schema_ready: bool,
}

impl CommandStoreRuntime {
    fn new() -> Self {
        Self { schema_ready: false }
    }
}

enum CommandStoreRoute {
    ConfigSet,
    ConfigStatus,
    EventsAppend,
    EventsExport,
    EventsClear,
}

impl CommandStoreRoute {
    fn from_path(path: &str) -> Option<Self> {
        match path.trim_start_matches('/') {
            "config/set" => Some(Self::ConfigSet),
            "config/status" => Some(Self::ConfigStatus),
            "events/append" => Some(Self::EventsAppend),
            "events/export" => Some(Self::EventsExport),
            "events/clear" => Some(Self::EventsClear),
            _ => None,
        }
    }
}

#[durable_object]
pub struct CommandStore {
    state: State,
    inner: RefCell<CommandStoreRuntime>,
}

impl DurableObject for CommandStore {
    fn new(state: State, _env: Env) -> Self {
        Self {
            state,
            inner: RefCell::new(CommandStoreRuntime::new()),
        }
    }

    async fn fetch(&self, mut req: Request) -> Result<Response> {
        self.ensure_schema()?;
        let Some(route) = CommandStoreRoute::from_path(&req.path()) else {
            return Response::error("not found", 404);
        };
        match (req.method(), route) {
            (Method::Post, CommandStoreRoute::ConfigSet) => {
                let body = req.bytes().await?;
                let Some(msg) = decode::<CommandStoreSetRequest>(&body) else {
                    return Response::error("invalid request", 400);
                };
                let status = match self.set_config(msg.enabled, msg.max_events) {
                    Ok(status) => status,
                    Err(message) => return Response::error(&message, 500),
                };
                let Some(bytes) = encode(&status) else {
                    return Response::error("failed to encode response", 500);
                };
                Response::from_bytes(bytes)
            }
            (Method::Post, CommandStoreRoute::ConfigStatus) => {
                let status = match self.status() {
                    Ok(status) => status,
                    Err(message) => return Response::error(&message, 500),
                };
                let Some(bytes) = encode(&status) else {
                    return Response::error("failed to encode response", 500);
                };
                Response::from_bytes(bytes)
            }
            (Method::Post, CommandStoreRoute::EventsAppend) => {
                let body = req.bytes().await?;
                let Some(msg) = decode::<CommandStoreAppendRequest>(&body) else {
                    return Response::error("invalid request", 400);
                };
                let response = match self.append(msg) {
                    Ok(response) => response,
                    Err(message) => return Response::error(&message, 500),
                };
                let Some(bytes) = encode(&response) else {
                    return Response::error("failed to encode response", 500);
                };
                Response::from_bytes(bytes)
            }
            (Method::Post, CommandStoreRoute::EventsExport) => {
                let body = req.bytes().await?;
                let Some(msg) = decode::<CommandStoreExportRequest>(&body) else {
                    return Response::error("invalid request", 400);
                };
                let export = match self.export(msg.after_id, msg.limit) {
                    Ok(export) => export,
                    Err(message) => return Response::error(&message, 500),
                };
                let Some(bytes) = encode(&export) else {
                    return Response::error("failed to encode response", 500);
                };
                Response::from_bytes(bytes)
            }
            (Method::Post, CommandStoreRoute::EventsClear) => {
                match self.clear_events() {
                    Ok(()) => Response::ok("ok"),
                    Err(message) => Response::error(&message, 500),
                }
            }
            _ => Response::error("not found", 404),
        }
    }
}

impl CommandStore {
    fn ensure_schema(&self) -> Result<()> {
        let mut inner = self.inner.borrow_mut();
        if inner.schema_ready {
            return Ok(());
        }
        let sql = self.state.storage().sql();
        sql.exec(
            "CREATE TABLE IF NOT EXISTS command_config (
                id INTEGER PRIMARY KEY CHECK (id = 1),
                enabled INTEGER NOT NULL,
                capped INTEGER NOT NULL,
                max_events INTEGER NOT NULL,
                dropped_events INTEGER NOT NULL
            )",
            None,
        )?;
        sql.exec(
            "CREATE TABLE IF NOT EXISTS command_events (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                ts_ms INTEGER NOT NULL,
                client_id TEXT NOT NULL,
                kind INTEGER NOT NULL,
                piece_id INTEGER,
                anchor_id INTEGER,
                pos_x REAL,
                pos_y REAL,
                rot_deg REAL,
                client_seq INTEGER,
                room_seq INTEGER,
                outcome INTEGER NOT NULL,
                reason TEXT
            )",
            None,
        )?;
        let existing = sql.exec("SELECT id FROM command_config WHERE id = 1", None)?;
        if existing.raw().next().is_none() {
            sql.exec(
                "INSERT INTO command_config
                    (id, enabled, capped, max_events, dropped_events)
                    VALUES (1, 0, 0, ?1, 0)",
                Some(vec![(DEFAULT_RECORDING_MAX_EVENTS as i64).into()]),
            )?;
        }
        inner.schema_ready = true;
        Ok(())
    }

    fn load_config(&self) -> std::result::Result<CommandStoreConfig, String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        let mut rows = sql
            .exec(
                "SELECT enabled, capped, max_events, dropped_events
                    FROM command_config
                    WHERE id = 1",
                None,
            )
            .map_err(|err| err.to_string())?
            .raw();
        let Some(row) = rows.next() else {
            return Ok(CommandStoreConfig::default());
        };
        let row = row.map_err(|err| err.to_string())?;
        if row.len() != 4 {
            return Err("invalid command config row".to_string());
        }
        let enabled = Self::sql_required_i64(&row[0], "enabled")? != 0;
        let capped = Self::sql_required_i64(&row[1], "capped")? != 0;
        let max_events = Self::sql_required_u32(&row[2], "max_events")?;
        let dropped_events = Self::sql_required_u64(&row[3], "dropped_events")?;
        Ok(CommandStoreConfig {
            enabled,
            capped,
            max_events: max_events.max(1),
            dropped_events,
        })
    }

    fn save_config(&self, config: &CommandStoreConfig) -> std::result::Result<(), String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        sql.exec(
            "INSERT OR REPLACE INTO command_config
                (id, enabled, capped, max_events, dropped_events)
                VALUES (1, ?1, ?2, ?3, ?4)",
            Some(vec![
                if config.enabled { 1i64 } else { 0i64 }.into(),
                if config.capped { 1i64 } else { 0i64 }.into(),
                (config.max_events as i64).into(),
                (config.dropped_events as i64).into(),
            ]),
        )
        .map_err(|err| err.to_string())?;
        Ok(())
    }

    fn event_count(&self) -> std::result::Result<u64, String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        let mut rows = sql
            .exec("SELECT COUNT(1) FROM command_events", None)
            .map_err(|err| err.to_string())?
            .raw();
        let Some(row) = rows.next() else {
            return Ok(0);
        };
        let row = row.map_err(|err| err.to_string())?;
        if row.len() != 1 {
            return Err("invalid command count row".to_string());
        }
        Self::sql_required_u64(&row[0], "event_count")
    }

    fn status(&self) -> std::result::Result<CommandStoreStatus, String> {
        let config = self.load_config()?;
        let event_count = self.event_count()?;
        Ok(CommandStoreStatus {
            enabled: config.enabled,
            capped: config.capped,
            max_events: config.max_events,
            event_count,
            dropped_events: config.dropped_events,
        })
    }

    fn set_config(
        &self,
        enabled: bool,
        max_events: Option<u32>,
    ) -> std::result::Result<CommandStoreStatus, String> {
        let mut config = self.load_config()?;
        if let Some(max_events) = max_events {
            config.max_events = max_events.max(1);
        }
        config.enabled = enabled;
        if enabled {
            config.capped = false;
        }
        self.save_config(&config)?;
        self.status()
    }

    fn append(
        &self,
        request: CommandStoreAppendRequest,
    ) -> std::result::Result<CommandStoreAppendResponse, String> {
        let mut config = self.load_config()?;
        if !config.enabled {
            return Ok(CommandStoreAppendResponse {
                accepted: false,
                capped: config.capped,
            });
        }
        let count = self.event_count()?;
        if count >= config.max_events as u64 {
            config.enabled = false;
            config.capped = true;
            config.dropped_events = config.dropped_events.saturating_add(1);
            self.save_config(&config)?;
            return Ok(CommandStoreAppendResponse {
                accepted: false,
                capped: true,
            });
        }

        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        sql.exec(
            "INSERT INTO command_events
                (ts_ms, client_id, kind, piece_id, anchor_id, pos_x, pos_y, rot_deg, client_seq, room_seq, outcome, reason)
                VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12)",
            Some(vec![
                request.ts_ms.into(),
                format!("u64:{}", request.client_id.as_u64()).into(),
                (request.kind as i64).into(),
                Self::opt_i64(request.piece_id.map(|value| value as i64)),
                Self::opt_i64(request.anchor_id.map(|value| value as i64)),
                Self::opt_f64(request.pos.map(|value| value.0 as f64)),
                Self::opt_f64(request.pos.map(|value| value.1 as f64)),
                Self::opt_f64(request.rot_deg.map(|value| value as f64)),
                Self::opt_i64(request.client_seq.map(|value| value as i64)),
                Self::opt_i64(request.room_seq.map(|value| value as i64)),
                (request.outcome as i64).into(),
                Self::opt_string(request.reason),
            ]),
        )
        .map_err(|err| err.to_string())?;

        Ok(CommandStoreAppendResponse {
            accepted: true,
            capped: false,
        })
    }

    fn export(
        &self,
        after_id: Option<u64>,
        limit: u32,
    ) -> std::result::Result<CommandStoreExportResponse, String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let safe_limit = limit.clamp(1, 5000);
        let after_id = after_id.unwrap_or(0);
        let sql = self.state.storage().sql();
        let rows = sql
            .exec(
                "SELECT id, ts_ms, client_id, kind, piece_id, anchor_id, pos_x, pos_y, rot_deg, client_seq, room_seq, outcome, reason
                    FROM command_events
                    WHERE id > ?1
                    ORDER BY id ASC
                    LIMIT ?2",
                Some(vec![(after_id as i64).into(), (safe_limit as i64).into()]),
            )
            .map_err(|err| err.to_string())?;
        let mut out = Vec::new();
        let mut next_after_id = None;
        for row in rows.raw() {
            let row = row.map_err(|err| err.to_string())?;
            if row.len() != 13 {
                return Err("invalid command event row".to_string());
            }
            let id = Self::sql_required_u64(&row[0], "id")?;
            let ts_ms = Self::sql_required_i64(&row[1], "ts_ms")?;
            let client_id_raw = Self::sql_required_client_id(&row[2])?;
            let kind_raw = Self::sql_required_u32(&row[3], "kind")?;
            let outcome_raw = Self::sql_required_u32(&row[11], "outcome")?;
            let command = RecordedCommand {
                id,
                ts_ms,
                client_id: ClientId::from(client_id_raw),
                kind: Self::decode_kind(kind_raw)?,
                piece_id: Self::sql_optional_u32(&row[4]),
                anchor_id: Self::sql_optional_u32(&row[5]),
                pos: match (Self::sql_optional_f32(&row[6]), Self::sql_optional_f32(&row[7])) {
                    (Some(x), Some(y)) => Some((x, y)),
                    _ => None,
                },
                rot_deg: Self::sql_optional_f32(&row[8]),
                client_seq: Self::sql_optional_u64(&row[9]),
                room_seq: Self::sql_optional_u64(&row[10]),
                outcome: Self::decode_outcome(outcome_raw)?,
                reason: Self::sql_optional_string(&row[12]),
            };
            next_after_id = Some(id);
            out.push(command);
        }
        Ok(CommandStoreExportResponse {
            rows: out,
            next_after_id,
        })
    }

    fn clear_events(&self) -> std::result::Result<(), String> {
        self.ensure_schema().map_err(|err| err.to_string())?;
        let sql = self.state.storage().sql();
        sql.exec("DELETE FROM command_events", None)
            .map_err(|err| err.to_string())?;
        let mut config = self.load_config()?;
        config.capped = false;
        config.dropped_events = 0;
        self.save_config(&config)?;
        Ok(())
    }

    fn decode_kind(value: u32) -> std::result::Result<RecordedCommandKind, String> {
        match value {
            x if x == RecordedCommandKind::Init as u32 => Ok(RecordedCommandKind::Init),
            x if x == RecordedCommandKind::AssetRequest as u32 => {
                Ok(RecordedCommandKind::AssetRequest)
            }
            x if x == RecordedCommandKind::Select as u32 => Ok(RecordedCommandKind::Select),
            x if x == RecordedCommandKind::Move as u32 => Ok(RecordedCommandKind::Move),
            x if x == RecordedCommandKind::Transform as u32 => Ok(RecordedCommandKind::Transform),
            x if x == RecordedCommandKind::Rotate as u32 => Ok(RecordedCommandKind::Rotate),
            x if x == RecordedCommandKind::Place as u32 => Ok(RecordedCommandKind::Place),
            x if x == RecordedCommandKind::Flip as u32 => Ok(RecordedCommandKind::Flip),
            x if x == RecordedCommandKind::Release as u32 => Ok(RecordedCommandKind::Release),
            x if x == RecordedCommandKind::Ping as u32 => Ok(RecordedCommandKind::Ping),
            _ => Err("invalid command kind".to_string()),
        }
    }

    fn decode_outcome(value: u32) -> std::result::Result<RecordedCommandOutcome, String> {
        match value {
            x if x == RecordedCommandOutcome::Applied as u32 => {
                Ok(RecordedCommandOutcome::Applied)
            }
            x if x == RecordedCommandOutcome::AcceptedNoStateChange as u32 => {
                Ok(RecordedCommandOutcome::AcceptedNoStateChange)
            }
            x if x == RecordedCommandOutcome::Ignored as u32 => {
                Ok(RecordedCommandOutcome::Ignored)
            }
            x if x == RecordedCommandOutcome::Rejected as u32 => {
                Ok(RecordedCommandOutcome::Rejected)
            }
            x if x == RecordedCommandOutcome::HandlerError as u32 => {
                Ok(RecordedCommandOutcome::HandlerError)
            }
            _ => Err("invalid command outcome".to_string()),
        }
    }

    fn opt_i64(value: Option<i64>) -> SqlStorageValue {
        match value {
            Some(value) => value.into(),
            None => SqlStorageValue::Null,
        }
    }

    fn opt_f64(value: Option<f64>) -> SqlStorageValue {
        match value {
            Some(value) => value.into(),
            None => SqlStorageValue::Null,
        }
    }

    fn opt_string(value: Option<String>) -> SqlStorageValue {
        match value {
            Some(value) => value.into(),
            None => SqlStorageValue::Null,
        }
    }

    fn sql_required_i64(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<i64, String> {
        match value {
            SqlStorageValue::Integer(value) => Ok(*value),
            SqlStorageValue::Float(value) => Ok(*value as i64),
            _ => Err(format!("invalid command {field}")),
        }
    }

    fn sql_required_u32(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<u32, String> {
        let raw = Self::sql_required_i64(value, field)?;
        u32::try_from(raw).map_err(|_| format!("invalid command {field}"))
    }

    fn sql_required_u64(
        value: &SqlStorageValue,
        field: &str,
    ) -> std::result::Result<u64, String> {
        let raw = Self::sql_required_i64(value, field)?;
        u64::try_from(raw).map_err(|_| format!("invalid command {field}"))
    }

    fn sql_required_client_id(value: &SqlStorageValue) -> std::result::Result<u64, String> {
        match value {
            SqlStorageValue::String(value) => {
                let trimmed = value.trim();
                if let Some(rest) = trimmed.strip_prefix("u64:") {
                    return rest
                        .trim()
                        .parse::<u64>()
                        .map_err(|_| "invalid command client_id".to_string());
                }
                if let Ok(parsed) = trimmed.parse::<u64>() {
                    return Ok(parsed);
                }
                if let Ok(parsed) = trimmed.parse::<i64>() {
                    return Ok(parsed as u64);
                }
                Err("invalid command client_id".to_string())
            }
            SqlStorageValue::Integer(value) => Ok(*value as u64),
            SqlStorageValue::Float(value) => {
                let int = *value as i64;
                if (*value - int as f64).abs() < f64::EPSILON {
                    Ok(int as u64)
                } else {
                    Err("invalid command client_id".to_string())
                }
            }
            _ => Err("invalid command client_id".to_string()),
        }
    }

    fn sql_optional_u32(value: &SqlStorageValue) -> Option<u32> {
        match value {
            SqlStorageValue::Integer(value) => u32::try_from(*value).ok(),
            SqlStorageValue::Float(value) => u32::try_from(*value as i64).ok(),
            _ => None,
        }
    }

    fn sql_optional_u64(value: &SqlStorageValue) -> Option<u64> {
        match value {
            SqlStorageValue::Integer(value) => u64::try_from(*value).ok(),
            SqlStorageValue::Float(value) => u64::try_from(*value as i64).ok(),
            _ => None,
        }
    }

    fn sql_optional_f32(value: &SqlStorageValue) -> Option<f32> {
        match value {
            SqlStorageValue::Integer(value) => Some(*value as f32),
            SqlStorageValue::Float(value) => Some(*value as f32),
            _ => None,
        }
    }

    fn sql_optional_string(value: &SqlStorageValue) -> Option<String> {
        match value {
            SqlStorageValue::String(value) => Some(value.clone()),
            _ => None,
        }
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
