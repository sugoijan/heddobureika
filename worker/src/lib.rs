use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use heddobureika_core::codec::{decode, encode};
use heddobureika_core::game::{
    compute_workspace_layout, piece_bounds_px, rotate_vec, safety_corrections_after_detach,
    scramble_flips, scramble_layout_for_pieces, scramble_nonce_from_seed, scramble_pose,
    scramble_rotations, scramble_seed_from_topology, splitmix32, DEFAULT_TAB_DEPTH_CAP,
    FLIP_CHANCE, MAX_LINE_BEND_RATIO, PUZZLE_SEED,
};
use heddobureika_core::room_id::{is_valid_room_id, ROOM_ID_LEN};
use heddobureika_core::{
    logical_image_size, nearest_valid_grid, puzzle_by_slug, topology_kind_for_tag,
    DEFAULT_TARGET_COUNT, FALLBACK_GRID,
};
use heddobureika_core::{
    validate_image_ref, ActionId, AdminMsg, ClientId, ClientMsg, GameBridgeError, GameRules,
    MergePolicy, OwnershipReason, PlayableGameSnapshot, PlayablePoseSnapshot,
    PlayablePositionSnapshot, PlayableRoomUpdate, PlayableRoomUpdateKind, PlayableTopologySnapshot,
    PlayableUpdateBatch, PuzzleImageRef, PuzzleInfo, PuzzleSpec, RecordedCommand,
    RecordedCommandKind, RecordedCommandOutcome, RoomControlUpdate, RoomPersistence, ServerMsg,
    ASSET_CHUNK_BYTES, PRIVATE_ASSET_MAX_BYTES, PRIVATE_UPLOAD_MAX_BYTES,
};
use heddobureika_game::{
    AngleDeg, FlipState, GroupId, LogicalState, PlayableAction, PlayableState, TopologySpec,
};
use image_pipeline::{AlphaMode, PipelineConfig};
use imagesize::{Compression, ImageType};
use js_sys::Date;
use p256::ecdsa::{signature::Verifier, Signature, VerifyingKey};
use rkyv::{Archive, Deserialize, Serialize};
use serde::Deserialize as SerdeDeserialize;
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::collections::HashMap;
use worker::*;

const DEFAULT_ROOM_PATH_PREFIX: &str = "/ws/";
const META_KEY: &str = "room_meta";
const SNAPSHOT_KEY: &str = "playable_room_snapshot_v5";
const ROOM_ID_KEY: &str = "room_id";
const ASSET_STORAGE_CHUNK_BYTES: usize = 256 * 1024;
const DEFAULT_RECORDING_MAX_EVENTS: u32 = 200_000;

const INACTIVITY_WARNING_MS: i64 = 10 * 60 * 1000;
const INACTIVITY_EXPIRE_MS: i64 = 60 * 60 * 1000;
const FULL_STATE_INTERVAL_MS: i64 = 30 * 1000;
const OWNERSHIP_TIMEOUT_MS: i64 = 30 * 1000;
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

fn grid_dims_for_topology(topology: &PlayableTopologySnapshot) -> Option<(usize, usize)> {
    if topology.tag != "grid" {
        return None;
    }
    let spec: TopologySpec = topology.clone().into();
    let (cols, rows) = heddobureika_core::build_topology_from_spec(&spec)?.dims_hint()?;
    Some((cols as usize, rows as usize))
}

/// Resolve the `TopologySpec` to build for an admin puzzle request.
///
/// When the client supplies a fully resolved topology we use it, but first
/// re-fit it to the room's *actual* image dimensions: aspect-dependent
/// topologies (Voronoi, hexagonal) bake the image aspect into their payload,
/// and the client may have resolved against a stale or approximate size
/// (notably for private uploads, where the server is the first to know the
/// final logical size). `rebuild_for_image` is infallible by contract, so
/// this never loses a buildable spec. When no topology is supplied we keep
/// the legacy behaviour: a grid derived from the requested piece count.
fn resolve_topology_for_spec(
    topology: &Option<PlayableTopologySnapshot>,
    pieces: Option<u32>,
    shape_seed: Option<u32>,
    image_width: u32,
    image_height: u32,
) -> std::result::Result<TopologySpec, String> {
    match topology {
        Some(snapshot) => {
            let requested: TopologySpec = snapshot.clone().into();
            let kind = topology_kind_for_tag(&requested.tag)
                .ok_or_else(|| format!("unknown topology: {}", requested.tag))?;
            // Re-resolve the topology against the room's *actual* image
            // dimensions rather than trusting the client's resolution. The
            // client resolves against the selected catalog art's aspect, which
            // is wrong for a custom uploaded image of a different shape — so we
            // re-fit here using the requested spec's own piece count as the
            // target. The seed keeps seeded layouts (Voronoi) reproducible.
            let target = heddobureika_core::build_topology_from_spec(&requested)
                .map(|topology| topology.piece_count())
                .filter(|count| *count > 0)
                .unwrap_or(DEFAULT_TARGET_COUNT);
            let seed = shape_seed.unwrap_or(0);
            let resolved = (kind.resolve_target)(target, image_width, image_height, seed)
                .map(|choice| choice.spec)
                .unwrap_or(requested);
            // Reject specs we can't build, so the admin sees a clear error
            // instead of a generic "failed to initialize room".
            let built = heddobureika_core::build_topology_from_spec(&resolved)
                .ok_or_else(|| format!("unknown or invalid topology: {}", resolved.tag))?;
            if built.piece_count() == 0 {
                return Err("topology produced no pieces".to_string());
            }
            Ok(resolved)
        }
        None => {
            let target = pieces.unwrap_or(DEFAULT_TARGET_COUNT);
            let grid =
                nearest_valid_grid(image_width, image_height, target).unwrap_or(FALLBACK_GRID);
            Ok(TopologySpec::grid(grid.cols, grid.rows))
        }
    }
}

/// Scramble nonce for a resolved topology. Mirrors the grid-only derivation
/// for grids (so the same numeric seed reproduces the same scramble) and
/// falls back to a topology-agnostic `(0, 0)` salt for everything else —
/// `build_initial_snapshot` then re-salts the nonce with the full spec via
/// `scramble_seed_from_topology`, so distinct topologies still diverge.
fn scramble_override_for_topology(spec: &TopologySpec, seed: Option<u32>) -> Option<u32> {
    let snapshot: PlayableTopologySnapshot = spec.clone().into();
    let (cols, rows) = grid_dims_for_topology(&snapshot).unwrap_or((0, 0));
    seed.map(|seed| scramble_nonce_from_seed(PUZZLE_SEED, seed, cols, rows))
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
        Response::error("server error", 500)
            .unwrap_or_else(|_| Response::error("error", 500).unwrap())
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

/// In-memory live puzzle state for the durable-object worker.
///
/// `playable` is the authoritative live state — every action mutates it
/// directly. `snapshot` is the transport/persistence shadow; we resync
/// `snapshot.state` from `playable` whenever we are about to broadcast a
/// `ServerMsg::State` or persist to KV. Other snapshot fields (`puzzle`,
/// `rules`, `scramble_nonce`, `seq`) are owned by the snapshot and copied
/// or read in place.
struct RoomLivePuzzle {
    snapshot: PlayableGameSnapshot,
    playable: heddobureika_game::GenericPlayableState,
    /// Cached pixel geometry for the bounds clamp. `None` only when the
    /// puzzle is degenerate (zero image size / pieces). Recomputed only here,
    /// since a live puzzle's topology, image size, and rules never change
    /// once set — actions just move pieces.
    geometry: Option<RoomGeometry>,
}

impl RoomLivePuzzle {
    fn from_snapshot(snapshot: PlayableGameSnapshot) -> Option<Self> {
        let playable = snapshot.restore_playable_from_spec().ok()?;
        let geometry = RoomGeometry::from_topology(
            &playable.logical.topology,
            snapshot.puzzle.image_width,
            snapshot.puzzle.image_height,
            snapshot.rules.workspace_padding_ratio,
        );
        Some(Self {
            snapshot,
            playable,
            geometry,
        })
    }

    fn sync_snapshot_from_playable(&mut self) {
        let focused_piece = self.snapshot.state.focused_piece;
        self.snapshot.state = heddobureika_core::PlayableGameStateSnapshot::from_playable(
            &self.playable,
            focused_piece,
        );
        self.snapshot.seq = self.snapshot.state.revision;
    }

    /// Bumps the wire sequence number used for ordering room messages.
    ///
    /// All three of `snapshot.seq`, `state.revision`, and `playable.revision`
    /// are kept identical so that the next action's wire
    /// `revision_before` matches whatever the client has tracked from the
    /// latest control or playable update. Clients must mirror this by
    /// advancing their local `playable.revision` on every control update too.
    fn set_seq(&mut self, seq: u64) {
        self.snapshot.seq = seq;
        self.snapshot.state.revision = seq;
        self.playable.revision = seq;
    }

    /// Drag-start reorder: bring the group containing `anchor` toward the front
    /// to its geometry-aware "fitting depth" (see [`heddobureika_game::z_depth`]).
    /// Deterministic, so it matches the originating client's optimistic result.
    fn bring_forward(&mut self, anchor: u32) -> bool {
        let changed = self.playable.bring_forward_to_fitting_depth(&[anchor]);
        if changed {
            self.sync_snapshot_from_playable();
        }
        changed
    }

    /// Shake reorder: send the group containing `anchor` to its geometry-aware
    /// "fitting depth" (as far back as possible without hiding it). Deterministic.
    fn send_backward(&mut self, anchor: u32) -> bool {
        let changed = self.playable.send_backward_to_fitting_depth(&[anchor]);
        if changed {
            self.sync_snapshot_from_playable();
        }
        changed
    }
}

struct RoomRuntime {
    loaded: bool,
    room_id: Option<String>,
    meta: RoomMeta,
    live: Option<RoomLivePuzzle>,
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
            live: None,
            owners_by_anchor: HashMap::new(),
            owner_by_client: HashMap::new(),
            recent_nonces: HashMap::new(),
            pending_releases: HashMap::new(),
            assets: HashMap::new(),
            pending_uploads: HashMap::new(),
        }
    }
}

fn bridge_action_id(client_id: ClientId, client_seq: Option<u64>, room_seq: u64) -> ActionId {
    let sequence = client_seq.filter(|value| *value != 0).unwrap_or(room_seq);
    let mixed = mix_bridge_action_id(
        client_id.as_u64().wrapping_add(0x9e37_79b9_7f4a_7c15)
            ^ sequence.wrapping_mul(0xbf58_476d_1ce4_e5b9),
    );
    ActionId(if mixed == 0 { 1 } else { mixed })
}

fn mix_bridge_action_id(mut value: u64) -> u64 {
    value ^= value >> 30;
    value = value.wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value ^= value >> 27;
    value = value.wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

fn apply_bridge_live_move(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pos: PlayablePositionSnapshot,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let action = PlayableAction::TranslateGroup {
        group,
        drop_pos: drop_pos.to_position()?,
    };
    let batch = live.playable.apply_action_only(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_live_transform(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pose: PlayablePoseSnapshot,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let pose = drop_pose.to_pose()?;
    let action = PlayableAction::TransformGroupTo {
        group,
        drop_pos: heddobureika_game::Position2 {
            x: pose.x,
            y: pose.y,
        },
        drop_rotation: pose.rotation,
    };
    let batch = live.playable.apply_action_only(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_finalize(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pos: Option<PlayablePositionSnapshot>,
    rot_deg: Option<f32>,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let action = match (drop_pos, rot_deg) {
        (Some(drop_pos), Some(rot_deg)) => {
            let drop_rotation =
                AngleDeg::try_new(rot_deg).ok_or(GameBridgeError::InvalidActionPose)?;
            PlayableAction::TransformGroupTo {
                group,
                drop_pos: drop_pos.to_position()?,
                drop_rotation,
            }
        }
        (Some(drop_pos), None) => PlayableAction::TranslateGroup {
            group,
            drop_pos: drop_pos.to_position()?,
        },
        (None, Some(rot_deg)) => {
            let drop_rotation =
                AngleDeg::try_new(rot_deg).ok_or(GameBridgeError::InvalidActionPose)?;
            PlayableAction::RotateGroupTo {
                group,
                drop_rotation,
            }
        }
        (None, None) => PlayableAction::RotateGroupTo {
            group,
            drop_rotation: live
                .playable
                .pose_of(group)
                .ok_or(GameBridgeError::InvalidPieceId {
                    piece_id: anchor_id,
                })?
                .rotation,
        },
    };
    let batch =
        live.playable
            .apply_action_with_snap(action, Some(action_id), MergePolicy::KeepFixedGroup);
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn playable_group_for_anchor(
    playable: &heddobureika_game::GenericPlayableState,
    anchor_id: u32,
) -> Result<GroupId, GameBridgeError> {
    playable
        .logical
        .group_of(heddobureika_game::PieceId(anchor_id))
        .ok_or(GameBridgeError::InvalidPieceId {
            piece_id: anchor_id,
        })
}

fn apply_bridge_flip(
    live: &mut RoomLivePuzzle,
    piece_id: u32,
    flipped: bool,
    drop_pose: Option<PlayablePoseSnapshot>,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let piece = heddobureika_game::PieceId(piece_id);
    if piece.as_usize() >= live.playable.piece_count() {
        return Err(GameBridgeError::InvalidPieceId { piece_id });
    }
    // The client computes the click-pivoted post-flip pose and sends it, so
    // the authoritative state reproduces that exact pose. Fall back to the
    // current world pose (reflect-about-anchor) when none is supplied.
    let target_pose = match drop_pose.and_then(|pose| pose.to_pose().ok()) {
        Some(pose) => pose,
        None => live
            .playable
            .piece_world_pose(piece)
            .ok_or(GameBridgeError::InvalidPieceId { piece_id })?,
    };
    let target_flip = if flipped {
        heddobureika_game::FlipState::Flipped
    } else {
        heddobureika_game::FlipState::Normal
    };
    let action = heddobureika_game::RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let batch = live
        .playable
        .apply_restricted_action_batch(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_detach(
    live: &mut RoomLivePuzzle,
    piece_id: u32,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let piece = heddobureika_game::PieceId(piece_id);
    if piece.as_usize() >= live.playable.piece_count() {
        return Err(GameBridgeError::InvalidPieceId { piece_id });
    }
    let group = live
        .playable
        .logical
        .group_of(piece)
        .ok_or(GameBridgeError::InvalidPieceId { piece_id })?;
    let target_pose = live
        .playable
        .piece_world_pose(piece)
        .ok_or(GameBridgeError::InvalidPieceId { piece_id })?;
    let target_flip = live.playable.flip_of(group).unwrap_or_default();
    // Capture the original group's members before the detach so we can
    // visit every component the detach may produce and apply safety
    // corrections.
    let original_members: Vec<heddobureika_game::PieceId> =
        live.playable.logical.members_of(group).collect();
    let action = heddobureika_game::RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let mut batch = live
        .playable
        .apply_restricted_action_batch(action, Some(action_id));
    // Force-move each resulting group whose anchor sits outside its applicable
    // safety bound (loose for singletons, tight for multi-piece). The
    // corrections are merged into the detach batch's dirty-group set so the
    // wire update carries the post-correction poses atomically — a client
    // receiving the update sees the final state directly, no intermediate
    // "unsafe" pose flash.
    let puzzle = live.snapshot.puzzle.clone();
    let rules = live.snapshot.rules;
    let placement = heddobureika_core::build_topology_from_spec(&puzzle.to_spec())
        .map(|t| t.image_placement(puzzle.image_width, puzzle.image_height))
        .unwrap_or(heddobureika_game::ImagePlacement {
            pose_unit_px: [1.0, 1.0],
            origin_px: [0.0, 0.0],
            frame_px: [puzzle.image_width as f32, puzzle.image_height as f32],
        });
    let corrections = safety_corrections_after_detach(
        &live.playable,
        &original_members,
        &puzzle,
        &rules,
        placement,
    );
    for (corr_group, drop_pos) in corrections {
        let correction_batch = live.playable.apply_action_only(
            PlayableAction::TranslateGroup {
                group: corr_group,
                drop_pos,
            },
            None,
        );
        for dirty_group in correction_batch.delta.dirty_groups.iter().copied() {
            if !batch.delta.dirty_groups.contains(&dirty_group) {
                batch.delta.dirty_groups.push(dirty_group);
            }
        }
        batch.revision_after = correction_batch.revision_after;
    }
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn encode_stored_snapshot(snapshot: &PlayableGameSnapshot) -> Option<Vec<u8>> {
    encode(snapshot)
}

fn decode_stored_snapshot(bytes: &[u8]) -> Option<PlayableGameSnapshot> {
    decode::<PlayableGameSnapshot>(bytes)
}

fn state_msg_from_snapshot(snapshot: &PlayableGameSnapshot) -> ServerMsg {
    ServerMsg::State {
        seq: snapshot.seq,
        snapshot: snapshot.clone(),
    }
}

fn set_playable_snapshot_seq(snapshot: &mut PlayableGameSnapshot, seq: u64) {
    snapshot.seq = seq;
    snapshot.state.revision = seq;
}

fn playable_update_msg_from_batch(
    snapshot: &PlayableGameSnapshot,
    batch: &PlayableUpdateBatch,
    kind: PlayableRoomUpdateKind,
    source: Option<ClientId>,
    client_seq: Option<u64>,
) -> ServerMsg {
    ServerMsg::PlayableUpdate {
        seq: snapshot.seq,
        update: PlayableRoomUpdate::from_batch_and_state(kind, batch, &snapshot.state),
        source,
        client_seq,
    }
}

fn control_update_msg(
    seq: u64,
    update: RoomControlUpdate,
    source: Option<ClientId>,
    client_seq: Option<u64>,
) -> ServerMsg {
    ServerMsg::ControlUpdate {
        seq,
        update,
        source,
        client_seq,
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
                (inner.meta.persistence, inner.live.is_some())
            };
            let welcome = ServerMsg::Welcome {
                room_id: room_id.to_string(),
                persistence,
                initialized,
                client_id: Some(client_id),
            };
            let _ = self.send_server_msg(&server, &welcome);

            if initialized {
                let snapshot_clone = self
                    .inner
                    .borrow()
                    .live
                    .as_ref()
                    .map(|live| live.snapshot.clone());
                if let Some(snapshot) = snapshot_clone {
                    let msg = state_msg_from_snapshot(&snapshot);
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
                    AdminMsg::Create {
                        persistence,
                        puzzle,
                    } => {
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
                    AdminMsg::UploadPrivateEnd {
                        pieces,
                        seed,
                        topology,
                        shape_seed,
                    } => {
                        return self
                            .handle_admin_upload_end(ws, pieces, seed, topology, shape_seed)
                            .await;
                    }
                    AdminMsg::Scramble { seed } => {
                        return self.handle_admin_scramble(ws, seed).await;
                    }
                    AdminMsg::Solve => {
                        return self.handle_admin_solve(ws).await;
                    }
                    AdminMsg::RecordingSet {
                        enabled,
                        max_events,
                    } => {
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
            ClientMsg::Init { snapshot } => self.handle_init(ws.clone(), snapshot).await,
            ClientMsg::AssetRequest { hash } => self.handle_asset_request(ws.clone(), hash).await,
            ClientMsg::Select { piece_id } => self.handle_select(client_id, piece_id).await,
            ClientMsg::Move {
                piece_id,
                drop_pos,
                client_seq,
                base_revision: _,
            } => {
                self.handle_move(client_id, piece_id, drop_pos, client_seq)
                    .await
            }
            ClientMsg::Transform {
                piece_id,
                drop_pose,
                client_seq,
                base_revision: _,
            } => {
                self.handle_transform(client_id, piece_id, drop_pose, client_seq)
                    .await
            }
            ClientMsg::Rotate {
                piece_id,
                drop_rotation_deg,
                base_revision: _,
            } => {
                self.handle_rotate(client_id, piece_id, drop_rotation_deg)
                    .await
            }
            ClientMsg::Place {
                piece_id,
                drop_pose,
                client_seq,
                base_revision: _,
            } => {
                self.handle_place(client_id, piece_id, drop_pose, client_seq)
                    .await
            }
            ClientMsg::Flip {
                piece_id,
                flipped,
                drop_pose,
                base_revision: _,
            } => {
                self.handle_flip(client_id, piece_id, flipped, drop_pose)
                    .await
            }
            ClientMsg::Detach {
                piece_id,
                base_revision: _,
            } => self.handle_detach(client_id, piece_id).await,
            ClientMsg::Release { piece_id } => self.handle_release(client_id, piece_id).await,
            ClientMsg::SendToBack { piece_id } => {
                self.handle_send_to_back(client_id, piece_id).await
            }
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
                inner.live.is_some(),
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

/// Topology-agnostic pixel geometry for the authoritative bounds clamp.
///
/// Derived from the topology's [`ImagePlacement`] — the SAME mapping the client
/// renderer uses — so the server and client never disagree on where pieces sit.
/// A pose-mm point `p` maps to the pixel `origin + p * pose_unit`. `pose_unit`
/// is per-cell pixel size for stretch-to-fill topologies (grid/hex/voronoi) and
/// a uniform scale for cropping ones (triangular); `origin` is the frame's
/// top-left (non-zero only when letterboxed). `piece_half_*` hold each piece's
/// half bounding-box in pixels, and `view_*` is the workspace rect in pixels.
///
/// Built once per puzzle (cached on `RoomLivePuzzle`) — actions only move
/// pieces, never change the topology, image size, or rules it derives from.
struct RoomGeometry {
    pose_unit_x: f32,
    pose_unit_y: f32,
    origin_x: f32,
    origin_y: f32,
    view_min_x: f32,
    view_max_x: f32,
    view_min_y: f32,
    view_max_y: f32,
    piece_half_w: Vec<f32>,
    piece_half_h: Vec<f32>,
}

impl RoomGeometry {
    fn from_topology(
        topology: &heddobureika_game::GenericTopology,
        image_width: u32,
        image_height: u32,
        workspace_padding_ratio: f32,
    ) -> Option<Self> {
        if image_width == 0 || image_height == 0 {
            return None;
        }
        let total = topology.piece_count() as usize;
        if total == 0 {
            return None;
        }
        let placement = topology.image_placement(image_width, image_height);
        let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
        let [origin_x, origin_y] = placement.origin_px;
        let [frame_w, frame_h] = placement.frame_px;
        if pose_unit_x <= 0.0 || pose_unit_y <= 0.0 || frame_w <= 0.0 || frame_h <= 0.0 {
            return None;
        }
        let layout = compute_workspace_layout(
            origin_x,
            origin_y,
            frame_w,
            frame_h,
            workspace_padding_ratio,
        );
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let view_min_x = layout.view_min_x / puzzle_scale;
        let view_min_y = layout.view_min_y / puzzle_scale;
        let view_max_x = view_min_x + layout.view_width / puzzle_scale;
        let view_max_y = view_min_y + layout.view_height / puzzle_scale;
        let mut piece_half_w = Vec::with_capacity(total);
        let mut piece_half_h = Vec::with_capacity(total);
        for idx in 0..total as u32 {
            let (ex, ey) = topology.piece_extent_mm(heddobureika_game::PieceId(idx));
            piece_half_w.push(ex.as_mm_f32() * pose_unit_x * 0.5);
            piece_half_h.push(ey.as_mm_f32() * pose_unit_y * 0.5);
        }
        Some(Self {
            pose_unit_x,
            pose_unit_y,
            origin_x,
            origin_y,
            view_min_x,
            view_max_x,
            view_min_y,
            view_max_y,
            piece_half_w,
            piece_half_h,
        })
    }
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
            if let Some(snapshot) = decode_stored_snapshot(&bytes) {
                inner.live = RoomLivePuzzle::from_snapshot(snapshot);
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
        let payload: AuthPayload = serde_json::from_slice(&payload_bytes)
            .map_err(|_| error_response("invalid auth", 401))?;
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
        let valid = verify_signature(&pubkey_bytes, &message, &sig_bytes)
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
        self.inner
            .borrow()
            .live
            .as_ref()
            .map(|live| live.snapshot.seq)
    }

    fn current_snapshot_progress(&self) -> Option<SnapshotProgress> {
        let inner = self.inner.borrow();
        let live = inner.live.as_ref()?;
        Self::progress_for_snapshot(&live.snapshot)
    }

    fn progress_for_snapshot(snapshot: &PlayableGameSnapshot) -> Option<SnapshotProgress> {
        let total = snapshot.state.topology_piece_count as usize;
        if total == 0 || snapshot.state.piece_group.len() != total {
            return None;
        }

        let mut group_sizes = HashMap::<u32, u32>::new();
        for group in snapshot.state.piece_group.iter().copied() {
            *group_sizes.entry(group).or_insert(0) += 1;
        }
        let group_count = group_sizes.len().max(1) as u32;
        let largest_group = group_sizes.values().copied().max().unwrap_or(0);
        let connected_edges = snapshot
            .state
            .edge_active
            .iter()
            .filter(|active| **active)
            .count()
            .min(u32::MAX as usize) as u32;
        let total_edges = snapshot.state.topology_edge_count;

        let border_done = grid_dims_for_topology(&snapshot.state.topology)
            .map(|(cols, rows)| Self::grid_border_done_from_playable(snapshot, cols, rows))
            .unwrap_or(false);

        Some(SnapshotProgress {
            groups: group_count,
            largest_group,
            connected_edges,
            total_edges,
            border_done,
            solved: group_count <= 1,
        })
    }

    fn grid_border_done_from_playable(
        snapshot: &PlayableGameSnapshot,
        cols: usize,
        rows: usize,
    ) -> bool {
        if cols == 0 || rows == 0 {
            return false;
        }
        let total = cols.saturating_mul(rows);
        if snapshot.state.piece_group.len() != total {
            return false;
        }
        let mut border_group = None::<u32>;
        let mut border_seen = false;
        for row in 0..rows {
            for col in 0..cols {
                if row != 0 && col != 0 && row + 1 != rows && col + 1 != cols {
                    continue;
                }
                border_seen = true;
                let idx = row * cols + col;
                let Some(group) = snapshot.state.piece_group.get(idx).copied() else {
                    return false;
                };
                if let Some(existing) = border_group {
                    if existing != group {
                        return false;
                    }
                } else {
                    border_group = Some(group);
                }
            }
        }
        border_seen
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
                piece_id,
                drop_pos,
                client_seq,
                ..
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Move,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: Some((drop_pos.x_mm, drop_pos.y_mm)),
                rot_deg: None,
                client_seq: Some(*client_seq),
            },
            ClientMsg::Transform {
                piece_id,
                drop_pose,
                client_seq,
                ..
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Transform,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: Some((drop_pose.x_mm, drop_pose.y_mm)),
                rot_deg: Some(drop_pose.rotation_deg),
                client_seq: Some(*client_seq),
            },
            ClientMsg::Rotate {
                piece_id,
                drop_rotation_deg,
                ..
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Rotate,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: Some(*drop_rotation_deg),
                client_seq: None,
            },
            ClientMsg::Place {
                piece_id,
                drop_pose,
                client_seq,
                ..
            } => ClientCommandRecord {
                kind: RecordedCommandKind::Place,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: Some((drop_pose.x_mm, drop_pose.y_mm)),
                rot_deg: Some(drop_pose.rotation_deg),
                client_seq: Some(*client_seq),
            },
            ClientMsg::Flip { piece_id, .. } => ClientCommandRecord {
                kind: RecordedCommandKind::Flip,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Detach { piece_id, .. } => ClientCommandRecord {
                kind: RecordedCommandKind::Detach,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::Release { piece_id } => ClientCommandRecord {
                kind: RecordedCommandKind::Release,
                piece_id: Some(*piece_id),
                anchor_id: None,
                pos: None,
                rot_deg: None,
                client_seq: None,
            },
            ClientMsg::SendToBack { piece_id } => ClientCommandRecord {
                kind: RecordedCommandKind::SendToBack,
                piece_id: Some(*piece_id),
                anchor_id: None,
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
            | ClientMsg::Detach { .. }
            | ClientMsg::Release { .. }
            | ClientMsg::SendToBack { .. } => {
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
        let mut resp = stub
            .fetch_with_request(req)
            .await
            .map_err(|err| err.to_string())?;
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
        let req = CommandStoreExportRequest { after_id, limit };
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

    fn build_puzzle_from_spec(
        &self,
        spec: PuzzleSpec,
        rules: &GameRules,
    ) -> Result<(PuzzleInfo, Option<u32>), String> {
        validate_image_ref(&spec.image_ref)?;
        let (label, image_ref, image_width, image_height) = match &spec.image_ref {
            PuzzleImageRef::BuiltIn { slug } => {
                let entry =
                    puzzle_by_slug(slug).ok_or_else(|| format!("unknown puzzle: {slug}"))?;
                let (width, height) =
                    logical_image_size(entry.width, entry.height, rules.image_max_dimension);
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
        let topology_spec = resolve_topology_for_spec(
            &spec.topology,
            spec.pieces,
            spec.shape_seed,
            image_width,
            image_height,
        )?;
        let scramble_override = scramble_override_for_topology(&topology_spec, spec.seed);
        let puzzle = PuzzleInfo {
            label,
            image_ref,
            topology: topology_spec.into(),
            shape_seed: spec.shape_seed.unwrap_or(PUZZLE_SEED),
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
        let snapshot = match self.build_initial_snapshot(puzzle, rules, scramble_override) {
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
            inner.live = RoomLivePuzzle::from_snapshot(snapshot);
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

    async fn handle_admin_change_puzzle(&self, ws: WebSocket, puzzle: PuzzleSpec) -> Result<()> {
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
                .live
                .as_ref()
                .map(|live| live.snapshot.rules.clone())
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
        let mut snapshot = match self.build_initial_snapshot(puzzle, rules, scramble_override) {
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
                .live
                .as_ref()
                .map(|live| live.snapshot.seq.saturating_add(1))
                .unwrap_or(0);
            set_playable_snapshot_seq(&mut snapshot, next_seq);
            inner.live = RoomLivePuzzle::from_snapshot(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&state_msg_from_snapshot(&snapshot))?;
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

    async fn handle_admin_upload_chunk(&self, ws: WebSocket, bytes: Vec<u8>) -> Result<()> {
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
        topology: Option<PlayableTopologySnapshot>,
        shape_seed: Option<u32>,
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
                .live
                .as_ref()
                .map(|live| live.snapshot.rules.clone())
                .unwrap_or_default()
        };
        let (image_width, image_height) =
            logical_image_size(stored_width, stored_height, rules.image_max_dimension);
        let hash = sha256_hex(&stored_bytes);
        let size = stored_bytes.len() as u32;
        let chunks =
            ((size as usize + ASSET_STORAGE_CHUNK_BYTES - 1) / ASSET_STORAGE_CHUNK_BYTES) as u32;
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
        let topology_spec = match resolve_topology_for_spec(
            &topology,
            pieces,
            shape_seed,
            image_width,
            image_height,
        ) {
            Ok(spec) => spec,
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
        let scramble_override = scramble_override_for_topology(&topology_spec, seed);
        let puzzle = PuzzleInfo {
            label: String::new(),
            image_ref: PuzzleImageRef::Private { hash: hash.clone() },
            topology: topology_spec.into(),
            shape_seed: shape_seed.unwrap_or(PUZZLE_SEED),
            image_width,
            image_height,
        };
        let mut snapshot = match self.build_initial_snapshot(puzzle, rules, scramble_override) {
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
                .live
                .as_ref()
                .map(|live| live.snapshot.seq.saturating_add(1))
                .unwrap_or(0);
            set_playable_snapshot_seq(&mut snapshot, next_seq);
            inner.live = RoomLivePuzzle::from_snapshot(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        let _ = self.send_server_msg(&ws, &ServerMsg::UploadAck { hash: hash.clone() });
        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&state_msg_from_snapshot(&snapshot))?;
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
            let Some(snapshot) = inner.live.as_ref() else {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "uninitialized".to_string(),
                        message: "room not initialized".to_string(),
                    },
                );
                return Ok(());
            };
            (
                snapshot.snapshot.puzzle.clone(),
                snapshot.snapshot.rules.clone(),
            )
        };
        // Scramble nonce is salted with the topology so the same numeric
        // seed yields different scrambles for different topologies. Grid
        // gives us `(cols, rows)`; other topologies fall back to a salt
        // derived from piece count and topology tag bytes.
        let (cols, rows) = grid_dims_for_topology(&puzzle.topology).unwrap_or((0, 0));
        let scramble_override =
            seed.map(|seed| scramble_nonce_from_seed(PUZZLE_SEED, seed, cols, rows));
        let mut snapshot = match self.build_initial_snapshot(puzzle, rules, scramble_override) {
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
                .live
                .as_ref()
                .map(|live| live.snapshot.seq.saturating_add(1))
                .unwrap_or(0);
            set_playable_snapshot_seq(&mut snapshot, next_seq);
            inner.live = RoomLivePuzzle::from_snapshot(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&state_msg_from_snapshot(&snapshot))?;
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_admin_solve(&self, ws: WebSocket) -> Result<()> {
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
            let Some(snapshot) = inner.live.as_ref() else {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "uninitialized".to_string(),
                        message: "room not initialized".to_string(),
                    },
                );
                return Ok(());
            };
            (
                snapshot.snapshot.puzzle.clone(),
                snapshot.snapshot.rules.clone(),
            )
        };
        let mut snapshot = match Self::build_solved_snapshot(puzzle, rules) {
            Some(snapshot) => snapshot,
            None => {
                let _ = self.send_server_msg(
                    &ws,
                    &ServerMsg::Error {
                        code: "invalid_init".to_string(),
                        message: "failed to solve room".to_string(),
                    },
                );
                return Ok(());
            }
        };
        {
            let mut inner = self.inner.borrow_mut();
            let next_seq = inner
                .live
                .as_ref()
                .map(|live| live.snapshot.seq.saturating_add(1))
                .unwrap_or(0);
            set_playable_snapshot_seq(&mut snapshot, next_seq);
            inner.live = RoomLivePuzzle::from_snapshot(snapshot.clone());
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&state_msg_from_snapshot(&snapshot))?;
        self.schedule_alarm().await?;
        Ok(())
    }

    /// Builds a fully-solved snapshot (every piece in one connected group at
    /// its canonical pose) for the given puzzle. Topology-agnostic.
    fn build_solved_snapshot(puzzle: PuzzleInfo, rules: GameRules) -> Option<PlayableGameSnapshot> {
        let topology = heddobureika_core::build_topology_from_spec(&puzzle.to_spec())?;
        let play_rules = rules.to_play_rules().ok()?;
        let playable = PlayableState::solved(topology, play_rules);
        playable.validate().ok()?;
        Some(PlayableGameSnapshot::from_playable(
            puzzle, rules, 0, &playable, None,
        ))
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

    async fn store_asset(&self, hash: &str, asset: StoredAsset) -> std::result::Result<(), String> {
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

    async fn load_asset(&self, hash: &str) -> std::result::Result<Option<StoredAsset>, String> {
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
        let mut resp = stub
            .fetch_with_request(req)
            .await
            .map_err(|err| err.to_string())?;
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
        let mut resp = stub
            .fetch_with_request(req)
            .await
            .map_err(|err| err.to_string())?;
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
        let req =
            Request::new_with_init("https://asset/clear", &init).map_err(|err| err.to_string())?;
        let mut resp = stub
            .fetch_with_request(req)
            .await
            .map_err(|err| err.to_string())?;
        if !(200..300).contains(&resp.status_code()) {
            let message = resp
                .text()
                .await
                .unwrap_or_else(|_| "asset store failed".to_string());
            return Err(message);
        }
        Ok(())
    }

    async fn handle_init(&self, ws: WebSocket, mut snapshot: PlayableGameSnapshot) -> Result<()> {
        let now = now_ms();
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
            if inner.live.is_some() {
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

        if let Err(message) = validate_image_ref(&snapshot.puzzle.image_ref) {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "invalid_puzzle".to_string(),
                    message,
                },
            );
            return Ok(());
        }

        if snapshot.puzzle.image_width == 0
            || snapshot.puzzle.image_height == 0
            || snapshot.puzzle.piece_count() == 0
            || snapshot.restore_playable_from_spec().is_err()
        {
            let _ = self.send_server_msg(
                &ws,
                &ServerMsg::Error {
                    code: "invalid_init".to_string(),
                    message: "invalid playable snapshot".to_string(),
                },
            );
            return Ok(());
        }
        snapshot.seq = snapshot.state.revision;

        let stored_snapshot = snapshot.clone();
        {
            let mut inner = self.inner.borrow_mut();
            inner.live = RoomLivePuzzle::from_snapshot(stored_snapshot);
            inner.owners_by_anchor.clear();
            inner.owner_by_client.clear();
        }

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        self.broadcast(&state_msg_from_snapshot(&snapshot))?;
        self.schedule_alarm().await?;
        Ok(())
    }

    fn build_initial_snapshot(
        &self,
        puzzle: PuzzleInfo,
        rules: GameRules,
        scramble_override: Option<u32>,
    ) -> Option<PlayableGameSnapshot> {
        if puzzle.image_width == 0 || puzzle.image_height == 0 {
            return None;
        }
        // Topology-agnostic snapshot construction. `image_placement` is the
        // SAME pose→pixel mapping the client renderer uses (stretch-to-fill for
        // grid/hex/voronoi, uniform-scale + centred frame for triangular), so
        // scattered poses render in exactly the spot the worker intends.
        let topology = heddobureika_core::build_topology_from_spec(&puzzle.to_spec())?;
        let total = topology.piece_count() as usize;
        if total == 0 {
            return None;
        }
        let placement = topology.image_placement(puzzle.image_width, puzzle.image_height);
        let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
        let [origin_x, origin_y] = placement.origin_px;
        let [frame_w, frame_h] = placement.frame_px;
        if pose_unit_x <= 0.0 || pose_unit_y <= 0.0 || frame_w <= 0.0 || frame_h <= 0.0 {
            return None;
        }
        let layout = compute_workspace_layout(
            origin_x,
            origin_y,
            frame_w,
            frame_h,
            rules.workspace_padding_ratio,
        );
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
        let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
        let puzzle_view_width = layout.view_width / puzzle_scale;
        let puzzle_view_height = layout.view_height / puzzle_scale;
        let margin = pose_unit_x.max(pose_unit_y) * (DEFAULT_TAB_DEPTH_CAP + MAX_LINE_BEND_RATIO);

        let scramble_nonce = match scramble_override {
            Some(value) => value,
            None => {
                let now_seed = splitmix32(now_ms() as u32 ^ splitmix32(total as u32));
                splitmix32(now_seed ^ 0xA5A5_55AA)
            }
        };
        let seed = scramble_seed_from_topology(PUZZLE_SEED, scramble_nonce, &puzzle.to_spec());
        let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
        let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
        // Per-piece bounding boxes from the topology's pose-unit extents (same
        // source as the authoritative bounds clamp). The worker has no shaped
        // render geometry, so the anchor is approximated at the bbox centre —
        // a sub-piece offset that keeps scattered pieces inside the padded view.
        let piece_bounds = piece_bounds_px(&topology, placement);
        let (positions, order) = scramble_layout_for_pieces(
            seed,
            &piece_bounds,
            puzzle_view_min_x,
            puzzle_view_min_y,
            puzzle_view_width,
            puzzle_view_height,
            margin,
        );
        let rotations = scramble_rotations(rotation_seed, total, rules.rotation_enabled);
        let flips = scramble_flips(flip_seed, total, FLIP_CHANCE);
        let play_rules = rules.to_play_rules().ok()?;
        let mut playable = PlayableState::new(LogicalState::new(topology), play_rules);
        // Non-square pose units (stretch-to-fill topologies) feed rotation math.
        if pose_unit_x > 0.0 && pose_unit_y > 0.0 {
            playable.set_piece_aspect_ratio(pose_unit_y / pose_unit_x);
        }
        for idx in 0..total {
            let (x, y) = *positions.get(idx)?;
            let rotation = *rotations.get(idx)?;
            // Shared topology-driven mapping — identical to the client's scramble.
            let pose = scramble_pose(
                &playable.logical.topology,
                placement,
                heddobureika_game::PieceId(idx as u32),
                (x, y),
                rotation,
            )?;
            if let Some(group_pose) = playable.group_pose.get_mut(idx) {
                *group_pose = pose;
            }
            if let Some(group_flip) = playable.group_flip.get_mut(idx) {
                *group_flip = if flips.get(idx).copied().unwrap_or(false) {
                    FlipState::Flipped
                } else {
                    FlipState::Normal
                };
            }
        }
        playable.z_order = order
            .into_iter()
            .filter(|id| *id < total)
            .map(|id| GroupId(id as u32))
            .collect();
        if playable.z_order.len() != total {
            return None;
        }
        for slot in playable.z_index_of.iter_mut() {
            *slot = u32::MAX;
        }
        for (idx, group) in playable.z_order.iter().copied().enumerate() {
            if let Some(slot) = playable.z_index_of.get_mut(group.as_usize()) {
                *slot = idx as u32;
            }
        }
        playable.validate().ok()?;
        Some(PlayableGameSnapshot::from_playable(
            puzzle,
            rules,
            scramble_nonce,
            &playable,
            None,
        ))
    }

    async fn handle_select(&self, client_id: ClientId, piece_id: u32) -> Result<()> {
        let now = now_ms();
        let (pending_updates, update_msg, group_order_update) = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;

            if let Some(existing) = inner.owners_by_anchor.get(&anchor_id) {
                if existing.owner_id != client_id {
                    inner.live = Some(runtime_snapshot);
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
                    let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
                    runtime_snapshot.set_seq(seq);
                    pending_updates.push(control_update_msg(
                        seq,
                        RoomControlUpdate::Ownership {
                            group_anchor: prev_anchor,
                            owner: None,
                            reason: OwnershipReason::AutoRelease,
                        },
                        Some(client_id),
                        None,
                    ));
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

            // Bring the selected group toward the front to its geometry-aware
            // fitting depth (matches the client's optimistic reorder), then
            // broadcast the resulting full order.
            let _ = runtime_snapshot.bring_forward(anchor_id);
            let group_order = playable_group_order_anchors(&runtime_snapshot.snapshot);

            let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
            runtime_snapshot.set_seq(seq);
            let update_msg = control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: Some(client_id),
                    reason: OwnershipReason::Granted,
                },
                Some(client_id),
                None,
            );
            let group_order_update = control_update_msg(
                seq,
                RoomControlUpdate::GroupOrder { order: group_order },
                Some(client_id),
                None,
            );

            inner.live = Some(runtime_snapshot);
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

    /// Shake-to-back: demote the owner's group to the bottom of the z-stack and
    /// broadcast the resulting order. Only the group's current owner may
    /// reorder it; a no-op order change is silently ignored.
    async fn handle_send_to_back(&self, client_id: ClientId, piece_id: u32) -> Result<()> {
        let now = now_ms();
        let group_order_update = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            // Send the group to its geometry-aware fitting depth (matches the
            // client's optimistic reorder); skip the broadcast if nothing moved.
            if !runtime_snapshot.send_backward(anchor_id) {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            let group_order = playable_group_order_anchors(&runtime_snapshot.snapshot);

            let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
            runtime_snapshot.set_seq(seq);
            let group_order_update = control_update_msg(
                seq,
                RoomControlUpdate::GroupOrder { order: group_order },
                Some(client_id),
                None,
            );

            inner.live = Some(runtime_snapshot);
            group_order_update
        };

        self.touch_command(now, false).await?;
        self.persist_snapshot_if_needed().await?;
        let _ = self.broadcast(&group_order_update);
        self.schedule_alarm().await?;

        Ok(())
    }

    async fn handle_move(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pos: PlayablePositionSnapshot,
        client_seq: u64,
    ) -> Result<()> {
        let now = now_ms();
        let update = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
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
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            if let Some(owner) = inner.owners_by_anchor.get_mut(&anchor_id) {
                owner.since_ms = now;
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                console_log!(
                    "move ignored: anchor mismatch (client={} anchor={} canonical={})",
                    client_id,
                    anchor_id,
                    anchor_id_usize
                );
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            // A move keeps the group's current rotation; project members about
            // the proposed drop center and reject only if the whole group would
            // leave the workspace.
            let out_of_bounds = if let Some(geometry) = runtime_snapshot.geometry.as_ref() {
                let group = runtime_snapshot
                    .snapshot
                    .state
                    .piece_group
                    .get(anchor_id_usize)
                    .map(|g| *g as usize)
                    .unwrap_or(0);
                let anchor_rot = runtime_snapshot
                    .snapshot
                    .state
                    .group_pose
                    .get(group)
                    .map(|pose| pose.rotation_deg)
                    .unwrap_or(0.0);
                !group_in_bounds(
                    &runtime_snapshot.snapshot,
                    geometry,
                    &members,
                    anchor_id_usize,
                    (drop_pos.x_mm, drop_pos.y_mm),
                    anchor_rot,
                )
            } else {
                false
            };
            if out_of_bounds {
                console_log!(
                    "move ignored: out of bounds (client={} anchor={})",
                    client_id,
                    anchor_id
                );
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let action_id =
                bridge_action_id(client_id, Some(client_seq), runtime_snapshot.snapshot.seq);
            let batch =
                match apply_bridge_live_move(&mut runtime_snapshot, anchor_id, drop_pos, action_id)
                {
                    Ok(batch) => batch,
                    Err(err) => {
                        console_log!(
                            "move ignored: bridge error (client={} anchor={} err={:?})",
                            client_id,
                            anchor_id,
                            err
                        );
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };

            let update = Some(playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::ActionOnly,
                Some(client_id),
                Some(client_seq).filter(|value| *value != 0),
            ));
            inner.live = Some(runtime_snapshot);
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
                    ServerMsg::PlayableUpdate { seq, .. } => *seq,
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
        drop_pose: PlayablePoseSnapshot,
        client_seq: u64,
    ) -> Result<()> {
        let now = now_ms();
        let update = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            if let Some(owner) = inner.owners_by_anchor.get_mut(&anchor_id) {
                owner.since_ms = now;
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                console_log!(
                    "transform ignored: anchor mismatch (client={} anchor={} canonical={})",
                    client_id,
                    anchor_id,
                    anchor_id_usize
                );
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            // A transform sets the group's rotation to the drop pose's; project
            // members about the proposed center + rotation.
            let out_of_bounds = if let Some(geometry) = runtime_snapshot.geometry.as_ref() {
                !group_in_bounds(
                    &runtime_snapshot.snapshot,
                    geometry,
                    &members,
                    anchor_id_usize,
                    (drop_pose.x_mm, drop_pose.y_mm),
                    drop_pose.rotation_deg,
                )
            } else {
                false
            };
            if out_of_bounds {
                console_log!(
                    "transform ignored: out of bounds (client={} anchor={})",
                    client_id,
                    anchor_id
                );
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let action_id =
                bridge_action_id(client_id, Some(client_seq), runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_live_transform(
                &mut runtime_snapshot,
                anchor_id,
                drop_pose,
                action_id,
            ) {
                Ok(batch) => batch,
                Err(err) => {
                    console_log!(
                        "transform ignored: bridge error (client={} anchor={} err={:?})",
                        client_id,
                        anchor_id,
                        err
                    );
                    inner.live = Some(runtime_snapshot);
                    return Ok(());
                }
            };

            let update = Some(playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::ActionOnly,
                Some(client_id),
                Some(client_seq).filter(|value| *value != 0),
            ));
            inner.live = Some(runtime_snapshot);
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
                    ServerMsg::PlayableUpdate { seq, .. } => *seq,
                    _ => 0,
                }
            );
            let _ = self.broadcast(&update);
        }
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_flip(
        &self,
        client_id: ClientId,
        piece_id: u32,
        flipped: bool,
        drop_pose: PlayablePoseSnapshot,
    ) -> Result<()> {
        let now = now_ms();
        let updates = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
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
                inner.live = Some(runtime_snapshot);
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
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let action_id = bridge_action_id(client_id, None, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_flip(
                &mut runtime_snapshot,
                piece_id,
                flipped,
                Some(drop_pose),
                action_id,
            ) {
                Ok(batch) => batch,
                Err(err) => {
                    console_log!(
                        "flip rejected: bridge error (client={} piece={} err={:?})",
                        client_id,
                        piece_id,
                        err
                    );
                    inner.live = Some(runtime_snapshot);
                    return Ok(());
                }
            };

            inner.owner_by_client.remove(&client_id);
            inner.owners_by_anchor.remove(&anchor_id);

            let flip_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::RestrictedAction,
                Some(client_id),
                None,
            );
            let ownership_update = control_update_msg(
                runtime_snapshot.snapshot.seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                Some(client_id),
                None,
            );
            inner.live = Some(runtime_snapshot);
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

    async fn handle_detach(&self, client_id: ClientId, piece_id: u32) -> Result<()> {
        let now = now_ms();
        let updates = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;
            if members.len() == 1 && anchor_id == piece_id {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                console_log!(
                    "detach rejected: not owner (client={} piece={} anchor={})",
                    client_id,
                    piece_id,
                    anchor_id
                );
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let action_id = bridge_action_id(client_id, None, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_detach(&mut runtime_snapshot, piece_id, action_id) {
                Ok(batch) => batch,
                Err(err) => {
                    console_log!(
                        "detach rejected: bridge error (client={} piece={} err={:?})",
                        client_id,
                        piece_id,
                        err
                    );
                    inner.live = Some(runtime_snapshot);
                    return Ok(());
                }
            };

            inner.owner_by_client.insert(client_id, piece_id);
            if anchor_id != piece_id {
                inner.owners_by_anchor.remove(&anchor_id);
            }
            inner.owners_by_anchor.insert(
                piece_id,
                Ownership {
                    owner_id: client_id,
                    anchor_id: piece_id,
                    since_ms: now,
                },
            );

            let detach_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::RestrictedAction,
                Some(client_id),
                None,
            );
            let seq = runtime_snapshot.snapshot.seq;
            let mut ownership_updates = Vec::new();
            if anchor_id != piece_id {
                ownership_updates.push(control_update_msg(
                    seq,
                    RoomControlUpdate::Ownership {
                        group_anchor: anchor_id,
                        owner: None,
                        reason: OwnershipReason::Released,
                    },
                    Some(client_id),
                    None,
                ));
            }
            ownership_updates.push(control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: piece_id,
                    owner: Some(client_id),
                    reason: OwnershipReason::Granted,
                },
                Some(client_id),
                None,
            ));
            inner.live = Some(runtime_snapshot);
            Some((detach_update, ownership_updates))
        };

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some((detach_update, ownership_updates)) = updates {
            console_log!("detach accepted: client={} piece={}", client_id, piece_id);
            let _ = self.broadcast(&detach_update);
            for update in ownership_updates {
                let _ = self.broadcast(&update);
            }
        }
        self.schedule_alarm().await?;
        Ok(())
    }

    async fn handle_rotate(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        drop_rotation_deg: f32,
    ) -> Result<()> {
        self.handle_finalize(client_id, anchor_id, None, Some(drop_rotation_deg), None)
            .await
    }

    async fn handle_place(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pose: PlayablePoseSnapshot,
        client_seq: u64,
    ) -> Result<()> {
        self.handle_finalize(
            client_id,
            anchor_id,
            Some(PlayablePositionSnapshot {
                x_mm: drop_pose.x_mm,
                y_mm: drop_pose.y_mm,
            }),
            Some(drop_pose.rotation_deg),
            Some(client_seq),
        )
        .await
    }

    async fn handle_finalize(
        &self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pos: Option<PlayablePositionSnapshot>,
        rot_deg: Option<f32>,
        client_seq: Option<u64>,
    ) -> Result<()> {
        let now = now_ms();
        let (playable_update, released_anchor) = {
            let mut inner = self.inner.borrow_mut();
            let mut runtime_snapshot = match inner.live.take() {
                Some(snapshot) => snapshot,
                None => return Ok(()),
            };
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        inner.live = Some(runtime_snapshot);
                        return Ok(());
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                inner.live = Some(runtime_snapshot);
                return Ok(());
            }

            let action_id = bridge_action_id(client_id, client_seq, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_finalize(
                &mut runtime_snapshot,
                anchor_id,
                drop_pos,
                rot_deg,
                action_id,
            ) {
                Ok(batch) => batch,
                Err(err) => {
                    console_log!(
                        "finalize ignored: bridge error (client={} anchor={} err={:?})",
                        client_id,
                        anchor_id,
                        err
                    );
                    inner.live = Some(runtime_snapshot);
                    return Ok(());
                }
            };

            let released_anchor = inner.owner_by_client.remove(&client_id);
            if let Some(released_anchor) = released_anchor {
                inner.owners_by_anchor.remove(&released_anchor);
            }
            let playable_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::Snap,
                Some(client_id),
                client_seq.filter(|value| *value != 0),
            );
            inner.live = Some(runtime_snapshot);
            (playable_update, released_anchor)
        };

        self.touch_command(now, true).await?;
        self.persist_snapshot_if_needed().await?;
        if let Some(anchor_id) = released_anchor {
            let update = control_update_msg(
                match &playable_update {
                    ServerMsg::PlayableUpdate { seq, .. } => *seq,
                    _ => 0,
                },
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                Some(client_id),
                client_seq.filter(|value| *value != 0),
            );
            let _ = self.broadcast(&update);
        }
        let seq = match &playable_update {
            ServerMsg::PlayableUpdate { seq, .. } => *seq,
            _ => 0,
        };
        self.broadcast(&playable_update)?;
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

    fn release_by_client(&self, client_id: ClientId, reason: OwnershipReason) -> Result<()> {
        if let Some(anchor_id) = self.clear_ownership_for_client(client_id) {
            let seq = self.bump_seq_for_update();
            let msg = control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: None,
                    reason,
                },
                Some(client_id),
                None,
            );
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
            let seq = self.bump_seq_for_update();
            let msg = control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: owner.anchor_id,
                    owner: None,
                    reason: OwnershipReason::Timeout,
                },
                None,
                None,
            );
            let _ = self.broadcast(&msg);
        }
        Ok(())
    }

    fn bump_seq_for_update(&self) -> u64 {
        let mut inner = self.inner.borrow_mut();
        if let Some(live) = inner.live.as_mut() {
            let next = live.snapshot.seq.saturating_add(1);
            live.set_seq(next);
            next
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
            (
                inner.meta.persistence,
                inner.live.as_ref().map(|live| live.snapshot.clone()),
            )
        };

        if matches!(persistence, RoomPersistence::Durable) {
            if let Some(snapshot) = snapshot {
                if let Some(bytes) = encode_stored_snapshot(&snapshot) {
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
                inner.live.is_some(),
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
            inner.live = None;
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
            let Some(snapshot) = inner.live.as_ref().map(|live| live.snapshot.clone()) else {
                return Ok(());
            };
            inner.meta.last_full_state_at = Some(now_ms());
            snapshot
        };
        self.persist_meta().await?;
        let seq = snapshot.seq;
        let msg = state_msg_from_snapshot(&snapshot);
        let result = self.broadcast(&msg);
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
        Self {
            schema_ready: false,
        }
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
            (Method::Get, AssetStoreRoute::Asset { hash }) => match self.load_asset_sql(&hash) {
                Ok(Some(asset)) => {
                    let Some(body) = encode(&asset) else {
                        return Response::error("failed to encode asset", 500);
                    };
                    Response::from_bytes(body)
                }
                Ok(None) => Response::error("not found", 404),
                Err(message) => Response::error(&message, 500),
            },
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
        let expected_chunks =
            ((size as usize + ASSET_STORAGE_CHUNK_BYTES - 1) / ASSET_STORAGE_CHUNK_BYTES) as u32;
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

    fn load_asset_sql(&self, hash: &str) -> std::result::Result<Option<StoredAsset>, String> {
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

    fn sql_value_i64(value: &SqlStorageValue, field: &str) -> std::result::Result<i64, String> {
        match value {
            SqlStorageValue::Integer(value) => Ok(*value),
            SqlStorageValue::Float(value) => Ok(*value as i64),
            _ => Err(format!("invalid asset {field}")),
        }
    }

    fn sql_value_u32(value: &SqlStorageValue, field: &str) -> std::result::Result<u32, String> {
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
        Self {
            schema_ready: false,
        }
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
            (Method::Post, CommandStoreRoute::EventsClear) => match self.clear_events() {
                Ok(()) => Response::ok("ok"),
                Err(message) => Response::error(&message, 500),
            },
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
                pos: match (
                    Self::sql_optional_f32(&row[6]),
                    Self::sql_optional_f32(&row[7]),
                ) {
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
            x if x == RecordedCommandKind::Detach as u32 => Ok(RecordedCommandKind::Detach),
            x if x == RecordedCommandKind::SendToBack as u32 => {
                Ok(RecordedCommandKind::SendToBack)
            }
            _ => Err("invalid command kind".to_string()),
        }
    }

    fn decode_outcome(value: u32) -> std::result::Result<RecordedCommandOutcome, String> {
        match value {
            x if x == RecordedCommandOutcome::Applied as u32 => Ok(RecordedCommandOutcome::Applied),
            x if x == RecordedCommandOutcome::AcceptedNoStateChange as u32 => {
                Ok(RecordedCommandOutcome::AcceptedNoStateChange)
            }
            x if x == RecordedCommandOutcome::Ignored as u32 => Ok(RecordedCommandOutcome::Ignored),
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

    fn sql_required_i64(value: &SqlStorageValue, field: &str) -> std::result::Result<i64, String> {
        match value {
            SqlStorageValue::Integer(value) => Ok(*value),
            SqlStorageValue::Float(value) => Ok(*value as i64),
            _ => Err(format!("invalid command {field}")),
        }
    }

    fn sql_required_u32(value: &SqlStorageValue, field: &str) -> std::result::Result<u32, String> {
        let raw = Self::sql_required_i64(value, field)?;
        u32::try_from(raw).map_err(|_| format!("invalid command {field}"))
    }

    fn sql_required_u64(value: &SqlStorageValue, field: &str) -> std::result::Result<u64, String> {
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

fn playable_group_members(snapshot: &PlayableGameSnapshot, piece_id: usize) -> Option<Vec<usize>> {
    let total = snapshot.state.topology_piece_count as usize;
    if piece_id >= total || snapshot.state.piece_group.len() != total {
        return None;
    }
    let group = *snapshot.state.piece_group.get(piece_id)?;
    let mut members = snapshot
        .state
        .piece_group
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, candidate)| (candidate == group).then_some(idx))
        .collect::<Vec<_>>();
    members.sort_unstable();
    (!members.is_empty()).then_some(members)
}

fn playable_group_anchor(snapshot: &PlayableGameSnapshot, group: u32) -> Option<usize> {
    snapshot
        .state
        .piece_group
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, candidate)| (candidate == group).then_some(idx))
        .min()
}

fn playable_group_order_anchors(snapshot: &PlayableGameSnapshot) -> Vec<u32> {
    let mut order = Vec::with_capacity(snapshot.state.z_order.len());
    for group in snapshot.state.z_order.iter().copied() {
        let Some(anchor) = playable_group_anchor(snapshot, group) else {
            continue;
        };
        let anchor = anchor as u32;
        if !order.contains(&anchor) {
            order.push(anchor);
        }
    }
    order
}

/// Authoritative bounds clamp for a proposed group move/transform.
///
/// Projects every member piece to its pixel center for the proposed anchor
/// pose, then accepts the placement when at least one piece center stays
/// inside the workspace (inset by that piece's own half extent — and a full
/// extent for multi-piece groups, matching the original grid heuristic). The
/// rejection condition is "the whole group would leave the workspace".
///
/// Works for any topology: `pose_unit_*` scale pose-mm to pixels (equal to a
/// grid cell's pixel size for grids, so grid behaviour is preserved) and
/// `piece_half_*` carry each piece's true bounding box.
fn group_in_bounds(
    snapshot: &PlayableGameSnapshot,
    geometry: &RoomGeometry,
    members: &[usize],
    anchor_id: usize,
    anchor_center_mm: (f32, f32),
    anchor_rot_deg: f32,
) -> bool {
    let Some(group) = snapshot
        .state
        .piece_group
        .get(anchor_id)
        .map(|g| *g as usize)
    else {
        return false;
    };
    let Some(anchor_local) = snapshot.state.piece_local_pose.get(anchor_id).copied() else {
        return false;
    };
    let flipped = snapshot
        .state
        .group_flip
        .get(group)
        .copied()
        .unwrap_or(false);
    let multi = members.len() > 1;
    let anchor_px = (
        geometry.origin_x + anchor_center_mm.0 * geometry.pose_unit_x,
        geometry.origin_y + anchor_center_mm.1 * geometry.pose_unit_y,
    );
    for &id in members {
        let Some(piece_local) = snapshot.state.piece_local_pose.get(id).copied() else {
            continue;
        };
        // Scale the local offset to pixels first, THEN rotate — the same
        // aspect-aware order the renderer uses (pose units are not square).
        let mut dx = (piece_local.x_mm - anchor_local.x_mm) * geometry.pose_unit_x;
        let dy = (piece_local.y_mm - anchor_local.y_mm) * geometry.pose_unit_y;
        if flipped {
            dx = -dx;
        }
        let (rx, ry) = rotate_vec(dx, dy, anchor_rot_deg);
        let center_x = anchor_px.0 + rx;
        let center_y = anchor_px.1 + ry;
        let half_w = geometry
            .piece_half_w
            .get(id)
            .copied()
            .unwrap_or(geometry.pose_unit_x * 0.5);
        let half_h = geometry
            .piece_half_h
            .get(id)
            .copied()
            .unwrap_or(geometry.pose_unit_y * 0.5);
        let inset_w = if multi { half_w * 3.0 } else { half_w };
        let inset_h = if multi { half_h * 3.0 } else { half_h };
        let (mut min_x, mut max_x) = (geometry.view_min_x + inset_w, geometry.view_max_x - inset_w);
        if max_x < min_x {
            let mid = (geometry.view_min_x + geometry.view_max_x) * 0.5;
            min_x = mid;
            max_x = mid;
        }
        let (mut min_y, mut max_y) = (geometry.view_min_y + inset_h, geometry.view_max_y - inset_h);
        if max_y < min_y {
            let mid = (geometry.view_min_y + geometry.view_max_y) * 0.5;
            min_y = mid;
            max_y = mid;
        }
        if center_x >= min_x && center_x <= max_x && center_y >= min_y && center_y <= max_y {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use heddobureika_core::{
        EdgeId, FlipState, GenericPlayableState, GridTopology, LogicalState, PieceId, PlayRules,
        PlayableState, Pose2, PuzzleTopology,
    };

    /// The server's geometry must use the SAME pose→pixel placement the client
    /// renders with, or scattered/clamped pieces would land in different spots
    /// in multiplayer than in single-player. For triangular (uniform scale +
    /// letterboxed centred frame) this used to diverge from the worker's old
    /// stretch-to-fill model.
    #[test]
    fn room_geometry_matches_client_placement_for_triangular() {
        let spec = heddobureika_core::TopologySpec::triangular_tessellation(4, 5);
        let topology = heddobureika_core::build_topology_from_spec(&spec).expect("topology");
        let (w, h) = (1200u32, 700u32);
        let ratio = GameRules::default().workspace_padding_ratio;
        let geom = RoomGeometry::from_topology(&topology, w, h, ratio).expect("room geometry");
        let render = topology
            .build_render_geometry(
                w,
                h,
                0,
                &heddobureika_core::TriangularTessellationShapeSettings::default(),
            )
            .expect("render geometry");

        let approx = |a: f32, b: f32| (a - b).abs() < 1.0e-3;
        assert!(
            approx(geom.pose_unit_x, render.pose_unit_px[0])
                && approx(geom.pose_unit_y, render.pose_unit_px[1]),
            "pose units diverge: worker {:?} vs client {:?}",
            (geom.pose_unit_x, geom.pose_unit_y),
            render.pose_unit_px,
        );
        assert!(
            approx(geom.origin_x, render.pose_origin_px[0])
                && approx(geom.origin_y, render.pose_origin_px[1]),
            "frame origin diverges: worker {:?} vs client {:?}",
            (geom.origin_x, geom.origin_y),
            render.pose_origin_px,
        );
        // Triangular: square pose units (uniform scale) and a letterboxed,
        // non-zero centred origin — the exact properties the old worker lacked.
        assert!(
            approx(geom.pose_unit_x, geom.pose_unit_y),
            "triangular pose units should be square"
        );
        assert!(
            geom.origin_x > 0.0 || geom.origin_y > 0.0,
            "triangular frame should be letterboxed (centred origin)"
        );
    }

    /// Per-piece fixture data used by the 2x1 test builder.
    struct LegacyFixture {
        positions: Vec<(f32, f32)>,
        rotations: Vec<f32>,
        flips: Vec<bool>,
        connections: Vec<[bool; 4]>,
        group_order: Vec<u32>,
        scramble_nonce: u32,
    }

    /// Restores the playable state from a `PlayableGameSnapshot` so tests
    /// can read per-piece world poses, flips, and connectivity directly
    /// without going through a legacy projection helper.
    fn restore_grid(snapshot: &PlayableGameSnapshot) -> GenericPlayableState {
        snapshot
            .restore_playable_from_spec()
            .expect("restore playable")
    }

    fn piece_position_px(playable: &GenericPlayableState, piece: u32) -> (f32, f32) {
        let pose = playable
            .piece_world_pose(PieceId(piece))
            .expect("piece world pose");
        (pose.x_mm() * 100.0 - 50.0, pose.y_mm() * 100.0 - 50.0)
    }

    fn piece_rotation_deg(playable: &GenericPlayableState, piece: u32) -> f32 {
        playable
            .piece_world_pose(PieceId(piece))
            .expect("piece world pose")
            .rotation_degrees()
    }

    fn piece_flipped(playable: &GenericPlayableState, piece: u32) -> bool {
        let group = playable
            .logical
            .group_of(PieceId(piece))
            .expect("piece group");
        playable.flip_of(group) == Some(FlipState::Flipped)
    }

    fn group_order_anchors(playable: &GenericPlayableState) -> Vec<u32> {
        playable
            .iter_z_asc()
            .filter_map(|group| playable.anchor_piece_of_group(group))
            .map(|piece| piece.as_u32())
            .collect()
    }

    fn edge_active_between(playable: &GenericPlayableState, a: u32, b: u32) -> bool {
        let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
        for edge_idx in 0..playable.logical.edge_count() {
            let edge = EdgeId(edge_idx as u32);
            if playable.logical.is_edge_active(edge) != Some(true) {
                continue;
            }
            let (e_a, e_b) = playable.logical.topology.edge_endpoints(edge);
            let (e_lo, e_hi) = if e_a.as_u32() <= e_b.as_u32() {
                (e_a.as_u32(), e_b.as_u32())
            } else {
                (e_b.as_u32(), e_a.as_u32())
            };
            if e_lo == lo && e_hi == hi {
                return true;
            }
        }
        false
    }

    #[test]
    fn bridge_action_id_is_deterministic_and_uses_client_sequence_when_present() {
        let client = ClientId::from(42);

        let from_client_seq = bridge_action_id(client, Some(7), 100);
        assert_eq!(from_client_seq, bridge_action_id(client, Some(7), 200));
        assert_ne!(from_client_seq, bridge_action_id(client, Some(8), 100));
        assert_ne!(
            from_client_seq,
            bridge_action_id(ClientId::from(43), Some(7), 100)
        );

        let from_room_seq = bridge_action_id(client, None, 100);
        assert_eq!(from_room_seq, bridge_action_id(client, Some(0), 100));
        assert_ne!(from_room_seq, bridge_action_id(client, None, 101));
        assert_ne!(from_room_seq.0, 0);
    }

    #[test]
    fn bridge_live_move_updates_connected_group_positions_without_snapping() {
        // Start from the canonical solved state (pose `(0.5, 0.5)` →
        // piece 0 top-left at pixel `(0, 0)`) and move to a target far
        // enough from `(0.5, 0.5)` to stay outside the frame snap radius.
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(0.0, 0.0), (100.0, 0.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false, true, false, false], [false; 4]],
            group_order: vec![0],
            scramble_nonce: 7,
        }))
        .expect("live puzzle");
        let action_id = ActionId(11);

        let batch =
            apply_bridge_live_move(&mut live, 0, playable_position((200.0, 250.0)), action_id)
                .expect("move should apply");

        assert_eq!(batch.proposal.action_id, Some(action_id));
        assert_eq!(batch.revision_before, 5);
        assert_eq!(batch.revision_after, 6);
        assert_eq!(batch.delta.revision, 6);
        assert_eq!(live.snapshot.seq, 6);
        assert_eq!(live.snapshot.scramble_nonce, 7);
        let restored = restore_grid(&live.snapshot);
        let p0 = piece_position_px(&restored, 0);
        let p1 = piece_position_px(&restored, 1);
        assert_approx(p0.0, 200.0);
        assert_approx(p0.1, 250.0);
        assert_approx(p1.0, 300.0);
        assert_approx(p1.1, 250.0);
        assert_approx(piece_rotation_deg(&restored, 0), 0.0);
        assert_approx(piece_rotation_deg(&restored, 1), 0.0);
        assert!(edge_active_between(&restored, 0, 1));
    }

    #[test]
    fn bridge_live_transform_rotates_connected_group_without_snapping() {
        // Same baseline as the live-move test: start at the canonical pose
        // `(0.5, 0.5)` and transform to a far target so no frame snap fires.
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(0.0, 0.0), (100.0, 0.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false, true, false, false], [false; 4]],
            group_order: vec![0],
            scramble_nonce: 8,
        }))
        .expect("live puzzle");
        let action_id = ActionId(12);

        let batch = apply_bridge_live_transform(
            &mut live,
            0,
            playable_pose((200.0, 250.0), 90.0),
            action_id,
        )
        .expect("transform should apply");

        assert_eq!(batch.proposal.action_id, Some(action_id));
        assert_eq!(batch.revision_before, 5);
        assert_eq!(batch.revision_after, 6);
        assert_eq!(batch.delta.revision, 6);
        assert_eq!(live.snapshot.seq, 6);
        let restored = restore_grid(&live.snapshot);
        let p0 = piece_position_px(&restored, 0);
        let p1 = piece_position_px(&restored, 1);
        assert_approx(p0.0, 200.0);
        assert_approx(p0.1, 250.0);
        assert_approx(p1.0, 200.0);
        assert_approx(p1.1, 350.0);
        assert_approx(piece_rotation_deg(&restored, 0), 90.0);
        assert_approx(piece_rotation_deg(&restored, 1), 90.0);
        assert_eq!(group_order_anchors(&restored), vec![0]);
    }

    #[test]
    fn bridge_finalize_keeps_fixed_group_pose_and_snaps_neighbor() {
        // Two singletons positioned so piece 0 is already at the canonical
        // workspace TL (pose `(0.5, 0.5)`) and piece 1 sits slightly past
        // its solved neighbor pose. Finalize joins them; the completed
        // group then identity-snaps to anchor pose `(0.5, 0.5)` →
        // piece 0 TL `(0, 0)`, piece 1 TL `(100, 0)`.
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(0.0, 0.0), (105.0, 0.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false; 4]; 2],
            group_order: vec![0, 1],
            scramble_nonce: 9,
        }))
        .expect("live puzzle");
        let action_id = ActionId(13);

        let batch = apply_bridge_finalize(
            &mut live,
            1,
            Some(playable_position((105.0, 0.0))),
            Some(0.0),
            action_id,
        )
        .expect("finalize should apply");

        assert_eq!(batch.proposal.action_id, Some(action_id));
        assert_eq!(batch.revision_before, 5);
        assert_eq!(batch.revision_after, 6);
        assert_eq!(batch.delta.revision, 6);
        assert_eq!(live.snapshot.seq, 6);
        let restored = restore_grid(&live.snapshot);
        assert!(edge_active_between(&restored, 0, 1));
        let p0 = piece_position_px(&restored, 0);
        let p1 = piece_position_px(&restored, 1);
        assert_approx(p0.0, 0.0);
        assert_approx(p0.1, 0.0);
        assert_approx(p1.0, 100.0);
        assert_approx(p1.1, 0.0);
        assert_eq!(group_order_anchors(&restored), vec![0]);
    }

    #[test]
    fn bridge_flip_updates_singleton_with_playable_snapshot() {
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(-50.0, -50.0), (50.0, -50.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false; 4]; 2],
            group_order: vec![0, 1],
            scramble_nonce: 10,
        }))
        .expect("live puzzle");
        let action_id = ActionId(14);

        let batch =
            apply_bridge_flip(&mut live, 1, true, None, action_id).expect("flip should apply");

        assert_eq!(batch.proposal.action_id, Some(action_id));
        assert_eq!(batch.revision_before, 5);
        assert_eq!(batch.revision_after, 6);
        assert_eq!(batch.delta.revision, 6);
        assert_eq!(live.snapshot.seq, 6);
        assert_eq!(live.snapshot.scramble_nonce, 10);
        let restored = restore_grid(&live.snapshot);
        assert!(!piece_flipped(&restored, 0));
        assert!(piece_flipped(&restored, 1));
        assert!(!edge_active_between(&restored, 0, 1));
        assert_eq!(group_order_anchors(&restored), vec![0, 1]);
        let p1 = piece_position_px(&restored, 1);
        assert_approx(p1.0, 50.0);
        assert_approx(p1.1, -50.0);
    }

    #[test]
    fn bridge_detach_splits_piece_without_moving_original_group() {
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(20.0, 30.0), (120.0, 30.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false, true, false, false], [false; 4]],
            group_order: vec![0],
            scramble_nonce: 11,
        }))
        .expect("live puzzle");
        let action_id = ActionId(15);

        let batch = apply_bridge_detach(&mut live, 1, action_id).expect("detach should apply");

        assert_eq!(batch.proposal.action_id, Some(action_id));
        assert_eq!(batch.revision_before, 5);
        assert_eq!(batch.revision_after, 6);
        assert_eq!(batch.delta.revision, 6);
        assert_eq!(live.snapshot.seq, 6);
        let restored = restore_grid(&live.snapshot);
        assert!(!edge_active_between(&restored, 0, 1));
        assert_eq!(group_order_anchors(&restored), vec![0, 1]);
        let p0 = piece_position_px(&restored, 0);
        let p1 = piece_position_px(&restored, 1);
        assert_approx(p0.0, 20.0);
        assert_approx(p0.1, 30.0);
        assert_approx(p1.0, 120.0);
        assert_approx(p1.1, 30.0);
    }

    #[test]
    fn stored_snapshot_codec_writes_current_playable_snapshot() {
        let snapshot = snapshot_2x1(LegacyFixture {
            positions: vec![(-50.0, -50.0), (50.0, -50.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false, true, false, false], [false; 4]],
            group_order: vec![0],
            scramble_nonce: 21,
        });

        let bytes = encode_stored_snapshot(&snapshot).expect("snapshot should encode");
        let playable_snapshot =
            decode::<PlayableGameSnapshot>(&bytes).expect("stored bytes should be playable");
        assert_eq!(
            playable_snapshot.version,
            heddobureika_core::PLAYABLE_GAME_SNAPSHOT_VERSION
        );
        assert_eq!(playable_snapshot.seq, 5);
        assert_eq!(playable_snapshot.scramble_nonce, 21);

        let restored = decode_stored_snapshot(&bytes).expect("stored snapshot should restore");
        assert_eq!(
            restored.version,
            heddobureika_core::PLAYABLE_GAME_SNAPSHOT_VERSION
        );
        assert_eq!(restored.seq, snapshot.seq);
        assert_eq!(restored.puzzle, snapshot.puzzle);
        assert_eq!(restored.scramble_nonce, 21);
        let restored_playable = restore_grid(&restored);
        assert_eq!(group_order_anchors(&restored_playable), vec![0]);
        assert!(edge_active_between(&restored_playable, 0, 1));
    }

    #[test]
    fn stored_snapshot_codec_rejects_legacy_snapshot_bytes() {
        // Encode an arbitrary non-PlayableGameSnapshot value. The decoder
        // should reject anything that isn't the current playable snapshot.
        let bogus_bytes = encode(&(22u32, 7u32, 99u32)).expect("encode bogus payload");
        assert!(decode_stored_snapshot(&bogus_bytes).is_none());
    }

    #[test]
    fn state_message_uses_current_playable_snapshot() {
        let snapshot = snapshot_2x1(LegacyFixture {
            positions: vec![(-50.0, -50.0), (50.0, -50.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false; 4]; 2],
            group_order: vec![0, 1],
            scramble_nonce: 23,
        });

        let msg = state_msg_from_snapshot(&snapshot);
        let ServerMsg::State { seq, snapshot } = msg else {
            panic!("expected state message");
        };
        assert_eq!(seq, 5);
        assert_eq!(
            snapshot.version,
            heddobureika_core::PLAYABLE_GAME_SNAPSHOT_VERSION
        );
        assert_eq!(snapshot.seq, 5);
        assert_eq!(snapshot.scramble_nonce, 23);
        assert_eq!(snapshot.state.topology_piece_count, 2);
    }

    #[test]
    fn playable_update_message_carries_group_changes_and_revision() {
        let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
            positions: vec![(-50.0, -50.0), (50.0, -50.0)],
            rotations: vec![0.0, 0.0],
            flips: vec![false, false],
            connections: vec![[false, true, false, false], [false; 4]],
            group_order: vec![0],
            scramble_nonce: 24,
        }))
        .expect("live puzzle");
        let action_id = ActionId(15);
        let batch =
            apply_bridge_live_move(&mut live, 0, playable_position((10.0, 20.0)), action_id)
                .expect("move should apply");

        let msg = playable_update_msg_from_batch(
            &live.snapshot,
            &batch,
            PlayableRoomUpdateKind::ActionOnly,
            Some(ClientId::from(7)),
            Some(99),
        );

        let ServerMsg::PlayableUpdate {
            seq,
            update,
            source,
            client_seq,
        } = msg
        else {
            panic!("expected playable update");
        };
        assert_eq!(seq, 6);
        assert_eq!(source, Some(ClientId::from(7)));
        assert_eq!(client_seq, Some(99));
        assert_eq!(update.kind, PlayableRoomUpdateKind::ActionOnly);
        assert_eq!(update.action_id, Some(action_id.0));
        assert_eq!(update.revision_before, 5);
        assert_eq!(update.revision_after, 6);
        assert!(!update.group_changes.is_empty());
    }

    #[test]
    fn control_update_message_carries_ownership_without_room_update() {
        let msg = control_update_msg(
            12,
            RoomControlUpdate::Ownership {
                group_anchor: 3,
                owner: Some(ClientId::from(9)),
                reason: OwnershipReason::Granted,
            },
            Some(ClientId::from(9)),
            None,
        );

        let ServerMsg::ControlUpdate {
            seq,
            update,
            source,
            client_seq,
        } = msg
        else {
            panic!("expected control update");
        };
        assert_eq!(seq, 12);
        assert_eq!(source, Some(ClientId::from(9)));
        assert_eq!(client_seq, None);
        assert!(matches!(
            update,
            RoomControlUpdate::Ownership {
                group_anchor: 3,
                owner: Some(owner),
                reason: OwnershipReason::Granted,
            } if owner == ClientId::from(9)
        ));
    }

    fn snapshot_2x1(state: LegacyFixture) -> PlayableGameSnapshot {
        let puzzle = PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            topology: TopologySpec::grid(2, 1).into(),
            shape_seed: 1,
            image_width: 200,
            image_height: 100,
        };
        let scramble_nonce = state.scramble_nonce;
        let topology = GridTopology::try_new(2, 1).expect("valid grid");
        let mut logical = LogicalState::new(topology);
        // 2x1 grid has exactly one horizontal edge (id 0) between piece 0 and 1.
        let edge_between_0_and_1 = state.connections[0][heddobureika_core::DIR_RIGHT]
            || state.connections[1][heddobureika_core::DIR_LEFT];
        if edge_between_0_and_1 {
            assert!(logical.activate_edge(EdgeId(0)));
        }
        let mut playable = PlayableState::new(logical, PlayRules::default());
        for group in playable.logical.active_group_ids().collect::<Vec<_>>() {
            let anchor = playable
                .anchor_piece_of_group(group)
                .expect("group has anchor");
            let idx = anchor.as_usize();
            let pose = Pose2::try_from_mm_degrees(
                (state.positions[idx].0 + 50.0) / 100.0,
                (state.positions[idx].1 + 50.0) / 100.0,
                state.rotations[idx],
            )
            .expect("finite pose");
            playable.group_pose[group.as_usize()] = pose;
            playable.group_flip[group.as_usize()] = if state.flips[idx] {
                FlipState::Flipped
            } else {
                FlipState::Normal
            };
        }
        // Apply group_order if it differs from the default (anchor-ascending).
        if !state.group_order.is_empty() {
            let mut z: Vec<heddobureika_core::GroupId> = Vec::new();
            for anchor_id in &state.group_order {
                if let Some(group) = playable.logical.group_of(PieceId(*anchor_id)) {
                    if !z.contains(&group) {
                        z.push(group);
                    }
                }
            }
            for group in playable.logical.active_group_ids() {
                if !z.contains(&group) {
                    z.push(group);
                }
            }
            playable.z_order = z;
            playable.rebuild_z_indices_from_snapshot();
        }
        let mut snapshot = PlayableGameSnapshot::from_playable(
            puzzle,
            GameRules::default(),
            scramble_nonce,
            &playable,
            None,
        );
        set_playable_snapshot_seq(&mut snapshot, 5);
        snapshot
    }

    fn playable_position(pos: (f32, f32)) -> PlayablePositionSnapshot {
        PlayablePositionSnapshot {
            x_mm: (pos.0 + 50.0) / 100.0,
            y_mm: (pos.1 + 50.0) / 100.0,
        }
    }

    fn playable_pose(pos: (f32, f32), rot_deg: f32) -> PlayablePoseSnapshot {
        let drop_pos = playable_position(pos);
        PlayablePoseSnapshot {
            x_mm: drop_pos.x_mm,
            y_mm: drop_pos.y_mm,
            rotation_deg: rot_deg,
        }
    }

    fn assert_approx(actual: f32, expected: f32) {
        assert!(
            (actual - expected).abs() <= 1.0e-4,
            "expected {expected}, got {actual}"
        );
    }

    #[test]
    fn resolve_topology_for_spec_round_trips_every_registry_kind() {
        // Each selectable topology, resolved client-side and shipped over the
        // wire in `PuzzleSpec::topology`, must come back as a buildable,
        // non-empty topology of the same family.
        let (w, h) = (1600u32, 900u32);
        for kind in heddobureika_core::available_topologies() {
            let choice = (kind.resolve_target)(kind.default_target_count, w, h, 1)
                .unwrap_or_else(|| panic!("{} resolve_target", kind.tag));
            let snapshot: PlayableTopologySnapshot = choice.spec.into();
            let resolved = resolve_topology_for_spec(&Some(snapshot), None, Some(1), w, h)
                .unwrap_or_else(|err| panic!("{} resolve_topology_for_spec: {err}", kind.tag));
            assert_eq!(resolved.tag, kind.tag, "topology family preserved");
            let built = heddobureika_core::build_topology_from_spec(&resolved)
                .unwrap_or_else(|| panic!("{} build", kind.tag));
            assert!(built.piece_count() > 0, "{} produced pieces", kind.tag);
        }
    }

    #[test]
    fn resolve_topology_for_spec_falls_back_to_grid_without_topology() {
        let resolved =
            resolve_topology_for_spec(&None, Some(100), None, 1000, 1000).expect("grid fallback");
        assert_eq!(resolved.tag, "grid");
        let built =
            heddobureika_core::build_topology_from_spec(&resolved).expect("build grid fallback");
        assert!(built.piece_count() > 0);
    }

    #[test]
    fn resolve_topology_for_spec_rejects_unknown_tag() {
        let bogus = PlayableTopologySnapshot {
            tag: "not_a_real_topology".to_string(),
            payload: Vec::new(),
        };
        assert!(resolve_topology_for_spec(&Some(bogus), None, None, 800, 600).is_err());
    }

    #[test]
    fn resolve_topology_for_spec_refits_aspect_dependent_topology() {
        // The client resolves Voronoi against a square image; the room's real
        // image is wide. `rebuild_for_image` should re-fit the spec to the
        // room's aspect while preserving the requested piece count.
        let kind = heddobureika_core::topology_kind_for_tag("voronoi").expect("voronoi kind");
        let client_choice = (kind.resolve_target)(120, 1000, 1000, 7).expect("client resolve");
        let client_count = heddobureika_core::build_topology_from_spec(&client_choice.spec)
            .expect("client build")
            .piece_count();
        let snapshot: PlayableTopologySnapshot = client_choice.spec.into();
        let resolved = resolve_topology_for_spec(&Some(snapshot), None, Some(7), 1920, 600)
            .expect("server refit");
        assert_eq!(resolved.tag, "voronoi");
        let built = heddobureika_core::build_topology_from_spec(&resolved).expect("server build");
        assert_eq!(
            built.piece_count(),
            client_count,
            "piece count preserved across aspect refit"
        );
    }

    #[test]
    fn resolve_topology_refits_grid_to_real_image_aspect() {
        // A grid resolved client-side against a square catalog image must not
        // keep its square shape when applied to a wide custom upload — the
        // worker re-fits the grid to the room's real aspect.
        let kind = heddobureika_core::topology_kind_for_tag("grid").expect("grid kind");
        let square = (kind.resolve_target)(100, 1000, 1000, 0).expect("client grid");
        let snapshot: PlayableTopologySnapshot = square.spec.into();
        let resolved =
            resolve_topology_for_spec(&Some(snapshot), None, None, 1920, 600).expect("refit");
        let (cols, rows) = heddobureika_core::build_topology_from_spec(&resolved)
            .expect("build grid")
            .dims_hint()
            .expect("grid dims");
        assert!(
            cols > rows,
            "grid should widen for a wide image, got {cols}x{rows}"
        );
    }

    fn snapshot_for_topology(
        spec: TopologySpec,
        image_w: u32,
        image_h: u32,
    ) -> PlayableGameSnapshot {
        let topology =
            heddobureika_core::build_topology_from_spec(&spec).expect("buildable topology");
        let logical = LogicalState::new(topology);
        let playable = PlayableState::new(logical, PlayRules::default());
        let puzzle = PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            topology: spec.into(),
            shape_seed: 1,
            image_width: image_w,
            image_height: image_h,
        };
        let mut snapshot =
            PlayableGameSnapshot::from_playable(puzzle, GameRules::default(), 0, &playable, None);
        set_playable_snapshot_seq(&mut snapshot, 5);
        snapshot
    }

    #[test]
    fn every_topology_gets_per_piece_bounds_geometry() {
        // Regression: the bounds geometry used to be grid-only, so every
        // action handler bailed for non-grid topologies and no move/flip/etc.
        // processed on triangular/hexagonal/Voronoi rooms. Geometry is now
        // built from each topology's per-piece extents, so it's present for
        // all of them — and the action still applies.
        for spec in [
            TopologySpec::grid(3, 2),
            TopologySpec::triangular_tessellation(3, 2),
            TopologySpec::hexagonal(5, 4, 1.5),
            TopologySpec::voronoi(40, 1, 1.5),
        ] {
            let tag = spec.tag.clone();
            let live = RoomLivePuzzle::from_snapshot(snapshot_for_topology(spec, 600, 400))
                .expect("live puzzle");
            let geometry = live
                .geometry
                .as_ref()
                .unwrap_or_else(|| panic!("{tag} should have bounds geometry"));
            assert_eq!(
                geometry.piece_half_w.len(),
                live.snapshot.state.topology_piece_count as usize,
                "{tag} has a half-extent per piece"
            );
            assert!(
                geometry.piece_half_w.iter().all(|w| *w > 0.0),
                "{tag} piece extents are positive"
            );
        }

        // The action itself applies on a non-grid topology.
        let triangular =
            snapshot_for_topology(TopologySpec::triangular_tessellation(3, 2), 600, 400);
        let mut live = RoomLivePuzzle::from_snapshot(triangular).expect("live puzzle");
        let batch = apply_bridge_flip(&mut live, 0, true, None, ActionId(7)).expect("flip applies");
        assert_eq!(batch.proposal.action_id, Some(ActionId(7)));
        assert_eq!(batch.revision_after, batch.revision_before + 1);
        assert_eq!(live.snapshot.seq, batch.revision_after);
    }

    #[test]
    fn bridge_flip_honors_client_supplied_drop_pose() {
        // The click-pivot adjustment is computed client-side; the server must
        // reproduce it by applying the post-flip pose carried on the wire,
        // not by recomputing the pre-flip world pose.
        let triangular =
            snapshot_for_topology(TopologySpec::triangular_tessellation(3, 2), 600, 400);
        let mut live = RoomLivePuzzle::from_snapshot(triangular).expect("live puzzle");
        let group = heddobureika_game::GroupId(0);
        let pivoted = PlayablePoseSnapshot {
            x_mm: 4.25,
            y_mm: 1.75,
            rotation_deg: 0.0,
        };
        let _ = apply_bridge_flip(&mut live, 0, true, Some(pivoted.clone()), ActionId(9))
            .expect("flip applies");
        let pose = live.playable.pose_of(group).expect("group pose");
        assert!(
            (pose.x_mm() - pivoted.x_mm).abs() <= 1.0e-3
                && (pose.y_mm() - pivoted.y_mm).abs() <= 1.0e-3,
            "server must adopt the wire drop_pose ({pivoted:?}) but got {pose:?}"
        );
        assert_eq!(live.playable.flip_of(group), Some(FlipState::Flipped));
    }

    #[test]
    fn admin_solve_anchors_at_each_topologys_frame_anchor() {
        // The solved puzzle must sit at the topology's canonical frame anchor
        // so it lines up with the frame outline. Regression: the worker used
        // `(0.5, 0.5)` for every topology, which only aligns the grid and left
        // non-grid puzzles offset down/right of the frame.
        for spec in [
            TopologySpec::grid(3, 2),
            TopologySpec::triangular_tessellation(3, 2),
            TopologySpec::hexagonal(5, 4, 1.5),
            TopologySpec::voronoi(40, 1, 1.5),
        ] {
            let tag = spec.tag.clone();
            let topology =
                heddobureika_core::build_topology_from_spec(&spec).expect("buildable topology");
            let expected = topology
                .identity_frame_anchor()
                .map(|(_, pose)| pose)
                .unwrap_or_default();
            let puzzle = snapshot_for_topology(spec, 600, 400).puzzle;
            let solved = Room::build_solved_snapshot(puzzle, GameRules::default())
                .unwrap_or_else(|| panic!("{tag} solved snapshot"));
            let anchor = solved.state.group_pose[0];
            assert!(
                (anchor.x_mm - expected.x_mm()).abs() < 1.0e-4
                    && (anchor.y_mm - expected.y_mm()).abs() < 1.0e-4,
                "{tag}: solved anchor {:?} != frame anchor ({}, {})",
                (anchor.x_mm, anchor.y_mm),
                expected.x_mm(),
                expected.y_mm(),
            );
        }
    }

    #[test]
    fn group_in_bounds_accepts_centered_and_rejects_offscreen() {
        let live = RoomLivePuzzle::from_snapshot(snapshot_for_topology(
            TopologySpec::grid(3, 2),
            600,
            400,
        ))
        .expect("live puzzle");
        let geometry = live.geometry.as_ref().expect("geometry");
        let members = [0usize];
        // The image center in pose-mm (extent 3x2 → center ~ (1.5, 1.0)) sits
        // comfortably inside the workspace.
        assert!(group_in_bounds(
            &live.snapshot,
            geometry,
            &members,
            0,
            (1.5, 1.0),
            0.0
        ));
        // A wildly off-screen anchor is rejected.
        assert!(!group_in_bounds(
            &live.snapshot,
            geometry,
            &members,
            0,
            (100.0, 100.0),
            0.0
        ));
    }
}
