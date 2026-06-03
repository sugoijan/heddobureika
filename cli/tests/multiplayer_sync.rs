use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::catalog::PUZZLE_CATALOG;
use heddobureika_core::codec::{decode, encode};
use heddobureika_core::room_id::{ROOM_ID_ALPHABET, ROOM_ID_LEN};
use heddobureika_core::{
    AdminMsg, ClientMsg, GameRules, GridTopology, LogicalState, PieceId, PlayRules,
    PlayableGameSnapshot, PlayablePoseSnapshot, PlayablePositionSnapshot, PlayableState, Pose2,
    PuzzleImageRef, PuzzleInfo, PuzzleSpec, RoomPersistence, ServerMsg, TopologySpec,
    ASSET_CHUNK_BYTES,
};

/// Test fixture: each piece is its own singleton group with the given pose.
/// No edges are active.
struct SingletonFixture {
    positions: Vec<(f32, f32)>,
    rotations: Vec<f32>,
    scramble_nonce: u32,
}
use p256::ecdsa::{signature::Signer, Signature, SigningKey};
use p256::elliptic_curve::rand_core::{OsRng, RngCore};
use rand::RngExt;
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use tokio::net::TcpStream;
use tokio::time::{timeout, Duration};
use tokio_tungstenite::tungstenite::client::IntoClientRequest;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async, MaybeTlsStream, WebSocketStream};
use url::Url;

type WsStream = WebSocketStream<MaybeTlsStream<TcpStream>>;

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

fn env_or(key: &str, fallback: &str) -> String {
    std::env::var(key).unwrap_or_else(|_| fallback.to_string())
}

fn now_ms() -> i64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

fn derive_client_id(pubkey_sec1: &[u8]) -> u64 {
    let digest = Sha256::digest(pubkey_sec1);
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&digest[..8]);
    u64::from_be_bytes(bytes)
}

fn random_nonce() -> String {
    let mut bytes = [0u8; 12];
    OsRng.fill_bytes(&mut bytes);
    URL_SAFE_NO_PAD.encode(bytes)
}

fn build_auth_protocol(
    room_id: &str,
    admin_token: Option<&str>,
) -> Result<String, Box<dyn std::error::Error>> {
    const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
    const AUTH_CONTEXT: &str = "heddobureika-auth-v1";

    let signing_key = SigningKey::random(&mut OsRng);
    let verifying_key = signing_key.verifying_key();
    let public_key_sec1 = verifying_key.to_encoded_point(false).as_bytes().to_vec();
    let client_id = derive_client_id(&public_key_sec1);
    let ts = now_ms();
    let nonce = random_nonce();
    let message = format!("{room_id}\n{ts}\n{nonce}\n{AUTH_CONTEXT}");
    let signature: Signature = signing_key.sign(message.as_bytes());

    let payload = AuthPayload {
        v: 1,
        client_id: client_id.to_string(),
        ts,
        nonce,
        pubkey: URL_SAFE_NO_PAD.encode(&public_key_sec1),
        sig: URL_SAFE_NO_PAD.encode(signature.to_bytes()),
        admin_token: admin_token.map(|value| value.trim().to_string()),
    };
    let payload_bytes = serde_json::to_vec(&payload)?;
    Ok(format!(
        "{AUTH_PROTOCOL_PREFIX}{}",
        URL_SAFE_NO_PAD.encode(payload_bytes)
    ))
}

async fn connect_room_with_auth(
    base_url: &str,
    room_id: &str,
    admin_token: Option<&str>,
) -> Result<WsStream, Box<dyn std::error::Error>> {
    let room_url = build_room_url(base_url, room_id)?;
    let auth_protocol = build_auth_protocol(room_id, admin_token)?;
    let mut request = room_url
        .as_str()
        .into_client_request()
        .map_err(|err| format!("failed to build websocket request: {err}"))?;
    request
        .headers_mut()
        .append("Sec-WebSocket-Protocol", auth_protocol.parse()?);
    let (ws, _) = connect_async(request).await?;
    Ok(ws)
}

fn admin_token_from_env() -> Option<String> {
    std::env::var("ADMIN_TOKEN")
        .ok()
        .or_else(|| std::env::var("ROOM_ADMIN_TOKEN").ok())
}

fn generate_room_id() -> String {
    let mut rng = rand::rng();
    let alphabet = ROOM_ID_ALPHABET.as_bytes();
    let mut id = String::with_capacity(ROOM_ID_LEN);
    for _ in 0..ROOM_ID_LEN {
        let idx = rng.random_range(0..alphabet.len());
        id.push(alphabet[idx] as char);
    }
    id
}

fn build_room_url(base_url: &str, room_id: &str) -> Result<Url, url::ParseError> {
    let mut url = Url::parse(base_url)?;
    let base_path = url.path().trim_end_matches('/');
    let path = format!("{}/{}", base_path, room_id);
    url.set_path(&path);
    url.set_query(None);
    Ok(url)
}

async fn recv_server_msg(
    read: &mut futures_util::stream::SplitStream<WsStream>,
) -> Option<ServerMsg> {
    while let Some(message) = read.next().await {
        let Ok(message) = message else {
            continue;
        };
        match message {
            Message::Binary(bytes) => {
                if let Some(msg) = decode::<ServerMsg>(&bytes) {
                    return Some(msg);
                }
            }
            Message::Close(_) => return None,
            _ => {}
        }
    }
    None
}

async fn recv_with_timeout(
    read: &mut futures_util::stream::SplitStream<WsStream>,
    dur: Duration,
) -> Option<ServerMsg> {
    match timeout(dur, recv_server_msg(read)).await {
        Ok(msg) => msg,
        Err(_) => None,
    }
}

async fn send_client_msg(
    write: &mut futures_util::stream::SplitSink<WsStream, Message>,
    msg: ClientMsg,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(bytes) = encode(&msg) {
        write.send(Message::Binary(bytes.into())).await?;
    }
    Ok(())
}

async fn send_admin_msg(
    write: &mut futures_util::stream::SplitSink<WsStream, Message>,
    msg: AdminMsg,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(bytes) = encode(&msg) {
        write.send(Message::Binary(bytes.into())).await?;
    }
    Ok(())
}

fn build_init_payload() -> (PuzzleInfo, SingletonFixture) {
    let cols = 2u32;
    let rows = 2u32;
    let image_width = 400u32;
    let image_height = 400u32;
    let piece_width = image_width as f32 / cols as f32;
    let piece_height = image_height as f32 / rows as f32;
    let mut positions = Vec::new();
    for row in 0..rows {
        for col in 0..cols {
            positions.push((col as f32 * piece_width, row as f32 * piece_height));
        }
    }
    let rotations = vec![0.0; (cols * rows) as usize];
    let puzzle = PuzzleInfo {
        label: "Test".to_string(),
        image_ref: PuzzleImageRef::BuiltIn {
            slug: PUZZLE_CATALOG[0].slug.to_string(),
        },
        topology: TopologySpec::grid(cols, rows).into(),
        shape_seed: 0,
        image_width,
        image_height,
    };
    let state = SingletonFixture {
        positions,
        rotations,
        scramble_nonce: 0,
    };
    (puzzle, state)
}

fn singleton_playable(
    puzzle: &PuzzleInfo,
    state: &SingletonFixture,
) -> PlayableState<GridTopology> {
    let (cols, rows) = puzzle.grid_dims().expect("grid puzzle in test");
    let piece_width = puzzle.image_width as f32 / cols as f32;
    let piece_height = puzzle.image_height as f32 / rows as f32;
    let topology = GridTopology::try_new(cols, rows).expect("valid grid");
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());
    for (idx, &(x, y)) in state.positions.iter().enumerate() {
        let mm_x = (x + piece_width * 0.5) / piece_width;
        let mm_y = (y + piece_height * 0.5) / piece_height;
        let pose = Pose2::try_from_mm_degrees(mm_x, mm_y, state.rotations[idx]).expect("finite");
        playable.group_pose[idx] = pose;
    }
    playable
}

fn build_init_msg(puzzle: &PuzzleInfo, state: &SingletonFixture) -> ClientMsg {
    let playable = singleton_playable(puzzle, state);
    ClientMsg::Init {
        snapshot: PlayableGameSnapshot::from_playable(
            puzzle.clone(),
            GameRules::default(),
            state.scramble_nonce,
            &playable,
            None,
        ),
    }
}

fn playable_position(
    puzzle: &PuzzleInfo,
    anchor_id: u32,
    pos: (f32, f32),
) -> PlayablePositionSnapshot {
    let (cols, rows) = puzzle.grid_dims().expect("grid puzzle in test");
    let cols = cols as usize;
    let rows = rows as usize;
    assert!(cols > 0 && rows > 0);
    assert!((anchor_id as usize) < cols * rows);
    let piece_width = puzzle.image_width as f32 / cols as f32;
    let piece_height = puzzle.image_height as f32 / rows as f32;
    PlayablePositionSnapshot {
        x_mm: (pos.0 + piece_width * 0.5) / piece_width,
        y_mm: (pos.1 + piece_height * 0.5) / piece_height,
    }
}

fn playable_pose(
    puzzle: &PuzzleInfo,
    anchor_id: u32,
    pos: (f32, f32),
    rot_deg: f32,
) -> PlayablePoseSnapshot {
    let drop_pos = playable_position(puzzle, anchor_id, pos);
    PlayablePoseSnapshot {
        x_mm: drop_pos.x_mm,
        y_mm: drop_pos.y_mm,
        rotation_deg: rot_deg,
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn multiplayer_move_is_observed_by_second_client() -> Result<(), Box<dyn std::error::Error>> {
    let Some(admin_token) = admin_token_from_env() else {
        eprintln!("Skipping test: ADMIN_TOKEN/ROOM_ADMIN_TOKEN not set.");
        return Ok(());
    };
    let base_url = env_or("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws");
    let room_id = generate_room_id();

    let admin_ws = connect_room_with_auth(&base_url, &room_id, Some(&admin_token)).await?;
    let (mut admin_write, mut admin_read) = admin_ws.split();
    let admin_msg = AdminMsg::Create {
        persistence: RoomPersistence::Durable,
        puzzle: PuzzleSpec {
            image_ref: PuzzleImageRef::BuiltIn {
                slug: PUZZLE_CATALOG[0].slug.to_string(),
            },
            pieces: None,
            seed: None,
            topology: None,
            shape_seed: None,
        },
    };
    if let Some(bytes) = encode(&admin_msg) {
        admin_write.send(Message::Binary(bytes.into())).await?;
    }
    let _ = timeout(Duration::from_secs(2), recv_server_msg(&mut admin_read)).await;

    let ws_a = connect_room_with_auth(&base_url, &room_id, None).await?;
    let (mut a_write, mut a_read) = ws_a.split();
    let ws_b = connect_room_with_auth(&base_url, &room_id, None).await?;
    let (_b_write, mut b_read) = ws_b.split();

    let init_deadline = Duration::from_secs(5);
    let (puzzle, state) = build_init_payload();
    let mut initialized = false;
    let init = build_init_msg(&puzzle, &state);

    let a_welcome = recv_with_timeout(&mut a_read, init_deadline).await;
    if let Some(ServerMsg::Welcome {
        initialized: init_ok,
        ..
    }) = a_welcome
    {
        if !init_ok {
            send_client_msg(&mut a_write, init.clone()).await?;
            initialized = true;
        }
    }
    while !initialized {
        if let Some(msg) = recv_with_timeout(&mut a_read, init_deadline).await {
            match msg {
                ServerMsg::NeedInit => {
                    send_client_msg(&mut a_write, init.clone()).await?;
                    initialized = true;
                }
                ServerMsg::State { .. } => {
                    initialized = true;
                }
                _ => {}
            }
        }
    }

    let mut b_initialized = false;
    while !b_initialized {
        if let Some(msg) = recv_with_timeout(&mut b_read, init_deadline).await {
            match msg {
                ServerMsg::State { .. } => b_initialized = true,
                _ => {}
            }
        }
    }

    let anchor_id = 0u32;
    let start_pos = state.positions[anchor_id as usize];
    let new_pos = (start_pos.0 + 10.0, start_pos.1 + 10.0);
    send_client_msg(
        &mut a_write,
        ClientMsg::Select {
            piece_id: anchor_id,
        },
    )
    .await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Move {
            piece_id: anchor_id,
            drop_pos: playable_position(&puzzle, anchor_id, new_pos),
            client_seq: 1,
            base_revision: 0,
        },
    )
    .await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Place {
            piece_id: anchor_id,
            drop_pose: playable_pose(&puzzle, anchor_id, new_pos, 0.0),
            client_seq: 2,
            base_revision: 0,
        },
    )
    .await?;

    let mut observed = false;
    let deadline = Duration::from_secs(5);
    while let Some(msg) = recv_with_timeout(&mut b_read, deadline).await {
        match msg {
            ServerMsg::State { snapshot, .. } => {
                let restored = match snapshot.restore_playable_from_spec() {
                    Ok(p) => p,
                    Err(err) => {
                        return Err(format!("restore state snapshot failed: {err:?}").into());
                    }
                };
                let (cols, rows) = puzzle.grid_dims().expect("grid puzzle in test");
                let piece_width = puzzle.image_width as f32 / cols as f32;
                let piece_height = puzzle.image_height as f32 / rows as f32;
                let pos = restored
                    .piece_world_pose(PieceId(anchor_id))
                    .map(|pose| {
                        (
                            pose.x_mm() * piece_width - piece_width * 0.5,
                            pose.y_mm() * piece_height - piece_height * 0.5,
                        )
                    })
                    .unwrap_or(start_pos);
                if (pos.0 - new_pos.0).abs() < 0.01 && (pos.1 - new_pos.1).abs() < 0.01 {
                    observed = true;
                    break;
                }
            }
            ServerMsg::PlayableUpdate { update, .. } => {
                let (cols, rows) = puzzle.grid_dims().expect("grid puzzle in test");
                let piece_width = puzzle.image_width as f32 / cols as f32;
                let piece_height = puzzle.image_height as f32 / rows as f32;
                let pos = update
                    .group_changes
                    .iter()
                    .find(|change| change.group == anchor_id)
                    .map(|change| {
                        (
                            change.pose.x_mm * piece_width - piece_width * 0.5,
                            change.pose.y_mm * piece_height - piece_height * 0.5,
                        )
                    })
                    .unwrap_or(start_pos);
                if (pos.0 - new_pos.0).abs() < 0.01 && (pos.1 - new_pos.1).abs() < 0.01 {
                    observed = true;
                    break;
                }
            }
            _ => {}
        }
    }

    assert!(observed, "client B did not observe client A move");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn multiplayer_private_upload_reloads_and_serves_asset(
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(admin_token) = admin_token_from_env() else {
        eprintln!("Skipping test: ADMIN_TOKEN/ROOM_ADMIN_TOKEN not set.");
        return Ok(());
    };
    let base_url = env_or("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws");
    let room_id = generate_room_id();

    let admin_ws = connect_room_with_auth(&base_url, &room_id, Some(&admin_token)).await?;
    let (mut admin_write, mut admin_read) = admin_ws.split();

    let create_msg = AdminMsg::Create {
        persistence: RoomPersistence::Durable,
        puzzle: PuzzleSpec {
            image_ref: PuzzleImageRef::BuiltIn {
                slug: PUZZLE_CATALOG[0].slug.to_string(),
            },
            pieces: None,
            seed: None,
            topology: None,
            shape_seed: None,
        },
    };
    if let Some(bytes) = encode(&create_msg) {
        admin_write.send(Message::Binary(bytes.into())).await?;
    }

    let create_deadline = Duration::from_secs(5);
    let mut created = false;
    while let Some(msg) = recv_with_timeout(&mut admin_read, create_deadline).await {
        match msg {
            ServerMsg::AdminAck { .. } => {
                created = true;
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("create failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(created, "did not receive admin create ack");

    let image_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../puzzles/zoe-potter.jpg");
    let upload_bytes = std::fs::read(&image_path)?;
    assert!(
        upload_bytes.len() > 128 * 1024,
        "expected test image to exceed 128KiB"
    );

    let upload_begin = AdminMsg::UploadPrivateBegin {
        mime: "image/jpeg".to_string(),
        size: upload_bytes.len() as u32,
    };
    if let Some(bytes) = encode(&upload_begin) {
        admin_write.send(Message::Binary(bytes.into())).await?;
    }

    for chunk in upload_bytes.chunks(ASSET_CHUNK_BYTES) {
        let msg = AdminMsg::UploadPrivateChunk {
            bytes: chunk.to_vec(),
        };
        if let Some(bytes) = encode(&msg) {
            admin_write.send(Message::Binary(bytes.into())).await?;
        }
    }

    let upload_end = AdminMsg::UploadPrivateEnd {
        pieces: None,
        seed: None,
        topology: None,
        shape_seed: None,
    };
    if let Some(bytes) = encode(&upload_end) {
        admin_write.send(Message::Binary(bytes.into())).await?;
    }

    let upload_deadline = Duration::from_secs(30);
    let mut uploaded_hash: Option<String> = None;
    while let Some(msg) = recv_with_timeout(&mut admin_read, upload_deadline).await {
        match msg {
            ServerMsg::UploadAck { hash } => {
                uploaded_hash = Some(hash);
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("upload failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    let uploaded_hash = uploaded_hash.ok_or("did not receive upload ack")?;

    let client_ws = connect_room_with_auth(&base_url, &room_id, None).await?;
    let (mut client_write, mut client_read) = client_ws.split();

    let state_deadline = Duration::from_secs(10);
    let mut saw_private_state = false;
    while let Some(msg) = recv_with_timeout(&mut client_read, state_deadline).await {
        match msg {
            ServerMsg::State { snapshot, .. } => {
                if let PuzzleImageRef::Private { hash } = snapshot.puzzle.image_ref {
                    if hash == uploaded_hash {
                        saw_private_state = true;
                        break;
                    }
                }
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("client state failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(
        saw_private_state,
        "new client did not receive state with uploaded private image hash"
    );

    send_client_msg(
        &mut client_write,
        ClientMsg::AssetRequest {
            hash: uploaded_hash.clone(),
        },
    )
    .await?;

    let asset_deadline = Duration::from_secs(10);
    let mut begin_hash: Option<String> = None;
    let mut begin_size: Option<u32> = None;
    let mut received = Vec::new();
    while let Some(msg) = recv_with_timeout(&mut client_read, asset_deadline).await {
        match msg {
            ServerMsg::AssetBegin { hash, size, .. } => {
                begin_hash = Some(hash);
                begin_size = Some(size);
            }
            ServerMsg::AssetChunk { hash, bytes, .. } => {
                if begin_hash.as_deref() == Some(hash.as_str()) {
                    received.extend_from_slice(&bytes);
                }
            }
            ServerMsg::AssetEnd { hash } => {
                if begin_hash.as_deref() == Some(hash.as_str()) {
                    break;
                }
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("asset fetch failed with {code}: {message}").into());
            }
            _ => {}
        }
    }

    assert_eq!(begin_hash.as_deref(), Some(uploaded_hash.as_str()));
    assert_eq!(begin_size, Some(received.len() as u32));
    assert!(
        !received.is_empty(),
        "did not receive any asset bytes from asset request"
    );

    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn recording_caps_and_exports_rows() -> Result<(), Box<dyn std::error::Error>> {
    let Some(admin_token) = admin_token_from_env() else {
        eprintln!("Skipping test: ADMIN_TOKEN/ROOM_ADMIN_TOKEN not set.");
        return Ok(());
    };
    let base_url = env_or("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws");
    let room_id = generate_room_id();

    let admin_ws = connect_room_with_auth(&base_url, &room_id, Some(&admin_token)).await?;
    let (mut admin_write, mut admin_read) = admin_ws.split();
    send_admin_msg(
        &mut admin_write,
        AdminMsg::Create {
            persistence: RoomPersistence::Durable,
            puzzle: PuzzleSpec {
                image_ref: PuzzleImageRef::BuiltIn {
                    slug: PUZZLE_CATALOG[0].slug.to_string(),
                },
                pieces: None,
                seed: None,
                topology: None,
                shape_seed: None,
            },
        },
    )
    .await?;

    let mut created = false;
    while let Some(msg) = recv_with_timeout(&mut admin_read, Duration::from_secs(5)).await {
        match msg {
            ServerMsg::AdminAck { .. } => {
                created = true;
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("create failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(created, "did not receive admin ack");

    let ws_a = connect_room_with_auth(&base_url, &room_id, None).await?;
    let (mut a_write, mut a_read) = ws_a.split();

    let (puzzle, state) = build_init_payload();
    let init = build_init_msg(&puzzle, &state);

    let mut initialized = false;
    while let Some(msg) = recv_with_timeout(&mut a_read, Duration::from_secs(5)).await {
        match msg {
            ServerMsg::NeedInit => {
                send_client_msg(&mut a_write, init.clone()).await?;
            }
            ServerMsg::State { .. } => {
                initialized = true;
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("init failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(initialized, "client did not receive initial state");

    send_admin_msg(
        &mut admin_write,
        AdminMsg::RecordingSet {
            enabled: true,
            max_events: Some(3),
        },
    )
    .await?;

    let mut enabled = false;
    while let Some(msg) = recv_with_timeout(&mut admin_read, Duration::from_secs(5)).await {
        match msg {
            ServerMsg::RecordingStatus {
                enabled: status_enabled,
                ..
            } => {
                enabled = status_enabled;
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("recording set failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(enabled, "recording should be enabled");

    send_client_msg(&mut a_write, ClientMsg::Select { piece_id: 0 }).await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Move {
            piece_id: 0,
            drop_pos: playable_position(&puzzle, 0, (30.0, 30.0)),
            client_seq: 1,
            base_revision: 0,
        },
    )
    .await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Place {
            piece_id: 0,
            drop_pose: playable_pose(&puzzle, 0, (30.0, 30.0), 0.0),
            client_seq: 2,
            base_revision: 0,
        },
    )
    .await?;
    send_client_msg(&mut a_write, ClientMsg::Select { piece_id: 1 }).await?;

    tokio::time::sleep(Duration::from_millis(200)).await;

    send_admin_msg(&mut admin_write, AdminMsg::RecordingStatus).await?;
    let mut capped = false;
    let mut event_count = 0u64;
    let mut dropped_events = 0u64;
    while let Some(msg) = recv_with_timeout(&mut admin_read, Duration::from_secs(5)).await {
        match msg {
            ServerMsg::RecordingStatus {
                enabled: status_enabled,
                capped: status_capped,
                event_count: status_event_count,
                dropped_events: status_dropped,
                ..
            } => {
                capped = status_capped;
                event_count = status_event_count;
                dropped_events = status_dropped;
                assert!(!status_enabled, "recording should auto-disable at cap");
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("recording status failed with {code}: {message}").into());
            }
            _ => {}
        }
    }
    assert!(capped, "recording should be capped");
    assert!(event_count <= 3, "event count should not exceed cap");
    assert!(
        dropped_events >= 1,
        "expected dropped event count to increment"
    );

    send_admin_msg(
        &mut admin_write,
        AdminMsg::RecordingExport {
            after_id: None,
            limit: 2,
        },
    )
    .await?;

    let mut rows = Vec::new();
    let mut next_after = None;
    while let Some(msg) = recv_with_timeout(&mut admin_read, Duration::from_secs(5)).await {
        match msg {
            ServerMsg::RecordingRows {
                rows: chunk,
                next_after_id,
            } => {
                rows.extend(chunk);
                next_after = next_after_id;
                break;
            }
            ServerMsg::Error { code, message } => {
                return Err(format!("recording export failed with {code}: {message}").into());
            }
            _ => {}
        }
    }

    if let Some(cursor) = next_after {
        send_admin_msg(
            &mut admin_write,
            AdminMsg::RecordingExport {
                after_id: Some(cursor),
                limit: 2,
            },
        )
        .await?;
        while let Some(msg) = recv_with_timeout(&mut admin_read, Duration::from_secs(5)).await {
            match msg {
                ServerMsg::RecordingRows { rows: chunk, .. } => {
                    rows.extend(chunk);
                    break;
                }
                ServerMsg::Error { code, message } => {
                    return Err(
                        format!("recording export page 2 failed with {code}: {message}").into(),
                    );
                }
                _ => {}
            }
        }
    }

    assert!(!rows.is_empty(), "expected at least one exported row");
    let mut last_id = 0u64;
    for row in rows {
        assert!(
            row.id > last_id,
            "export row ids should be strictly increasing"
        );
        last_id = row.id;
    }

    Ok(())
}
