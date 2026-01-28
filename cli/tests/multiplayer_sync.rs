use futures_util::{SinkExt, StreamExt};
use heddobureika_core::catalog::DEFAULT_PUZZLE_SLUG;
use heddobureika_core::codec::{decode, encode};
use heddobureika_core::{
    AdminMsg, ClientMsg, PuzzleInfo, PuzzleStateSnapshot, RoomPersistence, ServerMsg,
};
use heddobureika_core::room_id::{ROOM_ID_ALPHABET, ROOM_ID_LEN};
use rand::Rng;
use tokio::net::TcpStream;
use tokio::time::{timeout, Duration};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async, MaybeTlsStream, WebSocketStream};
use url::Url;

type WsStream = WebSocketStream<MaybeTlsStream<TcpStream>>;

fn env_or(key: &str, fallback: &str) -> String {
    std::env::var(key).unwrap_or_else(|_| fallback.to_string())
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

fn build_admin_url(base_url: &str, room_id: &str, token: &str) -> Result<Url, url::ParseError> {
    let mut url = build_room_url(base_url, room_id)?;
    url.query_pairs_mut().append_pair("admin_token", token);
    Ok(url)
}

async fn recv_server_msg(read: &mut futures_util::stream::SplitStream<WsStream>) -> Option<ServerMsg> {
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

fn build_init_payload() -> (PuzzleInfo, PuzzleStateSnapshot) {
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
    let flips = vec![false; (cols * rows) as usize];
    let connections = vec![[false; 4]; (cols * rows) as usize];
    let group_order = (0..(cols * rows)).map(|id| id as u32).collect();
    let puzzle = PuzzleInfo {
        label: "Test".to_string(),
        image_src: "test://image".to_string(),
        rows,
        cols,
        shape_seed: 0,
        image_width,
        image_height,
    };
    let state = PuzzleStateSnapshot {
        positions,
        rotations,
        flips,
        connections,
        group_order,
        scramble_nonce: 0,
    };
    (puzzle, state)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn multiplayer_move_is_observed_by_second_client() -> Result<(), Box<dyn std::error::Error>> {
    let Some(admin_token) = admin_token_from_env() else {
        eprintln!("Skipping test: ADMIN_TOKEN/ROOM_ADMIN_TOKEN not set.");
        return Ok(());
    };
    let base_url = env_or("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws");
    let room_id = generate_room_id();

    let admin_url = build_admin_url(&base_url, &room_id, &admin_token)?;
    let (admin_ws, _) = connect_async(admin_url.as_str()).await?;
    let (mut admin_write, mut admin_read) = admin_ws.split();
    let admin_msg = AdminMsg::Create {
        persistence: RoomPersistence::Durable,
        puzzle: DEFAULT_PUZZLE_SLUG.to_string(),
        pieces: None,
        seed: None,
    };
    if let Some(bytes) = encode(&admin_msg) {
        admin_write.send(Message::Binary(bytes.into())).await?;
    }
    let _ = timeout(Duration::from_secs(2), recv_server_msg(&mut admin_read)).await;

    let room_url = build_room_url(&base_url, &room_id)?;
    let (ws_a, _) = connect_async(room_url.as_str()).await?;
    let (mut a_write, mut a_read) = ws_a.split();
    let (ws_b, _) = connect_async(room_url.as_str()).await?;
    let (_b_write, mut b_read) = ws_b.split();

    let init_deadline = Duration::from_secs(5);
    let (puzzle, state) = build_init_payload();
    let mut initialized = false;
    let init = ClientMsg::Init {
        puzzle: puzzle.clone(),
        rules: None,
        state: Some(state.clone()),
    };

    let a_welcome = recv_with_timeout(&mut a_read, init_deadline).await;
    if let Some(ServerMsg::Welcome { initialized: init_ok, .. }) = a_welcome {
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
    send_client_msg(&mut a_write, ClientMsg::Select { piece_id: anchor_id }).await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Move {
            anchor_id,
            pos: new_pos,
        },
    )
    .await?;
    send_client_msg(
        &mut a_write,
        ClientMsg::Place {
            anchor_id,
            pos: new_pos,
            rot_deg: 0.0,
        },
    )
    .await?;

    let mut observed = false;
    let deadline = Duration::from_secs(5);
    while let Some(msg) = recv_with_timeout(&mut b_read, deadline).await {
        match msg {
            ServerMsg::Update { update, .. } => {
                if let heddobureika_core::protocol::RoomUpdate::GroupTransform { anchor_id: a, pos, .. } = update {
                    if a == anchor_id && (pos.0 - new_pos.0).abs() < 0.01 && (pos.1 - new_pos.1).abs() < 0.01 {
                        observed = true;
                        break;
                    }
                }
            }
            ServerMsg::State { snapshot, .. } => {
                let pos = snapshot.state.positions.get(anchor_id as usize).copied().unwrap_or(start_pos);
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
