use std::cell::RefCell;

use heddobureika_core::codec::{decode, encode};
use heddobureika_core::protocol::{AdminMsg, ClientMsg, RoomPersistence, ServerMsg};
use heddobureika_core::room_id::{is_valid_room_id, ROOM_ID_LEN};
use js_sys::Date;
use rkyv::{Archive, Deserialize, Serialize};
use worker::*;

const ROOM_PATH_PREFIX: &str = "/ws/";
const META_KEY: &str = "room_meta";
const SNAPSHOT_KEY: &str = "room_snapshot";

const INACTIVITY_WARNING_MS: i64 = 10 * 60 * 1000;
const INACTIVITY_EXPIRE_MS: i64 = 60 * 60 * 1000;

#[event(fetch)]
pub async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    let path = req.path();
    let room_id = match extract_room_id(&path) {
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

fn extract_room_id(path: &str) -> Option<&str> {
    let room = path.strip_prefix(ROOM_PATH_PREFIX)?;
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

fn admin_token_from_request(req: &Request) -> Result<Option<String>> {
    let url = req.url()?;
    for (key, value) in url.query_pairs() {
        if key == "admin_token" {
            return Ok(Some(value.into_owned()));
        }
    }
    Ok(None)
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
struct RoomMeta {
    activated: bool,
    persistence: RoomPersistence,
    last_command_at: Option<i64>,
    last_warning_at: Option<i64>,
}

impl Default for RoomMeta {
    fn default() -> Self {
        Self {
            activated: false,
            persistence: RoomPersistence::Durable,
            last_command_at: None,
            last_warning_at: None,
        }
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, Default)]
struct RoomSnapshot {
    command_count: u64,
    last_action: Option<String>,
    last_payload: Option<Vec<u8>>,
}

struct RoomRuntime {
    loaded: bool,
    meta: RoomMeta,
    snapshot: RoomSnapshot,
}

impl RoomRuntime {
    fn new() -> Self {
        Self {
            loaded: false,
            meta: RoomMeta::default(),
            snapshot: RoomSnapshot::default(),
        }
    }
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

        let admin_token = admin_token_from_request(&req)?;
        let is_admin = match admin_token {
            Some(token) => {
                let expected = self.admin_token()?;
                if token != expected {
                    return Response::error("invalid admin token", 403);
                }
                true
            }
            None => false,
        };

        self.ensure_loaded().await?;

        let activated = { self.inner.borrow().meta.activated };
        if !activated && !is_admin {
            return Response::error("room not activated", 403);
        }

        let pair = WebSocketPair::new()?;
        let server = pair.server;

        if is_admin {
            self.state.accept_websocket_with_tags(&server, &["admin"]);
        } else {
            self.state.accept_web_socket(&server);
        }

        if activated {
            let persistence = { self.inner.borrow().meta.persistence };
            let path = req.path();
            let room_id = extract_room_id(&path).unwrap_or("unknown");
            let welcome = ServerMsg::Welcome {
                room_id: room_id.to_string(),
                persistence,
            };
            let _ = self.send_server_msg(&server, &welcome);
        }

        Response::from_websocket(pair.client)
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

        let is_admin = self
            .state
            .get_tags(&ws)
            .iter()
            .any(|tag| tag == "admin");

        if is_admin {
            if let Some(AdminMsg::Create { persistence }) = decode::<AdminMsg>(&bytes) {
                return self.handle_admin_create(ws, persistence).await;
            }
            return Ok(());
        }

        let Some(msg) = decode::<ClientMsg>(&bytes) else {
            return Ok(());
        };

        match msg {
            ClientMsg::Command { action, payload } => {
                self.handle_command(action, payload).await?;
            }
            ClientMsg::Ping { nonce } => {
                let response = ServerMsg::Event {
                    event: "pong".to_string(),
                    payload: encode(&nonce).unwrap_or_default(),
                };
                let _ = self.send_server_msg(&ws, &response);
            }
        }

        Ok(())
    }

    async fn websocket_close(&self, _ws: WebSocket, _code: usize, _reason: String, _was_clean: bool) -> Result<()> {
        Ok(())
    }

    async fn websocket_error(&self, _ws: WebSocket, _error: Error) -> Result<()> {
        Ok(())
    }

    async fn alarm(&self) -> Result<Response> {
        self.ensure_loaded().await?;

        let meta = { self.inner.borrow().meta.clone() };
        if !meta.activated {
            return Response::ok("inactive");
        }

        let Some(last_command_at) = meta.last_command_at else {
            return Response::ok("no commands");
        };

        let now = now_ms();
        let warn_at = last_command_at + INACTIVITY_WARNING_MS;
        let expire_at = last_command_at + INACTIVITY_EXPIRE_MS;

        if now >= expire_at {
            self.expire_room().await?;
            return Response::ok("expired");
        }

        if now >= warn_at && meta.last_warning_at.map(|t| t < warn_at).unwrap_or(true) {
            self.broadcast(&ServerMsg::Warning { minutes_idle: 10 })?;
            self.update_last_warning_at(now).await?;
        }

        self.schedule_alarm(last_command_at).await?;
        Response::ok("scheduled")
    }
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

        let mut inner = self.inner.borrow_mut();
        inner.loaded = true;
        if let Some(bytes) = meta_bytes {
            if let Some(meta) = decode::<RoomMeta>(&bytes) {
                inner.meta = meta;
            }
        }
        if let Some(bytes) = snapshot_bytes {
            if let Some(snapshot) = decode::<RoomSnapshot>(&bytes) {
                inner.snapshot = snapshot;
            }
        }

        Ok(())
    }

    async fn handle_admin_create(&self, ws: WebSocket, persistence: RoomPersistence) -> Result<()> {
        let now = now_ms();
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
            inner.snapshot = RoomSnapshot::default();
        }

        self.persist_meta().await?;
        self.persist_snapshot_if_needed().await?;
        self.schedule_alarm(now).await?;

        let _ = self.send_server_msg(
            &ws,
            &ServerMsg::AdminAck {
            room_id: "unknown".to_string(),
            persistence,
            },
        );
        Ok(())
    }

    async fn handle_command(&self, action: String, payload: Vec<u8>) -> Result<()> {
        let now = now_ms();
        {
            let mut inner = self.inner.borrow_mut();
            if !inner.meta.activated {
                return Ok(());
            }
            inner.meta.last_command_at = Some(now);
            inner.meta.last_warning_at = None;
            inner.snapshot.command_count += 1;
            inner.snapshot.last_action = Some(action);
            inner.snapshot.last_payload = Some(payload);
        }

        self.persist_meta().await?;
        self.persist_snapshot_if_needed().await?;
        self.schedule_alarm(now).await?;

        let payload = encode(&self.inner.borrow().snapshot.command_count).unwrap_or_default();
        let event = ServerMsg::Event {
            event: "command".to_string(),
            payload,
        };
        self.broadcast(&event)?;

        Ok(())
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
            if let Some(bytes) = encode(&snapshot) {
                self.state.storage().put(SNAPSHOT_KEY, bytes).await?;
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

    async fn schedule_alarm(&self, last_command_at: i64) -> Result<()> {
        let now = now_ms();
        let warn_at = last_command_at + INACTIVITY_WARNING_MS;
        let expire_at = last_command_at + INACTIVITY_EXPIRE_MS;
        let next_at = if now < warn_at { warn_at } else { expire_at };
        let offset = (next_at - now).max(0);
        self.state.storage().set_alarm(offset).await
    }

    async fn expire_room(&self) -> Result<()> {
        {
            let mut inner = self.inner.borrow_mut();
            inner.meta.activated = false;
            inner.meta.last_command_at = None;
            inner.meta.last_warning_at = None;
            inner.snapshot = RoomSnapshot::default();
        }

        self.persist_meta().await?;
        let _ = self.state.storage().delete(SNAPSHOT_KEY).await;

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
