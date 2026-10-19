//! Experimental two-player transport, reusable in Wasm and native Rust.
//! The application drives the returned runner and consumes bounded events.
use async_channel::{Receiver, Sender};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use futures_util::FutureExt;
use heddobureika_core::{
    ClientId, ClientMsg, OwnershipReason, PlayableGameSnapshot, PuzzleImageRef, RoomControlUpdate,
    RoomPersistence, ServerMsg, decode, encode,
};
use heddobureika_session::SessionAuthority;
use iroh::{
    Endpoint, EndpointAddr, SecretKey,
    endpoint::{Connection, ConnectionError, RecvStream, SendStream, presets},
};
use n0_future::{
    task::JoinSet,
    time::{self, Instant},
};
use rkyv::{Archive, Deserialize, Serialize};
use std::time::Duration;

pub const ALPN: &[u8] = b"heddobureika/p2p/1";
const VERSION: u16 = 1;
const MAX_FRAME: usize = 4 * 1024 * 1024;
const QUEUE: usize = 128;
// Replacement is terminal for the old guest; ordinary disconnects still retry.
const CLOSE_REPLACED: u32 = 2;
pub const HOST: ClientId = ClientId(1);
pub const GUEST: ClientId = ClientId(2);
pub type Result<T> = std::result::Result<T, String>;

/// An invite is a capability. Deliberately has no Debug implementation.
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Invite {
    version: u16,
    pub room_id: String,
    address: EndpointAddr,
    secret: [u8; 32],
}
impl Invite {
    pub fn parse(value: &str) -> Result<Self> {
        if value.len() > 8192 {
            return Err("P2P invite is too large".into());
        }
        let bytes = URL_SAFE_NO_PAD
            .decode(value)
            .map_err(|_| "Invalid P2P invite")?;
        let invite: Self = serde_json::from_slice(&bytes).map_err(|_| "Invalid P2P invite")?;
        if invite.version != VERSION {
            return Err("This P2P invite uses an unsupported version".into());
        }
        if !heddobureika_core::is_valid_room_id(&invite.room_id) || invite.address.addrs.len() > 16
        {
            return Err("Invalid P2P invite".into());
        }
        Ok(invite)
    }
    pub fn encode(&self) -> String {
        URL_SAFE_NO_PAD.encode(serde_json::to_vec(self).expect("invite serialisation"))
    }
}

/// Private, portable room identity. Store with the authoritative checkpoint;
/// only `invite()` is intended to be shared with a guest.
#[derive(Clone, Archive, Serialize, Deserialize)]
pub struct HostIdentity {
    room_id: String,
    key: [u8; 32],
    invite: String,
    guest: Option<[u8; 32]>,
}
impl HostIdentity {
    pub fn room_id(&self) -> &str {
        &self.room_id
    }
    pub fn key(&self) -> Identity {
        Identity::from_bytes(&self.key)
    }
    pub fn invite(&self) -> &str {
        &self.invite
    }
    pub fn validate(&self) -> Result<()> {
        let invite = Invite::parse(&self.invite)?;
        if invite.room_id != self.room_id || invite.address.id != self.key().public() {
            return Err("Saved P2P room identity is inconsistent".into());
        }
        if self
            .guest
            .is_some_and(|key| iroh::EndpointId::from_bytes(&key).is_err())
        {
            return Err("Saved P2P guest identity is invalid".into());
        }
        Ok(())
    }
}
impl std::fmt::Debug for HostIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HostIdentity")
            .field("room_id", &self.room_id)
            .field("has_guest", &self.guest.is_some())
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub enum Event {
    Status {
        message: String,
        connected: bool,
    },
    Invite(String),
    Messages(Vec<ServerMsg>),
    /// Clears optimistic state before applying the following messages.
    Reset(Vec<ServerMsg>),
    /// Host-only canonical progress, at most four times per second and on
    /// shutdown. Persist this separately from transport credentials and claims.
    Checkpoint(PlayableGameSnapshot),
    /// Commit a changed guest admission before the host welcomes that guest.
    /// Persistent adapters acknowledge only after the entire record commits.
    PersistRoom {
        identity: HostIdentity,
        snapshot: PlayableGameSnapshot,
        committed: Sender<Result<()>>,
    },
    Rejected(String),
}

impl std::fmt::Debug for Event {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invite(_) => f.write_str("Invite(<redacted>)"),
            Self::Status { message, connected } => f
                .debug_struct("Status")
                .field("message", message)
                .field("connected", connected)
                .finish(),
            Self::Messages(messages) => f.debug_tuple("Messages").field(&messages.len()).finish(),
            Self::Reset(messages) => f.debug_tuple("Reset").field(&messages.len()).finish(),
            Self::Checkpoint(snapshot) => f.debug_tuple("Checkpoint").field(&snapshot.seq).finish(),
            Self::PersistRoom { .. } => f.write_str("PersistRoom(<redacted>)"),
            Self::Rejected(message) => f.debug_tuple("Rejected").field(message).finish(),
        }
    }
}

#[derive(Clone)]
pub struct Handle {
    commands: Sender<ClientMsg>,
    endpoint: Endpoint,
    checkpoints: Option<Sender<CheckpointRequest>>,
}
struct CheckpointRequest {
    reply: Sender<PlayableGameSnapshot>,
    pause: bool,
}
impl Handle {
    /// Capture state in the authority's command loop, never from predictions.
    pub async fn checkpoint(&self) -> Result<PlayableGameSnapshot> {
        self.request_checkpoint(false).await
    }
    /// Stop accepting mutations, disconnect the guest, then capture the final
    /// state. Keep the host lock until reload/close; save failures can retry.
    pub async fn prepare_reload(&self) -> Result<PlayableGameSnapshot> {
        self.request_checkpoint(true).await
    }
    async fn request_checkpoint(&self, pause: bool) -> Result<PlayableGameSnapshot> {
        let tx = self
            .checkpoints
            .as_ref()
            .ok_or("Only the host can save progress")?;
        let (reply, rx) = async_channel::bounded(1);
        tx.send(CheckpointRequest { reply, pause })
            .await
            .map_err(|_| "P2P host is closed")?;
        time::timeout(Duration::from_secs(10), rx.recv())
            .await
            .map_err(|_| "P2P checkpoint timed out")?
            .map_err(|_| "P2P host is closed".into())
    }
    pub fn send(&self, command: ClientMsg) -> Result<()> {
        self.commands
            .try_send(command)
            .map_err(|_| "P2P connection is busy or closed".into())
    }
    pub fn close(&self) {
        self.commands.close();
        let endpoint = self.endpoint.clone();
        n0_future::task::spawn(async move {
            endpoint.close().await;
        });
    }
}

#[derive(Archive, Serialize, Deserialize)]
enum ClientFrame {
    Hello {
        version: u16,
        room_id: String,
        secret: [u8; 32],
    },
    Command {
        id: u64,
        claim: u64,
        command: ClientMsg,
    },
}
#[derive(Clone, Archive, Serialize, Deserialize)]
enum ServerFrame {
    Events(Vec<ServerMsg>),
    Reset(Vec<ServerMsg>),
    Rejected(String),
}

pub async fn bind(secret: Option<SecretKey>) -> Result<Endpoint> {
    let transport = iroh::endpoint::QuicTransportConfig::builder()
        .max_idle_timeout(Some(
            Duration::from_secs(10).try_into().expect("valid timeout"),
        ))
        .keep_alive_interval(Duration::from_secs(2))
        .build();
    let mut builder = Endpoint::builder(presets::N0)
        .alpns(vec![ALPN.to_vec()])
        .transport_config(transport);
    if let Some(secret) = secret {
        builder = builder.secret_key(secret);
    }
    builder
        .bind()
        .await
        .map_err(|_| "Could not start P2P networking".into())
}
pub use iroh::SecretKey as Identity;

pub async fn wait_online(endpoint: &Endpoint) -> Result<()> {
    time::timeout(Duration::from_secs(20), endpoint.online())
        .await
        .map_err(|_| "P2P relay unavailable; reload to retry".into())
}

/// Validate progress before offering to restore it or starting a host.
pub fn validate_snapshot(snapshot: &PlayableGameSnapshot) -> Result<()> {
    if snapshot.version != heddobureika_core::PLAYABLE_GAME_SNAPSHOT_VERSION {
        return Err("This P2P puzzle uses an incompatible save version".into());
    }
    if !matches!(snapshot.puzzle.image_ref, PuzzleImageRef::BuiltIn { .. }) {
        return Err("P2P currently supports catalog puzzles only".into());
    }
    let PuzzleImageRef::BuiltIn { slug } = &snapshot.puzzle.image_ref else {
        unreachable!()
    };
    let entry = heddobureika_core::puzzle_by_slug(slug)
        .ok_or("This puzzle is missing from your catalog")?;
    let dimensions = heddobureika_core::logical_image_size(
        entry.width,
        entry.height,
        snapshot.rules.image_max_dimension,
    );
    if dimensions != (snapshot.puzzle.image_width, snapshot.puzzle.image_height) {
        return Err("This P2P puzzle is incompatible with your catalog".into());
    }
    if snapshot.state.topology_piece_count > 20_000
        || snapshot.puzzle.topology.payload.len() > 256 * 1024
    {
        return Err("This puzzle is too large for experimental P2P".into());
    }
    let piece_count = snapshot.puzzle.piece_count();
    if piece_count == 0
        || piece_count > 20_000
        || piece_count != snapshot.state.topology_piece_count
        || snapshot.puzzle.topology != snapshot.state.topology
        || snapshot.restore_playable_from_spec().is_err()
    {
        return Err("Invalid P2P puzzle snapshot".into());
    }
    if encode(snapshot).is_none_or(|bytes| bytes.len() > MAX_FRAME / 2) {
        return Err("This puzzle is too large for experimental P2P".into());
    }
    Ok(())
}

pub struct Host {
    endpoint: Endpoint,
    authority: SessionAuthority,
    invite: Invite,
    commands: Receiver<ClientMsg>,
    events: Sender<Event>,
    checkpoints: Receiver<CheckpointRequest>,
    identity: HostIdentity,
    persistent: bool,
}
impl Host {
    pub fn new(
        endpoint: Endpoint,
        room_id: String,
        snapshot: PlayableGameSnapshot,
    ) -> Result<(Self, Handle, Receiver<Event>)> {
        validate_snapshot(&snapshot)?;
        if !heddobureika_core::is_valid_room_id(&room_id) {
            return Err("Invalid P2P session".into());
        }
        let mut authority = SessionAuthority::new();
        if !authority.set_snapshot(snapshot) {
            return Err("Invalid P2P puzzle".into());
        }
        let invite = Invite {
            version: VERSION,
            room_id,
            address: endpoint.addr(),
            secret: SecretKey::generate().to_bytes(),
        };
        let (tx, commands) = async_channel::bounded(QUEUE);
        let (events, rx) = async_channel::bounded(QUEUE);
        let (checkpoint_tx, checkpoints) = async_channel::bounded(1);
        let identity = HostIdentity {
            room_id: invite.room_id.clone(),
            key: endpoint.secret_key().to_bytes(),
            invite: invite.encode(),
            guest: None,
        };
        let handle = Handle {
            commands: tx,
            endpoint: endpoint.clone(),
            checkpoints: Some(checkpoint_tx),
        };
        Ok((
            Self {
                endpoint,
                authority,
                invite,
                commands,
                events,
                checkpoints,
                identity,
                persistent: false,
            },
            handle,
            rx,
        ))
    }
    pub fn identity(&self) -> HostIdentity {
        self.identity.clone()
    }
    pub fn snapshot(&self) -> PlayableGameSnapshot {
        self.authority.snapshot().unwrap().clone()
    }
    /// Enable durable guest admission after the adapter has saved the initial
    /// identity/checkpoint, and before publishing its invite.
    pub fn require_persistence(&mut self) {
        self.persistent = true;
    }
    pub fn resume(
        endpoint: Endpoint,
        identity: HostIdentity,
        snapshot: PlayableGameSnapshot,
    ) -> Result<(Self, Handle, Receiver<Event>)> {
        identity.validate()?;
        if endpoint.id() != identity.key().public() {
            return Err("P2P endpoint does not match the saved room".into());
        }
        let (mut host, handle, events) = Self::new(endpoint, identity.room_id.clone(), snapshot)?;
        host.invite = Invite::parse(identity.invite())?;
        host.identity = identity;
        host.persistent = true;
        Ok((host, handle, events))
    }
    /// For native harnesses with direct addresses. Browser callers wait for
    /// `Event::Invite`, after the relay is online, before sharing a link.
    pub fn invite(&self) -> String {
        self.invite.encode()
    }
    pub async fn run(mut self) {
        let (net_tx, net_rx) = async_channel::bounded(QUEUE);
        let mut tasks = JoinSet::new();
        let started = Instant::now();
        let now = || started.elapsed().as_millis().min(i64::MAX as u128) as i64;
        let mut tick = time::interval(Duration::from_millis(250));
        let mut admitted = self
            .identity
            .guest
            .map(|key| iroh::EndpointId::from_bytes(&key).expect("validated guest"));
        let mut peer: Option<Peer> = None;
        let mut generation = 0u64;
        let mut host_seq = Sequence::default();
        let mut guest_seq = Sequence::default();
        let mut pending = 0usize;
        let mut disconnected_at = None;
        let mut invitation_ready = false;
        let mut paused = false;
        // Publish a fresh relay-bearing address without blocking host gameplay.
        let endpoint = self.endpoint.clone();
        let tx = net_tx.clone();
        tasks.spawn(async move {
            if time::timeout(Duration::from_secs(20), endpoint.online())
                .await
                .is_ok()
            {
                let _ = tx.send(Network::Online).await;
            } else {
                let _ = tx.send(Network::Offline).await;
            }
        });
        let _ = self.events.send(Event::Reset(self.state(HOST, true))).await;
        let mut saved_seq = None;
        self.checkpoint(&mut saved_seq).await;
        'running: loop {
            futures_util::select_biased! {
                command = self.commands.recv().fuse() => {
                    let Ok(command) = command else { break };
                    if paused { continue; }
                    let (id, claim) = host_seq.stamp(&command);
                    let messages = self.apply(HOST, id, claim, command, &mut host_seq, now());
                    self.broadcast(messages, &mut peer, &self.events).await;
                },
                request = self.checkpoints.recv().fuse() => {
                    if let Ok(request) = request {
                        if request.pause {
                            paused = true;
                            if let Some(old) = peer.take() { old.conn.close(0u8.into(), b"host reloading"); }
                            let _ = self.events.send(Event::Status { message: "P2P paused · preparing reload".into(), connected: false }).await;
                        }
                        let _ = request.reply.send(self.authority.snapshot().unwrap().clone()).await;
                    }
                },
                network = net_rx.recv().fuse() => {
                    let Ok(network) = network else { break };
                    match network {
                        Network::Online => {
                            if paused { continue; }
                            if !self.persistent { self.invite.address = self.endpoint.addr(); }
                            invitation_ready = true;
                            let _ = self.events.send(Event::Invite(self.invite.encode())).await;
                            let _ = self.events.send(Event::Status { message: "Hosting P2P · waiting for a friend".into(), connected: true }).await;
                        }
                        Network::Offline => { let _ = self.events.send(Event::Status { message: "P2P relay unavailable · leave and try again".into(), connected: true }).await; }
                        Network::Handshake(result) => {
                            pending = pending.saturating_sub(1);
                            let Some((conn, send, recv, hello)) = result else { continue };
                            if paused { conn.close(0u8.into(), b"host reloading"); continue; }
                            let ClientFrame::Hello { version, room_id, secret } = hello else { conn.close(1u8.into(), b"handshake required"); continue };
                            let rejection = if version != VERSION { Some("P2P version mismatch") }
                                else if room_id != self.invite.room_id || secret != self.invite.secret { Some("Invalid P2P invite") }
                                else if admitted.is_some_and(|id| id != conn.remote_id()) { Some("This puzzle already has two players") }
                                else { None };
                            if let Some(message) = rejection {
                                tasks.spawn(async move { let mut send = send; let _ = write_server(&mut send, &ServerFrame::Rejected(message.into())).await; let _ = send.finish(); let _ = time::timeout(Duration::from_secs(2), send.stopped()).await; conn.close(1u8.into(), b"rejected"); });
                                continue;
                            }
                            if admitted.is_none() {
                                self.identity.guest = Some(*conn.remote_id().as_bytes());
                                if self.persistent {
                                    let (committed, result) = async_channel::bounded(1);
                                    let _ = self.events.send(Event::PersistRoom {
                                        identity: self.identity.clone(),
                                        snapshot: self.authority.snapshot().unwrap().clone(), committed,
                                    }).await;
                                    if !matches!(time::timeout(Duration::from_secs(10), result.recv()).await, Ok(Ok(Ok(())))) {
                                        conn.close(1u8.into(), b"room save failed");
                                        let _ = self.events.send(Event::Rejected("Could not save P2P room admission; reload to retry".into())).await;
                                        break 'running;
                                    }
                                }
                            }
                            admitted = Some(conn.remote_id());
                            if let Some(old) = peer.take() { old.conn.close(CLOSE_REPLACED.into(), b"replaced"); }
                            generation += 1;
                            guest_seq = Sequence::default();
                            disconnected_at = None;
                            if let Some(event) = self.authority.release_client(GUEST, OwnershipReason::Released) {
                                let _ = self.events.send(Event::Messages(vec![event])).await;
                            }
                            let (outgoing, rx) = async_channel::bounded(QUEUE);
                            let _ = outgoing.try_send(ServerFrame::Reset(self.state(GUEST, true)));
                            pump(&mut tasks, conn.clone(), send, recv, generation, net_tx.clone(), rx);
                            peer = Some(Peer { conn, outgoing, generation });
                            let _ = self.events.send(Event::Status { message: "Hosting P2P · friend connected".into(), connected: true }).await;
                        }
                        Network::Frame(epoch, frame) if peer.as_ref().is_some_and(|p| p.generation == epoch) => {
                            if let ClientFrame::Command { id, claim, command } = frame {
                                let messages = self.apply(GUEST, id, claim, command, &mut guest_seq, now());
                                self.broadcast(messages, &mut peer, &self.events).await;
                            }
                        }
                        Network::Gone(epoch) if peer.as_ref().is_some_and(|p| p.generation == epoch) => {
                            if let Some(old) = peer.take() { old.conn.close(0u8.into(), b"disconnected"); }
                            disconnected_at = Some(now());
                            let _ = self.events.send(Event::Status { message: "Hosting P2P · waiting for your friend to reconnect".into(), connected: true }).await;
                        }
                        _ => {}
                    }
                },
                incoming = self.endpoint.accept().fuse() => {
                    let Some(incoming) = incoming else { break };
                    if paused { incoming.refuse(); continue; }
                    if pending >= 4 { incoming.refuse(); continue; }
                    pending += 1;
                    let tx = net_tx.clone();
                    tasks.spawn(async move {
                        let result = time::timeout(Duration::from_secs(10), async {
                            let conn = incoming.await.ok()?;
                            let (send, mut recv) = conn.accept_bi().await.ok()?;
                            let frame = read_client(&mut recv).await.ok()?;
                            Some((conn, send, recv, frame))
                        }).await.ok().flatten();
                        let _ = tx.send(Network::Handshake(result)).await;
                    });
                },
                _ = tick.tick().fuse() => {
                    if paused { continue; }
                    self.checkpoint(&mut saved_seq).await;
                    let mut events = self.authority.expire_ownership(now());
                    if disconnected_at.is_some_and(|at| now().saturating_sub(at) >= 5000) {
                        disconnected_at = None;
                        if let Some(event) = self.authority.release_client(GUEST, OwnershipReason::Released) { events.push(event); }
                    }
                    if !events.is_empty() { self.broadcast(ServerFrame::Events(events), &mut peer, &self.events).await; }
                    while let Some(Some(_)) = n0_future::future::now_or_never(tasks.join_next()) {}
                    if invitation_ready && !self.persistent {
                        // Tickets retain the secret but follow relay changes.
                        let addr = self.endpoint.addr();
                        if addr != self.invite.address { self.invite.address = addr; let _ = self.events.send(Event::Invite(self.invite.encode())).await; }
                    }
                }
            }
        }
        self.checkpoint(&mut saved_seq).await;
        tasks.abort_all();
        if let Some(peer) = peer {
            peer.conn.close(0u8.into(), b"host left");
        }
        self.endpoint.close().await;
    }
    async fn checkpoint(&self, saved_seq: &mut Option<u64>) {
        let snapshot = self.authority.snapshot().expect("initialised host");
        if *saved_seq != Some(snapshot.seq) {
            *saved_seq = Some(snapshot.seq);
            let _ = self.events.send(Event::Checkpoint(snapshot.clone())).await;
        }
    }
    fn state(&self, actor: ClientId, welcome: bool) -> Vec<ServerMsg> {
        let snapshot = self.authority.snapshot().expect("initialised host").clone();
        let mut messages = Vec::new();
        if welcome {
            messages.push(ServerMsg::Welcome {
                room_id: self.invite.room_id.clone(),
                persistence: RoomPersistence::BestEffort,
                initialized: true,
                client_id: Some(actor),
            });
        }
        messages.push(ServerMsg::State {
            seq: snapshot.seq,
            snapshot,
        });
        for owner in self.authority.ownerships() {
            messages.push(ServerMsg::ControlUpdate {
                seq: self.authority.snapshot().unwrap().seq,
                update: RoomControlUpdate::Ownership {
                    group_anchor: owner.anchor_id,
                    owner: Some(owner.owner_id),
                    reason: OwnershipReason::Granted,
                },
                source: None,
                client_seq: None,
            });
        }
        messages
    }
    fn apply(
        &mut self,
        actor: ClientId,
        id: u64,
        claim: u64,
        command: ClientMsg,
        sequence: &mut Sequence,
        now: i64,
    ) -> ServerFrame {
        if !sequence.accept(id, claim, &command) {
            return ServerFrame::Reset(self.state(actor, true));
        }
        let select = matches!(command, ClientMsg::Select { .. });
        if let Some(effects) = self.authority.apply_exclusive(actor, command, now) {
            if select {
                sequence.granted = claim;
            }
            if self.authority.ownerships().all(|o| o.owner_id != actor) {
                sequence.granted = 0;
            }
            ServerFrame::Events(effects.events)
        } else {
            ServerFrame::Reset(self.state(actor, true))
        }
    }
    async fn broadcast(&self, frame: ServerFrame, peer: &mut Option<Peer>, events: &Sender<Event>) {
        // Reset is a rejection/resync for the requester. Both clients receive
        // their own IDs and canonical state, never the other player's Welcome.
        match frame {
            ServerFrame::Reset(messages) => {
                let actor = messages.iter().find_map(|m| {
                    if let ServerMsg::Welcome { client_id, .. } = m {
                        *client_id
                    } else {
                        None
                    }
                });
                if actor == Some(HOST) {
                    let _ = events.send(Event::Reset(messages)).await;
                } else if let Some(peer) = peer {
                    if peer
                        .outgoing
                        .try_send(ServerFrame::Reset(messages))
                        .is_err()
                    {
                        peer.conn.close(1u8.into(), b"backpressure");
                    }
                }
            }
            ServerFrame::Events(messages) => {
                let _ = events.send(Event::Messages(messages.clone())).await;
                if let Some(peer) = peer {
                    if peer
                        .outgoing
                        .try_send(ServerFrame::Events(messages))
                        .is_err()
                    {
                        peer.conn.close(1u8.into(), b"backpressure");
                    }
                }
            }
            ServerFrame::Rejected(_) => unreachable!(),
        }
    }
}

#[derive(Default)]
struct Sequence {
    next: u64,
    tentative: u64,
    last: u64,
    granted: u64,
}
impl Sequence {
    fn stamp(&mut self, command: &ClientMsg) -> (u64, u64) {
        self.next += 1;
        if matches!(command, ClientMsg::Select { .. }) {
            self.tentative = self.next;
        }
        (self.next, self.tentative)
    }
    fn accept(&mut self, id: u64, claim: u64, command: &ClientMsg) -> bool {
        if id <= self.last || id == 0 {
            return false;
        }
        self.last = id;
        match command {
            ClientMsg::Select { .. } => claim == id,
            ClientMsg::Init { .. } | ClientMsg::AssetRequest { .. } | ClientMsg::Ping { .. } => {
                false
            }
            _ => claim != 0 && claim == self.granted,
        }
    }
}
struct Peer {
    conn: Connection,
    outgoing: Sender<ServerFrame>,
    generation: u64,
}
enum Network {
    Online,
    Offline,
    Handshake(Option<(Connection, SendStream, RecvStream, ClientFrame)>),
    Frame(u64, ClientFrame),
    Gone(u64),
}
fn pump(
    tasks: &mut JoinSet<()>,
    conn: Connection,
    mut send: SendStream,
    mut recv: RecvStream,
    generation: u64,
    net: Sender<Network>,
    outgoing: Receiver<ServerFrame>,
) {
    let read_conn = conn.clone();
    tasks.spawn(async move {
        while let Ok(frame) = read_client(&mut recv).await {
            if net.send(Network::Frame(generation, frame)).await.is_err() {
                break;
            }
        }
        read_conn.close(0u8.into(), b"reader ended");
        let _ = net.send(Network::Gone(generation)).await;
    });
    tasks.spawn(async move {
        let writer = async {
            while let Ok(frame) = outgoing.recv().await {
                if write_server(&mut send, &frame).await.is_err() {
                    break;
                }
            }
        };
        futures_util::pin_mut!(writer);
        let _ = futures_util::future::select(writer, Box::pin(conn.closed())).await;
        conn.close(0u8.into(), b"writer ended");
    });
}

pub struct Guest {
    endpoint: Endpoint,
    invite: Invite,
    commands: Receiver<ClientMsg>,
    events: Sender<Event>,
}
impl Guest {
    pub fn new(endpoint: Endpoint, invite: Invite) -> (Self, Handle, Receiver<Event>) {
        let (commands, rx) = async_channel::bounded(QUEUE);
        let (events, output) = async_channel::bounded(QUEUE);
        let handle = Handle {
            commands,
            endpoint: endpoint.clone(),
            checkpoints: None,
        };
        (
            Self {
                endpoint,
                invite,
                commands: rx,
                events,
            },
            handle,
            output,
        )
    }
    pub async fn run(self) {
        loop {
            let _ = self
                .events
                .send(Event::Status {
                    message: "Connecting to P2P host".into(),
                    connected: false,
                })
                .await;
            let connect = time::timeout(
                Duration::from_secs(15),
                self.endpoint.connect(self.invite.address.clone(), ALPN),
            );
            let result =
                futures_util::future::select(Box::pin(connect), Box::pin(self.endpoint.closed()))
                    .await;
            let conn = match result {
                futures_util::future::Either::Left((Ok(Ok(conn)), _)) => conn,
                futures_util::future::Either::Right(_) => break,
                _ => {
                    let _ = self
                        .events
                        .send(Event::Status {
                            message: "Waiting for host · retrying".into(),
                            connected: false,
                        })
                        .await;
                    time::sleep(Duration::from_secs(2)).await;
                    if self.commands.is_closed() {
                        break;
                    }
                    continue;
                }
            };
            let result = self.connected(&conn).await;
            let result = match conn.close_reason() {
                Some(ConnectionError::ApplicationClosed(close))
                    if close.error_code == CLOSE_REPLACED.into() =>
                {
                    Err(
                        "P2P session opened in another tab or app; this connection has stopped"
                            .into(),
                    )
                }
                _ => result,
            };
            conn.close(0u8.into(), b"guest reconnecting");
            if self.commands.is_closed() {
                break;
            }
            if let Err(message) = result {
                let _ = self.events.send(Event::Rejected(message)).await;
                break;
            }
            let _ = self
                .events
                .send(Event::Status {
                    message: "Waiting for host · retrying".into(),
                    connected: false,
                })
                .await;
            while self.commands.try_recv().is_ok() {}
            time::sleep(Duration::from_secs(1)).await;
        }
        self.endpoint.close().await;
    }
    async fn connected(&self, conn: &Connection) -> Result<()> {
        let Ok((mut send, mut recv)) = conn.open_bi().await else {
            return Ok(());
        };
        if write_client(
            &mut send,
            &ClientFrame::Hello {
                version: VERSION,
                room_id: self.invite.room_id.clone(),
                secret: self.invite.secret,
            },
        )
        .await
        .is_err()
        {
            return Ok(());
        }
        let initial = match time::timeout(Duration::from_secs(10), read_server(&mut recv)).await {
            Ok(Ok(frame)) => frame,
            _ => return Ok(()),
        };
        match initial {
            ServerFrame::Rejected(message) => return Err(message),
            ServerFrame::Reset(messages) => {
                let snapshot = messages
                    .iter()
                    .find_map(|m| {
                        if let ServerMsg::State { snapshot, .. } = m {
                            Some(snapshot)
                        } else {
                            None
                        }
                    })
                    .ok_or("P2P host did not send a puzzle")?;
                validate_snapshot(snapshot)?;
                let _ = self.events.send(Event::Reset(messages)).await;
            }
            _ => return Err("Invalid P2P handshake".into()),
        }
        let _ = self
            .events
            .send(Event::Status {
                message: "P2P connected".into(),
                connected: true,
            })
            .await;
        let mut sequence = Sequence::default();
        let reader = async {
            loop {
                let frame = match read_server(&mut recv).await {
                    Ok(frame) => frame,
                    Err(_) => return Ok(()),
                };
                let event = match frame {
                    ServerFrame::Events(m) => Event::Messages(m),
                    ServerFrame::Reset(m) => Event::Reset(m),
                    ServerFrame::Rejected(message) => return Err(message),
                };
                if self.events.send(event).await.is_err() {
                    return Ok(());
                }
            }
        };
        let writer = async {
            while let Ok(command) = self.commands.recv().await {
                let (id, claim) = sequence.stamp(&command);
                if write_client(&mut send, &ClientFrame::Command { id, claim, command })
                    .await
                    .is_err()
                {
                    break;
                }
            }
        };
        futures_util::pin_mut!(reader, writer);
        match futures_util::future::select(reader, writer).await {
            futures_util::future::Either::Left((result, _)) => result,
            _ => Ok(()),
        }
    }
}

async fn write_raw(send: &mut SendStream, bytes: Vec<u8>) -> Result<()> {
    if bytes.len() > MAX_FRAME {
        return Err("P2P message is too large".into());
    }
    send.write_all(&(bytes.len() as u32).to_be_bytes())
        .await
        .map_err(|_| "P2P disconnected")?;
    send.write_all(&bytes)
        .await
        .map_err(|_| "P2P disconnected".into())
}
async fn read_raw(recv: &mut RecvStream) -> Result<Vec<u8>> {
    let mut prefix = [0; 4];
    recv.read_exact(&mut prefix)
        .await
        .map_err(|_| "P2P disconnected")?;
    let len = u32::from_be_bytes(prefix) as usize;
    if len == 0 || len > MAX_FRAME {
        return Err("Invalid P2P message size".into());
    }
    let mut bytes = vec![0; len];
    recv.read_exact(&mut bytes)
        .await
        .map_err(|_| "P2P disconnected")?;
    Ok(bytes)
}
async fn write_client(send: &mut SendStream, frame: &ClientFrame) -> Result<()> {
    write_raw(send, encode(frame).ok_or("Could not encode P2P command")?).await
}
async fn read_client(recv: &mut RecvStream) -> Result<ClientFrame> {
    decode(&read_raw(recv).await?).ok_or("Invalid P2P command".into())
}
async fn write_server(send: &mut SendStream, frame: &ServerFrame) -> Result<()> {
    write_raw(send, encode(frame).ok_or("Could not encode P2P update")?).await
}
async fn read_server(recv: &mut RecvStream) -> Result<ServerFrame> {
    decode(&read_raw(recv).await?).ok_or("Invalid P2P update".into())
}

#[cfg(test)]
mod tests;
