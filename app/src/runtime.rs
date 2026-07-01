use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use std::collections::hash_map::DefaultHasher;

use crate::app_core::AppSnapshot;
use crate::core::InitMode;
use crate::local_snapshot::{
    build_playable_snapshot_from_app, clear_local_snapshot, load_local_snapshot,
    save_local_snapshot,
};
use heddobureika_core::{
    ClientId, PlayableGameSnapshot, PlayableRoomUpdate, RoomControlUpdate, RoomPersistence,
};
pub use heddobureika_core::{CoreAction, SyncAction};

#[derive(Clone)]
pub struct ViewHooks {
    pub on_action: Rc<dyn Fn(CoreAction)>,
}

/// Why a multiplayer connection attempt failed. The retry scheduler uses this
/// to decide whether to back off and retry or give up and fall back to the
/// local game.
///
/// A browser WebSocket cannot observe the HTTP status of a failed *upgrade*
/// handshake (a 403 room-gone rejection surfaces as an opaque code-1006 close,
/// indistinguishable from a network blip), so `NeverOpened` is deliberately
/// ambiguous — the scheduler resolves it with an explicit HTTP status probe.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FailReason {
    /// The attempt could not even start (misconfiguration, unusable identity).
    /// Retrying will not help; fall back to local immediately.
    Terminal,
    /// The socket opened at least once and then closed. The room was reachable,
    /// so this is most likely a transient network drop worth the full backoff.
    Dropped,
    /// The socket never opened. Could be a gone/expired room (403 on the
    /// upgrade) or a transient failure; the scheduler probes to decide.
    NeverOpened,
}

#[derive(Clone)]
pub struct SyncHooks {
    pub on_remote_action: Rc<dyn Fn(CoreAction)>,
    pub on_snapshot: Rc<dyn Fn(AppSnapshot)>,
    pub on_remote_snapshot: Rc<dyn Fn(PlayableGameSnapshot, u64)>,
    pub on_remote_update: Rc<dyn Fn(RoomControlUpdate, u64, Option<ClientId>, Option<u64>)>,
    pub on_remote_playable_update:
        Rc<dyn Fn(PlayableRoomUpdate, u64, Option<ClientId>, Option<u64>)>,
    pub on_event: Rc<dyn Fn(SyncEvent)>,
    pub on_asset: Rc<dyn Fn(AssetEvent)>,
}

impl SyncHooks {
    pub fn empty() -> Self {
        Self {
            on_remote_action: Rc::new(|_| {}),
            on_snapshot: Rc::new(|_| {}),
            on_remote_snapshot: Rc::new(|_, _| {}),
            on_remote_update: Rc::new(|_, _, _, _| {}),
            on_remote_playable_update: Rc::new(|_, _, _, _| {}),
            on_event: Rc::new(|_| {}),
            on_asset: Rc::new(|_| {}),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SyncEvent {
    Connected {
        room_id: Option<String>,
        persistence: Option<RoomPersistence>,
        initialized: bool,
        client_id: Option<ClientId>,
    },
    NeedInit,
    Warning {
        minutes_idle: u32,
    },
    Ownership {
        anchor_id: u32,
        owner: Option<ClientId>,
    },
    DropNotReady,
    Error {
        code: String,
        message: String,
    },
}

#[derive(Clone, Debug)]
pub enum AssetEvent {
    Begin {
        hash: String,
        mime: String,
        width: u32,
        height: u32,
        size: u32,
    },
    Chunk {
        hash: String,
        index: u32,
        bytes: Vec<u8>,
    },
    End {
        hash: String,
    },
    UploadAck {
        hash: String,
    },
}

pub trait GameSyncView {
    fn mode(&self) -> InitMode;
    fn connected(&self) -> bool;
    fn client_id(&self) -> Option<ClientId>;
    fn init_required(&self) -> bool;
    fn room_id(&self) -> Option<&str>;
    fn persistence(&self) -> Option<RoomPersistence>;
    fn ownership_by_anchor(&self) -> Rc<HashMap<u32, ClientId>>;
}

#[derive(Clone, Debug)]
pub struct SyncView {
    mode: InitMode,
    connected: bool,
    client_id: Option<ClientId>,
    init_required: bool,
    room_id: Option<String>,
    persistence: Option<RoomPersistence>,
    ownership_by_anchor: Rc<HashMap<u32, ClientId>>,
}

impl Default for SyncView {
    fn default() -> Self {
        Self {
            mode: InitMode::Local,
            connected: false,
            client_id: None,
            init_required: false,
            room_id: None,
            persistence: None,
            ownership_by_anchor: Rc::new(HashMap::new()),
        }
    }
}

impl SyncView {
    pub(crate) fn new(
        mode: InitMode,
        connected: bool,
        client_id: Option<ClientId>,
        init_required: bool,
        room_id: Option<String>,
        persistence: Option<RoomPersistence>,
        ownership_by_anchor: Rc<HashMap<u32, ClientId>>,
    ) -> Self {
        Self {
            mode,
            connected,
            client_id,
            init_required,
            room_id,
            persistence,
            ownership_by_anchor,
        }
    }

    pub fn mode(&self) -> InitMode {
        self.mode
    }

    pub fn connected(&self) -> bool {
        self.connected
    }

    pub fn client_id(&self) -> Option<ClientId> {
        self.client_id
    }

    pub fn init_required(&self) -> bool {
        self.init_required
    }

    pub fn room_id(&self) -> Option<&str> {
        self.room_id.as_deref()
    }

    pub fn persistence(&self) -> Option<RoomPersistence> {
        self.persistence
    }

    pub fn ownership_by_anchor(&self) -> Rc<HashMap<u32, ClientId>> {
        self.ownership_by_anchor.clone()
    }
}

impl GameSyncView for SyncView {
    fn mode(&self) -> InitMode {
        self.mode()
    }

    fn connected(&self) -> bool {
        self.connected()
    }

    fn client_id(&self) -> Option<ClientId> {
        self.client_id()
    }

    fn init_required(&self) -> bool {
        self.init_required()
    }

    fn room_id(&self) -> Option<&str> {
        self.room_id()
    }

    fn persistence(&self) -> Option<RoomPersistence> {
        self.persistence()
    }

    fn ownership_by_anchor(&self) -> Rc<HashMap<u32, ClientId>> {
        self.ownership_by_anchor()
    }
}

pub trait GameView {
    fn init(&mut self, hooks: ViewHooks);
    fn render(&mut self, snapshot: &AppSnapshot, sync_view: &dyn GameSyncView);
}

pub trait GameSync {
    fn init(&mut self, hooks: SyncHooks);
    fn handle_local_action(&mut self, action: &CoreAction);
    fn shutdown(&mut self);
    fn sync_view(&self) -> SyncView;
}

pub struct LocalSyncAdapter {
    hooks: Option<SyncHooks>,
    observer: Option<Rc<dyn Fn(&CoreAction)>>,
    pending_snapshot: Option<PlayableGameSnapshot>,
    pending_loaded: bool,
    last_saved_fingerprint: Option<u64>,
}

impl LocalSyncAdapter {
    pub fn new() -> Self {
        Self {
            hooks: None,
            observer: None,
            pending_snapshot: None,
            pending_loaded: false,
            last_saved_fingerprint: None,
        }
    }

    pub fn set_observer(&mut self, observer: Option<Rc<dyn Fn(&CoreAction)>>) {
        self.observer = observer;
    }

    pub fn take_pending_snapshot(&mut self) -> Option<PlayableGameSnapshot> {
        self.ensure_pending_loaded();
        self.pending_snapshot.take()
    }

    pub fn requeue_pending_snapshot(&mut self, snapshot: PlayableGameSnapshot) {
        self.pending_snapshot = Some(snapshot);
    }

    pub fn save_if_needed(&mut self, snapshot: &AppSnapshot) {
        self.maybe_save(snapshot);
    }

    pub fn clear_saved_snapshot(&mut self) {
        clear_local_snapshot();
        self.pending_snapshot = None;
        self.pending_loaded = true;
        self.last_saved_fingerprint = None;
    }

    pub fn clear_storage() {
        clear_local_snapshot();
    }

    fn ensure_pending_loaded(&mut self) {
        if self.pending_loaded {
            return;
        }
        self.pending_snapshot = load_local_snapshot();
        self.pending_loaded = true;
    }

    fn maybe_save(&mut self, snapshot: &AppSnapshot) {
        let fingerprint = snapshot_fingerprint(snapshot);
        if fingerprint.is_none() {
            return;
        }
        if self.last_saved_fingerprint == fingerprint {
            return;
        }
        let Some(game_snapshot) = build_playable_snapshot_from_app(snapshot) else {
            return;
        };
        save_local_snapshot(&game_snapshot);
        self.last_saved_fingerprint = fingerprint;
    }
}

impl Default for LocalSyncAdapter {
    fn default() -> Self {
        Self::new()
    }
}

fn snapshot_fingerprint(snapshot: &AppSnapshot) -> Option<u64> {
    let info = snapshot.puzzle_info.as_ref()?;
    let game = snapshot.game.as_ref()?;
    let mut hasher = DefaultHasher::new();
    info.label.hash(&mut hasher);
    info.image_ref.hash(&mut hasher);
    // The topology spec is opaque from the runtime's perspective; we
    // hash the tag and the raw payload bytes so any topology that
    // round-trips through the snapshot codec ends up with a stable cache
    // fingerprint.
    info.topology.tag.hash(&mut hasher);
    info.topology.payload.hash(&mut hasher);
    info.shape_seed.hash(&mut hasher);
    info.image_width.hash(&mut hasher);
    info.image_height.hash(&mut hasher);
    game.scramble_nonce.hash(&mut hasher);
    game.playable.revision.hash(&mut hasher);
    for pose in game.visual.piece_visual_pose() {
        pose.x_mm().to_bits().hash(&mut hasher);
        pose.y_mm().to_bits().hash(&mut hasher);
        pose.rotation_degrees().to_bits().hash(&mut hasher);
    }
    for id in &snapshot.z_order {
        id.hash(&mut hasher);
    }
    Some(hasher.finish())
}

impl GameSync for LocalSyncAdapter {
    fn init(&mut self, hooks: SyncHooks) {
        self.hooks = Some(hooks);
    }

    fn handle_local_action(&mut self, action: &CoreAction) {
        if let Some(observer) = self.observer.as_ref() {
            observer(action);
        }
    }

    fn shutdown(&mut self) {
        self.hooks = None;
    }

    fn sync_view(&self) -> SyncView {
        SyncView::default()
    }
}
