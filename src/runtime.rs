use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use std::collections::hash_map::DefaultHasher;

use crate::app_core::AppSnapshot;
use crate::core::InitMode;
use crate::local_snapshot::{build_game_snapshot_from_app, clear_local_snapshot, load_local_snapshot, save_local_snapshot};
pub use heddobureika_core::{CoreAction, SyncAction};
use heddobureika_core::{ClientId, GameSnapshot, RoomPersistence, RoomUpdate};

#[derive(Clone)]
pub struct ViewHooks {
    pub on_action: Rc<dyn Fn(CoreAction)>,
}

#[derive(Clone)]
pub struct SyncHooks {
    pub on_remote_action: Rc<dyn Fn(CoreAction)>,
    pub on_snapshot: Rc<dyn Fn(AppSnapshot)>,
    pub on_remote_snapshot: Rc<dyn Fn(GameSnapshot, u64)>,
    pub on_remote_update: Rc<dyn Fn(RoomUpdate, u64, Option<ClientId>, Option<u64>)>,
    pub on_event: Rc<dyn Fn(SyncEvent)>,
}

impl SyncHooks {
    pub fn empty() -> Self {
        Self {
            on_remote_action: Rc::new(|_| {}),
            on_snapshot: Rc::new(|_| {}),
            on_remote_snapshot: Rc::new(|_, _| {}),
            on_remote_update: Rc::new(|_, _, _, _| {}),
            on_event: Rc::new(|_| {}),
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
    Warning { minutes_idle: u32 },
    Ownership { anchor_id: u32, owner: Option<ClientId> },
    DropNotReady,
    Error { code: String, message: String },
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
    pending_snapshot: Option<heddobureika_core::GameSnapshot>,
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

    pub fn take_pending_snapshot(&mut self) -> Option<heddobureika_core::GameSnapshot> {
        self.ensure_pending_loaded();
        self.pending_snapshot.take()
    }

    pub fn requeue_pending_snapshot(&mut self, snapshot: heddobureika_core::GameSnapshot) {
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
        let Some(game_snapshot) = build_game_snapshot_from_app(snapshot) else {
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
    let cols = info.cols as usize;
    let rows = info.rows as usize;
    let total = cols * rows;
    if total == 0 {
        return None;
    }
    if snapshot.core.positions.len() != total
        || snapshot.core.rotations.len() != total
        || snapshot.core.flips.len() != total
        || snapshot.core.connections.len() != total
    {
        return None;
    }
    let mut hasher = DefaultHasher::new();
    info.label.hash(&mut hasher);
    info.image_src.hash(&mut hasher);
    info.rows.hash(&mut hasher);
    info.cols.hash(&mut hasher);
    info.shape_seed.hash(&mut hasher);
    info.image_width.hash(&mut hasher);
    info.image_height.hash(&mut hasher);
    snapshot.core.scramble_nonce.hash(&mut hasher);
    for (x, y) in &snapshot.core.positions {
        x.to_bits().hash(&mut hasher);
        y.to_bits().hash(&mut hasher);
    }
    for rot in &snapshot.core.rotations {
        rot.to_bits().hash(&mut hasher);
    }
    for flip in &snapshot.core.flips {
        flip.hash(&mut hasher);
    }
    for conn in &snapshot.core.connections {
        conn.hash(&mut hasher);
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
