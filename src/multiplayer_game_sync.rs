use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use gloo::console;
use wasm_bindgen_futures::spawn_local;

use crate::app_router;
use crate::runtime::{AssetEvent, CoreAction, GameSync, SyncAction, SyncEvent, SyncHooks, SyncView};
use crate::core::InitMode;
use crate::multiplayer_identity;
use crate::multiplayer_sync::MultiplayerSyncAdapter;
use heddobureika_core::{ClientId, ClientMsg, RoomPersistence, RoomUpdate, ServerMsg};

pub(crate) struct MultiplayerGameSync {
    adapter: MultiplayerSyncAdapter,
    last_seq: Rc<Cell<u64>>,
    next_client_seq: Rc<Cell<u64>>,
    last_sent_by_anchor: Rc<RefCell<HashMap<u32, u64>>>,
    state_applied: Rc<Cell<bool>>,
    handler: RefCell<Option<Rc<dyn Fn(ServerMsg)>>>,
    connected: Rc<Cell<bool>>,
    client_id: Rc<Cell<Option<ClientId>>>,
    connect_seq: Rc<Cell<u64>>,
    init_required: Rc<Cell<bool>>,
    room_id: Rc<RefCell<Option<String>>>,
    persistence: Rc<Cell<Option<RoomPersistence>>>,
    ownership_by_anchor: Rc<RefCell<Rc<HashMap<u32, ClientId>>>>,
    ownership_seq_by_anchor: Rc<RefCell<HashMap<u32, u64>>>,
    hooks: RefCell<SyncHooks>,
    local_transform_observer:
        Rc<RefCell<Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64)>>>>,
    local_flip_observer: Rc<RefCell<Option<Rc<dyn Fn(u32, bool)>>>>,
}

impl MultiplayerGameSync {
    pub(crate) fn new() -> Self {
        Self {
            adapter: MultiplayerSyncAdapter::new(),
            last_seq: Rc::new(Cell::new(0)),
            next_client_seq: Rc::new(Cell::new(0)),
            last_sent_by_anchor: Rc::new(RefCell::new(HashMap::new())),
            state_applied: Rc::new(Cell::new(false)),
            handler: RefCell::new(None),
            connected: Rc::new(Cell::new(false)),
            client_id: Rc::new(Cell::new(None)),
            connect_seq: Rc::new(Cell::new(0)),
            init_required: Rc::new(Cell::new(false)),
            room_id: Rc::new(RefCell::new(None)),
            persistence: Rc::new(Cell::new(None)),
            ownership_by_anchor: Rc::new(RefCell::new(Rc::new(HashMap::new()))),
            ownership_seq_by_anchor: Rc::new(RefCell::new(HashMap::new())),
            hooks: RefCell::new(SyncHooks::empty()),
            local_transform_observer: Rc::new(RefCell::new(None)),
            local_flip_observer: Rc::new(RefCell::new(None)),
        }
    }

    pub(crate) fn connect(
        &mut self,
        room_id: &str,
        on_fail: Rc<dyn Fn()>,
    ) {
        self.reset_state();
        let room_id = room_id.trim();
        if room_id.is_empty() {
            console::warn!("missing room id for multiplayer connect");
            on_fail();
            return;
        }
        let Some(ws_base) = app_router::default_ws_base() else {
            console::warn!("missing websocket base for multiplayer connect");
            on_fail();
            return;
        };
        *self.room_id.borrow_mut() = Some(room_id.to_string());
        let url = app_router::build_room_ws_url(&ws_base, room_id);
        let hooks = self.hooks.borrow().clone();
        let handler = self.install_handler(hooks);
        let mut adapter = self.adapter.clone();
        let on_fail = on_fail.clone();
        let room_id = room_id.to_string();
        let url = url.clone();
        let connect_seq = self.connect_seq.clone();
        let seq = connect_seq.get().wrapping_add(1);
        connect_seq.set(seq);
        spawn_local(async move {
            let protocol = match multiplayer_identity::build_auth_protocol(&room_id, None).await {
                Ok(protocol) => protocol,
                Err(err) => {
                    console::warn!("multiplayer auth failed", err);
                    on_fail();
                    return;
                }
            };
            if connect_seq.get() != seq {
                return;
            }
            adapter.connect_with_open(&url, handler, on_fail, None, Some(vec![protocol]));
        });
    }

    pub(crate) fn disconnect(&mut self) {
        let next = self.connect_seq.get().wrapping_add(1);
        self.connect_seq.set(next);
        self.adapter.disconnect();
        self.reset_state();
        self.handler.borrow_mut().take();
    }

    pub(crate) fn send(&self, msg: ClientMsg) {
        self.adapter.send(msg);
    }

    pub(crate) fn set_local_transform_observer(
        &self,
        observer: Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64)>>,
    ) {
        *self.local_transform_observer.borrow_mut() = observer;
    }

    pub(crate) fn set_local_flip_observer(&self, observer: Option<Rc<dyn Fn(u32, bool)>>) {
        *self.local_flip_observer.borrow_mut() = observer;
    }

    fn next_client_seq_for(&self, anchor_id: u32) -> u64 {
        let next = self.next_client_seq.get().saturating_add(1);
        self.next_client_seq.set(next);
        self.last_sent_by_anchor
            .borrow_mut()
            .insert(anchor_id, next);
        next
    }

    pub(crate) fn set_state_applied(&self, value: bool) {
        self.state_applied.set(value);
    }

    pub(crate) fn install_handler(
        &mut self,
        hooks: SyncHooks,
    ) -> Rc<dyn Fn(ServerMsg)> {
        let handler = self.build_handler(hooks);
        *self.handler.borrow_mut() = Some(handler.clone());
        handler
    }

    fn reset_state(&self) {
        self.last_seq.set(0);
        self.next_client_seq.set(0);
        self.last_sent_by_anchor.borrow_mut().clear();
        self.state_applied.set(false);
        self.connected.set(false);
        self.client_id.set(None);
        self.init_required.set(false);
        *self.room_id.borrow_mut() = None;
        self.persistence.set(None);
        *self.ownership_by_anchor.borrow_mut() = Rc::new(HashMap::new());
        self.ownership_seq_by_anchor.borrow_mut().clear();
    }

    fn build_handler(&self, hooks: SyncHooks) -> Rc<dyn Fn(ServerMsg)> {
        let last_seq = Rc::clone(&self.last_seq);
        let state_applied = Rc::clone(&self.state_applied);
        let connected_cell = Rc::clone(&self.connected);
        let client_id_cell = Rc::clone(&self.client_id);
        let init_required_cell = Rc::clone(&self.init_required);
        let room_id_cell = Rc::clone(&self.room_id);
        let persistence_cell = Rc::clone(&self.persistence);
        let ownership_by_anchor = Rc::clone(&self.ownership_by_anchor);
        let ownership_seq_by_anchor = Rc::clone(&self.ownership_seq_by_anchor);
        let on_event = hooks.on_event.clone();
        let on_asset = hooks.on_asset.clone();
        let on_remote_snapshot = hooks.on_remote_snapshot.clone();
        let on_remote_update = hooks.on_remote_update.clone();
        Rc::new(move |msg: ServerMsg| match msg {
            ServerMsg::Welcome {
                room_id,
                persistence,
                initialized,
                client_id,
            } => {
                console::log!(
                    "multiplayer connected",
                    room_id.clone(),
                    format!("{persistence:?}"),
                    format!("initialized={initialized}"),
                    format!("client_id={:?}", client_id)
                );
                connected_cell.set(true);
                client_id_cell.set(client_id);
                init_required_cell.set(!initialized);
                *room_id_cell.borrow_mut() = Some(room_id.clone());
                persistence_cell.set(Some(persistence));
                *ownership_by_anchor.borrow_mut() = Rc::new(HashMap::new());
                (on_event)(SyncEvent::Connected {
                    room_id: Some(room_id),
                    persistence: Some(persistence),
                    initialized,
                    client_id,
                });
                if !initialized {
                    (on_event)(SyncEvent::NeedInit);
                }
            }
            ServerMsg::AdminAck { room_id, persistence } => {
                *room_id_cell.borrow_mut() = Some(room_id);
                persistence_cell.set(Some(persistence));
            }
            ServerMsg::NeedInit => {
                init_required_cell.set(true);
                (on_event)(SyncEvent::NeedInit);
            }
            ServerMsg::Warning { minutes_idle } => {
                console::warn!("room idle for", minutes_idle, "minutes");
                (on_event)(SyncEvent::Warning { minutes_idle });
            }
            ServerMsg::AssetBegin {
                hash,
                mime,
                width,
                height,
                size,
            } => {
                (on_asset)(AssetEvent::Begin {
                    hash,
                    mime,
                    width,
                    height,
                    size,
                });
            }
            ServerMsg::AssetChunk { hash, index, bytes } => {
                (on_asset)(AssetEvent::Chunk { hash, index, bytes });
            }
            ServerMsg::AssetEnd { hash } => {
                (on_asset)(AssetEvent::End { hash });
            }
            ServerMsg::State { seq, snapshot } => {
                console::log!(
                    "multiplayer state received",
                    seq,
                    snapshot.state.positions.len(),
                    format!("{}x{}", snapshot.puzzle.cols, snapshot.puzzle.rows)
                );
                last_seq.set(seq);
                init_required_cell.set(false);
                (on_remote_snapshot)(snapshot, seq);
            }
            ServerMsg::Update {
                seq,
                update,
                source,
                client_seq,
            } => {
                let kind = update_kind(&update);
                console::log!(
                    "multiplayer update received",
                    seq,
                    kind,
                    format!("source={:?}", source)
                );
                if let RoomUpdate::Ownership { anchor_id, owner, .. } = &update {
                    let should_apply = {
                        let mut seq_map = ownership_seq_by_anchor.borrow_mut();
                        match seq_map.get(anchor_id) {
                            Some(last_seq) if *last_seq >= seq => false,
                            _ => {
                                seq_map.insert(*anchor_id, seq);
                                true
                            }
                        }
                    };
                    if !should_apply {
                        console::warn!(
                            "multiplayer ownership update dropped (stale)",
                            seq,
                            format!("anchor={anchor_id}")
                        );
                        return;
                    }
                    let next = {
                        let current = ownership_by_anchor.borrow();
                        let mut next = (**current).clone();
                        if let Some(owner_id) = owner {
                            next.insert(*anchor_id, *owner_id);
                        } else {
                            next.remove(anchor_id);
                        }
                        next
                    };
                    *ownership_by_anchor.borrow_mut() = Rc::new(next);
                    #[cfg(target_arch = "wasm32")]
                    {
                        crate::wgpu_app::request_render();
                    }
                    (on_event)(SyncEvent::Ownership {
                        anchor_id: *anchor_id,
                        owner: *owner,
                    });
                    return;
                }
                let last = last_seq.get();
                if seq <= last {
                    console::warn!("multiplayer update dropped (stale)", seq, last, kind);
                    return;
                }
                last_seq.set(seq);
                if !state_applied.get() {
                    console::warn!("multiplayer update dropped (not ready)", seq, kind);
                    (on_event)(SyncEvent::DropNotReady);
                    return;
                }
                (on_remote_update)(update, seq, source, client_seq);
            }
            ServerMsg::Pong { .. } => {}
            ServerMsg::UploadAck { hash } => {
                (on_asset)(AssetEvent::UploadAck { hash });
            }
            ServerMsg::Error { code, message } => {
                console::warn!("server error", code.clone(), message.clone());
                (on_event)(SyncEvent::Error { code, message });
            }
        })
    }
}

impl GameSync for MultiplayerGameSync {
    fn init(&mut self, hooks: SyncHooks) {
        *self.hooks.borrow_mut() = hooks;
    }

    fn handle_local_action(&mut self, action: &CoreAction) {
        match action {
            CoreAction::BeginDrag { piece_id, .. } => {
                self.send(ClientMsg::Select {
                    piece_id: *piece_id as u32,
                });
            }
            CoreAction::Sync(sync_action) => match sync_action {
                SyncAction::Move { anchor_id, pos } => {
                    let client_seq = self.next_client_seq_for(*anchor_id as u32);
                    self.send(ClientMsg::Move {
                        anchor_id: *anchor_id as u32,
                        pos: *pos,
                        client_seq,
                    });
                    if let Some(observer) = self.local_transform_observer.borrow().as_ref() {
                        observer(*anchor_id as u32, *pos, None, client_seq);
                    }
                }
                SyncAction::Transform {
                    anchor_id,
                    pos,
                    rot_deg,
                } => {
                    let client_seq = self.next_client_seq_for(*anchor_id as u32);
                    self.send(ClientMsg::Transform {
                        anchor_id: *anchor_id as u32,
                        pos: *pos,
                        rot_deg: *rot_deg,
                        client_seq,
                    });
                    if let Some(observer) = self.local_transform_observer.borrow().as_ref() {
                        observer(*anchor_id as u32, *pos, Some(*rot_deg), client_seq);
                    }
                }
                SyncAction::Place {
                    anchor_id,
                    pos,
                    rot_deg,
                } => {
                    self.send(ClientMsg::Place {
                        anchor_id: *anchor_id as u32,
                        pos: *pos,
                        rot_deg: *rot_deg,
                    });
                    if let Some(observer) = self.local_transform_observer.borrow().as_ref() {
                        let client_seq = self.next_client_seq_for(*anchor_id as u32);
                        observer(*anchor_id as u32, *pos, Some(*rot_deg), client_seq);
                    }
                }
                SyncAction::Flip { piece_id, flipped } => {
                    self.send(ClientMsg::Flip {
                        piece_id: *piece_id as u32,
                        flipped: *flipped,
                    });
                    if let Some(observer) = self.local_flip_observer.borrow().as_ref() {
                        observer(*piece_id as u32, *flipped);
                    }
                }
                SyncAction::Release { anchor_id } => {
                    self.send(ClientMsg::Release {
                        anchor_id: *anchor_id as u32,
                    });
                }
            },
            _ => {}
        }
    }

    fn shutdown(&mut self) {
        self.disconnect();
    }

    fn sync_view(&self) -> SyncView {
        SyncView::new(
            InitMode::Online,
            self.connected.get(),
            self.client_id.get(),
            self.init_required.get(),
            self.room_id.borrow().clone(),
            self.persistence.get(),
            self.ownership_by_anchor.borrow().clone(),
        )
    }
}

pub(crate) fn update_kind(update: &RoomUpdate) -> &'static str {
    match update {
        RoomUpdate::Ownership { .. } => "ownership",
        RoomUpdate::GroupTransform { .. } => "group_transform",
        RoomUpdate::Flip { .. } => "flip",
        RoomUpdate::GroupOrder { .. } => "group_order",
        RoomUpdate::Connections { .. } => "connections",
    }
}
