use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use gloo::console;
use js_sys::Date;
use wasm_bindgen_futures::spawn_local;

use crate::app_core::{AppCore, AppSubscription};
use crate::game_state::AppGameState;
use crate::local_snapshot::build_playable_snapshot_from_app;
use crate::persisted::{PrivateImageEntry, PrivateImageRefs};
use crate::persisted_store;
use crate::runtime::{AssetEvent, SyncEvent, SyncHooks};
use crate::sync_runtime;
use heddobureika_core::{
    angle_matches, safety_corrections_after_detach, AngleDeg, ClientId, ClientMsg, FlipState,
    GameRules, MergePolicy, PieceId, PlayableAction, PlayableGameSnapshot, PlayableRoomUpdate,
    PlayableState, Pose2, Position2, PuzzleImageRef, PuzzleInfo, PuzzleTopology,
    RestrictedPlayableAction, RoomControlUpdate, RoomPersistence, PRIVATE_ASSET_MAX_BYTES,
};

const PENDING_POS_EPS: f32 = 0.02;
const PENDING_ROT_EPS: f32 = 0.05;

fn now_ms_u64() -> u64 {
    Date::now().max(0.0) as u64
}

#[derive(Clone, Debug)]
struct PendingTransform {
    pos: (f32, f32),
    rot_deg: Option<f32>,
    client_seq: u64,
    snap: bool,
}

#[derive(Clone, Debug)]
struct AssetDownload {
    mime: String,
    width: u32,
    height: u32,
    size: u32,
    received: u32,
    bytes: Vec<u8>,
}

struct MultiplayerBridgeState {
    core: Rc<AppCore>,
    local_state: RefCell<Option<AppGameState>>,
    pending_by_anchor: RefCell<HashMap<u32, PendingTransform>>,
    pending_snaps: RefCell<Vec<(u32, PendingTransform)>>,
    pending_flips: RefCell<HashMap<u32, bool>>,
    pending_detaches: RefCell<HashSet<u32>>,
    init_pending: Cell<bool>,
    subscription: RefCell<Option<AppSubscription>>,
    room_id: RefCell<Option<String>>,
    asset_downloads: RefCell<HashMap<String, AssetDownload>>,
    requested_assets: RefCell<HashSet<String>>,
}

impl MultiplayerBridgeState {
    fn new(core: Rc<AppCore>) -> Self {
        Self {
            core,
            local_state: RefCell::new(None),
            pending_by_anchor: RefCell::new(HashMap::new()),
            pending_snaps: RefCell::new(Vec::new()),
            pending_flips: RefCell::new(HashMap::new()),
            pending_detaches: RefCell::new(HashSet::new()),
            init_pending: Cell::new(false),
            subscription: RefCell::new(None),
            room_id: RefCell::new(None),
            asset_downloads: RefCell::new(HashMap::new()),
            requested_assets: RefCell::new(HashSet::new()),
        }
    }

    fn install(self: &Rc<Self>) {
        let hooks = self.build_hooks();
        sync_runtime::set_system_hooks(hooks);
        let state = Rc::clone(self);
        sync_runtime::set_multiplayer_local_transform_observer(Some(Rc::new(
            move |anchor_id, pos, rot_deg, client_seq, snap| {
                state.record_pending_transform(anchor_id, pos, rot_deg, client_seq, snap);
            },
        )));
        let state = Rc::clone(self);
        sync_runtime::set_multiplayer_local_flip_observer(Some(Rc::new(
            move |piece_id, flipped| {
                state.record_pending_flip(piece_id, flipped);
            },
        )));
        let state = Rc::clone(self);
        sync_runtime::set_multiplayer_local_detach_observer(Some(Rc::new(move |piece_id| {
            state.record_pending_detach(piece_id);
        })));
        let state = Rc::clone(self);
        let subscription = self.core.subscribe(Rc::new(move || {
            state.try_send_init();
        }));
        *self.subscription.borrow_mut() = Some(subscription);
    }

    fn build_hooks(self: &Rc<Self>) -> SyncHooks {
        let state = Rc::clone(self);
        let on_event = Rc::new(move |event: SyncEvent| match event {
            SyncEvent::Connected {
                room_id,
                persistence,
                initialized,
                client_id,
            } => {
                let Some(room_id) = room_id else {
                    return;
                };
                let Some(persistence) = persistence else {
                    return;
                };
                state.handle_welcome(room_id, persistence, initialized, client_id);
            }
            SyncEvent::NeedInit => state.handle_need_init(),
            SyncEvent::Warning { minutes_idle } => state.handle_warning(minutes_idle),
            SyncEvent::Ownership { anchor_id, owner } => state.handle_ownership(anchor_id, owner),
            SyncEvent::DropNotReady => state.handle_drop_not_ready(),
            SyncEvent::Error { code, message } => state.handle_error(code, message),
        });
        let state = Rc::clone(self);
        let on_remote_snapshot = Rc::new(move |snapshot, seq| {
            state.handle_state(snapshot, seq);
        });
        let state = Rc::clone(self);
        let on_remote_update = Rc::new(move |update, seq, source, client_seq| {
            state.handle_update(update, seq, source, client_seq);
        });
        let state = Rc::clone(self);
        let on_remote_playable_update = Rc::new(move |update, seq, source, client_seq| {
            state.handle_playable_update(update, seq, source, client_seq);
        });
        let state = Rc::clone(self);
        let on_asset = Rc::new(move |event: AssetEvent| {
            state.handle_asset(event);
        });
        SyncHooks {
            on_remote_action: Rc::new(|_| {}),
            on_snapshot: Rc::new(|_| {}),
            on_remote_snapshot,
            on_remote_update,
            on_remote_playable_update,
            on_event,
            on_asset,
        }
    }

    fn handle_welcome(
        &self,
        room_id: String,
        _persistence: RoomPersistence,
        initialized: bool,
        _client_id: Option<ClientId>,
    ) {
        self.init_pending.set(!initialized);
        *self.room_id.borrow_mut() = Some(room_id);
        self.asset_downloads.borrow_mut().clear();
        self.requested_assets.borrow_mut().clear();
        if let Some(sync) = sync_runtime::multiplayer_handle() {
            sync.borrow().set_state_applied(false);
        }
        *self.local_state.borrow_mut() = None;
        self.pending_by_anchor.borrow_mut().clear();
        self.pending_snaps.borrow_mut().clear();
        self.pending_flips.borrow_mut().clear();
        self.pending_detaches.borrow_mut().clear();
        if !initialized {
            self.try_send_init();
        }
    }

    fn handle_need_init(&self) {
        self.init_pending.set(true);
        self.pending_by_anchor.borrow_mut().clear();
        self.pending_snaps.borrow_mut().clear();
        self.pending_flips.borrow_mut().clear();
        self.pending_detaches.borrow_mut().clear();
        self.asset_downloads.borrow_mut().clear();
        self.requested_assets.borrow_mut().clear();
        self.try_send_init();
    }

    fn handle_warning(&self, _minutes_idle: u32) {}

    fn handle_state(self: &Rc<Self>, snapshot: PlayableGameSnapshot, _seq: u64) {
        let Ok(game_state) = AppGameState::from_snapshot(snapshot.clone()) else {
            return;
        };
        let applied = self.apply_playable_snapshot(&snapshot, game_state);
        if applied {
            self.init_pending.set(false);
            if let Some(sync) = sync_runtime::multiplayer_handle() {
                sync.borrow().set_state_applied(true);
            }
        }
        self.request_private_asset_if_missing(&snapshot.puzzle);
    }

    fn handle_update(
        &self,
        update: RoomControlUpdate,
        seq: u64,
        _source: Option<ClientId>,
        _client_seq: Option<u64>,
    ) {
        // Mirror the server's `set_seq` behavior: every control update
        // bumps `snapshot.seq`, `state.revision`, and `playable.revision`
        // together. If we don't advance our local revision here, the next
        // `PlayableUpdate.revision_before` will exceed our
        // `playable.revision` and `apply_to_playable` rejects the update
        // as stale.
        if let Some(state) = self.local_state.borrow_mut().as_mut() {
            if state.playable.revision < seq {
                state.playable.revision = seq;
                state.seq = seq;
            }
        }
        self.apply_control_update(&update);
    }

    /// Applies an authoritative playable update by mutating the live
    /// `AppGameState` shadow in place, then projecting back to the legacy
    /// `AppCore` for the existing renderer.
    fn handle_playable_update(
        self: &Rc<Self>,
        update: PlayableRoomUpdate,
        seq: u64,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    ) {
        let local_source = source.is_some() && source == sync_runtime::sync_view().client_id();
        let newer_local_pending = local_source
            && match client_seq {
                Some(seq) => self.has_pending_transform_after(seq),
                None => self.has_pending_transform_for_update(&update),
            };
        let updated_game = {
            let mut local = self.local_state.borrow_mut();
            let Some(state) = local.as_mut() else {
                return;
            };
            if !state.apply_wire_update(&update, seq) {
                return;
            }
            state.clone()
        };
        if local_source {
            if let Some(client_seq) = client_seq {
                self.ack_pending_transform(client_seq);
            }
        }
        let core_snapshot = self.core.snapshot();
        let preserve_drag = !core_snapshot.dragging_members.is_empty();
        // Prune first so we know whether any pending overlay survives the
        // wire update. If it does, `apply_predicted_state` will install the
        // overlay on top of `local_state` — calling `install_game` here too
        // would briefly paint the bare authoritative pose between the two
        // installs, which the user sees as the dragged piece (or just-placed
        // piece) flickering back to its previous position every echo.
        self.prune_pending_against_state(&updated_game);
        let has_pending = self.has_pending_prediction();
        if has_pending {
            let predicted = if newer_local_pending {
                self.apply_predicted_state_required(preserve_drag)
            } else {
                self.apply_predicted_state(preserve_drag)
            };
            if !predicted && !newer_local_pending {
                self.core.install_game(updated_game, preserve_drag);
            }
        } else if newer_local_pending {
            return;
        } else {
            self.core.install_game(updated_game, preserve_drag);
        }
    }

    fn handle_ownership(&self, anchor_id: u32, owner: Option<ClientId>) {
        self.maybe_drop_drag_on_ownership(anchor_id, owner);
    }

    fn handle_drop_not_ready(&self) {}

    fn handle_error(&self, _code: String, _message: String) {}

    fn handle_asset(self: &Rc<Self>, event: AssetEvent) {
        match event {
            AssetEvent::Begin {
                hash,
                mime,
                width,
                height,
                size,
            } => {
                if size == 0 || size > PRIVATE_ASSET_MAX_BYTES {
                    return;
                }
                self.asset_downloads.borrow_mut().insert(
                    hash,
                    AssetDownload {
                        mime,
                        width,
                        height,
                        size,
                        received: 0,
                        bytes: Vec::with_capacity(size as usize),
                    },
                );
            }
            AssetEvent::Chunk { hash, index, bytes } => {
                let mut downloads = self.asset_downloads.borrow_mut();
                let Some(entry) = downloads.get_mut(&hash) else {
                    return;
                };
                let _ = index;
                let next = entry
                    .received
                    .saturating_add(bytes.len().min(u32::MAX as usize) as u32);
                if next > entry.size || next > PRIVATE_ASSET_MAX_BYTES {
                    downloads.remove(&hash);
                    return;
                }
                entry.bytes.extend_from_slice(&bytes);
                entry.received = next;
            }
            AssetEvent::End { hash } => {
                let download = self.asset_downloads.borrow_mut().remove(&hash);
                let Some(download) = download else {
                    return;
                };
                if download.received != download.size
                    || download.bytes.len() != download.size as usize
                {
                    return;
                }
                let scope_key = self.room_scope_key();
                spawn_local(async move {
                    let now = now_ms_u64();
                    let entry = PrivateImageEntry {
                        bytes: download.bytes,
                        mime: download.mime,
                        width: download.width,
                        height: download.height,
                        size: download.size,
                        created_at: now,
                        last_used_at: now,
                    };
                    if let Err(message) = persisted_store::save_private_image(&hash, entry).await {
                        console::warn!("failed to store private image", message);
                        return;
                    }
                    if let Some(scope_key) = scope_key {
                        let refs = persisted_store::load_private_image_refs(&scope_key)
                            .await
                            .ok()
                            .flatten();
                        let mut hashes = refs.map(|refs| refs.hashes).unwrap_or_else(Vec::new);
                        if !hashes.iter().any(|value| value == &hash) {
                            hashes.push(hash.clone());
                        }
                        let refs = PrivateImageRefs {
                            hashes,
                            updated_at: now,
                        };
                        let _ = persisted_store::save_private_image_refs(&scope_key, refs).await;
                    }
                    #[cfg(target_arch = "wasm32")]
                    {
                        crate::wgpu_app::request_render();
                        crate::svg_app::request_render();
                    }
                });
            }
            AssetEvent::UploadAck { hash } => {
                console::log!("upload ack", hash);
            }
        }
    }

    fn room_scope_key(&self) -> Option<String> {
        self.room_id
            .borrow()
            .as_ref()
            .map(|room_id| format!("room:{room_id}"))
    }

    fn request_private_asset_if_missing(self: &Rc<Self>, puzzle: &PuzzleInfo) {
        let hash = match &puzzle.image_ref {
            PuzzleImageRef::Private { hash } => hash.clone(),
            _ => return,
        };
        let state = Rc::clone(self);
        spawn_local(async move {
            match persisted_store::load_private_image(&hash).await {
                Ok(Some(_)) => {
                    if let Some(scope_key) = state.room_scope_key() {
                        let now = now_ms_u64();
                        let refs = persisted_store::load_private_image_refs(&scope_key)
                            .await
                            .ok()
                            .flatten();
                        let mut hashes = refs.map(|refs| refs.hashes).unwrap_or_else(Vec::new);
                        if !hashes.iter().any(|value| value == &hash) {
                            hashes.push(hash.clone());
                        }
                        let refs = PrivateImageRefs {
                            hashes,
                            updated_at: now,
                        };
                        let _ = persisted_store::save_private_image_refs(&scope_key, refs).await;
                    }
                    return;
                }
                Ok(None) => {}
                Err(message) => {
                    console::warn!("failed to check private image", message);
                }
            }
            let should_request = {
                let mut requested = state.requested_assets.borrow_mut();
                if requested.contains(&hash) {
                    false
                } else {
                    requested.insert(hash.clone());
                    true
                }
            };
            if !should_request {
                return;
            }
            if let Some(sync) = sync_runtime::multiplayer_handle() {
                sync.borrow().send(ClientMsg::AssetRequest { hash });
            }
        });
    }

    fn record_pending_transform(
        &self,
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: Option<f32>,
        client_seq: u64,
        snap: bool,
    ) {
        if client_seq == 0 {
            return;
        }
        let mut pending = self.pending_by_anchor.borrow_mut();
        let rot_deg = rot_deg.or_else(|| pending.get(&anchor_id).and_then(|entry| entry.rot_deg));
        let pending_entry = PendingTransform {
            pos,
            rot_deg,
            client_seq,
            snap,
        };
        pending.insert(anchor_id, pending_entry.clone());
        drop(pending);
        if snap {
            let mut snaps = self.pending_snaps.borrow_mut();
            snaps.retain(|(_, entry)| entry.client_seq != client_seq);
            snaps.push((anchor_id, pending_entry));
        }
        // During an active drag, drag_move writes the cursor pose directly
        // into state.game.visual — running apply_predicted_state per
        // drag_move clones playable + rebuilds visual + reinstalls state.game
        // every frame, which made the drag visibly janky. The pending entry
        // is still recorded so prune_pending_against_state can clear it when
        // the server echoes the move back.
        if self.core.snapshot().dragging_members.is_empty() {
            let _ = self.apply_predicted_state(false);
        }
    }

    fn record_pending_flip(&self, piece_id: u32, flipped: bool) {
        self.pending_flips.borrow_mut().insert(piece_id, flipped);
        if self.core.snapshot().dragging_members.is_empty() {
            let _ = self.apply_predicted_state(false);
        }
    }

    fn record_pending_detach(&self, piece_id: u32) {
        self.pending_detaches.borrow_mut().insert(piece_id);
        if self.core.snapshot().dragging_members.is_empty() {
            let _ = self.apply_predicted_state(false);
        }
    }

    fn prune_pending_against_state(&self, game: &AppGameState) {
        let active_drag_anchor = self.active_drag_anchor();
        // Topology-agnostic pose-unit derivation: ask the topology how
        // many pose-mm units it spans and divide image pixels into that.
        // Falls back to `(1.0, 1.0)` so the comparison still runs (with a
        // looser tolerance) for puzzles whose topology can't be built.
        let (piece_width, piece_height) =
            heddobureika_core::build_topology_from_spec(&game.puzzle.to_spec())
                .map(|topology| {
                    let (ex, ey) = topology.image_extent_in_pose_units();
                    (
                        game.puzzle.image_width as f32 / ex.max(1.0),
                        game.puzzle.image_height as f32 / ey.max(1.0),
                    )
                })
                .unwrap_or((1.0, 1.0));
        self.pending_snaps
            .borrow_mut()
            .retain(|(anchor_id, entry)| {
                !pending_transform_matches_state(game, *anchor_id, entry, piece_width, piece_height)
                    .unwrap_or(true)
            });
        let mut pending = self.pending_by_anchor.borrow_mut();
        if !pending.is_empty() {
            let mut to_remove = Vec::new();
            for (anchor_id, entry) in pending.iter() {
                if Some(*anchor_id) == active_drag_anchor {
                    continue;
                }
                if pending_transform_matches_state(
                    game,
                    *anchor_id,
                    entry,
                    piece_width,
                    piece_height,
                )
                .unwrap_or(true)
                {
                    to_remove.push(*anchor_id);
                }
            }
            for anchor_id in to_remove {
                pending.remove(&anchor_id);
            }
        }
        drop(pending);
        let mut pending_detaches = self.pending_detaches.borrow_mut();
        if !pending_detaches.is_empty() {
            let mut detaches_remove = Vec::new();
            for piece_id in pending_detaches.iter() {
                if pending_detach_matches_state(game, *piece_id) {
                    detaches_remove.push(*piece_id);
                }
            }
            for piece_id in detaches_remove {
                pending_detaches.remove(&piece_id);
            }
        }
        drop(pending_detaches);
        let mut pending_flips = self.pending_flips.borrow_mut();
        if pending_flips.is_empty() {
            return;
        }
        let mut flips_remove = Vec::new();
        for (piece_id, desired) in pending_flips.iter() {
            let piece = PieceId(*piece_id);
            let Some(group) = game.playable.logical.group_of(piece) else {
                flips_remove.push(*piece_id);
                continue;
            };
            let current = game.playable.flip_of(group) == Some(FlipState::Flipped);
            if current == *desired {
                flips_remove.push(*piece_id);
            }
        }
        for piece_id in flips_remove {
            pending_flips.remove(&piece_id);
        }
    }

    fn ack_pending_transform(&self, client_seq: u64) {
        let active_drag_anchor = self.active_drag_anchor();
        self.pending_by_anchor
            .borrow_mut()
            .retain(|anchor_id, pending| {
                pending.client_seq > client_seq || Some(*anchor_id) == active_drag_anchor
            });
        self.pending_snaps
            .borrow_mut()
            .retain(|(_, pending)| pending.client_seq > client_seq);
    }

    fn has_pending_transform_after(&self, client_seq: u64) -> bool {
        self.pending_by_anchor
            .borrow()
            .values()
            .any(|pending| pending.client_seq > client_seq)
            || self
                .pending_snaps
                .borrow()
                .iter()
                .any(|(_, pending)| pending.client_seq > client_seq)
    }

    fn has_pending_transform_for_update(&self, update: &PlayableRoomUpdate) -> bool {
        let mut affected = HashSet::new();
        affected.insert(update.mover_group);
        if let Some(group) = update.fixed_group {
            affected.insert(group);
        }
        for merge in &update.merged_groups {
            affected.insert(merge.keep);
            affected.insert(merge.absorbed);
        }
        for change in &update.group_changes {
            affected.insert(change.group);
        }
        for change in &update.piece_changes {
            affected.insert(change.piece);
            affected.insert(change.group);
        }
        self.pending_by_anchor
            .borrow()
            .keys()
            .any(|anchor_id| affected.contains(anchor_id))
            || self
                .pending_snaps
                .borrow()
                .iter()
                .any(|(anchor_id, _)| affected.contains(anchor_id))
    }

    fn has_pending_prediction(&self) -> bool {
        !self.pending_by_anchor.borrow().is_empty()
            || !self.pending_snaps.borrow().is_empty()
            || !self.pending_flips.borrow().is_empty()
            || !self.pending_detaches.borrow().is_empty()
    }

    fn active_drag_anchor(&self) -> Option<u32> {
        self.core
            .snapshot()
            .dragging_members
            .first()
            .copied()
            .map(|anchor| anchor as u32)
    }

    fn maybe_drop_drag_on_ownership(&self, anchor_id: u32, owner: Option<ClientId>) {
        // Only interrupt the local drag (and clear our pending overlay)
        // when another client has actually taken the anchor. A release
        // update (`owner == None`) for our previous drag can arrive AFTER
        // we've already begun a fresh re-grab on the same anchor; cancelling
        // here would yank the in-flight drag back to its previous pose,
        // which the user sees as the piece "sliding into a past position"
        // mid-drag.
        //
        // Why: outgoing wire order is `Place` → server release →
        // `ServerMsg::PlayableUpdate` + `ownership(anchor, None)`. With
        // simulated latency we can do `release → click → drag → ...` faster
        // than the round-trip, so the ownership=None echo for the FIRST
        // drag is processed while the SECOND drag's `Select` is still
        // in flight. The new drag's pending pose is fine; let
        // `handle_playable_update`'s prune handle stale entries normally.
        let my_id = sync_runtime::sync_view().client_id();
        let owned_by_other = match (owner, my_id) {
            (Some(o), Some(m)) => o != m,
            (Some(_), None) => true,
            (None, _) => false,
        };
        if !owned_by_other {
            return;
        }
        let snapshot = self.core.snapshot();
        let drag_anchor = snapshot.dragging_members.first().copied();
        let dragging_anchor = drag_anchor.map(|id| id as u32) == Some(anchor_id);
        let removed_pending = self
            .pending_by_anchor
            .borrow_mut()
            .remove(&anchor_id)
            .is_some();
        self.pending_snaps
            .borrow_mut()
            .retain(|(pending_anchor, _)| *pending_anchor != anchor_id);
        if removed_pending || dragging_anchor {
            self.drop_pending_flips_for_anchor(anchor_id);
            self.drop_pending_detaches_for_anchor(anchor_id);
            if !self.apply_predicted_state(false) && dragging_anchor {
                self.core.cancel_drag();
            }
        }
    }

    fn drop_pending_flips_for_anchor(&self, anchor_id: u32) {
        let local = self.local_state.borrow();
        let Some(game) = local.as_ref() else {
            return;
        };
        let mut pending = self.pending_flips.borrow_mut();
        pending.retain(|piece_id, _| {
            let piece = PieceId(*piece_id);
            let Some(group) = game.playable.logical.group_of(piece) else {
                return true;
            };
            let Some(group_anchor) = game.playable.anchor_piece_of_group(group) else {
                return true;
            };
            group_anchor.as_u32() != anchor_id
        });
    }

    fn drop_pending_detaches_for_anchor(&self, anchor_id: u32) {
        let local = self.local_state.borrow();
        let Some(game) = local.as_ref() else {
            return;
        };
        let mut pending = self.pending_detaches.borrow_mut();
        pending.retain(|piece_id| {
            let piece = PieceId(*piece_id);
            let Some(group) = game.playable.logical.group_of(piece) else {
                return true;
            };
            let Some(group_anchor) = game.playable.anchor_piece_of_group(group) else {
                return true;
            };
            group_anchor.as_u32() != anchor_id
        });
    }

    fn try_send_init(&self) {
        if !self.init_pending.get() {
            return;
        }
        let snapshot = self.core.snapshot();
        let Some(snapshot) = build_playable_snapshot_from_app(&snapshot) else {
            return;
        };
        let msg = ClientMsg::Init { snapshot };
        if let Some(sync) = sync_runtime::multiplayer_handle() {
            sync.borrow().send(msg);
        }
        self.init_pending.set(false);
    }

    fn apply_playable_snapshot(
        &self,
        snapshot: &PlayableGameSnapshot,
        game_state: AppGameState,
    ) -> bool {
        if snapshot.puzzle.image_width == 0 || snapshot.puzzle.image_height == 0 {
            console::warn!("multiplayer snapshot missing image size");
            return false;
        }
        let total = snapshot.puzzle.piece_count() as usize;
        if total == 0 {
            console::warn!("multiplayer snapshot invalid topology");
            return false;
        }
        let core_snapshot = self.core.snapshot();
        let should_update_core = core_snapshot
            .puzzle_info
            .as_ref()
            .map(|info| {
                info.image_ref != snapshot.puzzle.image_ref
                    || info.image_width != snapshot.puzzle.image_width
                    || info.image_height != snapshot.puzzle.image_height
                    || info.topology != snapshot.puzzle.topology
            })
            .unwrap_or(true);
        let preserve_drag = !should_update_core && !core_snapshot.dragging_members.is_empty();
        if should_update_core {
            self.core.set_puzzle_with_topology(
                snapshot.puzzle.label.clone(),
                snapshot.puzzle.image_ref.clone(),
                (snapshot.puzzle.image_width, snapshot.puzzle.image_height),
                snapshot.puzzle.to_spec(),
                None,
            );
        }
        self.prune_pending_against_state(&game_state);
        *self.local_state.borrow_mut() = Some(game_state.clone());
        let has_pending = self.has_pending_prediction();
        if has_pending {
            let _ = self.apply_predicted_state(preserve_drag);
        } else {
            self.core.install_game(game_state, preserve_drag);
        }
        true
    }

    fn apply_predicted_state(&self, preserve_drag: bool) -> bool {
        self.apply_predicted_state_inner(preserve_drag, false)
    }

    fn apply_predicted_state_required(&self, preserve_drag: bool) -> bool {
        self.apply_predicted_state_inner(preserve_drag, true)
    }

    fn apply_predicted_state_inner(
        &self,
        preserve_drag: bool,
        require_valid_pending: bool,
    ) -> bool {
        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer prediction skipped (puzzle info not ready)");
            return false;
        };
        let total = info.piece_count() as usize;
        if total == 0 {
            return false;
        }
        let Some(mut predicted_state) = self.local_state.borrow().clone() else {
            console::warn!("multiplayer prediction skipped (state not ready)");
            return false;
        };
        // Pose unit comes from the rendered geometry; if that's not yet
        // populated, fall back to deriving it from the topology's
        // declared image span (topology-agnostic — works for grid,
        // triangular, future Voronoi).
        let unit_x = snapshot.pose_unit_px[0];
        let unit_y = snapshot.pose_unit_px[1];
        let (fallback_x, fallback_y) = heddobureika_core::build_topology_from_spec(&info.to_spec())
            .map(|topology| {
                let (ex, ey) = topology.image_extent_in_pose_units();
                (
                    info.image_width as f32 / ex.max(1.0),
                    info.image_height as f32 / ey.max(1.0),
                )
            })
            .unwrap_or((1.0, 1.0));
        let piece_width = if unit_x > 0.0 { unit_x } else { fallback_x };
        let piece_height = if unit_y > 0.0 { unit_y } else { fallback_y };
        let pending_detaches_snapshot = self.pending_detaches.borrow().clone();
        if !pending_detaches_snapshot.is_empty() {
            let mut invalid = Vec::new();
            let mut pending_detaches = pending_detaches_snapshot.into_iter().collect::<Vec<_>>();
            pending_detaches.sort_unstable();
            let puzzle = predicted_state.puzzle.clone();
            let rules = predicted_state.rules;
            for piece_id in pending_detaches {
                if predict_pending_detach(&mut predicted_state.playable, piece_id, &puzzle, &rules)
                    .is_err()
                {
                    invalid.push(piece_id);
                }
            }
            if !invalid.is_empty() {
                if require_valid_pending {
                    return false;
                }
                let mut pending = self.pending_detaches.borrow_mut();
                for piece_id in invalid {
                    pending.remove(&piece_id);
                }
            }
        }
        let pending_snapshot = self.pending_by_anchor.borrow().clone();
        let pending_snaps_snapshot = self.pending_snaps.borrow().clone();
        if !pending_snapshot.is_empty() || !pending_snaps_snapshot.is_empty() {
            let mut invalid = Vec::new();
            let snap_seqs = pending_snaps_snapshot
                .iter()
                .map(|(_, pending)| pending.client_seq)
                .collect::<HashSet<_>>();
            let mut pending_transforms = pending_snaps_snapshot;
            pending_transforms.extend(
                pending_snapshot
                    .into_iter()
                    .filter(|(_, pending)| !snap_seqs.contains(&pending.client_seq)),
            );
            pending_transforms.sort_by_key(|(_, pending)| pending.client_seq);
            for (anchor_id, pending) in pending_transforms.iter() {
                if predict_pending_transform(
                    &mut predicted_state.playable,
                    *anchor_id,
                    pending.pos,
                    pending.rot_deg,
                    piece_width,
                    piece_height,
                    pending.snap,
                )
                .is_err()
                {
                    invalid.push(*anchor_id);
                }
            }
            if !invalid.is_empty() {
                if require_valid_pending {
                    return false;
                }
                let mut pending = self.pending_by_anchor.borrow_mut();
                for anchor_id in &invalid {
                    pending.remove(&anchor_id);
                }
                self.pending_snaps
                    .borrow_mut()
                    .retain(|(anchor_id, _)| !invalid.contains(anchor_id));
            }
        }
        let pending_flips_snapshot = self.pending_flips.borrow().clone();
        if !pending_flips_snapshot.is_empty() {
            let mut invalid = Vec::new();
            for (piece_id, flipped) in pending_flips_snapshot.iter() {
                if predict_pending_flip(&mut predicted_state.playable, *piece_id, *flipped).is_err()
                {
                    invalid.push(*piece_id);
                }
            }
            if !invalid.is_empty() {
                if require_valid_pending {
                    return false;
                }
                let mut pending = self.pending_flips.borrow_mut();
                for piece_id in invalid {
                    pending.remove(&piece_id);
                }
            }
        }
        predicted_state.rebuild_visual();
        self.core.install_game(predicted_state, preserve_drag);
        true
    }

    fn apply_control_update(&self, update: &RoomControlUpdate) {
        if let RoomControlUpdate::Ownership { .. } = update {
            return;
        }

        let RoomControlUpdate::GroupOrder { order } = update else {
            return;
        };

        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer update dropped (puzzle info not ready)");
            return;
        };
        let total = info.piece_count() as usize;
        if total == 0 {
            return;
        }
        let pruned_game = {
            let mut local = self.local_state.borrow_mut();
            let Some(state) = local.as_mut() else {
                console::warn!("multiplayer update dropped (state not ready)");
                return;
            };
            if !apply_group_order_to_playable(&mut state.playable, order) {
                return;
            }
            state.rebuild_visual();
            state.clone()
        };
        self.prune_pending_against_state(&pruned_game);
        let preserve_drag = !snapshot.dragging_members.is_empty();
        let _ = self.apply_predicted_state(preserve_drag);
    }
}

thread_local! {
    static BRIDGE_STATE: RefCell<Option<Rc<MultiplayerBridgeState>>> = RefCell::new(None);
}

fn install_with_core(core: Rc<AppCore>) -> Rc<MultiplayerBridgeState> {
    if let Some(state) = BRIDGE_STATE.with(|slot| slot.borrow().clone()) {
        return state;
    }
    let state = Rc::new(MultiplayerBridgeState::new(core));
    state.install();
    BRIDGE_STATE.with(|slot| {
        *slot.borrow_mut() = Some(state.clone());
    });
    state
}

pub(crate) fn install(core: Rc<AppCore>) {
    let _ = install_with_core(core);
}

#[cfg(test)]
pub(crate) fn hooks_for_tests(core: Rc<AppCore>) -> SyncHooks {
    let state = install_with_core(core);
    state.build_hooks()
}

fn predict_pending_transform<T: PuzzleTopology>(
    playable: &mut PlayableState<T>,
    anchor_id: u32,
    pos: (f32, f32),
    rot_deg: Option<f32>,
    piece_width: f32,
    piece_height: f32,
    snap: bool,
) -> Result<(), ()> {
    let piece = PieceId(anchor_id);
    if piece.as_usize() >= playable.piece_count() {
        return Err(());
    }
    let group = playable.logical.group_of(piece).ok_or(())?;
    if piece_width <= 0.0 || piece_height <= 0.0 {
        return Err(());
    }
    let target_x_mm = (pos.0 + piece_width * 0.5) / piece_width;
    let target_y_mm = (pos.1 + piece_height * 0.5) / piece_height;
    let current_piece_pose = playable.piece_world_pose(piece).ok_or(())?;
    let group_rotation = match rot_deg {
        Some(rot) => {
            let group_anchor = playable.anchor_piece_of_group(group).ok_or(())?;
            let anchor_local = playable.piece_local_pose_of(group_anchor).ok_or(())?;
            let piece_local = playable.piece_local_pose_of(piece).ok_or(())?;
            AngleDeg::try_new(
                rot - piece_local.rotation_degrees() + anchor_local.rotation_degrees(),
            )
            .ok_or(())?
        }
        None => playable.pose_of(group).ok_or(())?.rotation,
    };
    let target_piece_rotation = rot_deg.unwrap_or_else(|| current_piece_pose.rotation_degrees());
    let target_piece_pose =
        Pose2::try_from_mm_degrees(target_x_mm, target_y_mm, target_piece_rotation).ok_or(())?;
    let drop_group_pose = playable
        .group_pose_to_place_piece(group, piece, target_piece_pose, group_rotation)
        .ok_or(())?;
    let drop_pos =
        Position2::try_from_mm(drop_group_pose.x_mm(), drop_group_pose.y_mm()).ok_or(())?;
    let action = PlayableAction::TransformGroupTo {
        group,
        drop_pos,
        drop_rotation: group_rotation,
    };
    if snap {
        let _ = playable.apply_action_with_snap(action, None, MergePolicy::KeepFixedGroup);
    } else {
        let _ = playable.apply_action_only(action, None);
    }
    Ok(())
}

fn predict_pending_flip<T: PuzzleTopology>(
    playable: &mut PlayableState<T>,
    piece_id: u32,
    flipped: bool,
) -> Result<(), ()> {
    let piece = PieceId(piece_id);
    if piece.as_usize() >= playable.piece_count() {
        return Err(());
    }
    let group = playable.logical.group_of(piece).ok_or(())?;
    if playable.anchor_piece_of_group(group) != Some(piece)
        || playable.logical.members_of(group).nth(1).is_some()
    {
        return Err(());
    }
    let target_pose = playable.piece_world_pose(piece).ok_or(())?;
    let target_flip = if flipped {
        FlipState::Flipped
    } else {
        FlipState::Normal
    };
    let action = RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let _ = playable.apply_restricted_action_batch(action, None);
    Ok(())
}

fn predict_pending_detach<T: PuzzleTopology>(
    playable: &mut PlayableState<T>,
    piece_id: u32,
    puzzle: &PuzzleInfo,
    rules: &GameRules,
) -> Result<(), ()> {
    let piece = PieceId(piece_id);
    if piece.as_usize() >= playable.piece_count() {
        return Err(());
    }
    let group = playable.logical.group_of(piece).ok_or(())?;
    if playable.anchor_piece_of_group(group) == Some(piece)
        && playable.logical.members_of(group).nth(1).is_none()
    {
        return Ok(());
    }
    let target_pose = playable.piece_world_pose(piece).ok_or(())?;
    let target_flip = playable.flip_of(group).unwrap_or(FlipState::Normal);
    let original_members: Vec<PieceId> = playable.logical.members_of(group).collect();
    let action = RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let _ = playable.apply_restricted_action_batch(action, None);
    // Mirror the server's post-detach safety force-move so the predicted
    // state matches the wire echo and we don't snap poses when it arrives.
    let pose_unit_px = heddobureika_core::build_topology_from_spec(&puzzle.to_spec())
        .map(|t| {
            let (ex, ey) = t.image_extent_in_pose_units();
            (
                puzzle.image_width as f32 / ex.max(1.0),
                puzzle.image_height as f32 / ey.max(1.0),
            )
        })
        .unwrap_or((1.0, 1.0));
    let corrections =
        safety_corrections_after_detach(playable, &original_members, puzzle, rules, pose_unit_px);
    for (group, drop_pos) in corrections {
        let _ =
            playable.apply_action_only(PlayableAction::TranslateGroup { group, drop_pos }, None);
    }
    Ok(())
}

fn pending_detach_matches_state(game: &AppGameState, piece_id: u32) -> bool {
    let piece = PieceId(piece_id);
    let Some(group) = game.playable.logical.group_of(piece) else {
        return true;
    };
    game.playable.anchor_piece_of_group(group) == Some(piece)
        && game.playable.logical.members_of(group).nth(1).is_none()
}

fn pending_transform_matches_state(
    game: &AppGameState,
    anchor_id: u32,
    entry: &PendingTransform,
    piece_width: f32,
    piece_height: f32,
) -> Option<bool> {
    let piece = PieceId(anchor_id);
    game.playable.logical.group_of(piece)?;
    let pose = game.playable.piece_world_pose(piece)?;
    let px = pose.x_mm() * piece_width - piece_width * 0.5;
    let py = pose.y_mm() * piece_height - piece_height * 0.5;
    let rot = pose.rotation_degrees();
    let pos_match =
        (px - entry.pos.0).abs() <= PENDING_POS_EPS && (py - entry.pos.1).abs() <= PENDING_POS_EPS;
    let rot_match = match entry.rot_deg {
        Some(target) => angle_matches(rot, target, PENDING_ROT_EPS),
        None => true,
    };
    Some(pos_match && rot_match)
}

fn apply_group_order_to_playable<T: PuzzleTopology>(
    playable: &mut PlayableState<T>,
    anchors: &[u32],
) -> bool {
    playable.set_z_order_by_anchors(anchors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::GridChoice;
    use heddobureika_core::{
        build_topology_from_spec, EdgeId, GameRules, GridShapeSettings, GridTopology, LogicalState,
        PlayRules, PlayableRoomUpdateKind, ProposalApplyStatusSnapshot, TopologySpec,
    };

    fn two_piece_puzzle() -> PuzzleInfo {
        PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            topology: TopologySpec::grid(2, 1).into(),
            shape_seed: 0,
            image_width: 200,
            image_height: 100,
        }
    }

    fn unsnapped_two_piece_game() -> AppGameState {
        let puzzle = two_piece_puzzle();
        let _grid = GridChoice {
            target_count: 2,
            cols: 2,
            rows: 1,
            actual_count: 2,
        };
        let topology = build_topology_from_spec(&TopologySpec::grid(2, 1)).expect("topology");
        let geometry = topology
            .build_render_geometry(
                puzzle.image_width,
                puzzle.image_height,
                puzzle.shape_seed,
                &GridShapeSettings::default(),
            )
            .expect("render geometry");
        AppGameState::scrambled(
            puzzle,
            GameRules::default(),
            TopologySpec::grid(2, 1),
            &geometry,
            0,
            &[(0.0, 0.0), (250.0, 0.0)],
            &[0.0, 0.0],
            &[false, false],
            &[0, 1],
        )
        .expect("valid test game")
    }

    #[test]
    fn pending_transform_prediction_rebases_non_anchor_piece() {
        let topology = GridTopology::try_new(2, 1).expect("valid grid");
        let mut logical = LogicalState::new(topology);
        assert!(logical.activate_edge(EdgeId(0)));
        let mut playable = PlayableState::new(logical, PlayRules::default());
        playable.group_pose[0] = Pose2::try_from_mm_degrees(0.5, 0.5, 0.0).expect("finite pose");

        predict_pending_transform(
            &mut playable,
            1,
            (250.0, 0.0),
            Some(90.0),
            100.0,
            100.0,
            false,
        )
        .expect("prediction should place a non-anchor piece");

        let pose = playable
            .piece_world_pose(PieceId(1))
            .expect("piece 1 world pose");
        assert!((pose.x_mm() - 3.0).abs() <= 1.0e-4);
        assert!((pose.y_mm() - 0.5).abs() <= 1.0e-4);
        assert!(angle_matches(pose.rotation_degrees(), 90.0, 1.0e-4));
    }

    #[test]
    fn pending_detach_prediction_splits_connected_group() {
        let topology = GridTopology::try_new(2, 1).expect("valid grid");
        let mut playable = PlayableState::solved(topology, PlayRules::default());
        let before = playable
            .piece_world_pose(PieceId(1))
            .expect("piece 1 world pose");

        let puzzle = two_piece_puzzle();
        let rules = GameRules::default();
        predict_pending_detach(&mut playable, 1, &puzzle, &rules)
            .expect("prediction should detach piece");

        let piece = PieceId(1);
        let group = playable.logical.group_of(piece).expect("piece group");
        assert_eq!(playable.anchor_piece_of_group(group), Some(piece));
        assert!(playable.logical.members_of(group).nth(1).is_none());
        assert!(!playable.is_solved());
        assert_eq!(
            playable
                .piece_world_pose(piece)
                .expect("piece 1 world pose"),
            before
        );
    }

    #[test]
    fn seq_less_local_update_keeps_affected_pending_transform_protected() {
        let core = AppCore::new();
        let bridge = MultiplayerBridgeState::new(core);
        bridge.pending_by_anchor.borrow_mut().insert(
            1,
            PendingTransform {
                pos: (100.0, 0.0),
                rot_deg: Some(90.0),
                client_seq: 2,
                snap: false,
            },
        );
        let update = PlayableRoomUpdate {
            kind: PlayableRoomUpdateKind::Snap,
            action_id: None,
            status: ProposalApplyStatusSnapshot::ActionOnly,
            rejection: None,
            rebased: false,
            base_revision: 0,
            revision_before: 0,
            revision_after: 1,
            mover_group: 1,
            fixed_group: None,
            activated_edges: Vec::new(),
            deactivated_edges: Vec::new(),
            merged_groups: Vec::new(),
            group_changes: Vec::new(),
            piece_changes: Vec::new(),
            z_order_changed: false,
            membership_changed: false,
            solved_changed: false,
        };

        assert!(bridge.has_pending_transform_for_update(&update));
    }

    #[test]
    fn pending_place_prediction_keeps_optimistic_snap_membership() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "test".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            (200, 100),
            TopologySpec::grid(2, 1),
            None,
        );

        let bridge = MultiplayerBridgeState::new(core.clone());
        *bridge.local_state.borrow_mut() = Some(unsnapped_two_piece_game());

        bridge.record_pending_transform(1, (100.0, 0.0), Some(0.0), 1, true);

        let snapshot = core.snapshot();
        assert_eq!(snapshot.piece_group_anchor.len(), 2);
        assert_eq!(
            snapshot.piece_group_anchor[0],
            snapshot.piece_group_anchor[1]
        );
    }

    #[test]
    fn pending_shift_drag_prediction_places_detached_piece_only() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "test".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            (200, 100),
            TopologySpec::grid(2, 1),
            None,
        );

        let bridge = MultiplayerBridgeState::new(core.clone());
        let game = AppGameState::solved(two_piece_puzzle(), GameRules::default())
            .expect("valid solved game");
        *bridge.local_state.borrow_mut() = Some(game);
        bridge.pending_detaches.borrow_mut().insert(1);

        bridge.record_pending_transform(1, (250.0, 0.0), Some(0.0), 1, true);

        let snapshot = core.snapshot();
        assert_eq!(snapshot.piece_group_anchor.len(), 2);
        assert_ne!(
            snapshot.piece_group_anchor[0],
            snapshot.piece_group_anchor[1]
        );
        assert!(!snapshot.solved);
        let piece0 = snapshot.piece_world_poses[0];
        let piece1 = snapshot.piece_world_poses[1];
        assert!((piece0.x_mm() - 0.5).abs() <= 1.0e-4);
        assert!((piece1.x_mm() - 3.0).abs() <= 1.0e-4);
    }

    #[test]
    fn pending_flip_prediction_rejects_connected_group() {
        let topology = GridTopology::try_new(2, 1).expect("valid grid");
        let mut playable = PlayableState::solved(topology, PlayRules::default());

        assert!(predict_pending_flip(&mut playable, 0, true).is_err());
        assert!(playable.is_solved());
    }

    #[test]
    fn solved_authoritative_state_prunes_stale_pending_flip() {
        let core = AppCore::new();
        core.set_puzzle_with_topology(
            "test".to_string(),
            PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            (200, 100),
            TopologySpec::grid(2, 1),
            None,
        );

        let bridge = MultiplayerBridgeState::new(core.clone());
        let game = AppGameState::solved(two_piece_puzzle(), GameRules::default())
            .expect("valid solved game");
        *bridge.local_state.borrow_mut() = Some(game);
        bridge.pending_flips.borrow_mut().insert(0, true);

        assert!(bridge.apply_predicted_state(false));

        assert!(bridge.pending_flips.borrow().is_empty());
        assert!(core.snapshot().solved);
    }
}
