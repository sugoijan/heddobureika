use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use gloo::console;

use crate::app_core::{AppCore, AppSubscription};
use crate::core::{
    build_group_order_from_piece_order, build_grid_choices, build_piece_order_from_groups,
    clear_piece_connections, grid_choice_index, groups_from_connections, GridChoice, FALLBACK_GRID,
};
use crate::multiplayer_game_sync::MultiplayerSyncCallbacks;
use crate::sync_runtime;
use heddobureika_core::{
    angle_matches, apply_room_update_to_snapshot, ClientId, ClientMsg, GameSnapshot, PuzzleInfo,
    PuzzleStateSnapshot, RoomPersistence, RoomUpdate,
};

const PENDING_POS_EPS: f32 = 0.02;
const PENDING_ROT_EPS: f32 = 0.05;

#[derive(Clone)]
pub(crate) struct MultiplayerUiHooks {
    pub(crate) on_welcome: Rc<dyn Fn(String, RoomPersistence, bool, Option<ClientId>)>,
    pub(crate) on_need_init: Rc<dyn Fn()>,
    pub(crate) on_warning: Rc<dyn Fn(u32)>,
    pub(crate) on_state: Rc<dyn Fn(GameSnapshot, u64)>,
    pub(crate) on_update: Rc<dyn Fn(RoomUpdate, u64, Option<ClientId>, Option<u64>)>,
    pub(crate) on_ownership: Rc<dyn Fn(u32, Option<ClientId>)>,
    pub(crate) on_drop_not_ready: Rc<dyn Fn()>,
    pub(crate) on_error: Rc<dyn Fn(String, String)>,
}

#[derive(Clone, Debug)]
struct LocalSnapshot {
    positions: Vec<(f32, f32)>,
    rotations: Vec<f32>,
    flips: Vec<bool>,
    connections: Vec<[bool; 4]>,
    group_order: Vec<u32>,
    scramble_nonce: u32,
}

impl LocalSnapshot {
    fn empty() -> Self {
        Self {
            positions: Vec::new(),
            rotations: Vec::new(),
            flips: Vec::new(),
            connections: Vec::new(),
            group_order: Vec::new(),
            scramble_nonce: 0,
        }
    }

    fn from_room(snapshot: &GameSnapshot) -> Self {
        Self {
            positions: snapshot.state.positions.clone(),
            rotations: snapshot.state.rotations.clone(),
            flips: snapshot.state.flips.clone(),
            connections: snapshot.state.connections.clone(),
            group_order: snapshot.state.group_order.clone(),
            scramble_nonce: snapshot.state.scramble_nonce,
        }
    }

    fn from_state(snapshot: PuzzleStateSnapshot) -> Self {
        Self {
            positions: snapshot.positions,
            rotations: snapshot.rotations,
            flips: snapshot.flips,
            connections: snapshot.connections,
            group_order: snapshot.group_order,
            scramble_nonce: snapshot.scramble_nonce,
        }
    }

    fn to_state_snapshot(&self) -> PuzzleStateSnapshot {
        PuzzleStateSnapshot {
            positions: self.positions.clone(),
            rotations: self.rotations.clone(),
            flips: self.flips.clone(),
            connections: self.connections.clone(),
            group_order: self.group_order.clone(),
            scramble_nonce: self.scramble_nonce,
        }
    }
}

#[derive(Clone, Debug)]
struct PendingTransform {
    pos: (f32, f32),
    rot_deg: Option<f32>,
    client_seq: u64,
}

struct MultiplayerBridgeState {
    core: Rc<AppCore>,
    local_snapshot: RefCell<LocalSnapshot>,
    pending_by_anchor: RefCell<HashMap<u32, PendingTransform>>,
    pending_flips: RefCell<HashMap<u32, bool>>,
    init_pending: Cell<bool>,
    ui_hooks: RefCell<Vec<(u64, MultiplayerUiHooks)>>,
    next_ui_hook_id: Cell<u64>,
    subscription: RefCell<Option<AppSubscription>>,
}

impl MultiplayerBridgeState {
    fn new(core: Rc<AppCore>) -> Self {
        Self {
            core,
            local_snapshot: RefCell::new(LocalSnapshot::empty()),
            pending_by_anchor: RefCell::new(HashMap::new()),
            pending_flips: RefCell::new(HashMap::new()),
            init_pending: Cell::new(false),
            ui_hooks: RefCell::new(Vec::new()),
            next_ui_hook_id: Cell::new(0),
            subscription: RefCell::new(None),
        }
    }

    fn install(self: &Rc<Self>) {
        let callbacks = self.build_callbacks();
        sync_runtime::set_callbacks(callbacks);
        let state = Rc::clone(self);
        sync_runtime::set_multiplayer_local_transform_observer(Some(Rc::new(
            move |anchor_id, pos, rot_deg, client_seq| {
                state.record_pending_transform(anchor_id, pos, rot_deg, client_seq);
            },
        )));
        let state = Rc::clone(self);
        sync_runtime::set_multiplayer_local_flip_observer(Some(Rc::new(
            move |piece_id, flipped| {
                state.record_pending_flip(piece_id, flipped);
            },
        )));
        let state = Rc::clone(self);
        let subscription = self.core.subscribe(Rc::new(move || {
            state.try_send_init();
        }));
        *self.subscription.borrow_mut() = Some(subscription);
    }

    fn build_callbacks(self: &Rc<Self>) -> MultiplayerSyncCallbacks {
        let state = Rc::clone(self);
        let on_welcome = Rc::new(move |room_id, persistence, initialized, client_id| {
            state.handle_welcome(room_id, persistence, initialized, client_id);
        });
        let state = Rc::clone(self);
        let on_need_init = Rc::new(move || {
            state.handle_need_init();
        });
        let state = Rc::clone(self);
        let on_warning = Rc::new(move |minutes_idle| {
            state.handle_warning(minutes_idle);
        });
        let state = Rc::clone(self);
        let on_state = Rc::new(move |snapshot, seq| {
            state.handle_state(snapshot, seq);
        });
        let state = Rc::clone(self);
        let on_update = Rc::new(move |update, seq, source, client_seq| {
            state.handle_update(update, seq, source, client_seq);
        });
        let state = Rc::clone(self);
        let on_ownership = Rc::new(move |anchor_id, owner| {
            state.handle_ownership(anchor_id, owner);
        });
        let state = Rc::clone(self);
        let on_drop_not_ready = Rc::new(move || {
            state.handle_drop_not_ready();
        });
        let state = Rc::clone(self);
        let on_error = Rc::new(move |code, message| {
            state.handle_error(code, message);
        });
        MultiplayerSyncCallbacks {
            on_welcome,
            on_need_init,
            on_warning,
            on_state,
            on_update,
            on_ownership,
            on_drop_not_ready,
            on_error,
        }
    }

    fn ui_hooks_snapshot(&self) -> Vec<MultiplayerUiHooks> {
        self.ui_hooks
            .borrow()
            .iter()
            .map(|(_, hooks)| hooks.clone())
            .collect()
    }

    fn register_ui_hooks(self: &Rc<Self>, hooks: MultiplayerUiHooks) -> UiHooksHandle {
        let id = self.next_ui_hook_id.get();
        self.next_ui_hook_id.set(id.wrapping_add(1));
        self.ui_hooks.borrow_mut().push((id, hooks));
        UiHooksHandle {
            id,
            state: Rc::clone(self),
        }
    }

    fn remove_ui_hooks(&self, id: u64) {
        self.ui_hooks.borrow_mut().retain(|(hook_id, _)| *hook_id != id);
    }

    fn handle_welcome(
        &self,
        room_id: String,
        persistence: RoomPersistence,
        initialized: bool,
        client_id: Option<ClientId>,
    ) {
        self.init_pending.set(!initialized);
        if let Some(sync) = sync_runtime::multiplayer_handle() {
            sync.borrow().set_state_applied(false);
        }
        *self.local_snapshot.borrow_mut() = LocalSnapshot::empty();
        self.pending_by_anchor.borrow_mut().clear();
        self.pending_flips.borrow_mut().clear();
        if !initialized {
            self.try_send_init();
        }
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_welcome)(room_id.clone(), persistence, initialized, client_id);
        }
    }

    fn handle_need_init(&self) {
        self.init_pending.set(true);
        self.pending_by_anchor.borrow_mut().clear();
        self.pending_flips.borrow_mut().clear();
        self.try_send_init();
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_need_init)();
        }
    }

    fn handle_warning(&self, minutes_idle: u32) {
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_warning)(minutes_idle);
        }
    }

    fn handle_state(&self, snapshot: GameSnapshot, seq: u64) {
        let applied = self.apply_room_snapshot(&snapshot);
        if applied {
            self.init_pending.set(false);
            if let Some(sync) = sync_runtime::multiplayer_handle() {
                sync.borrow().set_state_applied(true);
            }
        }
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_state)(snapshot.clone(), seq);
        }
    }

    fn handle_update(
        &self,
        update: RoomUpdate,
        seq: u64,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    ) {
        self.ack_pending_transform(&update, source, client_seq);
        self.apply_room_update(&update);
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_update)(update.clone(), seq, source, client_seq);
        }
    }

    fn handle_ownership(&self, anchor_id: u32, owner: Option<ClientId>) {
        self.maybe_drop_drag_on_ownership(anchor_id, owner);
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_ownership)(anchor_id, owner);
        }
    }

    fn handle_drop_not_ready(&self) {
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_drop_not_ready)();
        }
    }

    fn handle_error(&self, code: String, message: String) {
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_error)(code.clone(), message.clone());
        }
    }

    fn record_pending_transform(
        &self,
        anchor_id: u32,
        pos: (f32, f32),
        rot_deg: Option<f32>,
        client_seq: u64,
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
        };
        pending.insert(anchor_id, pending_entry);
    }

    fn record_pending_flip(&self, piece_id: u32, flipped: bool) {
        self.pending_flips.borrow_mut().insert(piece_id, flipped);
    }

    fn ack_pending_transform(
        &self,
        update: &RoomUpdate,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    ) {
        let Some(client_seq) = client_seq else {
            return;
        };
        let Some(source_id) = source else {
            return;
        };
        if sync_runtime::sync_view().client_id() != Some(source_id) {
            return;
        }
        let anchor_id = match update {
            RoomUpdate::GroupTransform { anchor_id, .. } => *anchor_id,
            _ => return,
        };
        let mut pending = self.pending_by_anchor.borrow_mut();
        let Some(entry) = pending.get(&anchor_id) else {
            return;
        };
        if client_seq >= entry.client_seq {
            pending.remove(&anchor_id);
        }
    }

    fn prune_pending_against_state(&self, state: &PuzzleStateSnapshot) {
        let mut pending = self.pending_by_anchor.borrow_mut();
        if !pending.is_empty() {
            let mut to_remove = Vec::new();
            for (anchor_id, entry) in pending.iter() {
                let idx = *anchor_id as usize;
                let Some(pos) = state.positions.get(idx) else {
                    to_remove.push(*anchor_id);
                    continue;
                };
                let Some(rot) = state.rotations.get(idx) else {
                    to_remove.push(*anchor_id);
                    continue;
                };
                let pos_match = (pos.0 - entry.pos.0).abs() <= PENDING_POS_EPS
                    && (pos.1 - entry.pos.1).abs() <= PENDING_POS_EPS;
                let rot_match = match entry.rot_deg {
                    Some(target) => angle_matches(*rot, target, PENDING_ROT_EPS),
                    None => true,
                };
                if pos_match && rot_match {
                    to_remove.push(*anchor_id);
                }
            }
            for anchor_id in to_remove {
                pending.remove(&anchor_id);
            }
        }
        drop(pending);
        let mut pending_flips = self.pending_flips.borrow_mut();
        if pending_flips.is_empty() {
            return;
        }
        let mut flips_remove = Vec::new();
        for (piece_id, desired) in pending_flips.iter() {
            let idx = *piece_id as usize;
            let Some(current) = state.flips.get(idx) else {
                flips_remove.push(*piece_id);
                continue;
            };
            if current == desired {
                flips_remove.push(*piece_id);
            }
        }
        for piece_id in flips_remove {
            pending_flips.remove(&piece_id);
        }
    }

    fn maybe_drop_drag_on_ownership(&self, anchor_id: u32, owner: Option<ClientId>) {
        let snapshot = self.core.snapshot();
        let Some(drag_anchor) = snapshot.dragging_members.first().copied() else {
            return;
        };
        if drag_anchor as u32 != anchor_id {
            return;
        }
        let my_id = sync_runtime::sync_view().client_id();
        let lost = match my_id {
            Some(id) => owner != Some(id),
            None => false,
        };
        if !lost {
            return;
        }
        self.pending_by_anchor.borrow_mut().remove(&anchor_id);
        self.drop_pending_flips_for_anchor(anchor_id);
        if !self.apply_predicted_state(false) {
            self.core.cancel_drag();
        }
    }

    fn drop_pending_flips_for_anchor(&self, anchor_id: u32) {
        let snapshot = self.core.snapshot();
        let cols = snapshot.grid.cols as usize;
        let rows = snapshot.grid.rows as usize;
        let total = cols * rows;
        let local = self.local_snapshot.borrow();
        if total == 0 || local.connections.len() != total {
            return;
        }
        let anchor_of = anchor_of_from_connections(&local.connections, cols, rows);
        let mut pending = self.pending_flips.borrow_mut();
        pending.retain(|piece_id, _| {
            let idx = *piece_id as usize;
            anchor_of.get(idx).copied().unwrap_or(usize::MAX) != anchor_id as usize
        });
    }

    fn try_send_init(&self) {
        if !self.init_pending.get() {
            return;
        }
        let snapshot = self.core.snapshot();
        let Some(puzzle) = snapshot.puzzle_info.clone() else {
            return;
        };
        let cols = puzzle.cols as usize;
        let rows = puzzle.rows as usize;
        let total = cols * rows;
        if total == 0 {
            return;
        }
        if snapshot.positions.len() != total
            || snapshot.rotations.len() != total
            || snapshot.flips.len() != total
            || snapshot.connections.len() != total
        {
            return;
        }
        let piece_order = if snapshot.z_order.len() == total {
            snapshot.z_order.clone()
        } else {
            (0..total).collect()
        };
        let anchor_of = anchor_of_from_connections(&snapshot.connections, cols, rows);
        let group_order =
            build_group_order_from_piece_order(&piece_order, &anchor_of);
        let group_order_u32: Vec<u32> = group_order
            .into_iter()
            .filter_map(|id| u32::try_from(id).ok())
            .collect();
        let state = PuzzleStateSnapshot {
            positions: snapshot.positions.clone(),
            rotations: snapshot.rotations.clone(),
            flips: snapshot.flips.clone(),
            connections: snapshot.connections.clone(),
            group_order: group_order_u32,
            scramble_nonce: snapshot.scramble_nonce,
        };
        let msg = ClientMsg::Init {
            puzzle,
            rules: None,
            state: Some(state),
        };
        if let Some(sync) = sync_runtime::multiplayer_handle() {
            sync.borrow().send(msg);
        }
        self.init_pending.set(false);
    }

    fn apply_room_snapshot(&self, snapshot: &GameSnapshot) -> bool {
        if snapshot.puzzle.image_width == 0 || snapshot.puzzle.image_height == 0 {
            console::warn!("multiplayer snapshot missing image size");
            return false;
        }
        let cols = snapshot.puzzle.cols as usize;
        let rows = snapshot.puzzle.rows as usize;
        let total = cols * rows;
        if total == 0 {
            console::warn!("multiplayer snapshot invalid grid");
            return false;
        }
        if snapshot.state.positions.len() != total
            || snapshot.state.rotations.len() != total
            || snapshot.state.flips.len() != total
            || snapshot.state.connections.len() != total
        {
            console::warn!("multiplayer snapshot invalid sizes");
            return false;
        }
        let grid_override = grid_override_for_puzzle(&snapshot.puzzle);
        let core_snapshot = self.core.snapshot();
        let should_update_core = core_snapshot
            .puzzle_info
            .as_ref()
            .map(|info| {
                info.image_src != snapshot.puzzle.image_src
                    || info.image_width != snapshot.puzzle.image_width
                    || info.image_height != snapshot.puzzle.image_height
                    || info.cols != snapshot.puzzle.cols
                    || info.rows != snapshot.puzzle.rows
            })
            .unwrap_or(true);
        let preserve_drag = !should_update_core && !core_snapshot.dragging_members.is_empty();
        if should_update_core {
            self.core.set_puzzle_with_grid(
                snapshot.puzzle.label.clone(),
                snapshot.puzzle.image_src.clone(),
                (snapshot.puzzle.image_width, snapshot.puzzle.image_height),
                Some(grid_override),
            );
        }
        let group_order = filter_group_order(&snapshot.state.group_order, total);
        let anchor_of = anchor_of_from_connections(&snapshot.state.connections, cols, rows);
        let piece_order = build_piece_order_from_groups(&group_order, &anchor_of);
        self.core.apply_snapshot_with_drag(
            snapshot.state.positions.clone(),
            snapshot.state.rotations.clone(),
            snapshot.state.flips.clone(),
            snapshot.state.connections.clone(),
            piece_order,
            snapshot.state.scramble_nonce,
            preserve_drag,
        );
        *self.local_snapshot.borrow_mut() = LocalSnapshot::from_room(snapshot);
        self.prune_pending_against_state(&snapshot.state);
        if !self.pending_by_anchor.borrow().is_empty() || !self.pending_flips.borrow().is_empty() {
            let _ = self.apply_predicted_state(preserve_drag);
        }
        true
    }

    fn apply_state_snapshot(
        &self,
        state_snapshot: PuzzleStateSnapshot,
        preserve_drag: bool,
    ) -> bool {
        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer state apply skipped (puzzle info not ready)");
            return false;
        };
        let cols = info.cols as usize;
        let rows = info.rows as usize;
        let total = cols * rows;
        if total == 0 {
            return false;
        }
        if state_snapshot.positions.len() != total
            || state_snapshot.rotations.len() != total
            || state_snapshot.flips.len() != total
            || state_snapshot.connections.len() != total
        {
            console::warn!("multiplayer state apply skipped (state size mismatch)");
            return false;
        }
        let group_order = filter_group_order(&state_snapshot.group_order, total);
        let anchor_of = anchor_of_from_connections(&state_snapshot.connections, cols, rows);
        let piece_order = build_piece_order_from_groups(&group_order, &anchor_of);
        self.core.apply_snapshot_with_drag(
            state_snapshot.positions.clone(),
            state_snapshot.rotations.clone(),
            state_snapshot.flips.clone(),
            state_snapshot.connections.clone(),
            piece_order,
            state_snapshot.scramble_nonce,
            preserve_drag,
        );
        true
    }

    fn apply_predicted_state(&self, preserve_drag: bool) -> bool {
        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer prediction skipped (puzzle info not ready)");
            return false;
        };
        let cols = info.cols as usize;
        let rows = info.rows as usize;
        let total = cols * rows;
        if total == 0 {
            return false;
        }
        let piece_width = info.image_width as f32 / info.cols as f32;
        let piece_height = info.image_height as f32 / info.rows as f32;
        let mut predicted = self.local_snapshot.borrow().clone().to_state_snapshot();
        let pending_snapshot = self.pending_by_anchor.borrow().clone();
        if !pending_snapshot.is_empty() {
            let mut invalid = Vec::new();
            for (anchor_id, pending) in pending_snapshot.iter() {
                let anchor = *anchor_id as usize;
                let rot_deg = pending.rot_deg.unwrap_or_else(|| {
                    predicted
                        .rotations
                        .get(anchor)
                        .copied()
                        .unwrap_or(0.0)
                });
                let update = RoomUpdate::GroupTransform {
                    anchor_id: *anchor_id,
                    pos: pending.pos,
                    rot_deg,
                };
                if !apply_room_update_to_snapshot(
                    &update,
                    &mut predicted,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                ) {
                    invalid.push(*anchor_id);
                }
            }
            if !invalid.is_empty() {
                let mut pending = self.pending_by_anchor.borrow_mut();
                for anchor_id in invalid {
                    pending.remove(&anchor_id);
                }
            }
        }
        let pending_flips_snapshot = self.pending_flips.borrow().clone();
        if !pending_flips_snapshot.is_empty() {
            let mut invalid = Vec::new();
            for (piece_id, flipped) in pending_flips_snapshot.iter() {
                let idx = *piece_id as usize;
                if idx >= predicted.flips.len() || idx >= predicted.connections.len() {
                    invalid.push(*piece_id);
                    continue;
                }
                if let Some(slot) = predicted.flips.get_mut(idx) {
                    *slot = *flipped;
                }
                clear_piece_connections(&mut predicted.connections, idx, cols, rows);
            }
            if !invalid.is_empty() {
                let mut pending = self.pending_flips.borrow_mut();
                for piece_id in invalid {
                    pending.remove(&piece_id);
                }
            }
            let group_order = filter_group_order(&predicted.group_order, total);
            let anchor_of = anchor_of_from_connections(&predicted.connections, cols, rows);
            let piece_order = build_piece_order_from_groups(&group_order, &anchor_of);
            let next_group_order = build_group_order_from_piece_order(&piece_order, &anchor_of);
            predicted.group_order = next_group_order
                .into_iter()
                .filter_map(|id| u32::try_from(id).ok())
                .collect();
        }
        self.apply_state_snapshot(predicted, preserve_drag)
    }

    fn apply_room_update(&self, update: &RoomUpdate) {
        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer update dropped (puzzle info not ready)");
            return;
        };
        let cols = info.cols as usize;
        let rows = info.rows as usize;
        let total = cols * rows;
        if total == 0 {
            return;
        }
        let piece_width = info.image_width as f32 / info.cols as f32;
        let piece_height = info.image_height as f32 / info.rows as f32;
        let local = self.local_snapshot.borrow().clone();
        if local.positions.len() != total
            || local.rotations.len() != total
            || local.flips.len() != total
            || local.connections.len() != total
        {
            console::warn!("multiplayer update dropped (state size mismatch)");
            return;
        }
        let mut state_snapshot = local.to_state_snapshot();
        if !apply_room_update_to_snapshot(
            update,
            &mut state_snapshot,
            cols,
            rows,
            piece_width,
            piece_height,
        ) {
            console::warn!("multiplayer update rejected");
            return;
        }
        self.prune_pending_against_state(&state_snapshot);
        *self.local_snapshot.borrow_mut() = LocalSnapshot::from_state(state_snapshot);
        let preserve_drag = !snapshot.dragging_members.is_empty();
        let _ = self.apply_predicted_state(preserve_drag);
    }
}

pub(crate) struct UiHooksHandle {
    id: u64,
    state: Rc<MultiplayerBridgeState>,
}

impl Drop for UiHooksHandle {
    fn drop(&mut self) {
        self.state.remove_ui_hooks(self.id);
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

pub(crate) fn register_ui_hooks(hooks: MultiplayerUiHooks) -> UiHooksHandle {
    let state = install_with_core(AppCore::shared());
    state.register_ui_hooks(hooks)
}

#[cfg(test)]
pub(crate) fn callbacks_for_tests(core: Rc<AppCore>) -> MultiplayerSyncCallbacks {
    let state = install_with_core(core);
    state.build_callbacks()
}

fn filter_group_order(group_order: &[u32], total: usize) -> Vec<usize> {
    group_order
        .iter()
        .filter_map(|id| {
            let id = *id as usize;
            if id < total {
                Some(id)
            } else {
                None
            }
        })
        .collect()
}

fn grid_override_for_puzzle(puzzle: &PuzzleInfo) -> GridChoice {
    let mut choices = build_grid_choices(puzzle.image_width, puzzle.image_height);
    if choices.is_empty() {
        choices.push(FALLBACK_GRID);
    }
    if let Some(index) = grid_choice_index(&choices, puzzle.cols, puzzle.rows) {
        return choices.get(index).copied().unwrap_or_else(|| fallback_grid(puzzle));
    }
    fallback_grid(puzzle)
}

fn fallback_grid(puzzle: &PuzzleInfo) -> GridChoice {
    let count = puzzle.cols.saturating_mul(puzzle.rows);
    GridChoice {
        target_count: count,
        cols: puzzle.cols,
        rows: puzzle.rows,
        actual_count: count,
    }
}

fn anchor_of_from_connections(connections: &[[bool; 4]], cols: usize, rows: usize) -> Vec<usize> {
    let total = cols * rows;
    let mut anchor_of = vec![0usize; total];
    let groups = groups_from_connections(connections, cols, rows);
    for group in groups {
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
