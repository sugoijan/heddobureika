use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use gloo::console;

use crate::app_core::{AppCore, AppSubscription};
use crate::core::{
    build_group_order_from_piece_order, build_grid_choices, build_piece_order_from_groups,
    clear_piece_connections, grid_choice_index, groups_from_connections, is_solved, GridChoice,
    FALLBACK_GRID,
};
use crate::sync_runtime;
use crate::runtime::{SyncEvent, SyncHooks};
use heddobureika_core::{
    angle_matches, apply_room_update_to_snapshot, ClientId, ClientMsg, CoreSnapshot, GameSnapshot,
    PuzzleInfo, PuzzleStateSnapshot, RoomPersistence, RoomUpdate,
};

const PENDING_POS_EPS: f32 = 0.02;
const PENDING_ROT_EPS: f32 = 0.05;

#[derive(Clone, Debug)]
struct LocalSnapshot {
    core: CoreSnapshot,
}

impl LocalSnapshot {
    fn empty() -> Self {
        Self {
            core: CoreSnapshot {
                positions: Vec::new(),
                rotations: Vec::new(),
                flips: Vec::new(),
                connections: Vec::new(),
                group_order: Vec::new(),
                scramble_nonce: 0,
            },
        }
    }

    fn from_room(snapshot: &GameSnapshot) -> Self {
        Self {
            core: snapshot.state.clone(),
        }
    }

    fn from_state(snapshot: CoreSnapshot) -> Self {
        Self {
            core: snapshot,
        }
    }

    fn to_state_snapshot(&self) -> PuzzleStateSnapshot {
        self.core.clone()
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
            subscription: RefCell::new(None),
        }
    }

    fn install(self: &Rc<Self>) {
        let hooks = self.build_hooks();
        sync_runtime::set_system_hooks(hooks);
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
        SyncHooks {
            on_remote_action: Rc::new(|_| {}),
            on_snapshot: Rc::new(|_| {}),
            on_remote_snapshot,
            on_remote_update,
            on_event,
        }
    }

    fn handle_welcome(
        &self,
        _room_id: String,
        _persistence: RoomPersistence,
        initialized: bool,
        _client_id: Option<ClientId>,
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
    }

    fn handle_need_init(&self) {
        self.init_pending.set(true);
        self.pending_by_anchor.borrow_mut().clear();
        self.pending_flips.borrow_mut().clear();
        self.try_send_init();
    }

    fn handle_warning(&self, _minutes_idle: u32) {
    }

    fn handle_state(&self, snapshot: GameSnapshot, _seq: u64) {
        let applied = self.apply_room_snapshot(&snapshot);
        if applied {
            self.init_pending.set(false);
            if let Some(sync) = sync_runtime::multiplayer_handle() {
                sync.borrow().set_state_applied(true);
            }
        }
    }

    fn handle_update(
        &self,
        update: RoomUpdate,
        _seq: u64,
        source: Option<ClientId>,
        client_seq: Option<u64>,
    ) {
        self.ack_pending_transform(&update, source, client_seq);
        self.apply_room_update(&update);
    }

    fn handle_ownership(&self, anchor_id: u32, owner: Option<ClientId>) {
        self.maybe_drop_drag_on_ownership(anchor_id, owner);
    }

    fn handle_drop_not_ready(&self) {
    }

    fn handle_error(&self, _code: String, _message: String) {
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
        drop(pending);
        let _ = self.apply_predicted_state(true);
    }

    fn record_pending_flip(&self, piece_id: u32, flipped: bool) {
        self.pending_flips.borrow_mut().insert(piece_id, flipped);
        let _ = self.apply_predicted_state(true);
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
        if total == 0 || local.core.connections.len() != total {
            return;
        }
        let anchor_of = anchor_of_from_connections(&local.core.connections, cols, rows);
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
        if snapshot.core.positions.len() != total
            || snapshot.core.rotations.len() != total
            || snapshot.core.flips.len() != total
            || snapshot.core.connections.len() != total
        {
            return;
        }
        let piece_order = if snapshot.z_order.len() == total {
            snapshot.z_order.clone()
        } else {
            (0..total).collect()
        };
        let anchor_of = anchor_of_from_connections(&snapshot.core.connections, cols, rows);
        let group_order =
            build_group_order_from_piece_order(&piece_order, &anchor_of);
        let group_order_u32: Vec<u32> = group_order
            .into_iter()
            .filter_map(|id| u32::try_from(id).ok())
            .collect();
        let state = PuzzleStateSnapshot {
            positions: snapshot.core.positions.clone(),
            rotations: snapshot.core.rotations.clone(),
            flips: snapshot.core.flips.clone(),
            connections: snapshot.core.connections.clone(),
            group_order: group_order_u32,
            scramble_nonce: snapshot.core.scramble_nonce,
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
        let group_order_u32: Vec<u32> = group_order
            .iter()
            .filter_map(|id| u32::try_from(*id).ok())
            .collect();
        let piece_width = info.image_width as f32 / info.cols as f32;
        let piece_height = info.image_height as f32 / info.rows as f32;
        let rotation_enabled = self.core.rotation_enabled();
        let solved = is_solved(
            &state_snapshot.positions,
            &state_snapshot.rotations,
            &state_snapshot.flips,
            &state_snapshot.connections,
            cols,
            rows,
            piece_width,
            piece_height,
            rotation_enabled,
        );
        let positions = state_snapshot.positions;
        let rotations = state_snapshot.rotations;
        let flips = state_snapshot.flips;
        let connections = state_snapshot.connections;
        let scramble_nonce = state_snapshot.scramble_nonce;
        self.core.mutate_snapshot(move |snapshot| {
            snapshot.core.positions = positions;
            snapshot.core.rotations = rotations;
            snapshot.core.flips = flips;
            snapshot.core.connections = connections;
            snapshot.core.group_order = group_order_u32;
            snapshot.core.scramble_nonce = scramble_nonce;
            snapshot.z_order = piece_order;
            snapshot.solved = solved;
            if !preserve_drag {
                snapshot.dragging_members.clear();
                snapshot.active_id = None;
                snapshot.drag_cursor = None;
                snapshot.drag_pointer_id = None;
                snapshot.drag_rotate_mode = false;
                snapshot.drag_right_click = false;
                snapshot.drag_primary_id = None;
            }
        });
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
        if local.core.positions.len() != total
            || local.core.rotations.len() != total
            || local.core.flips.len() != total
            || local.core.connections.len() != total
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
