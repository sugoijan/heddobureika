use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::console;

use crate::app_core::{AppCore, AppSubscription};
use crate::core::{
    build_group_order_from_piece_order, build_grid_choices, build_piece_order_from_groups,
    grid_choice_index, groups_from_connections, GridChoice, FALLBACK_GRID,
};
use crate::multiplayer_game_sync::MultiplayerSyncCallbacks;
use crate::sync_runtime;
use heddobureika_core::{
    apply_room_update_to_snapshot, ClientMsg, GameSnapshot, PuzzleInfo, PuzzleStateSnapshot,
    RoomPersistence, RoomUpdate,
};

#[derive(Clone)]
pub(crate) struct MultiplayerUiHooks {
    pub(crate) on_welcome: Rc<dyn Fn(String, RoomPersistence, bool, Option<u64>)>,
    pub(crate) on_need_init: Rc<dyn Fn()>,
    pub(crate) on_warning: Rc<dyn Fn(u32)>,
    pub(crate) on_state: Rc<dyn Fn(GameSnapshot, u64)>,
    pub(crate) on_update: Rc<dyn Fn(RoomUpdate, u64)>,
    pub(crate) on_ownership: Rc<dyn Fn(u32, Option<u64>)>,
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

struct MultiplayerBridgeState {
    core: Rc<AppCore>,
    local_snapshot: RefCell<LocalSnapshot>,
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
        let on_update = Rc::new(move |update, seq| {
            state.handle_update(update, seq);
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
        client_id: Option<u64>,
    ) {
        self.init_pending.set(!initialized);
        if let Some(sync) = sync_runtime::multiplayer_handle() {
            sync.borrow().set_state_applied(false);
        }
        *self.local_snapshot.borrow_mut() = LocalSnapshot::empty();
        if !initialized {
            self.try_send_init();
        }
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_welcome)(room_id.clone(), persistence, initialized, client_id);
        }
    }

    fn handle_need_init(&self) {
        self.init_pending.set(true);
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

    fn handle_update(&self, update: RoomUpdate, seq: u64) {
        self.apply_room_update(&update);
        for hooks in self.ui_hooks_snapshot() {
            (hooks.on_update)(update.clone(), seq);
        }
    }

    fn handle_ownership(&self, anchor_id: u32, owner: Option<u64>) {
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
        true
    }

    fn apply_room_update(&self, update: &RoomUpdate) {
        let snapshot = self.core.snapshot();
        let Some(info) = snapshot.puzzle_info.clone() else {
            console::warn!("multiplayer update dropped (puzzle info not ready)");
            return;
        };
        let preserve_drag = !snapshot.dragging_members.is_empty();
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
        *self.local_snapshot.borrow_mut() = LocalSnapshot::from_state(state_snapshot);
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
