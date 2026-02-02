use std::cell::RefCell;
use std::rc::Rc;

use gloo::timers::callback::Timeout;

use crate::app_core::{AppCore, AppSnapshot, AppSubscription};
use crate::app_router::{self, MultiplayerConfig};
use crate::core::{collect_group, InitMode};
use crate::local_snapshot::{apply_game_snapshot_to_core, ApplySnapshotResult};
use crate::multiplayer_game_sync::MultiplayerGameSync;
use crate::runtime::{CoreAction, GameSync, LocalSyncAdapter, SyncAction, SyncHooks, SyncView};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ActiveSync {
    Local,
    Multiplayer,
}

struct SyncRuntimeState {
    local_sync: Option<LocalSyncAdapter>,
    ui_hooks: SyncHooks,
    system_hooks: SyncHooks,
    core: Option<Rc<AppCore>>,
    core_subscription: Option<AppSubscription>,
    last_snapshot: Option<AppSnapshot>,
    last_sync_view: SyncView,
    multiplayer: Option<Rc<RefCell<MultiplayerGameSync>>>,
    active: ActiveSync,
    config: Option<MultiplayerConfig>,
    on_fail: Rc<dyn Fn()>,
    active_room: Option<String>,
    retry_attempts: u32,
    retry_timer: Option<Timeout>,
    sync_view_hooks: Vec<(u64, Rc<dyn Fn()>)>,
    next_sync_view_hook_id: u64,
    mp_local_transform_observer:
        Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64)>>,
    mp_local_flip_observer: Option<Rc<dyn Fn(u32, bool)>>,
}

impl SyncRuntimeState {
    fn new() -> Self {
        Self {
            local_sync: None,
            ui_hooks: SyncHooks::empty(),
            system_hooks: SyncHooks::empty(),
            core: None,
            core_subscription: None,
            last_snapshot: None,
            last_sync_view: SyncView::default(),
            multiplayer: None,
            active: ActiveSync::Local,
            config: None,
            on_fail: default_on_fail(),
            active_room: None,
            retry_attempts: 0,
            retry_timer: None,
            sync_view_hooks: Vec::new(),
            next_sync_view_hook_id: 0,
            mp_local_transform_observer: None,
            mp_local_flip_observer: None,
        }
    }

    fn ensure_backend(&mut self) {
        let wants_multiplayer = self.config.is_some();
        if wants_multiplayer {
            self.active = ActiveSync::Multiplayer;
            if self.multiplayer.is_none() {
                self.multiplayer = Some(Rc::new(RefCell::new(MultiplayerGameSync::new())));
            }
            if let Some(sync) = self.multiplayer.as_ref() {
                sync.borrow_mut().init(self.combined_hooks());
            }
            if let (Some(sync), Some(observer)) =
                (self.multiplayer.as_ref(), self.mp_local_transform_observer.as_ref())
            {
                sync.borrow()
                    .set_local_transform_observer(Some(observer.clone()));
            }
            if let (Some(sync), Some(observer)) =
                (self.multiplayer.as_ref(), self.mp_local_flip_observer.as_ref())
            {
                sync.borrow()
                    .set_local_flip_observer(Some(observer.clone()));
            }
            if let Some(mut local_sync) = self.local_sync.take() {
                local_sync.shutdown();
            }
            return;
        }
        self.active = ActiveSync::Local;
        if let Some(handle) = self.multiplayer.take() {
            handle.borrow_mut().disconnect();
        }
        self.active_room = None;
        self.retry_attempts = 0;
        self.retry_timer.take();
        if self.local_sync.is_none() {
            let mut local_sync = LocalSyncAdapter::new();
            local_sync.init(self.combined_hooks());
            self.local_sync = Some(local_sync);
        }
    }
}

fn merge_hooks(primary: &SyncHooks, secondary: &SyncHooks) -> SyncHooks {
    let on_remote_action_a = primary.on_remote_action.clone();
    let on_remote_action_b = secondary.on_remote_action.clone();
    let on_snapshot_a = primary.on_snapshot.clone();
    let on_snapshot_b = secondary.on_snapshot.clone();
    let on_remote_snapshot_a = primary.on_remote_snapshot.clone();
    let on_remote_snapshot_b = secondary.on_remote_snapshot.clone();
    let on_remote_update_a = primary.on_remote_update.clone();
    let on_remote_update_b = secondary.on_remote_update.clone();
    let on_event_a = primary.on_event.clone();
    let on_event_b = secondary.on_event.clone();
    SyncHooks {
        on_remote_action: Rc::new(move |action| {
            on_remote_action_a(action.clone());
            on_remote_action_b(action);
        }),
        on_snapshot: Rc::new(move |snapshot| {
            on_snapshot_a(snapshot.clone());
            on_snapshot_b(snapshot);
        }),
        on_remote_snapshot: Rc::new(move |snapshot, seq| {
            on_remote_snapshot_a(snapshot.clone(), seq);
            on_remote_snapshot_b(snapshot, seq);
        }),
        on_remote_update: Rc::new(move |update, seq, source, client_seq| {
            on_remote_update_a(update.clone(), seq, source, client_seq);
            on_remote_update_b(update, seq, source, client_seq);
        }),
        on_event: Rc::new(move |event| {
            on_event_a(event.clone());
            on_event_b(event);
        }),
    }
}

impl SyncRuntimeState {
    fn combined_hooks(&self) -> SyncHooks {
        merge_hooks(&self.system_hooks, &self.ui_hooks)
    }
}

const RETRY_DELAYS_MS: &[u32] = &[200, 500, 1_000, 2_000, 4_000, 8_000, 15_000, 30_000];

fn schedule_multiplayer_retry() {
    let mut should_notify = false;
    let (on_fail, delay_ms) = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if state.active != ActiveSync::Multiplayer || state.config.is_none() {
            return (None, None);
        }
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().disconnect();
        }
        state.active_room = None;
        should_notify = true;
        let Some(delay) = RETRY_DELAYS_MS.get(state.retry_attempts as usize).copied() else {
            state.retry_attempts = 0;
            state.retry_timer.take();
            return (Some(state.on_fail.clone()), None);
        };
        state.retry_attempts = state.retry_attempts.saturating_add(1);
        let timer = Timeout::new(delay, || {
            STATE.with(|slot| {
                let mut state = slot.borrow_mut();
                if state.active != ActiveSync::Multiplayer {
                    return;
                }
                connect_if_ready(&mut state);
            });
        });
        state.retry_timer = Some(timer);
        (None, Some(delay))
    });
    let _ = delay_ms;
    if should_notify {
        notify_sync_view_changed();
    }
    if let Some(on_fail) = on_fail {
        on_fail();
    }
}

fn notify_sync_view_changed() {
    let hooks = STATE.with(|slot| {
        let state = slot.borrow();
        state
            .sync_view_hooks
            .iter()
            .map(|(_, hook)| hook.clone())
            .collect::<Vec<_>>()
    });
    for hook in hooks {
        hook();
    }
}

thread_local! {
    static STATE: RefCell<SyncRuntimeState> = RefCell::new(SyncRuntimeState::new());
}

fn handle_local_snapshot(snapshot: AppSnapshot, core: Rc<AppCore>) {
    let (allow_persist, hook, pending) = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.last_snapshot = Some(snapshot.clone());
        let allow_persist = matches!(state.active, ActiveSync::Local);
        let pending = if allow_persist {
            state
                .local_sync
                .as_mut()
                .and_then(|sync| sync.take_pending_snapshot())
        } else {
            None
        };
        let hook = state.combined_hooks().on_snapshot;
        (allow_persist, hook, pending)
    });

    if allow_persist {
        let mut skip_save = false;
        if let Some(pending_snapshot) = pending {
            match apply_game_snapshot_to_core(&pending_snapshot, &core, &snapshot) {
                ApplySnapshotResult::Applied => {
                    return;
                }
                ApplySnapshotResult::NotReady => {
                    skip_save = true;
                    STATE.with(|slot| {
                        if let Some(sync) = slot.borrow_mut().local_sync.as_mut() {
                            sync.requeue_pending_snapshot(pending_snapshot);
                        }
                    });
                }
                ApplySnapshotResult::Mismatch => {}
            }
        }
        if !skip_save {
            STATE.with(|slot| {
                if let Some(sync) = slot.borrow_mut().local_sync.as_mut() {
                    sync.save_if_needed(&snapshot);
                }
            });
        }
    }

    hook(snapshot);
}

fn default_on_fail() -> Rc<dyn Fn()> {
    Rc::new(move || {
        app_router::clear_room_session();
        app_router::save_mode_preference(InitMode::Local);
        app_router::clear_location_hash();
        if let Some(window) = web_sys::window() {
            let _ = window.location().reload();
        }
    })
}

fn connect_if_ready(state: &mut SyncRuntimeState) {
    let Some(sync) = state.multiplayer.as_ref() else {
        state.active_room = None;
        return;
    };
    let Some(config) = state.config.clone() else {
        if state.active_room.is_some() {
            sync.borrow_mut().disconnect();
            state.active_room = None;
        }
        return;
    };
    if state.active_room.as_deref() == Some(config.room_id.as_str()) {
        return;
    }
    sync.borrow_mut().disconnect();
    sync.borrow_mut().connect(
        &config.room_id,
        Rc::new(move || {
            schedule_multiplayer_retry();
        }),
    );
    state.active_room = Some(config.room_id);
}

pub(crate) fn init_from_config(config: Option<MultiplayerConfig>) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.config = config;
        state.retry_attempts = 0;
        state.retry_timer.take();
        state.ensure_backend();
        connect_if_ready(&mut state);
    });
    notify_sync_view_changed();
}

pub(crate) fn attach_core(core: Rc<AppCore>) {
    let already_installed = STATE.with(|slot| slot.borrow().core_subscription.is_some());
    if already_installed {
        return;
    }
    let core_for_subscription = core.clone();
    let subscription = core.subscribe(Rc::new(move || {
        let snapshot = core_for_subscription.snapshot();
        handle_local_snapshot(snapshot, core_for_subscription.clone());
    }));
    let snapshot = core.snapshot();
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if state.core_subscription.is_some() {
            return;
        }
        state.core = Some(core.clone());
        state.last_snapshot = Some(snapshot.clone());
        state.core_subscription = Some(subscription);
    });
    handle_local_snapshot(snapshot, core);
}

pub(crate) fn set_system_hooks(hooks: SyncHooks) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.system_hooks = hooks;
        state.ensure_backend();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().init(state.combined_hooks());
        }
    });
}

pub(crate) fn multiplayer_handle() -> Option<Rc<RefCell<MultiplayerGameSync>>> {
    STATE.with(|slot| slot.borrow().multiplayer.clone())
}

pub(crate) fn set_state_applied(value: bool) {
    STATE.with(|slot| {
        if let Some(sync) = slot.borrow().multiplayer.as_ref() {
            sync.borrow().set_state_applied(value);
        }
    });
}

pub(crate) fn set_sync_hooks(hooks: SyncHooks) {
    let (snapshot, core) = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.ui_hooks = hooks;
        let hooks = state.combined_hooks();
        if let Some(sync) = state.local_sync.as_mut() {
            sync.init(hooks.clone());
        }
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().init(hooks);
        }
        (state.last_snapshot.clone(), state.core.clone())
    });
    if let (Some(snapshot), Some(core)) = (snapshot, core) {
        handle_local_snapshot(snapshot, core);
    }
}

pub(crate) fn set_local_observer(observer: Option<Rc<dyn Fn(&CoreAction)>>) {
    STATE.with(|slot| {
        if let Some(sync) = slot.borrow_mut().local_sync.as_mut() {
            sync.set_observer(observer);
        }
    });
}

pub(crate) fn set_multiplayer_local_transform_observer(
    observer: Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64)>>,
) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_transform_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_transform_observer(observer);
        }
    });
}

pub(crate) fn set_multiplayer_local_flip_observer(observer: Option<Rc<dyn Fn(u32, bool)>>) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_flip_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_flip_observer(observer);
        }
    });
}

pub(crate) fn clear_sync_hooks() {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.ui_hooks = SyncHooks::empty();
        if let Some(sync) = state.local_sync.as_mut() {
            sync.shutdown();
        }
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().init(state.combined_hooks());
        }
    });
}

pub(crate) fn handle_local_action(action: &CoreAction) {
    enum SyncHandle {
        Local(LocalSyncAdapter),
        Multiplayer(Rc<RefCell<MultiplayerGameSync>>),
        None,
    }

    let handle = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        match state.active {
            ActiveSync::Local => state
                .local_sync
                .take()
                .map(SyncHandle::Local)
                .unwrap_or(SyncHandle::None),
            ActiveSync::Multiplayer => state
                .multiplayer
                .clone()
                .map(SyncHandle::Multiplayer)
                .unwrap_or(SyncHandle::None),
        }
    });

    match handle {
        SyncHandle::Local(mut sync) => {
            sync.handle_local_action(action);
            STATE.with(|slot| {
                let mut state = slot.borrow_mut();
                if state.active == ActiveSync::Local && state.local_sync.is_none() {
                    state.local_sync = Some(sync);
                }
            });
        }
        SyncHandle::Multiplayer(sync) => {
            sync.borrow_mut().handle_local_action(action);
        }
        SyncHandle::None => {}
    }
}

fn should_block_actions() -> bool {
    STATE.with(|slot| {
        let state = slot.borrow();
        if state.active != ActiveSync::Multiplayer {
            return false;
        }
        let Some(sync) = state.multiplayer.as_ref() else {
            return true;
        };
        let connected = sync.borrow().sync_view().connected();
        !connected
    })
}

fn anchor_for_piece(snapshot: &AppSnapshot, piece_id: usize) -> Option<usize> {
    let cols = snapshot.grid.cols as usize;
    let rows = snapshot.grid.rows as usize;
    if cols == 0 || rows == 0 {
        return None;
    }
    let total = cols * rows;
    if piece_id >= total {
        return None;
    }
    if snapshot.core.connections.len() < total {
        return None;
    }
    let mut members = collect_group(&snapshot.core.connections, piece_id, cols, rows);
    if members.is_empty() {
        members.push(piece_id);
    }
    members.into_iter().min()
}

fn should_block_owned_action(core: &AppCore, action: &CoreAction) -> bool {
    let CoreAction::BeginDrag { piece_id, .. } = action else {
        return false;
    };
    let snapshot = core.snapshot();
    STATE.with(|slot| {
        let state = slot.borrow();
        if state.active != ActiveSync::Multiplayer {
            return false;
        }
        let Some(sync) = state.multiplayer.as_ref() else {
            return false;
        };
        let sync_view = sync.borrow().sync_view();
        let ownership = sync_view.ownership_by_anchor();
        if ownership.is_empty() {
            return false;
        }
        let Some(anchor_id) = anchor_for_piece(&snapshot, *piece_id) else {
            return false;
        };
        if let Some(owner_id) = ownership.get(&(anchor_id as u32)) {
            Some(*owner_id) != sync_view.client_id()
        } else {
            false
        }
    })
}

pub(crate) fn dispatch_view_action(core: &AppCore, action: CoreAction, apply_core: bool) {
    if should_block_actions() && !matches!(action, CoreAction::SetHovered { .. }) {
        return;
    }
    if should_block_owned_action(core, &action) {
        return;
    }
    let mut drag_anchor_before = None;
    let mut drag_primary_before = None;
    let mut flip_before = None;
    if apply_core {
        if matches!(action, CoreAction::DragEnd { .. }) {
            let snapshot = core.snapshot();
            drag_anchor_before = snapshot.dragging_members.first().copied();
            drag_primary_before = snapshot.drag_primary_id;
            if let Some(id) = drag_primary_before {
                flip_before = snapshot.core.flips.get(id).copied();
            }
        }
        core.apply_action(action.clone());
    }
    handle_local_action(&action);
    if !apply_core {
        return;
    }
    match action {
        CoreAction::DragMove { .. } => {
            let snapshot = core.snapshot();
            let Some(anchor_id) = snapshot.dragging_members.first().copied() else {
                return;
            };
            if anchor_id >= snapshot.core.positions.len() {
                return;
            }
            if snapshot.drag_rotate_mode {
                if anchor_id >= snapshot.core.rotations.len() {
                    return;
                }
                let pos = snapshot.core.positions[anchor_id];
                let rot_deg = snapshot.core.rotations[anchor_id];
                handle_local_action(&CoreAction::Sync(SyncAction::Transform {
                    anchor_id,
                    pos,
                    rot_deg,
                }));
            } else {
                let pos = snapshot.core.positions[anchor_id];
                handle_local_action(&CoreAction::Sync(SyncAction::Move { anchor_id, pos }));
            }
        }
        CoreAction::DragEnd { .. } => {
            let Some(anchor_id) = drag_anchor_before else {
                return;
            };
            let snapshot = core.snapshot();
            if !snapshot.dragging_members.is_empty() {
                return;
            }
            if let (Some(primary_id), Some(before_flip)) = (drag_primary_before, flip_before) {
                if let Some(after_flip) = snapshot.core.flips.get(primary_id).copied() {
                    if after_flip != before_flip {
                        handle_local_action(&CoreAction::Sync(SyncAction::Flip {
                            piece_id: primary_id,
                            flipped: after_flip,
                        }));
                        return;
                    }
                }
            }
            if anchor_id >= snapshot.core.positions.len() || anchor_id >= snapshot.core.rotations.len() {
                return;
            }
            let pos = snapshot.core.positions[anchor_id];
            let rot_deg = snapshot.core.rotations[anchor_id];
            handle_local_action(&CoreAction::Sync(SyncAction::Place {
                anchor_id,
                pos,
                rot_deg,
            }));
        }
        _ => {}
    }
}

pub(crate) fn clear_local_snapshot() {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if let Some(sync) = state.local_sync.as_mut() {
            sync.clear_saved_snapshot();
        } else {
            LocalSyncAdapter::clear_storage();
        }
    });
}

pub(crate) fn sync_view() -> SyncView {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        match state.active {
            ActiveSync::Local => {
                let view = state
                    .local_sync
                    .as_ref()
                    .map(|sync| sync.sync_view())
                    .unwrap_or_default();
                state.last_sync_view = view.clone();
                view
            }
            ActiveSync::Multiplayer => {
                let multiplayer = state.multiplayer.clone();
                let Some(sync) = multiplayer else {
                    return state.last_sync_view.clone();
                };
                let view = match sync.try_borrow() {
                    Ok(sync) => Some(sync.sync_view()),
                    Err(_) => None,
                };
                if let Some(view) = view {
                    state.last_sync_view = view.clone();
                    view
                } else {
                    state.last_sync_view.clone()
                }
            }
        }
    })
}

pub(crate) fn set_on_fail(handler: Rc<dyn Fn()>) {
    STATE.with(|slot| {
        slot.borrow_mut().on_fail = handler;
    });
}

pub(crate) struct SyncViewHookHandle {
    id: u64,
}

impl Drop for SyncViewHookHandle {
    fn drop(&mut self) {
        STATE.with(|slot| {
            slot.borrow_mut()
                .sync_view_hooks
                .retain(|(id, _)| *id != self.id);
        });
    }
}

pub(crate) fn register_sync_view_hook(hook: Rc<dyn Fn()>) -> SyncViewHookHandle {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        let id = state.next_sync_view_hook_id;
        state.next_sync_view_hook_id = id.wrapping_add(1);
        state.sync_view_hooks.push((id, hook));
        SyncViewHookHandle { id }
    })
}

#[cfg(test)]
pub(crate) fn install_test_handler(
    hooks: SyncHooks,
) -> Rc<dyn Fn(heddobureika_core::ServerMsg)> {
    STATE.with(|slot| {
        if let Some(sync) = slot.borrow().multiplayer.as_ref() {
            sync.borrow_mut().install_handler(hooks)
        } else {
            Rc::new(|_msg: heddobureika_core::ServerMsg| {})
        }
    })
}
