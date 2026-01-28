use std::cell::RefCell;
use std::rc::Rc;

use gloo::timers::callback::Timeout;

use crate::app_core::{AppCore, AppSnapshot, AppSubscription};
use crate::app_router::{self, MultiplayerConfig};
use crate::core::InitMode;
use crate::local_snapshot::{apply_game_snapshot_to_core, ApplySnapshotResult};
use crate::multiplayer_game_sync::{MultiplayerGameSync, MultiplayerSyncCallbacks};
use crate::runtime::{CoreAction, GameSync, LocalSyncAdapter, SyncAction, SyncHooks, SyncView};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ActiveSync {
    Local,
    Multiplayer,
}

struct SyncRuntimeState {
    local_sync: Option<LocalSyncAdapter>,
    hooks: Option<SyncHooks>,
    core: Option<Rc<AppCore>>,
    core_subscription: Option<AppSubscription>,
    last_snapshot: Option<AppSnapshot>,
    multiplayer: Option<Rc<RefCell<MultiplayerGameSync>>>,
    active: ActiveSync,
    config: Option<MultiplayerConfig>,
    callbacks: Option<MultiplayerSyncCallbacks>,
    on_fail: Rc<dyn Fn()>,
    active_room: Option<String>,
    retry_attempts: u32,
    retry_timer: Option<Timeout>,
    sync_view_hooks: Vec<(u64, Rc<dyn Fn()>)>,
    next_sync_view_hook_id: u64,
}

impl SyncRuntimeState {
    fn new() -> Self {
        Self {
            local_sync: None,
            hooks: None,
            core: None,
            core_subscription: None,
            last_snapshot: None,
            multiplayer: None,
            active: ActiveSync::Local,
            config: None,
            callbacks: None,
            on_fail: default_on_fail(),
            active_room: None,
            retry_attempts: 0,
            retry_timer: None,
            sync_view_hooks: Vec::new(),
            next_sync_view_hook_id: 0,
        }
    }

    fn ensure_backend(&mut self) {
        let wants_multiplayer = self.config.is_some();
        if wants_multiplayer {
            self.active = ActiveSync::Multiplayer;
            if self.multiplayer.is_none() {
                self.multiplayer = Some(Rc::new(RefCell::new(MultiplayerGameSync::new())));
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
            if let Some(hooks) = self.hooks.clone() {
                local_sync.init(hooks);
            }
            self.local_sync = Some(local_sync);
        }
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

fn reset_multiplayer_retry() {
    let mut should_notify = false;
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.retry_attempts = 0;
        state.retry_timer.take();
        should_notify = true;
    });
    if should_notify {
        notify_sync_view_changed();
    }
}

fn fail_multiplayer_now() {
    let mut should_notify = false;
    let on_fail = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if state.active != ActiveSync::Multiplayer {
            return None;
        }
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().disconnect();
        }
        state.active_room = None;
        state.retry_attempts = 0;
        state.retry_timer.take();
        should_notify = true;
        Some(state.on_fail.clone())
    });
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
        let hook = state.hooks.as_ref().map(|hooks| hooks.on_snapshot.clone());
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

    if let Some(hook) = hook {
        hook(snapshot);
    }
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
    let Some(callbacks) = state.callbacks.clone() else {
        return;
    };
    if state.active_room.as_deref() == Some(config.room_id.as_str()) {
        return;
    }
    sync.borrow_mut().disconnect();
    sync.borrow_mut().connect(
        &config.room_id,
        callbacks,
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

pub(crate) fn set_callbacks(callbacks: MultiplayerSyncCallbacks) {
    let on_welcome_inner = callbacks.on_welcome.clone();
    let on_error_inner = callbacks.on_error.clone();
    let callbacks = MultiplayerSyncCallbacks {
        on_welcome: Rc::new(move |room_id, persistence, initialized, client_id| {
            reset_multiplayer_retry();
            (on_welcome_inner)(room_id, persistence, initialized, client_id);
        }),
        on_need_init: callbacks.on_need_init.clone(),
        on_warning: callbacks.on_warning.clone(),
        on_state: callbacks.on_state.clone(),
        on_update: callbacks.on_update.clone(),
        on_ownership: callbacks.on_ownership.clone(),
        on_drop_not_ready: callbacks.on_drop_not_ready.clone(),
        on_error: Rc::new(move |code, message| {
            (on_error_inner)(code, message);
            fail_multiplayer_now();
        }),
    };
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.callbacks = Some(callbacks);
        state.ensure_backend();
        connect_if_ready(&mut state);
    });
}

#[allow(dead_code)]
pub(crate) fn clear_callbacks() {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.callbacks = None;
        if let Some(sync) = state.multiplayer.as_ref() {
            if state.active_room.is_some() {
                sync.borrow_mut().disconnect();
                state.active_room = None;
            }
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

pub(crate) fn init_local_hooks(hooks: SyncHooks) {
    let (snapshot, core) = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.hooks = Some(hooks.clone());
        if let Some(sync) = state.local_sync.as_mut() {
            sync.init(hooks);
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

pub(crate) fn shutdown_local_hooks() {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.hooks = None;
        if let Some(sync) = state.local_sync.as_mut() {
            sync.shutdown();
        }
    });
}

pub(crate) fn handle_local_action(action: &CoreAction) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        match state.active {
            ActiveSync::Local => {
                if let Some(sync) = state.local_sync.as_mut() {
                    sync.handle_local_action(action);
                }
            }
            ActiveSync::Multiplayer => {
                if let Some(sync) = state.multiplayer.as_ref() {
                    sync.borrow_mut().handle_local_action(action);
                }
            }
        }
    });
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

pub(crate) fn dispatch_view_action(core: &AppCore, action: CoreAction, apply_core: bool) {
    if should_block_actions() && !matches!(action, CoreAction::SetHovered { .. }) {
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
                flip_before = snapshot.flips.get(id).copied();
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
            if anchor_id >= snapshot.positions.len() {
                return;
            }
            if snapshot.drag_rotate_mode {
                if anchor_id >= snapshot.rotations.len() {
                    return;
                }
                let pos = snapshot.positions[anchor_id];
                let rot_deg = snapshot.rotations[anchor_id];
                handle_local_action(&CoreAction::Sync(SyncAction::Transform {
                    anchor_id,
                    pos,
                    rot_deg,
                }));
            } else {
                let pos = snapshot.positions[anchor_id];
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
                if let Some(after_flip) = snapshot.flips.get(primary_id).copied() {
                    if after_flip != before_flip {
                        handle_local_action(&CoreAction::Sync(SyncAction::Flip {
                            piece_id: primary_id,
                            flipped: after_flip,
                        }));
                        return;
                    }
                }
            }
            if anchor_id >= snapshot.positions.len() || anchor_id >= snapshot.rotations.len() {
                return;
            }
            let pos = snapshot.positions[anchor_id];
            let rot_deg = snapshot.rotations[anchor_id];
            handle_local_action(&CoreAction::Sync(SyncAction::Place {
                anchor_id,
                pos,
                rot_deg,
            }));
        }
        _ => {}
    }
}

pub(crate) fn sync_view() -> SyncView {
    STATE.with(|slot| {
        let state = slot.borrow();
        match state.active {
            ActiveSync::Local => state
                .local_sync
                .as_ref()
                .map(|sync| sync.sync_view())
                .unwrap_or_default(),
            ActiveSync::Multiplayer => state
                .multiplayer
                .as_ref()
                .map(|sync| sync.borrow().sync_view())
                .unwrap_or_default(),
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
    callbacks: MultiplayerSyncCallbacks,
) -> Rc<dyn Fn(heddobureika_core::ServerMsg)> {
    STATE.with(|slot| {
        if let Some(sync) = slot.borrow().multiplayer.as_ref() {
            sync.borrow_mut().install_handler(callbacks)
        } else {
            Rc::new(|_msg: heddobureika_core::ServerMsg| {})
        }
    })
}
