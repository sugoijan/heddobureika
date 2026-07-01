use std::cell::RefCell;
use std::rc::Rc;

use gloo::timers::callback::Timeout;
use gloo::timers::future::TimeoutFuture;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::{spawn_local, JsFuture};

use crate::app_core::{AppCore, AppSnapshot, AppSubscription};
use crate::app_router::{self, MultiplayerConfig};
use crate::boot_runtime::{self, BootState};
use crate::core::InitMode;
use crate::local_snapshot::{apply_playable_snapshot_to_core, ApplySnapshotResult};
use crate::multiplayer_game_sync::MultiplayerGameSync;
use crate::runtime::{
    CoreAction, FailReason, GameSync, LocalSyncAdapter, SyncAction, SyncHooks, SyncView,
};

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
    mp_local_transform_observer: Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64, bool)>>,
    mp_local_flip_observer: Option<Rc<dyn Fn(u32, bool, (f32, f32), f32)>>,
    mp_local_detach_observer: Option<Rc<dyn Fn(u32)>>,
    mp_local_send_to_back_observer: Option<Rc<dyn Fn(u32)>>,
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
            mp_local_detach_observer: None,
            mp_local_send_to_back_observer: None,
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
            if let (Some(sync), Some(observer)) = (
                self.multiplayer.as_ref(),
                self.mp_local_transform_observer.as_ref(),
            ) {
                sync.borrow()
                    .set_local_transform_observer(Some(observer.clone()));
            }
            if let (Some(sync), Some(observer)) = (
                self.multiplayer.as_ref(),
                self.mp_local_flip_observer.as_ref(),
            ) {
                sync.borrow()
                    .set_local_flip_observer(Some(observer.clone()));
            }
            if let (Some(sync), Some(observer)) = (
                self.multiplayer.as_ref(),
                self.mp_local_detach_observer.as_ref(),
            ) {
                sync.borrow()
                    .set_local_detach_observer(Some(observer.clone()));
            }
            if let (Some(sync), Some(observer)) = (
                self.multiplayer.as_ref(),
                self.mp_local_send_to_back_observer.as_ref(),
            ) {
                sync.borrow()
                    .set_local_send_to_back_observer(Some(observer.clone()));
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
    let on_remote_playable_update_a = primary.on_remote_playable_update.clone();
    let on_remote_playable_update_b = secondary.on_remote_playable_update.clone();
    let on_event_a = primary.on_event.clone();
    let on_event_b = secondary.on_event.clone();
    let on_asset_a = primary.on_asset.clone();
    let on_asset_b = secondary.on_asset.clone();
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
        on_remote_playable_update: Rc::new(move |update, seq, source, client_seq| {
            on_remote_playable_update_a(update.clone(), seq, source, client_seq);
            on_remote_playable_update_b(update, seq, source, client_seq);
        }),
        on_event: Rc::new(move |event| {
            on_event_a(event.clone());
            on_event_b(event);
        }),
        on_asset: Rc::new(move |event| {
            on_asset_a(event.clone());
            on_asset_b(event);
        }),
    }
}

impl SyncRuntimeState {
    fn combined_hooks(&self) -> SyncHooks {
        merge_hooks(&self.system_hooks, &self.ui_hooks)
    }
}

const RETRY_DELAYS_MS: &[u32] = &[200, 500, 1_000, 2_000, 4_000, 8_000, 15_000, 30_000];
const BOOT_WAIT_POLL_MS: u32 = 25;
const BOOT_WAIT_TIMEOUT_MS: u32 = 10_000;

fn schedule_multiplayer_retry(reason: FailReason) {
    match reason {
        // The room was reachable (the socket had opened) — a drop is most
        // likely a transient network issue, so keep the resilient backoff.
        FailReason::Dropped => multiplayer_backoff_retry(),
        // Nothing to retry against: fall back to the local game immediately.
        FailReason::Terminal => multiplayer_fail_to_local(),
        // Ambiguous: a gone/expired room rejects the WebSocket upgrade with a
        // 403 that the browser cannot see, so it looks identical to a network
        // blip here. Ask the server over plain HTTP which one it is before
        // deciding whether to keep retrying or give up.
        FailReason::NeverOpened => {
            let probe = STATE.with(|slot| {
                let state = slot.borrow();
                if state.active != ActiveSync::Multiplayer {
                    return None;
                }
                let config = state.config.as_ref()?;
                let url = app_router::build_room_probe_url(&config.room_id)?;
                Some((url, config.resumed))
            });
            let Some((url, resumed)) = probe else {
                // No probe target (not multiplayer any more, or no ws base):
                // preserve the previous behaviour and just back off.
                multiplayer_backoff_retry();
                return;
            };
            spawn_local(async move {
                if probe_room_gone(&url, resumed).await {
                    multiplayer_fail_to_local();
                } else {
                    multiplayer_backoff_retry();
                }
            });
        }
    }
}

fn multiplayer_backoff_retry() {
    let mut should_notify = false;
    let on_fail = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if state.active != ActiveSync::Multiplayer || state.config.is_none() {
            return None;
        }
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow_mut().disconnect();
        }
        state.active_room = None;
        should_notify = true;
        let Some(delay) = RETRY_DELAYS_MS.get(state.retry_attempts as usize).copied() else {
            state.retry_attempts = 0;
            state.retry_timer.take();
            return Some(state.on_fail.clone());
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
        None
    });
    if should_notify {
        notify_sync_view_changed();
    }
    if let Some(on_fail) = on_fail {
        on_fail();
    }
}

fn multiplayer_fail_to_local() {
    let mut should_notify = false;
    let on_fail = STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        if state.active != ActiveSync::Multiplayer || state.config.is_none() {
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

/// Asks the server whether a room is gone. Returns `true` only when the room is
/// definitively unavailable so retrying is pointless; any ambiguous outcome
/// (200, 5xx, or a network error) returns `false` so we keep the resilient
/// backoff rather than nuking a still-valid session on a transient blip.
async fn probe_room_gone(url: &str, resumed: bool) -> bool {
    let Some(window) = web_sys::window() else {
        return false;
    };
    let response = match JsFuture::from(window.fetch_with_str(url)).await {
        Ok(value) => value,
        Err(_) => return false,
    };
    let Ok(response) = response.dyn_into::<web_sys::Response>() else {
        return false;
    };
    match response.status() {
        // "Room not activated": for a resumed session the room was live when we
        // saved it, so this means it expired — terminal. For an explicit join
        // the room may just be activating, so keep retrying.
        403 => resumed,
        // Unknown/invalid room id: gone regardless of how we got here.
        404 => true,
        // Reachable (200), server hiccup (5xx), or anything else: transient.
        _ => false,
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
            match apply_playable_snapshot_to_core(&pending_snapshot, &core, &snapshot) {
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
        Rc::new(move |reason| {
            schedule_multiplayer_retry(reason);
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

pub(crate) async fn wait_for_ready() {
    let mut waited_ms = 0u32;
    loop {
        let view = sync_view();
        match view.mode() {
            InitMode::Local => return,
            InitMode::Online => {
                if view.connected() {
                    return;
                }
            }
        }
        if waited_ms >= BOOT_WAIT_TIMEOUT_MS {
            let on_fail = STATE.with(|slot| slot.borrow().on_fail.clone());
            on_fail();
            return;
        }
        TimeoutFuture::new(BOOT_WAIT_POLL_MS).await;
        waited_ms = waited_ms.saturating_add(BOOT_WAIT_POLL_MS);
    }
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

pub(crate) fn current_app_snapshot() -> Option<AppSnapshot> {
    STATE.with(|slot| slot.borrow().last_snapshot.clone())
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
    observer: Option<Rc<dyn Fn(u32, (f32, f32), Option<f32>, u64, bool)>>,
) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_transform_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_transform_observer(observer);
        }
    });
}

pub(crate) fn set_multiplayer_local_flip_observer(
    observer: Option<Rc<dyn Fn(u32, bool, (f32, f32), f32)>>,
) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_flip_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_flip_observer(observer);
        }
    });
}

pub(crate) fn set_multiplayer_local_detach_observer(observer: Option<Rc<dyn Fn(u32)>>) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_detach_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_detach_observer(observer);
        }
    });
}

pub(crate) fn set_multiplayer_local_send_to_back_observer(observer: Option<Rc<dyn Fn(u32)>>) {
    STATE.with(|slot| {
        let mut state = slot.borrow_mut();
        state.mp_local_send_to_back_observer = observer.clone();
        if let Some(sync) = state.multiplayer.as_ref() {
            sync.borrow().set_local_send_to_back_observer(observer);
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
    if !matches!(boot_runtime::boot_state(), BootState::Ready) {
        return true;
    }
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
    // Topology-agnostic lookup: the snapshot already tracks per-piece group
    // anchors derived from the authoritative `PlayableState`. If the game
    // hasn't been initialised yet (no piece_group_anchor populated), the
    // caller treats this piece as having no owner.
    if piece_id >= snapshot.piece_group_anchor.len() {
        return None;
    }
    snapshot
        .piece_group_anchor
        .get(piece_id)
        .copied()
        .map(|anchor| anchor as usize)
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
                flip_before = snapshot.piece_flipped.get(id).copied();
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
            // A shake-to-back gesture may have fired during this move; the
            // z-order change was applied locally, so just propagate it.
            if let Some(anchor) = core.take_pending_shake_to_back() {
                handle_local_action(&CoreAction::Sync(SyncAction::SendToBack {
                    anchor_id: anchor,
                }));
            }
            let snapshot = core.snapshot();
            let Some(anchor_id) = snapshot.dragging_members.first().copied() else {
                return;
            };
            let Some((pos, rot_deg)) = piece_grid_pose(&snapshot, anchor_id) else {
                return;
            };
            if snapshot.drag_rotate_mode {
                handle_local_action(&CoreAction::Sync(SyncAction::Transform {
                    anchor_id,
                    pos,
                    rot_deg,
                }));
            } else {
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
                if let Some(after_flip) = snapshot.piece_flipped.get(primary_id).copied() {
                    if after_flip != before_flip {
                        // Carry the post-flip world pose so the server and
                        // peers reproduce the click-pivot adjustment rather
                        // than recomputing the pre-flip pose.
                        let (pos, rot_deg) =
                            piece_grid_pose(&snapshot, primary_id).unwrap_or(((0.0, 0.0), 0.0));
                        handle_local_action(&CoreAction::Sync(SyncAction::Flip {
                            piece_id: primary_id,
                            flipped: after_flip,
                            pos,
                            rot_deg,
                        }));
                        return;
                    }
                }
            }
            let Some((pos, rot_deg)) = piece_grid_pose(&snapshot, anchor_id) else {
                return;
            };
            handle_local_action(&CoreAction::Sync(SyncAction::Place {
                anchor_id,
                pos,
                rot_deg,
            }));
        }
        _ => {}
    }
}

/// Reads a piece's legacy pixel-coord pose `((x_px, y_px), rot_deg)` from the
/// new `piece_world_poses` accessor. Used by sync paths that still feed the
/// legacy `SyncAction` wire format (px coords).
fn piece_grid_pose(
    snapshot: &crate::app_core::AppSnapshot,
    piece_id: usize,
) -> Option<((f32, f32), f32)> {
    let pose = snapshot.piece_world_poses.get(piece_id).copied()?;
    // Convert mm → px using the topology's pose unit scale (which matches
    // `piece_width`/`piece_height` for grid puzzles but not for triangular
    // or irregular topologies). The "-0.5 * pose_unit" centers the legacy
    // wire format's top-left convention on the canonical piece position.
    let unit_x = snapshot.pose_unit_px[0];
    let unit_y = snapshot.pose_unit_px[1];
    let origin_x = snapshot.pose_origin_px[0];
    let origin_y = snapshot.pose_origin_px[1];
    let px = origin_x + pose.x_mm() * unit_x - unit_x * 0.5;
    let py = origin_y + pose.y_mm() * unit_y - unit_y * 0.5;
    Some(((px, py), pose.rotation_degrees()))
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
pub(crate) fn install_test_handler(hooks: SyncHooks) -> Rc<dyn Fn(heddobureika_core::ServerMsg)> {
    STATE.with(|slot| {
        if let Some(sync) = slot.borrow().multiplayer.as_ref() {
            sync.borrow_mut().install_handler(hooks)
        } else {
            Rc::new(|_msg: heddobureika_core::ServerMsg| {})
        }
    })
}
