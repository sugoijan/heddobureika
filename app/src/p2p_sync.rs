use crate::{
    app_router::{MultiplayerConfig, MultiplayerConnection},
    runtime::P2pView,
};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use heddobureika_core::{ClientMsg, PlayableGameSnapshot, ServerMsg};
use heddobureika_p2p::{Event, Handle, Identity};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

#[wasm_bindgen::prelude::wasm_bindgen(inline_js = "
export function acquireP2pHostLock() {
  if (!navigator.locks) return Promise.reject(new Error('This browser cannot safely resume P2P hosting'));
  return new Promise((resolve, reject) => {
    navigator.locks.request('hb.p2p.host', {ifAvailable: true}, async lock => {
      if (!lock) throw new Error('Another tab is already hosting P2P');
      let release;
      const held = new Promise(done => { release = done; });
      resolve(release);
      await held;
    }).catch(reject);
  });
}

export function requestP2pGuestLock(identity, onReplaced) {
  if (!navigator.locks || typeof BroadcastChannel === 'undefined') {
    throw new Error('This browser cannot safely coordinate P2P guest tabs');
  }
  const name = 'hb.p2p.guest.' + identity;
  const channel = new BroadcastChannel(name);
  const controller = new AbortController();
  let acquired = false, active = false, timedOut = false;
  let timeout, retry;
  const cleanup = () => {
    clearTimeout(timeout);
    clearInterval(retry);
    channel.onmessage = null;
    channel.close();
  };
  const ready = new Promise((resolve, reject) => {
    channel.onmessage = event => {
      if (active && event.data === 'takeover') {
        active = false;
        onReplaced();
      }
    };
    timeout = setTimeout(() => {
      timedOut = true;
      controller.abort();
    }, 10000);
    // Repeat while queued so simultaneous duplicates cannot miss a handoff
    // request made before the preceding tab acquired its lock.
    retry = setInterval(() => channel.postMessage('takeover'), 100);
    navigator.locks.request(name, {signal: controller.signal}, async () => {
      acquired = true;
      active = true;
      clearTimeout(timeout);
      clearInterval(retry);
      let release;
      const held = new Promise(done => { release = done; });
      resolve(() => {
        active = false;
        cleanup();
        release();
      });
      await held;
    }).catch(error => {
      cleanup();
      reject(timedOut
        ? new Error('Another tab is still closing this P2P session; try again')
        : error);
    });
    channel.postMessage('takeover');
  });
  return {
    ready,
    cancel() {
      if (!acquired) {
        cleanup();
        controller.abort();
      }
    },
  };
}
")]
extern "C" {
    #[wasm_bindgen::prelude::wasm_bindgen(catch, js_name = acquireP2pHostLock)]
    async fn acquire_host_lock() -> Result<wasm_bindgen::JsValue, wasm_bindgen::JsValue>;

    type GuestLockRequest;
    #[wasm_bindgen::prelude::wasm_bindgen(catch, js_name = requestP2pGuestLock)]
    fn request_guest_lock(
        identity: &str,
        on_replaced: &js_sys::Function,
    ) -> Result<GuestLockRequest, wasm_bindgen::JsValue>;
    #[wasm_bindgen::prelude::wasm_bindgen(method, getter)]
    fn ready(this: &GuestLockRequest) -> js_sys::Promise;
    #[wasm_bindgen::prelude::wasm_bindgen(method)]
    fn cancel(this: &GuestLockRequest);
}

struct BrowserLock(js_sys::Function);
impl Drop for BrowserLock {
    fn drop(&mut self) {
        let _ = self.0.call0(&wasm_bindgen::JsValue::NULL);
    }
}

pub(crate) struct P2pSync {
    handle: Rc<RefCell<Option<Handle>>>,
    stopped: Rc<Cell<bool>>,
    connected: Rc<Cell<bool>>,
    pub view: Rc<RefCell<P2pView>>,
    saver: Option<Rc<crate::p2p_progress::Saver>>,
    guest_lock_request: Rc<RefCell<Option<GuestLockRequest>>>,
}
impl P2pSync {
    pub fn connect(
        config: &MultiplayerConfig,
        snapshot: Option<PlayableGameSnapshot>,
        handler: Rc<dyn Fn(ServerMsg)>,
        connected: Rc<Cell<bool>>,
    ) -> Self {
        let hosting = matches!(config.connection, MultiplayerConnection::P2pHost);
        let mut this = Self {
            handle: Rc::new(RefCell::new(None)),
            stopped: Rc::new(Cell::new(false)),
            connected: connected.clone(),
            view: Rc::new(RefCell::new(P2pView {
                hosting,
                invite: None,
                status: "Starting P2P".into(),
                save_status: hosting.then(|| "Waiting to save progress on this device".into()),
            })),
            saver: None,
            guest_lock_request: Rc::new(RefCell::new(None)),
        };
        let handle_slot = this.handle.clone();
        let stopped = this.stopped.clone();
        let view = this.view.clone();
        let saver = hosting.then(|| Rc::new(crate::p2p_progress::Saver::new(view.clone())));
        this.saver = saver.clone();
        let config = config.clone();
        let guest_lock_request = this.guest_lock_request.clone();
        wasm_bindgen_futures::spawn_local(async move {
            let start = async {
                use wasm_bindgen::JsCast;
                let guest_secret = (!hosting).then(guest_identity);
                let on_replaced = {
                    let stopped = stopped.clone();
                    let connected = connected.clone();
                    let view = view.clone();
                    let handle_slot = handle_slot.clone();
                    let handler = handler.clone();
                    wasm_bindgen::closure::Closure::<dyn FnMut()>::new(move || {
                        if stopped.replace(true) {
                            return;
                        }
                        connected.set(false);
                        let message =
                            "P2P session opened in another tab; this connection has stopped";
                        view.borrow_mut().status = message.into();
                        if let Some(handle) = handle_slot.borrow_mut().take() {
                            handle.close();
                        }
                        handler(ServerMsg::Error {
                            code: "p2p_paused".into(),
                            message: message.into(),
                        });
                        crate::sync_runtime::notify_sync_view_changed();
                    })
                };
                let _lock = if hosting {
                    let release = acquire_host_lock().await.map_err(crate::idb::js_err)?;
                    BrowserLock(release.unchecked_into())
                } else {
                    let request = request_guest_lock(
                        &guest_secret.as_ref().unwrap().public().to_string(),
                        on_replaced.as_ref().unchecked_ref(),
                    )
                    .map_err(crate::idb::js_err)?;
                    let ready = request.ready();
                    *guest_lock_request.borrow_mut() = Some(request);
                    let release = wasm_bindgen_futures::JsFuture::from(ready).await;
                    guest_lock_request.borrow_mut().take();
                    BrowserLock(release.map_err(crate::idb::js_err)?.unchecked_into())
                };
                if stopped.get() {
                    return Err("P2P cancelled".into());
                }
                let resumed = if hosting && config.resumed {
                    Some(
                        crate::p2p_progress::resume_room(&config.room_id)
                            .ok_or("Saved P2P room is unavailable")?,
                    )
                } else {
                    None
                };
                let secret = if hosting {
                    resumed.as_ref().map(|(identity, _)| identity.key())
                } else {
                    guest_secret
                };
                let endpoint = heddobureika_p2p::bind(secret).await?;
                if stopped.get() {
                    endpoint.close().await;
                    return Err("P2P cancelled".to_string());
                }
                let (handle, events) = if hosting {
                    // The first invite includes a usable relay hint. It stays
                    // byte-for-byte stable on resume; discovery updates routing.
                    heddobureika_p2p::wait_online(&endpoint).await?;
                    if stopped.get() {
                        endpoint.close().await;
                        return Err("P2P cancelled".into());
                    }
                    let (mut host, handle, events) = if let Some((identity, snapshot)) = resumed {
                        heddobureika_p2p::Host::resume(endpoint, identity, snapshot)?
                    } else {
                        let snapshot = snapshot.ok_or("Load a puzzle before hosting P2P")?;
                        heddobureika_p2p::Host::new(endpoint, config.room_id.clone(), snapshot)?
                    };
                    let saver = saver.as_ref().unwrap();
                    saver.set_identity(host.identity());
                    let initial = host.snapshot();
                    saver.flush(initial).await?;
                    if stopped.get() {
                        handle.close();
                        return Err("P2P cancelled".into());
                    }
                    crate::p2p_progress::activate(&config.room_id)?;
                    host.require_persistence();
                    wasm_bindgen_futures::spawn_local(host.run());
                    (handle, events)
                } else {
                    let MultiplayerConnection::P2pGuest { invite } = config.connection else {
                        unreachable!()
                    };
                    let invite = heddobureika_p2p::Invite::parse(&invite)?;
                    let (guest, handle, events) = heddobureika_p2p::Guest::new(endpoint, invite);
                    wasm_bindgen_futures::spawn_local(guest.run());
                    (handle, events)
                };
                *handle_slot.borrow_mut() = Some(handle);
                let mut failure = None;
                while let Ok(event) = events.recv().await {
                    if let Event::Checkpoint(snapshot) = event {
                        if let Some(saver) = &saver {
                            saver.submit(snapshot);
                        }
                        continue;
                    }
                    if let Event::PersistRoom {
                        identity,
                        snapshot,
                        committed,
                    } = event
                    {
                        let result = if let Some(saver) = &saver {
                            saver.set_identity(identity);
                            saver.flush(snapshot).await
                        } else {
                            Err("P2P host storage unavailable".into())
                        };
                        let _ = committed.send(result).await;
                        continue;
                    }
                    if stopped.get() || failure.is_some() {
                        // Drain through the driver's final checkpoint even
                        // after leaving; the storage queue outlives this UI.
                        continue;
                    }
                    match event {
                        Event::Checkpoint(_) => unreachable!(),
                        Event::PersistRoom { .. } => unreachable!(),
                        Event::Invite(invite) => view.borrow_mut().invite = Some(invite),
                        Event::Status {
                            message: status,
                            connected: ready,
                        } => {
                            if !ready {
                                connected.set(false);
                                handler(ServerMsg::Error {
                                    code: "p2p_paused".into(),
                                    message: status.clone(),
                                });
                            }
                            view.borrow_mut().status = status;
                        }
                        Event::Messages(messages) => {
                            for message in messages {
                                handler(message);
                            }
                        }
                        Event::Reset(messages) => {
                            handler(ServerMsg::Error {
                                code: "p2p_reset".into(),
                                message: String::new(),
                            });
                            for message in messages {
                                handler(message);
                            }
                        }
                        Event::Rejected(message) => {
                            connected.set(false);
                            handler(ServerMsg::Error {
                                code: "p2p_paused".into(),
                                message: message.clone(),
                            });
                            view.borrow_mut().status = message.clone();
                            // Keep the identity lock through driver shutdown,
                            // including endpoint.close(), before another tab binds.
                            failure = Some(message);
                        }
                    }
                    crate::sync_runtime::notify_sync_view_changed();
                }
                Err::<(), String>(failure.unwrap_or_else(|| "P2P session ended".into()))
            }
            .await;
            if !stopped.get() {
                if let Err(message) = start {
                    connected.set(false);
                    view.borrow_mut().status = message;
                    crate::sync_runtime::notify_sync_view_changed();
                }
            }
        });
        this
    }
    pub(crate) fn reload_handle(
        &self,
    ) -> Result<Option<(Handle, Rc<crate::p2p_progress::Saver>)>, String> {
        let Some(saver) = &self.saver else {
            return Ok(None);
        };
        let handle = self
            .handle
            .borrow()
            .clone()
            .ok_or("P2P room is still starting; try changing the renderer once it is ready")?;
        Ok(Some((handle, saver.clone())))
    }
    pub fn send(&self, command: ClientMsg) {
        if matches!(command, ClientMsg::Ping { .. }) {
            return;
        }
        if let Some(handle) = self.handle.borrow().as_ref() {
            if let Err(message) = handle.send(command) {
                self.view.borrow_mut().status = message;
                self.connected.set(false);
                handle.close();
                wasm_bindgen_futures::spawn_local(async {
                    crate::sync_runtime::notify_sync_view_changed();
                });
            }
        }
    }
    pub fn close(&self) {
        self.stopped.set(true);
        if let Some(request) = self.guest_lock_request.borrow_mut().take() {
            request.cancel();
        }
        if let Some(handle) = self.handle.borrow_mut().take() {
            handle.close();
        }
    }
}
impl Drop for P2pSync {
    fn drop(&mut self) {
        self.close();
    }
}

fn guest_identity() -> Identity {
    // Tab-scoped identity lets a reload reconnect without giving independent
    // browser tabs the same player identity.
    let storage = web_sys::window().and_then(|w| w.session_storage().ok().flatten());
    if let Some(key) = storage
        .as_ref()
        .and_then(|s| s.get_item("hb.p2p.guest-key.v1").ok().flatten())
    {
        if let Ok(bytes) = URL_SAFE_NO_PAD.decode(key) {
            if let Ok(bytes) = <[u8; 32]>::try_from(bytes) {
                return Identity::from_bytes(&bytes);
            }
        }
    }
    let identity = Identity::generate();
    if let Some(storage) = storage {
        let _ = storage.set_item(
            "hb.p2p.guest-key.v1",
            &URL_SAFE_NO_PAD.encode(identity.to_bytes()),
        );
    }
    identity
}

#[cfg(all(test, target_arch = "wasm32"))]
mod tests {
    use super::*;
    use crate::{
        app_core::AppCore, app_router, boot_runtime, local_snapshot, multiplayer_bridge,
        sync_runtime,
    };
    use gloo::timers::future::TimeoutFuture;
    use heddobureika_core::{PieceId, PuzzleImageRef, TopologySpec};
    use wasm_bindgen_test::*;
    wasm_bindgen_test_configure!(run_in_browser);

    async fn wait_until(message: &str, condition: impl Fn() -> bool) {
        let started = js_sys::Date::now();
        while !condition() {
            assert!(js_sys::Date::now() - started < 30_000.0, "{message}");
            TimeoutFuture::new(10).await;
        }
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_guest_lock_cancel_releases_pending_handoff() {
        use wasm_bindgen::{JsCast, closure::Closure};
        use wasm_bindgen_futures::JsFuture;

        let name = Identity::generate().public().to_string();
        let replaced = Rc::new(Cell::new(false));
        let flag = replaced.clone();
        let callback = Closure::<dyn FnMut()>::new(move || flag.set(true));
        let original = request_guest_lock(&name, callback.as_ref().unchecked_ref()).unwrap();
        let held = BrowserLock(
            JsFuture::from(original.ready())
                .await
                .unwrap()
                .unchecked_into(),
        );
        let no_op = Closure::<dyn FnMut()>::new(|| {});
        // Another identity must not stop this guest or wait for its lock.
        let unrelated = request_guest_lock(
            &Identity::generate().public().to_string(),
            no_op.as_ref().unchecked_ref(),
        )
        .unwrap();
        let unrelated = BrowserLock(
            JsFuture::from(unrelated.ready())
                .await
                .unwrap()
                .unchecked_into(),
        );
        assert!(!replaced.get());
        drop(unrelated);

        let pending = request_guest_lock(&name, no_op.as_ref().unchecked_ref()).unwrap();
        wait_until("old guest was not told to stop", || replaced.get()).await;
        // Cancelling while the original transport is still closing must not
        // leave a queued acquisition or a timer that stops a later session.
        pending.cancel();
        assert!(JsFuture::from(pending.ready()).await.is_err());
        drop(held);
        let resumed = request_guest_lock(&name, no_op.as_ref().unchecked_ref()).unwrap();
        let resumed = BrowserLock(
            JsFuture::from(resumed.ready())
                .await
                .unwrap()
                .unchecked_into(),
        );
        drop(resumed);
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_guest_handoff_stops_old_relay_endpoint_and_allows_reload() {
        console_error_panic_hook::set_once();
        let core = AppCore::new();
        let entry = &heddobureika_core::PUZZLE_CATALOG[0];
        let dims =
            heddobureika_core::logical_image_size(entry.width, entry.height, core.image_max_dim());
        core.set_puzzle_with_topology(
            entry.label.into(),
            PuzzleImageRef::BuiltIn {
                slug: entry.slug.into(),
            },
            dims,
            TopologySpec::grid(2, 1),
            Some(8),
        );
        let snapshot = local_snapshot::build_playable_snapshot_from_app(&core.snapshot()).unwrap();
        let (host, host_handle, events) = heddobureika_p2p::Host::new(
            heddobureika_p2p::bind(None).await.unwrap(),
            "P2Phandoff".into(),
            snapshot,
        )
        .unwrap();
        wasm_bindgen_futures::spawn_local(host.run());
        let invite = loop {
            if let Event::Invite(invite) = events.recv().await.unwrap() {
                break invite;
            }
        };
        wasm_bindgen_futures::spawn_local(async move { while events.recv().await.is_ok() {} });
        let config = MultiplayerConfig {
            room_id: "P2Phandoff".into(),
            connection: MultiplayerConnection::P2pGuest { invite },
            clear_hash: false,
            resumed: false,
        };
        let connect = || {
            let connected = Rc::new(Cell::new(false));
            let ready = connected.clone();
            let welcomes = Rc::new(Cell::new(0));
            let count = welcomes.clone();
            let state = Rc::new(RefCell::new(None));
            let latest = state.clone();
            let updates = Rc::new(Cell::new(0));
            let received = updates.clone();
            let handler = Rc::new(move |message| match message {
                ServerMsg::Welcome { .. } => {
                    ready.set(true);
                    count.set(count.get() + 1);
                }
                ServerMsg::State { snapshot, .. } => *latest.borrow_mut() = Some(snapshot),
                ServerMsg::PlayableUpdate { .. } => received.set(received.get() + 1),
                _ => {}
            });
            let sync = P2pSync::connect(&config, None, handler, connected.clone());
            (sync, connected, welcomes, state, updates)
        };
        let (first, first_connected, first_welcomes, _, first_updates) = connect();
        wait_until("first relay guest did not connect", || {
            first_connected.get()
        })
        .await;
        let old_handle = first.handle.borrow().clone().unwrap();
        first.send(ClientMsg::Select { piece_id: 0 });
        first.send(ClientMsg::Move {
            piece_id: 0,
            drop_pos: heddobureika_core::PlayablePositionSnapshot {
                x_mm: 1.0,
                y_mm: 0.75,
            },
            client_seq: 1,
            base_revision: 0,
        });
        wait_until("first guest move was not confirmed", || {
            first_updates.get() > 0
        })
        .await;

        // Both instances read the same sessionStorage key, just as duplicated
        // tabs do. The second must wait until the first endpoint has closed.
        let (second, second_connected, _, second_state, second_updates) = connect();
        wait_until("replacement relay guest did not connect", || {
            second_connected.get()
        })
        .await;
        assert!(first.stopped.get());
        assert!(!first_connected.get());
        assert!(first.handle.borrow().is_none());
        assert!(old_handle.send(ClientMsg::Select { piece_id: 0 }).is_err());
        assert!(first.view.borrow().status.contains("another tab"));
        let pose = second_state
            .borrow()
            .as_ref()
            .unwrap()
            .restore_playable_from_spec()
            .unwrap()
            .piece_world_pose(PieceId(0))
            .unwrap();
        assert!((pose.x_mm() - 1.0).abs() < 0.001);
        assert!((pose.y_mm() - 0.75).abs() < 0.001);
        second.send(ClientMsg::Select { piece_id: 0 });
        second.send(ClientMsg::Move {
            piece_id: 0,
            drop_pos: heddobureika_core::PlayablePositionSnapshot {
                x_mm: 1.25,
                y_mm: 0.9,
            },
            client_seq: 1,
            base_revision: 0,
        });
        wait_until("replacement guest could not move", || {
            second_updates.get() > 0
        })
        .await;
        assert_eq!(first_welcomes.get(), 1, "displaced guest must never rejoin");

        // Leaving and starting with the same saved key models a normal reload.
        second.close();
        let (resumed, resumed_connected, _, _, _) = connect();
        wait_until("normal guest reload could not reacquire identity", || {
            resumed_connected.get()
        })
        .await;
        assert_eq!(first_welcomes.get(), 1);
        assert!(first.view.borrow().status.contains("another tab"));
        first.close();
        resumed.close();
        host_handle.close();
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_guest_route_applies_host_updates_without_a_game_server() {
        console_error_panic_hook::set_once();
        boot_runtime::set_boot_state(boot_runtime::BootState::Ready);
        sync_runtime::init_from_config(None);
        let saved_before = crate::p2p_progress::saved_snapshot()
            .as_ref()
            .and_then(heddobureika_core::encode);
        let core = AppCore::new();
        let entry = &heddobureika_core::PUZZLE_CATALOG[0];
        let dims =
            heddobureika_core::logical_image_size(entry.width, entry.height, core.image_max_dim());
        core.set_puzzle_with_topology(
            entry.label.into(),
            PuzzleImageRef::BuiltIn {
                slug: entry.slug.into(),
            },
            dims,
            TopologySpec::grid(2, 1),
            Some(8),
        );
        let snapshot = local_snapshot::build_playable_snapshot_from_app(&core.snapshot()).unwrap();
        let endpoint = heddobureika_p2p::bind(None).await.unwrap();
        // A published invite is available only once the relay address exists.
        let (host, host_handle, host_events) =
            heddobureika_p2p::Host::new(endpoint, "P2Proute01".into(), snapshot).unwrap();
        wasm_bindgen_futures::spawn_local(host.run());
        let invite = loop {
            match host_events.recv().await.unwrap() {
                Event::Invite(invite) => break invite,
                Event::Status { message, .. } if message.contains("unavailable") => {
                    panic!("{message}")
                }
                _ => {}
            }
        };
        let window = web_sys::window().unwrap();
        let hash = format!("#p2p={invite}");
        window
            .history()
            .unwrap()
            .replace_state_with_url(&wasm_bindgen::JsValue::NULL, "", Some(&hash))
            .unwrap();
        let config = app_router::load_init_config().multiplayer.unwrap();
        assert!(matches!(
            config.connection,
            app_router::MultiplayerConnection::P2pGuest { .. }
        ));
        sync_runtime::attach_core(core.clone());
        multiplayer_bridge::install(core.clone());
        sync_runtime::init_from_config(Some(config));
        let started = js_sys::Date::now();
        loop {
            if sync_runtime::sync_view().connected()
                && sync_runtime::sync_view().client_id() == Some(heddobureika_p2p::GUEST)
            {
                break;
            }
            assert!(
                js_sys::Date::now() - started < 30_000.0,
                "P2P guest did not connect"
            );
            TimeoutFuture::new(25).await;
        }
        // A direct command exercises framing and the real app's authoritative
        // update bridge, while leaving the renderer out of this network test.
        let sync = sync_runtime::multiplayer_handle().unwrap();
        sync.borrow().send(ClientMsg::Select { piece_id: 0 });
        sync.borrow().send(ClientMsg::Move {
            piece_id: 0,
            drop_pos: heddobureika_core::PlayablePositionSnapshot {
                x_mm: 1.0,
                y_mm: 0.75,
            },
            client_seq: 1,
            base_revision: 0,
        });
        loop {
            let snapshot =
                local_snapshot::build_playable_snapshot_from_app(&core.snapshot()).unwrap();
            let pose = snapshot
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(PieceId(0))
                .unwrap();
            if (pose.x_mm() - 1.0).abs() < 0.001 && (pose.y_mm() - 0.75).abs() < 0.001 {
                break;
            }
            assert!(
                js_sys::Date::now() - started < 30_000.0,
                "confirmed P2P update was not applied"
            );
            TimeoutFuture::new(25).await;
        }
        // Releasing the host's separate group advances only ownership. The
        // guest's next held-group movement must still reach its live shadow.
        host_handle.send(ClientMsg::Select { piece_id: 1 }).unwrap();
        host_handle
            .send(ClientMsg::Release { piece_id: 1 })
            .unwrap();
        loop {
            if let Event::Messages(messages) = host_events.recv().await.unwrap() {
                if messages.iter().any(|message| {
                    matches!(
                        message,
                        ServerMsg::ControlUpdate {
                            update: heddobureika_core::RoomControlUpdate::Ownership {
                                group_anchor: 1,
                                owner: None,
                                ..
                            },
                            ..
                        }
                    )
                }) {
                    break;
                }
            }
            assert!(
                js_sys::Date::now() - started < 30_000.0,
                "host claim was not released"
            );
        }
        sync.borrow().send(ClientMsg::Move {
            piece_id: 0,
            drop_pos: heddobureika_core::PlayablePositionSnapshot {
                x_mm: 1.25,
                y_mm: 0.9,
            },
            client_seq: 2,
            base_revision: 0,
        });
        loop {
            let snapshot =
                local_snapshot::build_playable_snapshot_from_app(&core.snapshot()).unwrap();
            let pose = snapshot
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(PieceId(0))
                .unwrap();
            if (pose.x_mm() - 1.25).abs() < 0.001 && (pose.y_mm() - 0.9).abs() < 0.001 {
                break;
            }
            assert!(
                js_sys::Date::now() - started < 30_000.0,
                "ownership-only release broke subsequent movement"
            );
            TimeoutFuture::new(25).await;
        }
        assert!(window.location().hash().unwrap().starts_with("#p2p="));
        assert!(crate::persisted_store::boot_record().room_session.is_none());
        assert_eq!(
            crate::p2p_progress::saved_snapshot()
                .as_ref()
                .and_then(heddobureika_core::encode),
            saved_before,
            "playing as a guest must not replace this device's hosted puzzle save"
        );
        host_handle.close();
        sync_runtime::init_from_config(None);
        app_router::clear_location_hash();
    }
}
