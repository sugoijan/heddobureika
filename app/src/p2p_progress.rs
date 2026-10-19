//! Browser storage for confirmed progress and private resumable room identity.
use crate::{idb, runtime::P2pView};
use heddobureika_core::{PlayableGameSnapshot, decode, encode};
use std::{cell::RefCell, rc::Rc};

const KEY: &str = "p2p-host-progress.v1";
const VERSION: u32 = 1;
const ROOM_KEY: &str = "p2p-host-room.v1";
const ACTIVE_ROOM_KEY: &str = "hb.p2p.host-room.v1";

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
struct RoomRecord {
    version: u32,
    snapshot: PlayableGameSnapshot,
    identity: heddobureika_p2p::HostIdentity,
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
struct Record {
    version: u32,
    snapshot: PlayableGameSnapshot,
}

struct Pending {
    snapshot: PlayableGameSnapshot,
    view: Rc<RefCell<P2pView>>,
    identity: Option<heddobureika_p2p::HostIdentity>,
    receipts: Vec<futures_channel::oneshot::Sender<Result<(), String>>>,
}

#[derive(Default)]
struct Store {
    db: Option<web_sys::IdbDatabase>,
    saved: Option<PlayableGameSnapshot>,
    identity: Option<heddobureika_p2p::HostIdentity>,
    load_error: Option<String>,
    generation: u64,
    pending: Option<Pending>,
    writing: bool,
}

thread_local! {
    static STORE: RefCell<Store> = RefCell::new(Store::default());
}

fn decode_record(bytes: &[u8]) -> Result<PlayableGameSnapshot, String> {
    let record = decode::<Record>(bytes).ok_or("Saved P2P progress is unreadable")?;
    if record.version != VERSION {
        return Err("Saved P2P progress uses an incompatible version".into());
    }
    heddobureika_p2p::validate_snapshot(&record.snapshot)?;
    Ok(record.snapshot)
}

pub(crate) async fn load(db: &web_sys::IdbDatabase) {
    let result = async {
        if let Some(bytes) = idb::idb_get_bytes(db, idb::IDB_STORE_SNAPSHOT, ROOM_KEY)
            .await
            .map_err(idb::js_err)?
        {
            let record = decode::<RoomRecord>(&bytes).ok_or("Saved P2P room is unreadable")?;
            if record.version != VERSION {
                return Err("Saved P2P room uses an incompatible version".into());
            }
            record.identity.validate()?;
            heddobureika_p2p::validate_snapshot(&record.snapshot)?;
            return Ok((Some(record.snapshot), Some(record.identity)));
        }
        idb::idb_get_bytes(db, idb::IDB_STORE_SNAPSHOT, KEY)
            .await
            .map_err(idb::js_err)?
            .map(|bytes| decode_record(&bytes))
            .transpose()
            .map(|snapshot| (snapshot, None))
    }
    .await;
    STORE.with(|slot| {
        let mut store = slot.borrow_mut();
        store.db = Some(db.clone());
        match result {
            Ok((snapshot, identity)) => {
                store.saved = snapshot;
                store.identity = identity;
                store.load_error = None;
            }
            Err(message) => {
                store.saved = None;
                store.identity = None;
                store.load_error = Some(message);
            }
        }
    });
}

fn tab_storage() -> Option<web_sys::Storage> {
    web_sys::window()?.session_storage().ok().flatten()
}

pub(crate) fn stop_resuming() {
    if let Some(storage) = tab_storage() {
        let _ = storage.remove_item(ACTIVE_ROOM_KEY);
    }
}

pub(crate) fn activate(room_id: &str) -> Result<(), String> {
    tab_storage()
        .ok_or("Tab storage is unavailable; this room cannot resume after reload")?
        .set_item(ACTIVE_ROOM_KEY, room_id)
        .map_err(idb::js_err)
}

pub(crate) fn active_room_id() -> Option<String> {
    let room_id = tab_storage()?.get_item(ACTIVE_ROOM_KEY).ok()??;
    STORE.with(|slot| {
        slot.borrow()
            .identity
            .as_ref()
            .filter(|identity| identity.room_id() == room_id)
            .map(|_| room_id)
    })
}

pub(crate) fn resume_room(
    room_id: &str,
) -> Option<(heddobureika_p2p::HostIdentity, PlayableGameSnapshot)> {
    STORE.with(|slot| {
        let store = slot.borrow();
        let identity = store
            .identity
            .as_ref()
            .filter(|identity| identity.room_id() == room_id)?;
        Some((identity.clone(), store.saved.clone()?))
    })
}

pub(crate) fn saved_snapshot() -> Option<PlayableGameSnapshot> {
    STORE.with(|slot| slot.borrow().saved.clone())
}

pub(crate) fn has_saved_progress() -> bool {
    STORE.with(|slot| slot.borrow().saved.is_some())
}

pub(crate) fn load_error() -> Option<String> {
    STORE.with(|slot| slot.borrow().load_error.clone())
}

/// A new host supersedes older hosts' late shutdown events. All writes share
/// one queue so an older in-flight transaction cannot overwrite a newer save.
pub(crate) struct Saver {
    generation: u64,
    view: Rc<RefCell<P2pView>>,
    identity: RefCell<Option<heddobureika_p2p::HostIdentity>>,
}

impl Saver {
    pub fn new(view: Rc<RefCell<P2pView>>) -> Self {
        let generation = STORE.with(|slot| {
            let mut store = slot.borrow_mut();
            store.generation += 1;
            store.generation
        });
        Self {
            generation,
            view,
            identity: RefCell::new(None),
        }
    }

    pub fn set_identity(&self, identity: heddobureika_p2p::HostIdentity) {
        *self.identity.borrow_mut() = Some(identity);
    }

    pub async fn flush(&self, snapshot: PlayableGameSnapshot) -> Result<(), String> {
        let (tx, rx) = futures_channel::oneshot::channel();
        self.queue(snapshot, Some(tx));
        rx.await
            .map_err(|_| "P2P save was superseded or interrupted".to_string())?
    }

    pub fn submit(&self, snapshot: PlayableGameSnapshot) {
        self.queue(snapshot, None);
    }

    fn queue(
        &self,
        snapshot: PlayableGameSnapshot,
        receipt: Option<futures_channel::oneshot::Sender<Result<(), String>>>,
    ) {
        let start = STORE.with(|slot| {
            let mut store = slot.borrow_mut();
            if self.generation != store.generation {
                return None;
            }
            self.view.borrow_mut().save_status = Some("Saving progress on this device…".into());
            let mut receipts = store
                .pending
                .take()
                .map(|pending| {
                    if Rc::ptr_eq(&pending.view, &self.view) {
                        pending.receipts
                    } else {
                        for receipt in pending.receipts {
                            let _ = receipt.send(Err("P2P save was superseded".into()));
                        }
                        Vec::new()
                    }
                })
                .unwrap_or_default();
            receipts.extend(receipt);
            store.pending = Some(Pending {
                snapshot,
                view: self.view.clone(),
                identity: self.identity.borrow().clone(),
                receipts,
            });
            if store.writing {
                return Some(false);
            }
            store.writing = true;
            Some(true)
        });
        let Some(start) = start else { return };
        crate::sync_runtime::notify_sync_view_changed();
        if start {
            wasm_bindgen_futures::spawn_local(write_pending());
        }
    }
}

async fn save(
    snapshot: &PlayableGameSnapshot,
    identity: &Option<heddobureika_p2p::HostIdentity>,
) -> Result<(), String> {
    let (key, bytes) = if let Some(identity) = identity {
        (
            ROOM_KEY,
            encode(&RoomRecord {
                version: VERSION,
                snapshot: snapshot.clone(),
                identity: identity.clone(),
            }),
        )
    } else {
        (
            KEY,
            encode(&Record {
                version: VERSION,
                snapshot: snapshot.clone(),
            }),
        )
    };
    let bytes = bytes.ok_or("Could not encode P2P progress")?;
    // Reuse the boot connection rather than opening a database on every drag
    // checkpoint. Drop it on write errors so a later checkpoint can reconnect.
    let db = STORE.with(|slot| slot.borrow().db.clone());
    let db = match db {
        Some(db) => db,
        None => {
            let db = idb::open_db().await.map_err(idb::js_err)?;
            STORE.with(|slot| slot.borrow_mut().db = Some(db.clone()));
            db
        }
    };
    let result = idb::idb_put_bytes_committed(&db, idb::IDB_STORE_SNAPSHOT, key, &bytes)
        .await
        .map_err(idb::js_err);
    if result.is_err() {
        STORE.with(|slot| {
            slot.borrow_mut().db.take();
        });
        db.close();
    }
    result
}

async fn write_pending() {
    loop {
        let pending = STORE.with(|slot| {
            let mut store = slot.borrow_mut();
            let pending = store.pending.take();
            if pending.is_none() {
                store.writing = false;
            }
            pending
        });
        let Some(pending) = pending else { break };
        let result = save(&pending.snapshot, &pending.identity).await;
        for receipt in pending.receipts {
            let _ = receipt.send(result.clone());
        }
        match result {
            Ok(()) => {
                let newer_pending = STORE.with(|slot| {
                    let mut store = slot.borrow_mut();
                    store.saved = Some(pending.snapshot);
                    store.identity = pending.identity;
                    store.load_error = None;
                    store
                        .pending
                        .as_ref()
                        .is_some_and(|next| Rc::ptr_eq(&next.view, &pending.view))
                });
                if !newer_pending {
                    pending.view.borrow_mut().save_status =
                        Some("Progress saved on this device".into());
                }
            }
            Err(message) => {
                pending.view.borrow_mut().save_status =
                    Some(format!("Progress could not be saved: {message}"));
            }
        }
        crate::sync_runtime::notify_sync_view_changed();
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod tests {
    use super::*;
    use crate::{app_core::AppCore, local_snapshot::build_playable_snapshot_from_app};
    use gloo::timers::future::TimeoutFuture;
    use heddobureika_core::{PuzzleImageRef, TopologySpec};
    use wasm_bindgen_test::*;
    wasm_bindgen_test_configure!(run_in_browser);

    fn snapshot() -> PlayableGameSnapshot {
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
            Some(42),
        );
        build_playable_snapshot_from_app(&core.snapshot()).unwrap()
    }

    fn view() -> Rc<RefCell<P2pView>> {
        Rc::new(RefCell::new(P2pView {
            hosting: true,
            invite: None,
            status: String::new(),
            save_status: None,
        }))
    }

    async fn wait_for_writes() {
        let started = js_sys::Date::now();
        while STORE.with(|slot| slot.borrow().writing) {
            assert!(js_sys::Date::now() - started < 10_000.0, "save timed out");
            TimeoutFuture::new(10).await;
        }
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_room_identity_commits_with_progress_and_resumes_the_same_tab() {
        console_error_panic_hook::set_once();
        crate::app_router::clear_location_hash();
        let endpoint = heddobureika_p2p::bind(None).await.unwrap();
        let initial = snapshot();
        let (host, handle, _events) =
            heddobureika_p2p::Host::new(endpoint, "P2Pstore01".into(), initial.clone()).unwrap();
        let identity = host.identity();
        let invite = identity.invite().to_owned();
        let key = identity.key().public();
        let saver = Saver::new(view());
        saver.set_identity(identity);
        saver.flush(initial.clone()).await.unwrap();
        activate("P2Pstore01").unwrap();
        let db = idb::open_db().await.unwrap();
        STORE.with(|slot| {
            let mut store = slot.borrow_mut();
            store.saved = None;
            store.identity = None;
        });
        load(&db).await;
        assert_eq!(active_room_id().as_deref(), Some("P2Pstore01"));
        let (identity, progress) = resume_room("P2Pstore01").unwrap();
        assert_eq!(identity.invite(), invite);
        assert_eq!(identity.key().public(), key);
        assert_eq!(encode(&progress), encode(&initial));
        let config = crate::app_router::load_init_config().multiplayer.unwrap();
        assert!(config.resumed);
        assert!(matches!(
            config.connection,
            crate::app_router::MultiplayerConnection::P2pHost
        ));
        crate::sync_runtime::init_from_config(None);
        assert!(
            active_room_id().is_none(),
            "explicitly leaving must stop automatic hosting"
        );
        assert!(
            resume_room("P2Pstore01").is_some(),
            "leaving keeps saved progress"
        );
        // Unsupported room schemas cannot silently start a new identity using
        // an existing room marker or fall back to unrelated local progress.
        let record = RoomRecord {
            version: VERSION + 1,
            snapshot: initial,
            identity,
        };
        idb::idb_put_bytes_committed(
            &db,
            idb::IDB_STORE_SNAPSHOT,
            ROOM_KEY,
            &encode(&record).unwrap(),
        )
        .await
        .unwrap();
        activate("P2Pstore01").unwrap();
        load(&db).await;
        assert!(active_room_id().is_none());
        assert!(load_error().is_some());
        stop_resuming();
        handle.close();
        db.close();
        STORE.with(|slot| slot.borrow_mut().db = None);
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_progress_commits_serialized_writes_and_survives_cache_reload() {
        console_error_panic_hook::set_once();
        let db = idb::open_db().await.unwrap();
        let local_before =
            idb::idb_get_bytes(&db, idb::IDB_STORE_SNAPSHOT, crate::persisted::SNAPSHOT_KEY)
                .await
                .unwrap();
        let boot_before =
            idb::idb_get_bytes(&db, idb::IDB_STORE_BOOT, crate::persisted::BOOT_RECORD_KEY)
                .await
                .unwrap();
        let old = Saver::new(view());
        let mut first = snapshot();
        first.scramble_nonce = 10;
        old.submit(first.clone());
        // This queued write must never land after the new host's checkpoint.
        first.scramble_nonce = 11;
        old.submit(first.clone());
        let current_view = view();
        let current = Saver::new(current_view.clone());
        let mut latest = first.clone();
        latest.scramble_nonce = 12;
        current.submit(latest.clone());
        old.submit(first);
        wait_for_writes().await;
        assert_eq!(
            current_view.borrow().save_status.as_deref(),
            Some("Progress saved on this device")
        );
        STORE.with(|slot| slot.borrow_mut().saved = None);
        load(&db).await;
        assert_eq!(encode(&saved_snapshot().unwrap()), encode(&latest));
        assert_eq!(
            idb::idb_get_bytes(&db, idb::IDB_STORE_SNAPSHOT, crate::persisted::SNAPSHOT_KEY)
                .await
                .unwrap(),
            local_before
        );
        assert_eq!(
            idb::idb_get_bytes(&db, idb::IDB_STORE_BOOT, crate::persisted::BOOT_RECORD_KEY)
                .await
                .unwrap(),
            boot_before
        );
        db.close();
        STORE.with(|slot| slot.borrow_mut().db = None);
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_progress_save_failure_preserves_previous_checkpoint_and_recovers() {
        console_error_panic_hook::set_once();
        let view = view();
        let saver = Saver::new(view.clone());
        let baseline = snapshot();
        saver.submit(baseline.clone());
        wait_for_writes().await;
        STORE.with(|slot| {
            if let Some(db) = slot.borrow_mut().db.take() {
                db.close();
            }
        });
        let factory = web_sys::window().unwrap().indexed_db().unwrap().unwrap();
        let key = wasm_bindgen::JsValue::from_str("open");
        let original = js_sys::Reflect::get(&factory, &key).unwrap();
        let unavailable =
            js_sys::Function::new_no_args("throw new Error('storage unavailable for test')");
        js_sys::Reflect::set(&factory, &key, &unavailable).unwrap();
        let mut changed = baseline.clone();
        changed.scramble_nonce += 1;
        saver.submit(changed.clone());
        wait_for_writes().await;
        js_sys::Reflect::set(&factory, &key, &original).unwrap();
        assert!(
            view.borrow()
                .save_status
                .as_deref()
                .unwrap()
                .starts_with("Progress could not be saved:")
        );
        let db = idb::open_db().await.unwrap();
        load(&db).await;
        assert_eq!(encode(&saved_snapshot().unwrap()), encode(&baseline));
        saver.submit(changed.clone());
        wait_for_writes().await;
        load(&db).await;
        assert_eq!(encode(&saved_snapshot().unwrap()), encode(&changed));
        assert_eq!(
            view.borrow().save_status.as_deref(),
            Some("Progress saved on this device")
        );
        db.close();
        STORE.with(|slot| slot.borrow_mut().db = None);
    }

    #[wasm_bindgen_test(async)]
    async fn p2p_progress_rejects_corrupt_and_incompatible_saves() {
        console_error_panic_hook::set_once();
        let mut record = Record {
            version: VERSION,
            snapshot: snapshot(),
        };
        assert!(decode_record(&[1, 2, 3]).is_err());
        record.version += 1;
        assert!(decode_record(&encode(&record).unwrap()).is_err());
        record.version = VERSION;
        record.snapshot.version += 1;
        assert!(decode_record(&encode(&record).unwrap()).is_err());
        record.snapshot = snapshot();
        record.snapshot.puzzle.image_ref = PuzzleImageRef::BuiltIn {
            slug: "missing-catalog-entry".into(),
        };
        assert!(decode_record(&encode(&record).unwrap()).is_err());
        record.snapshot = snapshot();
        record.snapshot.puzzle.topology.tag = "unknown-topology".into();
        assert!(decode_record(&encode(&record).unwrap()).is_err());
        let db = idb::open_db().await.unwrap();
        idb::idb_put_bytes_committed(&db, idb::IDB_STORE_SNAPSHOT, KEY, &[1, 2, 3])
            .await
            .unwrap();
        load(&db).await;
        assert!(saved_snapshot().is_none());
        assert!(load_error().is_some());
        // Reading an unsupported record never deletes it or normal local saves.
        assert_eq!(
            idb::idb_get_bytes(&db, idb::IDB_STORE_SNAPSHOT, KEY)
                .await
                .unwrap(),
            Some(vec![1, 2, 3])
        );
        db.close();
        STORE.with(|slot| slot.borrow_mut().db = None);
    }
}
