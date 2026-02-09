use std::cell::RefCell;

use wasm_bindgen_futures::spawn_local;

use crate::idb;
use crate::persisted::{
    BootRecord, PrivateImageEntry, PrivateImageRefs, SettingsBlob, BOOT_RECORD_KEY,
    BOOT_RECORD_VERSION, SETTINGS_KEY, SETTINGS_VERSION, SNAPSHOT_KEY,
};
use heddobureika_core::{decode, encode, GameSnapshot, GAME_SNAPSHOT_VERSION};

thread_local! {
    static BOOT_RECORD_CACHE: RefCell<Option<BootRecord>> = RefCell::new(None);
    static SETTINGS_CACHE: RefCell<Option<SettingsBlob>> = RefCell::new(None);
    static SNAPSHOT_CACHE: RefCell<Option<GameSnapshot>> = RefCell::new(None);
}

pub(crate) async fn bootstrap() -> Result<(), String> {
    let db = idb::open_db().await.map_err(idb::js_err)?;
    let boot_record = load_boot_record(&db).await.unwrap_or_default();
    let settings = load_settings_blob(&db).await.unwrap_or_default();
    let snapshot = load_snapshot(&db).await;
    BOOT_RECORD_CACHE.with(|slot| {
        *slot.borrow_mut() = Some(boot_record);
    });
    SETTINGS_CACHE.with(|slot| {
        *slot.borrow_mut() = Some(settings);
    });
    SNAPSHOT_CACHE.with(|slot| {
        *slot.borrow_mut() = snapshot;
    });
    Ok(())
}

pub(crate) fn boot_record() -> BootRecord {
    BOOT_RECORD_CACHE
        .with(|slot| slot.borrow().clone())
        .unwrap_or_default()
}

pub(crate) fn settings_blob() -> SettingsBlob {
    SETTINGS_CACHE
        .with(|slot| slot.borrow().clone())
        .unwrap_or_default()
}

pub(crate) fn snapshot() -> Option<GameSnapshot> {
    SNAPSHOT_CACHE.with(|slot| slot.borrow().clone())
}

pub(crate) fn update_boot_record<F>(update: F)
where
    F: FnOnce(&mut BootRecord),
{
    let record = BOOT_RECORD_CACHE.with(|slot| {
        let mut record = slot.borrow().clone().unwrap_or_default();
        update(&mut record);
        *slot.borrow_mut() = Some(record.clone());
        record
    });
    spawn_local(async move {
        let _ = save_boot_record(record).await;
    });
}

pub(crate) fn update_settings_blob<F>(update: F)
where
    F: FnOnce(&mut SettingsBlob),
{
    let settings = SETTINGS_CACHE.with(|slot| {
        let mut settings = slot.borrow().clone().unwrap_or_default();
        update(&mut settings);
        *slot.borrow_mut() = Some(settings.clone());
        settings
    });
    spawn_local(async move {
        let _ = save_settings_blob(settings).await;
    });
}

pub(crate) fn set_snapshot(next: Option<GameSnapshot>) {
    SNAPSHOT_CACHE.with(|slot| {
        *slot.borrow_mut() = next.clone();
    });
    spawn_local(async move {
        let _ = save_snapshot(next).await;
    });
}

pub(crate) async fn load_private_image(hash: &str) -> Result<Option<PrivateImageEntry>, String> {
    let db = idb::open_db().await.map_err(idb::js_err)?;
    let bytes = idb::idb_get_bytes(&db, idb::IDB_STORE_PRIVATE_IMAGES, hash)
        .await
        .map_err(idb::js_err)?;
    let Some(bytes) = bytes else {
        return Ok(None);
    };
    Ok(decode::<PrivateImageEntry>(&bytes))
}

pub(crate) async fn save_private_image(
    hash: &str,
    entry: PrivateImageEntry,
) -> Result<(), String> {
    let Some(bytes) = encode(&entry) else {
        return Ok(());
    };
    let db = idb::open_db().await.map_err(idb::js_err)?;
    idb::idb_put_bytes(&db, idb::IDB_STORE_PRIVATE_IMAGES, hash, &bytes)
        .await
        .map_err(idb::js_err)?;
    Ok(())
}

pub(crate) async fn load_private_image_refs(
    scope_key: &str,
) -> Result<Option<PrivateImageRefs>, String> {
    let db = idb::open_db().await.map_err(idb::js_err)?;
    let bytes = idb::idb_get_bytes(&db, idb::IDB_STORE_PRIVATE_IMAGE_REFS, scope_key)
        .await
        .map_err(idb::js_err)?;
    let Some(bytes) = bytes else {
        return Ok(None);
    };
    Ok(decode::<PrivateImageRefs>(&bytes))
}

pub(crate) async fn save_private_image_refs(
    scope_key: &str,
    refs: PrivateImageRefs,
) -> Result<(), String> {
    let Some(bytes) = encode(&refs) else {
        return Ok(());
    };
    let db = idb::open_db().await.map_err(idb::js_err)?;
    idb::idb_put_bytes(&db, idb::IDB_STORE_PRIVATE_IMAGE_REFS, scope_key, &bytes)
        .await
        .map_err(idb::js_err)?;
    Ok(())
}

async fn load_boot_record(db: &web_sys::IdbDatabase) -> Option<BootRecord> {
    let bytes = idb::idb_get_bytes(db, idb::IDB_STORE_BOOT, BOOT_RECORD_KEY)
        .await
        .ok()
        .flatten()?;
    let record = decode::<BootRecord>(&bytes)?;
    if record.version != BOOT_RECORD_VERSION {
        return None;
    }
    Some(record)
}

async fn load_settings_blob(db: &web_sys::IdbDatabase) -> Option<SettingsBlob> {
    let bytes = idb::idb_get_bytes(db, idb::IDB_STORE_SETTINGS, SETTINGS_KEY)
        .await
        .ok()
        .flatten()?;
    let settings = decode::<SettingsBlob>(&bytes)?;
    if settings.version != SETTINGS_VERSION {
        return None;
    }
    Some(settings)
}

async fn load_snapshot(db: &web_sys::IdbDatabase) -> Option<GameSnapshot> {
    let bytes = idb::idb_get_bytes(db, idb::IDB_STORE_SNAPSHOT, SNAPSHOT_KEY)
        .await
        .ok()
        .flatten()?;
    let snapshot = decode::<GameSnapshot>(&bytes)?;
    if snapshot.version != GAME_SNAPSHOT_VERSION {
        return None;
    }
    Some(snapshot)
}

async fn save_boot_record(record: BootRecord) -> Result<(), String> {
    let Some(bytes) = encode(&record) else {
        return Ok(());
    };
    let db = idb::open_db().await.map_err(idb::js_err)?;
    idb::idb_put_bytes(&db, idb::IDB_STORE_BOOT, BOOT_RECORD_KEY, &bytes)
        .await
        .map_err(idb::js_err)?;
    Ok(())
}

async fn save_settings_blob(settings: SettingsBlob) -> Result<(), String> {
    let Some(bytes) = encode(&settings) else {
        return Ok(());
    };
    let db = idb::open_db().await.map_err(idb::js_err)?;
    idb::idb_put_bytes(&db, idb::IDB_STORE_SETTINGS, SETTINGS_KEY, &bytes)
        .await
        .map_err(idb::js_err)?;
    Ok(())
}

async fn save_snapshot(snapshot: Option<GameSnapshot>) -> Result<(), String> {
    let db = idb::open_db().await.map_err(idb::js_err)?;
    match snapshot {
        Some(snapshot) => {
            let Some(bytes) = encode(&snapshot) else {
                return Ok(());
            };
            idb::idb_put_bytes(&db, idb::IDB_STORE_SNAPSHOT, SNAPSHOT_KEY, &bytes)
                .await
                .map_err(idb::js_err)?;
        }
        None => {
            idb::idb_delete_key(&db, idb::IDB_STORE_SNAPSHOT, SNAPSHOT_KEY)
                .await
                .map_err(idb::js_err)?;
        }
    }
    Ok(())
}
