use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Event, IdbDatabase, IdbFactory, IdbOpenDbRequest, IdbRequest, IdbTransactionMode};

pub(crate) const IDB_NAME: &str = "heddobureika";
pub(crate) const IDB_VERSION: u32 = 2;

pub(crate) const IDB_STORE_BOOT: &str = "boot";
pub(crate) const IDB_STORE_SETTINGS: &str = "settings";
pub(crate) const IDB_STORE_SNAPSHOT: &str = "snapshot";
pub(crate) const IDB_STORE_IDENTITY: &str = "identity";

const ALL_STORES: &[&str] = &[
    IDB_STORE_BOOT,
    IDB_STORE_SETTINGS,
    IDB_STORE_SNAPSHOT,
    IDB_STORE_IDENTITY,
];

pub(crate) async fn open_db() -> Result<IdbDatabase, JsValue> {
    let factory = idb_factory()?;
    let request = factory.open_with_u32(IDB_NAME, IDB_VERSION)?;
    attach_upgrade_handler(&request);
    let db_value = wasm_bindgen_futures::JsFuture::from(request_to_promise(request.into())).await?;
    db_value.dyn_into::<IdbDatabase>()
}

pub(crate) async fn idb_get_bytes(
    db: &IdbDatabase,
    store: &str,
    key: &str,
) -> Result<Option<Vec<u8>>, JsValue> {
    let tx = db.transaction_with_str_and_mode(store, IdbTransactionMode::Readonly)?;
    let store = tx.object_store(store)?;
    let request = store.get(&JsValue::from_str(key))?;
    let value = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    if value.is_undefined() || value.is_null() {
        return Ok(None);
    }
    let array = js_sys::Uint8Array::new(&value);
    Ok(Some(array.to_vec()))
}

pub(crate) async fn idb_put_bytes(
    db: &IdbDatabase,
    store: &str,
    key: &str,
    value: &[u8],
) -> Result<(), JsValue> {
    let tx = db.transaction_with_str_and_mode(store, IdbTransactionMode::Readwrite)?;
    let store = tx.object_store(store)?;
    let array = js_sys::Uint8Array::from(value);
    let request = store.put_with_key(&array.buffer(), &JsValue::from_str(key))?;
    let _ = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    Ok(())
}

pub(crate) async fn idb_get_string(
    db: &IdbDatabase,
    store: &str,
    key: &str,
) -> Result<Option<String>, JsValue> {
    let tx = db.transaction_with_str_and_mode(store, IdbTransactionMode::Readonly)?;
    let store = tx.object_store(store)?;
    let request = store.get(&JsValue::from_str(key))?;
    let value = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    if value.is_undefined() || value.is_null() {
        return Ok(None);
    }
    let raw = value.as_string().unwrap_or_default();
    if raw.trim().is_empty() {
        return Ok(None);
    }
    Ok(Some(raw))
}

pub(crate) async fn idb_put_string(
    db: &IdbDatabase,
    store: &str,
    key: &str,
    value: &str,
) -> Result<(), JsValue> {
    let tx = db.transaction_with_str_and_mode(store, IdbTransactionMode::Readwrite)?;
    let store = tx.object_store(store)?;
    let request = store.put_with_key(&JsValue::from_str(value), &JsValue::from_str(key))?;
    let _ = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    Ok(())
}

pub(crate) async fn idb_delete_key(
    db: &IdbDatabase,
    store: &str,
    key: &str,
) -> Result<(), JsValue> {
    let tx = db.transaction_with_str_and_mode(store, IdbTransactionMode::Readwrite)?;
    let store = tx.object_store(store)?;
    let request = store.delete(&JsValue::from_str(key))?;
    let _ = wasm_bindgen_futures::JsFuture::from(request_to_promise(request)).await?;
    Ok(())
}

pub(crate) fn js_err(error: JsValue) -> String {
    if let Some(value) = error.as_string() {
        return value;
    }
    if let Ok(json) = js_sys::JSON::stringify(&error) {
        if let Some(value) = json.as_string() {
            return value;
        }
    }
    "js error".to_string()
}

fn idb_factory() -> Result<IdbFactory, JsValue> {
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("missing window"))?;
    window
        .indexed_db()?
        .ok_or_else(|| JsValue::from_str("indexeddb unavailable"))
}

fn attach_upgrade_handler(request: &IdbOpenDbRequest) {
    let request_for_cb = request.clone();
    let on_upgrade = wasm_bindgen::closure::Closure::once(move |_event: Event| {
        let Ok(result) = request_for_cb.result() else {
            return;
        };
        let Ok(db) = result.dyn_into::<IdbDatabase>() else {
            return;
        };
        reset_stores(&db);
    });
    request.set_onupgradeneeded(Some(on_upgrade.as_ref().unchecked_ref()));
    on_upgrade.forget();
}

fn reset_stores(db: &IdbDatabase) {
    for store in ALL_STORES {
        let _ = db.delete_object_store(store);
    }
    for store in ALL_STORES {
        let _ = db.create_object_store(store);
    }
}

fn request_to_promise(request: IdbRequest) -> js_sys::Promise {
    js_sys::Promise::new(&mut |resolve, reject| {
        let success_request = request.clone();
        let on_success = wasm_bindgen::closure::Closure::once(move |_event: Event| {
            let result = success_request.result().unwrap_or(JsValue::UNDEFINED);
            let _ = resolve.call1(&JsValue::NULL, &result);
        });
        let on_error = wasm_bindgen::closure::Closure::once(move |_event: Event| {
            let _ = reject.call1(&JsValue::NULL, &JsValue::from_str("indexeddb request failed"));
        });
        request.set_onsuccess(Some(on_success.as_ref().unchecked_ref()));
        request.set_onerror(Some(on_error.as_ref().unchecked_ref()));
        on_success.forget();
        on_error.forget();
    })
}
