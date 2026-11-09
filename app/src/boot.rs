//! Bridge to the boot shell in `index.html` (`window.__HB_BOOT`), which owns
//! the loading overlay, failure screen and crash recovery.

use std::cell::Cell;

use wasm_bindgen::JsValue;

thread_local! {
    static READY_SENT: Cell<bool> = const { Cell::new(false) };
}

/// Calls `__HB_BOOT.<method>(...args())` when the boot shell is present. The
/// arguments are built lazily so native builds, where `JsValue`s cannot be
/// constructed, never evaluate them.
#[cfg(target_arch = "wasm32")]
fn call(method: &str, args: impl FnOnce() -> Vec<JsValue>) {
    use js_sys::{Function, Reflect};
    use wasm_bindgen::JsCast;

    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(value) = Reflect::get(&window, &JsValue::from_str("__HB_BOOT")) else {
        return;
    };
    if value.is_null() || value.is_undefined() {
        return;
    }
    let Ok(boot) = value.dyn_into::<js_sys::Object>() else {
        return;
    };
    let Ok(value) = Reflect::get(&boot, &JsValue::from_str(method)) else {
        return;
    };
    let Ok(func) = value.dyn_into::<Function>() else {
        return;
    };
    let array = js_sys::Array::new();
    for arg in args() {
        array.push(&arg);
    }
    let _ = func.apply(&boot, &array);
}

#[cfg(not(target_arch = "wasm32"))]
fn call(_method: &str, _args: impl FnOnce() -> Vec<JsValue>) {}

pub(crate) fn set_phase(label: &str, detail: &str) {
    call("setPhase", || {
        vec![JsValue::from_str(label), JsValue::from_str(detail)]
    });
}

pub(crate) fn fail(code: &str, message: &str, hint: &str) {
    call("fail", || {
        vec![
            JsValue::from_str(code),
            JsValue::from_str(message),
            JsValue::from_str(hint),
        ]
    });
}

/// Reports a Rust panic to the boot shell, which owns recovery (overlay +
/// reload). The wasm instance is unusable once this is called.
pub(crate) fn crash(message: &str) {
    call("crash", || vec![JsValue::from_str(message)]);
}

/// Records the joined multiplayer room so a crash reload can restore the
/// `#room=` route (the hash is cleared after joining).
pub(crate) fn note_room(room_id: &str) {
    call("noteRoom", || vec![JsValue::from_str(room_id)]);
}

pub(crate) fn clear_room() {
    call("clearRoom", Vec::new);
}

pub(crate) fn ready() {
    if READY_SENT.replace(true) {
        return;
    }
    call("ready", Vec::new);
}
