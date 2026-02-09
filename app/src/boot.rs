#[cfg(target_arch = "wasm32")]
use std::cell::Cell;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::{JsCast, JsValue};

#[cfg(target_arch = "wasm32")]
use js_sys::{Function, Reflect};

#[cfg(target_arch = "wasm32")]
thread_local! {
    static READY_SENT: Cell<bool> = Cell::new(false);
}

#[cfg(target_arch = "wasm32")]
fn with_boot<F: FnOnce(&js_sys::Object)>(action: F) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(value) = Reflect::get(&window, &JsValue::from_str("__HB_BOOT")) else {
        return;
    };
    if value.is_null() || value.is_undefined() {
        return;
    }
    let Ok(obj) = value.dyn_into::<js_sys::Object>() else {
        return;
    };
    action(&obj);
}

#[cfg(target_arch = "wasm32")]
fn call(method: &str, args: &[JsValue]) {
    with_boot(|boot| {
        let Ok(value) = Reflect::get(boot, &JsValue::from_str(method)) else {
            return;
        };
        let Ok(func) = value.dyn_into::<Function>() else {
            return;
        };
        let array = js_sys::Array::new();
        for arg in args {
            array.push(arg);
        }
        let _ = func.apply(boot, &array);
    });
}

pub(crate) fn set_phase(label: &str, detail: &str) {
    #[cfg(target_arch = "wasm32")]
    {
        call(
            "setPhase",
            &[JsValue::from_str(label), JsValue::from_str(detail)],
        );
    }
}

#[allow(dead_code)]
pub(crate) fn set_progress(value: f32) {
    #[cfg(target_arch = "wasm32")]
    {
        call("setProgress", &[JsValue::from_f64(value as f64)]);
    }
}

pub(crate) fn fail(code: &str, message: &str, hint: &str) {
    #[cfg(target_arch = "wasm32")]
    {
        call(
            "fail",
            &[
                JsValue::from_str(code),
                JsValue::from_str(message),
                JsValue::from_str(hint),
            ],
        );
    }
}

pub(crate) fn ready() {
    #[cfg(target_arch = "wasm32")]
    {
        let already_sent = READY_SENT.with(|flag| {
            if flag.get() {
                true
            } else {
                flag.set(true);
                false
            }
        });
        if already_sent {
            return;
        }
        call("ready", &[]);
    }
}
