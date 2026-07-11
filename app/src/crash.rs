//! Post-panic containment. A wasm panic aborts without unwinding, so any
//! `RefCell` guard alive at the panic stays flagged borrowed and the whole
//! instance becomes unusable — re-entering it produces cascading borrow
//! panics and raw wasm traps. The only real recovery is a reload, driven by
//! the boot shell (`__HB_BOOT.crash`); this module reports the panic, flags
//! the instance as aborted so entry points go inert, and hands off to JS.

use std::cell::Cell;

#[cfg(target_arch = "wasm32")]
use crate::boot;

thread_local! {
    static ABORTED: Cell<bool> = Cell::new(false);
    static PANIC_IN_RENDER: Cell<bool> = Cell::new(false);
}

/// True once a panic has fired. All state is untrusted after that; render,
/// input, and network entry points check this and return immediately.
pub(crate) fn is_aborted() -> bool {
    ABORTED.with(|flag| flag.get())
}

pub(crate) fn install_hook() {
    #[cfg(target_arch = "wasm32")]
    {
        let hook: Box<dyn Fn(&std::panic::PanicHookInfo<'_>) + Sync + Send + 'static> =
            Box::new(|info| {
                ABORTED.with(|flag| flag.set(true));
                console_error_panic_hook::hook(info);
                boot::crash(&info.to_string());
            });
        // The yew dev panel's Renderer::render() overwrites any panic hook
        // that wasn't registered through yew::set_custom_panic_hook.
        #[cfg(feature = "dev-panel-yew")]
        yew::set_custom_panic_hook(hook);
        #[cfg(not(feature = "dev-panel-yew"))]
        std::panic::set_hook(hook);
        #[cfg(debug_assertions)]
        install_test_hooks();
    }
}

/// Debug-only console helpers to exercise the crash path end-to-end:
/// `__HB_TEST_PANIC()` panics immediately, `__HB_TEST_PANIC_IN_RENDER()`
/// panics from inside the next render pass (reproducing a leaked borrow).
#[cfg(all(target_arch = "wasm32", debug_assertions))]
fn install_test_hooks() {
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen::{JsCast, JsValue};

    let Some(window) = web_sys::window() else {
        return;
    };
    let panic_now: Box<dyn Fn()> = Box::new(|| {
        panic!("injected test panic");
    });
    let panic_now = Closure::wrap(panic_now);
    let _ = js_sys::Reflect::set(
        &window,
        &JsValue::from_str("__HB_TEST_PANIC"),
        panic_now.as_ref().unchecked_ref(),
    );
    panic_now.forget();
    let panic_in_render = Closure::wrap(Box::new(|| {
        PANIC_IN_RENDER.with(|flag| flag.set(true));
        crate::wgpu_app::request_render();
        crate::svg_app::request_render();
    }) as Box<dyn Fn()>);
    let _ = js_sys::Reflect::set(
        &window,
        &JsValue::from_str("__HB_TEST_PANIC_IN_RENDER"),
        panic_in_render.as_ref().unchecked_ref(),
    );
    panic_in_render.forget();
}

/// Consumed by the render path; true at most once after the debug test hook
/// armed it.
pub(crate) fn take_panic_in_render() -> bool {
    PANIC_IN_RENDER.with(|flag| flag.replace(false))
}
