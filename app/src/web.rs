//! Thin wrappers over browser APIs shared by the renderers, the dev panel and
//! the sync layer. Everything here compiles on native targets too (as inert
//! `web-sys` externs or explicit no-ops), so callers need no `cfg` blocks.

use js_sys::{Date, Function, Reflect};
use wasm_bindgen::{JsCast, JsValue};

/// Wall-clock milliseconds folded into `f32` range. Only differences between
/// two samples are meaningful; for a monotonic clock use
/// [`crate::app_core::now_ms_f32`].
pub(crate) fn now_ms() -> f32 {
    (Date::now() % 1_000_000.0) as f32
}

/// Wall-clock milliseconds since the Unix epoch.
pub(crate) fn now_ms_u64() -> u64 {
    Date::now().max(0.0) as u64
}

/// The `matchMedia("(prefers-color-scheme: dark)")` list, when available.
pub(crate) fn color_scheme_query() -> Option<JsValue> {
    let window = web_sys::window()?;
    let match_media = Reflect::get(&window, &"matchMedia".into()).ok()?;
    let match_media = match_media.dyn_into::<Function>().ok()?;
    match_media
        .call1(&window, &"(prefers-color-scheme: dark)".into())
        .ok()
}

/// Whether the browser currently prefers a dark colour scheme.
pub(crate) fn prefers_dark_mode() -> bool {
    let Some(query) = color_scheme_query() else {
        return false;
    };
    Reflect::get(&query, &"matches".into())
        .ok()
        .and_then(|value| value.as_bool())
        .unwrap_or(false)
}

/// Opens `url` in a new tab.
pub(crate) fn open_url(url: &str) {
    if let Some(window) = web_sys::window() {
        let _ = window.open_with_url_and_target(url, "_blank");
    }
}

/// `console.log`, a no-op on native targets.
pub(crate) fn log(message: &str) {
    #[cfg(target_arch = "wasm32")]
    gloo::console::log!(message);
}

/// `console.warn`, a no-op on native targets.
pub(crate) fn warn(message: &str) {
    #[cfg(target_arch = "wasm32")]
    gloo::console::warn!(message);
}
