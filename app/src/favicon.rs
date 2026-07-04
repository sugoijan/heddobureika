// The template ships as the static fallback icon; edit the glyph there.
const FAVICON_TEMPLATE: &str = include_str!("../../seo/favicon.svg");

struct FaviconPalette {
    bg: &'static str,
    glyph: &'static str,
}

const LIGHT_PALETTE: FaviconPalette = FaviconPalette {
    bg: "#141312",
    glyph: "#fff",
};

const DARK_PALETTE: FaviconPalette = FaviconPalette {
    bg: "#fff",
    glyph: "#141312",
};

fn favicon_svg(dark: bool) -> String {
    let palette = if dark { DARK_PALETTE } else { LIGHT_PALETTE };
    let svg = replace_css_var(FAVICON_TEMPLATE, "--hb-favicon-bg", palette.bg);
    replace_css_var(&svg, "--hb-favicon-glyph", palette.glyph)
}

// Replaces every `var(<name>, <fallback>)` in the template with `value`, so
// edits to the fallback colors in seo/favicon.svg don't break the swap.
fn replace_css_var(svg: &str, name: &str, value: &str) -> String {
    let token = format!("var({name}");
    let mut result = String::with_capacity(svg.len());
    let mut rest = svg;
    while let Some(start) = rest.find(&token) {
        result.push_str(&rest[..start]);
        let after_token = &rest[start + token.len()..];
        let Some(end) = after_token.find(')') else {
            result.push_str(&rest[start..]);
            return result;
        };
        result.push_str(value);
        rest = &after_token[end + 1..];
    }
    result.push_str(rest);
    result
}

#[cfg(target_arch = "wasm32")]
pub(crate) use dom::init;

#[cfg(target_arch = "wasm32")]
mod dom {
    use std::cell::{Cell, RefCell};
    use std::rc::Rc;

    use js_sys::{Function, Reflect};
    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;

    use super::*;
    use crate::app_core::{AppCore, AppSubscription};
    use crate::core::ThemeMode;

    const FAVICON_SELECTOR: &str = r#"link[data-hb-favicon]"#;

    thread_local! {
        static CURRENT_MODE: Cell<ThemeMode> = const { Cell::new(ThemeMode::System) };
        static LAST_APPLIED_DARK: Cell<Option<bool>> = const { Cell::new(None) };
        static CORE_SUBSCRIPTION: RefCell<Option<AppSubscription>> = const { RefCell::new(None) };
        static SCHEME_LISTENER: RefCell<Option<Closure<dyn FnMut()>>> = const { RefCell::new(None) };
    }

    pub(crate) fn init(core: &Rc<AppCore>) {
        let weak_core = Rc::downgrade(core);
        let subscription = core.subscribe(Rc::new(move || {
            if let Some(core) = weak_core.upgrade() {
                refresh(core.snapshot().app_settings.theme_mode);
            }
        }));
        CORE_SUBSCRIPTION.with(|slot| *slot.borrow_mut() = Some(subscription));
        install_scheme_listener();
        refresh(core.snapshot().app_settings.theme_mode);
    }

    pub(crate) fn refresh(mode: ThemeMode) {
        CURRENT_MODE.with(|current| current.set(mode));
        let dark = match mode {
            ThemeMode::Dark => true,
            ThemeMode::Light => false,
            ThemeMode::System => prefers_dark_mode(),
        };
        apply(dark);
    }

    fn apply(dark: bool) {
        if LAST_APPLIED_DARK.with(|last| last.get()) == Some(dark) {
            return;
        }
        let Some(document) = web_sys::window().and_then(|window| window.document()) else {
            return;
        };
        let Ok(Some(link)) = document.query_selector(FAVICON_SELECTOR) else {
            return;
        };
        if link.set_attribute("href", &favicon_data_url(dark)).is_ok() {
            LAST_APPLIED_DARK.with(|last| last.set(Some(dark)));
        }
    }

    fn favicon_data_url(dark: bool) -> String {
        let encoded = js_sys::encode_uri_component(&favicon_svg(dark));
        format!("data:image/svg+xml,{}", String::from(encoded))
    }

    // Re-applies the favicon when the OS scheme flips while the theme mode is System.
    fn install_scheme_listener() {
        let Some(query) = color_scheme_query() else {
            return;
        };
        let listener = Closure::<dyn FnMut()>::new(move || {
            refresh(CURRENT_MODE.with(|current| current.get()));
        });
        if Reflect::set(&query, &"onchange".into(), listener.as_ref()).is_ok() {
            SCHEME_LISTENER.with(|slot| *slot.borrow_mut() = Some(listener));
        }
    }

    fn prefers_dark_mode() -> bool {
        let Some(query) = color_scheme_query() else {
            return false;
        };
        Reflect::get(&query, &"matches".into())
            .ok()
            .and_then(|value| value.as_bool())
            .unwrap_or(false)
    }

    fn color_scheme_query() -> Option<wasm_bindgen::JsValue> {
        let window = web_sys::window()?;
        let match_media = Reflect::get(&window, &"matchMedia".into()).ok()?;
        let match_media = match_media.dyn_into::<Function>().ok()?;
        match_media
            .call1(&window, &"(prefers-color-scheme: dark)".into())
            .ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn light_favicon_uses_light_palette() {
        let svg = favicon_svg(false);
        assert!(svg.contains("fill: #141312"));
        assert!(svg.contains("fill: #fff"));
    }

    #[test]
    fn dark_favicon_swaps_palette() {
        let svg = favicon_svg(true);
        assert!(svg.contains(".hb-favicon-bg { fill: #fff; }"));
        assert!(svg.contains(".hb-favicon-glyph { fill: #141312; }"));
    }

    #[test]
    fn favicon_has_no_remaining_css_vars() {
        for dark in [false, true] {
            let svg = favicon_svg(dark);
            assert!(!svg.contains("var(--hb-"));
        }
    }

    #[test]
    fn favicon_preserves_geometry() {
        let svg = favicon_svg(false);
        assert!(svg.contains(r#"viewBox="0 0 100 100""#));
        assert!(svg.contains("<path"));
    }
}
