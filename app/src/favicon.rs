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
    apply_favicon_palette(FAVICON_TEMPLATE, dark)
}

fn apply_favicon_palette(template: &str, dark: bool) -> String {
    let palette = if dark { DARK_PALETTE } else { LIGHT_PALETTE };
    let svg = replace_css_var(template, "--hb-favicon-bg", palette.bg);
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
    use crate::web::{color_scheme_query, prefers_dark_mode};
    use std::cell::{Cell, RefCell};
    use std::rc::Rc;

    use js_sys::Reflect;
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const THEMED_TEMPLATE: &str = r#"<svg viewBox="0 0 100 100">
<style>
.hb-favicon-bg { fill: var(--hb-favicon-bg, pink); }
.hb-favicon-glyph { fill: var(--hb-favicon-glyph, blue); }
</style>
<path d="M0 0L100 100"/>
</svg>"#;

    #[test]
    fn light_favicon_uses_light_palette() {
        let svg = apply_favicon_palette(THEMED_TEMPLATE, false);
        assert!(svg.contains(".hb-favicon-bg { fill: #141312; }"));
        assert!(svg.contains(".hb-favicon-glyph { fill: #fff; }"));
    }

    #[test]
    fn dark_favicon_swaps_palette() {
        let svg = apply_favicon_palette(THEMED_TEMPLATE, true);
        assert!(svg.contains(".hb-favicon-bg { fill: #fff; }"));
        assert!(svg.contains(".hb-favicon-glyph { fill: #141312; }"));
    }

    #[test]
    fn favicon_has_no_remaining_css_vars() {
        for template in [THEMED_TEMPLATE, FAVICON_TEMPLATE] {
            for dark in [false, true] {
                let svg = apply_favicon_palette(template, dark);
                assert!(!svg.contains("var(--hb-"));
            }
        }
    }

    #[test]
    fn favicon_preserves_geometry() {
        for dark in [false, true] {
            let themed = apply_favicon_palette(THEMED_TEMPLATE, dark);
            assert!(themed.contains(r#"viewBox="0 0 100 100""#));
            assert!(themed.contains(r#"<path d="M0 0L100 100"/>"#));
            // The Sketch export uses fixed brand colors, so palette application
            // must preserve the entire exported icon, including its geometry.
            assert_eq!(favicon_svg(dark), FAVICON_TEMPLATE);
        }
    }
}
