use base64::Engine as _;
use heddobureika_core::{
    catalog::{puzzle_by_slug, BLANK_BLACK_SLUG, BLANK_WHITE_SLUG},
    PuzzleImageRef,
};

pub(crate) fn resolve_puzzle_image_src(image_ref: &PuzzleImageRef) -> Option<String> {
    match image_ref {
        PuzzleImageRef::BuiltIn { slug } => {
            if let Some(fill) = blank_fill_color(slug) {
                return Some(blank_image_data_url(fill));
            }
            puzzle_by_slug(slug).map(|entry| entry.src.to_string())
        }
        PuzzleImageRef::Private { .. } => None,
    }
}

/// Maps a blank test slug to its solid fill color, or `None` for ordinary
/// puzzles. Kept in lock-step with `BLANK_PUZZLES` in the core catalog.
fn blank_fill_color(slug: &str) -> Option<&'static str> {
    let trimmed = slug.trim();
    if trimmed.eq_ignore_ascii_case(BLANK_WHITE_SLUG) {
        Some("white")
    } else if trimmed.eq_ignore_ascii_case(BLANK_BLACK_SLUG) {
        Some("black")
    } else {
        None
    }
}

/// Synthesizes a flat-color image as a base64 SVG data URL. The blank test
/// puzzles have no backing file, so the renderer feeds this straight into the
/// usual `HtmlImageElement` load path instead of fetching a `src`.
fn blank_image_data_url(fill: &str) -> String {
    let svg = format!(
        "<svg xmlns='http://www.w3.org/2000/svg' width='1024' height='1024'>\
<rect width='1024' height='1024' fill='{fill}'/></svg>"
    );
    let encoded = base64::engine::general_purpose::STANDARD.encode(svg.as_bytes());
    format!("data:image/svg+xml;base64,{encoded}")
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn create_object_url(bytes: &[u8], mime: &str) -> Result<String, wasm_bindgen::JsValue> {
    use web_sys::{Blob, BlobPropertyBag, Url};

    let array = js_sys::Array::new();
    let u8_array = js_sys::Uint8Array::from(bytes);
    array.push(&u8_array.buffer());
    let options = BlobPropertyBag::new();
    if !mime.trim().is_empty() {
        options.set_type(mime);
    }
    let blob = Blob::new_with_u8_array_sequence_and_options(&array, &options)?;
    Url::create_object_url_with_blob(&blob)
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn revoke_object_url(url: &str) {
    let _ = web_sys::Url::revoke_object_url(url);
}
