use heddobureika_core::{catalog::puzzle_by_slug, PuzzleImageRef};

pub(crate) fn resolve_puzzle_image_src(image_ref: &PuzzleImageRef) -> Option<String> {
    match image_ref {
        PuzzleImageRef::BuiltIn { slug } => {
            puzzle_by_slug(slug).map(|entry| entry.src.to_string())
        }
        PuzzleImageRef::Private { .. } => None,
    }
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
