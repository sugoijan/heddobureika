use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::timers::future::TimeoutFuture;
use js_sys::{Date, Math};
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    Element, Event, File, HtmlImageElement, HtmlInputElement, HtmlSelectElement, InputEvent,
    KeyboardEvent, MouseEvent,
};
use yew::prelude::*;

use crate::app_builder;
use crate::app_core::AppCore;
use crate::app_router;
use crate::app_runtime;
use crate::boot_runtime::{self, BootState};
use crate::core::*;
use crate::model::*;
#[cfg(all(test, target_arch = "wasm32"))]
use crate::multiplayer_bridge;
use crate::multiplayer_identity;
use crate::multiplayer_sync::MultiplayerSyncAdapter;
use crate::persisted::{PrivateImageEntry, PrivateImageRefs, LOCAL_PRIVATE_SCOPE};
use crate::persisted_store;
use crate::runtime::{FailReason, SyncEvent, SyncHooks};
use crate::sync_runtime;
use crate::view_runtime;
use heddobureika_core::catalog::{
    blank_puzzle_by_slug, PuzzleCatalogEntry, BLANK_PUZZLES, PUZZLE_CATALOG,
};
use heddobureika_core::{
    available_topologies, is_valid_room_id, logical_image_size, topology_kind_for_tag, AdminMsg,
    ClientId, PieceCountChoice, PlayableGameSnapshot, PuzzleImageRef, PuzzleInfo, PuzzleSpec,
    PuzzleTopology, RoomControlUpdate, ServerMsg, TopologySpec, ASSET_CHUNK_BYTES,
    PRIVATE_UPLOAD_MAX_BYTES, ROOM_ID_ALPHABET, ROOM_ID_LEN,
};
use image_pipeline::{AlphaMode, PipelineConfig};
use sha2::{Digest, Sha256};
#[derive(Clone, Copy, PartialEq, Eq)]
enum AdminStatus {
    Idle,
    Connecting,
    Accepted,
    Failed,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AdminUploadStatus {
    Idle,
    Reading,
    Sending,
    AwaitingAck,
    Done,
    Failed,
}

#[derive(Clone)]
enum AdminSocketEvent {
    Welcome,
    AdminAck,
    Error(String),
    ConnectionFailed(String),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RoomSetupStatus {
    Idle,
    Creating,
    Connecting,
    Failed,
}

#[derive(Properties)]
struct AppProps {
    core: Rc<AppCore>,
}

impl PartialEq for AppProps {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.core, &other.core)
    }
}

const WS_DELAY_IN_KEY: &str = "heddobureika.debug.ws_in_ms";
const WS_DELAY_OUT_KEY: &str = "heddobureika.debug.ws_out_ms";
const WS_DELAY_JITTER_KEY: &str = "heddobureika.debug.ws_jitter_ms";
const DEV_PANEL_GROUP_PUZZLE_KEY: &str = "heddobureika.devpanel.group.puzzle.v1";
const DEV_PANEL_GROUP_MULTIPLAYER_KEY: &str = "heddobureika.devpanel.group.multiplayer.v1";
const DEV_PANEL_GROUP_GRAPHICS_KEY: &str = "heddobureika.devpanel.group.graphics.v1";
const DEV_PANEL_GROUP_RULES_KEY: &str = "heddobureika.devpanel.group.rules.v1";
const DEV_PANEL_GROUP_SHAPING_KEY: &str = "heddobureika.devpanel.group.shaping.v1";

type PuzzleArt = PuzzleCatalogEntry;
const PUZZLE_ARTS: &[PuzzleArt] = PUZZLE_CATALOG;

#[derive(Clone)]
struct PuzzleInfoStore {
    state: UseStateHandle<Option<PuzzleInfo>>,
    live: Rc<RefCell<Option<PuzzleInfo>>>,
}

impl PuzzleInfoStore {
    fn new(
        state: UseStateHandle<Option<PuzzleInfo>>,
        live: Rc<RefCell<Option<PuzzleInfo>>>,
    ) -> Self {
        Self { state, live }
    }

    #[cfg(test)]
    fn get(&self) -> Option<PuzzleInfo> {
        self.live.borrow().clone()
    }

    fn set(&self, info: Option<PuzzleInfo>) {
        *self.live.borrow_mut() = info.clone();
        self.state.set(info);
    }
}

struct AdminSocket {
    adapter: Rc<RefCell<MultiplayerSyncAdapter>>,
    connect_key: Option<String>,
    connected: Rc<Cell<bool>>,
    pending: Rc<RefCell<Vec<AdminMsg>>>,
    status: Rc<Cell<AdminStatus>>,
    status_hook: Option<Rc<dyn Fn(AdminStatus)>>,
    connect_seq: Rc<Cell<u64>>,
    upload_pending: Rc<Cell<bool>>,
    upload_status_hook: Option<Rc<dyn Fn(AdminUploadStatus, Option<String>)>>,
    event_hook: Option<Rc<dyn Fn(AdminSocketEvent)>>,
}

impl AdminSocket {
    fn new() -> Self {
        Self {
            adapter: Rc::new(RefCell::new(MultiplayerSyncAdapter::new())),
            connect_key: None,
            connected: Rc::new(Cell::new(false)),
            pending: Rc::new(RefCell::new(Vec::new())),
            status: Rc::new(Cell::new(AdminStatus::Idle)),
            status_hook: None,
            connect_seq: Rc::new(Cell::new(0)),
            upload_pending: Rc::new(Cell::new(false)),
            upload_status_hook: None,
            event_hook: None,
        }
    }

    fn set_status_hook(&mut self, hook: Rc<dyn Fn(AdminStatus)>) {
        self.status_hook = Some(hook);
    }

    fn set_upload_status_hook(&mut self, hook: Rc<dyn Fn(AdminUploadStatus, Option<String>)>) {
        self.upload_status_hook = Some(hook);
    }

    fn set_event_hook(&mut self, hook: Rc<dyn Fn(AdminSocketEvent)>) {
        self.event_hook = Some(hook);
    }

    fn reset(&mut self) {
        self.adapter.borrow_mut().disconnect();
        self.connect_key = None;
        self.connected.set(false);
        self.pending.borrow_mut().clear();
        self.update_status(AdminStatus::Idle);
        self.upload_pending.set(false);
        self.notify_upload_status(AdminUploadStatus::Idle, None);
        let next = self.connect_seq.get().wrapping_add(1);
        self.connect_seq.set(next);
    }

    fn send(&mut self, ws_base: String, room_id: String, admin_token: String, msg: AdminMsg) {
        self.pending.borrow_mut().push(msg);
        let url = app_router::build_room_ws_url(&ws_base, &room_id);
        let connect_key = format!("{url}|{admin_token}");
        let url_changed = self.connect_key.as_deref() != Some(connect_key.as_str());
        if url_changed {
            self.connect_key = Some(connect_key);
            self.connected.set(false);
            self.update_status(AdminStatus::Connecting);
            self.connect(url, room_id, admin_token);
            return;
        }
        if self.connected.get() {
            self.flush();
        } else {
            self.update_status(AdminStatus::Connecting);
            self.connect(url, room_id, admin_token);
        }
    }

    fn send_upload(
        &mut self,
        ws_base: String,
        room_id: String,
        admin_token: String,
        msgs: Vec<AdminMsg>,
    ) {
        if msgs.is_empty() {
            return;
        }
        self.upload_pending.set(true);
        self.pending.borrow_mut().extend(msgs);
        let url = app_router::build_room_ws_url(&ws_base, &room_id);
        let connect_key = format!("{url}|{admin_token}");
        let url_changed = self.connect_key.as_deref() != Some(connect_key.as_str());
        if url_changed {
            self.connect_key = Some(connect_key);
            self.connected.set(false);
            self.update_status(AdminStatus::Connecting);
            self.connect(url, room_id, admin_token);
            return;
        }
        if self.connected.get() {
            self.flush();
        } else {
            self.update_status(AdminStatus::Connecting);
            self.connect(url, room_id, admin_token);
        }
    }

    fn ensure_connected(&mut self, ws_base: String, room_id: String, admin_token: String) {
        let url = app_router::build_room_ws_url(&ws_base, &room_id);
        let connect_key = format!("{url}|{admin_token}");
        if self.connect_key.as_deref() == Some(connect_key.as_str()) && self.connected.get() {
            return;
        }
        self.connect_key = Some(connect_key);
        self.connected.set(false);
        self.update_status(AdminStatus::Connecting);
        self.connect(url, room_id, admin_token);
    }

    fn connect(&mut self, url: String, room_id: String, admin_token: String) {
        let adapter = self.adapter.clone();
        let adapter_for_open = adapter.clone();
        let pending = self.pending.clone();
        let connected = self.connected.clone();
        let status_cell = self.status.clone();
        let status_on_fail = status_cell.clone();
        let status_on_msg = status_cell.clone();
        let status_hook = self.status_hook.clone();
        let event_hook = self.event_hook.clone();
        let on_open = Rc::new(move || {
            connected.set(true);
            let messages = pending.borrow_mut().drain(..).collect::<Vec<_>>();
            for msg in messages {
                adapter_for_open.borrow().send_admin(msg);
            }
        });
        let connected_on_fail = self.connected.clone();
        let upload_pending_on_fail = self.upload_pending.clone();
        let upload_status_hook_on_fail = self.upload_status_hook.clone();
        let event_hook_on_fail = event_hook.clone();
        let on_fail = Rc::new(move |_reason: FailReason| {
            connected_on_fail.set(false);
            status_on_fail.set(AdminStatus::Failed);
            if let Some(hook) = status_hook.as_ref() {
                hook(AdminStatus::Failed);
            }
            if let Some(hook) = event_hook_on_fail.as_ref() {
                hook(AdminSocketEvent::ConnectionFailed(
                    "admin connection failed".to_string(),
                ));
            }
            if upload_pending_on_fail.get() {
                upload_pending_on_fail.set(false);
                if let Some(hook) = upload_status_hook_on_fail.as_ref() {
                    hook(
                        AdminUploadStatus::Failed,
                        Some("admin connection failed".to_string()),
                    );
                }
            }
        });
        let status_hook = self.status_hook.clone();
        let upload_pending_on_msg = self.upload_pending.clone();
        let upload_status_hook_on_msg = self.upload_status_hook.clone();
        let event_hook_on_msg = event_hook;
        let on_server_msg = Rc::new(move |msg: ServerMsg| match msg {
            ServerMsg::AdminAck {
                room_id,
                persistence,
            } => {
                status_on_msg.set(AdminStatus::Accepted);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Accepted);
                }
                if let Some(hook) = event_hook_on_msg.as_ref() {
                    hook(AdminSocketEvent::AdminAck);
                }
                gloo::console::log!("admin ack", room_id, format!("{persistence:?}"));
            }
            ServerMsg::Welcome { .. } => {
                status_on_msg.set(AdminStatus::Accepted);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Accepted);
                }
                if let Some(hook) = event_hook_on_msg.as_ref() {
                    hook(AdminSocketEvent::Welcome);
                }
            }
            ServerMsg::Error { code, message } => {
                status_on_msg.set(AdminStatus::Failed);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Failed);
                }
                let error_message = format!("{code}: {message}");
                if let Some(hook) = event_hook_on_msg.as_ref() {
                    hook(AdminSocketEvent::Error(error_message.clone()));
                }
                gloo::console::warn!("admin error", code.clone(), message.clone());
                if upload_pending_on_msg.get() {
                    upload_pending_on_msg.set(false);
                    if let Some(hook) = upload_status_hook_on_msg.as_ref() {
                        hook(AdminUploadStatus::Failed, Some(error_message));
                    }
                }
            }
            ServerMsg::UploadAck { hash } => {
                if upload_pending_on_msg.get() {
                    upload_pending_on_msg.set(false);
                    if let Some(hook) = upload_status_hook_on_msg.as_ref() {
                        hook(AdminUploadStatus::Done, Some(format!("upload ok: {hash}")));
                    }
                }
            }
            _ => {}
        });
        let connect_seq = self.connect_seq.clone();
        let seq = connect_seq.get().wrapping_add(1);
        connect_seq.set(seq);
        let url_for_connect = url.clone();
        spawn_local(async move {
            let protocol =
                match multiplayer_identity::build_auth_protocol(&room_id, Some(&admin_token)).await
                {
                    Ok(protocol) => protocol,
                    Err(err) => {
                        gloo::console::warn!("admin auth failed", err);
                        on_fail(FailReason::Terminal);
                        return;
                    }
                };
            if connect_seq.get() != seq {
                return;
            }
            adapter.borrow_mut().connect_with_open(
                &url_for_connect,
                on_server_msg,
                on_fail,
                Some(on_open),
                Some(vec![protocol]),
            );
        });
    }

    fn flush(&self) {
        if !self.connected.get() {
            return;
        }
        let messages = self.pending.borrow_mut().drain(..).collect::<Vec<_>>();
        for msg in messages {
            self.adapter.borrow().send_admin(msg);
        }
    }

    fn update_status(&mut self, status: AdminStatus) {
        if self.status.get() == status {
            return;
        }
        self.status.set(status);
        if let Some(hook) = self.status_hook.as_ref() {
            hook(status);
        }
    }

    fn notify_upload_status(&self, status: AdminUploadStatus, message: Option<String>) {
        if let Some(hook) = self.upload_status_hook.as_ref() {
            hook(status, message);
        }
    }
}

/// Whether a keyboard event originated from an editable field, so global
/// shortcuts can bow out while the user is typing.
fn event_target_is_editable(event: &KeyboardEvent) -> bool {
    event
        .target()
        .and_then(|target| target.dyn_into::<web_sys::Element>().ok())
        .map(|element| {
            let tag = element.tag_name();
            tag.eq_ignore_ascii_case("input")
                || tag.eq_ignore_ascii_case("textarea")
                || tag.eq_ignore_ascii_case("select")
        })
        .unwrap_or(false)
}

fn puzzle_art_index_by_slug(slug: &str) -> Option<usize> {
    let trimmed = slug.trim();
    PUZZLE_ARTS
        .iter()
        .position(|art| art.slug.eq_ignore_ascii_case(trimmed))
}

fn encode_hash_value(value: &str) -> String {
    let raw = value.trim();
    if raw.is_empty() {
        return String::new();
    }
    js_sys::encode_uri_component(raw)
        .as_string()
        .unwrap_or_else(|| raw.to_string())
}

fn generate_room_id() -> String {
    let alphabet = ROOM_ID_ALPHABET.as_bytes();
    let mut room_id = String::with_capacity(ROOM_ID_LEN);
    for _ in 0..ROOM_ID_LEN {
        let idx = (Math::random() * alphabet.len() as f64).floor() as usize;
        room_id.push(alphabet[idx.min(alphabet.len().saturating_sub(1))] as char);
    }
    room_id
}

fn room_setup_status_label(status: RoomSetupStatus) -> &'static str {
    match status {
        RoomSetupStatus::Idle => "",
        RoomSetupStatus::Creating => "Creating room...",
        RoomSetupStatus::Connecting => "Connecting...",
        RoomSetupStatus::Failed => "Setup failed",
    }
}

fn base_url_without_hash() -> Option<String> {
    let window = web_sys::window()?;
    let href = window.location().href().ok()?;
    let base = href.split('#').next().unwrap_or(&href).to_string();
    Some(base)
}

#[cfg(test)]
fn initial_show_controls() -> bool {
    true
}

#[cfg(not(test))]
fn initial_show_controls() -> bool {
    false
}

#[cfg(test)]
thread_local! {
    static MP_TEST_HOOKS: std::cell::RefCell<Option<MpTestHooks>> = std::cell::RefCell::new(None);
    static MP_TEST_LAST_WARN: std::cell::RefCell<Option<String>> = std::cell::RefCell::new(None);
}

#[cfg(test)]
#[derive(Clone)]
struct MpTestHooks {
    send_msg: Rc<dyn Fn(ServerMsg)>,
    set_puzzle_info: Rc<dyn Fn(Option<PuzzleInfo>)>,
    set_server_state_applied: Rc<dyn Fn(bool)>,
}

#[cfg(test)]
fn set_mp_test_hooks(hooks: MpTestHooks) {
    MP_TEST_HOOKS.with(|slot| {
        *slot.borrow_mut() = Some(hooks);
    });
}

#[cfg(test)]
fn clear_mp_test_hooks() {
    MP_TEST_HOOKS.with(|slot| {
        slot.borrow_mut().take();
    });
}

#[cfg(test)]
fn record_mp_warn(msg: &str) {
    MP_TEST_LAST_WARN.with(|slot| {
        *slot.borrow_mut() = Some(msg.to_string());
    });
}

#[cfg(test)]
fn take_mp_warn() -> Option<String> {
    MP_TEST_LAST_WARN.with(|slot| slot.borrow_mut().take())
}

fn initial_render_settings() -> RenderSettings {
    #[cfg(all(test, target_arch = "wasm32"))]
    {
        let mut settings = RenderSettings::default();
        settings.renderer = RendererKind::Svg;
        settings
    }
    #[cfg(all(test, not(target_arch = "wasm32")))]
    {
        RenderSettings::default()
    }
    #[cfg(not(test))]
    {
        app_router::load_render_settings_with_init()
    }
}

fn load_theme_mode() -> Option<ThemeMode> {
    Some(persisted_store::settings_blob().theme_mode)
}

#[cfg(target_arch = "wasm32")]
const THEME_MODE_KEY: &str = "hb.theme_mode";

#[cfg(target_arch = "wasm32")]
fn persist_theme_mode(mode: ThemeMode) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(Some(storage)) = window.local_storage() else {
        return;
    };
    let value = match mode {
        ThemeMode::System => "system",
        ThemeMode::Light => "light",
        ThemeMode::Dark => "dark",
    };
    let _ = storage.set_item(THEME_MODE_KEY, value);
}

#[cfg(not(target_arch = "wasm32"))]
fn persist_theme_mode(_mode: ThemeMode) {}

fn clear_saved_game() {
    crate::sync_runtime::clear_local_snapshot();
}

fn now_ms_u64() -> u64 {
    Date::now().max(0.0) as u64
}

async fn read_file_bytes(file: File) -> Result<Vec<u8>, String> {
    let buffer = wasm_bindgen_futures::JsFuture::from(file.array_buffer())
        .await
        .map_err(|_| "failed to read file".to_string())?;
    let array = js_sys::Uint8Array::new(&buffer);
    Ok(array.to_vec())
}

fn is_avif_bytes(bytes: &[u8]) -> bool {
    if bytes.len() < 12 {
        return false;
    }
    if &bytes[4..8] != b"ftyp" {
        return false;
    }
    matches!(&bytes[8..12], b"avif" | b"avis")
}

async fn load_image_dimensions(file: File) -> Result<(u32, u32), String> {
    let url = web_sys::Url::create_object_url_with_blob(&file)
        .map_err(|_| "failed to read image".to_string())?;
    let img = HtmlImageElement::new().map_err(|_| "failed to read image".to_string())?;
    let img = std::rc::Rc::new(img);
    let promise = js_sys::Promise::new(&mut |resolve, reject| {
        let img_onload = img.clone();
        let url_for_onload = url.clone();
        let onload = wasm_bindgen::closure::Closure::once(move || {
            let width = img_onload.natural_width();
            let height = img_onload.natural_height();
            let _ = web_sys::Url::revoke_object_url(&url_for_onload);
            let result = js_sys::Array::new();
            result.push(&wasm_bindgen::JsValue::from_f64(width as f64));
            result.push(&wasm_bindgen::JsValue::from_f64(height as f64));
            let _ = resolve.call1(&wasm_bindgen::JsValue::NULL, &result);
        });
        let url_for_onerror = url.clone();
        let onerror = wasm_bindgen::closure::Closure::once(move || {
            let _ = web_sys::Url::revoke_object_url(&url_for_onerror);
            let _ = reject.call1(
                &wasm_bindgen::JsValue::NULL,
                &wasm_bindgen::JsValue::from_str("image_load_failed"),
            );
        });
        img.set_onload(Some(onload.as_ref().unchecked_ref()));
        img.set_onerror(Some(onerror.as_ref().unchecked_ref()));
        img.set_src(&url);
        onload.forget();
        onerror.forget();
    });
    let value = wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|_| "failed to read image".to_string())?;
    let array = js_sys::Array::from(&value);
    let width = array.get(0).as_f64().unwrap_or(0.0) as u32;
    let height = array.get(1).as_f64().unwrap_or(0.0) as u32;
    if width == 0 || height == 0 {
        return Err("invalid image dimensions".to_string());
    }
    Ok((width, height))
}

fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut hex = String::with_capacity(digest.len() * 2);
    for byte in digest {
        let _ = std::fmt::Write::write_fmt(&mut hex, format_args!("{:02x}", byte));
    }
    hex
}

fn save_theme_mode(mode: ThemeMode) {
    persisted_store::update_settings_blob(|settings| {
        settings.theme_mode = mode;
    });
    persist_theme_mode(mode);
}

fn load_admin_token() -> Option<String> {
    persisted_store::settings_blob()
        .admin_token
        .map(|token| token.trim().to_string())
        .filter(|token| !token.is_empty())
}

fn save_admin_token(token: &str) {
    let token = token.trim().to_string();
    persisted_store::update_settings_blob(|settings| {
        settings.admin_token = if token.is_empty() {
            None
        } else {
            Some(token.clone())
        };
    });
}

fn load_ws_delay_value(key: &str) -> String {
    let ws_delay = persisted_store::settings_blob().ws_delay;
    let value = match key {
        WS_DELAY_IN_KEY => ws_delay.inbound_ms,
        WS_DELAY_OUT_KEY => ws_delay.outbound_ms,
        WS_DELAY_JITTER_KEY => ws_delay.jitter_ms,
        _ => None,
    };
    value.map(|value| value.to_string()).unwrap_or_default()
}

fn save_ws_delay_value(key: &str, raw: &str) {
    let trimmed = raw.trim();
    let value = if trimmed.is_empty() {
        None
    } else {
        trimmed.parse::<u32>().ok()
    };
    if trimmed.is_empty() || value.is_some() {
        persisted_store::update_settings_blob(|settings| match key {
            WS_DELAY_IN_KEY => settings.ws_delay.inbound_ms = value,
            WS_DELAY_OUT_KEY => settings.ws_delay.outbound_ms = value,
            WS_DELAY_JITTER_KEY => settings.ws_delay.jitter_ms = value,
            _ => {}
        });
    }
}

fn load_dev_panel_group_open(key: &str, default_value: bool) -> bool {
    let dev_panel = persisted_store::settings_blob().dev_panel;
    match key {
        DEV_PANEL_GROUP_PUZZLE_KEY => dev_panel.puzzle_open,
        DEV_PANEL_GROUP_MULTIPLAYER_KEY => dev_panel.multiplayer_open,
        DEV_PANEL_GROUP_GRAPHICS_KEY => dev_panel.graphics_open,
        DEV_PANEL_GROUP_RULES_KEY => dev_panel.rules_open,
        DEV_PANEL_GROUP_SHAPING_KEY => dev_panel.shaping_open,
        _ => default_value,
    }
}

fn save_dev_panel_group_open(key: &str, value: bool) {
    persisted_store::update_settings_blob(|settings| match key {
        DEV_PANEL_GROUP_PUZZLE_KEY => settings.dev_panel.puzzle_open = value,
        DEV_PANEL_GROUP_MULTIPLAYER_KEY => settings.dev_panel.multiplayer_open = value,
        DEV_PANEL_GROUP_GRAPHICS_KEY => settings.dev_panel.graphics_open = value,
        DEV_PANEL_GROUP_RULES_KEY => settings.dev_panel.rules_open = value,
        DEV_PANEL_GROUP_SHAPING_KEY => settings.dev_panel.shaping_open = value,
        _ => {}
    });
}

fn details_toggle(handle: UseStateHandle<bool>, key: &'static str) -> Callback<Event> {
    Callback::from(move |event: Event| {
        let element: Element = event.target_unchecked_into();
        let details = element.closest("details").ok().flatten().unwrap_or(element);
        let open = details.has_attribute("open");
        handle.set(open);
        save_dev_panel_group_open(key, open);
    })
}

fn sync_theme_checkbox(input: &HtmlInputElement, mode: ThemeMode) {
    let (checked, indeterminate) = match mode {
        ThemeMode::System => (false, true),
        ThemeMode::Light => (false, false),
        ThemeMode::Dark => (true, false),
    };
    input.set_checked(checked);
    input.set_indeterminate(indeterminate);
}

fn anchor_of_from_state(state: &PuzzleState) -> Vec<usize> {
    state.pieces.iter().map(|piece| piece.group()).collect()
}

fn group_transforms_from_state(state: &PuzzleState, total: usize) -> (Vec<(f32, f32)>, Vec<f32>) {
    let mut group_pos = vec![(0.0, 0.0); total];
    let mut group_rot = vec![0.0; total];
    for (id, group) in state.groups.iter().enumerate() {
        if let Some(group) = group {
            if id < total {
                group_pos[id] = (group.transform.pos[0], group.transform.pos[1]);
                group_rot[id] = group.transform.rot_deg;
            }
        }
    }
    (group_pos, group_rot)
}

struct UiDerived {
    positions: Vec<(f32, f32)>,
    rotations: Vec<f32>,
    z_order: Vec<usize>,
    anchor_of: Vec<usize>,
    group_pos: Vec<(f32, f32)>,
    group_rot: Vec<f32>,
    group_order: Vec<usize>,
}

fn derive_ui_state_from_puzzle(
    state: &PuzzleState,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
) -> UiDerived {
    let total = state.groups.len();
    let (positions, rotations) = state.derive_piece_transforms(cols, piece_width, piece_height);
    let z_order = state.build_piece_order();
    let anchor_of = anchor_of_from_state(state);
    let (group_pos, group_rot) = group_transforms_from_state(state, total);
    let group_order = state.group_order.clone();
    UiDerived {
        positions,
        rotations,
        z_order,
        anchor_of,
        group_pos,
        group_rot,
        group_order,
    }
}

fn on_setting_change<F>(
    app_core: Rc<AppCore>,
    settings: UseStateHandle<ShapeSettings>,
    updater: F,
) -> Callback<InputEvent>
where
    F: Fn(&mut ShapeSettings, f32) + 'static,
{
    Callback::from(move |event: InputEvent| {
        let input: HtmlInputElement = event.target_unchecked_into();
        if let Ok(value) = input.value().parse::<f32>() {
            let mut next = (*settings).clone();
            updater(&mut next, value);
            settings.set(next.clone());
            app_core.set_shape_settings(next);
        }
    })
}

fn parse_optional_seed(raw: &str) -> Option<u32> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (value, radix) = if let Some(rest) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        (rest, 16)
    } else {
        (trimmed, 10)
    };
    u32::from_str_radix(value, radix).ok()
}

/// One puzzle-configuration change emitted by any of the shared puzzle
/// controls (art, topology, piece count, regenerate). The dispatcher
/// (`apply_puzzle_change` in the component) decides whether to apply it
/// locally or send it to the room as an admin action.
#[derive(Clone)]
struct PuzzleChangeRequest {
    entry: PuzzleCatalogEntry,
    grid_override: Option<GridChoice>,
    descriptor: TopologySpec,
    shape_seed: u32,
}

fn request_local_puzzle_change(
    app_core: Rc<AppCore>,
    image_max_dim: u32,
    entry: PuzzleCatalogEntry,
    grid_override: Option<GridChoice>,
    descriptor: TopologySpec,
    shape_seed: u32,
) {
    // Grid catalog entries get a dedicated path that also saves the
    // current grid pick for restore-on-reload.
    if descriptor.tag == "grid" {
        app_builder::request_puzzle_change(
            app_core,
            image_max_dim,
            entry,
            grid_override,
            shape_seed,
        );
        return;
    }
    let (logical_width, logical_height) =
        logical_image_size(entry.width, entry.height, image_max_dim);
    // Re-fit the spec to THIS entry's dimensions. When the user switches art,
    // the descriptor was resolved against the previously-active image, so an
    // aspect-dependent topology (triangular re-picks direction + lines/points;
    // Voronoi/hex re-derive their stretch) would otherwise carry a layout that
    // doesn't match the new aspect. `rebuild_for_image` preserves piece count
    // and is a no-op / idempotent for aspect-independent specs.
    let descriptor = topology_kind_for_tag(&descriptor.tag)
        .map(|kind| (kind.rebuild_for_image)(&descriptor, logical_width, logical_height))
        .unwrap_or(descriptor);
    app_core.set_puzzle_with_topology_seeded(
        entry.label.to_string(),
        PuzzleImageRef::BuiltIn {
            slug: entry.slug.to_string(),
        },
        (logical_width, logical_height),
        descriptor,
        None,
        shape_seed,
    );
}

#[function_component(App)]
fn app(props: &AppProps) -> Html {
    #[cfg(test)]
    gloo::console::log!("app render");
    let app_core = props.core.clone();
    let app_snapshot = use_state(|| app_core.snapshot());
    let show_dev_panel = true;
    let puzzle_info = use_state(|| None::<PuzzleInfo>);
    let puzzle_info_live = use_mut_ref(|| None::<PuzzleInfo>);
    let puzzle_info_store = PuzzleInfoStore::new(puzzle_info.clone(), puzzle_info_live.clone());
    let puzzle_info_value = (*puzzle_info).clone();
    let puzzle_dims_value = puzzle_info_value
        .as_ref()
        .map(|info| (info.image_width, info.image_height));
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let mut grid_choices = if let Some(info) = puzzle_info_value.as_ref() {
        build_grid_choices(info.image_width, info.image_height)
    } else {
        Vec::new()
    };
    if puzzle_info_value.is_some() && grid_choices.is_empty() {
        grid_choices.push(FALLBACK_GRID);
    }
    let grid_default_index = grid_choices
        .iter()
        .position(|choice| choice.target_count == DEFAULT_TARGET_COUNT)
        .unwrap_or(0);
    let grid_index = use_state(|| grid_default_index);
    let grid_index_value = *grid_index;
    let grid_custom_count = use_state(|| None::<u32>);
    let grid_custom_count_value = *grid_custom_count;
    let grid_custom_active = grid_custom_count_value.is_some();
    let preset_grid = grid_choices
        .get(grid_index_value)
        .copied()
        .or_else(|| grid_choices.first().copied())
        .unwrap_or(FALLBACK_GRID);
    let grid = if let (Some(count), Some((width, height))) =
        (grid_custom_count_value, puzzle_dims_value)
    {
        nearest_valid_grid(width, height, count).unwrap_or(preset_grid)
    } else {
        preset_grid
    };
    let grid_custom_input_value = grid_custom_count_value
        .map(|count| count.to_string())
        .unwrap_or_default();
    let local_topology = use_state(|| "grid".to_string());
    let local_topology_value = (*local_topology).clone();
    let active_topology_kind = topology_kind_for_tag(&local_topology_value)
        .copied()
        .unwrap_or(available_topologies()[0]);
    let local_topology_is_grid = active_topology_kind.tag == "grid";
    // Unified non-grid piece-count picker state. Grid keeps its own
    // historical state (`grid_index` / `grid_custom_count`) since it
    // also stores a *catalog selection* on disk; non-grid topologies
    // just need a target count + a seed for re-roll.
    let non_grid_target_count = use_state(|| active_topology_kind.default_target_count);
    let non_grid_custom_count = use_state(|| None::<u32>);
    // Initialised lazily from the live `info.shape_seed` once the
    // snapshot lands. The `0` placeholder gets replaced by the snapshot
    // observer below before any user can click Regenerate.
    let regenerate_seed = use_state(|| 0u32);
    let non_grid_target_count_value = *non_grid_target_count;
    let non_grid_custom_count_value = *non_grid_custom_count;
    let regenerate_seed_value = *regenerate_seed;
    let non_grid_custom_active = non_grid_custom_count_value.is_some();
    let total = (grid.cols * grid.rows) as usize;
    let grid_label = if puzzle_info_value.is_some() && !grid_choices.is_empty() {
        grid_choice_label(&grid)
    } else {
        "--".to_string()
    };
    let solve_time_label = if puzzle_info_value.is_some() {
        let pieces = grid.actual_count as f32;
        format_duration(SOLVE_TIME_FACTOR * pieces.powf(SOLVE_TIME_EXPONENT))
    } else {
        "--".to_string()
    };
    let grid_options: Html = grid_choices
        .iter()
        .enumerate()
        .map(|(index, choice)| {
            let label = grid_choice_label(choice);
            html! {
                <option
                    value={index.to_string()}
                    selected={!grid_custom_active && index == grid_index_value}
                >
                    {label}
                </option>
            }
        })
        .chain(std::iter::once(html! {
            <option value="custom" selected={grid_custom_active}>
                { "Custom\u{2026}" }
            </option>
        }))
        .collect();
    let initial_puzzle_art_index = app_core
        .snapshot()
        .puzzle_info
        .as_ref()
        .and_then(|info| match &info.image_ref {
            PuzzleImageRef::BuiltIn { slug } => puzzle_art_index_by_slug(slug),
            _ => None,
        })
        .unwrap_or(0);
    let puzzle_art_index = use_state(|| initial_puzzle_art_index);
    let puzzle_art_index_value = *puzzle_art_index;
    // Whether the "Custom…" puzzle-art option is selected. It reveals the
    // private-image upload controls; the snapshot observer keeps it in sync
    // with the live puzzle (a private image selects "Custom").
    let puzzle_art_custom = use_state(|| false);
    let puzzle_art_custom_value = *puzzle_art_custom;
    // Which blank test puzzle (if any) is selected. Blanks live outside the
    // catalog, so they can't be addressed by index; we track the slug instead.
    let initial_blank_slug = app_core
        .snapshot()
        .puzzle_info
        .as_ref()
        .and_then(|info| match &info.image_ref {
            PuzzleImageRef::BuiltIn { slug } => blank_puzzle_by_slug(slug).map(|entry| entry.slug),
            _ => None,
        });
    let puzzle_art_blank = use_state(|| initial_blank_slug);
    let puzzle_art_blank_value = *puzzle_art_blank;
    let puzzle_art = PUZZLE_ARTS
        .get(puzzle_art_index_value)
        .copied()
        .unwrap_or(PUZZLE_ARTS[0]);
    let puzzle_art_options: Html = PUZZLE_ARTS
        .iter()
        .enumerate()
        .map(|(index, art)| {
            html! {
                <option
                    value={index.to_string()}
                    selected={puzzle_art_blank_value.is_none()
                        && !puzzle_art_custom_value
                        && index == puzzle_art_index_value}
                >
                    {art.label}
                </option>
            }
        })
        .chain(BLANK_PUZZLES.iter().map(|art| {
            html! {
                <option value={art.slug} selected={puzzle_art_blank_value == Some(art.slug)}>
                    {art.label}
                </option>
            }
        }))
        .chain(std::iter::once(html! {
            <option value="custom" selected={puzzle_art_custom_value}>
                { "Custom\u{2026}" }
            </option>
        }))
        .collect();
    let private_label = use_state(|| String::new());
    let private_label_value = (*private_label).clone();
    let private_error = use_state(|| None::<String>);
    let private_error_value = (*private_error).clone();
    let private_status = use_state(|| None::<String>);
    let private_status_value = (*private_status).clone();
    let admin_private_error = use_state(|| None::<String>);
    let admin_private_error_value = (*admin_private_error).clone();
    let admin_private_status = use_state(|| AdminUploadStatus::Idle);
    let admin_private_status_value = *admin_private_status;
    let admin_private_status_note = use_state(|| None::<String>);
    let admin_private_status_note_value = (*admin_private_status_note).clone();
    let init_config = app_runtime::init_config();
    let multiplayer_config = use_state(|| init_config.multiplayer.clone());
    let multiplayer_config_value = (*multiplayer_config).clone();
    let online_setup = use_state(|| false);
    let online_setup_value = *online_setup;
    let room_id_draft = use_state(|| {
        init_config
            .multiplayer
            .as_ref()
            .map(|config| config.room_id.clone())
            .unwrap_or_else(generate_room_id)
    });
    let room_id_draft_value = (*room_id_draft).clone();
    let room_setup_status = use_state(|| RoomSetupStatus::Idle);
    let room_setup_status_value = *room_setup_status;
    let room_setup_error = use_state(|| None::<String>);
    let room_setup_error_value = (*room_setup_error).clone();
    let admin_token_input = use_state(|| load_admin_token().unwrap_or_default());
    let admin_token_input_value = (*admin_token_input).clone();
    let admin_token_active = use_state(|| load_admin_token().unwrap_or_default());
    let admin_token_active_value = (*admin_token_active).clone();
    let admin_status = use_state(|| AdminStatus::Idle);
    let admin_status_value = *admin_status;
    let admin_seed = use_state(|| String::new());
    let admin_seed_value = (*admin_seed).clone();
    let admin_socket = use_mut_ref(AdminSocket::new);
    let room_transition_seq = use_mut_ref(|| 0u64);
    let pending_created_room = use_mut_ref(|| None::<String>);
    let ws_delay_in = use_state(|| load_ws_delay_value(WS_DELAY_IN_KEY));
    let ws_delay_in_value = (*ws_delay_in).clone();
    let ws_delay_out = use_state(|| load_ws_delay_value(WS_DELAY_OUT_KEY));
    let ws_delay_out_value = (*ws_delay_out).clone();
    let ws_delay_jitter = use_state(|| load_ws_delay_value(WS_DELAY_JITTER_KEY));
    let ws_delay_jitter_value = (*ws_delay_jitter).clone();
    let puzzle_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_PUZZLE_KEY, true));
    let multiplayer_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_MULTIPLAYER_KEY, true));
    let graphics_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_GRAPHICS_KEY, false));
    let rules_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_RULES_KEY, false));
    let shaping_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_SHAPING_KEY, false));
    let sync_revision = use_state(|| 0u32);
    let boot_ready = use_state(|| matches!(boot_runtime::boot_state(), BootState::Ready));
    {
        let boot_ready = boot_ready.clone();
        use_effect(move || {
            let hook = Rc::new(move || {
                boot_ready.set(matches!(boot_runtime::boot_state(), BootState::Ready));
            });
            let id = boot_runtime::add_boot_state_hook(hook);
            move || {
                boot_runtime::remove_boot_state_hook(id);
            }
        });
    }
    let boot_ready_value = *boot_ready;
    let sync_view = sync_runtime::sync_view();
    let multiplayer_active = matches!(sync_view.mode(), InitMode::Online);
    let multiplayer_controls_online = multiplayer_active || online_setup_value;
    let show_online_setup = online_setup_value && !multiplayer_active;
    let mp_init_required_value = sync_view.init_required();
    // Admins may reconfigure the room through the same puzzle controls, so
    // they stay unlocked for them. Non-admin players in an initialized room
    // see the controls disabled (reflecting the room's current puzzle).
    let admin_enabled = multiplayer_active && matches!(admin_status_value, AdminStatus::Accepted);
    let lock_puzzle_controls =
        !boot_ready_value || (multiplayer_active && !mp_init_required_value && !admin_enabled);
    let _ = *sync_revision;
    let tab_width_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.tab_width = value.clamp(TAB_WIDTH_MIN, TAB_WIDTH_MAX);
        });
    let tab_depth_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.tab_depth = value.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
        });
    let tab_size_scale_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.tab_size_scale = value.clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
        });
    let tab_size_min_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            let max_allowed = settings
                .tab_size_max
                .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
            settings.tab_size_min = value.clamp(TAB_SIZE_MIN_LIMIT, max_allowed);
        });
    let tab_size_max_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            let min_allowed = settings
                .tab_size_min
                .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
            settings.tab_size_max = value.clamp(min_allowed, TAB_SIZE_MAX_LIMIT);
        });
    let skew_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.skew_range = value.clamp(0.0, SKEW_RANGE_MAX);
    });
    let jitter_strength_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.jitter_strength = value.clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
        });
    let jitter_len_bias_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.jitter_len_bias = value.clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);
        });
    let tab_depth_cap_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.tab_depth_cap = value.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
        });
    let curve_detail_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.curve_detail = value.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
        });
    let variation_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.variation = value.clamp(VARIATION_MIN, VARIATION_MAX);
        });
    let line_bend_input =
        on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
            settings.line_bend_ratio = value.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
        });
    let puzzle_state = use_state(PuzzleState::empty);
    let ui_revision = use_state(|| 0u32);
    let bump_ui_revision: Rc<dyn Fn()> = {
        let ui_revision = ui_revision.clone();
        Rc::new(move || {
            ui_revision.set(ui_revision.wrapping_add(1));
        })
    };
    let group_anchor = use_state(Vec::<usize>::new);
    let group_pos = use_state(Vec::<(f32, f32)>::new);
    let group_rot = use_state(Vec::<f32>::new);
    let group_order = use_state(Vec::<usize>::new);
    let positions_live = use_mut_ref(Vec::<(f32, f32)>::new);
    let rotations_live = use_mut_ref(Vec::<f32>::new);
    let group_pos_live = use_mut_ref(Vec::<(f32, f32)>::new);
    let group_rot_live = use_mut_ref(Vec::<f32>::new);
    let theme_mode = use_state(|| load_theme_mode().unwrap_or(ThemeMode::System));
    let theme_mode_value = *theme_mode;
    let theme_toggle_ref = use_node_ref();
    let workspace_padding_ratio = use_state(|| WORKSPACE_PADDING_RATIO_DEFAULT);
    let workspace_padding_ratio_value = *workspace_padding_ratio;
    let workspace_padding_label = puzzle_dims_value
        .map(|(width, height)| {
            let min_dim = width.min(height).max(1) as f32;
            let padding = min_dim * workspace_padding_ratio_value;
            format!(
                "{} (x{})",
                fmt_f32(padding),
                fmt_f32(workspace_padding_ratio_value)
            )
        })
        .unwrap_or_else(|| fmt_f32(workspace_padding_ratio_value));
    let view_controls_disabled = puzzle_dims_value.is_none();
    let z_order = use_state(Vec::<usize>::new);
    let rotation_enabled = use_state(|| true);
    let rotation_enabled_value = *rotation_enabled;
    let render_settings = use_state(initial_render_settings);
    let render_settings_value = (*render_settings).clone();
    let image_max_dim = render_settings_value
        .image_max_dim
        .clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
    // Resolve the active topology spec from UI state. Grid follows its
    // catalog-aware path (cols/rows from `grid`); every other topology
    // delegates to its `TopologyKind::resolve_target`, which builds a
    // spec from `(target_count, image_dims, seed)`.
    let local_topology_descriptor = if local_topology_is_grid {
        TopologySpec::grid(grid.cols, grid.rows)
    } else {
        let (w, h) = puzzle_dims_value.unwrap_or_else(|| {
            logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim)
        });
        let target = non_grid_custom_count_value.unwrap_or(non_grid_target_count_value);
        (active_topology_kind.resolve_target)(target, w, h, regenerate_seed_value)
            .map(|choice| choice.spec)
            .unwrap_or_else(|| TopologySpec::grid(grid.cols, grid.rows))
    };
    // Choices the non-grid picker offers (only iterated when non-grid is active).
    let non_grid_dims = puzzle_dims_value
        .unwrap_or_else(|| logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim));
    let non_grid_choices: Vec<PieceCountChoice> = if local_topology_is_grid {
        Vec::new()
    } else {
        (active_topology_kind.piece_count_choices)(non_grid_dims.0, non_grid_dims.1)
    };
    let non_grid_index = if non_grid_custom_active {
        None
    } else {
        non_grid_choices
            .iter()
            .position(|choice| choice.target_count == non_grid_target_count_value)
    };
    let non_grid_custom_input_value = non_grid_custom_count_value
        .map(|count| count.to_string())
        .unwrap_or_default();
    let non_grid_select_value = if non_grid_custom_active {
        "custom".to_string()
    } else {
        non_grid_index
            .map(|i| i.to_string())
            .unwrap_or_else(|| "custom".to_string())
    };
    let renderer_kind = render_settings_value.renderer;
    let svg_settings_value = render_settings_value.svg.clone();
    let wgpu_settings_value = render_settings_value.wgpu.clone();
    let svg_settings_visible = matches!(renderer_kind, RendererKind::Svg);
    {
        let app_core = app_core.clone();
        let app_snapshot = app_snapshot.clone();
        use_effect_with((), move |_| {
            let app_core_for_cb = app_core.clone();
            let subscription = app_core.subscribe(Rc::new(move || {
                app_snapshot.set(app_core_for_cb.snapshot());
            }));
            move || drop(subscription)
        });
    }

    let svg_animations_enabled = svg_settings_value.animations;
    let svg_emboss_enabled = svg_settings_value.emboss;
    let svg_fast_render = svg_settings_value.fast_render;
    let svg_fast_filter = svg_settings_value.fast_filter;
    let wgpu_show_fps = wgpu_settings_value.show_fps;
    let wgpu_edge_aa = wgpu_settings_value.edge_aa;
    let wgpu_render_scale = wgpu_settings_value.render_scale;
    let wgpu_rotate_anim = wgpu_settings_value.rotate_anim;
    let wgpu_rotate_anim_response = wgpu_settings_value.rotate_anim_response;
    let wgpu_rotate_anim_damping = wgpu_settings_value.rotate_anim_damping;
    let wgpu_shadow = wgpu_settings_value.shadow;
    let wgpu_shadow_distance = wgpu_settings_value.shadow_distance;
    let wgpu_shadow_radius = wgpu_settings_value.shadow_radius;
    let wgpu_shadow_darkness = wgpu_settings_value.shadow_darkness;
    let wgpu_flip_thickness_mm = wgpu_settings_value.flip_thickness_mm;
    let rotation_noise = use_state(|| ROTATION_NOISE_DEFAULT);
    let rotation_noise_value = *rotation_noise;
    let rotation_snap_tolerance = use_state(|| ROTATION_SNAP_TOLERANCE_DEFAULT_DEG);
    let rotation_snap_tolerance_value = *rotation_snap_tolerance;
    let rotation_lock_threshold = use_state(|| ROTATION_LOCK_THRESHOLD_DEFAULT);
    let rotation_lock_threshold_value = *rotation_lock_threshold;
    let snap_distance_ratio = use_state(|| SNAP_DISTANCE_RATIO_DEFAULT);
    let snap_distance_ratio_value = *snap_distance_ratio;
    let scramble_nonce = use_state(|| 0u32);
    let scramble_nonce_value = *scramble_nonce;
    let include_share_seed = use_state(|| false);
    let include_share_seed_value = *include_share_seed;
    let on_share_seed_toggle = {
        let include_share_seed = include_share_seed.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            include_share_seed.set(input.checked());
        })
    };
    let shareable_slug = if multiplayer_active {
        None
    } else {
        puzzle_info_value
            .as_ref()
            .and_then(|info| match &info.image_ref {
                PuzzleImageRef::BuiltIn { slug } => Some(slug.clone()),
                _ => None,
            })
    };
    let shareable_local = shareable_slug.is_some();
    let share_seed_value = if shareable_slug.is_some() && include_share_seed_value {
        Some(scramble_seed(
            PUZZLE_SEED,
            scramble_nonce_value,
            grid.cols as usize,
            grid.rows as usize,
        ))
    } else {
        None
    };
    let share_link_label = if multiplayer_active {
        "Room link"
    } else {
        "Share link"
    };
    let share_link = if let Some(config) = multiplayer_config_value.as_ref() {
        base_url_without_hash()
            .map(|base| {
                let room_id = encode_hash_value(&config.room_id);
                let fragment = format!("room={room_id}");
                format!("{base}#{fragment}")
            })
            .unwrap_or_default()
    } else if let Some(slug) = shareable_slug {
        base_url_without_hash()
            .map(|base| {
                let slug = encode_hash_value(&slug);
                let mut fragment = format!("puzzle={slug};pieces={}", grid.actual_count);
                if let Some(seed) = share_seed_value {
                    fragment.push_str(&format!(";seed={:#x}", seed));
                }
                format!("{base}#{fragment}")
            })
            .unwrap_or_default()
    } else if puzzle_info_value.is_none() {
        "--".to_string()
    } else {
        "Not shareable for private images".to_string()
    };
    let mp_room_label = sync_view
        .room_id()
        .map(|room_id| room_id.to_string())
        .or_else(|| {
            multiplayer_config_value
                .as_ref()
                .map(|config| config.room_id.clone())
        })
        .unwrap_or_else(|| "—".to_string());
    let mp_connection_label = if sync_view.connected() {
        "Connected"
    } else {
        "Connecting"
    };
    let admin_room_id = sync_view
        .room_id()
        .map(|room_id| room_id.to_string())
        .or_else(|| {
            multiplayer_config_value
                .as_ref()
                .map(|config| config.room_id.clone())
        });
    let room_id_draft_trimmed = room_id_draft_value.trim().to_string();
    let room_id_draft_valid = is_valid_room_id(&room_id_draft_trimmed);
    let next_room_draft = admin_room_id
        .clone()
        .or_else(|| {
            if room_id_draft_trimmed.is_empty() {
                None
            } else {
                Some(room_id_draft_trimmed.clone())
            }
        })
        .unwrap_or_else(generate_room_id);
    let room_setup_busy = matches!(
        room_setup_status_value,
        RoomSetupStatus::Creating | RoomSetupStatus::Connecting
    );
    let cancel_online_setup: Rc<dyn Fn()> = {
        let online_setup = online_setup.clone();
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        let admin_socket = admin_socket.clone();
        let room_transition_seq = room_transition_seq.clone();
        let pending_created_room = pending_created_room.clone();
        Rc::new(move || {
            {
                let mut seq = room_transition_seq.borrow_mut();
                *seq = seq.wrapping_add(1);
            }
            pending_created_room.borrow_mut().take();
            online_setup.set(false);
            room_setup_status.set(RoomSetupStatus::Idle);
            room_setup_error.set(None);
            admin_socket.borrow_mut().reset();
        })
    };
    let leave_online_room: Rc<dyn Fn(Option<String>)> = {
        let room_id_draft = room_id_draft.clone();
        let next_room_draft = next_room_draft.clone();
        let online_setup = online_setup.clone();
        let multiplayer_config = multiplayer_config.clone();
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        let admin_socket = admin_socket.clone();
        let room_transition_seq = room_transition_seq.clone();
        let pending_created_room = pending_created_room.clone();
        Rc::new(move |error: Option<String>| {
            let show_setup = error.is_some();
            {
                let mut seq = room_transition_seq.borrow_mut();
                *seq = seq.wrapping_add(1);
            }
            pending_created_room.borrow_mut().take();
            room_id_draft.set(next_room_draft.clone());
            online_setup.set(show_setup);
            multiplayer_config.set(None);
            admin_socket.borrow_mut().reset();
            app_router::clear_room_session();
            app_router::save_mode_preference(InitMode::Local);
            app_router::clear_location_hash();
            sync_runtime::init_from_config(None);
            room_setup_status.set(if error.is_some() {
                RoomSetupStatus::Failed
            } else {
                RoomSetupStatus::Idle
            });
            room_setup_error.set(error);
        })
    };
    let connect_room_live: Rc<dyn Fn(String)> = {
        let online_setup = online_setup.clone();
        let multiplayer_config = multiplayer_config.clone();
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        let room_transition_seq = room_transition_seq.clone();
        let leave_online_room = leave_online_room.clone();
        let pending_created_room = pending_created_room.clone();
        Rc::new(move |room_id: String| {
            {
                let mut seq = room_transition_seq.borrow_mut();
                *seq = seq.wrapping_add(1);
            }
            let seq = *room_transition_seq.borrow();
            pending_created_room.borrow_mut().take();
            room_setup_status.set(RoomSetupStatus::Connecting);
            room_setup_error.set(None);
            online_setup.set(false);
            multiplayer_config.set(Some(app_router::MultiplayerConfig {
                room_id: room_id.clone(),
                clear_hash: false,
                resumed: false,
            }));
            app_router::save_room_session(&room_id);
            app_router::save_mode_preference(InitMode::Online);
            sync_runtime::init_from_config(Some(app_router::MultiplayerConfig {
                room_id: room_id.clone(),
                clear_hash: false,
                resumed: false,
            }));
            let room_transition_seq = room_transition_seq.clone();
            let room_setup_status = room_setup_status.clone();
            let room_setup_error = room_setup_error.clone();
            let leave_online_room = leave_online_room.clone();
            spawn_local(async move {
                let mut waited_ms = 0u32;
                loop {
                    if *room_transition_seq.borrow() != seq {
                        return;
                    }
                    let view = sync_runtime::sync_view();
                    if matches!(view.mode(), InitMode::Online)
                        && view.connected()
                        && view.room_id() == Some(room_id.as_str())
                    {
                        room_setup_status.set(RoomSetupStatus::Idle);
                        room_setup_error.set(None);
                        return;
                    }
                    if waited_ms >= 5_000 {
                        if *room_transition_seq.borrow() == seq {
                            leave_online_room(Some(format!("failed to connect to room {room_id}")));
                        }
                        return;
                    }
                    TimeoutFuture::new(25).await;
                    waited_ms = waited_ms.saturating_add(25);
                }
            });
        })
    };
    {
        let admin_socket = admin_socket.clone();
        let admin_status = admin_status.clone();
        use_effect_with((), move |_| {
            admin_socket
                .borrow_mut()
                .set_status_hook(Rc::new(move |status| {
                    admin_status.set(status);
                }));
            || ()
        });
    }
    {
        let admin_socket = admin_socket.clone();
        let admin_private_status = admin_private_status.clone();
        let admin_private_status_note = admin_private_status_note.clone();
        let admin_private_error = admin_private_error.clone();
        use_effect_with((), move |_| {
            admin_socket
                .borrow_mut()
                .set_upload_status_hook(Rc::new(move |status, message| {
                    admin_private_status.set(status);
                    admin_private_status_note.set(message.clone());
                    if status == AdminUploadStatus::Failed {
                        if let Some(message) = message {
                            admin_private_error.set(Some(message));
                        }
                    } else if status == AdminUploadStatus::Done || status == AdminUploadStatus::Idle
                    {
                        admin_private_error.set(None);
                    }
                }));
            || ()
        });
    }
    {
        let admin_socket = admin_socket.clone();
        let connect_room_live = connect_room_live.clone();
        let pending_created_room = pending_created_room.clone();
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        use_effect_with((), move |_| {
            admin_socket
                .borrow_mut()
                .set_event_hook(Rc::new(move |event| {
                    let pending_room_id = pending_created_room.borrow().clone();
                    match event {
                        AdminSocketEvent::AdminAck => {
                            if let Some(room_id) = pending_room_id {
                                connect_room_live(room_id);
                            }
                        }
                        AdminSocketEvent::Error(message)
                        | AdminSocketEvent::ConnectionFailed(message) => {
                            if pending_room_id.is_some() {
                                pending_created_room.borrow_mut().take();
                                room_setup_status.set(RoomSetupStatus::Failed);
                                room_setup_error.set(Some(message));
                            }
                        }
                        AdminSocketEvent::Welcome => {}
                    }
                }));
            || ()
        });
    }
    {
        let admin_socket = admin_socket.clone();
        let admin_status = admin_status.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        let multiplayer_active = multiplayer_active;
        let online_setup_value = online_setup_value;
        use_effect_with(
            (
                admin_token_value.clone(),
                admin_room_id.clone(),
                multiplayer_active,
                online_setup_value,
            ),
            move |(token, room_id, active, setup_active)| {
                let cleanup = || ();
                if !*active {
                    if !*setup_active {
                        admin_socket.borrow_mut().reset();
                        admin_status.set(AdminStatus::Idle);
                    }
                    return cleanup;
                }
                let Some(room_id) = room_id.as_ref() else {
                    admin_socket.borrow_mut().reset();
                    admin_status.set(AdminStatus::Idle);
                    return cleanup;
                };
                let token = token.trim();
                if token.is_empty() {
                    admin_socket.borrow_mut().reset();
                    admin_status.set(AdminStatus::Idle);
                    return cleanup;
                }
                let Some(ws_base) = app_router::default_ws_base() else {
                    return cleanup;
                };
                admin_socket.borrow_mut().ensure_connected(
                    ws_base,
                    room_id.clone(),
                    token.to_string(),
                );
                cleanup
            },
        );
    }
    let admin_status_label = match admin_status_value {
        AdminStatus::Idle => "Admin token not verified",
        AdminStatus::Connecting => "Admin token: connecting...",
        AdminStatus::Accepted => "Admin token accepted",
        AdminStatus::Failed => "Admin token rejected",
    };
    let room_setup_status_text = room_setup_status_label(room_setup_status_value);
    let admin_private_status_label = match admin_private_status_value {
        AdminUploadStatus::Idle => "Idle",
        AdminUploadStatus::Reading => "Reading file...",
        AdminUploadStatus::Sending => "Sending upload...",
        AdminUploadStatus::AwaitingAck => "Waiting for server...",
        AdminUploadStatus::Done => "Upload complete",
        AdminUploadStatus::Failed => "Upload failed",
    };
    let admin_private_status_display = if let Some(note) = admin_private_status_note_value.clone() {
        format!("{admin_private_status_label} ({note})")
    } else {
        admin_private_status_label.to_string()
    };
    let admin_private_upload_busy = matches!(
        admin_private_status_value,
        AdminUploadStatus::Reading | AdminUploadStatus::Sending | AdminUploadStatus::AwaitingAck
    );
    let on_admin_private_file_input = {
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        let admin_seed_value = admin_seed_value.clone();
        let upload_descriptor = local_topology_descriptor.clone();
        let upload_shape_seed = regenerate_seed_value;
        let admin_private_error = admin_private_error.clone();
        let admin_private_status = admin_private_status.clone();
        let admin_private_status_note = admin_private_status_note.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let Some(files) = input.files() else {
                return;
            };
            let Some(file) = files.get(0) else {
                return;
            };
            input.set_value("");
            admin_private_error.set(None);
            admin_private_status.set(AdminUploadStatus::Reading);
            admin_private_status_note.set(None);
            let Some(room_id) = admin_room_id.as_ref() else {
                admin_private_error.set(Some("Missing room id".to_string()));
                admin_private_status.set(AdminUploadStatus::Failed);
                return;
            };
            let token = admin_token_value.trim();
            if token.is_empty() {
                admin_private_error.set(Some("Admin token required".to_string()));
                admin_private_status.set(AdminUploadStatus::Failed);
                return;
            }
            let Some(ws_base) = app_router::default_ws_base() else {
                admin_private_error.set(Some("Missing websocket base".to_string()));
                admin_private_status.set(AdminUploadStatus::Failed);
                return;
            };
            let mime = file.type_();
            if !mime.starts_with("image/") {
                admin_private_error.set(Some("Unsupported file type".to_string()));
                admin_private_status.set(AdminUploadStatus::Failed);
                return;
            }
            let size = file.size() as u64;
            if size == 0 || size > PRIVATE_UPLOAD_MAX_BYTES as u64 {
                admin_private_error.set(Some(format!(
                    "upload exceeds limit (max {} bytes)",
                    PRIVATE_UPLOAD_MAX_BYTES
                )));
                admin_private_status.set(AdminUploadStatus::Failed);
                return;
            }
            // Topology is fully resolved client-side and re-fit to the
            // uploaded image's real dimensions by the worker. `pieces` stays
            // `None` because the topology spec is authoritative.
            let pieces: Option<u32> = None;
            let seed = parse_optional_seed(&admin_seed_value);
            let topology = Some(upload_descriptor.clone().into());
            let shape_seed = Some(upload_shape_seed);
            let admin_socket = admin_socket.clone();
            let admin_private_error = admin_private_error.clone();
            let admin_private_status = admin_private_status.clone();
            let admin_private_status_note = admin_private_status_note.clone();
            let room_id = room_id.clone();
            let token = token.to_string();
            spawn_local(async move {
                let bytes = match read_file_bytes(file.clone()).await {
                    Ok(bytes) => bytes,
                    Err(message) => {
                        admin_private_error.set(Some(message));
                        admin_private_status.set(AdminUploadStatus::Failed);
                        return;
                    }
                };
                if bytes.is_empty() {
                    admin_private_error.set(Some("puzzle file is empty".to_string()));
                    admin_private_status.set(AdminUploadStatus::Failed);
                    return;
                }
                if bytes.len() > PRIVATE_UPLOAD_MAX_BYTES as usize {
                    admin_private_error.set(Some(format!(
                        "upload exceeds limit (max {} bytes)",
                        PRIVATE_UPLOAD_MAX_BYTES
                    )));
                    admin_private_status.set(AdminUploadStatus::Failed);
                    return;
                }
                admin_private_status.set(AdminUploadStatus::Sending);
                let mut messages = Vec::new();
                messages.push(AdminMsg::UploadPrivateBegin {
                    mime,
                    size: bytes.len() as u32,
                });
                for chunk in bytes.chunks(ASSET_CHUNK_BYTES) {
                    messages.push(AdminMsg::UploadPrivateChunk {
                        bytes: chunk.to_vec(),
                    });
                }
                messages.push(AdminMsg::UploadPrivateEnd {
                    pieces,
                    seed,
                    topology,
                    shape_seed,
                });
                admin_socket
                    .borrow_mut()
                    .send_upload(ws_base, room_id, token, messages);
                admin_private_status.set(AdminUploadStatus::AwaitingAck);
                admin_private_status_note.set(Some("waiting for server".to_string()));
            });
        })
    };
    let on_admin_scramble = {
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        let admin_seed_value = admin_seed_value.clone();
        Callback::from(move |_: MouseEvent| {
            let Some(room_id) = admin_room_id.as_ref() else {
                return;
            };
            let token = admin_token_value.trim();
            if token.is_empty() {
                return;
            }
            let Some(ws_base) = app_router::default_ws_base() else {
                return;
            };
            let seed = parse_optional_seed(&admin_seed_value);
            admin_socket.borrow_mut().send(
                ws_base,
                room_id.clone(),
                token.to_string(),
                AdminMsg::Scramble { seed },
            );
        })
    };
    let on_admin_solve = {
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        Callback::from(move |_: MouseEvent| {
            let Some(room_id) = admin_room_id.as_ref() else {
                return;
            };
            let token = admin_token_value.trim();
            if token.is_empty() {
                return;
            }
            let Some(ws_base) = app_router::default_ws_base() else {
                return;
            };
            admin_socket.borrow_mut().send(
                ws_base,
                room_id.clone(),
                token.to_string(),
                AdminMsg::Solve,
            );
        })
    };
    let save_revision = use_state(|| 0u32);
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(initial_show_controls);
    let show_controls_value = *show_controls && boot_ready_value;
    let menu_visible = use_state(|| false);
    let menu_visible_value = *menu_visible && boot_ready_value;
    let show_debug = use_state(|| false);
    let show_debug_value = *show_debug;
    let auto_pan_outer_ratio = use_state(|| AUTO_PAN_OUTER_RATIO_DEFAULT);
    let auto_pan_outer_ratio_value = *auto_pan_outer_ratio;
    let auto_pan_inner_ratio = use_state(|| AUTO_PAN_INNER_RATIO_DEFAULT);
    let auto_pan_inner_ratio_value = *auto_pan_inner_ratio;
    let auto_pan_speed_ratio = use_state(|| AUTO_PAN_SPEED_RATIO_DEFAULT);
    let auto_pan_speed_ratio_value = *auto_pan_speed_ratio;
    let app_snapshot_value = (*app_snapshot).clone();
    let bump_sync_revision: Rc<dyn Fn()> = {
        let sync_revision = sync_revision.clone();
        Rc::new(move || {
            sync_revision.set(sync_revision.wrapping_add(1));
        })
    };
    {
        let bump_sync_revision = bump_sync_revision.clone();
        use_effect(move || {
            let hook = sync_runtime::register_sync_view_hook(Rc::new(move || {
                bump_sync_revision();
            }));
            move || drop(hook)
        });
    }
    let on_remote_snapshot = {
        let bump_sync_revision = bump_sync_revision.clone();
        Rc::new(move |_snapshot: PlayableGameSnapshot, _seq: u64| {
            bump_sync_revision();
        })
    };
    let on_remote_update = {
        let bump_sync_revision = bump_sync_revision.clone();
        #[cfg(test)]
        let puzzle_info = puzzle_info_store.clone();
        Rc::new(
            move |_update: RoomControlUpdate,
                  _seq: u64,
                  _source: Option<ClientId>,
                  _client_seq: Option<u64>| {
                #[cfg(test)]
                {
                    if puzzle_info.get().is_none() {
                        record_mp_warn("puzzle info not ready");
                    }
                }
                bump_sync_revision();
            },
        )
    };
    let on_event = {
        let bump_sync_revision = bump_sync_revision.clone();
        Rc::new(move |_event: SyncEvent| {
            bump_sync_revision();
        })
    };
    {
        let app_core = app_core.clone();
        let puzzle_state = puzzle_state.clone();
        let group_anchor = group_anchor.clone();
        let group_pos = group_pos.clone();
        let group_rot = group_rot.clone();
        let group_order = group_order.clone();
        let z_order = z_order.clone();
        let scramble_nonce = scramble_nonce.clone();
        let solved = solved.clone();
        let bump_ui_revision = bump_ui_revision.clone();
        let puzzle_info = puzzle_info_store.clone();
        let puzzle_art_index = puzzle_art_index.clone();
        let puzzle_art_custom = puzzle_art_custom.clone();
        let puzzle_art_blank = puzzle_art_blank.clone();
        let grid_index = grid_index.clone();
        let grid_custom_count = grid_custom_count.clone();
        let local_topology = local_topology.clone();
        let non_grid_target_count = non_grid_target_count.clone();
        let non_grid_custom_count = non_grid_custom_count.clone();
        let regenerate_seed = regenerate_seed.clone();
        let save_revision = save_revision.clone();
        let positions_live = positions_live.clone();
        let rotations_live = rotations_live.clone();
        let group_pos_live = group_pos_live.clone();
        let group_rot_live = group_rot_live.clone();
        let app_snapshot = app_snapshot.clone();
        use_effect_with(multiplayer_active, move |_| {
            sync_runtime::set_local_observer(None);
            let on_snapshot = Rc::new(move |snapshot: crate::app_core::AppSnapshot| {
                app_snapshot.set(snapshot.clone());
                puzzle_info.set(snapshot.puzzle_info.clone());
                if let Some(info) = snapshot.puzzle_info.as_ref() {
                    // Mirror the engine's shape_seed so the Regenerate
                    // button bumps from the actual live value instead of
                    // a stale UI placeholder.
                    if *regenerate_seed != info.shape_seed {
                        regenerate_seed.set(info.shape_seed);
                    }
                    if let Some(game) = snapshot.game.as_ref() {
                        let topology = &game.playable.logical.topology;
                        let descriptor = topology.to_spec();
                        // Resolve the spec's tag through the registry so
                        // legacy aliases (e.g. `voronoi_canary`) collapse
                        // onto the current kind.
                        let resolved_tag = topology_kind_for_tag(&descriptor.tag)
                            .map(|kind| kind.tag.to_string())
                            .unwrap_or_else(|| "grid".to_string());
                        if *local_topology != resolved_tag {
                            local_topology.set(resolved_tag.clone());
                        }
                        // For non-grid topologies, sync the unified picker
                        // state from the live spec. Reverse-lookup the
                        // user-facing *target* (the hint) from the preset
                        // choices so the picker keeps showing the preset
                        // the user actually selected — `topology.piece_count()`
                        // returns the *actual* piece count, which often
                        // differs from any preset target and would force the
                        // picker to fall through to "Custom".
                        if resolved_tag != "grid" {
                            if let Some(kind) = topology_kind_for_tag(&resolved_tag).copied() {
                                let choices =
                                    (kind.piece_count_choices)(info.image_width, info.image_height);
                                let actual_count = topology.piece_count();
                                let current_target = *non_grid_target_count;
                                // Compare by `actual_count` — `target_count` is
                                // the user's hint and `spec` may include a seed
                                // that doesn't match the picker's canonical
                                // choice list (e.g. Voronoi). Prefer the
                                // currently selected target if it still
                                // resolves to this spec's piece count.
                                let matched = choices
                                    .iter()
                                    .find(|c| {
                                        c.target_count == current_target
                                            && c.actual_count == actual_count
                                    })
                                    .or_else(|| {
                                        choices.iter().find(|c| c.actual_count == actual_count)
                                    });
                                // Sticky custom: once the user has opened the
                                // custom input we keep it open (just reflect the
                                // live count) even if that count happens to equal
                                // a preset. Collapsing to a preset here is what
                                // made selecting "Custom" snap back to the list.
                                if non_grid_custom_count.is_some() {
                                    let custom = clamp_custom_piece_count(actual_count);
                                    if *non_grid_custom_count != Some(custom) {
                                        non_grid_custom_count.set(Some(custom));
                                    }
                                    if *non_grid_target_count != custom {
                                        non_grid_target_count.set(custom);
                                    }
                                } else if let Some(matched) = matched {
                                    if *non_grid_target_count != matched.target_count {
                                        non_grid_target_count.set(matched.target_count);
                                    }
                                } else {
                                    let custom = clamp_custom_piece_count(actual_count);
                                    non_grid_custom_count.set(Some(custom));
                                    if *non_grid_target_count != custom {
                                        non_grid_target_count.set(custom);
                                    }
                                }
                            }
                        }
                    }
                    // Reflect the room's image in the art picker: a built-in
                    // image selects its catalog entry (and leaves custom mode);
                    // a private image selects the "Custom…" option so its upload
                    // controls stay visible.
                    match &info.image_ref {
                        PuzzleImageRef::BuiltIn { slug } => {
                            if let Some(entry) = blank_puzzle_by_slug(slug) {
                                // A blank test puzzle: select its slug option and
                                // leave both custom and catalog-index modes.
                                if *puzzle_art_custom {
                                    puzzle_art_custom.set(false);
                                }
                                if *puzzle_art_blank != Some(entry.slug) {
                                    puzzle_art_blank.set(Some(entry.slug));
                                }
                            } else {
                                if puzzle_art_blank.is_some() {
                                    puzzle_art_blank.set(None);
                                }
                                if *puzzle_art_custom {
                                    puzzle_art_custom.set(false);
                                }
                                if let Some(index) = puzzle_art_index_by_slug(slug) {
                                    if *puzzle_art_index != index {
                                        puzzle_art_index.set(index);
                                    }
                                }
                            }
                        }
                        PuzzleImageRef::Private { .. } => {
                            if puzzle_art_blank.is_some() {
                                puzzle_art_blank.set(None);
                            }
                            if !*puzzle_art_custom {
                                puzzle_art_custom.set(true);
                            }
                        }
                    }
                    if info.image_width > 0 && info.image_height > 0 {
                        // The puzzle picker UI is intrinsically grid-only;
                        // the catalog only stores grid puzzles. We read the
                        // grid dims via the topology descriptor accessor so
                        // non-grid puzzles fall through gracefully.
                        if let Some((cols, rows)) = info.grid_dims() {
                            let mut choices =
                                build_grid_choices(info.image_width, info.image_height);
                            if choices.is_empty() {
                                choices.push(FALLBACK_GRID);
                            }
                            // Sticky custom (see the non-grid picker above):
                            // keep the custom input open once the user opened it.
                            if (*grid_custom_count).is_some() {
                                let actual_count = cols.saturating_mul(rows);
                                if actual_count > 0 && *grid_custom_count != Some(actual_count) {
                                    grid_custom_count.set(Some(actual_count));
                                }
                            } else if let Some(index) = grid_choice_index(&choices, cols, rows) {
                                if *grid_index != index {
                                    grid_index.set(index);
                                }
                            } else {
                                let actual_count = cols.saturating_mul(rows);
                                if actual_count > 0 {
                                    grid_custom_count.set(Some(actual_count));
                                }
                            }
                        }
                    }
                }
                // The legacy yew preview is grid-only: it expects a
                // row/col indexable connection table. For non-grid
                // topologies (triangular, future Voronoi) we just refresh
                // the bare snapshot fields and leave the detailed
                // `PuzzleState` empty.
                let is_grid = snapshot
                    .puzzle_info
                    .as_ref()
                    .map(|info| info.topology.tag.as_str())
                    == Some("grid");
                let (cols, rows) = snapshot
                    .puzzle_info
                    .as_ref()
                    .and_then(|info| info.grid_dims())
                    .map(|(c, r)| (c as usize, r as usize))
                    .unwrap_or((0, 0));
                let total = cols * rows;
                let positions = snapshot.piece_positions_px();
                let rotations = snapshot.piece_rotations_deg();
                let connections: Vec<[bool; 4]> = if is_grid {
                    snapshot
                        .game
                        .as_ref()
                        .map(|game| {
                            grid_piece_connections_from_playable(&game.playable, cols, rows)
                        })
                        .unwrap_or_default()
                } else {
                    Vec::new()
                };
                if !is_grid
                    || total == 0
                    || positions.len() != total
                    || rotations.len() != total
                    || snapshot.piece_flipped.len() != total
                    || connections.len() != total
                {
                    z_order.set(snapshot.z_order.clone());
                    scramble_nonce.set(snapshot.scramble_nonce);
                    solved.set(snapshot.solved);
                    puzzle_state.set(PuzzleState::empty());
                    bump_ui_revision();
                    return;
                }
                let piece_order = if snapshot.z_order.len() == total {
                    snapshot.z_order.clone()
                } else {
                    (0..total).collect()
                };
                let next_state = PuzzleState::rebuild_from_piece_state(
                    &positions,
                    &rotations,
                    &snapshot.piece_flipped,
                    &connections,
                    cols,
                    rows,
                    Some(piece_order.as_slice()),
                    snapshot.scramble_nonce,
                );
                let derived = derive_ui_state_from_puzzle(
                    &next_state,
                    cols,
                    snapshot.pose_unit_px[0],
                    snapshot.pose_unit_px[1],
                );
                group_anchor.set(derived.anchor_of.clone());
                group_pos.set(derived.group_pos.clone());
                group_rot.set(derived.group_rot.clone());
                group_order.set(derived.group_order.clone());
                z_order.set(derived.z_order.clone());
                scramble_nonce.set(snapshot.scramble_nonce);
                solved.set(snapshot.solved);
                puzzle_state.set(next_state);
                *positions_live.borrow_mut() = derived.positions;
                *rotations_live.borrow_mut() = derived.rotations;
                *group_pos_live.borrow_mut() = derived.group_pos;
                *group_rot_live.borrow_mut() = derived.group_rot;
                bump_ui_revision();
                save_revision.set(save_revision.wrapping_add(1));
            });
            let app_core_for_action = app_core.clone();
            sync_runtime::set_sync_hooks(SyncHooks {
                on_snapshot: on_snapshot.clone(),
                on_remote_action: Rc::new(move |action| {
                    app_core_for_action.apply_action(action);
                }),
                on_remote_snapshot: on_remote_snapshot.clone(),
                on_remote_update: on_remote_update.clone(),
                on_remote_playable_update: Rc::new(|_, _, _, _| {}),
                on_event: on_event.clone(),
                on_asset: Rc::new(|_| {}),
            });
            move || {
                sync_runtime::clear_sync_hooks();
            }
        });
    }
    // Legacy Yew renderer tick removed.
    // Legacy renderer sync dispatch removed.
    #[cfg(all(test, target_arch = "wasm32"))]
    {
        let send_msg = {
            let multiplayer_hooks = multiplayer_bridge::hooks_for_tests(app_core.clone());
            sync_runtime::install_test_handler(multiplayer_hooks)
        };
        let set_puzzle_info = {
            let puzzle_info = puzzle_info_store.clone();
            Rc::new(move |info: Option<PuzzleInfo>| {
                puzzle_info.set(info);
            })
        };
        let set_server_state_applied = {
            Rc::new(move |ready: bool| {
                sync_runtime::set_state_applied(ready);
            })
        };
        use_effect_with((), move |_| {
            gloo::console::log!("mp hooks set");
            set_mp_test_hooks(MpTestHooks {
                send_msg,
                set_puzzle_info,
                set_server_state_applied,
            });
            || {
                clear_mp_test_hooks();
            }
        });
    }
    {
        let multiplayer_config = multiplayer_config_value.clone();
        let bump_sync_revision = bump_sync_revision.clone();
        use_effect_with(multiplayer_config, move |config| {
            if let Some(config) = config.as_ref() {
                if config.clear_hash {
                    app_router::clear_location_hash();
                }
            }
            sync_runtime::set_state_applied(false);
            bump_sync_revision();
            || ()
        });
    }
    let renderer_value = match renderer_kind {
        RendererKind::Wgpu => "wgpu",
        RendererKind::Svg => "svg",
    };
    let mode_value = if multiplayer_controls_online {
        "online"
    } else {
        "local"
    };
    {
        let render_settings_value = render_settings_value.clone();
        let app_core = app_core.clone();
        use_effect_with(render_settings_value, move |settings| {
            app_router::save_render_settings(settings);
            app_core.set_renderer_kind(settings.renderer);
            view_runtime::apply_render_settings(settings);
            app_core.set_image_max_dim(settings.image_max_dim);
            || ()
        });
    }
    {
        let app_core = app_core.clone();
        use_effect_with(theme_mode_value, move |mode| {
            save_theme_mode(*mode);
            app_core.set_theme_mode(*mode);
            || ()
        });
    }
    let status_label = if solved_value {
        "Solved"
    } else {
        "In progress"
    };
    let status_class = if solved_value {
        "status status-solved"
    } else {
        "status"
    };
    let seed_label = if puzzle_info_value.is_some() {
        let cols = grid.cols as usize;
        let rows = grid.rows as usize;
        format!(
            "{:#x}",
            scramble_seed(PUZZLE_SEED, scramble_nonce_value, cols, rows)
        )
    } else {
        "--".to_string()
    };
    let (connections_label, border_connections_label) = if let Some(game) =
        app_snapshot_value.game.as_ref()
    {
        let connections_label = format_progress(
            game.playable.logical.active_edge_count(),
            game.playable.logical.edge_count(),
        );
        let border_connections_label = if game.playable.logical.topology.to_spec().tag == "grid" {
            let connections_value = grid_piece_connections_from_playable(
                &game.playable,
                grid.cols as usize,
                grid.rows as usize,
            );
            if connections_value.len() == total {
                let (connected, border_connected, total_expected, border_expected) =
                    count_connections(&connections_value, grid.cols as usize, grid.rows as usize);
                let _ = (connected, total_expected);
                format_progress(border_connected, border_expected)
            } else {
                "--".to_string()
            }
        } else {
            "--".to_string()
        };
        (connections_label, border_connections_label)
    } else {
        ("--".to_string(), "--".to_string())
    };
    // Single entry point for every shared puzzle control. In an initialized
    // online room an admin's change is sent as `ChangePuzzle` (carrying the
    // resolved topology + shape seed); local play, online setup (room not yet
    // created), and the NeedInit handshake all build the puzzle locally — the
    // handshake then ships that local snapshot to the server, and it already
    // carries the full topology.
    let apply_puzzle_change: Callback<PuzzleChangeRequest> = {
        let app_core = app_core.clone();
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_active_value = admin_token_active_value.clone();
        let admin_seed_value = admin_seed_value.clone();
        let multiplayer_active = multiplayer_active;
        let admin_enabled = admin_enabled;
        let mp_init_required_value = mp_init_required_value;
        let image_max_dim = image_max_dim;
        Callback::from(move |req: PuzzleChangeRequest| {
            if multiplayer_active && !mp_init_required_value {
                if !admin_enabled {
                    return;
                }
                let Some(room_id) = admin_room_id.as_ref() else {
                    return;
                };
                let token = admin_token_active_value.trim();
                if token.is_empty() {
                    return;
                }
                let Some(ws_base) = app_router::default_ws_base() else {
                    return;
                };
                // Re-fit the spec to the new image's aspect (see the local
                // path in `request_local_puzzle_change` for the rationale).
                let (logical_width, logical_height) =
                    logical_image_size(req.entry.width, req.entry.height, image_max_dim);
                let descriptor = topology_kind_for_tag(&req.descriptor.tag)
                    .map(|kind| {
                        (kind.rebuild_for_image)(&req.descriptor, logical_width, logical_height)
                    })
                    .unwrap_or_else(|| req.descriptor.clone());
                admin_socket.borrow_mut().send(
                    ws_base,
                    room_id.clone(),
                    token.to_string(),
                    AdminMsg::ChangePuzzle {
                        puzzle: PuzzleSpec {
                            image_ref: PuzzleImageRef::BuiltIn {
                                slug: req.entry.slug.to_string(),
                            },
                            pieces: None,
                            seed: parse_optional_seed(&admin_seed_value),
                            topology: Some(descriptor.into()),
                            shape_seed: Some(req.shape_seed),
                        },
                    },
                );
                return;
            }
            request_local_puzzle_change(
                app_core.clone(),
                image_max_dim,
                req.entry,
                req.grid_override,
                req.descriptor,
                req.shape_seed,
            );
        })
    };
    let on_grid_change = {
        let grid_index = grid_index.clone();
        let grid_custom_count = grid_custom_count.clone();
        let grid_choices = grid_choices.clone();
        let grid_choices_len = grid_choices.len();
        let lock_puzzle_controls = lock_puzzle_controls;
        let apply_puzzle_change = apply_puzzle_change.clone();
        let puzzle_art = puzzle_art;
        let preset_grid = preset_grid;
        let regenerate_seed_value = regenerate_seed_value;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            let raw = select.value();
            if raw == "custom" {
                // Reveal the free-entry input, pre-filled with the current
                // count. The change applies on input commit; applying here
                // would re-scramble the board and snap the picker back.
                let initial = clamp_custom_piece_count(preset_grid.target_count.max(1));
                grid_custom_count.set(Some(initial));
                return;
            }
            if let Ok(value) = raw.parse::<usize>() {
                if value < grid_choices_len {
                    grid_custom_count.set(None);
                    grid_index.set(value);
                    clear_saved_game();
                    if let Some(grid) = grid_choices.get(value).copied() {
                        apply_puzzle_change.emit(PuzzleChangeRequest {
                            entry: puzzle_art,
                            grid_override: Some(grid),
                            descriptor: TopologySpec::grid(grid.cols, grid.rows),
                            shape_seed: regenerate_seed_value,
                        });
                    }
                }
            }
        })
    };
    let on_grid_custom_commit = {
        let grid_custom_count = grid_custom_count.clone();
        let lock_puzzle_controls = lock_puzzle_controls;
        let apply_puzzle_change = apply_puzzle_change.clone();
        let puzzle_art = puzzle_art;
        let preset_grid = preset_grid;
        let puzzle_dims_value = puzzle_dims_value;
        let current_custom = grid_custom_count_value;
        let regenerate_seed_value = regenerate_seed_value;
        Callback::from(move |input: HtmlInputElement| {
            if lock_puzzle_controls {
                return;
            }
            let raw = input.value();
            let parsed = raw.trim().parse::<u32>().ok();
            let next = parsed
                .map(clamp_custom_piece_count)
                .or(current_custom)
                .unwrap_or(clamp_custom_piece_count(preset_grid.target_count.max(1)));
            input.set_value(&next.to_string());
            if current_custom == Some(next) {
                return;
            }
            grid_custom_count.set(Some(next));
            clear_saved_game();
            if let Some((width, height)) = puzzle_dims_value {
                let grid = nearest_valid_grid(width, height, next).unwrap_or(preset_grid);
                apply_puzzle_change.emit(PuzzleChangeRequest {
                    entry: puzzle_art,
                    grid_override: Some(grid),
                    descriptor: TopologySpec::grid(grid.cols, grid.rows),
                    shape_seed: regenerate_seed_value,
                });
            }
        })
    };
    let on_grid_custom_blur = {
        let on_grid_custom_commit = on_grid_custom_commit.clone();
        Callback::from(move |event: FocusEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            on_grid_custom_commit.emit(input);
        })
    };
    let on_grid_custom_keydown = {
        let on_grid_custom_commit = on_grid_custom_commit.clone();
        Callback::from(move |event: KeyboardEvent| {
            if event.key() == "Enter" {
                event.prevent_default();
                let input: HtmlInputElement = event.target_unchecked_into();
                on_grid_custom_commit.emit(input.clone());
                let _ = input.blur();
            }
        })
    };
    let on_local_topology_change = {
        let local_topology = local_topology.clone();
        let non_grid_target_count = non_grid_target_count.clone();
        let non_grid_custom_count = non_grid_custom_count.clone();
        let apply_puzzle_change = apply_puzzle_change.clone();
        let image_max_dim = image_max_dim;
        let puzzle_art = puzzle_art;
        let grid = grid;
        let regenerate_seed_value = regenerate_seed_value;
        let lock_puzzle_controls = lock_puzzle_controls;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            let value = select.value();
            let kind = topology_kind_for_tag(&value)
                .copied()
                .unwrap_or(available_topologies()[0]);
            let descriptor = if value == "grid" {
                TopologySpec::grid(grid.cols, grid.rows)
            } else {
                let (w, h) = logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim);
                // Default to the topology's default target if we're
                // switching from grid; otherwise keep the current
                // non-grid target so the picker doesn't reset.
                let target = if *local_topology == "grid" {
                    non_grid_custom_count
                        .as_ref()
                        .map(|x| *x)
                        .unwrap_or(kind.default_target_count)
                } else {
                    non_grid_custom_count
                        .as_ref()
                        .map(|x| *x)
                        .unwrap_or(*non_grid_target_count)
                };
                (kind.resolve_target)(target, w, h, regenerate_seed_value)
                    .map(|choice| {
                        // Persist the resolved target so subsequent
                        // re-renders show the right `<option>` selected.
                        non_grid_target_count.set(choice.target_count);
                        choice.spec
                    })
                    .unwrap_or_else(|| TopologySpec::grid(grid.cols, grid.rows))
            };
            local_topology.set(value);
            clear_saved_game();
            let grid_override = if descriptor.tag == "grid" {
                Some(grid)
            } else {
                None
            };
            apply_puzzle_change.emit(PuzzleChangeRequest {
                entry: puzzle_art,
                grid_override,
                descriptor,
                shape_seed: regenerate_seed_value,
            });
        })
    };
    // Unified non-grid piece-count handlers. The `<select>` (preset
    // counts) and the custom `<input>` both feed `non_grid_target_count`
    // / `non_grid_custom_count`; the active topology kind's
    // `resolve_target` produces the actual spec.
    let on_non_grid_count_change = {
        let non_grid_target_count = non_grid_target_count.clone();
        let non_grid_custom_count = non_grid_custom_count.clone();
        let apply_puzzle_change = apply_puzzle_change.clone();
        let image_max_dim = image_max_dim;
        let puzzle_art = puzzle_art;
        let active_kind = active_topology_kind;
        let regenerate_seed_value = regenerate_seed_value;
        let lock_puzzle_controls = lock_puzzle_controls;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            let value = select.value();
            if value == "custom" {
                // Just reveal the free-entry input, pre-filled with the current
                // count. Applying here would re-scramble the board for an
                // unchanged count and let the snapshot observer snap the picker
                // back to a preset. The actual change happens on input commit.
                let initial = clamp_custom_piece_count(*non_grid_target_count);
                non_grid_custom_count.set(Some(initial));
                return;
            }
            let Ok(index) = value.parse::<usize>() else {
                return;
            };
            let (w, h) = logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim);
            let choices = (active_kind.piece_count_choices)(w, h);
            let Some(choice) = choices.get(index) else {
                return;
            };
            non_grid_custom_count.set(None);
            non_grid_target_count.set(choice.target_count);
            clear_saved_game();
            apply_puzzle_change.emit(PuzzleChangeRequest {
                entry: puzzle_art,
                grid_override: None,
                descriptor: choice.spec.clone(),
                shape_seed: regenerate_seed_value,
            });
        })
    };
    let on_non_grid_custom_commit = {
        let non_grid_target_count = non_grid_target_count.clone();
        let non_grid_custom_count = non_grid_custom_count.clone();
        let apply_puzzle_change = apply_puzzle_change.clone();
        let image_max_dim = image_max_dim;
        let puzzle_art = puzzle_art;
        let active_kind = active_topology_kind;
        let regenerate_seed_value = regenerate_seed_value;
        let lock_puzzle_controls = lock_puzzle_controls;
        Callback::from(move |input: HtmlInputElement| {
            if lock_puzzle_controls {
                return;
            }
            let next = input
                .value()
                .trim()
                .parse::<u32>()
                .map(clamp_custom_piece_count)
                .unwrap_or_else(|_| active_kind.default_target_count);
            input.set_value(&next.to_string());
            non_grid_custom_count.set(Some(next));
            non_grid_target_count.set(next);
            clear_saved_game();
            let (w, h) = logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim);
            if let Some(choice) = (active_kind.resolve_target)(next, w, h, regenerate_seed_value) {
                apply_puzzle_change.emit(PuzzleChangeRequest {
                    entry: puzzle_art,
                    grid_override: None,
                    descriptor: choice.spec,
                    shape_seed: regenerate_seed_value,
                });
            }
        })
    };
    let on_non_grid_custom_blur = {
        let on_non_grid_custom_commit = on_non_grid_custom_commit.clone();
        Callback::from(move |event: FocusEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            on_non_grid_custom_commit.emit(input);
        })
    };
    let on_non_grid_custom_keydown = {
        let on_non_grid_custom_commit = on_non_grid_custom_commit.clone();
        Callback::from(move |event: KeyboardEvent| {
            if event.key() == "Enter" {
                event.prevent_default();
                let input: HtmlInputElement = event.target_unchecked_into();
                on_non_grid_custom_commit.emit(input.clone());
                let _ = input.blur();
            }
        })
    };
    let on_regenerate_click = {
        let regenerate_seed = regenerate_seed.clone();
        let non_grid_target_count = non_grid_target_count.clone();
        let non_grid_custom_count = non_grid_custom_count.clone();
        let apply_puzzle_change = apply_puzzle_change.clone();
        let image_max_dim = image_max_dim;
        let puzzle_art = puzzle_art;
        let active_kind = active_topology_kind;
        let grid = grid;
        let lock_puzzle_controls = lock_puzzle_controls;
        Callback::from(move |_: MouseEvent| {
            if lock_puzzle_controls {
                return;
            }
            // Bump the seed. Every topology consumes it via
            // `info.shape_seed` (tab/blank directions); Voronoi also
            // bakes it into its spec so its layout regenerates too.
            let next_seed = (*regenerate_seed).wrapping_add(1).max(1);
            regenerate_seed.set(next_seed);
            clear_saved_game();
            if active_kind.tag == "grid" {
                apply_puzzle_change.emit(PuzzleChangeRequest {
                    entry: puzzle_art,
                    grid_override: Some(grid),
                    descriptor: TopologySpec::grid(grid.cols, grid.rows),
                    shape_seed: next_seed,
                });
                return;
            }
            let target = (*non_grid_custom_count).unwrap_or(*non_grid_target_count);
            let (w, h) = logical_image_size(puzzle_art.width, puzzle_art.height, image_max_dim);
            if let Some(choice) = (active_kind.resolve_target)(target, w, h, next_seed) {
                apply_puzzle_change.emit(PuzzleChangeRequest {
                    entry: puzzle_art,
                    grid_override: None,
                    descriptor: choice.spec,
                    shape_seed: next_seed,
                });
            }
        })
    };
    let on_puzzle_art_change = {
        let puzzle_art_index = puzzle_art_index.clone();
        let puzzle_art_custom = puzzle_art_custom.clone();
        let puzzle_art_blank = puzzle_art_blank.clone();
        let puzzle_art_len = PUZZLE_ARTS.len();
        let lock_puzzle_controls = lock_puzzle_controls;
        let apply_puzzle_change = apply_puzzle_change.clone();
        let descriptor = local_topology_descriptor.clone();
        let grid = grid;
        let regenerate_seed_value = regenerate_seed_value;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            let raw = select.value();
            if raw == "custom" {
                // Reveal the private-image upload controls. The puzzle only
                // changes once a file is actually uploaded.
                puzzle_art_blank.set(None);
                puzzle_art_custom.set(true);
                return;
            }
            // Blank test puzzles are addressed by slug, not catalog index.
            if let Some(entry) = blank_puzzle_by_slug(&raw) {
                puzzle_art_custom.set(false);
                puzzle_art_blank.set(Some(entry.slug));
                clear_saved_game();
                let grid_override = if descriptor.tag == "grid" {
                    Some(grid)
                } else {
                    None
                };
                apply_puzzle_change.emit(PuzzleChangeRequest {
                    entry: *entry,
                    grid_override,
                    descriptor: descriptor.clone(),
                    shape_seed: regenerate_seed_value,
                });
                return;
            }
            if let Ok(value) = raw.parse::<usize>() {
                if value < puzzle_art_len {
                    puzzle_art_custom.set(false);
                    puzzle_art_blank.set(None);
                    puzzle_art_index.set(value);
                    clear_saved_game();
                    let entry = PUZZLE_ARTS.get(value).copied().unwrap_or(PUZZLE_ARTS[0]);
                    let grid_override = if descriptor.tag == "grid" {
                        Some(grid)
                    } else {
                        None
                    };
                    apply_puzzle_change.emit(PuzzleChangeRequest {
                        entry,
                        grid_override,
                        descriptor: descriptor.clone(),
                        shape_seed: regenerate_seed_value,
                    });
                }
            }
        })
    };
    let on_private_label_input = {
        let private_label = private_label.clone();
        let private_error = private_error.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            private_label.set(input.value());
            private_error.set(None);
        })
    };
    let private_upload_busy = private_status_value.is_some();
    let on_private_file_input = {
        let private_label = private_label.clone();
        let private_error = private_error.clone();
        let private_status = private_status.clone();
        let app_core = app_core.clone();
        let image_max_dim = image_max_dim;
        let descriptor = local_topology_descriptor.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let Some(files) = input.files() else {
                return;
            };
            let Some(file) = files.get(0) else {
                return;
            };
            private_error.set(None);
            private_status.set(None);
            let mime = file.type_();
            if !mime.starts_with("image/") {
                private_error.set(Some("Unsupported file type".to_string()));
                return;
            }
            let label = (*private_label).trim().to_string();
            let app_core = app_core.clone();
            let private_error = private_error.clone();
            let private_status = private_status.clone();
            let descriptor = descriptor.clone();
            spawn_local(async move {
                private_status.set(Some("Reading file...".to_string()));
                let bytes = match read_file_bytes(file.clone()).await {
                    Ok(bytes) => bytes,
                    Err(message) => {
                        private_error.set(Some(message));
                        private_status.set(None);
                        return;
                    }
                };
                private_status.set(Some("Processing image...".to_string()));
                TimeoutFuture::new(0).await;
                let is_avif = mime == "image/avif" || is_avif_bytes(&bytes);
                let (stored_bytes, width, height) = if is_avif {
                    let (width, height) = match load_image_dimensions(file).await {
                        Ok(value) => value,
                        Err(message) => {
                            private_error.set(Some(message));
                            private_status.set(None);
                            return;
                        }
                    };
                    (bytes, width, height)
                } else {
                    let mut config = PipelineConfig::default();
                    config.alpha_mode = AlphaMode::Preserve;
                    let result = match image_pipeline::transcode_to_avif(&bytes, config) {
                        Ok(result) => result,
                        Err(message) => {
                            private_error.set(Some(format!("transcode failed: {message}")));
                            private_status.set(None);
                            return;
                        }
                    };
                    (result.bytes, result.width, result.height)
                };
                let (logical_width, logical_height) =
                    logical_image_size(width, height, image_max_dim);
                // The descriptor was built against the previously-active
                // image's dimensions. Each topology decides whether its
                // identity depends on image aspect (Voronoi, hexagonal and
                // triangular re-fit their layout to the new aspect; grid
                // passes through unchanged).
                let descriptor = topology_kind_for_tag(&descriptor.tag)
                    .map(|kind| {
                        (kind.rebuild_for_image)(&descriptor, logical_width, logical_height)
                    })
                    .unwrap_or(descriptor.clone());
                let hash = sha256_hex(&stored_bytes);
                let now = now_ms_u64();
                let size = (stored_bytes.len() as u64).min(u32::MAX as u64) as u32;
                let entry = PrivateImageEntry {
                    bytes: stored_bytes,
                    mime: "image/avif".to_string(),
                    width: logical_width,
                    height: logical_height,
                    size,
                    created_at: now,
                    last_used_at: now,
                };
                if let Err(message) = persisted_store::save_private_image(&hash, entry).await {
                    private_error.set(Some(message));
                    private_status.set(None);
                    return;
                }
                let refs = match persisted_store::load_private_image_refs(LOCAL_PRIVATE_SCOPE).await
                {
                    Ok(Some(mut refs)) => {
                        if !refs.hashes.iter().any(|value| value == &hash) {
                            refs.hashes.push(hash.clone());
                        }
                        refs.updated_at = now;
                        refs
                    }
                    Ok(None) => PrivateImageRefs {
                        hashes: vec![hash.clone()],
                        updated_at: now,
                    },
                    Err(message) => {
                        private_error.set(Some(message));
                        private_status.set(None);
                        return;
                    }
                };
                if let Err(message) =
                    persisted_store::save_private_image_refs(LOCAL_PRIVATE_SCOPE, refs).await
                {
                    private_error.set(Some(message));
                    private_status.set(None);
                    return;
                }
                clear_saved_game();
                let image_ref = PuzzleImageRef::Private { hash };
                app_core.set_puzzle_with_topology(
                    label,
                    image_ref,
                    (logical_width, logical_height),
                    descriptor.clone(),
                    None,
                );
                private_status.set(None);
            });
        })
    };
    let on_admin_token_input = {
        let admin_token_input = admin_token_input.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            admin_token_input.set(input.value());
        })
    };
    let on_admin_token_apply = {
        let admin_token_input = admin_token_input.clone();
        let admin_token_active = admin_token_active.clone();
        let admin_status = admin_status.clone();
        let admin_socket = admin_socket.clone();
        Callback::from(move |_: MouseEvent| {
            let value = (*admin_token_input).clone();
            let trimmed = value.trim().to_string();
            save_admin_token(&value);
            admin_token_active.set(trimmed);
            admin_socket.borrow_mut().reset();
            admin_status.set(AdminStatus::Idle);
        })
    };
    let on_room_id_draft_input = {
        let room_id_draft = room_id_draft.clone();
        let room_setup_error = room_setup_error.clone();
        let room_setup_status = room_setup_status.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            room_id_draft.set(input.value());
            room_setup_error.set(None);
            if *room_setup_status == RoomSetupStatus::Failed {
                room_setup_status.set(RoomSetupStatus::Idle);
            }
        })
    };
    let on_admin_seed_input = {
        let admin_seed = admin_seed.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            admin_seed.set(input.value());
        })
    };
    let on_ws_delay_in_input = {
        let ws_delay_in = ws_delay_in.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let value = input.value();
            ws_delay_in.set(value.clone());
            save_ws_delay_value(WS_DELAY_IN_KEY, &value);
        })
    };
    let on_ws_delay_out_input = {
        let ws_delay_out = ws_delay_out.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let value = input.value();
            ws_delay_out.set(value.clone());
            save_ws_delay_value(WS_DELAY_OUT_KEY, &value);
        })
    };
    let on_ws_delay_jitter_input = {
        let ws_delay_jitter = ws_delay_jitter.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let value = input.value();
            ws_delay_jitter.set(value.clone());
            save_ws_delay_value(WS_DELAY_JITTER_KEY, &value);
        })
    };
    let on_identity_reset = Callback::from(move |_: MouseEvent| {
        spawn_local(async {
            if let Err(err) = multiplayer_identity::reset_identity().await {
                gloo::console::warn!("failed to reset identity", err);
            }
        });
    });
    let on_workspace_padding_ratio = {
        let workspace_padding_ratio = workspace_padding_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WORKSPACE_PADDING_RATIO_MIN, WORKSPACE_PADDING_RATIO_MAX);
                workspace_padding_ratio.set(value);
                app_core.set_workspace_padding_ratio(value);
            }
        })
    };
    let on_zoom_in = {
        let app_core = app_core.clone();
        let app_snapshot = app_snapshot.clone();
        Callback::from(move |_: MouseEvent| {
            let view = (*app_snapshot).view;
            let center_x = view.min_x + view.width * 0.5;
            let center_y = view.min_y + view.height * 0.5;
            app_core.zoom_view_at(1.1, center_x, center_y);
        })
    };
    let on_zoom_out = {
        let app_core = app_core.clone();
        let app_snapshot = app_snapshot.clone();
        Callback::from(move |_: MouseEvent| {
            let view = (*app_snapshot).view;
            let center_x = view.min_x + view.width * 0.5;
            let center_y = view.min_y + view.height * 0.5;
            app_core.zoom_view_at(1.0 / 1.1, center_x, center_y);
        })
    };
    let on_fit_workspace = {
        let app_core = app_core.clone();
        Callback::from(move |_: MouseEvent| {
            app_core.reset_view_to_fit();
        })
    };
    let on_fit_frame = {
        let app_core = app_core.clone();
        Callback::from(move |_: MouseEvent| {
            app_core.fit_view_to_frame();
        })
    };
    let on_frame_snap = {
        let frame_snap_ratio = frame_snap_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(FRAME_SNAP_MIN, FRAME_SNAP_MAX);
                frame_snap_ratio.set(value);
                app_core.set_frame_snap_ratio(value);
            }
        })
    };
    let on_snap_distance = {
        let snap_distance_ratio = snap_distance_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(SNAP_DISTANCE_RATIO_MIN, SNAP_DISTANCE_RATIO_MAX);
                snap_distance_ratio.set(value);
                app_core.set_snap_distance_ratio(value);
            }
        })
    };
    let on_rotation_toggle = {
        let rotation_enabled = rotation_enabled.clone();
        let save_revision = save_revision.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            rotation_enabled.set(enabled);
            app_core.set_rotation_enabled(enabled);
            if !enabled {
                // Match the prior behavior: disabling rotation zeros every
                // group's rotation in addition to changing the rule.
                app_core.clear_all_group_rotations();
            }
            save_revision.set(save_revision.wrapping_add(1));
        })
    };
    let on_rotation_noise = {
        let rotation_noise = rotation_noise.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                rotation_noise.set(value.clamp(ROTATION_NOISE_MIN, ROTATION_NOISE_MAX));
            }
        })
    };
    let on_rotation_snap_tolerance = {
        let rotation_snap_tolerance = rotation_snap_tolerance.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(
                    ROTATION_SNAP_TOLERANCE_MIN_DEG,
                    ROTATION_SNAP_TOLERANCE_MAX_DEG,
                );
                rotation_snap_tolerance.set(value);
                app_core.set_rotation_snap_tolerance(value);
            }
        })
    };
    let on_rotation_lock_threshold = {
        let rotation_lock_threshold = rotation_lock_threshold.clone();
        let app_snapshot = app_snapshot.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let snapshot = &*app_snapshot;
                let max_value = snapshot
                    .piece_world_poses
                    .len()
                    .max(ROTATION_LOCK_THRESHOLD_MIN);
                let rounded = value.round() as usize;
                let clamped = rounded.max(ROTATION_LOCK_THRESHOLD_MIN).min(max_value);
                rotation_lock_threshold.set(clamped);
            }
        })
    };
    let on_create_room = {
        let admin_token_input = admin_token_input.clone();
        let admin_token_active = admin_token_active.clone();
        let admin_status = admin_status.clone();
        let admin_socket = admin_socket.clone();
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        let pending_created_room = pending_created_room.clone();
        let room_id_draft_trimmed = room_id_draft_trimmed.clone();
        let room_id_draft_valid = room_id_draft_valid;
        let admin_seed_value = admin_seed_value.clone();
        let puzzle_art = puzzle_art;
        let create_descriptor = local_topology_descriptor.clone();
        let regenerate_seed_value = regenerate_seed_value;
        Callback::from(move |_: MouseEvent| {
            let room_id = room_id_draft_trimmed.trim().to_string();
            if !room_id_draft_valid {
                room_setup_status.set(RoomSetupStatus::Failed);
                room_setup_error.set(Some(format!(
                    "room id must be {ROOM_ID_LEN} alphanumeric characters"
                )));
                return;
            }
            let token_value = (*admin_token_input).clone();
            let token = token_value.trim().to_string();
            if token.is_empty() {
                room_setup_status.set(RoomSetupStatus::Failed);
                room_setup_error.set(Some("admin token required to create a room".to_string()));
                return;
            }
            let Some(ws_base) = app_router::default_ws_base() else {
                room_setup_status.set(RoomSetupStatus::Failed);
                room_setup_error.set(Some("Missing websocket base".to_string()));
                return;
            };
            save_admin_token(&token_value);
            admin_token_active.set(token.clone());
            admin_socket.borrow_mut().reset();
            admin_status.set(AdminStatus::Idle);
            pending_created_room.borrow_mut().replace(room_id.clone());
            room_setup_status.set(RoomSetupStatus::Creating);
            room_setup_error.set(None);
            let seed = parse_optional_seed(&admin_seed_value);
            admin_socket.borrow_mut().send(
                ws_base,
                room_id,
                token,
                AdminMsg::Create {
                    persistence: heddobureika_core::RoomPersistence::Durable,
                    puzzle: PuzzleSpec {
                        image_ref: PuzzleImageRef::BuiltIn {
                            slug: puzzle_art.slug.to_string(),
                        },
                        pieces: None,
                        seed,
                        topology: Some(create_descriptor.clone().into()),
                        shape_seed: Some(regenerate_seed_value),
                    },
                },
            );
        })
    };
    let on_join_room = {
        let room_setup_status = room_setup_status.clone();
        let room_setup_error = room_setup_error.clone();
        let connect_room_live = connect_room_live.clone();
        let room_id_draft_trimmed = room_id_draft_trimmed.clone();
        let room_id_draft_valid = room_id_draft_valid;
        Callback::from(move |_: MouseEvent| {
            let room_id = room_id_draft_trimmed.trim().to_string();
            if !room_id_draft_valid {
                room_setup_status.set(RoomSetupStatus::Failed);
                room_setup_error.set(Some(format!(
                    "room id must be {ROOM_ID_LEN} alphanumeric characters"
                )));
                return;
            }
            connect_room_live(room_id);
        })
    };
    let on_mode_change = {
        let multiplayer_active = multiplayer_active;
        let show_online_setup = show_online_setup;
        let online_setup = online_setup.clone();
        let room_id_draft = room_id_draft.clone();
        let cancel_online_setup = cancel_online_setup.clone();
        let leave_online_room = leave_online_room.clone();
        Callback::from(move |event: Event| {
            let input: HtmlSelectElement = event.target_unchecked_into();
            let next_mode = match input.value().as_str() {
                "online" => InitMode::Online,
                "local" => InitMode::Local,
                _ => InitMode::Local,
            };
            if show_online_setup {
                if matches!(next_mode, InitMode::Online) {
                    return;
                }
                cancel_online_setup();
                return;
            }
            let current_mode = if multiplayer_active {
                InitMode::Online
            } else {
                InitMode::Local
            };
            if next_mode == current_mode {
                return;
            }
            if matches!(next_mode, InitMode::Local) {
                if multiplayer_active {
                    leave_online_room(None);
                } else {
                    cancel_online_setup();
                }
                return;
            }
            if (*room_id_draft).trim().is_empty() {
                room_id_draft.set(generate_room_id());
            }
            online_setup.set(true);
        })
    };
    let on_leave_room = {
        let leave_online_room = leave_online_room.clone();
        Callback::from(move |_| {
            leave_online_room(None);
        })
    };
    let on_renderer_change = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlSelectElement = event.target_unchecked_into();
            let next_renderer = match input.value().as_str() {
                "svg" => RendererKind::Svg,
                "wgpu" => RendererKind::Wgpu,
                _ => RendererKind::Wgpu,
            };
            let current = (*render_settings).renderer;
            if current == next_renderer {
                return;
            }
            let mut next = (*render_settings).clone();
            next.renderer = next_renderer;
            app_router::save_renderer_preference(next_renderer, &next);
            render_settings.set(next);
            app_core.set_renderer_kind(next_renderer);
            if let Some(window) = web_sys::window() {
                let _ = window.location().reload();
            }
        })
    };
    let on_animations_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.animations = enabled;
            render_settings.set(next);
            if !enabled {}
        })
    };
    let on_emboss_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.emboss = enabled;
            render_settings.set(next);
        })
    };
    let on_wgpu_fps_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.wgpu.show_fps = enabled;
            render_settings.set(next);
        })
    };
    let on_wgpu_edge_aa = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_EDGE_AA_MIN, WGPU_EDGE_AA_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.edge_aa = value;
                render_settings.set(next);
            }
        })
    };
    let on_image_max_dim = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<u32>() {
                let value = value.clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
                let mut next = (*render_settings).clone();
                next.image_max_dim = value;
                render_settings.set(next);
                app_core.set_image_max_dim(value);
            }
        })
    };
    let on_wgpu_render_scale = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_RENDER_SCALE_MIN, WGPU_RENDER_SCALE_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.render_scale = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_rotate_anim_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.wgpu.rotate_anim = enabled;
            render_settings.set(next);
        })
    };
    let on_wgpu_rotate_anim_response = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value =
                    value.clamp(WGPU_ROTATE_ANIM_RESPONSE_MIN, WGPU_ROTATE_ANIM_RESPONSE_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.rotate_anim_response = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_rotate_anim_damping = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_ROTATE_ANIM_DAMPING_MIN, WGPU_ROTATE_ANIM_DAMPING_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.rotate_anim_damping = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_flip_thickness = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value =
                    value.clamp(WGPU_FLIP_THICKNESS_MM_MIN, WGPU_FLIP_THICKNESS_MM_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.flip_thickness_mm = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_shadow_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.wgpu.shadow = enabled;
            render_settings.set(next);
        })
    };
    let on_wgpu_shadow_distance = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_SHADOW_DISTANCE_MIN, WGPU_SHADOW_DISTANCE_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.shadow_distance = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_shadow_radius = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_SHADOW_RADIUS_MIN, WGPU_SHADOW_RADIUS_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.shadow_radius = value;
                render_settings.set(next);
            }
        })
    };
    let on_wgpu_shadow_darkness = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_SHADOW_DARKNESS_MIN, WGPU_SHADOW_DARKNESS_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.shadow_darkness = value;
                render_settings.set(next);
            }
        })
    };
    let on_reset_render_settings = {
        let render_settings = render_settings.clone();
        Callback::from(move |_: MouseEvent| {
            // Reset every render setting to its default, but stay on the
            // currently-active renderer so the reset doesn't switch/reload it.
            // The render_settings effect re-persists, re-applies, and syncs
            // image_max_dim to the core automatically.
            let mut next = RenderSettings::default();
            next.renderer = (*render_settings).renderer;
            render_settings.set(next);
        })
    };
    let on_theme_toggle = {
        let theme_mode = theme_mode.clone();
        let theme_toggle_ref = theme_toggle_ref.clone();
        let app_core = app_core.clone();
        Callback::from(move |_: Event| {
            let next = match *theme_mode {
                ThemeMode::System => ThemeMode::Light,
                ThemeMode::Light => ThemeMode::Dark,
                ThemeMode::Dark => ThemeMode::System,
            };
            theme_mode.set(next);
            app_core.set_theme_mode(next);
            if let Some(input) = theme_toggle_ref.cast::<HtmlInputElement>() {
                sync_theme_checkbox(&input, next);
            }
        })
    };
    let on_fast_render_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.fast_render = enabled;
            render_settings.set(next);
        })
    };
    let on_fast_filter_toggle = {
        let render_settings = render_settings.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.fast_filter = enabled;
            render_settings.set(next);
        })
    };
    let on_debug_toggle = {
        let show_debug = show_debug.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            show_debug.set(enabled);
            app_core.set_show_debug(enabled);
        })
    };
    let on_auto_pan_outer_ratio = {
        let auto_pan_outer_ratio = auto_pan_outer_ratio.clone();
        let auto_pan_inner_ratio = auto_pan_inner_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(AUTO_PAN_OUTER_RATIO_MIN, AUTO_PAN_OUTER_RATIO_MAX);
                auto_pan_outer_ratio.set(value);
                app_core.set_auto_pan_outer_ratio(value);
                let inner_value = (*auto_pan_inner_ratio).max(value);
                if (inner_value - *auto_pan_inner_ratio).abs() > f32::EPSILON {
                    auto_pan_inner_ratio.set(inner_value);
                    app_core.set_auto_pan_inner_ratio(inner_value);
                }
            }
        })
    };
    let on_auto_pan_inner_ratio = {
        let auto_pan_outer_ratio = auto_pan_outer_ratio.clone();
        let auto_pan_inner_ratio = auto_pan_inner_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let min_value = (*auto_pan_outer_ratio).max(AUTO_PAN_INNER_RATIO_MIN);
                let value = value.clamp(min_value, AUTO_PAN_INNER_RATIO_MAX);
                auto_pan_inner_ratio.set(value);
                app_core.set_auto_pan_inner_ratio(value);
            }
        })
    };
    let on_auto_pan_speed_ratio = {
        let auto_pan_speed_ratio = auto_pan_speed_ratio.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(AUTO_PAN_SPEED_RATIO_MIN, AUTO_PAN_SPEED_RATIO_MAX);
                auto_pan_speed_ratio.set(value);
                app_core.set_auto_pan_speed_ratio(value);
            }
        })
    };
    let on_menu_toggle = {
        let menu_visible = menu_visible.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            menu_visible.set(input.checked());
        })
    };

    {
        let theme_toggle_ref = theme_toggle_ref.clone();
        use_effect_with(
            (theme_mode_value, show_controls_value),
            move |(mode, _show_controls_value)| {
                if let Some(input) = theme_toggle_ref.cast::<HtmlInputElement>() {
                    sync_theme_checkbox(&input, *mode);
                }
                || ()
            },
        );
    }

    {
        use_effect_with(theme_mode_value, move |mode| {
            if let Some(window) = web_sys::window() {
                if let Some(document) = window.document() {
                    if let Some(body) = document.body() {
                        let theme_value = match *mode {
                            ThemeMode::System => "system",
                            ThemeMode::Light => "light",
                            ThemeMode::Dark => "dark",
                        };
                        let _ = body.set_attribute("data-theme", theme_value);
                    }
                }
            }
            || ()
        });
    }

    // Legacy Yew renderer wiring removed; dev-panel does not render puzzle content.

    {
        let multiplayer_config = multiplayer_config_value.clone();
        use_effect_with(multiplayer_config, move |config| {
            if let Some(config) = config.as_ref() {
                app_router::save_room_session(&config.room_id);
            }
            || ()
        });
    }

    {
        let show_controls = show_controls.clone();
        let app_core = app_core.clone();
        use_effect_with(show_controls_value, move |show_controls_value| {
            let current = *show_controls_value;
            let window = web_sys::window().expect("window available");
            let options = EventListenerOptions {
                phase: EventListenerPhase::Capture,
                passive: false,
            };
            let listener = EventListener::new_with_options(
                &window,
                "keydown",
                options,
                move |event: &Event| {
                    if let Some(event) = event.dyn_ref::<KeyboardEvent>() {
                        if event.repeat() {
                            return;
                        }
                        // Don't hijack keystrokes while the user is typing in a
                        // field (piece-count input, room id, etc.).
                        if event_target_is_editable(event) {
                            return;
                        }
                        let key = event.key();
                        let code = event.code();
                        let toggle = matches!(key.as_str(), "?" | "d" | "D")
                            || matches!(code.as_str(), "KeyD" | "Slash");
                        if toggle {
                            let next = !current;
                            gloo::console::log!(
                                "controls",
                                format!("{} -> {}", current, next),
                                key,
                                code
                            );
                            show_controls.set(next);
                            event.prevent_default();
                            return;
                        }
                        // View shortcuts mirror the dev panel's Rules tab
                        // buttons: 1 zoom in, 2 zoom out, 3 fit workspace,
                        // 4 fit frame. Match the digit by `code` so the row
                        // keys work regardless of Shift/keyboard layout.
                        let handled = match code.as_str() {
                            "Digit1" | "Numpad1" => {
                                app_core.zoom_view_by(1.1);
                                true
                            }
                            "Digit2" | "Numpad2" => {
                                app_core.zoom_view_by(1.0 / 1.1);
                                true
                            }
                            "Digit3" | "Numpad3" => {
                                app_core.reset_view_to_fit();
                                true
                            }
                            "Digit4" | "Numpad4" => {
                                app_core.fit_view_to_frame();
                                true
                            }
                            _ => false,
                        };
                        if handled {
                            event.prevent_default();
                        }
                    }
                },
            );
            || drop(listener)
        });
    }

    // Legacy SVG/WGPU input + image scaling removed.

    let (on_scramble, on_solve, on_solve_rotation, on_unflip, scramble_disabled) =
        if puzzle_dims_value.is_some() {
            let on_scramble = {
                let app_core = app_core.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    app_core.rescramble();
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            let on_solve = {
                let app_core = app_core.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    app_core.solve_puzzle();
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            let on_solve_rotation = {
                let app_core = app_core.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    app_core.clear_all_group_rotations();
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            let on_unflip = {
                let app_core = app_core.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    app_core.clear_all_group_flips();
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            (on_scramble, on_solve, on_solve_rotation, on_unflip, false)
        } else {
            (
                Callback::from(|_: MouseEvent| {}),
                Callback::from(|_: MouseEvent| {}),
                Callback::from(|_: MouseEvent| {}),
                Callback::from(|_: MouseEvent| {}),
                true,
            )
        };

    let on_puzzle_toggle = details_toggle(puzzle_group_open.clone(), DEV_PANEL_GROUP_PUZZLE_KEY);
    let on_multiplayer_toggle = details_toggle(
        multiplayer_group_open.clone(),
        DEV_PANEL_GROUP_MULTIPLAYER_KEY,
    );
    let on_graphics_toggle =
        details_toggle(graphics_group_open.clone(), DEV_PANEL_GROUP_GRAPHICS_KEY);
    let on_rules_toggle = details_toggle(rules_group_open.clone(), DEV_PANEL_GROUP_RULES_KEY);
    let on_shaping_toggle = details_toggle(shaping_group_open.clone(), DEV_PANEL_GROUP_SHAPING_KEY);
    let puzzle_controls = html! {
        <>
            <div class="control">
                <label>
                    { "Seed" }
                    <span class="control-value">{ seed_label }</span>
                </label>
            </div>
            <div class="control">
                <label for="share-link">
                    { share_link_label }
                </label>
                <input
                    id="share-link"
                    type="text"
                    value={share_link}
                    readonly=true
                />
            </div>
            { if !multiplayer_active {
                html! {
                    <div class="control">
                        <label for="share-seed">
                            { "Include shuffle seed" }
                            <input
                                id="share-seed"
                                type="checkbox"
                                checked={include_share_seed_value}
                                disabled={!shareable_local}
                                onchange={on_share_seed_toggle}
                            />
                        </label>
                    </div>
                }
            } else {
                html! {}
            }}
            <div class="control">
                <label>
                    { "Expected solve time" }
                    <span class="control-value">{ solve_time_label }</span>
                </label>
            </div>
            <div class="control">
                <label>
                    { "Connections" }
                    <span class="control-value">{ connections_label }</span>
                </label>
            </div>
            <div class="control">
                <label>
                    { "Border connections" }
                    <span class="control-value">{ border_connections_label }</span>
                </label>
            </div>
            <div class="control">
                <label>
                    { "Puzzle art" }
                    <span class="control-value">{ puzzle_art.label }</span>
                </label>
                <select
                    id="puzzle-art-select"
                    onchange={on_puzzle_art_change}
                    disabled={lock_puzzle_controls}
                >
                    {puzzle_art_options}
                </select>
            </div>
            { {
                let topology_options: Html = available_topologies()
                    .iter()
                    .map(|kind| {
                        let value = kind.tag.to_string();
                        let selected = kind.tag == active_topology_kind.tag;
                        let label = kind.display_name.to_string();
                        html! { <option {value} {selected}>{ label }</option> }
                    })
                    .collect();
                html! {
                    <div class="control">
                        <label>
                            { "Topology" }
                            <span class="control-value">{ active_topology_kind.display_name }</span>
                        </label>
                        <select
                            id="local-topology-select"
                            onchange={on_local_topology_change}
                            disabled={lock_puzzle_controls}
                        >
                            { topology_options }
                        </select>
                    </div>
                }
            } }
            { if local_topology_is_grid {
                // Grid keeps its dedicated picker so the catalog-aware
                // request_puzzle_change path can save the selection on
                // disk. Other topologies use the unified non-grid picker
                // below.
                html! {
                    <div class="control">
                        <label>
                            { "Grid" }
                            <span class="control-value">{ grid_label }</span>
                        </label>
                        <select
                            id="grid-select"
                            onchange={on_grid_change}
                            disabled={lock_puzzle_controls}
                        >
                            {grid_options}
                        </select>
                        { if grid_custom_active {
                            html! {
                                <input
                                    id="grid-custom-count"
                                    type="number"
                                    min={CUSTOM_PIECE_COUNT_MIN.to_string()}
                                    max={CUSTOM_PIECE_COUNT_MAX.to_string()}
                                    step="1"
                                    value={grid_custom_input_value.clone()}
                                    onblur={on_grid_custom_blur}
                                    onkeydown={on_grid_custom_keydown}
                                    disabled={lock_puzzle_controls}
                                />
                            }
                        } else {
                            html! {}
                        }}
                        <button
                            type="button"
                            id="grid-regenerate"
                            class="control-button"
                            onclick={on_regenerate_click.clone()}
                            disabled={lock_puzzle_controls}
                        >
                            { "Regenerate" }
                        </button>
                    </div>
                }
            } else {
                // Unified non-grid piece-count picker driven by the
                // active topology kind's `piece_count_choices` /
                // `resolve_target`.
                let non_grid_options: Html = non_grid_choices
                    .iter()
                    .enumerate()
                    .map(|(index, choice)| {
                        let value = index.to_string();
                        let selected = !non_grid_custom_active
                            && non_grid_index.map(|i| i == index).unwrap_or(false);
                        let label = choice.label.clone();
                        html! { <option {value} {selected}>{ label }</option> }
                    })
                    .chain(std::iter::once(html! {
                        <option value="custom" selected={non_grid_custom_active}>
                            { "Custom" }
                        </option>
                    }))
                    .collect();
                let regenerate_button = html! {
                    <button
                        type="button"
                        id="non-grid-regenerate"
                        class="control-button"
                        onclick={on_regenerate_click.clone()}
                    >
                        { "Regenerate" }
                    </button>
                };
                html! {
                    <div class="control">
                        <select
                            id="non-grid-piece-count"
                            value={non_grid_select_value.clone()}
                            onchange={on_non_grid_count_change}
                        >
                            { non_grid_options }
                        </select>
                        { if non_grid_custom_active {
                            html! {
                                <input
                                    id="non-grid-custom-count"
                                    type="number"
                                    min={CUSTOM_PIECE_COUNT_MIN.to_string()}
                                    max={CUSTOM_PIECE_COUNT_MAX.to_string()}
                                    step="1"
                                    value={non_grid_custom_input_value.clone()}
                                    onblur={on_non_grid_custom_blur}
                                    onkeydown={on_non_grid_custom_keydown}
                                />
                            }
                        } else {
                            html! {}
                        }}
                        { regenerate_button }
                    </div>
                }
            } }
            { if puzzle_art_custom_value { html! {
                <>
            <hr class="control-separator" />
            <div class="control">
                <label for="private-file">
                    { "Private image file" }
                </label>
                <input
                    id="private-file"
                    type="file"
                    accept="image/*"
                    onchange={if admin_enabled { on_admin_private_file_input } else { on_private_file_input }}
                    disabled={lock_puzzle_controls || private_upload_busy || admin_private_upload_busy}
                />
            </div>
            { if !multiplayer_active {
                html! {
                    <div class="control">
                        <label for="private-label">
                            { "Private label" }
                            <span class="control-value">{ "optional" }</span>
                        </label>
                        <input
                            id="private-label"
                            type="text"
                            value={private_label_value.clone()}
                            oninput={on_private_label_input}
                            disabled={lock_puzzle_controls || private_upload_busy}
                        />
                    </div>
                }
            } else {
                html! {}
            }}
            { {
                // Show the upload status/error appropriate to the active path:
                // the admin (websocket) upload for admins, the local upload
                // otherwise.
                let status = if admin_enabled {
                    if matches!(admin_private_status_value, AdminUploadStatus::Idle) {
                        None
                    } else {
                        Some(admin_private_status_display.clone())
                    }
                } else {
                    private_status_value.clone()
                };
                let error = if admin_enabled {
                    admin_private_error_value.clone()
                } else {
                    private_error_value.clone()
                };
                html! {
                    <>
                        { if let Some(message) = status {
                            html! {
                                <div class="control">
                                    <label>
                                        { "Private image status" }
                                        <span class="control-value">{ message }</span>
                                    </label>
                                </div>
                            }
                        } else {
                            html! {}
                        }}
                        { if let Some(message) = error {
                            html! {
                                <div class="control">
                                    <label>
                                        { "Private image error" }
                                        <span class="control-value">{ message }</span>
                                    </label>
                                </div>
                            }
                        } else {
                            html! {}
                        }}
                    </>
                }
            } }
                </>
            } } else { html! {} } }
            { if !multiplayer_active || admin_enabled {
                html! {
                    <>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={if admin_enabled { on_admin_scramble } else { on_scramble }}
                                disabled={scramble_disabled && !admin_enabled}
                            >
                                { "Scramble" }
                            </button>
                        </div>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={if admin_enabled { on_admin_solve } else { on_solve }}
                                disabled={scramble_disabled && !admin_enabled}
                            >
                                { "Solve" }
                            </button>
                        </div>
                    </>
                }
            } else {
                html! {}
            }}
            { if !multiplayer_active {
                html! {
                    <>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_solve_rotation}
                                disabled={scramble_disabled}
                            >
                                { "Solve rotation" }
                            </button>
                        </div>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_unflip}
                                disabled={scramble_disabled}
                            >
                                { "Unflip all" }
                            </button>
                        </div>
                    </>
                }
            } else {
                html! {}
            }}
        </>
    };
    let multiplayer_controls = html! {
        <>
            <div class="control">
                <label for="mode-select">{ "Mode" }</label>
                <select
                    id="mode-select"
                    value={mode_value}
                    onchange={on_mode_change}
                >
                    <option value="local" selected={!multiplayer_controls_online}>
                        { "Local" }
                    </option>
                    <option value="online" selected={multiplayer_controls_online}>
                        { "Online" }
                    </option>
                </select>
            </div>
            <div class="control">
                <label for="admin-token">
                    { "Admin token" }
                </label>
                <input
                    id="admin-token"
                    type="password"
                    value={admin_token_input_value.clone()}
                    oninput={on_admin_token_input}
                />
                <button
                    class="control-button"
                    type="button"
                    onclick={on_admin_token_apply}
                >
                    { "Apply" }
                </button>
                <span class="control-value">{ admin_status_label }</span>
            </div>
            { if show_online_setup {
                html! {
                    <>
                        <div class="control">
                            <label for="room-id-draft">{ "Room id" }</label>
                            <input
                                id="room-id-draft"
                                type="text"
                                value={room_id_draft_value.clone()}
                                oninput={on_room_id_draft_input}
                                disabled={room_setup_busy}
                            />
                        </div>
                        <div class="control">
                            <label for="admin-seed">
                                { "Shuffle seed (optional)" }
                            </label>
                            <input
                                id="admin-seed"
                                type="text"
                                value={admin_seed_value.clone()}
                                placeholder="0x1234"
                                oninput={on_admin_seed_input.clone()}
                                disabled={room_setup_busy}
                            />
                        </div>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_create_room}
                                disabled={room_setup_busy}
                            >
                                { "Create room" }
                            </button>
                        </div>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_join_room}
                                disabled={room_setup_busy}
                            >
                                { "Join room" }
                            </button>
                        </div>
                        { if !room_setup_status_text.is_empty() {
                            html! {
                                <div class="control">
                                    <label>
                                        { "Room setup" }
                                        <span class="control-value">{ room_setup_status_text }</span>
                                    </label>
                                </div>
                            }
                        } else {
                            html! {}
                        }}
                        { if let Some(message) = room_setup_error_value.clone() {
                            html! {
                                <div class="control">
                                    <label>
                                        { "Room setup error" }
                                        <span class="control-value">{ message }</span>
                                    </label>
                                </div>
                            }
                        } else {
                            html! {}
                        }}
                    </>
                }
            } else {
                html! {}
            }}
            { if multiplayer_active {
                html! {
                    <>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_leave_room}
                            >
                                { "Leave room" }
                            </button>
                        </div>
                        <div class="control">
                            <label>
                                { "Room" }
                                <span class="control-value">{ mp_room_label.clone() }</span>
                            </label>
                        </div>
                        <div class="control">
                            <label>
                                { "Connection" }
                                <span class="control-value">{ mp_connection_label }</span>
                            </label>
                        </div>
                        { if admin_enabled {
                            // Puzzle configuration (art, topology, pieces,
                            // regenerate, scramble, private upload) lives in
                            // the Puzzle group and is shared with local play.
                            // Only the shuffle seed — meaningful for
                            // reproducible server-side scrambles — stays here.
                            html! {
                                <div class="control">
                                    <label for="admin-seed">
                                        { "Shuffle seed (optional)" }
                                    </label>
                                    <input
                                        id="admin-seed"
                                        type="text"
                                        value={admin_seed_value.clone()}
                                        placeholder="0x1234"
                                        oninput={on_admin_seed_input}
                                    />
                                </div>
                            }
                        } else {
                            html! {}
                        }}
                    </>
                }
            } else {
                html! {}
            }}
            <div class="control">
                <label for="ws-delay-in">{ "WS in delay (ms)" }</label>
                <input
                    id="ws-delay-in"
                    type="number"
                    min="0"
                    step="1"
                    value={ws_delay_in_value.clone()}
                    oninput={on_ws_delay_in_input}
                />
            </div>
            <div class="control">
                <label for="ws-delay-out">{ "WS out delay (ms)" }</label>
                <input
                    id="ws-delay-out"
                    type="number"
                    min="0"
                    step="1"
                    value={ws_delay_out_value.clone()}
                    oninput={on_ws_delay_out_input}
                />
            </div>
            <div class="control">
                <label for="ws-delay-jitter">{ "WS jitter (ms)" }</label>
                <input
                    id="ws-delay-jitter"
                    type="number"
                    min="0"
                    step="1"
                    value={ws_delay_jitter_value.clone()}
                    oninput={on_ws_delay_jitter_input}
                />
            </div>
            <div class="control">
                <button
                    class="control-button"
                    type="button"
                    onclick={on_identity_reset}
                >
                    { "Reset identity" }
                </button>
            </div>
        </>
    };
    let auto_pan_inner_min = auto_pan_outer_ratio_value.max(AUTO_PAN_INNER_RATIO_MIN);
    let graphics_controls = html! {
        <>
            <div class="control">
                <label for="menu-visible">
                    { "Menu overlay" }
                    <input
                        id="menu-visible"
                        type="checkbox"
                        checked={menu_visible_value}
                        onchange={on_menu_toggle}
                    />
                </label>
            </div>
            <div class="control">
                <label for="renderer-select">{ "Renderer" }</label>
                <select
                    id="renderer-select"
                    value={renderer_value}
                    onchange={on_renderer_change}
                >
                    <option value="wgpu" selected={renderer_kind == RendererKind::Wgpu}>
                        { "WGPU" }
                    </option>
                    <option value="svg" selected={renderer_kind == RendererKind::Svg}>
                        { "SVG" }
                    </option>
                </select>
            </div>
            <div class="control">
                <label for="image-max-dim">
                    { "Image max dimension" }
                    <span class="control-value">{ image_max_dim }</span>
                </label>
                <input
                    id="image-max-dim"
                    type="range"
                    min={IMAGE_MAX_DIMENSION_MIN.to_string()}
                    max={IMAGE_MAX_DIMENSION_MAX.to_string()}
                    step="256"
                    value={image_max_dim.to_string()}
                    onchange={on_image_max_dim}
                />
            </div>
            { if svg_settings_visible {
                html! {
                    <>
                        <div class="control">
                            <label for="animations-enabled">
                                { "Animations: " }
                                { if svg_animations_enabled { "On" } else { "Off" } }
                                <input
                                    id="animations-enabled"
                                    type="checkbox"
                                    checked={svg_animations_enabled}
                                    onchange={on_animations_toggle}
                                />
                            </label>
                        </div>
                        <div class="control">
                            <label for="emboss-enabled">
                                { "Emboss: " }
                                { if svg_emboss_enabled { "On" } else { "Off" } }
                                <input
                                    id="emboss-enabled"
                                    type="checkbox"
                                    checked={svg_emboss_enabled}
                                    onchange={on_emboss_toggle}
                                />
                            </label>
                        </div>
                        <div class="control">
                            <label for="fast-render">
                                { "Fast render: " }
                                { if svg_fast_render { "On" } else { "Off" } }
                                <input
                                    id="fast-render"
                                    type="checkbox"
                                    checked={svg_fast_render}
                                    onchange={on_fast_render_toggle}
                                />
                            </label>
                        </div>
                        <div class="control">
                            <label for="fast-filter">
                                { "Fast filter: " }
                                { if svg_fast_filter { "On" } else { "Off" } }
                                <input
                                    id="fast-filter"
                                    type="checkbox"
                                    checked={svg_fast_filter}
                                    onchange={on_fast_filter_toggle}
                                />
                            </label>
                        </div>
                    </>
                }
            } else {
                html! {
                    <>
                        <div class="control">
                            <label for="wgpu-show-fps">
                                { "Show FPS: " }
                                { if wgpu_show_fps { "On" } else { "Off" } }
                                <input
                                    id="wgpu-show-fps"
                                    type="checkbox"
                                    checked={wgpu_show_fps}
                                    onchange={on_wgpu_fps_toggle}
                                />
                            </label>
                        </div>
                        <div class="control">
                            <label for="wgpu-edge-aa">
                                { "Edge AA" }
                                <span class="control-value">{ fmt_f32(wgpu_edge_aa) }</span>
                            </label>
                            <input
                                id="wgpu-edge-aa"
                                type="range"
                                min={WGPU_EDGE_AA_MIN.to_string()}
                                max={WGPU_EDGE_AA_MAX.to_string()}
                                step="0.01"
                                value={wgpu_edge_aa.to_string()}
                                oninput={on_wgpu_edge_aa}
                            />
                        </div>
                        <div class="control">
                            <label for="wgpu-render-scale">
                                { "Render scale" }
                                <span class="control-value">{ fmt_f32(wgpu_render_scale) }</span>
                            </label>
                            <input
                                id="wgpu-render-scale"
                                type="range"
                                min={WGPU_RENDER_SCALE_MIN.to_string()}
                                max={WGPU_RENDER_SCALE_MAX.to_string()}
                                step="0.05"
                                value={wgpu_render_scale.to_string()}
                                oninput={on_wgpu_render_scale}
                            />
                        </div>
                        <div class="control">
                            <label for="wgpu-rotate-anim">
                                { "Move animations: " }
                                { if wgpu_rotate_anim { "On" } else { "Off" } }
                                <input
                                    id="wgpu-rotate-anim"
                                    type="checkbox"
                                    checked={wgpu_rotate_anim}
                                    onchange={on_wgpu_rotate_anim_toggle}
                                />
                            </label>
                        </div>
                        { if wgpu_rotate_anim {
                            html! {
                                <>
                                    <div class="control">
                                        <label for="wgpu-rotate-anim-response">
                                            { "Rotate response" }
                                            <span class="control-value">{ fmt_f32(wgpu_rotate_anim_response) }</span>
                                        </label>
                                        <input
                                            id="wgpu-rotate-anim-response"
                                            type="range"
                                            min={WGPU_ROTATE_ANIM_RESPONSE_MIN.to_string()}
                                            max={WGPU_ROTATE_ANIM_RESPONSE_MAX.to_string()}
                                            step="0.01"
                                            value={wgpu_rotate_anim_response.to_string()}
                                            oninput={on_wgpu_rotate_anim_response}
                                        />
                                    </div>
                                    <div class="control">
                                        <label for="wgpu-rotate-anim-damping">
                                            { "Rotate damping" }
                                            <span class="control-value">{ fmt_f32(wgpu_rotate_anim_damping) }</span>
                                        </label>
                                        <input
                                            id="wgpu-rotate-anim-damping"
                                            type="range"
                                            min={WGPU_ROTATE_ANIM_DAMPING_MIN.to_string()}
                                            max={WGPU_ROTATE_ANIM_DAMPING_MAX.to_string()}
                                            step="0.05"
                                            value={wgpu_rotate_anim_damping.to_string()}
                                            oninput={on_wgpu_rotate_anim_damping}
                                        />
                                    </div>
                                </>
                            }
                        } else {
                            html! {}
                        } }
                        <div class="control">
                            <label for="wgpu-flip-thickness">
                                { "Piece thickness (mm)" }
                                <span class="control-value">{ fmt_f32(wgpu_flip_thickness_mm) }</span>
                            </label>
                            <input
                                id="wgpu-flip-thickness"
                                type="range"
                                min={WGPU_FLIP_THICKNESS_MM_MIN.to_string()}
                                max={WGPU_FLIP_THICKNESS_MM_MAX.to_string()}
                                step="0.5"
                                value={wgpu_flip_thickness_mm.to_string()}
                                oninput={on_wgpu_flip_thickness}
                            />
                        </div>
                        <div class="control">
                            <label for="wgpu-shadow">
                                { "Shadow: " }
                                { if wgpu_shadow { "On" } else { "Off" } }
                                <input
                                    id="wgpu-shadow"
                                    type="checkbox"
                                    checked={wgpu_shadow}
                                    onchange={on_wgpu_shadow_toggle}
                                />
                            </label>
                        </div>
                        { if wgpu_shadow {
                            html! {
                                <>
                                    <div class="control">
                                        <label for="wgpu-shadow-distance">
                                            { "Shadow distance" }
                                            <span class="control-value">{ fmt_f32(wgpu_shadow_distance) }</span>
                                        </label>
                                        <input
                                            id="wgpu-shadow-distance"
                                            type="range"
                                            min={WGPU_SHADOW_DISTANCE_MIN.to_string()}
                                            max={WGPU_SHADOW_DISTANCE_MAX.to_string()}
                                            step="0.5"
                                            value={wgpu_shadow_distance.to_string()}
                                            oninput={on_wgpu_shadow_distance}
                                        />
                                    </div>
                                    <div class="control">
                                        <label for="wgpu-shadow-radius">
                                            { "Shadow radius" }
                                            <span class="control-value">{ fmt_f32(wgpu_shadow_radius) }</span>
                                        </label>
                                        <input
                                            id="wgpu-shadow-radius"
                                            type="range"
                                            min={WGPU_SHADOW_RADIUS_MIN.to_string()}
                                            max={WGPU_SHADOW_RADIUS_MAX.to_string()}
                                            step="0.5"
                                            value={wgpu_shadow_radius.to_string()}
                                            oninput={on_wgpu_shadow_radius}
                                        />
                                    </div>
                                    <div class="control">
                                        <label for="wgpu-shadow-darkness">
                                            { "Shadow darkness" }
                                            <span class="control-value">{ fmt_f32(wgpu_shadow_darkness) }</span>
                                        </label>
                                        <input
                                            id="wgpu-shadow-darkness"
                                            type="range"
                                            min={WGPU_SHADOW_DARKNESS_MIN.to_string()}
                                            max={WGPU_SHADOW_DARKNESS_MAX.to_string()}
                                            step="0.05"
                                            value={wgpu_shadow_darkness.to_string()}
                                            oninput={on_wgpu_shadow_darkness}
                                        />
                                    </div>
                                </>
                            }
                        } else {
                            html! {}
                        } }
                    </>
                }
            } }
            <div class="control">
                <button
                    type="button"
                    class="control-button"
                    onclick={on_reset_render_settings}
                >
                    { "Reset render settings" }
                </button>
            </div>
            <div class="control">
                <label for="theme-mode">
                    { "Theme: " }
                    { match theme_mode_value {
                        ThemeMode::System => "System",
                        ThemeMode::Light => "Light",
                        ThemeMode::Dark => "Dark",
                    } }
                    <input
                        id="theme-mode"
                        type="checkbox"
                        checked={theme_mode_value == ThemeMode::Dark}
                        ref={theme_toggle_ref}
                        onchange={on_theme_toggle}
                    />
                </label>
            </div>
            <div class="control">
                <label for="debug-enabled">
                    { "Debug overlay: " } { if show_debug_value { "On" } else { "Off" } }
                    <input
                        id="debug-enabled"
                        type="checkbox"
                        checked={show_debug_value}
                        onchange={on_debug_toggle}
                    />
                </label>
            </div>
            <div class="control">
                <label for="auto-pan-outer">
                    { "Auto-pan outer" }
                    <span class="control-value">{ fmt_f32(auto_pan_outer_ratio_value) }</span>
                </label>
                <input
                    id="auto-pan-outer"
                    type="range"
                    min={AUTO_PAN_OUTER_RATIO_MIN.to_string()}
                    max={AUTO_PAN_OUTER_RATIO_MAX.to_string()}
                    step="0.005"
                    value={auto_pan_outer_ratio_value.to_string()}
                    oninput={on_auto_pan_outer_ratio}
                />
            </div>
            <div class="control">
                <label for="auto-pan-inner">
                    { "Auto-pan inner" }
                    <span class="control-value">{ fmt_f32(auto_pan_inner_ratio_value) }</span>
                </label>
                <input
                    id="auto-pan-inner"
                    type="range"
                    min={auto_pan_inner_min.to_string()}
                    max={AUTO_PAN_INNER_RATIO_MAX.to_string()}
                    step="0.005"
                    value={auto_pan_inner_ratio_value.to_string()}
                    oninput={on_auto_pan_inner_ratio}
                />
            </div>
            <div class="control">
                <label for="auto-pan-speed">
                    { "Auto-pan speed" }
                    <span class="control-value">{ fmt_f32(auto_pan_speed_ratio_value) }</span>
                </label>
                <input
                    id="auto-pan-speed"
                    type="range"
                    min={AUTO_PAN_SPEED_RATIO_MIN.to_string()}
                    max={AUTO_PAN_SPEED_RATIO_MAX.to_string()}
                    step="0.05"
                    value={auto_pan_speed_ratio_value.to_string()}
                    oninput={on_auto_pan_speed_ratio}
                />
            </div>
        </>
    };
    let rules_controls = html! {
        <>
            <div class="control">
                <label for="workspace-padding">
                    { "Workspace padding" }
                    <span class="control-value">{ workspace_padding_label }</span>
                </label>
                <input
                    id="workspace-padding"
                    type="range"
                    min={WORKSPACE_PADDING_RATIO_MIN.to_string()}
                    max={WORKSPACE_PADDING_RATIO_MAX.to_string()}
                    step="0.05"
                    value={workspace_padding_ratio_value.to_string()}
                    oninput={on_workspace_padding_ratio}
                />
            </div>
            <div class="control">
                <button
                    class="control-button"
                    type="button"
                    onclick={on_zoom_in}
                    disabled={view_controls_disabled}
                >
                    { "Zoom in" }
                </button>
                <button
                    class="control-button"
                    type="button"
                    onclick={on_zoom_out}
                    disabled={view_controls_disabled}
                >
                    { "Zoom out" }
                </button>
                <button
                    class="control-button"
                    type="button"
                    onclick={on_fit_workspace}
                    disabled={view_controls_disabled}
                >
                    { "Fit workspace" }
                </button>
                <button
                    class="control-button"
                    type="button"
                    onclick={on_fit_frame}
                    disabled={view_controls_disabled}
                >
                    { "Fit frame" }
                </button>
            </div>
            <div class="control">
                <label for="frame-snap">
                    { "Frame snap" }
                    <span class="control-value">{ fmt_f32(frame_snap_ratio_value) }</span>
                </label>
                <input
                    id="frame-snap"
                    type="range"
                    min={FRAME_SNAP_MIN.to_string()}
                    max={FRAME_SNAP_MAX.to_string()}
                    step="0.05"
                    value={frame_snap_ratio_value.to_string()}
                    oninput={on_frame_snap}
                />
            </div>
            <div class="control">
                <label for="snap-distance">
                    { "Snap distance tol" }
                    <span class="control-value">{ fmt_f32(snap_distance_ratio_value) }</span>
                </label>
                <input
                    id="snap-distance"
                    type="range"
                    min={SNAP_DISTANCE_RATIO_MIN.to_string()}
                    max={SNAP_DISTANCE_RATIO_MAX.to_string()}
                    step="0.01"
                    value={snap_distance_ratio_value.to_string()}
                    oninput={on_snap_distance}
                />
            </div>
            <div class="control">
                <label for="rotation-snap-tolerance">
                    { "Snap angle tol (deg)" }
                    <span class="control-value">{ fmt_f32(rotation_snap_tolerance_value) }</span>
                </label>
                <input
                    id="rotation-snap-tolerance"
                    type="range"
                    min={ROTATION_SNAP_TOLERANCE_MIN_DEG.to_string()}
                    max={ROTATION_SNAP_TOLERANCE_MAX_DEG.to_string()}
                    step="0.5"
                    value={rotation_snap_tolerance_value.to_string()}
                    oninput={on_rotation_snap_tolerance}
                />
            </div>
            <div class="control">
                <label for="rotation-lock-threshold">
                    { "Aligned rotate <= " }
                    <span class="control-value">{ rotation_lock_threshold_value }</span>
                </label>
                <input
                    id="rotation-lock-threshold"
                    type="range"
                    min={ROTATION_LOCK_THRESHOLD_MIN.to_string()}
                    max={total.max(ROTATION_LOCK_THRESHOLD_MIN).to_string()}
                    step="1"
                    value={rotation_lock_threshold_value.to_string()}
                    oninput={on_rotation_lock_threshold}
                />
            </div>
            <div class="control">
                <label for="rotation-enabled">
                    { "Rotation: " } { if rotation_enabled_value { "On" } else { "Off" } }
                    <input
                        id="rotation-enabled"
                        type="checkbox"
                        checked={rotation_enabled_value}
                        onchange={on_rotation_toggle}
                    />
                </label>
            </div>
            <div class="control">
                <label for="rotation-noise">
                    { "Rotation noise" }
                    <span class="control-value">{ fmt_f32(rotation_noise_value) }</span>
                </label>
                <input
                    id="rotation-noise"
                    type="range"
                    min={ROTATION_NOISE_MIN.to_string()}
                    max={ROTATION_NOISE_MAX.to_string()}
                    step="0.1"
                    value={rotation_noise_value.to_string()}
                    oninput={on_rotation_noise}
                />
            </div>
        </>
    };
    let shaping_controls = html! {
        <>
            <div class="control">
                <label for="tab-width">
                    { "Tab size" }
                    <span class="control-value">{ fmt_f32(settings_value.tab_width) }</span>
                </label>
                <input
                    id="tab-width"
                    type="range"
                    min={TAB_WIDTH_MIN.to_string()}
                    max={TAB_WIDTH_MAX.to_string()}
                    step="0.005"
                    value={settings_value.tab_width.to_string()}
                    oninput={tab_width_input}
                />
            </div>
            <div class="control">
                <label for="tab-depth">
                    { "Tab depth" }
                    <span class="control-value">{ fmt_f32(settings_value.tab_depth) }</span>
                </label>
                <input
                    id="tab-depth"
                    type="range"
                    min={TAB_DEPTH_MIN.to_string()}
                    max={TAB_DEPTH_MAX.to_string()}
                    step="0.01"
                    value={settings_value.tab_depth.to_string()}
                    oninput={tab_depth_input}
                />
            </div>
            <div class="control">
                <label for="tab-size-scale">
                    { "Tab size scale" }
                    <span class="control-value">
                        { fmt_f32(settings_value.tab_size_scale) }
                    </span>
                </label>
                <input
                    id="tab-size-scale"
                    type="range"
                    min={TAB_SIZE_SCALE_MIN.to_string()}
                    max={TAB_SIZE_SCALE_MAX.to_string()}
                    step="0.005"
                    value={settings_value.tab_size_scale.to_string()}
                    oninput={tab_size_scale_input}
                />
            </div>
            <div class="control">
                <label for="tab-size-min">
                    { "Tab size min" }
                    <span class="control-value">{ fmt_f32(settings_value.tab_size_min) }</span>
                </label>
                <input
                    id="tab-size-min"
                    type="range"
                    min={TAB_SIZE_MIN_LIMIT.to_string()}
                    max={settings_value.tab_size_max.to_string()}
                    step="0.005"
                    value={settings_value.tab_size_min.to_string()}
                    oninput={tab_size_min_input}
                />
            </div>
            <div class="control">
                <label for="tab-size-max">
                    { "Tab size max" }
                    <span class="control-value">{ fmt_f32(settings_value.tab_size_max) }</span>
                </label>
                <input
                    id="tab-size-max"
                    type="range"
                    min={settings_value.tab_size_min.to_string()}
                    max={TAB_SIZE_MAX_LIMIT.to_string()}
                    step="0.005"
                    value={settings_value.tab_size_max.to_string()}
                    oninput={tab_size_max_input}
                />
            </div>
            <div class="control">
                <label for="skew-range">
                    { "Center skew" }
                    <span class="control-value">{ fmt_f32(settings_value.skew_range) }</span>
                </label>
                <input
                    id="skew-range"
                    type="range"
                    min="0.0"
                    max={SKEW_RANGE_MAX.to_string()}
                    step="0.005"
                    value={settings_value.skew_range.to_string()}
                    oninput={skew_input}
                />
            </div>
            <div class="control">
                <label for="variation">
                    { "Variation" }
                    <span class="control-value">{ fmt_f32(settings_value.variation) }</span>
                </label>
                <input
                    id="variation"
                    type="range"
                    min={VARIATION_MIN.to_string()}
                    max={VARIATION_MAX.to_string()}
                    step="0.01"
                    value={settings_value.variation.to_string()}
                    oninput={variation_input}
                />
            </div>
            <div class="control">
                <label for="jitter-strength">
                    { "Jitter strength" }
                    <span class="control-value">
                        { fmt_f32(settings_value.jitter_strength) }
                    </span>
                </label>
                <input
                    id="jitter-strength"
                    type="range"
                    min={JITTER_STRENGTH_MIN.to_string()}
                    max={JITTER_STRENGTH_MAX.to_string()}
                    step="0.005"
                    value={settings_value.jitter_strength.to_string()}
                    oninput={jitter_strength_input}
                />
            </div>
            <div class="control">
                <label for="jitter-len-bias">
                    { "Length jitter bias" }
                    <span class="control-value">
                        { fmt_f32(settings_value.jitter_len_bias) }
                    </span>
                </label>
                <input
                    id="jitter-len-bias"
                    type="range"
                    min={JITTER_LEN_BIAS_MIN.to_string()}
                    max={JITTER_LEN_BIAS_MAX.to_string()}
                    step="0.01"
                    value={settings_value.jitter_len_bias.to_string()}
                    oninput={jitter_len_bias_input}
                />
            </div>
            <div class="control">
                <label for="line-bend">
                    { "Grid bend" }
                    <span class="control-value">{ fmt_f32(settings_value.line_bend_ratio) }</span>
                </label>
                <input
                    id="line-bend"
                    type="range"
                    min={LINE_BEND_MIN.to_string()}
                    max={MAX_LINE_BEND_RATIO.to_string()}
                    step="0.01"
                    value={settings_value.line_bend_ratio.to_string()}
                    oninput={line_bend_input}
                />
            </div>
            <div class="control">
                <label for="tab-depth-cap">
                    { "Tab depth cap" }
                    <span class="control-value">
                        { fmt_f32(settings_value.tab_depth_cap) }
                    </span>
                </label>
                <input
                    id="tab-depth-cap"
                    type="range"
                    min={TAB_DEPTH_CAP_MIN.to_string()}
                    max={TAB_DEPTH_CAP_MAX.to_string()}
                    step="0.01"
                    value={settings_value.tab_depth_cap.to_string()}
                    oninput={tab_depth_cap_input}
                />
            </div>
            <div class="control">
                <label for="curve-detail">
                    { "Curve detail" }
                    <span class="control-value">
                        { fmt_f32(settings_value.curve_detail) }
                    </span>
                </label>
                <input
                    id="curve-detail"
                    type="range"
                    min={CURVE_DETAIL_MIN.to_string()}
                    max={CURVE_DETAIL_MAX.to_string()}
                    step="0.05"
                    value={settings_value.curve_detail.to_string()}
                    oninput={curve_detail_input}
                />
            </div>
        </>
    };
    let controls_panel = if show_dev_panel && show_controls_value {
        html! {
            <aside class="controls">
                <h2>{ "Dev Panel" }</h2>
                <p class={status_class}>{ status_label }</p>
                <details
                    class="control-group"
                    open={*puzzle_group_open}
                    ontoggle={on_puzzle_toggle}
                >
                    <summary class="control-group-title">{ "Puzzle" }</summary>
                    <div class="control-group-body">{ puzzle_controls }</div>
                </details>
                <details
                    class="control-group"
                    open={*multiplayer_group_open}
                    ontoggle={on_multiplayer_toggle}
                >
                    <summary class="control-group-title">{ "Multiplayer" }</summary>
                    <div class="control-group-body">{ multiplayer_controls }</div>
                </details>
                <details
                    class="control-group"
                    open={*graphics_group_open}
                    ontoggle={on_graphics_toggle}
                >
                    <summary class="control-group-title">{ "Graphics" }</summary>
                    <div class="control-group-body">{ graphics_controls }</div>
                </details>
                <details
                    class="control-group"
                    open={*rules_group_open}
                    ontoggle={on_rules_toggle}
                >
                    <summary class="control-group-title">{ "Rules" }</summary>
                    <div class="control-group-body">{ rules_controls }</div>
                </details>
                <details
                    class="control-group"
                    open={*shaping_group_open}
                    ontoggle={on_shaping_toggle}
                >
                    <summary class="control-group-title">{ "Piece shaping" }</summary>
                    <div class="control-group-body">{ shaping_controls }</div>
                </details>
            </aside>
        }
    } else {
        html! {}
    };
    let sync_status = if multiplayer_active {
        html! {
            <div class="sync-status" title="Server disconnected">
                { "!" }
            </div>
        }
    } else {
        html! {}
    };
    let body = html! {
        <>
            {sync_status}
            {controls_panel}
        </>
    };
    html! {
        <div class="dev-panel-root">
            {body}
        </div>
    }
}

pub(crate) fn run_dev_panel(core: Rc<AppCore>) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Some(document) = window.document() else {
        return;
    };
    let Some(root) = document.get_element_by_id("dev-panel-root") else {
        return;
    };
    let _app_handle = yew::Renderer::<App>::with_root_and_props(root, AppProps { core }).render();
}

#[cfg(test)]
mod tests {
    use super::*;
    use console_error_panic_hook::set_once as set_panic_hook;
    use gloo::timers::future::TimeoutFuture;
    use js_sys::Date;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_test::*;
    use web_sys::{Event, HtmlInputElement, HtmlSelectElement};

    wasm_bindgen_test_configure!(run_in_browser);

    fn assert_close(actual: f32, expected: f32) {
        let delta = (actual - expected).abs();
        assert!(
            delta <= 1e-6,
            "expected {:.6} got {:.6} (delta {:.6})",
            expected,
            actual,
            delta
        );
    }

    #[wasm_bindgen_test]
    fn generated_room_ids_are_valid() {
        set_panic_hook();
        for _ in 0..32 {
            let room_id = generate_room_id();
            assert_eq!(room_id.len(), ROOM_ID_LEN);
            assert!(is_valid_room_id(&room_id), "invalid room id: {room_id}");
        }
    }

    #[wasm_bindgen_test(async)]
    async fn multiplayer_warns_when_image_missing() {
        set_panic_hook();
        gloo::console::log!("mp test start");
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document.create_element("div").expect("create test root");
        root.set_id("wasm-test-root");
        document
            .body()
            .expect("body available")
            .append_child(&root)
            .expect("append test root");
        let _app_handle = yew::Renderer::<App>::with_root_and_props(
            root,
            AppProps {
                core: AppCore::new(),
            },
        )
        .render();
        gloo::console::log!("mp test rendered app");
        let start = Date::now();
        let hooks = loop {
            if let Some(hooks) = MP_TEST_HOOKS.with(|slot| slot.borrow().clone()) {
                break hooks;
            }
            if Date::now() - start > 5000.0 {
                panic!("mp hooks not set after 5s (App may not have rendered)");
            }
            TimeoutFuture::new(10).await;
        };
        gloo::console::log!("mp test hooks ready");

        (hooks.set_server_state_applied)(true);
        (hooks.set_puzzle_info)(None);
        MP_TEST_LAST_WARN.with(|slot| slot.borrow_mut().take());

        let update = RoomControlUpdate::GroupOrder { order: vec![0] };
        (hooks.send_msg)(ServerMsg::ControlUpdate {
            seq: 1,
            update,
            source: None,
            client_seq: None,
        });

        TimeoutFuture::new(0).await;
        let warn = take_mp_warn();
        assert_eq!(warn.as_deref(), Some("puzzle info not ready"));
    }

    #[wasm_bindgen_test(async)]
    async fn switching_to_online_shows_room_setup_without_reload() {
        set_panic_hook();
        boot_runtime::set_boot_state(BootState::Ready);
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document.create_element("div").expect("create test root");
        root.set_id("wasm-test-mode-root");
        document
            .body()
            .expect("body available")
            .append_child(&root)
            .expect("append test root");
        let _app_handle = yew::Renderer::<App>::with_root_and_props(
            root.clone(),
            AppProps {
                core: AppCore::new(),
            },
        )
        .render();

        let mode_select = loop {
            if let Some(select) = document
                .get_element_by_id("mode-select")
                .and_then(|element| element.dyn_into::<HtmlSelectElement>().ok())
            {
                break select;
            }
            TimeoutFuture::new(10).await;
        };
        assert_eq!(mode_select.value(), "local");

        mode_select.set_value("online");
        mode_select
            .dispatch_event(&Event::new("change").expect("change event"))
            .expect("dispatch change");
        TimeoutFuture::new(0).await;

        let room_input = loop {
            if let Some(input) = document
                .get_element_by_id("room-id-draft")
                .and_then(|element| element.dyn_into::<HtmlInputElement>().ok())
            {
                break input;
            }
            TimeoutFuture::new(10).await;
        };
        assert!(!room_input.value().trim().is_empty());
        assert_eq!(
            document
                .get_element_by_id("mode-select")
                .and_then(|element| element.dyn_into::<HtmlSelectElement>().ok())
                .expect("mode select after switch")
                .value(),
            "online"
        );
        let text = root.text_content().unwrap_or_default();
        assert!(text.contains("Create room"));
        assert!(text.contains("Join room"));

        root.remove();
    }

    #[wasm_bindgen_test]
    fn wasm_smoke() {
        set_panic_hook();
        assert_eq!(1 + 1, 2);
    }
}
