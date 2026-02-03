use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use js_sys::Date;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    Element, Event, File, HtmlImageElement, HtmlInputElement, HtmlSelectElement, InputEvent,
    KeyboardEvent, MouseEvent,
};
use yew::prelude::*;

use crate::app_core::AppCore;
use crate::app_builder;
use crate::app_router;
use crate::app_runtime;
use crate::boot_runtime::{self, BootState};
use crate::sync_runtime;
use crate::view_runtime;
use crate::core::*;
use crate::model::*;
#[cfg(test)]
use crate::multiplayer_bridge;
use crate::multiplayer_identity;
use crate::multiplayer_sync::MultiplayerSyncAdapter;
use crate::persisted_store;
use crate::persisted::{PrivateImageEntry, PrivateImageRefs, LOCAL_PRIVATE_SCOPE};
use crate::runtime::{SyncEvent, SyncHooks};
use heddobureika_core::{
    logical_image_size, AdminMsg, ClientId, GameSnapshot, PuzzleImageRef,
    PuzzleInfo, PuzzleSpec, RoomUpdate, ServerMsg, ASSET_CHUNK_BYTES, PRIVATE_UPLOAD_MAX_BYTES,
};
use heddobureika_core::catalog::{PuzzleCatalogEntry, PUZZLE_CATALOG};
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
    fn new(state: UseStateHandle<Option<PuzzleInfo>>, live: Rc<RefCell<Option<PuzzleInfo>>>) -> Self {
        Self { state, live }
    }

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
        }
    }

    fn set_status_hook(&mut self, hook: Rc<dyn Fn(AdminStatus)>) {
        self.status_hook = Some(hook);
    }

    fn set_upload_status_hook(&mut self, hook: Rc<dyn Fn(AdminUploadStatus, Option<String>)>) {
        self.upload_status_hook = Some(hook);
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
        let on_fail = Rc::new(move || {
            connected_on_fail.set(false);
            status_on_fail.set(AdminStatus::Failed);
            if let Some(hook) = status_hook.as_ref() {
                hook(AdminStatus::Failed);
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
        let on_server_msg = Rc::new(move |msg: ServerMsg| match msg {
            ServerMsg::AdminAck { room_id, persistence } => {
                status_on_msg.set(AdminStatus::Accepted);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Accepted);
                }
                gloo::console::log!(
                    "admin ack",
                    room_id,
                    format!("{persistence:?}")
                );
            }
            ServerMsg::Welcome { .. } => {
                status_on_msg.set(AdminStatus::Accepted);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Accepted);
                }
            }
            ServerMsg::Error { code, message } => {
                status_on_msg.set(AdminStatus::Failed);
                if let Some(hook) = status_hook.as_ref() {
                    hook(AdminStatus::Failed);
                }
                let error_message = format!("{code}: {message}");
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
            let protocol = match multiplayer_identity::build_auth_protocol(&room_id, Some(&admin_token)).await {
                Ok(protocol) => protocol,
                Err(err) => {
                    gloo::console::warn!("admin auth failed", err);
                    on_fail();
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

fn base_url_without_hash() -> Option<String> {
    let window = web_sys::window()?;
    let href = window.location().href().ok()?;
    let base = href.split('#').next().unwrap_or(&href).to_string();
    Some(base)
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

fn log_state_update(label: &str, len: usize, context: &str) {
    gloo::console::log!("state vector updated", label, len, context);
}


fn initial_render_settings() -> RenderSettings {
    #[cfg(test)]
    {
        let mut settings = RenderSettings::default();
        settings.renderer = RendererKind::Svg;
        settings
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
        settings.admin_token = if token.is_empty() { None } else { Some(token.clone()) };
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
        persisted_store::update_settings_blob(|settings| {
            match key {
                WS_DELAY_IN_KEY => settings.ws_delay.inbound_ms = value,
                WS_DELAY_OUT_KEY => settings.ws_delay.outbound_ms = value,
                WS_DELAY_JITTER_KEY => settings.ws_delay.jitter_ms = value,
                _ => {}
            }
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
    persisted_store::update_settings_blob(|settings| {
        match key {
            DEV_PANEL_GROUP_PUZZLE_KEY => settings.dev_panel.puzzle_open = value,
            DEV_PANEL_GROUP_MULTIPLAYER_KEY => settings.dev_panel.multiplayer_open = value,
            DEV_PANEL_GROUP_GRAPHICS_KEY => settings.dev_panel.graphics_open = value,
            DEV_PANEL_GROUP_RULES_KEY => settings.dev_panel.rules_open = value,
            DEV_PANEL_GROUP_SHAPING_KEY => settings.dev_panel.shaping_open = value,
            _ => {}
        }
    });
}

fn details_toggle(handle: UseStateHandle<bool>, key: &'static str) -> Callback<Event> {
    Callback::from(move |event: Event| {
        let element: Element = event.target_unchecked_into();
        let details = element
            .closest("details")
            .ok()
            .flatten()
            .unwrap_or(element);
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

fn group_transforms_from_state(
    state: &PuzzleState,
    total: usize,
) -> (Vec<(f32, f32)>, Vec<f32>) {
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

fn time_nonce(previous: u32) -> u32 {
    let now = Date::now() as u32;
    splitmix32(now ^ previous.wrapping_add(0x9E37_79B9))
}

fn parse_optional_seed(raw: &str) -> Option<u32> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (value, radix) = if let Some(rest) = trimmed.strip_prefix("0x").or_else(|| trimmed.strip_prefix("0X")) {
        (rest, 16)
    } else {
        (trimmed, 10)
    };
    u32::from_str_radix(value, radix).ok()
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
    let puzzle_dims_value =
        puzzle_info_value.as_ref().map(|info| (info.image_width, info.image_height));
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let depth_cap = settings_value
        .tab_depth_cap
        .clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
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
    let grid = grid_choices
        .get(grid_index_value)
        .copied()
        .or_else(|| grid_choices.first().copied())
        .unwrap_or(FALLBACK_GRID);
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
                <option value={index.to_string()} selected={index == grid_index_value}>
                    {label}
                </option>
            }
        })
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
    let admin_puzzle_index = use_state(|| initial_puzzle_art_index);
    let admin_puzzle_index_value = *admin_puzzle_index;
    let puzzle_art = PUZZLE_ARTS
        .get(puzzle_art_index_value)
        .copied()
        .unwrap_or(PUZZLE_ARTS[0]);
    let puzzle_art_options: Html = PUZZLE_ARTS
        .iter()
        .enumerate()
        .map(|(index, art)| {
            html! {
                <option value={index.to_string()} selected={index == puzzle_art_index_value}>
                    {art.label}
                </option>
            }
        })
        .collect();
    let admin_puzzle_options: Html = PUZZLE_ARTS
        .iter()
        .enumerate()
        .map(|(index, art)| {
            html! {
                <option value={index.to_string()} selected={index == admin_puzzle_index_value}>
                    {art.label}
                </option>
            }
        })
        .collect();
    let private_label = use_state(|| String::new());
    let private_label_value = (*private_label).clone();
    let private_error = use_state(|| None::<String>);
    let private_error_value = (*private_error).clone();
    let admin_private_error = use_state(|| None::<String>);
    let admin_private_error_value = (*admin_private_error).clone();
    let admin_private_status = use_state(|| AdminUploadStatus::Idle);
    let admin_private_status_value = *admin_private_status;
    let admin_private_status_note = use_state(|| None::<String>);
    let admin_private_status_note_value = (*admin_private_status_note).clone();
    let init_config = app_runtime::init_config();
    let multiplayer_config = use_state(|| init_config.multiplayer.clone());
    let multiplayer_config_value = (*multiplayer_config).clone();
    let admin_token_input = use_state(|| load_admin_token().unwrap_or_default());
    let admin_token_input_value = (*admin_token_input).clone();
    let admin_token_active = use_state(|| load_admin_token().unwrap_or_default());
    let admin_token_active_value = (*admin_token_active).clone();
    let admin_status = use_state(|| AdminStatus::Idle);
    let admin_status_value = *admin_status;
    let admin_seed = use_state(|| String::new());
    let admin_seed_value = (*admin_seed).clone();
    let admin_pieces_index = use_state(|| 0usize);
    let admin_pieces_index_value = *admin_pieces_index;
    let admin_socket = use_mut_ref(AdminSocket::new);
    let ws_delay_in = use_state(|| load_ws_delay_value(WS_DELAY_IN_KEY));
    let ws_delay_in_value = (*ws_delay_in).clone();
    let ws_delay_out = use_state(|| load_ws_delay_value(WS_DELAY_OUT_KEY));
    let ws_delay_out_value = (*ws_delay_out).clone();
    let ws_delay_jitter = use_state(|| load_ws_delay_value(WS_DELAY_JITTER_KEY));
    let ws_delay_jitter_value = (*ws_delay_jitter).clone();
    let puzzle_group_open = use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_PUZZLE_KEY, true));
    let multiplayer_group_open =
        use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_MULTIPLAYER_KEY, true));
    let graphics_group_open = use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_GRAPHICS_KEY, false));
    let rules_group_open = use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_RULES_KEY, false));
    let shaping_group_open = use_state(|| load_dev_panel_group_open(DEV_PANEL_GROUP_SHAPING_KEY, false));
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
    let mp_init_required_value = sync_view.init_required();
    let lock_puzzle_controls =
        !boot_ready_value || (multiplayer_active && !mp_init_required_value);
    let _ = *sync_revision;
    let tab_width_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.tab_width = value.clamp(TAB_WIDTH_MIN, TAB_WIDTH_MAX);
    });
    let tab_depth_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.tab_depth = value.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
    });
    let tab_size_scale_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.tab_size_scale = value.clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
    });
    let tab_size_min_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        let max_allowed = settings
            .tab_size_max
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_min = value.clamp(TAB_SIZE_MIN_LIMIT, max_allowed);
    });
    let tab_size_max_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        let min_allowed = settings
            .tab_size_min
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_max = value.clamp(min_allowed, TAB_SIZE_MAX_LIMIT);
    });
    let skew_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.skew_range = value.clamp(0.0, SKEW_RANGE_MAX);
    });
    let jitter_strength_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.jitter_strength = value.clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
    });
    let jitter_len_bias_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.jitter_len_bias = value.clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);
    });
    let tab_depth_cap_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.tab_depth_cap = value.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    });
    let curve_detail_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.curve_detail = value.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    });
    let variation_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
        settings.variation = value.clamp(VARIATION_MIN, VARIATION_MAX);
    });
    let line_bend_input = on_setting_change(app_core.clone(), settings.clone(), |settings, value| {
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
    let admin_puzzle_entry = PUZZLE_ARTS
        .get(admin_puzzle_index_value)
        .copied()
        .unwrap_or(PUZZLE_ARTS[0]);
    let (admin_logical_width, admin_logical_height) = logical_image_size(
        admin_puzzle_entry.width,
        admin_puzzle_entry.height,
        image_max_dim,
    );
    let mut admin_grid_choices = build_grid_choices(admin_logical_width, admin_logical_height);
    if admin_grid_choices.is_empty() {
        admin_grid_choices.push(FALLBACK_GRID);
    }
    let admin_pieces_options: Html = std::iter::once(html! {
        <option value="default" selected={admin_pieces_index_value == 0}>
            { "Default pieces" }
        </option>
    })
    .chain(admin_grid_choices.iter().enumerate().map(|(index, choice)| {
        let value = (index + 1).to_string();
        html! {
            <option value={value} selected={admin_pieces_index_value == index + 1}>
                { grid_choice_label(choice) }
            </option>
        }
    }))
    .collect();
    {
        let admin_pieces_index = admin_pieces_index.clone();
        use_effect_with(
            (admin_puzzle_index_value, image_max_dim),
            move |_| {
                admin_pieces_index.set(0);
                || ()
            },
        );
    }
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
        puzzle_info_value.as_ref().and_then(|info| match &info.image_ref {
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
    let share_link_label = if multiplayer_active { "Room link" } else { "Share link" };
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
        .or_else(|| multiplayer_config_value.as_ref().map(|config| config.room_id.clone()))
        .unwrap_or_else(|| "—".to_string());
    let mp_connection_label = if sync_view.connected() {
        "Connected"
    } else {
        "Connecting"
    };
    let admin_room_id = sync_view
        .room_id()
        .map(|room_id| room_id.to_string())
        .or_else(|| multiplayer_config_value.as_ref().map(|config| config.room_id.clone()));
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
                    } else if status == AdminUploadStatus::Done || status == AdminUploadStatus::Idle {
                        admin_private_error.set(None);
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
        use_effect_with(
            (
                admin_token_value.clone(),
                admin_room_id.clone(),
                multiplayer_active,
            ),
            move |(token, room_id, active)| {
                let cleanup = || ();
                if !*active {
                    admin_socket.borrow_mut().reset();
                    admin_status.set(AdminStatus::Idle);
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
                admin_socket
                    .borrow_mut()
                    .ensure_connected(ws_base, room_id.clone(), token.to_string());
                cleanup
            },
        );
    }
    let admin_enabled = multiplayer_active && matches!(admin_status_value, AdminStatus::Accepted);
    let admin_status_label = match admin_status_value {
        AdminStatus::Idle => "Admin token not verified",
        AdminStatus::Connecting => "Admin token: connecting...",
        AdminStatus::Accepted => "Admin token accepted",
        AdminStatus::Failed => "Admin token rejected",
    };
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
    let on_admin_change_puzzle = {
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        let admin_seed_value = admin_seed_value.clone();
        let admin_grid_choices = admin_grid_choices.clone();
        let admin_pieces_index_value = admin_pieces_index_value;
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
            let ws_base = ws_base;
            let entry = PUZZLE_ARTS
                .get(admin_puzzle_index_value)
                .copied()
                .unwrap_or(PUZZLE_ARTS[0]);
            let pieces = if admin_pieces_index_value == 0 {
                None
            } else {
                admin_grid_choices
                    .get(admin_pieces_index_value.saturating_sub(1))
                    .map(|choice| choice.target_count)
            };
            let seed = parse_optional_seed(&admin_seed_value);
            admin_socket.borrow_mut().send(
                ws_base,
                room_id.clone(),
                token.to_string(),
                AdminMsg::ChangePuzzle {
                    puzzle: PuzzleSpec {
                        image_ref: PuzzleImageRef::BuiltIn {
                            slug: entry.slug.to_string(),
                        },
                        pieces,
                        seed,
                    },
                },
            );
        })
    };
    let on_admin_private_file_input = {
        let admin_socket = admin_socket.clone();
        let admin_room_id = admin_room_id.clone();
        let admin_token_value = admin_token_active_value.clone();
        let admin_seed_value = admin_seed_value.clone();
        let admin_grid_choices = admin_grid_choices.clone();
        let admin_pieces_index_value = admin_pieces_index_value;
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
            let pieces = if admin_pieces_index_value == 0 {
                None
            } else {
                admin_grid_choices
                    .get(admin_pieces_index_value.saturating_sub(1))
                    .map(|choice| choice.target_count)
            };
            let seed = parse_optional_seed(&admin_seed_value);
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
                messages.push(AdminMsg::UploadPrivateEnd { pieces, seed });
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
            admin_socket
                .borrow_mut()
                .send(ws_base, room_id.clone(), token.to_string(), AdminMsg::Scramble { seed });
        })
    };
    let save_revision = use_state(|| 0u32);
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(|| false);
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
    let apply_puzzle_state: Rc<dyn Fn(PuzzleState, usize, f32, f32)> = {
        let bump_ui_revision = bump_ui_revision.clone();
        let z_order = z_order.clone();
        let group_anchor = group_anchor.clone();
        let group_pos = group_pos.clone();
        let group_rot = group_rot.clone();
        let group_order = group_order.clone();
        let scramble_nonce = scramble_nonce.clone();
        let puzzle_state = puzzle_state.clone();
        let app_core = app_core.clone();
        let multiplayer_active = multiplayer_active;
        Rc::new(move |next_state: PuzzleState, cols: usize, piece_width: f32, piece_height: f32| {
            let derived = derive_ui_state_from_puzzle(&next_state, cols, piece_width, piece_height);
            let next_flips = next_state.flips.clone();
            let next_connections = next_state.connections.clone();
            let next_scramble = next_state.scramble_nonce;
            log_state_update("positions", derived.positions.len(), "apply_puzzle_state");
            log_state_update("rotations", derived.rotations.len(), "apply_puzzle_state");
            log_state_update("flips", next_flips.len(), "apply_puzzle_state");
            log_state_update("connections", next_connections.len(), "apply_puzzle_state");
            let derived_z_order = derived.z_order.clone();
            bump_ui_revision();
            group_anchor.set(derived.anchor_of);
            group_pos.set(derived.group_pos);
            group_rot.set(derived.group_rot);
            group_order.set(derived.group_order);
            z_order.set(derived_z_order.clone());
            scramble_nonce.set(next_scramble);
            puzzle_state.set(next_state);
            if !multiplayer_active {
                app_core.apply_snapshot(
                    derived.positions.clone(),
                    derived.rotations.clone(),
                    next_flips,
                    next_connections,
                    derived_z_order,
                    next_scramble,
                );
            }
        })
    };
    let bump_sync_revision: Rc<dyn Fn()> = {
        let sync_revision = sync_revision.clone();
        Rc::new(move || {
            sync_revision.set(sync_revision.wrapping_add(1));
        })
    };
    let on_remote_snapshot = {
        let bump_sync_revision = bump_sync_revision.clone();
        Rc::new(move |_snapshot: GameSnapshot, _seq: u64| {
            bump_sync_revision();
        })
    };
    let on_remote_update = {
        let bump_sync_revision = bump_sync_revision.clone();
        #[cfg(test)]
        let puzzle_info = puzzle_info_store.clone();
        Rc::new(
            move |_update: RoomUpdate,
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
        let grid_index = grid_index.clone();
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
                    let desired_index = match &info.image_ref {
                        PuzzleImageRef::BuiltIn { slug } => puzzle_art_index_by_slug(slug),
                        _ => None,
                    };
                    if let Some(index) = desired_index {
                        if *puzzle_art_index != index {
                            puzzle_art_index.set(index);
                        }
                    }
                    if info.image_width > 0 && info.image_height > 0 {
                        let mut choices = build_grid_choices(info.image_width, info.image_height);
                        if choices.is_empty() {
                            choices.push(FALLBACK_GRID);
                        }
                        if let Some(index) = grid_choice_index(&choices, info.cols, info.rows) {
                            if *grid_index != index {
                                grid_index.set(index);
                            }
                        }
                    }
                }
                let cols = snapshot.grid.cols as usize;
                let rows = snapshot.grid.rows as usize;
                let total = cols * rows;
                if total == 0
                    || snapshot.core.positions.len() != total
                    || snapshot.core.rotations.len() != total
                    || snapshot.core.flips.len() != total
                    || snapshot.core.connections.len() != total
                {
                    z_order.set(snapshot.z_order.clone());
                    scramble_nonce.set(snapshot.core.scramble_nonce);
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
                    &snapshot.core.positions,
                    &snapshot.core.rotations,
                    &snapshot.core.flips,
                    &snapshot.core.connections,
                    cols,
                    rows,
                    Some(piece_order.as_slice()),
                    snapshot.core.scramble_nonce,
                );
                let derived = derive_ui_state_from_puzzle(
                    &next_state,
                    cols,
                    snapshot.piece_width,
                    snapshot.piece_height,
                );
                group_anchor.set(derived.anchor_of.clone());
                group_pos.set(derived.group_pos.clone());
                group_rot.set(derived.group_rot.clone());
                group_order.set(derived.group_order.clone());
                z_order.set(derived.z_order.clone());
                scramble_nonce.set(snapshot.core.scramble_nonce);
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
    #[cfg(test)]
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
    let mode_value = if multiplayer_active { "online" } else { "local" };
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
    let status_label = if solved_value { "Solved" } else { "In progress" };
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
    let (connections_label, border_connections_label) = if puzzle_info_value.is_some() {
        let connections_value = app_snapshot_value.core.connections.as_slice();
        if connections_value.len() == total {
            let (connected, border_connected, total_expected, border_expected) = count_connections(
                connections_value,
                grid.cols as usize,
                grid.rows as usize,
            );
            (
                format_progress(connected, total_expected),
                format_progress(border_connected, border_expected),
            )
        } else {
            ("--".to_string(), "--".to_string())
        }
    } else {
        ("--".to_string(), "--".to_string())
    };
    let on_grid_change = {
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let grid_choices_len = grid_choices.len();
        let lock_puzzle_controls = lock_puzzle_controls;
        let app_core = app_core.clone();
        let image_max_dim = image_max_dim;
        let puzzle_art = puzzle_art;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < grid_choices_len {
                    grid_index.set(value);
                    clear_saved_game();
                    if let Some(grid) = grid_choices.get(value).copied() {
                        app_builder::request_puzzle_change(
                            app_core.clone(),
                            image_max_dim,
                            puzzle_art,
                            Some(grid),
                        );
                    }
                }
            }
        })
    };
    let on_puzzle_art_change = {
        let puzzle_art_index = puzzle_art_index.clone();
        let puzzle_art_len = PUZZLE_ARTS.len();
        let lock_puzzle_controls = lock_puzzle_controls;
        let app_core = app_core.clone();
        let image_max_dim = image_max_dim;
        Callback::from(move |event: Event| {
            if lock_puzzle_controls {
                return;
            }
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < puzzle_art_len {
                    puzzle_art_index.set(value);
                    clear_saved_game();
                    let entry = PUZZLE_ARTS.get(value).copied().unwrap_or(PUZZLE_ARTS[0]);
                    app_builder::request_puzzle_change(
                        app_core.clone(),
                        image_max_dim,
                        entry,
                        None,
                    );
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
    let on_private_file_input = {
        let private_label = private_label.clone();
        let private_error = private_error.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let Some(files) = input.files() else {
                return;
            };
            let Some(file) = files.get(0) else {
                return;
            };
            private_error.set(None);
            let mime = file.type_();
            if !mime.starts_with("image/") {
                private_error.set(Some("Unsupported file type".to_string()));
                return;
            }
            let label = (*private_label).trim().to_string();
            let app_core = app_core.clone();
            let private_error = private_error.clone();
            spawn_local(async move {
                let bytes = match read_file_bytes(file.clone()).await {
                    Ok(bytes) => bytes,
                    Err(message) => {
                        private_error.set(Some(message));
                        return;
                    }
                };
                let (width, height) = match load_image_dimensions(file).await {
                    Ok(value) => value,
                    Err(message) => {
                        private_error.set(Some(message));
                        return;
                    }
                };
                let hash = sha256_hex(&bytes);
                let now = now_ms_u64();
                let size = (bytes.len() as u64).min(u32::MAX as u64) as u32;
                let entry = PrivateImageEntry {
                    bytes,
                    mime: mime.clone(),
                    width,
                    height,
                    size,
                    created_at: now,
                    last_used_at: now,
                };
                if let Err(message) = persisted_store::save_private_image(&hash, entry).await {
                    private_error.set(Some(message));
                    return;
                }
                let refs = match persisted_store::load_private_image_refs(LOCAL_PRIVATE_SCOPE).await {
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
                        return;
                    }
                };
                if let Err(message) =
                    persisted_store::save_private_image_refs(LOCAL_PRIVATE_SCOPE, refs).await
                {
                    private_error.set(Some(message));
                    return;
                }
                clear_saved_game();
                let image_ref = PuzzleImageRef::Private { hash };
                app_core.set_puzzle_with_grid(label, image_ref, (width, height), None);
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
    let on_admin_puzzle_change = {
        let admin_puzzle_index = admin_puzzle_index.clone();
        let puzzle_art_len = PUZZLE_ARTS.len();
        Callback::from(move |event: Event| {
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < puzzle_art_len {
                    admin_puzzle_index.set(value);
                }
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
    let on_admin_pieces_change = {
        let admin_pieces_index = admin_pieces_index.clone();
        Callback::from(move |event: Event| {
            let select: HtmlSelectElement = event.target_unchecked_into();
            let value = select.value();
            if value == "default" {
                admin_pieces_index.set(0);
                return;
            }
            if let Ok(index) = value.parse::<usize>() {
                admin_pieces_index.set(index);
            }
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
                let value = value.clamp(
                    WORKSPACE_PADDING_RATIO_MIN,
                    WORKSPACE_PADDING_RATIO_MAX,
                );
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
                let value = value.clamp(
                    SNAP_DISTANCE_RATIO_MIN,
                    SNAP_DISTANCE_RATIO_MAX,
                );
                snap_distance_ratio.set(value);
                app_core.set_snap_distance_ratio(value);
            }
        })
    };
    let on_rotation_toggle = {
        let rotation_enabled = rotation_enabled.clone();
        let app_snapshot = app_snapshot.clone();
        let z_order = z_order.clone();
        let apply_puzzle_state = apply_puzzle_state.clone();
        let scramble_nonce = scramble_nonce.clone();
        let puzzle_info = puzzle_info_store.clone();
        let solved = solved.clone();
        let save_revision = save_revision.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            rotation_enabled.set(enabled);
            app_core.set_rotation_enabled(enabled);
            let (positions_snapshot, rotations_snapshot, flips_snapshot, connections_snapshot) = {
                let snapshot = &*app_snapshot;
                let total = snapshot.core.positions.len();
                let rotations_snapshot = if enabled {
                    snapshot.core.rotations.clone()
                } else {
                    vec![0.0; total]
                };
                (
                    snapshot.core.positions.clone(),
                    rotations_snapshot,
                    snapshot.core.flips.clone(),
                    snapshot.core.connections.clone(),
                )
            };
            if let Some(info) = puzzle_info.get() {
                let cols = info.cols as usize;
                let rows = info.rows as usize;
                let piece_width = info.image_width as f32 / info.cols as f32;
                let piece_height = info.image_height as f32 / info.rows as f32;
                let order_snapshot = (*z_order).clone();
                let order_opt = if order_snapshot.len() == cols * rows {
                    Some(order_snapshot.as_slice())
                } else {
                    None
                };
                let (
                    _anchor_of,
                    _group_positions,
                    _group_rotations,
                    _group_order_value,
                    derived_positions,
                    derived_rotations,
                    piece_order,
                ) = rebuild_group_state(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    order_opt,
                );
                let solved_now = is_solved(
                    &derived_positions,
                    &derived_rotations,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    enabled,
                );
                let next_state = PuzzleState::rebuild_from_piece_state(
                    &derived_positions,
                    &derived_rotations,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    Some(piece_order.as_slice()),
                    *scramble_nonce,
                );
                apply_puzzle_state(next_state, cols, piece_width, piece_height);
                solved.set(solved_now);
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
                    .core
                    .positions
                    .len()
                    .max(ROTATION_LOCK_THRESHOLD_MIN);
                let rounded = value.round() as usize;
                let clamped = rounded
                    .max(ROTATION_LOCK_THRESHOLD_MIN)
                    .min(max_value);
                rotation_lock_threshold.set(clamped);
            }
        })
    };
    let on_mode_change = {
        let multiplayer_active = multiplayer_active;
        Callback::from(move |event: Event| {
            let input: HtmlSelectElement = event.target_unchecked_into();
            let next_mode = match input.value().as_str() {
                "online" => InitMode::Online,
                "local" => InitMode::Local,
                _ => InitMode::Local,
            };
            let current_mode = if multiplayer_active {
                InitMode::Online
            } else {
                InitMode::Local
            };
            if next_mode == current_mode {
                return;
            }
            app_router::save_mode_preference(next_mode);
            if matches!(next_mode, InitMode::Local) {
                app_router::clear_room_session();
                app_router::clear_location_hash();
            }
            if let Some(window) = web_sys::window() {
                let _ = window.location().reload();
            }
        })
    };
    let on_leave_room = {
        Callback::from(move |_| {
            app_router::clear_room_session();
            app_router::save_mode_preference(InitMode::Local);
            app_router::clear_location_hash();
            if let Some(window) = web_sys::window() {
                let _ = window.location().reload();
            }
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
            if !enabled {
            }
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
                let value = value
                    .clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
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
                let value = value
                    .clamp(min_value, AUTO_PAN_INNER_RATIO_MAX);
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
        use_effect_with(
            show_controls_value,
            move |show_controls_value| {
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
                            }
                        }
                    },
                );
                || drop(listener)
            },
        );
    }

    // Legacy SVG/WGPU input + image scaling removed.

    let (on_scramble, on_solve, on_solve_rotation, on_unflip, scramble_disabled) =
        if let Some((width, height)) = puzzle_dims_value {
            let width_f = width as f32;
            let height_f = height as f32;
            let layout = app_snapshot_value.layout;
            let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
            let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
            let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
            let puzzle_view_width = layout.view_width / puzzle_scale;
            let puzzle_view_height = layout.view_height / puzzle_scale;
            let piece_width = width_f / grid.cols as f32;
            let piece_height = height_f / grid.rows as f32;
            let max_depth = piece_width.max(piece_height) * depth_cap;
            let bend_ratio = settings_value
                .line_bend_ratio
                .clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
            let max_bend = piece_width.max(piece_height) * bend_ratio;
            let mask_pad = (max_depth + max_bend).ceil();

            let on_scramble = {
                let apply_puzzle_state = apply_puzzle_state.clone();
                let scramble_nonce = scramble_nonce.clone();
                let solved = solved.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let total = cols * rows;
                    if total == 0 {
                        return;
                    }
                    let next_nonce = time_nonce(*scramble_nonce);
                    let seed = scramble_seed(PUZZLE_SEED, next_nonce, cols, rows);
                    let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
                    let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
                    let (next_positions, order) = scramble_layout(
                        seed,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        puzzle_view_min_x,
                        puzzle_view_min_y,
                        puzzle_view_width,
                        puzzle_view_height,
                        mask_pad,
                    );
                    let next_rotations =
                        scramble_rotations(rotation_seed, total, rotation_enabled_value);
                    let next_connections = vec![[false; 4]; total];
                    let next_flips = scramble_flips(flip_seed, total, FLIP_CHANCE);
                    let next_state = PuzzleState::rebuild_from_piece_state(
                        &next_positions,
                        &next_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        Some(order.as_slice()),
                        next_nonce,
                    );
                    apply_puzzle_state(next_state, cols, piece_width, piece_height);
                    solved.set(false);
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            let on_solve = {
                let apply_puzzle_state = apply_puzzle_state.clone();
                let scramble_nonce = scramble_nonce.clone();
                let solved = solved.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let total = cols * rows;
                    if total == 0 {
                        return;
                    }
                    let mut next_positions = Vec::with_capacity(total);
                    for row in 0..rows {
                        for col in 0..cols {
                            next_positions.push((
                                col as f32 * piece_width,
                                row as f32 * piece_height,
                            ));
                        }
                    }
                    let order: Vec<usize> = (0..total).collect();
                    let next_rotations = vec![0.0; total];
                    let next_connections = build_full_connections(cols, rows);
                    let next_flips = vec![false; total];
                    let next_state = PuzzleState::rebuild_from_piece_state(
                        &next_positions,
                        &next_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        Some(order.as_slice()),
                        *scramble_nonce,
                    );
                    apply_puzzle_state(next_state, cols, piece_width, piece_height);
                    solved.set(true);
                    save_revision.set(save_revision.wrapping_add(1));
                })
            };
            let on_solve_rotation = {
                let app_snapshot = app_snapshot.clone();
                let z_order = z_order.clone();
                let apply_puzzle_state = apply_puzzle_state.clone();
                let scramble_nonce = scramble_nonce.clone();
                let solved = solved.clone();
                let save_revision = save_revision.clone();
                Callback::from(move |_: MouseEvent| {
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let total = cols * rows;
                    if total == 0 {
                        return;
                    }
                    let (positions_snapshot, flips_snapshot, connections_snapshot) = {
                        let snapshot = &*app_snapshot;
                        (
                            snapshot.core.positions.clone(),
                            snapshot.core.flips.clone(),
                            snapshot.core.connections.clone(),
                        )
                    };
                    let zeroed = vec![0.0; total];
                    let order_snapshot = (*z_order).clone();
                    let order_opt = if order_snapshot.len() == total {
                        Some(order_snapshot.as_slice())
                } else {
                    None
                };
                let next_state = PuzzleState::rebuild_from_piece_state(
                    &positions_snapshot,
                    &zeroed,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    order_opt,
                    *scramble_nonce,
                );
                let derived = derive_ui_state_from_puzzle(
                    &next_state,
                    cols,
                    piece_width,
                    piece_height,
                );
                let solved_now = is_solved(
                    &derived.positions,
                    &derived.rotations,
                    &next_state.flips,
                    &next_state.connections,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                apply_puzzle_state(next_state, cols, piece_width, piece_height);
                solved.set(solved_now);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_unflip = {
            let app_snapshot = app_snapshot.clone();
            let z_order = z_order.clone();
            let apply_puzzle_state = apply_puzzle_state.clone();
            let scramble_nonce = scramble_nonce.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let (positions_snapshot, rotations_snapshot, connections_snapshot) = {
                    let snapshot = &*app_snapshot;
                    (
                        snapshot.core.positions.clone(),
                        snapshot.core.rotations.clone(),
                        snapshot.core.connections.clone(),
                    )
                };
                let cleared = vec![false; total];
                let order_snapshot = (*z_order).clone();
                let order_opt = if order_snapshot.len() == total {
                    Some(order_snapshot.as_slice())
                } else {
                    None
                };
                let next_state = PuzzleState::rebuild_from_piece_state(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &cleared,
                    &connections_snapshot,
                    cols,
                    rows,
                    order_opt,
                    *scramble_nonce,
                );
                let derived = derive_ui_state_from_puzzle(
                    &next_state,
                    cols,
                    piece_width,
                    piece_height,
                );
                let solved_now = is_solved(
                    &derived.positions,
                    &derived.rotations,
                    &next_state.flips,
                    &next_state.connections,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                apply_puzzle_state(next_state, cols, piece_width, piece_height);
                solved.set(solved_now);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        (
            on_scramble,
            on_solve,
            on_solve_rotation,
            on_unflip,
            false,
        )
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
    let on_multiplayer_toggle =
        details_toggle(multiplayer_group_open.clone(), DEV_PANEL_GROUP_MULTIPLAYER_KEY);
    let on_graphics_toggle = details_toggle(graphics_group_open.clone(), DEV_PANEL_GROUP_GRAPHICS_KEY);
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
                { if !multiplayer_active {
                    html! {
                        <select
                            id="puzzle-art-select"
                            onchange={on_puzzle_art_change}
                        >
                            {puzzle_art_options}
                        </select>
                    }
                } else {
                    html! {}
                }}
            </div>
            <div class="control">
                <label>
                    { "Grid" }
                    <span class="control-value">{ grid_label }</span>
                </label>
                { if !multiplayer_active {
                    html! {
                        <select
                            id="grid-select"
                            onchange={on_grid_change}
                        >
                            {grid_options}
                        </select>
                    }
                } else {
                    html! {}
                }}
            </div>
            { if !multiplayer_active {
                html! {
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
                                onchange={on_private_file_input}
                                disabled={lock_puzzle_controls}
                            />
                        </div>
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
                                disabled={lock_puzzle_controls}
                            />
                        </div>
                        { if let Some(message) = private_error_value.clone() {
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
                                onclick={on_scramble}
                                disabled={scramble_disabled}
                            >
                                { "Scramble" }
                            </button>
                        </div>
                        <div class="control">
                            <button
                                class="control-button"
                                type="button"
                                onclick={on_solve}
                                disabled={scramble_disabled}
                            >
                                { "Solve" }
                            </button>
                        </div>
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
                    <option value="local" selected={!multiplayer_active}>
                        { "Local" }
                    </option>
                    <option value="online" selected={multiplayer_active}>
                        { "Online" }
                    </option>
                </select>
            </div>
            { if multiplayer_active {
                html! {
                    <div class="control">
                        <button
                            class="control-button"
                            type="button"
                            onclick={on_leave_room}
                        >
                            { "Leave room" }
                        </button>
                    </div>
                }
            } else {
                html! {}
            }}
            { if multiplayer_active {
                html! {
                    <>
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
                        { if admin_enabled {
                            html! {
                                <>
                                    <div class="control">
                                        <label for="admin-puzzle">
                                            { "Admin puzzle" }
                                        </label>
                                        <select
                                            id="admin-puzzle"
                                            onchange={on_admin_puzzle_change}
                                        >
                                            {admin_puzzle_options}
                                        </select>
                                    </div>
                                    <div class="control">
                                        <label for="admin-pieces">
                                            { "Admin pieces" }
                                        </label>
                                        <select
                                            id="admin-pieces"
                                            onchange={on_admin_pieces_change}
                                        >
                                            {admin_pieces_options}
                                        </select>
                                    </div>
                                    <div class="control">
                                        <label for="admin-seed">
                                            { "Admin seed (optional)" }
                                        </label>
                                        <input
                                            id="admin-seed"
                                            type="text"
                                            value={admin_seed_value.clone()}
                                            placeholder="0x1234"
                                            oninput={on_admin_seed_input}
                                        />
                                    </div>
                                    <div class="control">
                                        <button
                                            class="control-button"
                                            type="button"
                                            onclick={on_admin_change_puzzle}
                                        >
                                            { "Admin: Change puzzle" }
                                        </button>
                                    </div>
                                    <div class="control">
                                        <label for="admin-private-file">
                                            { "Admin private image file" }
                                        </label>
                                        <input
                                            id="admin-private-file"
                                            type="file"
                                            accept="image/*"
                                            onchange={on_admin_private_file_input}
                                            disabled={admin_private_upload_busy}
                                        />
                                    </div>
                                    <div class="control">
                                        <label>
                                            { "Admin upload status" }
                                            <span class="control-value">{ admin_private_status_display.clone() }</span>
                                        </label>
                                    </div>
                                    { if let Some(message) = admin_private_error_value.clone() {
                                        html! {
                                            <div class="control">
                                                <label>
                                                    { "Admin private image error" }
                                                    <span class="control-value">{ message }</span>
                                                </label>
                                            </div>
                                        }
                                    } else {
                                        html! {}
                                    }}
                                    <div class="control">
                                        <button
                                            class="control-button"
                                            type="button"
                                            onclick={on_admin_scramble}
                                        >
                                            { "Admin: Scramble" }
                                        </button>
                                    </div>
                                </>
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
                    </>
                }
            } }
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
    let _app_handle = yew::Renderer::<App>::with_root_and_props(
        root,
        AppProps {
            core,
        },
    )
    .render();
}

#[cfg(test)]
mod tests {
    use super::*;
    use console_error_panic_hook::set_once as set_panic_hook;
    use gloo::timers::future::TimeoutFuture;
    use js_sys::Date;
    use wasm_bindgen_test::*;

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
    fn align_group_sets_uniform_rotation() {
        let cols = 3usize;
        let piece_width = 100.0;
        let piece_height = 100.0;
        let mut positions = vec![(0.0, 0.0); 4];
        let mut rotations = vec![0.0; 4];
        rotations[0] = 11.027;
        rotations[1] = 359.504;
        positions[0] = (12.0, -44.0);
        positions[1] = (180.0, 36.0);
        let members = vec![0usize, 1usize];
        let anchor_id = 0usize;
        let anchor_center = (50.0, 50.0);
        let target_rot = 11.027;

        align_group_to_anchor(
            &mut positions,
            &mut rotations,
            &members,
            anchor_id,
            anchor_center,
            target_rot,
            cols,
            piece_width,
            piece_height,
        );

        for id in &members {
            assert_close(rotations[*id], normalize_angle(target_rot));
        }

        let base_rotation = rotations[members[0]];
        for id in &members[1..] {
            assert_close(rotations[*id], base_rotation);
        }

        for id in 0..rotations.len() {
            if !members.contains(&id) {
                assert_close(rotations[id], 0.0);
            }
        }

        let (dx, dy) = rotate_vec(piece_width, 0.0, normalize_angle(target_rot));
        let expected_center = (anchor_center.0 + dx, anchor_center.1 + dy);
        let pos = positions[1];
        let center = (pos.0 + piece_width * 0.5, pos.1 + piece_height * 0.5);
        assert_close(center.0, expected_center.0);
        assert_close(center.1, expected_center.1);
    }

    #[wasm_bindgen_test(async)]
    async fn multiplayer_warns_when_image_missing() {
        set_panic_hook();
        gloo::console::log!("mp test start");
        let document = web_sys::window()
            .and_then(|window| window.document())
            .expect("document available");
        let root = document
            .create_element("div")
            .expect("create test root");
        root.set_id("wasm-test-root");
        document
            .body()
            .expect("body available")
            .append_child(&root)
            .expect("append test root");
        let _app_handle = yew::Renderer::<App>::with_root(root).render();
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

        let update = RoomUpdate::GroupTransform {
            anchor_id: 0,
            pos: (10.0, 12.0),
            rot_deg: 0.0,
        };
        (hooks.send_msg)(ServerMsg::Update {
            seq: 1,
            update,
            source: None,
            client_seq: None,
        });

        TimeoutFuture::new(0).await;
        let warn = take_mp_warn();
        assert_eq!(warn.as_deref(), Some("puzzle info not ready"));
    }

    #[wasm_bindgen_test]
    fn wasm_smoke() {
        set_panic_hook();
        assert_eq!(1 + 1, 2);
    }
}
