use serde::{Deserialize, Serialize};
use js_sys::encode_uri_component;
use wasm_bindgen::JsValue;
use web_sys::UrlSearchParams;

use crate::core::{InitMode, RenderSettings, RendererKind, INIT_SETTINGS_KEY, RENDER_SETTINGS_KEY};
use heddobureika_core::is_valid_room_id;

const INIT_SETTINGS_VERSION: u32 = 1;
const ROOM_SESSION_KEY: &str = "heddobureika.room.v1";
const ROOM_SESSION_VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct MultiplayerConfig {
    pub(crate) room_id: String,
    pub(crate) clear_hash: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct InitSettings {
    version: u32,
    #[serde(default)]
    renderer: RendererKind,
    #[serde(default)]
    mode: InitMode,
}

impl Default for InitSettings {
    fn default() -> Self {
        Self {
            version: INIT_SETTINGS_VERSION,
            renderer: RendererKind::default(),
            mode: InitMode::default(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct SavedRoomSession {
    version: u32,
    room_id: String,
}

#[derive(Clone, Debug)]
pub(crate) struct InitConfig {
    #[allow(dead_code)]
    pub(crate) renderer: RendererKind,
    #[allow(dead_code)]
    pub(crate) mode: InitMode,
    #[allow(dead_code)]
    pub(crate) mode_preference: InitMode,
    pub(crate) multiplayer: Option<MultiplayerConfig>,
}

pub(crate) fn load_init_config() -> InitConfig {
    let init_settings = load_init_settings();
    let render_settings = load_render_settings();
    let renderer = init_settings
        .as_ref()
        .map(|settings| settings.renderer)
        .or_else(|| render_settings.as_ref().map(|settings| settings.renderer))
        .unwrap_or_default();
    let mode_preference = init_settings
        .as_ref()
        .map(|settings| settings.mode)
        .unwrap_or_default();
    let multiplayer = load_multiplayer_config();
    let mode = if multiplayer.is_some() {
        InitMode::Online
    } else {
        InitMode::Local
    };
    InitConfig {
        renderer,
        mode,
        mode_preference,
        multiplayer,
    }
}

pub(crate) fn load_renderer_preference() -> RendererKind {
    load_init_settings()
        .map(|settings| settings.renderer)
        .or_else(|| load_render_settings().map(|settings| settings.renderer))
        .unwrap_or_default()
}

pub(crate) fn load_render_settings_with_init() -> RenderSettings {
    let mut settings = load_render_settings().unwrap_or_default();
    if let Some(init) = load_init_settings() {
        settings.renderer = init.renderer;
    }
    settings
}

pub(crate) fn save_renderer_preference(renderer: RendererKind, render_settings: &RenderSettings) {
    let mut settings = render_settings.clone();
    settings.renderer = renderer;
    save_render_settings(&settings);
    let mut init_settings = load_init_settings().unwrap_or_default();
    init_settings.renderer = renderer;
    save_init_settings(&init_settings);
}

pub(crate) fn save_mode_preference(mode: InitMode) {
    let mut settings = load_init_settings().unwrap_or_default();
    settings.mode = mode;
    save_init_settings(&settings);
}

pub(crate) fn clear_location_hash() {
    let Some(window) = web_sys::window() else {
        return;
    };
    let location = window.location();
    let path = location.pathname().unwrap_or_default();
    let search = location.search().unwrap_or_default();
    let new_url = format!("{path}{search}");
    if let Ok(history) = window.history() {
        let _ = history.replace_state_with_url(&JsValue::NULL, "", Some(&new_url));
    } else {
        let _ = location.set_hash("");
    }
}

pub(crate) fn save_room_session(room_id: &str) {
    let session = SavedRoomSession {
        version: ROOM_SESSION_VERSION,
        room_id: room_id.to_string(),
    };
    let Ok(raw) = serde_json::to_string(&session) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(ROOM_SESSION_KEY, &raw);
}

pub(crate) fn clear_room_session() {
    clear_room_session_inner();
}

pub(crate) fn default_ws_base() -> Option<String> {
    if let Some(raw) = option_env!("HEDDOBUREIKA_WS_BASE")
        .or(option_env!("TRUNK_PUBLIC_HEDDOBUREIKA_WS_BASE"))
        .or(option_env!("TRUNK_PUBLIC_WS_BASE"))
    {
        let trimmed = raw.trim();
        if !trimmed.is_empty() {
            return Some(normalize_ws_base(trimmed));
        }
    }
    let window = web_sys::window()?;
    let location = window.location();
    let host = location.host().ok()?;
    if host.trim().is_empty() {
        return None;
    }
    let protocol = location.protocol().ok()?.to_ascii_lowercase();
    let scheme = if protocol == "https:" { "wss" } else { "ws" };
    Some(format!("{scheme}://{host}/ws"))
}

pub(crate) fn build_room_ws_url(ws_base: &str, room_id: &str) -> String {
    let base = ws_base.trim_end_matches('/');
    format!("{base}/{room_id}")
}

pub(crate) fn build_room_admin_ws_url(
    ws_base: &str,
    room_id: &str,
    admin_token: &str,
) -> String {
    let base = ws_base.trim_end_matches('/');
    let token = admin_token.trim();
    if token.is_empty() {
        return format!("{base}/{room_id}");
    }
    if let Ok(params) = UrlSearchParams::new() {
        params.append("admin_token", token);
        return format!("{base}/{room_id}?{}", params.to_string());
    }
    let encoded = encode_uri_component(token)
        .as_string()
        .unwrap_or_else(|| token.to_string());
    format!("{base}/{room_id}?admin_token={encoded}")
}

pub(crate) fn save_render_settings(settings: &RenderSettings) {
    let Ok(raw) = serde_json::to_string(settings) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(RENDER_SETTINGS_KEY, &raw);
}

fn load_render_settings() -> Option<RenderSettings> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(RENDER_SETTINGS_KEY).ok()??;
    serde_json::from_str(&raw).ok()
}

fn load_init_settings() -> Option<InitSettings> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(INIT_SETTINGS_KEY).ok()??;
    let settings: InitSettings = serde_json::from_str(&raw).ok()?;
    if settings.version != INIT_SETTINGS_VERSION {
        return None;
    }
    Some(settings)
}

fn save_init_settings(settings: &InitSettings) {
    let Ok(raw) = serde_json::to_string(settings) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(INIT_SETTINGS_KEY, &raw);
}

fn load_room_session() -> Option<SavedRoomSession> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(ROOM_SESSION_KEY).ok()??;
    let session: SavedRoomSession = serde_json::from_str(&raw).ok()?;
    if session.version != ROOM_SESSION_VERSION {
        return None;
    }
    if session.room_id.trim().is_empty() {
        return None;
    }
    Some(session)
}

fn clear_room_session_inner() {
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.remove_item(ROOM_SESSION_KEY);
}

fn normalize_ws_base(raw: &str) -> String {
    let trimmed = raw.trim();
    if let Some(rest) = trimmed.strip_prefix("http://") {
        format!("ws://{rest}")
    } else if let Some(rest) = trimmed.strip_prefix("https://") {
        format!("wss://{rest}")
    } else {
        trimmed.to_string()
    }
}

fn decode_hash_value(value: &str) -> String {
    let raw = value.trim();
    if raw.is_empty() {
        return String::new();
    }
    js_sys::decode_uri_component(raw)
        .ok()
        .and_then(|decoded| decoded.as_string())
        .unwrap_or_else(|| raw.to_string())
}

fn load_multiplayer_config() -> Option<MultiplayerConfig> {
    let window = web_sys::window()?;
    let hash = window.location().hash().ok()?;
    if let Some(config) = parse_multiplayer_config_from_hash(&hash) {
        return Some(config);
    }
    let search = window.location().search().ok()?;
    if let Some(config) = parse_multiplayer_config_from_query(&search) {
        return Some(config);
    }
    load_room_session().and_then(|session| {
        let room_id = session.room_id.trim().to_string();
        if room_id.is_empty() || !is_valid_room_id(&room_id) {
            clear_room_session_inner();
            return None;
        }
        Some(MultiplayerConfig {
            room_id,
            clear_hash: false,
        })
    })
}

fn parse_multiplayer_config_from_hash(hash: &str) -> Option<MultiplayerConfig> {
    let raw = hash.trim();
    if raw.is_empty() {
        return None;
    }
    let raw = raw.trim_start_matches('#').trim();
    if raw.is_empty() || raw.eq_ignore_ascii_case("resume") {
        return None;
    }
    let mut room_id = None;
    for chunk in raw.split(';') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        let mut iter = chunk.splitn(2, '=');
        let key = iter.next().unwrap_or("").trim();
        let value = iter.next().unwrap_or("").trim();
        if key.eq_ignore_ascii_case("room") || key.eq_ignore_ascii_case("room_id") {
            let decoded = decode_hash_value(value);
            room_id = Some(decoded);
        }
    }
    let room_id = room_id?;
    let room_id = room_id.trim().to_string();
    if room_id.is_empty() || !is_valid_room_id(&room_id) {
        return None;
    }
    Some(MultiplayerConfig {
        room_id,
        clear_hash: true,
    })
}

fn parse_multiplayer_config_from_query(search: &str) -> Option<MultiplayerConfig> {
    let search = search.trim();
    if search.is_empty() {
        return None;
    }
    let params = UrlSearchParams::new_with_str(search).ok()?;
    let room = params.get("room").or_else(|| params.get("room_id"))?;
    let room_id = room.trim().to_string();
    if room_id.is_empty() || !is_valid_room_id(&room_id) {
        return None;
    }
    Some(MultiplayerConfig {
        room_id,
        clear_hash: false,
    })
}
