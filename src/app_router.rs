use wasm_bindgen::JsValue;
use web_sys::UrlSearchParams;

use crate::core::{InitMode, RenderSettings, RendererKind};
use crate::persisted::BootRoomSession;
use crate::persisted_store;
use heddobureika_core::is_valid_room_id;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct MultiplayerConfig {
    pub(crate) room_id: String,
    pub(crate) clear_hash: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct InitConfig {
    pub(crate) renderer: RendererKind,
    pub(crate) mode: InitMode,
    pub(crate) mode_preference: InitMode,
    pub(crate) multiplayer: Option<MultiplayerConfig>,
}

pub(crate) fn load_init_config() -> InitConfig {
    let boot = persisted_store::boot_record();
    let renderer = boot.renderer_preference;
    let mode_preference = boot.mode_preference;
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
    persisted_store::boot_record().renderer_preference
}

pub(crate) fn load_render_settings_with_init() -> RenderSettings {
    let mut settings = persisted_store::settings_blob().render_settings;
    settings.renderer = persisted_store::boot_record().renderer_preference;
    settings
}

pub(crate) fn save_renderer_preference(renderer: RendererKind, render_settings: &RenderSettings) {
    persisted_store::update_settings_blob(|settings| {
        settings.render_settings = render_settings.clone();
        settings.render_settings.renderer = renderer;
    });
    persisted_store::update_boot_record(|record| {
        record.renderer_preference = renderer;
    });
}

pub(crate) fn save_mode_preference(mode: InitMode) {
    persisted_store::update_boot_record(|record| {
        record.mode_preference = mode;
    });
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
    let room_id = room_id.trim();
    if room_id.is_empty() {
        return;
    }
    persisted_store::update_boot_record(|record| {
        record.room_session = Some(BootRoomSession {
            room_id: room_id.to_string(),
        });
    });
}

pub(crate) fn clear_room_session() {
    persisted_store::update_boot_record(|record| {
        record.room_session = None;
    });
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

pub(crate) fn save_render_settings(settings: &RenderSettings) {
    let settings = settings.clone();
    persisted_store::update_settings_blob(|blob| {
        blob.render_settings = settings;
    });
}

fn load_room_session() -> Option<BootRoomSession> {
    persisted_store::boot_record().room_session
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
            clear_room_session();
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
