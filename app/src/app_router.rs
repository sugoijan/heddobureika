use wasm_bindgen::JsValue;
use web_sys::UrlSearchParams;

use crate::core::{InitMode, RenderSettings, RendererKind};
use crate::persisted::BootRoomSession;
use crate::persisted_store;
use heddobureika_core::is_valid_room_id;

#[derive(Clone, Default, PartialEq)]
pub(crate) enum MultiplayerConnection {
    #[default]
    Server,
    P2pHost,
    #[cfg_attr(not(feature = "experimental-p2p"), allow(dead_code))]
    P2pGuest {
        invite: String,
    },
}

impl std::fmt::Debug for MultiplayerConnection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Server => "Server",
            Self::P2pHost => "P2pHost",
            Self::P2pGuest { .. } => "P2pGuest",
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct MultiplayerConfig {
    pub(crate) room_id: String,
    pub(crate) connection: MultiplayerConnection,
    pub(crate) clear_hash: bool,
    /// True when this config was restored from a previously saved session
    /// rather than an explicit join (hash/query link or UI action). A resumed
    /// room that the server now reports as "not activated" must have expired,
    /// so the scheduler treats that as terminal; an explicit join to a
    /// not-yet-activated room is instead retried (it may be activating).
    pub(crate) resumed: bool,
}

impl MultiplayerConfig {
    pub(crate) fn is_p2p(&self) -> bool {
        !matches!(self.connection, MultiplayerConnection::Server)
    }
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
    persist_renderer_preference(renderer);
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

#[cfg(target_arch = "wasm32")]
pub(crate) fn watch_p2p_invites() {
    let Some(window) = web_sys::window() else {
        return;
    };
    // Navigating to a replacement invite in the same tab only changes the
    // fragment. Reboot the app so the old transport and pending input are
    // discarded and the new capability goes through the normal route parser.
    gloo::events::EventListener::new(&window.clone(), "hashchange", move |_| {
        if let Ok(hash) = window.location().hash() {
            if hash.starts_with("#p2p=") {
                let _ = window.location().reload();
            }
        }
    })
    .forget();
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

/// HTTP(S) URL for a plain (non-upgrade) GET against a room. The server answers
/// this with the room's liveness status, which the retry scheduler uses to tell
/// a gone/expired room from a transient failure — the WebSocket handshake hides
/// its HTTP status from the browser, so we ask over regular HTTP instead.
pub(crate) fn build_room_probe_url(room_id: &str) -> Option<String> {
    let ws_base = default_ws_base()?;
    let http_base = if let Some(rest) = ws_base.strip_prefix("wss://") {
        format!("https://{rest}")
    } else if let Some(rest) = ws_base.strip_prefix("ws://") {
        format!("http://{rest}")
    } else {
        ws_base
    };
    Some(build_room_ws_url(&http_base, room_id))
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

#[cfg(target_arch = "wasm32")]
const RENDERER_PREF_KEY: &str = "hb.renderer_preference";

#[cfg(target_arch = "wasm32")]
fn persist_renderer_preference(renderer: RendererKind) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(Some(storage)) = window.local_storage() else {
        return;
    };
    let value = match renderer {
        RendererKind::Svg => "svg",
        RendererKind::Wgpu => "wgpu",
    };
    let _ = storage.set_item(RENDERER_PREF_KEY, value);
}

#[cfg(not(target_arch = "wasm32"))]
fn persist_renderer_preference(_renderer: RendererKind) {}

pub(crate) fn p2p_link_error() -> Option<String> {
    let hash = web_sys::window()?.location().hash().ok()?;
    let value = hash.strip_prefix("#p2p=")?;
    #[cfg(feature = "experimental-p2p")]
    {
        heddobureika_p2p::Invite::parse(value).err()
    }
    #[cfg(not(feature = "experimental-p2p"))]
    {
        let _ = value;
        Some("P2P is unavailable in this version of the app".into())
    }
}

fn load_multiplayer_config() -> Option<MultiplayerConfig> {
    let window = web_sys::window()?;
    let hash = window.location().hash().ok()?;
    if let Some(_value) = hash.strip_prefix("#p2p=") {
        #[cfg(feature = "experimental-p2p")]
        if let Ok(invite) = heddobureika_p2p::Invite::parse(_value) {
            return Some(MultiplayerConfig {
                room_id: invite.room_id,
                clear_hash: false,
                resumed: false,
                connection: MultiplayerConnection::P2pGuest {
                    invite: _value.to_string(),
                },
            });
        }
        return None;
    }
    if let Some(config) = parse_multiplayer_config_from_hash(&hash) {
        return Some(config);
    }
    let search = window.location().search().ok()?;
    if let Some(config) = parse_multiplayer_config_from_query(&search) {
        return Some(config);
    }
    #[cfg(feature = "experimental-p2p")]
    if let Some(room_id) = crate::p2p_progress::active_room_id() {
        return Some(MultiplayerConfig {
            room_id,
            connection: MultiplayerConnection::P2pHost,
            clear_hash: false,
            resumed: true,
        });
    }
    load_room_session().and_then(|session| {
        let room_id = session.room_id.trim().to_string();
        if room_id.is_empty() || !is_valid_room_id(&room_id) {
            clear_room_session();
            return None;
        }
        Some(MultiplayerConfig {
            connection: MultiplayerConnection::Server,
            room_id,
            clear_hash: false,
            resumed: true,
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
        connection: MultiplayerConnection::Server,
        room_id,
        clear_hash: true,
        resumed: false,
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
        connection: MultiplayerConnection::Server,
        room_id,
        clear_hash: false,
        resumed: false,
    })
}
