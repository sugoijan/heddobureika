use crate::core::{InitMode, RenderSettings, RendererKind, ThemeMode};

pub(crate) const BOOT_RECORD_VERSION: u32 = 2;
pub(crate) const SETTINGS_VERSION: u32 = 1;

pub(crate) const BOOT_RECORD_KEY: &str = "boot.v2";
pub(crate) const SETTINGS_KEY: &str = "settings.v1";
pub(crate) const SNAPSHOT_KEY: &str = "snapshot.v1";

pub(crate) const LOCAL_PRIVATE_SCOPE: &str = "local";

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct BootPuzzleSelection {
    pub(crate) puzzle_slug: String,
    pub(crate) cols: u32,
    pub(crate) rows: u32,
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct BootRoomSession {
    pub(crate) room_id: String,
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct BootRecord {
    pub(crate) version: u32,
    pub(crate) mode_preference: InitMode,
    pub(crate) renderer_preference: RendererKind,
    pub(crate) last_puzzle: Option<BootPuzzleSelection>,
    pub(crate) room_session: Option<BootRoomSession>,
}

impl Default for BootRecord {
    fn default() -> Self {
        Self {
            version: BOOT_RECORD_VERSION,
            mode_preference: InitMode::default(),
            renderer_preference: RendererKind::default(),
            last_puzzle: None,
            room_session: None,
        }
    }
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct WsDelaySettings {
    pub(crate) inbound_ms: Option<u32>,
    pub(crate) outbound_ms: Option<u32>,
    pub(crate) jitter_ms: Option<u32>,
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct PrivateImageEntry {
    pub(crate) bytes: Vec<u8>,
    pub(crate) mime: String,
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) size: u32,
    pub(crate) created_at: u64,
    pub(crate) last_used_at: u64,
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct PrivateImageRefs {
    pub(crate) hashes: Vec<String>,
    pub(crate) updated_at: u64,
}

impl Default for WsDelaySettings {
    fn default() -> Self {
        Self {
            inbound_ms: None,
            outbound_ms: None,
            jitter_ms: None,
        }
    }
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct DevPanelSettings {
    pub(crate) puzzle_open: bool,
    pub(crate) multiplayer_open: bool,
    pub(crate) graphics_open: bool,
    pub(crate) rules_open: bool,
    pub(crate) shaping_open: bool,
}

impl Default for DevPanelSettings {
    fn default() -> Self {
        Self {
            puzzle_open: true,
            multiplayer_open: true,
            graphics_open: false,
            rules_open: false,
            shaping_open: false,
        }
    }
}

#[derive(Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub(crate) struct SettingsBlob {
    pub(crate) version: u32,
    pub(crate) render_settings: RenderSettings,
    pub(crate) theme_mode: ThemeMode,
    pub(crate) admin_token: Option<String>,
    pub(crate) ws_delay: WsDelaySettings,
    pub(crate) dev_panel: DevPanelSettings,
}

impl Default for SettingsBlob {
    fn default() -> Self {
        Self {
            version: SETTINGS_VERSION,
            render_settings: RenderSettings::default(),
            theme_mode: ThemeMode::System,
            admin_token: None,
            ws_delay: WsDelaySettings::default(),
            dev_panel: DevPanelSettings::default(),
        }
    }
}
