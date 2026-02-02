use crate::app_core::AppCore;
use crate::app_router;
use crate::app_runtime;
use crate::boot_runtime::{self, BootState};
use crate::sync_runtime;
use crate::core::{RendererKind, RenderSettings, GridChoice, InitMode};
use crate::local_snapshot::load_local_snapshot;
use crate::persisted::BootPuzzleSelection;
use crate::persisted_store;
use web_sys::window;
use js_sys::decode_uri_component;
use heddobureika_core::catalog::{
    logical_image_size, puzzle_by_label, puzzle_by_slug, puzzle_by_src, PuzzleCatalogEntry,
    DEFAULT_PUZZLE_SLUG, PUZZLE_CATALOG,
};
use crate::core::{
    best_grid_for_count, build_grid_choices, grid_choice_index, DEFAULT_TARGET_COUNT,
    FALLBACK_GRID, IMAGE_MAX_DIMENSION_MAX, IMAGE_MAX_DIMENSION_MIN, PUZZLE_SEED,
    scramble_nonce_from_seed,
};
use std::rc::Rc;
use wasm_bindgen_futures::spawn_local;

#[cfg(target_arch = "wasm32")]
use crate::multiplayer_bridge;

#[cfg(target_arch = "wasm32")]
use crate::wgpu_app;

#[cfg(target_arch = "wasm32")]
use crate::svg_app;

#[cfg(feature = "dev-panel-yew")]
use crate::yew_app;

#[derive(Clone, Copy)]
enum HashRouteMode {
    Resume,
    Puzzle {
        entry: PuzzleCatalogEntry,
        pieces: Option<u32>,
        seed: Option<u32>,
    },
}

#[derive(Clone, Copy)]
struct HashRoute {
    mode: HashRouteMode,
    clear_hash: bool,
}

#[derive(Clone, Copy)]
struct BootSelection {
    entry: PuzzleCatalogEntry,
    grid_override: Option<GridChoice>,
    seed: Option<u32>,
    clear_hash: bool,
    force_new: bool,
}

fn decode_hash_value(value: &str) -> String {
    let raw = value.trim();
    if raw.is_empty() {
        return String::new();
    }
    decode_uri_component(raw)
        .ok()
        .and_then(|decoded| decoded.as_string())
        .unwrap_or_else(|| raw.to_string())
}

fn parse_seed_value(value: &str) -> Option<u32> {
    let raw = value.trim();
    if raw.is_empty() {
        return None;
    }
    let normalized = raw.replace('_', "");
    let trimmed = normalized.trim();
    if let Some(hex) = trimmed.strip_prefix("0x").or_else(|| trimmed.strip_prefix("0X")) {
        u32::from_str_radix(hex, 16).ok()
    } else {
        trimmed.parse::<u32>().ok()
    }
}

fn parse_hash_route() -> Option<HashRoute> {
    let hash = window()?.location().hash().ok()?;
    let raw = hash.trim();
    if raw.is_empty() {
        return None;
    }
    let raw = raw.trim_start_matches('#').trim();
    if raw.is_empty() {
        return None;
    }
    if raw.eq_ignore_ascii_case("resume") {
        return Some(HashRoute {
            mode: HashRouteMode::Resume,
            clear_hash: true,
        });
    }
    let mut puzzle_entry = None;
    let mut pieces = None;
    let mut seed = None;
    let mut saw_param = false;
    let mut saw_known = false;
    for chunk in raw.split(';') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        saw_param = true;
        let mut iter = chunk.splitn(2, '=');
        let key = iter.next().unwrap_or("").trim();
        let value = iter.next().unwrap_or("").trim();
        if key.eq_ignore_ascii_case("puzzle") {
            saw_known = true;
            let decoded = decode_hash_value(value);
            puzzle_entry = puzzle_by_slug(&decoded)
                .or_else(|| puzzle_by_label(&decoded))
                .or_else(|| puzzle_by_src(&decoded))
                .copied();
        } else if key.eq_ignore_ascii_case("pieces") {
            saw_known = true;
            if let Ok(parsed) = value.parse::<u32>() {
                if parsed > 0 {
                    pieces = Some(parsed);
                }
            }
        } else if key.eq_ignore_ascii_case("seed") {
            saw_known = true;
            seed = parse_seed_value(value);
        }
    }
    if let Some(entry) = puzzle_entry {
        return Some(HashRoute {
            mode: HashRouteMode::Puzzle { entry, pieces, seed },
            clear_hash: true,
        });
    }
    if saw_param && saw_known {
        return Some(HashRoute {
            mode: HashRouteMode::Resume,
            clear_hash: true,
        });
    }
    None
}

fn load_saved_puzzle_selection() -> Option<BootPuzzleSelection> {
    let selection = persisted_store::boot_record().last_puzzle?;
    if selection.puzzle_src.trim().is_empty() || selection.cols == 0 || selection.rows == 0 {
        return None;
    }
    Some(selection)
}

fn clear_saved_puzzle_selection() {
    persisted_store::update_boot_record(|record| {
        record.last_puzzle = None;
    });
}

fn save_puzzle_selection(entry: PuzzleCatalogEntry, grid: GridChoice) {
    if entry.src.trim().is_empty() || grid.cols == 0 || grid.rows == 0 {
        return;
    }
    let selection = BootPuzzleSelection {
        puzzle_src: entry.src.to_string(),
        cols: grid.cols,
        rows: grid.rows,
    };
    persisted_store::update_boot_record(|record| {
        record.last_puzzle = Some(selection.clone());
    });
}

fn default_grid_choice(width: u32, height: u32) -> GridChoice {
    let choices = build_grid_choices(width, height);
    if choices.is_empty() {
        return FALLBACK_GRID;
    }
    choices
        .iter()
        .find(|choice| choice.target_count == DEFAULT_TARGET_COUNT)
        .copied()
        .unwrap_or_else(|| choices[0])
}

fn resolve_grid_override(
    width: u32,
    height: u32,
    cols: u32,
    rows: u32,
) -> Option<GridChoice> {
    let choices = build_grid_choices(width, height);
    let index = grid_choice_index(&choices, cols, rows)?;
    choices.get(index).copied()
}

fn resolve_boot_selection() -> Option<BootSelection> {
    if let Some(route) = parse_hash_route() {
        match route.mode {
            HashRouteMode::Puzzle { entry, pieces, seed } => {
                let grid_override = pieces.and_then(|target| best_grid_for_count(entry.width, entry.height, target));
                return Some(BootSelection {
                    entry,
                    grid_override,
                    seed,
                    clear_hash: route.clear_hash,
                    force_new: true,
                });
            }
            HashRouteMode::Resume => {
                return resolve_saved_selection(route.clear_hash);
            }
        }
    }
    resolve_saved_selection(false)
}

fn resolve_saved_selection(clear_hash: bool) -> Option<BootSelection> {
    if let Some(snapshot) = load_local_snapshot() {
        if let Some(entry) = puzzle_by_src(&snapshot.puzzle.image_src)
            .or_else(|| puzzle_by_label(&snapshot.puzzle.label))
            .copied()
        {
            let grid_override = resolve_grid_override(
                snapshot.puzzle.image_width,
                snapshot.puzzle.image_height,
                snapshot.puzzle.cols,
                snapshot.puzzle.rows,
            );
            return Some(BootSelection {
                entry,
                grid_override,
                seed: None,
                clear_hash,
                force_new: false,
            });
        }
    }
    if let Some(selection) = load_saved_puzzle_selection() {
        if let Some(entry) = puzzle_by_src(&selection.puzzle_src).copied() {
            let grid_override = resolve_grid_override(
                entry.width,
                entry.height,
                selection.cols,
                selection.rows,
            );
            return Some(BootSelection {
                entry,
                grid_override,
                seed: None,
                clear_hash,
                force_new: false,
            });
        }
    }
    puzzle_by_slug(DEFAULT_PUZZLE_SLUG)
        .or_else(|| PUZZLE_CATALOG.first())
        .copied()
        .map(|entry| BootSelection {
            entry,
            grid_override: None,
            seed: None,
            clear_hash,
            force_new: false,
        })
}

fn apply_selection(core: Rc<AppCore>, image_max_dim: u32, selection: BootSelection) {
    let max_dim = image_max_dim.clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
    let (logical_w, logical_h) =
        logical_image_size(selection.entry.width, selection.entry.height, max_dim);
    let grid = selection
        .grid_override
        .unwrap_or_else(|| default_grid_choice(logical_w, logical_h));
    let scramble_nonce = selection
        .seed
        .map(|seed| scramble_nonce_from_seed(PUZZLE_SEED, seed, grid.cols as usize, grid.rows as usize));
    core.set_puzzle_with_grid_with_nonce(
        selection.entry.label.to_string(),
        selection.entry.src.to_string(),
        (logical_w, logical_h),
        Some(grid),
        scramble_nonce,
    );
    save_puzzle_selection(selection.entry, grid);
}

pub(crate) fn request_puzzle_change(
    core: Rc<AppCore>,
    image_max_dim: u32,
    entry: PuzzleCatalogEntry,
    grid_override: Option<GridChoice>,
) {
    let selection = BootSelection {
        entry,
        grid_override,
        seed: None,
        clear_hash: false,
        force_new: false,
    };
    apply_selection(core, image_max_dim, selection);
}

fn boot_local_game(core: Rc<AppCore>, settings: &RenderSettings) {
    let Some(selection) = resolve_boot_selection() else {
        return;
    };
    if selection.force_new {
        clear_saved_puzzle_selection();
        sync_runtime::clear_local_snapshot();
    }
    if selection.clear_hash {
        app_router::clear_location_hash();
    }
    apply_selection(core, settings.image_max_dim, selection);
}

struct BootCoordinator {
    core: Rc<AppCore>,
}

impl BootCoordinator {
    async fn run(self) {
        boot_runtime::set_boot_state(BootState::LoadingRoute);
        boot_runtime::set_boot_state(BootState::LoadingStorage);
        let _ = persisted_store::bootstrap().await;
        let init = app_router::load_init_config();
        let _ = init.mode_preference;
        app_runtime::set_init_config(init.clone());
        let render_settings = app_router::load_render_settings_with_init();
        let renderer = init.renderer;

        self.core.set_renderer_kind(renderer);
        let render_settings_for_fallback = render_settings.clone();
        let core_for_fallback = self.core.clone();
        sync_runtime::set_on_fail(Rc::new(move || {
            app_router::clear_room_session();
            app_router::save_mode_preference(InitMode::Local);
            app_router::clear_location_hash();
            sync_runtime::init_from_config(None);
            boot_local_game(core_for_fallback.clone(), &render_settings_for_fallback);
        }));
        sync_runtime::init_from_config(init.multiplayer.clone());

        boot_runtime::set_boot_state(BootState::InitSync);
        if init.mode == InitMode::Local {
            boot_local_game(self.core.clone(), &render_settings);
        }

        match renderer {
            RendererKind::Wgpu => {
                #[cfg(target_arch = "wasm32")]
                {
                    wgpu_app::run(self.core.clone());
                }
            }
            RendererKind::Svg => {
                #[cfg(target_arch = "wasm32")]
                {
                    svg_app::run(self.core.clone());
                }
            }
        }

        #[cfg(feature = "dev-panel-yew")]
        {
            yew_app::run_dev_panel(self.core.clone());
        }

        sync_runtime::wait_for_ready().await;
        boot_runtime::set_boot_state(BootState::Ready);
    }
}

pub(crate) fn run() {
    let core = AppCore::new();
    sync_runtime::attach_core(core.clone());
    #[cfg(target_arch = "wasm32")]
    {
        multiplayer_bridge::install(core.clone());
    }
    spawn_local(async move {
        BootCoordinator { core }.run().await;
    });
}
