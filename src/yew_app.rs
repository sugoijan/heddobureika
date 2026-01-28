use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{request_animation_frame, AnimationFrame};
use gloo::timers::callback::Interval;
use glyphon::{Attrs, Buffer, Family, FontSystem, Metrics, Shaping};
use glyphon::cosmic_text::Align;
use js_sys::{Date, Function, Reflect};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    CanvasRenderingContext2d, Element, Event, HtmlCanvasElement, HtmlImageElement,
    HtmlInputElement, HtmlSelectElement, InputEvent, KeyboardEvent, Touch, TouchEvent,
};
use yew::prelude::*;
use taffy::prelude::*;

use crate::app_core::AppCore;
use crate::app_builder;
use crate::app_router;
use crate::app_runtime;
use crate::sync_runtime;
use crate::core::*;
use crate::model::*;
use crate::multiplayer_bridge::{self, MultiplayerUiHooks};
use crate::runtime::{CoreAction, SyncAction, SyncHooks};
use heddobureika_core::{
    GameSnapshot, PuzzleInfo, RoomUpdate,
};
#[cfg(test)]
use heddobureika_core::ServerMsg;
use heddobureika_core::catalog::{PuzzleCatalogEntry, PUZZLE_CATALOG};
use crate::renderer::{
    build_mask_atlas, Instance, InstanceBatch, InstanceSet, MaskAtlasData, UiRotationOrigin,
    UiTextId, UiTextSpec, WgpuRenderer,
};
#[cfg(feature = "backend-yew")]
use crate::svg_view;

#[derive(Clone, Copy, PartialEq)]
enum AppMode {
    Svg,
    DevPanel,
}

#[derive(Properties, PartialEq)]
struct AppProps {
    mode: AppMode,
}

impl Default for AppProps {
    fn default() -> Self {
        Self { mode: AppMode::Svg }
    }
}

const CREDIT_TEXT: &str = "coded by すごいジャン";
const UI_TITLE_TEXT: &str = "ヘッドブレイカー";
const CREDIT_URL: &str = "https://github.com/sugoijan/heddobureika";
const FPS_FONT_BYTES: &[u8] = include_bytes!("../fonts/chirufont.ttf");
const UI_FONT_FAMILY: &str = "KaoriGel";
const UI_CREDIT_FONT_RATIO: f32 = 0.026;
const UI_CREDIT_ROTATION_DEG: f32 = -1.1;
const UI_MENU_SUB_FONT_RATIO: f32 = 0.028;
const UI_MENU_SUB_ROTATION_DEG: f32 = 1.1;
const UI_MENU_TITLE_FONT_RATIO: f32 = 0.05;
const UI_MENU_TITLE_ROTATION_DEG: f32 = -0.8;
const UI_PROGRESS_FONT_RATIO: f32 = 0.032;
const UI_PROGRESS_ROTATION_DEG: f32 = -89.6;
const UI_SUCCESS_FONT_RATIO: f32 = 0.082;
const UI_SUCCESS_OFFSET_RATIO: f32 = 0.03;
const UI_SUCCESS_ROTATION_DEG: f32 = -1.3;
const UI_TEXT_ALPHA_SCALE: f32 = 0.8;
const UI_TEXT_LAYOUT_SCALE: f32 = 1.08;
const UI_TEXT_LAYOUT_PAD: f32 = 6.0;
const UI_TEXT_HITBOX_PAD: f32 = 8.0;
const UI_TITLE_FONT_RATIO: f32 = 0.058;
const UI_TITLE_ROTATION_DEG: f32 = 0.5;
const DRAG_SCALE: f32 = 1.01;
const DRAG_ROTATION_DEG: f32 = 1.0;
const OUTLINE_KIND_HOVER: f32 = 1.0;
const OUTLINE_KIND_OWNED: f32 = 2.0;
const OUTLINE_KIND_DEBUG: f32 = 3.0;

fn drag_angle_for_group(count: usize) -> f32 {
    let denom = (count.max(1) as f32).sqrt();
    DRAG_ROTATION_DEG / denom
}

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

    #[allow(dead_code)]
    fn view(&self) -> Option<PuzzleInfo> {
        (*self.state).clone()
    }
}

#[derive(Clone, Debug)]
struct LocalSnapshot {
    positions: Vec<(f32, f32)>,
    rotations: Vec<f32>,
    flips: Vec<bool>,
    connections: Vec<[bool; 4]>,
    group_order: Vec<u32>,
    z_order: Vec<usize>,
    scramble_nonce: u32,
}

impl LocalSnapshot {
    fn empty() -> Self {
        Self {
            positions: Vec::new(),
            rotations: Vec::new(),
            flips: Vec::new(),
            connections: Vec::new(),
            group_order: Vec::new(),
            z_order: Vec::new(),
            scramble_nonce: 0,
        }
    }
}

fn puzzle_art_index_by_src(src: &str) -> Option<usize> {
    PUZZLE_ARTS.iter().position(|art| art.src == src)
}

fn puzzle_art_index_by_label(label: &str) -> Option<usize> {
    let trimmed = label.trim();
    PUZZLE_ARTS
        .iter()
        .position(|art| art.label.eq_ignore_ascii_case(trimmed))
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

#[derive(Default)]
struct DragHandlers {
    on_move: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_release: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_touch_move: Option<Rc<dyn Fn(&TouchEvent)>>,
    on_touch_release: Option<Rc<dyn Fn(&TouchEvent)>>,
}

#[derive(Clone, PartialEq)]
struct HoverDeps {
    hovered_id: Option<usize>,
    active_id: Option<usize>,
    dragging_members: Vec<usize>,
    show_debug: bool,
}

#[derive(Clone, PartialEq)]
struct WgpuRenderDeps {
    using_wgpu: bool,
    puzzle_dims: Option<(u32, u32)>,
    grid: GridChoice,
    workspace_scale: f32,
    image_max_dim: u32,
    theme_mode: ThemeMode,
    solved: bool,
    menu_visible: bool,
    ui_revision: u32,
    z_order: Vec<usize>,
    hover_deps: HoverDeps,
    mask_atlas_revision: u32,
    wgpu_settings: WgpuRenderSettings,
    ownership_by_anchor: HashMap<u32, u64>,
    mp_client_id: Option<u64>,
}

#[derive(Clone, Copy)]
struct UiHitbox {
    center: [f32; 2],
    half_size: [f32; 2],
    rotation_deg: f32,
}

struct GlyphonMeasureState {
    font_system: FontSystem,
}

impl GlyphonMeasureState {
    fn new() -> Self {
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(FPS_FONT_BYTES.to_vec());
        Self { font_system }
    }
}

fn estimate_text_width(text: &str, font_size: f32) -> f32 {
    let mut units = 0.0;
    for ch in text.chars() {
        if ch.is_ascii_whitespace() {
            units += 0.35;
        } else if ch.is_ascii() {
            units += 0.6;
        } else {
            units += 1.0;
        }
    }
    units * font_size
}

fn estimate_text_block(spec: &UiTextSpec) -> (f32, f32) {
    let mut max_width = 0.0;
    let lines: Vec<&str> = spec.text.lines().collect();
    for line in &lines {
        let width = estimate_text_width(line, spec.font_size);
        if width > max_width {
            max_width = width;
        }
    }
    let line_count = lines.len().max(1) as f32;
    let height = spec.line_height * line_count;
    (max_width, height)
}

fn measure_text_bounds(buffer: &Buffer) -> (f32, f32) {
    let mut max_width = 0.0;
    let mut max_height = 0.0;
    for run in buffer.layout_runs() {
        if run.line_w > max_width {
            max_width = run.line_w;
        }
        let bottom = run.line_top + run.line_height;
        if bottom > max_height {
            max_height = bottom;
        }
    }
    (max_width, max_height)
}

fn measure_text_block(
    measure: &mut GlyphonMeasureState,
    spec: &UiTextSpec,
) -> (f32, f32) {
    let metrics = Metrics::new(spec.font_size, spec.line_height);
    let mut buffer = Buffer::new(&mut measure.font_system, metrics);
    buffer.set_size(&mut measure.font_system, None, None);
    let attrs = Attrs::new().family(Family::Name(UI_FONT_FAMILY));
    buffer.set_text(
        &mut measure.font_system,
        &spec.text,
        &attrs,
        Shaping::Advanced,
        Some(Align::Left),
    );
    buffer.shape_until_scroll(&mut measure.font_system, false);
    let (width, height) = measure_text_bounds(&buffer);
    if width <= 0.0 || height <= 0.0 {
        estimate_text_block(spec)
    } else {
        (width, height)
    }
}

fn layout_text_bounds(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> (f32, f32) {
    let (text_width, text_height) = measure_text_block(measure, spec);
    let scaled_width = text_width * UI_TEXT_LAYOUT_SCALE;
    let scaled_height = text_height * UI_TEXT_LAYOUT_SCALE;
    (
        (scaled_width + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
        (scaled_height + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
    )
}

fn rotation_offset_for(origin: UiRotationOrigin, width: f32, height: f32) -> [f32; 2] {
    let half_w = width * 0.5;
    let half_h = height * 0.5;
    match origin {
        UiRotationOrigin::Center => [0.0, 0.0],
        UiRotationOrigin::BottomLeft => [-half_w, half_h],
        UiRotationOrigin::BottomRight => [half_w, half_h],
        UiRotationOrigin::TopLeft => [-half_w, -half_h],
        UiRotationOrigin::TopRight => [half_w, -half_h],
    }
}

fn apply_taffy_layout(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    menu_visible: bool,
    specs: &mut [UiTextSpec],
) {
    let puzzle_width = width * layout.puzzle_scale;
    let puzzle_height = height * layout.puzzle_scale;
    let left_gutter = (-layout.view_min_x).max(0.0);
    let right_gutter = (layout.view_min_x + layout.view_width - puzzle_width).max(0.0);
    let top_gutter = (-layout.view_min_y).max(0.0);
    let bottom_gutter = (layout.view_min_y + layout.view_height - puzzle_height).max(0.0);
    let mut taffy: TaffyTree<()> = TaffyTree::new();
    let root_style = Style {
        display: Display::Grid,
        size: Size {
            width: length(layout.view_width),
            height: length(layout.view_height),
        },
        grid_template_columns: vec![
            length(left_gutter),
            length(puzzle_width),
            length(right_gutter),
        ],
        grid_template_rows: vec![length(top_gutter), length(puzzle_height), length(bottom_gutter)],
        justify_items: Some(JustifyItems::Center),
        align_items: Some(AlignItems::Center),
        ..Default::default()
    };
    let mut nodes: Vec<(UiTextId, NodeId)> = Vec::new();
    let root = if menu_visible {
        let mut menu_children = Vec::new();
        let mut menu_ids = Vec::new();
        let mut title_size = None;
        for spec in specs.iter_mut() {
            if !matches!(spec.id, UiTextId::MenuTitle | UiTextId::MenuSubtitle) {
                continue;
            }
            if matches!(spec.id, UiTextId::MenuTitle) {
                title_size = Some(spec.font_size);
            }
            let (text_width, text_height) = layout_text_bounds(measure, spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let child_style = Style {
                size: Size {
                    width: length(text_width),
                    height: length(text_height),
                },
                ..Default::default()
            };
            let Ok(node) = taffy.new_leaf(child_style) else {
                return;
            };
            menu_ids.push(node);
            menu_children.push((spec.id, node));
        }
        let gap = title_size.unwrap_or(0.0) * 0.4;
        let menu_style = Style {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            align_items: Some(AlignItems::Center),
            justify_content: Some(JustifyContent::Center),
            gap: Size {
                width: zero(),
                height: length(gap),
            },
            grid_row: line(2),
            grid_column: line(2),
            ..Default::default()
        };
        let Ok(menu_node) = taffy.new_with_children(menu_style, &menu_ids) else {
            return;
        };
        nodes.extend(menu_children);
        let Ok(root) = taffy.new_with_children(root_style, &[menu_node]) else {
            return;
        };
        root
    } else {
        let mut children = Vec::new();
        for spec in specs.iter_mut() {
            let (row, col, span_cols, justify, align) = match spec.id {
                UiTextId::Title => (1, 1, false, JustifySelf::Start, AlignSelf::Start),
                UiTextId::Progress => (3, 1, false, JustifySelf::Start, AlignSelf::End),
                UiTextId::Credit => (3, 3, false, JustifySelf::End, AlignSelf::End),
                UiTextId::Success => (1, 2, false, JustifySelf::Center, AlignSelf::Center),
                UiTextId::MenuTitle | UiTextId::MenuSubtitle => {
                    (2, 2, false, JustifySelf::Center, AlignSelf::Center)
                }
            };
            let (text_width, text_height) = layout_text_bounds(measure, spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let mut child_style = Style {
                size: Size {
                    width: length(text_width),
                    height: length(text_height),
                },
                justify_self: Some(justify),
                align_self: Some(align),
                ..Default::default()
            };
            child_style.grid_row = line(row);
            child_style.grid_column = if span_cols { span(3) } else { line(col) };
            let Ok(node) = taffy.new_leaf(child_style) else {
                return;
            };
            children.push(node);
            nodes.push((spec.id, node));
        }
        let Ok(root) = taffy.new_with_children(root_style, &children) else {
            return;
        };
        root
    };
    if taffy
        .compute_layout(
            root,
            Size {
                width: length(layout.view_width),
                height: length(layout.view_height),
            },
        )
        .is_err()
    {
        return;
    }
    let success_offset = width.min(height) * UI_SUCCESS_OFFSET_RATIO;
    for spec in specs.iter_mut() {
        let node = nodes
            .iter()
            .find(|(id, _)| *id == spec.id)
            .map(|(_, node)| *node);
        let Some(node) = node else {
            continue;
        };
        let Ok(node_layout) = taffy.layout(node) else {
            continue;
        };
        spec.pos = [
            layout.view_min_x + node_layout.location.x + node_layout.size.width * 0.5,
            layout.view_min_y + node_layout.location.y + node_layout.size.height * 0.5,
        ];
        if matches!(spec.id, UiTextId::Success) {
            spec.pos[0] += success_offset;
        }
    }
}

fn ui_hitbox_for_spec(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> UiHitbox {
    let (text_width, text_height) = measure_text_block(measure, spec);
    let pad = UI_TEXT_HITBOX_PAD;
    let offset = spec.rotation_offset;
    let angle = spec.rotation_deg.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();
    let pivot = [spec.pos[0] + offset[0], spec.pos[1] + offset[1]];
    let rel_center = [-offset[0], -offset[1]];
    let center = [
        pivot[0] + rel_center[0] * cos - rel_center[1] * sin,
        pivot[1] + rel_center[0] * sin + rel_center[1] * cos,
    ];
    UiHitbox {
        center,
        half_size: [
            (text_width + pad * 2.0) * 0.5,
            (text_height + pad * 2.0) * 0.5,
        ],
        rotation_deg: spec.rotation_deg,
    }
}

fn point_in_ui_hitbox(x: f32, y: f32, hitbox: UiHitbox) -> bool {
    let dx = x - hitbox.center[0];
    let dy = y - hitbox.center[1];
    let angle = -hitbox.rotation_deg.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();
    let local_x = dx * cos - dy * sin;
    let local_y = dx * sin + dy * cos;
    local_x.abs() <= hitbox.half_size[0] && local_y.abs() <= hitbox.half_size[1]
}

fn open_credit_url() {
    if let Some(window) = web_sys::window() {
        let _ = window.open_with_url_and_target(CREDIT_URL, "_blank");
    }
}

fn ui_scale_alpha(color: [u8; 4], scale: f32) -> [u8; 4] {
    let scaled = (color[3] as f32 * scale).round().clamp(0.0, 255.0) as u8;
    [color[0], color[1], color[2], scaled]
}

fn ui_scaled_alpha(alpha: u8) -> u8 {
    (alpha as f32 * UI_TEXT_ALPHA_SCALE)
        .round()
        .clamp(0.0, 255.0) as u8
}

fn ui_text_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [235, 234, 230, 230]
    } else {
        [20, 20, 20, 220]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}

fn ui_accent_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [168, 255, 208, 240]
    } else {
        [24, 120, 62, 230]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}

fn ui_with_alpha(color: [u8; 4], alpha: u8) -> [u8; 4] {
    [color[0], color[1], color[2], alpha]
}

fn ui_color_to_css(color: [u8; 4]) -> String {
    let alpha = (color[3] as f32) / 255.0;
    format!(
        "rgba({}, {}, {}, {:.3})",
        color[0], color[1], color[2], alpha
    )
}

fn build_ui_specs(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    title_text: &str,
    solved: bool,
    connections_label: &str,
    border_connections_label: &str,
    menu_visible: bool,
    is_dark: bool,
) -> Vec<UiTextSpec> {
    let min_dim = width.min(height).max(1.0);
    let base_color = ui_text_color(is_dark);
    let muted_color = ui_with_alpha(base_color, ui_scaled_alpha(170));
    let accent_color = ui_accent_color(is_dark);
    let mut specs = Vec::new();
    if menu_visible {
        let title_size = min_dim * UI_MENU_TITLE_FONT_RATIO;
        let sub_size = min_dim * UI_MENU_SUB_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::MenuTitle,
            text: "Pick a puzzle".to_string(),
            pos: [0.0, 0.0],
            rotation_deg: UI_MENU_TITLE_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::Center,
            rotation_offset: [0.0, 0.0],
            font_size: title_size,
            line_height: title_size * 1.08,
            color: base_color,
        });
        specs.push(UiTextSpec {
            id: UiTextId::MenuSubtitle,
            text: "Scramble / Puzzle / Pieces".to_string(),
            pos: [0.0, 0.0],
            rotation_deg: UI_MENU_SUB_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::Center,
            rotation_offset: [0.0, 0.0],
            font_size: sub_size,
            line_height: sub_size * 1.08,
            color: muted_color,
        });
    } else {
        if !title_text.is_empty() {
            let title_size = min_dim * UI_TITLE_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Title,
                text: title_text.to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_TITLE_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::BottomLeft,
                rotation_offset: [0.0, 0.0],
                font_size: title_size,
                line_height: title_size * 1.08,
                color: base_color,
            });
        }
        let progress_text = format!(
            "{} of borders and {} in total complete",
            border_connections_label, connections_label
        );
        let progress_size = min_dim * UI_PROGRESS_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::Progress,
            text: progress_text,
            pos: [0.0, 0.0],
            rotation_deg: UI_PROGRESS_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::TopLeft,
            rotation_offset: [0.0, 0.0],
            font_size: progress_size,
            line_height: progress_size * 1.08,
            color: base_color,
        });

        let credit_text = CREDIT_TEXT.to_string();
        let credit_size = min_dim * UI_CREDIT_FONT_RATIO;
        specs.push(UiTextSpec {
            id: UiTextId::Credit,
            text: credit_text,
            pos: [0.0, 0.0],
            rotation_deg: UI_CREDIT_ROTATION_DEG,
            rotation_origin: UiRotationOrigin::BottomLeft,
            rotation_offset: [0.0, 0.0],
            font_size: credit_size,
            line_height: credit_size * 1.08,
            color: base_color,
        });

        if solved {
            let success_size = min_dim * UI_SUCCESS_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Success,
                text: "Well done!".to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_SUCCESS_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::Center,
                rotation_offset: [0.0, 0.0],
                font_size: success_size,
                line_height: success_size * 1.1,
                color: accent_color,
            });
        }
    }

    apply_taffy_layout(measure, layout, width, height, menu_visible, &mut specs);
    specs
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
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(THEME_MODE_KEY).ok()??;
    serde_json::from_str(&raw).ok()
}

fn clear_saved_game() {
    crate::local_snapshot::clear_local_snapshot();
}

fn save_theme_mode(mode: ThemeMode) {
    let Ok(raw) = serde_json::to_string(&mode) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(THEME_MODE_KEY, &raw);
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

fn now_ms() -> f32 {
    if let Some(window) = web_sys::window() {
        if let Ok(perf) = Reflect::get(&window, &"performance".into()) {
            if let Ok(now_fn) = Reflect::get(&perf, &"now".into()).and_then(|value| value.dyn_into::<Function>()) {
                if let Ok(value) = now_fn.call0(&perf) {
                    if let Some(ms) = value.as_f64() {
                        return ms as f32;
                    }
                }
            }
        }
    }
    (Date::now() % 1_000_000.0) as f32
}

fn anchor_of_from_state(state: &PuzzleState) -> Vec<usize> {
    state.pieces.iter().map(|piece| piece.group()).collect()
}

fn anchor_of_from_connections(connections: &[[bool; 4]], cols: usize, rows: usize) -> Vec<usize> {
    let total = cols * rows;
    let mut anchor_of = vec![0usize; total];
    let groups = groups_from_connections(connections, cols, rows);
    for group in groups {
        if group.is_empty() {
            continue;
        }
        let anchor = group[0];
        for id in group {
            if id < total {
                anchor_of[id] = anchor;
            }
        }
    }
    anchor_of
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

fn event_to_svg_coords(
    event: &MouseEvent,
    svg_ref: &NodeRef,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
) -> Option<(f32, f32)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let rect_left = rect.left() as f32;
    let rect_top = rect.top() as f32;
    let x = view_min_x + (event.client_x() as f32 - rect_left) * view_width / rect_width;
    let y = view_min_y + (event.client_y() as f32 - rect_top) * view_height / rect_height;
    Some((x, y))
}

fn touch_from_event(event: &TouchEvent, touch_id: Option<i32>, use_changed: bool) -> Option<Touch> {
    let list = if use_changed {
        event.changed_touches()
    } else {
        event.touches()
    };
    if let Some(id) = touch_id {
        for idx in 0..list.length() {
            if let Some(touch) = list.item(idx) {
                if touch.identifier() == id {
                    return Some(touch);
                }
            }
        }
        None
    } else {
        list.item(0)
    }
}

fn touch_event_to_svg_coords(
    event: &TouchEvent,
    svg_ref: &NodeRef,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    touch_id: Option<i32>,
    use_changed: bool,
) -> Option<(f32, f32)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let touch = touch_from_event(event, touch_id, use_changed)?;
    let rect_left = rect.left() as f32;
    let rect_top = rect.top() as f32;
    let x = view_min_x + (touch.client_x() as f32 - rect_left) * view_width / rect_width;
    let y = view_min_y + (touch.client_y() as f32 - rect_top) * view_height / rect_height;
    Some((x, y))
}

fn workspace_to_puzzle_coords(scale: f32, x: f32, y: f32) -> (f32, f32) {
    let scale = scale.max(1.0e-4);
    (x / scale, y / scale)
}

fn workspace_to_puzzle_opt(scale: f32, coords: Option<(f32, f32)>) -> Option<(f32, f32)> {
    coords.map(|(x, y)| workspace_to_puzzle_coords(scale, x, y))
}

fn drag_group_center(
    positions: &[(f32, f32)],
    members: &[usize],
    piece_width: f32,
    piece_height: f32,
) -> Option<(f32, f32)> {
    if members.is_empty() {
        return None;
    }
    let mut sum_x = 0.0;
    let mut sum_y = 0.0;
    let mut count = 0.0;
    for id in members {
        if let Some(pos) = positions.get(*id) {
            sum_x += pos.0 + piece_width * 0.5;
            sum_y += pos.1 + piece_height * 0.5;
            count += 1.0;
        }
    }
    if count > 0.0 {
        Some((sum_x / count, sum_y / count))
    } else {
        None
    }
}

fn drag_group_position(
    pos: (f32, f32),
    center: (f32, f32),
    scale: f32,
    rotation_deg: f32,
    piece_width: f32,
    piece_height: f32,
) -> (f32, f32) {
    let piece_center = (pos.0 + piece_width * 0.5, pos.1 + piece_height * 0.5);
    let mut dx = piece_center.0 - center.0;
    let mut dy = piece_center.1 - center.1;
    dx *= scale;
    dy *= scale;
    let (rx, ry) = rotate_vec(dx, dy, rotation_deg);
    let new_center = (center.0 + rx, center.1 + ry);
    (
        new_center.0 - piece_width * 0.5,
        new_center.1 - piece_height * 0.5,
    )
}

fn drag_visual_from_state(
    drag_state: &Option<DragState>,
) -> Option<(Vec<usize>, (f32, f32), f32)> {
    let drag = drag_state.as_ref()?;
    if drag.members.is_empty() {
        return None;
    }
    let dir = if drag.right_click { -1.0 } else { 1.0 };
    Some((drag.members.clone(), (drag.cursor_x, drag.cursor_y), dir))
}

fn build_wgpu_instances(
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    connections: &[[bool; 4]],
    hovered_id: Option<usize>,
    show_debug: bool,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    mask_atlas: &MaskAtlasData,
    highlighted_members: Option<&[usize]>,
    drag_origin: Option<(f32, f32)>,
    drag_dir: f32,
    ownership_by_anchor: &HashMap<u32, u64>,
    own_client_id: Option<u64>,
) -> InstanceSet {
    let total = cols * rows;
    if total == 0 {
        return InstanceSet {
            instances: Vec::new(),
            batches: Vec::new(),
        };
    }
    let fallback_order = if z_order.len() == total {
        None
    } else {
        Some((0..total).collect::<Vec<_>>())
    };
    let order = match fallback_order.as_deref() {
        Some(slice) => slice,
        None => z_order,
    };
    let mut hovered_mask = vec![false; total];
    if show_debug {
        hovered_mask.fill(true);
    } else if let Some(members) = highlighted_members {
        for member in members {
            if *member < hovered_mask.len() {
                hovered_mask[*member] = true;
            }
        }
    } else if let Some(id) = hovered_id {
        if id < total && id < connections.len() {
            for member in collect_group(connections, id, cols, rows) {
                if member < hovered_mask.len() {
                    hovered_mask[member] = true;
                }
            }
        }
    }
    let mut drag_mask = vec![false; total];
    if let Some(members) = highlighted_members {
        for member in members {
            if *member < drag_mask.len() {
                drag_mask[*member] = true;
            }
        }
    }
    let drag_active = drag_dir.abs() > f32::EPSILON && drag_mask.iter().any(|val| *val);
    let drag_count = highlighted_members.map(|members| members.len()).unwrap_or(0);
    let drag_rotation = if drag_active {
        drag_angle_for_group(drag_count) * drag_dir.signum()
    } else {
        0.0
    };
    let drag_scale = if drag_active { DRAG_SCALE } else { 1.0 };
    let drag_center = if drag_active {
        drag_origin.or_else(|| {
            highlighted_members
                .and_then(|members| drag_group_center(positions, members, piece_width, piece_height))
        })
    } else {
        None
    };
    let mut owned_mask = vec![false; total];
    let mut group_id = vec![usize::MAX; total];
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut group_has_owned: Vec<bool> = Vec::new();
    let mut queue = VecDeque::new();
    for start in 0..total {
        if group_id[start] != usize::MAX {
            continue;
        }
        let owned = ownership_by_anchor
            .get(&(start as u32))
            .map(|owner_id| Some(*owner_id) != own_client_id)
            .unwrap_or(false);
        let gid = groups.len();
        let mut members = Vec::new();
        group_id[start] = gid;
        queue.push_back(start);
        while let Some(id) = queue.pop_front() {
            members.push(id);
            if owned {
                owned_mask[id] = true;
            }
            for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
                if connections
                    .get(id)
                    .map(|edges| edges[dir])
                    .unwrap_or(false)
                {
                    if let Some(neighbor) = neighbor_id(id, cols, rows, dir) {
                        if group_id[neighbor] == usize::MAX {
                            group_id[neighbor] = gid;
                            queue.push_back(neighbor);
                        }
                    }
                }
            }
        }
        groups.push(members);
        group_has_owned.push(owned);
    }
    let mut group_members: Vec<Vec<usize>> = vec![Vec::new(); groups.len()];
    let mut group_order = Vec::new();
    let mut group_seen = vec![false; groups.len()];
    for &id in order {
        let gid = group_id[id];
        group_members[gid].push(id);
        if !group_seen[gid] {
            group_seen[gid] = true;
            group_order.push(gid);
        }
    }
    let mut group_has_hover = vec![false; groups.len()];
    for (gid, members) in group_members.iter().enumerate() {
        if members.iter().any(|id| hovered_mask[*id]) {
            group_has_hover[gid] = true;
        }
    }
    let mut instances = Vec::with_capacity(order.len());
    let mut batches: Vec<InstanceBatch> = Vec::new();
    for gid in group_order {
        let members = &group_members[gid];
        if members.is_empty() {
            continue;
        }
        let start = instances.len() as u32;
        for &id in members {
            let col = id % cols;
            let row = id / cols;
            let base_x = col as f32 * piece_width;
            let base_y = row as f32 * piece_height;
            let pos = positions.get(id).copied().unwrap_or((base_x, base_y));
            let render_pos = if drag_mask.get(id).copied().unwrap_or(false) {
                if let Some(center) = drag_center {
                    drag_group_position(pos, center, drag_scale, drag_rotation, piece_width, piece_height)
                } else {
                    pos
                }
            } else {
                pos
            };
            let rotation = rotations.get(id).copied().unwrap_or(0.0);
            let flipped = flips.get(id).copied().unwrap_or(false);
            let hovered = hovered_mask.get(id).copied().unwrap_or(false);
            let owned = owned_mask.get(id).copied().unwrap_or(false);
            let mask_origin = mask_atlas.origins.get(id).copied().unwrap_or([0.0, 0.0]);
            instances.push(Instance {
                pos: [render_pos.0, render_pos.1],
                size: [piece_width, piece_height],
                rotation,
                flip: if flipped { 1.0 } else { 0.0 },
                hover: if show_debug {
                    OUTLINE_KIND_DEBUG
                } else if owned {
                    OUTLINE_KIND_OWNED
                } else if hovered {
                    OUTLINE_KIND_HOVER
                } else {
                    0.0
                },
                drag: if drag_mask.get(id).copied().unwrap_or(false) {
                    drag_rotation
                } else {
                    0.0
                },
                piece_origin: [base_x, base_y],
                mask_origin,
            });
        }
        let count = (instances.len() as u32) - start;
        if count == 0 {
            continue;
        }
        let draw_outline = show_debug || group_has_hover[gid] || group_has_owned[gid];
        if !show_debug && !draw_outline {
            if let Some(last) = batches.last_mut() {
                if !last.draw_outline {
                    last.count += count;
                    continue;
                }
            }
        }
        batches.push(InstanceBatch {
            start,
            count,
            draw_outline,
        });
    }
    InstanceSet { instances, batches }
}

fn pick_piece_at(
    x: f32,
    y: f32,
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    mask_atlas: &MaskAtlasData,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    mask_pad: f32,
) -> Option<usize> {
    if cols == 0 || piece_width <= 0.0 || piece_height <= 0.0 {
        return None;
    }
    let center_x = piece_width * 0.5;
    let center_y = piece_height * 0.5;
    let min_x = -mask_pad;
    let min_y = -mask_pad;
    let max_x = piece_width + mask_pad;
    let max_y = piece_height + mask_pad;
    let atlas_width = mask_atlas.width as i32;
    let atlas_height = mask_atlas.height as i32;
    for &piece_id in z_order.iter().rev() {
        let col = piece_id % cols;
        let row = piece_id / cols;
        let base_x = col as f32 * piece_width;
        let base_y = row as f32 * piece_height;
        let pos = positions.get(piece_id).copied().unwrap_or((base_x, base_y));
        let rotation = rotations.get(piece_id).copied().unwrap_or(0.0);
        let flipped = flips.get(piece_id).copied().unwrap_or(false);
        let mut local_x = x - pos.0;
        let mut local_y = y - pos.1;
        if rotation.abs() > f32::EPSILON {
            let (rx, ry) = rotate_point(local_x, local_y, center_x, center_y, -rotation);
            local_x = rx;
            local_y = ry;
        }
        if flipped {
            local_x = piece_width - local_x;
        }
        if local_x < min_x
            || local_y < min_y
            || local_x > max_x
            || local_y > max_y
        {
            continue;
        }
        let mask_origin = match mask_atlas.origins.get(piece_id) {
            Some(origin) => *origin,
            None => continue,
        };
        let mask_x = (mask_origin[0] + local_x).floor() as i32;
        let mask_y = (mask_origin[1] + local_y).floor() as i32;
        if mask_x < 0 || mask_y < 0 || mask_x >= atlas_width || mask_y >= atlas_height {
            continue;
        }
        let idx = (mask_y as u32 * mask_atlas.width + mask_x as u32) as usize * 4;
        let alpha = mask_atlas.pixels.get(idx).copied().unwrap_or(0);
        if alpha > 16 {
            return Some(piece_id);
        }
    }
    None
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

fn prefers_dark_mode() -> bool {
    let Some(window) = web_sys::window() else {
        return false;
    };
    let Ok(match_media) = Reflect::get(&window, &"matchMedia".into()) else {
        return false;
    };
    let Ok(match_media) = match_media.dyn_into::<Function>() else {
        return false;
    };
    let Ok(query) = match_media.call1(&window, &"(prefers-color-scheme: dark)".into()) else {
        return false;
    };
    Reflect::get(&query, &"matches".into())
        .ok()
        .and_then(|value| value.as_bool())
        .unwrap_or(false)
}

#[function_component(App)]
fn app(props: &AppProps) -> Html {
    #[cfg(test)]
    gloo::console::log!("app render");
    let app_core = AppCore::shared();
    let app_snapshot = use_state(|| app_core.snapshot());
    let show_dev_panel = matches!(props.mode, AppMode::DevPanel);
    let puzzle_info = use_state(|| None::<PuzzleInfo>);
    let puzzle_info_live = use_mut_ref(|| None::<PuzzleInfo>);
    let puzzle_info_store = PuzzleInfoStore::new(puzzle_info.clone(), puzzle_info_live.clone());
    let puzzle_info_value = (*puzzle_info).clone();
    let puzzle_dims_value =
        puzzle_info_value.as_ref().map(|info| (info.image_width, info.image_height));
    let image_revision = use_state(|| 0u32);
    let image_revision_value = *image_revision;
    let image_element = use_state(|| None::<HtmlImageElement>);
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let depth_cap = settings_value
        .tab_depth_cap
        .clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    let curve_detail = settings_value
        .curve_detail
        .clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
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
        .and_then(|info| {
            puzzle_art_index_by_src(&info.image_src)
                .or_else(|| puzzle_art_index_by_label(&info.label))
        })
        .unwrap_or(0);
    let puzzle_art_index = use_state(|| initial_puzzle_art_index);
    let puzzle_art_index_value = *puzzle_art_index;
    let puzzle_art = PUZZLE_ARTS
        .get(puzzle_art_index_value)
        .copied()
        .unwrap_or(PUZZLE_ARTS[0]);
    let puzzle_src = puzzle_art.src;
    let render_puzzle_src = puzzle_info_value
        .as_ref()
        .map(|info| info.image_src.clone())
        .unwrap_or_else(|| puzzle_src.to_string());
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
    let init_config = app_runtime::init_config();
    let multiplayer_config = use_state(|| init_config.multiplayer.clone());
    let multiplayer_config_value = (*multiplayer_config).clone();
    let sync_revision = use_state(|| 0u32);
    let sync_view = sync_runtime::sync_view();
    let multiplayer_active = matches!(sync_view.mode(), InitMode::Online);
    let multiplayer_connected = sync_view.connected();
    let multiplayer_disconnected = multiplayer_active && !multiplayer_connected;
    let allow_drag = !multiplayer_active || multiplayer_connected;
    let mp_init_required_value = sync_view.init_required();
    let lock_puzzle_controls = multiplayer_active && !mp_init_required_value;
    let _ = *sync_revision;
    let dispatch_view_action: Rc<dyn Fn(CoreAction)> = {
        let multiplayer_active = multiplayer_active;
        let app_core = app_core.clone();
        Rc::new(move |action| {
            sync_runtime::dispatch_view_action(&app_core, action, !multiplayer_active);
        })
    };

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
    let ui_revision_value = *ui_revision;
    let bump_ui_revision: Rc<dyn Fn()> = {
        let ui_revision = ui_revision.clone();
        Rc::new(move || {
            ui_revision.set(ui_revision.wrapping_add(1));
        })
    };
    let local_snapshot = use_mut_ref(LocalSnapshot::empty);
    let group_anchor = use_state(Vec::<usize>::new);
    let group_pos = use_state(Vec::<(f32, f32)>::new);
    let group_rot = use_state(Vec::<f32>::new);
    let group_order = use_state(Vec::<usize>::new);
    let positions_live = use_mut_ref(Vec::<(f32, f32)>::new);
    let rotations_live = use_mut_ref(Vec::<f32>::new);
    let group_pos_live = use_mut_ref(Vec::<(f32, f32)>::new);
    let group_rot_live = use_mut_ref(Vec::<f32>::new);
    let active_id = use_state(|| None::<usize>);
    let dragging_members = use_state(Vec::<usize>::new);
    let animating_members = use_state(Vec::<usize>::new);
    let drag_state = use_mut_ref(|| None::<DragState>);
    let drag_frame = use_mut_ref(|| None::<AnimationFrame>);
    let drag_pending = use_mut_ref(|| None::<(f32, f32)>);
    let rotation_anim = use_mut_ref(|| None::<RotationAnimation>);
    let rotation_anim_handle = use_mut_ref(|| None::<Interval>);
    let rotation_queue = use_mut_ref(|| VecDeque::<QueuedRotation>::new());
    let preview_corner = use_state(|| PreviewCorner::BottomLeft);
    let preview_revealed = use_state(|| false);
    let theme_mode = use_state(|| load_theme_mode().unwrap_or(ThemeMode::System));
    let theme_mode_value = *theme_mode;
    let theme_toggle_ref = use_node_ref();
    let drag_handlers = use_mut_ref(DragHandlers::default);
    let svg_ref = use_node_ref();
    let canvas_ref = use_node_ref();
    let workspace_scale = use_state(|| WORKSPACE_SCALE_DEFAULT);
    let workspace_scale_value = *workspace_scale;
    let z_order = use_state(Vec::<usize>::new);
    let _ = ui_revision_value;
    let rotation_enabled = use_state(|| true);
    let rotation_enabled_value = *rotation_enabled;
    let render_settings = use_state(initial_render_settings);
    let render_settings_value = (*render_settings).clone();
    let image_max_dim = render_settings_value
        .image_max_dim
        .clamp(IMAGE_MAX_DIMENSION_MIN, IMAGE_MAX_DIMENSION_MAX);
    let renderer_kind = render_settings_value.renderer;
    let svg_settings_value = render_settings_value.svg.clone();
    let wgpu_settings_value = render_settings_value.wgpu.clone();
    let renderer_is_wgpu = renderer_kind == RendererKind::Wgpu;
    let svg_enabled = renderer_kind == RendererKind::Svg;
    let yew_wgpu_enabled = false; // WGPU view now lives in `wgpu_app`, not the SVG Yew app.
    let show_svg = matches!(props.mode, AppMode::Svg) && svg_enabled;
    {
        let app_core = app_core.clone();
        let app_snapshot = app_snapshot.clone();
        let show_svg = show_svg;
        use_effect_with(show_svg, move |show_svg| {
            if *show_svg {
                #[cfg(feature = "backend-yew")]
                {
                    let app_snapshot_for_hook = app_snapshot.clone();
                    svg_view::set_render_hook(Rc::new(move |snapshot| {
                        app_snapshot_for_hook.set(snapshot);
                    }));
                    app_snapshot.set(app_core.snapshot());
                    return Box::new(|| svg_view::clear_render_hook()) as Box<dyn FnOnce()>;
                }
                #[cfg(not(feature = "backend-yew"))]
                {
                    return Box::new(|| ()) as Box<dyn FnOnce()>;
                }
            }
            let app_core_for_cb = app_core.clone();
            let subscription = app_core.subscribe(Rc::new(move || {
                app_snapshot.set(app_core_for_cb.snapshot());
            }));
            Box::new(move || drop(subscription)) as Box<dyn FnOnce()>
        });
    }
    let svg_animations_enabled = svg_settings_value.animations;
    let svg_emboss_enabled = svg_settings_value.emboss;
    let svg_fast_render = svg_settings_value.fast_render;
    let svg_fast_filter = svg_settings_value.fast_filter;
    let wgpu_show_fps = wgpu_settings_value.show_fps;
    let wgpu_edge_aa = wgpu_settings_value.edge_aa;
    let wgpu_render_scale = wgpu_settings_value.render_scale;
    let image_render_scale = wgpu_render_scale;
    let animations_enabled_value = svg_enabled && svg_animations_enabled;
    let emboss_enabled_value = svg_emboss_enabled;
    let fast_render_value = svg_fast_render;
    let fast_filter_value = svg_fast_filter;
    let wgpu_renderer = use_mut_ref(|| None::<WgpuRenderer>);
    let pending_instances = use_mut_ref(|| None::<InstanceSet>);
    let pending_ui = use_mut_ref(|| None::<Vec<UiTextSpec>>);
    let ui_credit_hitbox = use_mut_ref(|| None::<UiHitbox>);
    let ui_measure_state = use_mut_ref(GlyphonMeasureState::new);
    let mask_atlas = use_mut_ref(|| None::<Rc<MaskAtlasData>>);
    let mask_atlas_revision = use_state(|| 0u32);
    let hovered_id = use_state(|| None::<usize>);
    let set_hovered: Rc<dyn Fn(Option<usize>)> = {
        let hovered_id = hovered_id.clone();
        let dispatch_view_action = dispatch_view_action.clone();
        let multiplayer_active = multiplayer_active;
        Rc::new(move |next| {
            if multiplayer_active {
                hovered_id.set(next);
            }
            dispatch_view_action(CoreAction::SetHovered { hovered: next });
        })
    };
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
    let share_seed_value = if !multiplayer_active && include_share_seed_value {
        if puzzle_info_value.is_some() {
            Some(scramble_seed(
                PUZZLE_SEED,
                scramble_nonce_value,
                grid.cols as usize,
                grid.rows as usize,
            ))
        } else {
            None
        }
    } else {
        None
    };
    let share_link_label = if multiplayer_active { "Room link" } else { "Share link" };
    let share_link = base_url_without_hash()
        .map(|base| {
            if let Some(config) = multiplayer_config_value.as_ref() {
                let room_id = encode_hash_value(&config.room_id);
                let fragment = format!("room={room_id}");
                format!("{base}#{fragment}")
            } else {
                let slug = encode_hash_value(puzzle_art.slug);
                let mut fragment = format!("puzzle={slug};pieces={}", grid.actual_count);
                if let Some(seed) = share_seed_value {
                    fragment.push_str(&format!(";seed={:#x}", seed));
                }
                format!("{base}#{fragment}")
            }
        })
        .unwrap_or_default();
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
    let save_revision = use_state(|| 0u32);
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(|| false);
    let show_controls_value = *show_controls;
    let menu_visible = use_state(|| false);
    let menu_visible_value = *menu_visible;
    let ui_credit_hovered = use_state(|| false);
    let ui_credit_hovered_value = *ui_credit_hovered;
    let show_debug = use_state(|| false);
    let show_debug_value = *show_debug;
    let app_snapshot_value = (*app_snapshot).clone();
    let local_active_id = *active_id;
    let local_dragging_members = (*dragging_members).clone();
    let active_id_value = local_active_id.or(app_snapshot_value.active_id);
    let dragging_members_value = if !local_dragging_members.is_empty() {
        local_dragging_members
    } else {
        app_snapshot_value.dragging_members.clone()
    };
    let hovered_id_value = if !multiplayer_active {
        app_snapshot_value.hovered_id
    } else {
        *hovered_id
    };
    let animating_members_value = (*animating_members).clone();
    let mp_client_id_value = sync_view.client_id();
    let ownership_by_anchor_value = sync_view.ownership_by_anchor();
    let local_snapshot_value = (*local_snapshot.borrow()).clone();
    let use_local_snapshot = multiplayer_active || !dragging_members_value.is_empty();
    let render_positions = if use_local_snapshot {
        local_snapshot_value.positions.clone()
    } else {
        app_snapshot_value.positions.clone()
    };
    let render_rotations = if use_local_snapshot {
        local_snapshot_value.rotations.clone()
    } else {
        app_snapshot_value.rotations.clone()
    };
    let render_flips = if use_local_snapshot {
        local_snapshot_value.flips.clone()
    } else {
        app_snapshot_value.flips.clone()
    };
    let render_connections = if use_local_snapshot {
        local_snapshot_value.connections.clone()
    } else {
        app_snapshot_value.connections.clone()
    };
    let render_z_order = if use_local_snapshot {
        local_snapshot_value.z_order.clone()
    } else {
        app_snapshot_value.z_order.clone()
    };
    let positions_len = render_positions.len();
    let rotations_len = render_rotations.len();
    let flips_len = render_flips.len();
    let connections_len = render_connections.len();
    let board_ready_value = puzzle_info_value.is_some()
        && positions_len == total
        && rotations_len == total
        && flips_len == total
        && connections_len == total
        && z_order.len() == total;
    let hover_deps = HoverDeps {
        hovered_id: hovered_id_value,
        active_id: active_id_value,
        dragging_members: dragging_members_value.clone(),
        show_debug: show_debug_value,
    };
    let apply_puzzle_state: Rc<dyn Fn(PuzzleState, usize, f32, f32)> = {
        let local_snapshot = local_snapshot.clone();
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
            {
                let mut snapshot = local_snapshot.borrow_mut();
                snapshot.positions = derived.positions.clone();
                snapshot.rotations = derived.rotations.clone();
                snapshot.flips = next_flips.clone();
                snapshot.connections = next_connections.clone();
                snapshot.group_order = next_state
                    .group_order
                    .iter()
                    .filter_map(|id| u32::try_from(*id).ok())
                    .collect();
                snapshot.z_order = derived.z_order.clone();
                snapshot.scramble_nonce = next_scramble;
            }
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
    {
        let app_core = app_core.clone();
        let local_snapshot = local_snapshot.clone();
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
        use_effect_with(multiplayer_active, move |multiplayer_active| {
            let multiplayer_active = *multiplayer_active;
            sync_runtime::set_local_observer(None);
            let on_snapshot = Rc::new(move |snapshot: crate::app_core::AppSnapshot| {
                app_snapshot.set(snapshot.clone());
                puzzle_info.set(snapshot.puzzle_info.clone());
                if let Some(info) = snapshot.puzzle_info.as_ref() {
                    let desired_index = puzzle_art_index_by_src(&info.image_src)
                        .or_else(|| puzzle_art_index_by_label(&info.label));
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
                    || snapshot.positions.len() != total
                    || snapshot.rotations.len() != total
                    || snapshot.flips.len() != total
                    || snapshot.connections.len() != total
                {
                    let mut local = local_snapshot.borrow_mut();
                    local.positions = snapshot.positions.clone();
                    local.rotations = snapshot.rotations.clone();
                    local.flips = snapshot.flips.clone();
                    local.connections = snapshot.connections.clone();
                    local.z_order = snapshot.z_order.clone();
                    local.group_order.clear();
                    local.scramble_nonce = snapshot.scramble_nonce;
                    z_order.set(local.z_order.clone());
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
                    &snapshot.positions,
                    &snapshot.rotations,
                    &snapshot.flips,
                    &snapshot.connections,
                    cols,
                    rows,
                    Some(piece_order.as_slice()),
                    snapshot.scramble_nonce,
                );
                let derived = derive_ui_state_from_puzzle(
                    &next_state,
                    cols,
                    snapshot.piece_width,
                    snapshot.piece_height,
                );
                let group_order_u32: Vec<u32> = derived
                    .group_order
                    .iter()
                    .filter_map(|id| u32::try_from(*id).ok())
                    .collect();
                {
                    let mut local = local_snapshot.borrow_mut();
                    local.positions = derived.positions.clone();
                    local.rotations = derived.rotations.clone();
                    local.flips = snapshot.flips.clone();
                    local.connections = snapshot.connections.clone();
                    local.group_order = group_order_u32;
                    local.z_order = derived.z_order.clone();
                    local.scramble_nonce = snapshot.scramble_nonce;
                }
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
                if !multiplayer_active && snapshot.dragging_members.is_empty() {
                    save_revision.set(save_revision.wrapping_add(1));
                }
            });
            let app_core_for_action = app_core.clone();
            sync_runtime::init_local_hooks(SyncHooks {
                on_snapshot: on_snapshot.clone(),
                on_remote_action: Rc::new(move |action| {
                    app_core_for_action.apply_action(action);
                }),
            });
            move || {
                sync_runtime::shutdown_local_hooks();
            }
        });
    }
    let _render_wgpu_now: Rc<dyn Fn()> = {
        let puzzle_info = puzzle_info_store.clone();
        let local_snapshot = local_snapshot.clone();
        let wgpu_renderer = wgpu_renderer.clone();
        let mask_atlas = mask_atlas.clone();
        let z_order = z_order.clone();
        let hovered_id = hovered_id.clone();
        let active_id = active_id.clone();
        let dragging_members = dragging_members.clone();
        let show_debug = show_debug.clone();
        let drag_state = drag_state.clone();
        let solved = solved.clone();
        let menu_visible = menu_visible.clone();
        let yew_wgpu_enabled = yew_wgpu_enabled;
        Rc::new(move || {
            if !yew_wgpu_enabled || *menu_visible {
                return;
            }
            let Some(info) = puzzle_info.get() else {
                return;
            };
            if info.image_width == 0 || info.image_height == 0 {
                return;
            }
            let cols = info.cols as usize;
            let rows = info.rows as usize;
            let total = cols * rows;
            if total == 0 {
                return;
            }
            let piece_width = info.image_width as f32 / info.cols as f32;
            let piece_height = info.image_height as f32 / info.rows as f32;
            let mask_atlas = match mask_atlas.borrow().as_ref().cloned() {
                Some(mask_atlas) => mask_atlas,
                None => return,
            };
            let (positions_value, rotations_value, flips_value, connections_value, z_order_snapshot) = {
                let snapshot = local_snapshot.borrow();
                if snapshot.positions.len() != total
                    || snapshot.rotations.len() != total
                    || snapshot.flips.len() != total
                    || snapshot.connections.len() != total
                {
                    return;
                }
                (
                    snapshot.positions.clone(),
                    snapshot.rotations.clone(),
                    snapshot.flips.clone(),
                    snapshot.connections.clone(),
                    snapshot.z_order.clone(),
                )
            };
            let z_order_value = if z_order_snapshot.len() == total {
                z_order_snapshot
            } else {
                (*z_order).clone()
            };
            let hovered_id_value = *hovered_id;
            let show_debug_value = *show_debug;
            let dragging_members_value = (*dragging_members).clone();
            let active_id_value = *active_id;
            let sync_view = sync_runtime::sync_view();
            let ownership_by_anchor_value = sync_view.ownership_by_anchor();
            let mp_client_id_value = sync_view.client_id();
            let drag_visual = drag_visual_from_state(&drag_state.borrow());
            let mut drag_members_override: Option<Vec<usize>> = None;
            let mut drag_origin = None;
            let mut drag_dir = 0.0;
            if let Some((members, origin, dir)) = drag_visual {
                drag_members_override = Some(members);
                drag_origin = Some(origin);
                drag_dir = dir;
            }
            let highlight_members = if let Some(members) = drag_members_override.as_ref() {
                Some(members.as_slice())
            } else if active_id_value.is_some() && !dragging_members_value.is_empty() {
                Some(dragging_members_value.as_slice())
            } else {
                None
            };
            let instances = build_wgpu_instances(
                &positions_value,
                &rotations_value,
                &flips_value,
                &z_order_value,
                &connections_value,
                hovered_id_value,
                show_debug_value,
                cols,
                rows,
                piece_width,
                piece_height,
                &mask_atlas,
                highlight_members,
                drag_origin,
                drag_dir,
                &ownership_by_anchor_value,
                mp_client_id_value,
            );
            let mut renderer_ref = wgpu_renderer.borrow_mut();
            let Some(renderer) = renderer_ref.as_mut() else {
                return;
            };
            renderer.set_solved(*solved);
            renderer.update_instances(instances);
            renderer.render();
        })
    };
    let send_sync_action: Rc<dyn Fn(SyncAction)> = {
        let multiplayer_active = multiplayer_active;
        let app_core = app_core.clone();
        Rc::new(move |action: SyncAction| {
            if multiplayer_active {
                let action = CoreAction::Sync(action);
                sync_runtime::dispatch_view_action(&app_core, action, false);
            }
        })
    };
    let bump_sync_revision: Rc<dyn Fn()> = {
        let sync_revision = sync_revision.clone();
        Rc::new(move || {
            sync_revision.set(sync_revision.wrapping_add(1));
        })
    };
    let multiplayer_ui_hooks = {
        let on_welcome = {
            let bump_sync_revision = bump_sync_revision.clone();
            Rc::new(
                move |_room_id: String,
                      _persistence: heddobureika_core::RoomPersistence,
                      _initialized: bool,
                      _client_id: Option<u64>| {
                    bump_sync_revision();
                },
            )
        };
        let on_need_init = {
            let bump_sync_revision = bump_sync_revision.clone();
            Rc::new(move || {
                bump_sync_revision();
            })
        };
        let on_warning = Rc::new(|_minutes_idle| {});
        let on_state = {
            let bump_sync_revision = bump_sync_revision.clone();
            Rc::new(move |_snapshot: GameSnapshot, _seq: u64| {
                bump_sync_revision();
            })
        };
        let on_update = {
            let bump_sync_revision = bump_sync_revision.clone();
            #[cfg(test)]
            let puzzle_info = puzzle_info_store.clone();
            Rc::new(move |_update: RoomUpdate, _seq: u64| {
                #[cfg(test)]
                {
                    if puzzle_info.get().is_none() {
                        record_mp_warn("puzzle info not ready");
                    }
                }
                bump_sync_revision();
            })
        };
        let on_ownership = {
            let bump_sync_revision = bump_sync_revision.clone();
            Rc::new(move |_anchor_id: u32, _owner: Option<u64>| {
                bump_sync_revision();
            })
        };
        let on_drop_not_ready = Rc::new(move || {
            #[cfg(test)]
            record_mp_warn("not ready");
        });
        let on_error = Rc::new(|_code: String, _message: String| {});
        MultiplayerUiHooks {
            on_welcome,
            on_need_init,
            on_warning,
            on_state,
            on_update,
            on_ownership,
            on_drop_not_ready,
            on_error,
        }
    };
    {
        let multiplayer_ui_hooks = multiplayer_ui_hooks.clone();
        let bump_sync_revision = bump_sync_revision.clone();
        use_effect_with((), move |_| {
            let handle = multiplayer_bridge::register_ui_hooks(multiplayer_ui_hooks);
            bump_sync_revision();
            || drop(handle)
        });
    }
    {
        let bump_sync_revision = bump_sync_revision.clone();
        let show_svg = show_svg;
        use_effect_with(show_svg, move |show_svg| {
            if !*show_svg {
                return Box::new(|| ()) as Box<dyn FnOnce()>;
            }
            let handle =
                sync_runtime::register_sync_view_hook(Rc::new(move || bump_sync_revision()));
            Box::new(move || drop(handle)) as Box<dyn FnOnce()>
        });
    }
    {
        let active_id = active_id.clone();
        let dragging_members = dragging_members.clone();
        let animating_members = animating_members.clone();
        let drag_state = drag_state.clone();
        let drag_pending = drag_pending.clone();
        let drag_frame = drag_frame.clone();
        use_effect_with(multiplayer_disconnected, move |disconnected| {
            if !*disconnected {
                return Box::new(|| ()) as Box<dyn FnOnce()>;
            }
            active_id.set(None);
            dragging_members.set(Vec::new());
            animating_members.set(Vec::new());
            *drag_state.borrow_mut() = None;
            drag_pending.borrow_mut().take();
            drag_frame.borrow_mut().take();
            Box::new(|| ()) as Box<dyn FnOnce()>
        });
    }
    #[cfg(test)]
    {
        let send_msg = {
            let multiplayer_callbacks =
                multiplayer_bridge::callbacks_for_tests(app_core.clone());
            sync_runtime::install_test_handler(multiplayer_callbacks)
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
    let mask_atlas_revision_value = *mask_atlas_revision;
    let preview_revealed_value = *preview_revealed;
    let renderer_value = if renderer_is_wgpu { "wgpu" } else { "svg" };
    let mode_value = if multiplayer_active { "online" } else { "local" };
    {
        let render_settings_value = render_settings_value.clone();
        let app_core = app_core.clone();
        use_effect_with(render_settings_value, move |settings| {
            app_router::save_render_settings(settings);
            app_core.set_renderer_kind(settings.renderer);
            app_core.set_svg_animations(settings.svg.animations);
            app_core.set_svg_emboss(settings.svg.emboss);
            app_core.set_svg_fast_render(settings.svg.fast_render);
            app_core.set_svg_fast_filter(settings.svg.fast_filter);
            app_core.set_wgpu_show_fps(settings.wgpu.show_fps);
            app_core.set_wgpu_edge_aa(settings.wgpu.edge_aa);
            app_core.set_wgpu_render_scale(settings.wgpu.render_scale);
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
    let prefers_dark = prefers_dark_mode();
    let is_dark_theme = match theme_mode_value {
        ThemeMode::Dark => true,
        ThemeMode::Light => false,
        ThemeMode::System => prefers_dark,
    };
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
        let connections_value = render_connections.as_slice();
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
    let on_workspace_scale = {
        let workspace_scale = workspace_scale.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WORKSPACE_SCALE_MIN, WORKSPACE_SCALE_MAX);
                workspace_scale.set(value);
                app_core.set_workspace_scale(value);
            }
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
        let local_snapshot = local_snapshot.clone();
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
                let snapshot = local_snapshot.borrow();
                let total = snapshot.positions.len();
                let rotations_snapshot = if enabled {
                    snapshot.rotations.clone()
                } else {
                    vec![0.0; total]
                };
                (
                    snapshot.positions.clone(),
                    rotations_snapshot,
                    snapshot.flips.clone(),
                    snapshot.connections.clone(),
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
        let local_snapshot = local_snapshot.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let snapshot = local_snapshot.borrow();
                let max_value = snapshot
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
        let animating_members = animating_members.clone();
        let rotation_anim = rotation_anim.clone();
        let rotation_anim_handle = rotation_anim_handle.clone();
        let rotation_queue = rotation_queue.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.animations = enabled;
            render_settings.set(next);
            app_core.set_svg_animations(enabled);
            if !enabled {
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
            }
        })
    };
    let on_emboss_toggle = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.emboss = enabled;
            render_settings.set(next);
            app_core.set_svg_emboss(enabled);
        })
    };
    let on_wgpu_fps_toggle = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.wgpu.show_fps = enabled;
            render_settings.set(next);
            app_core.set_wgpu_show_fps(enabled);
        })
    };
    let on_wgpu_edge_aa = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_EDGE_AA_MIN, WGPU_EDGE_AA_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.edge_aa = value;
                render_settings.set(next);
                app_core.set_wgpu_edge_aa(value);
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
        let app_core = app_core.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let value = value.clamp(WGPU_RENDER_SCALE_MIN, WGPU_RENDER_SCALE_MAX);
                let mut next = (*render_settings).clone();
                next.wgpu.render_scale = value;
                render_settings.set(next);
                app_core.set_wgpu_render_scale(value);
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
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.fast_render = enabled;
            render_settings.set(next);
            app_core.set_svg_fast_render(enabled);
        })
    };
    let on_fast_filter_toggle = {
        let render_settings = render_settings.clone();
        let app_core = app_core.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            let mut next = (*render_settings).clone();
            next.svg.fast_filter = enabled;
            render_settings.set(next);
            app_core.set_svg_fast_filter(enabled);
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

    {
        let canvas_ref = canvas_ref.clone();
        let image_element = image_element.clone();
        let wgpu_renderer = wgpu_renderer.clone();
        let pending_instances = pending_instances.clone();
        let pending_ui = pending_ui.clone();
        let mask_atlas = mask_atlas.clone();
        let mask_atlas_revision = mask_atlas_revision.clone();
        let set_hovered = set_hovered.clone();
        let local_snapshot = local_snapshot.clone();
        let z_order = z_order.clone();
        let dragging_members = dragging_members.clone();
        let active_id = active_id.clone();
        let drag_state = drag_state.clone();
        let show_debug = show_debug.clone();
        let wgpu_show_fps = wgpu_show_fps;
        let wgpu_edge_aa = wgpu_edge_aa;
        let wgpu_render_scale = wgpu_render_scale;
        let solved = solved.clone();
        use_effect_with(
            (
                yew_wgpu_enabled,
                board_ready_value,
                puzzle_dims_value,
                image_revision_value,
                grid,
                settings_value.clone(),
                workspace_scale_value,
                image_max_dim,
                depth_cap,
                curve_detail,
                theme_mode_value,
                wgpu_render_scale,
            ),
            move |(
                yew_wgpu_enabled,
                board_ready_value,
                puzzle_dims_value,
                _image_revision_value,
                grid,
                settings_value,
                workspace_scale_value,
                image_max_dim,
                depth_cap,
                curve_detail,
                theme_mode_value,
                wgpu_render_scale,
            )| {
                let cleanup: fn() = || ();
                if *yew_wgpu_enabled {
                    set_hovered(None);
                }
                let build_inputs = if *yew_wgpu_enabled && *board_ready_value {
                    if let (Some((width, height)), Some(image), Some(canvas)) = (
                        *puzzle_dims_value,
                        (*image_element).clone(),
                        canvas_ref.cast::<HtmlCanvasElement>(),
                    ) {
                        Some((width, height, image, canvas))
                    } else {
                        None
                    }
                } else {
                    wgpu_renderer.borrow_mut().take();
                    mask_atlas.borrow_mut().take();
                    pending_instances.borrow_mut().take();
                    None
                };

                if let Some((width, height, image, canvas)) = build_inputs {
                    let grid = *grid;
                    let settings_value = settings_value.clone();
                    let width_f = width as f32;
                    let height_f = height as f32;
                    let layout = compute_workspace_layout(
                        width_f,
                        height_f,
                        *workspace_scale_value,
                        *image_max_dim as f32,
                    );
                    let view_width = layout.view_width;
                    let view_height = layout.view_height;
                    let view_min_x = layout.view_min_x;
                    let view_min_y = layout.view_min_y;
                    let piece_width = width_f / grid.cols as f32;
                    let piece_height = height_f / grid.rows as f32;

                    let pieces = build_pieces(grid.rows, grid.cols);
                    let (horizontal, vertical) =
                        build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &settings_value);
                    let (horizontal_waves, vertical_waves) = build_line_waves(
                        grid.rows,
                        grid.cols,
                        PUZZLE_SEED,
                        piece_width,
                        piece_height,
                        settings_value.line_bend_ratio,
                    );
                    let warp_field = WarpField {
                        width: width_f,
                        height: height_f,
                        horizontal: &horizontal_waves,
                        vertical: &vertical_waves,
                    };
                    let mut paths = Vec::with_capacity(pieces.len());
                    for piece in &pieces {
                        paths.push(build_piece_path(
                            piece,
                            piece_width,
                            piece_height,
                            &horizontal,
                            &vertical,
                            &warp_field,
                            *depth_cap,
                            *curve_detail,
                        ));
                    }

                    let max_depth = piece_width.max(piece_height) * *depth_cap;
                    let max_bend = horizontal_waves
                        .iter()
                        .chain(vertical_waves.iter())
                        .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
                    let mask_pad = (max_depth + max_bend).ceil();
                    let mask_atlas_data = match build_mask_atlas(
                        &pieces,
                        &paths,
                        piece_width,
                        piece_height,
                        grid,
                        mask_pad,
                    ) {
                        Ok(atlas) => Rc::new(atlas),
                        Err(err) => {
                            web_sys::console::error_1(&err);
                            return cleanup;
                        }
                    };
                    *mask_atlas.borrow_mut() = Some(mask_atlas_data.clone());
                    mask_atlas_revision
                        .set(mask_atlas_revision.wrapping_add(1));
                    wgpu_renderer.borrow_mut().take();
                    let prefers_dark = prefers_dark_mode();
                    let is_dark_theme = match theme_mode_value {
                        ThemeMode::Dark => true,
                        ThemeMode::Light => false,
                        ThemeMode::System => prefers_dark,
                    };
                    let render_scale_value = *wgpu_render_scale;
                    spawn_local(async move {
                        let mask_atlas_for_instances = mask_atlas_data.clone();
                        match WgpuRenderer::new(
                            canvas,
                            image,
                            pieces,
                            paths,
                            grid,
                            piece_width,
                            piece_height,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            layout.puzzle_scale,
                            mask_atlas_data,
                            mask_pad,
                            render_scale_value,
                            is_dark_theme,
                        )
                        .await
                        {
                            Ok(mut renderer) => {
                                renderer.set_emboss_enabled(true);
                                renderer.set_font_bytes(FPS_FONT_BYTES.to_vec());
                                renderer.set_ui_font_bytes(FPS_FONT_BYTES.to_vec());
                                renderer.set_edge_aa(wgpu_edge_aa);
                                renderer.set_solved(*solved);
                                let total = (grid.cols as usize) * (grid.rows as usize);
                                if let Some(instances) = pending_instances.borrow_mut().take() {
                                    if instances.instances.len() == total {
                                        renderer.update_instances(instances);
                                    }
                                    renderer.set_show_fps(wgpu_show_fps);
                                    if let Some(specs) = pending_ui.borrow_mut().take() {
                                        renderer.set_ui_texts(&specs);
                                    }
                                    renderer.render();
                                } else {
                                    let snapshot = local_snapshot.borrow();
                                    let positions_snapshot = snapshot.positions.clone();
                                    let rotations_snapshot = snapshot.rotations.clone();
                                    let flips_snapshot = snapshot.flips.clone();
                                    let connections_snapshot = snapshot.connections.clone();
                                    let z_order_snapshot = if snapshot.z_order.len() == total {
                                        snapshot.z_order.clone()
                                    } else {
                                        (*z_order).clone()
                                    };
                                    let hovered_snapshot = *hovered_id;
                                    let dragging_snapshot = (*dragging_members).clone();
                                    let drag_visual = drag_visual_from_state(&drag_state.borrow());
                                    let mut drag_members_override: Option<Vec<usize>> = None;
                                    let mut drag_origin = None;
                                    let mut drag_dir = 0.0;
                                    if let Some((members, origin, dir)) = drag_visual {
                                        drag_members_override = Some(members);
                                        drag_origin = Some(origin);
                                        drag_dir = dir;
                                    }
                                    let highlight_members = if let Some(members) =
                                        drag_members_override.as_ref()
                                    {
                                        Some(members.as_slice())
                                    } else if (*active_id).is_some()
                                        && !dragging_snapshot.is_empty()
                                    {
                                        Some(dragging_snapshot.as_slice())
                                    } else {
                                        None
                                    };
                                    let sync_view = sync_runtime::sync_view();
                                    let ownership_by_anchor_value =
                                        sync_view.ownership_by_anchor();
                                    let mp_client_id_value = sync_view.client_id();
                                    let show_debug_snapshot = *show_debug;
                                    let has_state = positions_snapshot.len() == total
                                        && rotations_snapshot.len() == total
                                        && flips_snapshot.len() == total
                                        && z_order_snapshot.len() == total
                                        && connections_snapshot.len() == total;
                                    if has_state {
                                        let instances = build_wgpu_instances(
                                            &positions_snapshot,
                                            &rotations_snapshot,
                                            &flips_snapshot,
                                            &z_order_snapshot,
                                            &connections_snapshot,
                                            hovered_snapshot,
                                            show_debug_snapshot,
                                            grid.cols as usize,
                                            grid.rows as usize,
                                            piece_width,
                                            piece_height,
                                            &mask_atlas_for_instances,
                                            highlight_members,
                                            drag_origin,
                                            drag_dir,
                                            &ownership_by_anchor_value,
                                            mp_client_id_value,
                                        );
                                        renderer.update_instances(instances);
                                    }
                                    renderer.set_show_fps(wgpu_show_fps);
                                    if let Some(specs) = pending_ui.borrow_mut().take() {
                                        renderer.set_ui_texts(&specs);
                                    }
                                    renderer.render();
                                }
                                *wgpu_renderer.borrow_mut() = Some(renderer);
                            }
                            Err(err) => {
                                web_sys::console::error_1(&err);
                            }
                        }
                    });
                }
                cleanup
            },
        );
    }

    {
        let wgpu_renderer = wgpu_renderer.clone();
        let pending_instances = pending_instances.clone();
        let pending_ui = pending_ui.clone();
        let ui_credit_hitbox = ui_credit_hitbox.clone();
        let ui_measure_state = ui_measure_state.clone();
        let ui_credit_hovered = ui_credit_hovered.clone();
        let mask_atlas = mask_atlas.clone();
        let drag_state = drag_state.clone();
        let local_snapshot = local_snapshot.clone();
        let wgpu_render_deps = WgpuRenderDeps {
            using_wgpu: yew_wgpu_enabled,
            puzzle_dims: puzzle_dims_value,
            grid,
            workspace_scale: workspace_scale_value,
            image_max_dim,
            theme_mode: theme_mode_value,
            solved: solved_value,
            menu_visible: menu_visible_value,
            ui_revision: ui_revision_value,
            z_order: (*z_order).clone(),
            hover_deps: hover_deps.clone(),
            mask_atlas_revision: mask_atlas_revision_value,
            wgpu_settings: wgpu_settings_value.clone(),
            ownership_by_anchor: ownership_by_anchor_value.as_ref().clone(),
            mp_client_id: mp_client_id_value,
        };
        use_effect_with(wgpu_render_deps, move |deps| {
                let cleanup: fn() = || ();
                let _ = deps.ui_revision;
                let _ = deps.mask_atlas_revision;
                if !deps.using_wgpu {
                    *ui_credit_hitbox.borrow_mut() = None;
                    if *ui_credit_hovered {
                        ui_credit_hovered.set(false);
                    }
                    return cleanup;
                }
                let Some((width, height)) = deps.puzzle_dims else {
                    return cleanup;
                };

                let prefers_dark = prefers_dark_mode();
                let is_dark_theme = match deps.theme_mode {
                    ThemeMode::Dark => true,
                    ThemeMode::Light => false,
                    ThemeMode::System => prefers_dark,
                };
                let width_f = width as f32;
                let height_f = height as f32;
                let layout = compute_workspace_layout(
                    width_f,
                    height_f,
                    deps.workspace_scale,
                    deps.image_max_dim as f32,
                );
                let cols = deps.grid.cols as usize;
                let rows = deps.grid.rows as usize;
                let piece_width = width as f32 / deps.grid.cols as f32;
                let piece_height = height as f32 / deps.grid.rows as f32;
                let total = cols * rows;
                let snapshot = local_snapshot.borrow();
                let positions_value = snapshot.positions.clone();
                let rotations_value = snapshot.rotations.clone();
                let flips_value = snapshot.flips.clone();
                let connections_value = snapshot.connections.clone();
                let z_order_value = if snapshot.z_order.len() == total {
                    snapshot.z_order.clone()
                } else {
                    deps.z_order.clone()
                };
                let HoverDeps {
                    hovered_id,
                    active_id,
                    dragging_members,
                    show_debug,
                } = deps.hover_deps.clone();
                let has_state = positions_value.len() == total
                    && rotations_value.len() == total
                    && flips_value.len() == total
                    && z_order_value.len() == total
                    && connections_value.len() == total;
                if !has_state {
                    pending_instances.borrow_mut().take();
                    return cleanup;
                }
                let Some(mask_atlas) = mask_atlas.borrow().as_ref().cloned() else {
                    pending_instances.borrow_mut().take();
                    return cleanup;
                };
                let drag_visual = drag_visual_from_state(&drag_state.borrow());
                let mut drag_members_override: Option<Vec<usize>> = None;
                let mut drag_origin = None;
                let mut drag_dir = 0.0;
                if let Some((members, origin, dir)) = drag_visual {
                    drag_members_override = Some(members);
                    drag_origin = Some(origin);
                    drag_dir = dir;
                }
                let ownership_by_anchor_value = &deps.ownership_by_anchor;
                let mp_client_id_value = deps.mp_client_id;
                let highlight_members = if let Some(members) = drag_members_override.as_ref() {
                    Some(members.as_slice())
                } else if active_id.is_some() && !dragging_members.is_empty() {
                    Some(dragging_members.as_slice())
                } else {
                    None
                };
                let show_menu = deps.menu_visible;
                let instances = if show_menu {
                    InstanceSet {
                        instances: Vec::new(),
                        batches: Vec::new(),
                    }
                } else {
                    build_wgpu_instances(
                        &positions_value,
                        &rotations_value,
                        &flips_value,
                        &z_order_value,
                        &connections_value,
                        hovered_id,
                        show_debug,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        &mask_atlas,
                        highlight_members,
                        drag_origin,
                        drag_dir,
                        ownership_by_anchor_value,
                        mp_client_id_value,
                    )
                };
                let (connections_label, border_connections_label) = if connections_value.len() == total {
                    let (connected, border_connected, total_expected, border_expected) =
                        count_connections(&connections_value, cols, rows);
                    (
                        format_progress(connected, total_expected),
                        format_progress(border_connected, border_expected),
                    )
                } else {
                    ("--".to_string(), "--".to_string())
                };
                let mut measure_state = ui_measure_state.borrow_mut();
                let ui_specs = build_ui_specs(
                    &mut measure_state,
                    layout,
                    width_f,
                    height_f,
                    UI_TITLE_TEXT,
                    deps.solved,
                    &connections_label,
                    &border_connections_label,
                    show_menu,
                    is_dark_theme,
                );
                let credit_hitbox = ui_specs
                    .iter()
                    .find(|spec| matches!(spec.id, UiTextId::Credit))
                    .map(|spec| ui_hitbox_for_spec(&mut measure_state, spec));
                *ui_credit_hitbox.borrow_mut() = credit_hitbox;
                if ui_credit_hitbox.borrow().is_none() && *ui_credit_hovered {
                    ui_credit_hovered.set(false);
                }
                let mut renderer_ref = wgpu_renderer.borrow_mut();
                if let Some(renderer) = renderer_ref.as_mut() {
                    renderer.set_emboss_enabled(true);
                    renderer.set_edge_aa(deps.wgpu_settings.edge_aa);
                    renderer.set_show_fps(deps.wgpu_settings.show_fps);
                    renderer.set_show_frame(!show_menu);
                    renderer.set_solved(deps.solved);
                    renderer.set_ui_texts(&ui_specs);
                    renderer.update_instances(instances);
                    renderer.render();
                    pending_instances.borrow_mut().take();
                } else {
                    *pending_instances.borrow_mut() = Some(instances);
                    *pending_ui.borrow_mut() = Some(ui_specs);
                }
                cleanup
            });
    }

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

    {
        let drag_handlers = drag_handlers.clone();
        use_effect_with(
            (),
            move |_| {
                let window = web_sys::window().expect("window available");
                let move_handlers = drag_handlers.clone();
                let move_listener = EventListener::new_with_options(
                    &window,
                    "mousemove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = move_handlers.borrow().on_move.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_move_handlers = drag_handlers.clone();
                let touch_move_listener = EventListener::new_with_options(
                    &window,
                    "touchmove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) = touch_move_handlers.borrow().on_touch_move.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let up_handlers = drag_handlers.clone();
                let up_listener = EventListener::new_with_options(
                    &window,
                    "mouseup",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = up_handlers.borrow().on_release.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_end_handlers = drag_handlers.clone();
                let touch_end_listener = EventListener::new_with_options(
                    &window,
                    "touchend",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_end_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_cancel_handlers = drag_handlers.clone();
                let touch_cancel_listener = EventListener::new_with_options(
                    &window,
                    "touchcancel",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_cancel_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                || {
                    drop(move_listener);
                    drop(touch_move_listener);
                    drop(up_listener);
                    drop(touch_end_listener);
                    drop(touch_cancel_listener);
                }
            },
        );
    }

    {
        let image_element = image_element.clone();
        let image_revision = image_revision.clone();
        let show_svg = show_svg;
        use_effect_with(
            (image_max_dim, image_render_scale, puzzle_src, show_svg),
            move |(image_max_dim, image_render_scale, puzzle_src, show_svg)| {
                let cleanup = || ();
                if !*show_svg {
                    return cleanup;
                }
                let img = HtmlImageElement::new().expect("create image element");
                let img_clone = img.clone();
                let logical_max_dim = *image_max_dim;
                let render_scale = image_render_scale
                    .clamp(WGPU_RENDER_SCALE_MIN, WGPU_RENDER_SCALE_MAX);
                let source_cap = (IMAGE_MAX_DIMENSION_MAX as f32 * WGPU_RENDER_SCALE_MAX)
                    .round()
                    .max(1.0) as u32;
                let mut source_max_dim =
                    ((logical_max_dim as f32) * render_scale).round().max(1.0) as u32;
                if source_max_dim > source_cap {
                    source_max_dim = source_cap;
                }
                let onload = Closure::<dyn FnMut()>::wrap(Box::new(move || {
                    let width = img_clone.natural_width();
                    let height = img_clone.natural_height();
                    let max_axis = width.max(height).max(1);
                    let source_scale = if max_axis > source_max_dim {
                        (source_max_dim as f64) / (max_axis as f64)
                    } else {
                        1.0
                    };
                    let target_w = ((width as f64) * source_scale).round().max(1.0) as u32;
                    let target_h = ((height as f64) * source_scale).round().max(1.0) as u32;
                    if target_w == width && target_h == height {
                        image_element.set(Some(img_clone.clone()));
                        let next = (*image_revision).wrapping_add(1);
                        image_revision.set(next);
                        return;
                    }
                    let document = match web_sys::window().and_then(|window| window.document()) {
                        Some(doc) => doc,
                        None => {
                            image_element.set(Some(img_clone.clone()));
                            let next = (*image_revision).wrapping_add(1);
                            image_revision.set(next);
                            return;
                        }
                    };
                    let canvas = match document
                        .create_element("canvas")
                        .ok()
                        .and_then(|node| node.dyn_into::<HtmlCanvasElement>().ok())
                    {
                        Some(canvas) => canvas,
                        None => {
                            image_element.set(Some(img_clone.clone()));
                            let next = (*image_revision).wrapping_add(1);
                            image_revision.set(next);
                            return;
                        }
                    };
                    canvas.set_width(target_w);
                    canvas.set_height(target_h);
                    let ctx = match canvas
                        .get_context("2d")
                        .ok()
                        .flatten()
                        .and_then(|ctx| ctx.dyn_into::<CanvasRenderingContext2d>().ok())
                    {
                        Some(ctx) => ctx,
                        None => {
                            image_element.set(Some(img_clone.clone()));
                            let next = (*image_revision).wrapping_add(1);
                            image_revision.set(next);
                            return;
                        }
                    };
                    ctx.set_image_smoothing_enabled(true);
                    if ctx
                        .draw_image_with_html_image_element_and_dw_and_dh(
                            &img_clone,
                            0.0,
                            0.0,
                            target_w as f64,
                            target_h as f64,
                        )
                        .is_err()
                    {
                        image_element.set(Some(img_clone.clone()));
                        let next = (*image_revision).wrapping_add(1);
                        image_revision.set(next);
                        return;
                    }
                    let data_url = match canvas.to_data_url() {
                        Ok(data_url) => data_url,
                        Err(_) => {
                            image_element.set(Some(img_clone.clone()));
                            let next = (*image_revision).wrapping_add(1);
                            image_revision.set(next);
                            return;
                        }
                    };
                    let scaled = match HtmlImageElement::new() {
                        Ok(image) => image,
                        Err(_) => {
                            image_element.set(Some(img_clone.clone()));
                            let next = (*image_revision).wrapping_add(1);
                            image_revision.set(next);
                            return;
                        }
                    };
                    let scaled_clone = scaled.clone();
                    let image_element_scaled = image_element.clone();
                    let image_revision_scaled = image_revision.clone();
                    let onload_scaled = Closure::<dyn FnMut()>::wrap(Box::new(move || {
                        image_element_scaled.set(Some(scaled_clone.clone()));
                        let next = (*image_revision_scaled).wrapping_add(1);
                        image_revision_scaled.set(next);
                    }));
                    scaled.set_onload(Some(onload_scaled.as_ref().unchecked_ref()));
                    scaled.set_src(&data_url);
                    onload_scaled.forget();
                }));
                img.set_onload(Some(onload.as_ref().unchecked_ref()));
                img.set_src(puzzle_src);
                onload.forget();
                cleanup
            },
        );
    }

    let (content, on_scramble, on_solve, on_solve_rotation, on_unflip, scramble_disabled) =
        if let Some((width, height)) = puzzle_dims_value {
        let width_f = width as f32;
        let height_f = height as f32;
        let layout = compute_workspace_layout(
            width_f,
            height_f,
            workspace_scale_value,
            image_max_dim as f32,
        );
        let view_width = layout.view_width;
        let view_height = layout.view_height;
        let view_min_x = layout.view_min_x;
        let view_min_y = layout.view_min_y;
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let puzzle_view_min_x = view_min_x / puzzle_scale;
        let puzzle_view_min_y = view_min_y / puzzle_scale;
        let puzzle_view_width = view_width / puzzle_scale;
        let puzzle_view_height = view_height / puzzle_scale;
        let view_box = format!(
            "{} {} {} {}",
            fmt_f32(view_min_x),
            fmt_f32(view_min_y),
            fmt_f32(view_width),
            fmt_f32(view_height)
        );
        let mut measure_state = ui_measure_state.borrow_mut();
        let ui_specs = build_ui_specs(
            &mut measure_state,
            layout,
            width_f,
            height_f,
            UI_TITLE_TEXT,
            solved_value,
            connections_label.as_str(),
            border_connections_label.as_str(),
            menu_visible_value,
            is_dark_theme,
        );
        let piece_width = width_f / grid.cols as f32;
        let piece_height = height_f / grid.rows as f32;
        let max_depth = piece_width.max(piece_height) * depth_cap;
        let bend_ratio =
            settings_value
                .line_bend_ratio
                .clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
        let max_bend = piece_width.max(piece_height) * bend_ratio;
        let mask_pad = (max_depth + max_bend).ceil();
        let base_w = piece_width + mask_pad * 2.0;
        let base_h = piece_height + mask_pad * 2.0;
        let base_diag = (base_w * base_w + base_h * base_h).sqrt();
        let rotation_pad = (base_diag - base_w.max(base_h)) * 0.5;
        let emboss_pad = EMBOSS_OFFSET.abs() + EMBOSS_RIM + rotation_pad;
        let mut frame_corner_radius_value = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
        let max_corner_radius = piece_width.min(piece_height) * 0.45;
        if frame_corner_radius_value > max_corner_radius {
            frame_corner_radius_value = max_corner_radius;
        }
        let frame_corner_radius = fmt_f32(frame_corner_radius_value);
        let mask_x = fmt_f32(-mask_pad);
        let mask_y = fmt_f32(-mask_pad);
        let mask_width = fmt_f32(piece_width + mask_pad * 2.0);
        let mask_height = fmt_f32(piece_height + mask_pad * 2.0);
        let emboss_x = fmt_f32(-mask_pad - emboss_pad);
        let emboss_y = fmt_f32(-mask_pad - emboss_pad);
        let emboss_width = fmt_f32(piece_width + (mask_pad + emboss_pad) * 2.0);
        let emboss_height = fmt_f32(piece_height + (mask_pad + emboss_pad) * 2.0);
        let emboss_offset_neg = fmt_f32(-EMBOSS_OFFSET);
        let emboss_rim_radius = fmt_f32(EMBOSS_RIM);
        let center_min_x = puzzle_view_min_x + piece_width * 0.5;
        let center_min_y = puzzle_view_min_y + piece_height * 0.5;
        let mut center_max_x = puzzle_view_min_x + puzzle_view_width - piece_width * 0.5;
        let mut center_max_y = puzzle_view_min_y + puzzle_view_height - piece_height * 0.5;
        if center_max_x < center_min_x {
            center_max_x = center_min_x;
        }
        if center_max_y < center_min_y {
            center_max_y = center_min_y;
        }
        let piece_shapes: Vec<(Piece, PiecePaths)> = if !svg_enabled {
            Vec::new()
        } else {
            let pieces = build_pieces(grid.rows, grid.cols);
            let (horizontal, vertical) =
                build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &settings_value);
            let (horizontal_waves, vertical_waves) = build_line_waves(
                grid.rows,
                grid.cols,
                PUZZLE_SEED,
                piece_width,
                piece_height,
                settings_value.line_bend_ratio,
            );
            let warp_field = WarpField {
                width: width_f,
                height: height_f,
                horizontal: &horizontal_waves,
                vertical: &vertical_waves,
            };
            pieces
                .iter()
                .map(|piece| {
                    let paths = build_piece_path(
                        piece,
                        piece_width,
                        piece_height,
                        &horizontal,
                        &vertical,
                        &warp_field,
                        depth_cap,
                        curve_detail,
                    );
                    (*piece, paths)
                })
                .collect()
        };

        let cols = grid.cols as usize;
        let rows = grid.rows as usize;
        let total = cols * rows;
        let positions_value = render_positions.clone();
        let rotations_value = render_rotations.clone();
        let flips_value = render_flips.clone();
        let connections_value = render_connections.clone();
        let hovered_group = if active_id_value.is_some() && !dragging_members_value.is_empty() {
            dragging_members_value.clone()
        } else if let Some(id) = hovered_id_value {
            if id < connections_value.len() {
                collect_group(&connections_value, id, grid.cols as usize, grid.rows as usize)
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        let mut hovered_mask = vec![false; total];
        for id in &hovered_group {
            if *id < hovered_mask.len() {
                hovered_mask[*id] = true;
            }
        }
        let mut owned_mask = vec![false; total];
        if !ownership_by_anchor_value.is_empty() {
            let anchor_of = anchor_of_from_connections(&connections_value, cols, rows);
            for (id, anchor) in anchor_of.iter().enumerate() {
                if let Some(owner_id) = ownership_by_anchor_value.get(&(*anchor as u32)) {
                    if Some(*owner_id) != mp_client_id_value {
                        owned_mask[id] = true;
                    }
                }
            }
        }
        let mut dragging_mask = vec![false; total];
        for id in &dragging_members_value {
            if *id < dragging_mask.len() {
                dragging_mask[*id] = true;
            }
        }
        let mut animating_mask = vec![false; total];
        for id in &animating_members_value {
            if *id < animating_mask.len() {
                animating_mask[*id] = true;
            }
        }
        let (drag_right_click, drag_origin) = drag_state
            .borrow()
            .as_ref()
            .map(|drag| (drag.right_click, Some((drag.cursor_x, drag.cursor_y))))
            .unwrap_or((false, None));
        let drag_dir: f32 = if dragging_members_value.is_empty() {
            0.0
        } else if drag_right_click {
            -1.0
        } else {
            1.0
        };
        let drag_rotation = if drag_dir.abs() > f32::EPSILON {
            drag_angle_for_group(dragging_members_value.len()) * drag_dir.signum()
        } else {
            0.0
        };
        let drag_scale = if drag_dir.abs() > f32::EPSILON {
            DRAG_SCALE
        } else {
            1.0
        };
        let drag_center = if drag_dir.abs() > f32::EPSILON {
            drag_origin.or_else(|| {
                drag_group_center(&positions_value, &dragging_members_value, piece_width, piece_height)
            })
        } else {
            None
        };
        let z_order_value = if render_z_order.len() == total {
            render_z_order.clone()
        } else {
            (*z_order).clone()
        };
        let drag_move_common: Rc<dyn Fn(f32, f32) -> bool> = {
            let local_snapshot = local_snapshot.clone();
            let bump_ui_revision = bump_ui_revision.clone();
            let positions_live = positions_live.clone();
            let rotations_live = rotations_live.clone();
            let group_anchor = group_anchor.clone();
            let group_pos = group_pos.clone();
            let group_rot = group_rot.clone();
            let group_pos_live = group_pos_live.clone();
            let group_rot_live = group_rot_live.clone();
            let drag_state = drag_state.clone();
            let wgpu_renderer = wgpu_renderer.clone();
            let mask_atlas = mask_atlas.clone();
            let z_order = z_order.clone();
            let show_debug = show_debug.clone();
            let send_sync_action = send_sync_action.clone();
            let dispatch_view_action = dispatch_view_action.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let wgpu_edge_aa = wgpu_edge_aa;
            let allow_drag = allow_drag;
            Rc::new(move |x: f32, y: f32| {
                if !allow_drag {
                    return false;
                }
                dispatch_view_action(CoreAction::DragMove { x, y });
                let drag = {
                    let mut drag_ref = drag_state.borrow_mut();
                    if let Some(drag) = drag_ref.as_mut() {
                        drag.cursor_x = x;
                        drag.cursor_y = y;
                        Some(drag.clone())
                    } else {
                        None
                    }
                };
                if let Some(drag) = drag {
                    let render_wgpu = |positions_snapshot: &[(f32, f32)],
                                       rotations_snapshot: &[f32],
                                       drag: &DragState| {
                        let mask_atlas_ref = mask_atlas.borrow();
                        let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                            return;
                        };
                        let sync_view = sync_runtime::sync_view();
                        let ownership_by_anchor_value = sync_view.ownership_by_anchor();
                        let mp_client_id_value = sync_view.client_id();
                        let snapshot = local_snapshot.borrow();
                        let flips_snapshot = snapshot.flips.as_slice();
                        let connections_snapshot = snapshot.connections.as_slice();
                        let z_order_snapshot = if snapshot.z_order.len() == cols * rows {
                            snapshot.z_order.as_slice()
                        } else {
                            &*z_order
                        };
                        let drag_dir = if drag.right_click { -1.0 } else { 1.0 };
                        let drag_origin = Some((drag.cursor_x, drag.cursor_y));
                        let instances = build_wgpu_instances(
                            positions_snapshot,
                            rotations_snapshot,
                            flips_snapshot,
                            z_order_snapshot,
                            connections_snapshot,
                            None,
                            *show_debug,
                            cols,
                            rows,
                            piece_width,
                            piece_height,
                            mask_atlas,
                            Some(drag.members.as_slice()),
                            drag_origin,
                            drag_dir,
                            &ownership_by_anchor_value,
                            mp_client_id_value,
                        );
                        let mut renderer_ref = wgpu_renderer.borrow_mut();
                        if let Some(renderer) = renderer_ref.as_mut() {
                            renderer.set_edge_aa(wgpu_edge_aa);
                            renderer.set_solved(solved_value);
                            renderer.update_instances(instances);
                            renderer.render();
                        }
                    };
                    if drag.rotate_mode && rotation_enabled_value {
                        let current_angle = (y - drag.pivot_y).atan2(x - drag.pivot_x);
                        let delta_deg = (current_angle - drag.start_angle).to_degrees();
                        let anchor_id = drag.anchor_id;
                        let flipped = local_snapshot
                            .borrow()
                            .flips
                            .get(anchor_id)
                            .copied()
                            .unwrap_or(false);
                        if yew_wgpu_enabled {
                            {
                                let mut group_pos_ref = group_pos_live.borrow_mut();
                                let mut group_rot_ref = group_rot_live.borrow_mut();
                                if anchor_id < group_pos_ref.len()
                                    && anchor_id < group_rot_ref.len()
                                {
                                    let anchor_center = (
                                        drag.anchor_pos.0 + piece_width * 0.5,
                                        drag.anchor_pos.1 + piece_height * 0.5,
                                    );
                                    let (rx, ry) = rotate_point(
                                        anchor_center.0,
                                        anchor_center.1,
                                        drag.pivot_x,
                                        drag.pivot_y,
                                        delta_deg,
                                    );
                                    group_pos_ref[anchor_id] = (
                                        rx - piece_width * 0.5,
                                        ry - piece_height * 0.5,
                                    );
                                    let signed_delta = if flipped { -delta_deg } else { delta_deg };
                                    group_rot_ref[anchor_id] =
                                        normalize_angle(drag.anchor_rot + signed_delta);
                                    let mut positions_ref = positions_live.borrow_mut();
                                    let mut rotations_ref = rotations_live.borrow_mut();
                                    update_group_members_state(
                                        &drag.members,
                                        anchor_id,
                                        &group_pos_ref,
                                        &group_rot_ref,
                                        cols,
                                        piece_width,
                                        piece_height,
                                        &mut positions_ref,
                                        &mut rotations_ref,
                                    );
                                }
                            }
                            let positions_snapshot = positions_live.borrow();
                            let rotations_snapshot = rotations_live.borrow();
                            render_wgpu(&positions_snapshot, &rotations_snapshot, &drag);
                            return true;
                        }
                        let mut next_group_pos = (*group_pos).clone();
                        let mut next_group_rot = (*group_rot).clone();
                        if anchor_id < next_group_pos.len() && anchor_id < next_group_rot.len() {
                            let anchor_center = (
                                drag.anchor_pos.0 + piece_width * 0.5,
                                drag.anchor_pos.1 + piece_height * 0.5,
                            );
                            let (rx, ry) = rotate_point(
                                anchor_center.0,
                                anchor_center.1,
                                drag.pivot_x,
                                drag.pivot_y,
                                delta_deg,
                            );
                            next_group_pos[anchor_id] = (
                                rx - piece_width * 0.5,
                                ry - piece_height * 0.5,
                            );
                            let signed_delta = if flipped { -delta_deg } else { delta_deg };
                            next_group_rot[anchor_id] =
                                normalize_angle(drag.anchor_rot + signed_delta);
                            let anchor_of = &*group_anchor;
                            if !anchor_of.is_empty() {
                                let (derived_positions, derived_rotations) = derive_piece_state(
                                    anchor_of,
                                    &next_group_pos,
                                    &next_group_rot,
                                    cols,
                                    piece_width,
                                    piece_height,
                                );
                                log_state_update(
                                    "positions",
                                    derived_positions.len(),
                                    "drag_rotate",
                                );
                                log_state_update(
                                    "rotations",
                                    derived_rotations.len(),
                                    "drag_rotate",
                                );
                                {
                                    let mut snapshot = local_snapshot.borrow_mut();
                                    snapshot.positions = derived_positions;
                                    snapshot.rotations = derived_rotations;
                                }
                                bump_ui_revision();
                            }
                            group_pos.set(next_group_pos);
                            group_rot.set(next_group_rot);
                        }
                        true
                    } else {
                        let mut dx = x - drag.start_x;
                        let mut dy = y - drag.start_y;
                        if !drag.start_positions.is_empty() {
                            let rubber_limit = piece_width.min(piece_height) * RUBBER_BAND_RATIO;
                            let (bounds_min_x, bounds_max_x, bounds_min_y, bounds_max_y) =
                                if drag.members.len() > 1 {
                                    let mut min_x = center_min_x + piece_width;
                                    let mut max_x = center_max_x - piece_width;
                                    let mut min_y = center_min_y + piece_height;
                                    let mut max_y = center_max_y - piece_height;
                                    if max_x < min_x {
                                        let mid = (center_min_x + center_max_x) * 0.5;
                                        min_x = mid;
                                        max_x = mid;
                                    }
                                    if max_y < min_y {
                                        let mid = (center_min_y + center_max_y) * 0.5;
                                        min_y = mid;
                                        max_y = mid;
                                    }
                                    (min_x, max_x, min_y, max_y)
                                } else {
                                    (
                                        center_min_x,
                                        center_max_x,
                                        center_min_y,
                                        center_max_y,
                                    )
                                };
                            let mut in_bounds = false;
                            let mut best_dx = dx;
                            let mut best_dy = dy;
                            let mut best_dist = f32::INFINITY;
                            for start in &drag.start_positions {
                                let center_x = start.0 + piece_width * 0.5;
                                let center_y = start.1 + piece_height * 0.5;
                                let min_dx = bounds_min_x - center_x;
                                let max_dx = bounds_max_x - center_x;
                                let min_dy = bounds_min_y - center_y;
                                let max_dy = bounds_max_y - center_y;
                                if dx >= min_dx && dx <= max_dx && dy >= min_dy && dy <= max_dy {
                                    in_bounds = true;
                                    break;
                                }
                                let cand_dx = rubber_band_clamp(dx, min_dx, max_dx, rubber_limit);
                                let cand_dy = rubber_band_clamp(dy, min_dy, max_dy, rubber_limit);
                                let delta_dx = cand_dx - dx;
                                let delta_dy = cand_dy - dy;
                                let dist = delta_dx * delta_dx + delta_dy * delta_dy;
                                if dist < best_dist {
                                    best_dist = dist;
                                    best_dx = cand_dx;
                                    best_dy = cand_dy;
                                }
                            }
                            if !in_bounds {
                                dx = best_dx;
                                dy = best_dy;
                            }
                        }
                        if yew_wgpu_enabled {
                            let anchor_id = drag.anchor_id;
                            {
                                let mut group_pos_ref = group_pos_live.borrow_mut();
                                let group_rot_ref = group_rot_live.borrow();
                                if anchor_id < group_pos_ref.len() {
                                    group_pos_ref[anchor_id] =
                                        (drag.anchor_pos.0 + dx, drag.anchor_pos.1 + dy);
                                    let anchor_pos = group_pos_ref[anchor_id];
                                    send_sync_action(SyncAction::Move {
                                        anchor_id,
                                        pos: anchor_pos,
                                    });
                                    let mut positions_ref = positions_live.borrow_mut();
                                    let mut rotations_ref = rotations_live.borrow_mut();
                                    update_group_members_state(
                                        &drag.members,
                                        anchor_id,
                                        &group_pos_ref,
                                        &group_rot_ref,
                                        cols,
                                        piece_width,
                                        piece_height,
                                        &mut positions_ref,
                                        &mut rotations_ref,
                                    );
                                }
                            }
                            let positions_snapshot = positions_live.borrow();
                            let rotations_snapshot = rotations_live.borrow();
                            render_wgpu(&positions_snapshot, &rotations_snapshot, &drag);
                            return true;
                        }
                        let mut next_group_pos = (*group_pos).clone();
                        let anchor_id = drag.anchor_id;
                        if anchor_id < next_group_pos.len() {
                            next_group_pos[anchor_id] =
                                (drag.anchor_pos.0 + dx, drag.anchor_pos.1 + dy);
                            let anchor_pos = next_group_pos[anchor_id];
                            send_sync_action(SyncAction::Move {
                                anchor_id,
                                pos: anchor_pos,
                            });
                            let anchor_of = &*group_anchor;
                            if !anchor_of.is_empty() {
                                let group_rot_snapshot = (*group_rot).clone();
                                let (derived_positions, derived_rotations) = derive_piece_state(
                                    anchor_of,
                                    &next_group_pos,
                                    &group_rot_snapshot,
                                    cols,
                                    piece_width,
                                    piece_height,
                                );
                                log_state_update(
                                    "positions",
                                    derived_positions.len(),
                                    "drag_move",
                                );
                                log_state_update(
                                    "rotations",
                                    derived_rotations.len(),
                                    "drag_move",
                                );
                                {
                                    let mut snapshot = local_snapshot.borrow_mut();
                                    snapshot.positions = derived_positions;
                                    snapshot.rotations = derived_rotations;
                                }
                                bump_ui_revision();
                            }
                            group_pos.set(next_group_pos);
                        }
                        true
                    }
                } else {
                    false
                }
            })
        };
        let schedule_drag_move: Rc<dyn Fn(f32, f32) -> bool> = {
            let drag_move_common = drag_move_common.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            Rc::new(move |x, y| {
                *drag_pending.borrow_mut() = Some((x, y));
                if drag_frame.borrow().is_some() {
                    return true;
                }
                let drag_pending = drag_pending.clone();
                let drag_frame_for_tick = drag_frame.clone();
                let drag_move_common = drag_move_common.clone();
                let handle = request_animation_frame(move |_| {
                    let coords = drag_pending.borrow_mut().take();
                    drag_frame_for_tick.borrow_mut().take();
                    if let Some((x, y)) = coords {
                        drag_move_common(x, y);
                    }
                });
                *drag_frame.borrow_mut() = Some(handle);
                true
            })
        };
        let drag_move = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let schedule_drag_move = schedule_drag_move.clone();
            let puzzle_scale = puzzle_scale;
            move |event: &MouseEvent| {
                let target_ref = if yew_wgpu_enabled {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                if let Some((x, y)) = event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) {
                    let (x, y) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                    if schedule_drag_move(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_move_touch = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let drag_state = drag_state.clone();
            let active_id = active_id.clone();
            let dragging_members = dragging_members.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            let schedule_drag_move = schedule_drag_move.clone();
            let puzzle_scale = puzzle_scale;
            let send_sync_action = send_sync_action.clone();
            move |event: &TouchEvent| {
                if event.touches().length() > 1 {
                    if let Some(drag) = drag_state.borrow().clone() {
                        send_sync_action(SyncAction::Release {
                            anchor_id: drag.anchor_id,
                        });
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                        drag_pending.borrow_mut().take();
                        drag_frame.borrow_mut().take();
                    }
                    return;
                }
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                let target_ref = if yew_wgpu_enabled {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                if let Some((x, y)) = touch_event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    false,
                ) {
                    let (x, y) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                    if schedule_drag_move(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_release_common: Rc<dyn Fn(Option<(f32, f32)>) -> bool> = {
            let local_snapshot = local_snapshot.clone();
            let bump_ui_revision = bump_ui_revision.clone();
            let positions_live = positions_live.clone();
            let rotations_live = rotations_live.clone();
            let apply_puzzle_state = apply_puzzle_state.clone();
            let puzzle_state = puzzle_state.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let group_anchor = group_anchor.clone();
            let group_pos = group_pos.clone();
            let group_rot = group_rot.clone();
            let group_order = group_order.clone();
            let z_order = z_order.clone();
            let scramble_nonce = scramble_nonce.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            let send_sync_action = send_sync_action.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let allow_drag = allow_drag;
            Rc::new(move |coords: Option<(f32, f32)>| {
                if !allow_drag {
                    active_id.set(None);
                    dragging_members.set(Vec::new());
                    animating_members.set(Vec::new());
                    *drag_state.borrow_mut() = None;
                    drag_pending.borrow_mut().take();
                    drag_frame.borrow_mut().take();
                    return false;
                }
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    let ctrl_flip = drag.rotate_mode;
                    let reverse_rotation = drag.right_click;
                    let (mut next, mut next_rotations) = if yew_wgpu_enabled {
                        (
                            positions_live.borrow().clone(),
                            rotations_live.borrow().clone(),
                        )
                    } else {
                        let snapshot = local_snapshot.borrow();
                        (snapshot.positions.clone(), snapshot.rotations.clone())
                    };
                    let (mut next_flips, mut next_connections) = {
                        let snapshot = local_snapshot.borrow();
                        (snapshot.flips.clone(), snapshot.connections.clone())
                    };
                    let start_positions_all = next.clone();
                    let start_rotations_all = next_rotations.clone();
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let snap_distance = piece_width.min(piece_height) * snap_distance_ratio_value;
                    let click_tolerance = piece_width.min(piece_height) * CLICK_MOVE_RATIO;
                    let start_group_animation = {
                        let apply_puzzle_state = apply_puzzle_state.clone();
                        let scramble_nonce = scramble_nonce.clone();
                        let local_snapshot = local_snapshot.clone();
                        let bump_ui_revision = bump_ui_revision.clone();
                        let group_anchor = group_anchor.clone();
                        let group_pos = group_pos.clone();
                        let group_rot = group_rot.clone();
                        let z_order = z_order.clone();
                        let solved = solved.clone();
                        let save_revision = save_revision.clone();
                        let rotation_anim = rotation_anim.clone();
                        let rotation_anim_handle = rotation_anim_handle.clone();
                        let animating_members = animating_members.clone();
                        let rotation_queue = rotation_queue.clone();
                        let rotation_enabled_value = rotation_enabled_value;
                        let rotation_noise_value = rotation_noise_value;
                        let animations_enabled_value = animations_enabled_value;
                        let rotation_snap_tolerance_value = rotation_snap_tolerance_value;
                        let cols = cols;
                        let rows = rows;
                        let piece_width = piece_width;
                        let piece_height = piece_height;
                        let frame_snap_ratio_value = frame_snap_ratio_value;
                        let snap_distance_ratio_value = snap_distance_ratio_value;
                        let center_min_x = center_min_x;
                        let center_max_x = center_max_x;
                        let center_min_y = center_min_y;
                        let center_max_y = center_max_y;
                        let view_min_x = view_min_x;
                        let view_min_y = view_min_y;
                        let view_width = view_width;
                        let view_height = view_height;
                        move |anim: RotationAnimation,
                              connections_override: Option<Vec<[bool; 4]>>| {
                            let connections_override = connections_override.map(Rc::new);
                            if !animations_enabled_value {
                                rotation_queue.borrow_mut().clear();
                                let (mut next_positions, mut next_rotations, mut connections_snapshot, flips_snapshot) =
                                    {
                                        let snapshot = local_snapshot.borrow();
                                        let next_positions = snapshot.positions.clone();
                                        let next_rotations = snapshot.rotations.clone();
                                        let connections_snapshot = connections_override
                                            .as_ref()
                                            .map(|snapshot| (**snapshot).clone())
                                            .unwrap_or_else(|| snapshot.connections.clone());
                                        let flips_snapshot = snapshot.flips.clone();
                                        (
                                            next_positions,
                                            next_rotations,
                                            connections_snapshot,
                                            flips_snapshot,
                                        )
                                    };
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) =
                                                anim.start_positions.get(index)
                                            {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    *delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + *delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        target_center,
                                        target_rot,
                                        ..
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let rot = normalize_angle(*target_rot);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                *target_center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                let should_snap = matches!(&anim.kind, AnimationKind::Pivot { .. });
                                if should_snap {
                                    let complete_snap = !*solved;
                                    let snap_distance = piece_width.min(piece_height)
                                        * snap_distance_ratio_value;
                                    apply_snaps_for_group(
                                        &anim.members,
                                        &mut next_positions,
                                        &mut next_rotations,
                                        &flips_snapshot,
                                        &mut connections_snapshot,
                                        cols,
                                        rows,
                                        piece_width,
                                        piece_height,
                                        snap_distance,
                                        frame_snap_ratio_value,
                                        complete_snap,
                                        center_min_x,
                                        center_max_x,
                                        center_min_y,
                                        center_max_y,
                                        view_min_x,
                                        view_min_y,
                                        view_width,
                                        view_height,
                                        rotation_snap_tolerance_value,
                                        rotation_enabled_value,
                                    );
                                }
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
                                    &next_positions,
                                    &next_rotations,
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
                                    rotation_enabled_value,
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
                                animating_members.set(Vec::new());
                                *rotation_anim.borrow_mut() = None;
                                rotation_anim_handle.borrow_mut().take();
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                return;
                            }
                            let members = anim.members.clone();
                            *rotation_anim.borrow_mut() = Some(anim);
                            animating_members.set(members);
                            rotation_anim_handle.borrow_mut().take();
                            let local_snapshot = local_snapshot.clone();
                            let bump_ui_revision = bump_ui_revision.clone();
                            let solved = solved.clone();
                            let rotation_anim = rotation_anim.clone();
                            let rotation_anim_handle = rotation_anim_handle.clone();
                            let animating_members = animating_members.clone();
                            let rotation_anim_handle_for_tick = rotation_anim_handle.clone();
                            let rotation_queue = rotation_queue.clone();
                            let connections_override = connections_override.clone();
                            let interval = Interval::new(16, move || {
                                let now = now_ms();
                                let anim = match rotation_anim.borrow().clone() {
                                    Some(value) => value,
                                    None => {
                                        rotation_anim_handle_for_tick.borrow_mut().take();
                                        animating_members.set(Vec::new());
                                        return;
                                    }
                                };
                                let mut t = (now - anim.start_time) / anim.duration;
                                if t < 0.0 {
                                    t = 0.0;
                                } else if t > 1.0 {
                                    t = 1.0;
                                }
                                let eased = t * t * (3.0 - 2.0 * t);
                                let (mut next_positions, mut next_rotations) = {
                                    let snapshot = local_snapshot.borrow();
                                    (snapshot.positions.clone(), snapshot.rotations.clone())
                                };
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        let current_delta = *delta * eased;
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) = anim.start_positions.get(index) {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    current_delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + current_delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let center = (
                                            start_center.0
                                                + (target_center.0 - start_center.0) * eased,
                                            start_center.1
                                                + (target_center.1 - start_center.1) * eased,
                                        );
                                        let delta = angle_delta(*target_rot, *start_rot);
                                        let rot = normalize_angle(*start_rot + delta * eased);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                log_state_update(
                                    "positions",
                                    next_positions.len(),
                                    "animation_step",
                                );
                                {
                                    let mut snapshot = local_snapshot.borrow_mut();
                                    snapshot.positions = next_positions.clone();
                                    snapshot.rotations = next_rotations.clone();
                                }
                                bump_ui_revision();
                                log_state_update(
                                    "rotations",
                                    next_rotations.len(),
                                    "animation_step",
                                );
                                let anchor_of_snapshot = (*group_anchor).clone();
                                if !anchor_of_snapshot.is_empty() {
                                    let (next_group_pos, next_group_rot) =
                                        group_transforms_from_anchor(
                                            &anchor_of_snapshot,
                                            &next_positions,
                                            &next_rotations,
                                        );
                                    group_pos.set(next_group_pos);
                                    group_rot.set(next_group_rot);
                                }
                                if t >= 1.0 {
                                    let queued = rotation_queue.borrow_mut().pop_front();
                                    if let Some(next_step) = queued {
                                        let members = next_step.members.clone();
                                        let mut start_positions =
                                            Vec::with_capacity(members.len());
                                        let mut start_rotations =
                                            Vec::with_capacity(members.len());
                                        for member in &members {
                                            if let Some(pos) =
                                                next_positions.get(*member)
                                            {
                                                start_positions.push(*pos);
                                            } else {
                                                start_positions.push((0.0, 0.0));
                                            }
                                            let rot = next_rotations
                                                .get(*member)
                                                .copied()
                                                .unwrap_or(0.0);
                                            start_rotations.push(rot);
                                        }
                                        let current_angle = members
                                            .first()
                                            .and_then(|id| next_rotations.get(*id))
                                            .copied()
                                            .unwrap_or(0.0);
                                        let delta = click_rotation_delta(
                                            current_angle,
                                            next_step.noise,
                                            rotation_noise_value,
                                            rotation_snap_tolerance_value,
                                        );
                                        let delta = if next_step.reverse { -delta } else { delta };
                                        *rotation_anim.borrow_mut() = Some(RotationAnimation {
                                            start_time: now_ms(),
                                            duration: SNAP_ANIMATION_MS,
                                            members: members.clone(),
                                            start_positions,
                                            start_rotations,
                                            kind: AnimationKind::Pivot {
                                                pivot_x: next_step.pivot_x,
                                                pivot_y: next_step.pivot_y,
                                                delta,
                                            },
                                        });
                                        animating_members.set(members);
                                        return;
                                    }
                                    *rotation_anim.borrow_mut() = None;
                                    animating_members.set(Vec::new());
                                    let should_snap =
                                        matches!(&anim.kind, AnimationKind::Pivot { .. });
                                    let mut snapped_positions = next_positions.clone();
                                    let mut snapped_rotations = next_rotations.clone();
                                    let (mut connections_snapshot, flips_snapshot) = {
                                        let snapshot = local_snapshot.borrow();
                                        let connections_snapshot = connections_override
                                            .as_ref()
                                            .map(|snapshot| (**snapshot).clone())
                                            .unwrap_or_else(|| snapshot.connections.clone());
                                        let flips_snapshot = snapshot.flips.clone();
                                        (connections_snapshot, flips_snapshot)
                                    };
                                    if should_snap {
                                        let complete_snap = !*solved;
                                        let snap_distance = piece_width.min(piece_height)
                                            * snap_distance_ratio_value;
                                        apply_snaps_for_group(
                                            &anim.members,
                                            &mut snapped_positions,
                                            &mut snapped_rotations,
                                            &flips_snapshot,
                                            &mut connections_snapshot,
                                            cols,
                                            rows,
                                            piece_width,
                                            piece_height,
                                            snap_distance,
                                            frame_snap_ratio_value,
                                            complete_snap,
                                            center_min_x,
                                            center_max_x,
                                            center_min_y,
                                            center_max_y,
                                            view_min_x,
                                            view_min_y,
                                            view_width,
                                            view_height,
                                            rotation_snap_tolerance_value,
                                            rotation_enabled_value,
                                        );
                                    }
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
                                        &snapped_positions,
                                        &snapped_rotations,
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
                                        rotation_enabled_value,
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
                                    save_revision.set(save_revision.wrapping_add(1));
                                    rotation_anim_handle_for_tick.borrow_mut().take();
                                }
                            });
                            *rotation_anim_handle.borrow_mut() = Some(interval);
                        }
                    };
                    if let Some((x, y)) = coords {
                        let dx = x - drag.start_x;
                        let dy = y - drag.start_y;
                        let dist = (dx * dx + dy * dy).sqrt();
                        let elapsed = now_ms() - drag.start_time;
                        let quick_click = elapsed <= CLICK_QUICK_TAP_MS;
                        if (dist <= click_tolerance && elapsed <= CLICK_MAX_DURATION_MS)
                            || quick_click
                        {
                            let click_id = drag.primary_id;
                            let was_flipped = next_flips.get(click_id).copied().unwrap_or(false);
                            if ctrl_flip {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = !*flip;
                                }
                                send_sync_action(SyncAction::Flip {
                                    piece_id: click_id,
                                    flipped: !was_flipped,
                                });
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
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
                                    &next,
                                    &next_rotations,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    order_opt,
                                );
                                let solved_now = is_solved(
                                    &derived_positions,
                                    &derived_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                let next_state = PuzzleState::rebuild_from_piece_state(
                                    &derived_positions,
                                    &derived_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    Some(piece_order.as_slice()),
                                    *scramble_nonce,
                                );
                                apply_puzzle_state(next_state, cols, piece_width, piece_height);
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                            if was_flipped {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = false;
                                }
                                send_sync_action(SyncAction::Flip {
                                    piece_id: click_id,
                                    flipped: false,
                                });
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
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
                                    &next,
                                    &next_rotations,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    order_opt,
                                );
                                let solved_now = is_solved(
                                    &derived_positions,
                                    &derived_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                let next_state = PuzzleState::rebuild_from_piece_state(
                                    &derived_positions,
                                    &derived_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    Some(piece_order.as_slice()),
                                    *scramble_nonce,
                                );
                                apply_puzzle_state(next_state, cols, piece_width, piece_height);
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                            if rotation_enabled_value && !drag.members.is_empty() {
                                let pivot_x = drag.start_x;
                                let pivot_y = drag.start_y;
                                let members = drag.members.clone();
                                let mut noise = 0.0;
                                if rotation_noise_value > 0.0 {
                                    let noise_seed = splitmix32(
                                        (drag.start_time as u32)
                                            ^ (drag.primary_id as u32)
                                                .wrapping_mul(0x9E37_79B9),
                                    );
                                    noise = rand_range(
                                        noise_seed,
                                        members.len() as u32,
                                        -rotation_noise_value,
                                        rotation_noise_value,
                                    );
                                }
                                if animations_enabled_value {
                                    if let Some(active_anim) = rotation_anim.borrow().clone() {
                                        if active_anim.members.contains(&click_id) {
                                            rotation_queue
                                                .borrow_mut()
                                                .push_back(QueuedRotation {
                                                    members: active_anim.members,
                                                    pivot_x,
                                                    pivot_y,
                                                    noise,
                                                    reverse: reverse_rotation,
                                                });
                                            dragging_members.set(Vec::new());
                                            active_id.set(None);
                                            *drag_state.borrow_mut() = None;
                                            drag_pending.borrow_mut().take();
                                            drag_frame.borrow_mut().take();
                                            return true;
                                        }
                                    }
                                }
                                let current_angle = members
                                    .first()
                                    .and_then(|id| next_rotations.get(*id))
                                    .copied()
                                    .unwrap_or(0.0);
                                let group_size = members.len();
                                let total = cols * rows;
                                let rotation_locked =
                                    group_size == total || group_size > rotation_lock_threshold_value;
                                if group_size > 1
                                    && rotation_locked
                                    && angle_matches(
                                        current_angle,
                                        0.0,
                                        rotation_snap_tolerance_value,
                                    )
                                {
                                    active_id.set(None);
                                    dragging_members.set(Vec::new());
                                    *drag_state.borrow_mut() = None;
                                    drag_pending.borrow_mut().take();
                                    drag_frame.borrow_mut().take();
                                    return true;
                                }
                                let delta = click_rotation_delta(
                                    current_angle,
                                    noise,
                                    rotation_noise_value,
                                    rotation_snap_tolerance_value,
                                );
                                let delta = if reverse_rotation { -delta } else { delta };
                                let mut start_positions = Vec::with_capacity(members.len());
                                let mut start_rotations = Vec::with_capacity(members.len());
                                for member in &members {
                                    if let Some(pos) = next.get(*member) {
                                        start_positions.push(*pos);
                                    } else {
                                        start_positions.push((0.0, 0.0));
                                    }
                                    let rot = next_rotations.get(*member).copied().unwrap_or(0.0);
                                    start_rotations.push(rot);
                                }
                                let anchor_id = if drag.anchor_id < next.len() {
                                    drag.anchor_id
                                } else {
                                    *members.first().unwrap_or(&drag.primary_id)
                                };
                                if anchor_id < next.len() && anchor_id < next_rotations.len() {
                                    let start_pos = next[anchor_id];
                                    let start_rot = next_rotations[anchor_id];
                                    let center = (
                                        start_pos.0 + piece_width * 0.5,
                                        start_pos.1 + piece_height * 0.5,
                                    );
                                    let (rx, ry) = rotate_point(
                                        center.0,
                                        center.1,
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    );
                                    let next_pos = (rx - piece_width * 0.5, ry - piece_height * 0.5);
                                    let next_rot = normalize_angle(start_rot + delta);
                                    send_sync_action(SyncAction::Place {
                                        anchor_id,
                                        pos: next_pos,
                                        rot_deg: next_rot,
                                    });
                                }
                                rotation_queue.borrow_mut().clear();
                                start_group_animation(
                                    RotationAnimation {
                                        start_time: now_ms(),
                                        duration: SNAP_ANIMATION_MS,
                                        members: members.clone(),
                                        start_positions,
                                        start_rotations,
                                        kind: AnimationKind::Pivot {
                                            pivot_x,
                                            pivot_y,
                                            delta,
                                        },
                                    },
                                    None,
                                );
                                dragging_members.set(Vec::new());
                                active_id.set(None);
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                        }
                    }
                    let complete_snap = !*solved;
                    let group_after = apply_snaps_for_group(
                        &drag.members,
                        &mut next,
                        &mut next_rotations,
                        &next_flips,
                        &mut next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        snap_distance,
                        frame_snap_ratio_value,
                        complete_snap,
                        center_min_x,
                        center_max_x,
                        center_min_y,
                        center_max_y,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                        rotation_snap_tolerance_value,
                        rotation_enabled_value,
                    );
                    if drag.anchor_id < next.len() && drag.anchor_id < next_rotations.len() {
                        let anchor_pos = next[drag.anchor_id];
                        let anchor_rot = next_rotations[drag.anchor_id];
                        send_sync_action(SyncAction::Place {
                            anchor_id: drag.anchor_id,
                            pos: anchor_pos,
                            rot_deg: anchor_rot,
                        });
                    }

                    let mut pending_animation = None;
                    if rotation_enabled_value && !group_after.is_empty() {
                        let anchor_id = if group_after.contains(&drag.primary_id) {
                            drag.primary_id
                        } else {
                            group_after[0]
                        };
                        if anchor_id < next.len() {
                            let start_rot = start_rotations_all
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(0.0);
                            let target_rot = next_rotations
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(start_rot);
                            if angle_delta(target_rot, start_rot).abs() > 0.01 {
                                let start_pos = start_positions_all
                                    .get(anchor_id)
                                    .copied()
                                    .unwrap_or(next[anchor_id]);
                                let target_pos = next[anchor_id];
                                let start_center = (
                                    start_pos.0 + piece_width * 0.5,
                                    start_pos.1 + piece_height * 0.5,
                                );
                                let target_center = (
                                    target_pos.0 + piece_width * 0.5,
                                    target_pos.1 + piece_height * 0.5,
                                );
                                let mut member_positions = Vec::with_capacity(group_after.len());
                                let mut member_rotations = Vec::with_capacity(group_after.len());
                                for member in &group_after {
                                    if let Some(pos) = start_positions_all.get(*member) {
                                        member_positions.push(*pos);
                                    } else {
                                        member_positions.push((0.0, 0.0));
                                    }
                                    let rot = start_rotations_all
                                        .get(*member)
                                        .copied()
                                        .unwrap_or(0.0);
                                    member_rotations.push(rot);
                                }
                                pending_animation = Some(RotationAnimation {
                                    start_time: now_ms(),
                                    duration: SNAP_ANIMATION_MS,
                                    members: group_after.clone(),
                                    start_positions: member_positions,
                                    start_rotations: member_rotations,
                                    kind: AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    },
                                });
                            }
                        }
                    }
                    let order_snapshot = (*z_order).clone();
                    let order_opt = if order_snapshot.len() == cols * rows {
                        Some(order_snapshot.as_slice())
                    } else {
                        None
                    };
                    let (
                        anchor_of,
                        _group_positions,
                        _group_rotations,
                        group_order_value,
                        derived_positions,
                        derived_rotations,
                        piece_order,
                    ) = rebuild_group_state(
                        &next,
                        &next_rotations,
                        &next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        order_opt,
                    );
                    let next_state = PuzzleState::rebuild_from_piece_state(
                        &derived_positions,
                        &derived_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        Some(piece_order.as_slice()),
                        *scramble_nonce,
                    );
                    if let Some(anim) = pending_animation {
                        let connections_snapshot = next_state.connections.clone();
                        let group_order_snapshot: Vec<u32> = next_state
                            .group_order
                            .iter()
                            .filter_map(|id| u32::try_from(*id).ok())
                            .collect();
                        let scramble_nonce_snapshot = next_state.scramble_nonce;
                        let (group_pos_now, group_rot_now) = group_transforms_from_anchor(
                            &anchor_of,
                            &start_positions_all,
                            &start_rotations_all,
                        );
                        puzzle_state.set(next_state);
                        group_anchor.set(anchor_of);
                        group_pos.set(group_pos_now);
                        group_rot.set(group_rot_now);
                        group_order.set(group_order_value);
                        let piece_order_snapshot = piece_order.clone();
                        z_order.set(piece_order);
                        log_state_update(
                            "connections",
                            connections_snapshot.len(),
                            "drag_finalize",
                        );
                        log_state_update("flips", next_flips.len(), "drag_finalize");
                        {
                            let mut snapshot = local_snapshot.borrow_mut();
                            snapshot.connections = connections_snapshot.clone();
                            snapshot.flips = next_flips.clone();
                            snapshot.group_order = group_order_snapshot;
                            snapshot.z_order = piece_order_snapshot;
                            snapshot.scramble_nonce = scramble_nonce_snapshot;
                        }
                        bump_ui_revision();
                        start_group_animation(anim, Some(connections_snapshot));
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                        drag_pending.borrow_mut().take();
                        drag_frame.borrow_mut().take();
                        return true;
                    }

                    let solved_now = is_solved(
                        &derived_positions,
                        &derived_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        rotation_enabled_value,
                    );
                    apply_puzzle_state(next_state, cols, piece_width, piece_height);
                    solved.set(solved_now);
                    save_revision.set(save_revision.wrapping_add(1));
                    active_id.set(None);
                    dragging_members.set(Vec::new());
                    *drag_state.borrow_mut() = None;
                    drag_pending.borrow_mut().take();
                    drag_frame.borrow_mut().take();
                    return true;
                }
                false
            })
        };
        let drag_release = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let drag_release_common = drag_release_common.clone();
            let dispatch_view_action = dispatch_view_action.clone();
            let puzzle_scale = puzzle_scale;
            move |event: &MouseEvent| {
                dispatch_view_action(CoreAction::DragEnd { touch_id: None });
                let target_ref = if yew_wgpu_enabled {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                let coords = workspace_to_puzzle_opt(
                    puzzle_scale,
                    event_to_svg_coords(
                        event,
                        target_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ),
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_release_touch = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let drag_state = drag_state.clone();
            let drag_release_common = drag_release_common.clone();
            let dispatch_view_action = dispatch_view_action.clone();
            let puzzle_scale = puzzle_scale;
            move |event: &TouchEvent| {
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                dispatch_view_action(CoreAction::DragEnd { touch_id });
                let target_ref = if yew_wgpu_enabled {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                let coords = workspace_to_puzzle_opt(
                    puzzle_scale,
                    touch_event_to_svg_coords(
                        event,
                        target_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                        touch_id,
                        true,
                    ),
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_move = Rc::new(drag_move);
        let drag_move_touch = Rc::new(drag_move_touch);
        let drag_release = Rc::new(drag_release);
        let drag_release_touch = Rc::new(drag_release_touch);
        {
            let mut handlers = drag_handlers.borrow_mut();
            handlers.on_move = Some(drag_move.clone());
            handlers.on_release = Some(drag_release.clone());
            handlers.on_touch_move = Some(drag_move_touch.clone());
            handlers.on_touch_release = Some(drag_release_touch.clone());
        }

        let on_scramble = {
            let apply_puzzle_state = apply_puzzle_state.clone();
            let scramble_nonce = scramble_nonce.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
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
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(false);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_solve = {
            let apply_puzzle_state = apply_puzzle_state.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
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
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(true);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_solve_rotation = {
            let local_snapshot = local_snapshot.clone();
            let z_order = z_order.clone();
            let apply_puzzle_state = apply_puzzle_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
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
                    let snapshot = local_snapshot.borrow();
                    (
                        snapshot.positions.clone(),
                        snapshot.flips.clone(),
                        snapshot.connections.clone(),
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
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_unflip = {
            let local_snapshot = local_snapshot.clone();
            let z_order = z_order.clone();
            let apply_puzzle_state = apply_puzzle_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
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
                    let snapshot = local_snapshot.borrow();
                    (
                        snapshot.positions.clone(),
                        snapshot.rotations.clone(),
                        snapshot.connections.clone(),
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
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
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
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                save_revision.set(save_revision.wrapping_add(1));
            })
        };

        let color_pattern_bg = "#8f5b32";
        let color_pattern_fg1 = "#734423";
        let color_pattern_fg2 = "#3a2418";
        let back_pattern = html! {
            <pattern
                id="piece-back-pattern"
                patternUnits="userSpaceOnUse"
                width="28"
                height="28"
            >
                <rect width="28" height="28" fill={color_pattern_bg} />
                <circle cx="7" cy="7" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="21" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="7" r="1.8" fill={color_pattern_fg2} />
                <circle cx="7" cy="21" r="1.8" fill={color_pattern_fg2} />
            </pattern>
        };

        let emboss_opacity = format!("{}", fmt_f32(EMBOSS_OPACITY));
        let emboss_filter_body = html! {
            <>
                <@{"feComponentTransfer"} in="SourceAlpha" result="a">
                    <@{"feFuncA"} type="linear" slope="1" />
                </@>
                <@{"feOffset"} in="a" dx={emboss_offset_neg.clone()} dy={emboss_offset_neg.clone()} result="aOff" />
                <@{"feFlood"} flood-color="#000" result="black" />
                <@{"feComposite"} in="black" in2="a" operator="in" result="blackShape" />
                <@{"feFlood"} flood-color="#fff" flood-opacity="0.6" result="white" />
                <@{"feComposite"} in="white" in2="aOff" operator="in" result="whiteShape" />
                <@{"feMorphology"} in="whiteShape" operator="erode" radius="0.6" result="whiteThin"/>
                <@{"feGaussianBlur"} in="whiteThin" stdDeviation="0.5" result="whiteShapeBlur"/>
                <@{"feComposite"} in="whiteShapeBlur" in2="blackShape" operator="over" result="overlayFull"/>
                <@{"feMorphology"} in="a" operator="erode" radius={emboss_rim_radius.clone()} result="aInner" />
                <@{"feComposite"} in="a" in2="aInner" operator="arithmetic" k1="0" k2="1" k3="-1" k4="0" result="rim" />
                <@{"feComposite"} in="overlayFull" in2="rim" operator="in" result="overlayRim" />
                <@{"feComponentTransfer"} in="overlayRim" result="overlayRimOpacity">
                  <@{"feFuncA"} type="linear" slope={emboss_opacity}/>
                </@>
                <@{"feMerge"}>
                    <@{"feMergeNode"} in="SourceGraphic" />
                    <@{"feMergeNode"} in="overlayRimOpacity" />
                </@>
            </>
        };
        let emboss_filter = if fast_filter_value {
            let filter_res = format!(
                "{} {}",
                fmt_f32(piece_width * 0.2),
                fmt_f32(piece_height * 0.2)
            );
            html! {
                <filter
                    id="emboss"
                    x={emboss_x.clone()}
                    y={emboss_y.clone()}
                    width={emboss_width.clone()}
                    height={emboss_height.clone()}
                    filterUnits="userSpaceOnUse"
                    primitiveUnits="userSpaceOnUse"
                    color-interpolation-filters="linearRGB"
                    filterRes={filter_res}
                >
                    {emboss_filter_body}
                </filter>
            }
        } else {
            html! {
                <filter
                    id="emboss"
                    x={emboss_x.clone()}
                    y={emboss_y.clone()}
                    width={emboss_width.clone()}
                    height={emboss_height.clone()}
                    filterUnits="userSpaceOnUse"
                    primitiveUnits="userSpaceOnUse"
                    color-interpolation-filters="linearRGB"
                >
                    {emboss_filter_body}
                </filter>
            }
        };

        let mask_defs: Html = piece_shapes
            .iter()
            .map(|(piece, paths)| {
                let mask_id = format!("piece-mask-{}", piece.id);
                html! {
                    <mask
                        id={mask_id}
                        maskUnits="userSpaceOnUse"
                        maskContentUnits="userSpaceOnUse"
                        x={mask_x.clone()}
                        y={mask_y.clone()}
                        width={mask_width.clone()}
                        height={mask_height.clone()}
                        mask-type="luminance"
                    >
                        <rect
                            x={mask_x.clone()}
                            y={mask_y.clone()}
                            width={mask_width.clone()}
                            height={mask_height.clone()}
                            fill="black"
                        />
                        <path d={paths.outline.clone()} fill="white" />
                    </mask>
                }
            })
            .collect();

        let begin_drag: Rc<dyn Fn(usize, f32, f32, bool, bool, bool, Option<i32>)> = {
            let local_snapshot = local_snapshot.clone();
            let positions_live = positions_live.clone();
            let rotations_live = rotations_live.clone();
            let group_pos_live = group_pos_live.clone();
            let group_rot_live = group_rot_live.clone();
            let apply_puzzle_state = apply_puzzle_state.clone();
            let scramble_nonce = scramble_nonce.clone();
            let drag_state = drag_state.clone();
            let active_id = active_id.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let z_order = z_order.clone();
            let dispatch_view_action = dispatch_view_action.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let allow_drag = allow_drag;
            Rc::new(move |piece_id, x, y, shift_key, rotate_mode, right_click, touch_id| {
                if !allow_drag {
                    return;
                }
                let action = CoreAction::BeginDrag {
                    piece_id,
                    x,
                    y,
                    shift_key,
                    rotate_mode,
                    right_click,
                    touch_id,
                };
                dispatch_view_action(action);
                let (positions_snapshot, rotations_snapshot, flips_snapshot, mut connections_snapshot) = {
                    let snapshot = local_snapshot.borrow();
                    (
                        snapshot.positions.clone(),
                        snapshot.rotations.clone(),
                        snapshot.flips.clone(),
                        snapshot.connections.clone(),
                    )
                };
                let mut members = if shift_key {
                    clear_piece_connections(&mut connections_snapshot, piece_id, cols, rows);
                    vec![piece_id]
                } else {
                    collect_group(&connections_snapshot, piece_id, cols, rows)
                };
                if members.is_empty() {
                    members.push(piece_id);
                }
                members.sort_unstable();
                let order_snapshot = (*z_order).clone();
                let order_opt = if order_snapshot.len() == cols * rows {
                    Some(order_snapshot.as_slice())
                } else {
                    None
                };
                let (
                    anchor_of,
                    group_positions,
                    group_rotations,
                    mut group_order_value,
                    derived_positions,
                    derived_rotations,
                    _piece_order,
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
                if yew_wgpu_enabled {
                    *positions_live.borrow_mut() = derived_positions.clone();
                    *rotations_live.borrow_mut() = derived_rotations.clone();
                    *group_pos_live.borrow_mut() = group_positions.clone();
                    *group_rot_live.borrow_mut() = group_rotations.clone();
                }
                let anchor_id = members.first().copied().unwrap_or(piece_id);
                group_order_value.retain(|id| *id != anchor_id);
                group_order_value.push(anchor_id);
                let piece_order = crate::core::build_piece_order_from_groups(&group_order_value, &anchor_of);
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
                let anchor_pos = group_positions
                    .get(anchor_id)
                    .copied()
                    .or_else(|| derived_positions.get(anchor_id).copied())
                    .unwrap_or((
                        (anchor_id % cols) as f32 * piece_width,
                        (anchor_id / cols) as f32 * piece_height,
                    ));
                let anchor_rot = group_rotations
                    .get(anchor_id)
                    .copied()
                    .or_else(|| derived_rotations.get(anchor_id).copied())
                    .unwrap_or(0.0);
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                animating_members.set(Vec::new());
                dragging_members.set(members.clone());
                let base_col = piece_id % cols;
                let base_row = piece_id / cols;
                let base_pos = (
                    base_col as f32 * piece_width,
                    base_row as f32 * piece_height,
                );
                let pos = derived_positions.get(piece_id).copied().unwrap_or(base_pos);
                let pivot_x = pos.0 + piece_width * 0.5;
                let pivot_y = pos.1 + piece_height * 0.5;
                let start_angle = (y - pivot_y).atan2(x - pivot_x);
                let mut start_positions = Vec::with_capacity(members.len());
                for id in &members {
                    if let Some(start) = derived_positions.get(*id) {
                        start_positions.push(*start);
                    } else {
                        start_positions.push(pos);
                    }
                }
                *drag_state.borrow_mut() = Some(DragState {
                    start_x: x,
                    start_y: y,
                    start_time: now_ms(),
                    primary_id: piece_id,
                    anchor_id,
                    anchor_pos,
                    anchor_rot,
                    touch_id,
                    rotate_mode,
                    right_click,
                    cursor_x: x,
                    cursor_y: y,
                    pivot_x,
                    pivot_y,
                    start_angle,
                    members,
                    start_positions,
                });
                active_id.set(Some(piece_id));
            })
        };

        let mut nodes = Vec::with_capacity(piece_shapes.len());
        for (piece, paths) in piece_shapes.iter() {
            let piece_id = piece.id;
            let piece_x = piece.col as f32 * piece_width;
            let piece_y = piece.row as f32 * piece_height;
            let current =
                positions_value.get(piece.id).copied().unwrap_or((piece_x, piece_y));
            let rotation = rotations_value.get(piece.id).copied().unwrap_or(0.0);
            let flipped = flips_value.get(piece.id).copied().unwrap_or(false);
            let center_x = piece_width * 0.5;
            let center_y = piece_height * 0.5;
            let is_dragging = dragging_mask.get(piece.id).copied().unwrap_or(false);
            let render_pos = if is_dragging {
                if let Some(center) = drag_center {
                    drag_group_position(
                        current,
                        center,
                        drag_scale,
                        drag_rotation,
                        piece_width,
                        piece_height,
                    )
                } else {
                    current
                }
            } else {
                current
            };
            let flip_transform = if flipped {
                format!(
                    " translate({} {}) scale(-1 1) translate(-{} -{})",
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(center_x),
                    fmt_f32(center_y)
                )
            } else {
                String::new()
            };
            let drag_transform = if is_dragging {
                format!(
                    " translate({} {}) scale({}) translate(-{} -{}) rotate({} {} {})",
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(drag_scale),
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(drag_rotation),
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                )
            } else {
                String::new()
            };
            let outer_transform = format!(
                "translate({} {})",
                fmt_f32(render_pos.0),
                fmt_f32(render_pos.1)
            );
            let inner_transform = format!(
                "{} rotate({} {} {}){}",
                flip_transform,
                fmt_f32(rotation),
                fmt_f32(center_x),
                fmt_f32(center_y),
                drag_transform
            );
            let mask_ref = format!("url(#piece-mask-{})", piece.id);
            let img_x = fmt_f32(-piece_x);
            let img_y = fmt_f32(-piece_y);
            let is_animating = animating_mask.get(piece.id).copied().unwrap_or(false);
            let is_hovered = hovered_mask.get(piece.id).copied().unwrap_or(false);
            let is_owned_other = owned_mask.get(piece.id).copied().unwrap_or(false);
            let mut class = if is_dragging {
                if flipped {
                    "piece dragging flipped".to_string()
                } else {
                    "piece dragging".to_string()
                }
            } else if is_animating {
                if flipped {
                    "piece animating flipped".to_string()
                } else {
                    "piece animating".to_string()
                }
            } else if flipped {
                "piece flipped".to_string()
            } else {
                "piece".to_string()
            };
            if is_hovered {
                class.push_str(" hovered");
            }
            if is_owned_other {
                class.push_str(" owned-other");
            }
            let connection = connections_value
                .get(piece.id)
                .copied()
                .unwrap_or([false; 4]);
            let mut external_path = String::new();
            for (dir, edge_path) in [
                (DIR_UP, &paths.edges[DIR_UP]),
                (DIR_RIGHT, &paths.edges[DIR_RIGHT]),
                (DIR_DOWN, &paths.edges[DIR_DOWN]),
                (DIR_LEFT, &paths.edges[DIR_LEFT]),
            ] {
                let connected = connection.get(dir).copied().unwrap_or(false);
                if !connected {
                    if !external_path.is_empty() {
                        external_path.push(' ');
                    }
                    external_path.push_str(edge_path);
                }
            }
            let (external_outline, internal_outline) = if external_path.is_empty() {
                (html! {}, html! {})
            } else if flipped {
                (
                    html! {},
                    html! {
                        <g class="piece-outline-group edge-internal">
                            <path
                                class="piece-outline edge-internal"
                                d={external_path}
                                mask={mask_ref.clone()}
                            />
                        </g>
                    },
                )
            } else {
                (
                    html! {
                        <g class="piece-outline-group edge-external">
                            <path class="piece-outline edge-external" d={external_path} />
                        </g>
                    },
                    html! {},
                )
            };
            let simple_outline = if emboss_enabled_value {
                html! {}
            } else {
                html! {
                    <path class="piece-outline piece-outline-simple" d={paths.outline.clone()} />
                }
            };
            let on_piece_down = {
                let svg_ref = svg_ref.clone();
                let begin_drag = begin_drag.clone();
                let puzzle_scale = puzzle_scale;
                Callback::from(move |event: MouseEvent| {
                    let right_click = event.button() == 2;
                    if let Some((x, y)) = event_to_svg_coords(
                        &event,
                        &svg_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ) {
                        let (x, y) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                        begin_drag(
                            piece_id,
                            x,
                            y,
                            event.shift_key(),
                            event.ctrl_key(),
                            right_click,
                            None,
                        );
                    }
                    event.prevent_default();
                })
            };
            let on_piece_touch = {
                let svg_ref = svg_ref.clone();
                let begin_drag = begin_drag.clone();
                let puzzle_scale = puzzle_scale;
                Callback::from(move |event: TouchEvent| {
                    if event.touches().length() > 1 {
                        return;
                    }
                    if let Some(touch) = touch_from_event(&event, None, true) {
                        let touch_id = Some(touch.identifier());
                        if let Some((x, y)) = touch_event_to_svg_coords(
                            &event,
                            &svg_ref,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            touch_id,
                            true,
                        ) {
                            let (x, y) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                            begin_drag(piece_id, x, y, false, false, false, touch_id);
                        }
                    }
                })
            };
            let on_piece_enter = {
                let set_hovered = set_hovered.clone();
                let piece_id = piece_id;
                Callback::from(move |_| {
                    set_hovered(Some(piece_id));
                })
            };
            let on_piece_leave = {
                let set_hovered = set_hovered.clone();
                Callback::from(move |_| {
                    set_hovered(None);
                })
            };
            let debug_overlay = if show_debug_value {
                let label = format!(
                    "#{}\nx:{}\ny:{}\nr:{}",
                    piece.id,
                    fmt_f32(current.0),
                    fmt_f32(current.1),
                    fmt_f32(rotation)
                );
                html! {
                    <>
                        <circle
                            class="piece-debug-center"
                            cx={fmt_f32(center_x)}
                            cy={fmt_f32(center_y)}
                            r="3"
                        />
                        <text
                            class="piece-debug-label"
                            x={fmt_f32(center_x)}
                            y={fmt_f32(center_y - 12.0)}
                        >
                            {label
                                .lines()
                                .enumerate()
                                .map(|(idx, line)| {
                                    html! {
                                        <tspan
                                            x={fmt_f32(center_x)}
                                            dy={if idx == 0 { "-20" } else { "20" }}
                                        >
                                            {line}
                                        </tspan>
                                    }
                                })
                                .collect::<Html>()}
                        </text>
                    </>
                }
            } else {
                html! {}
            };
            let emboss_target = if emboss_enabled_value && !flipped {
                "url(#emboss)"
            } else {
                "none"
            };
            let node = html! {
                <g
                    key={piece.id.to_string()}
                    class={class}
                    transform={outer_transform}
                    onmousedown={on_piece_down}
                    ontouchstart={on_piece_touch}
                    onmouseenter={on_piece_enter}
                    onmouseleave={on_piece_leave}
                >
                    <g transform={inner_transform.clone()}>
                        {external_outline}
                        <path class="piece-hitbox" d={paths.outline.clone()} />
                    </g>
                    <g class="piece-surface" filter={emboss_target}>
                        <g transform={inner_transform.clone()}>
                            <rect
                                class="piece-back"
                                x={img_x.clone()}
                                y={img_y.clone()}
                                width={width.to_string()}
                                height={height.to_string()}
                                fill="url(#piece-back-pattern)"
                                mask={mask_ref.clone()}
                            />
                            <image
                                class="piece-image"
                                href={render_puzzle_src.clone()}
                                x={img_x}
                                y={img_y}
                                width={width.to_string()}
                                height={height.to_string()}
                                preserveAspectRatio="xMidYMid meet"
                                mask={mask_ref}
                            />
                        </g>
                    </g>
                    <g transform={inner_transform}>
                        {internal_outline}
                        {simple_outline}
                        {debug_overlay}
                    </g>
                </g>
            };
            nodes.push(node);
        }
        let piece_nodes: Html = if menu_visible_value {
            html! {}
        } else if z_order_value.len() == nodes.len() {
            z_order_value
                .iter()
                .filter_map(|id| nodes.get(*id))
                .cloned()
                .collect()
        } else {
            nodes.into_iter().collect()
        };
        let on_canvas_down = {
            let canvas_ref = canvas_ref.clone();
            let ui_credit_hitbox = ui_credit_hitbox.clone();
            let ui_credit_hovered = ui_credit_hovered.clone();
            let local_snapshot = local_snapshot.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let begin_drag = begin_drag.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            let puzzle_scale = puzzle_scale;
            Callback::from(move |event: MouseEvent| {
                let right_click = event.button() == 2;
                let Some((x, y)) = event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) else {
                    return;
                };
                let (px, py) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                let credit_hit = ui_credit_hitbox
                    .borrow()
                    .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
                    .unwrap_or(false);
                let mask_atlas_ref = mask_atlas.borrow();
                let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                    if credit_hit {
                        open_credit_url();
                        event.prevent_default();
                    }
                    return;
                };
                let mut order = (*z_order).clone();
                let total = cols * rows;
                if order.len() != total {
                    order = (0..total).collect();
                }
                let piece_hit = {
                    let snapshot = local_snapshot.borrow();
                    pick_piece_at(
                        px,
                        py,
                        snapshot.positions.as_slice(),
                        snapshot.rotations.as_slice(),
                        snapshot.flips.as_slice(),
                        &order,
                        mask_atlas,
                        cols,
                        piece_width,
                        piece_height,
                        mask_pad,
                    )
                };
                if let Some(piece_id) = piece_hit {
                    if *ui_credit_hovered {
                        ui_credit_hovered.set(false);
                    }
                    begin_drag(
                        piece_id,
                        px,
                        py,
                        event.shift_key(),
                        event.ctrl_key(),
                        right_click,
                        None,
                    );
                    event.prevent_default();
                    return;
                }
                if credit_hit {
                    open_credit_url();
                    event.prevent_default();
                    return;
                }
                event.prevent_default();
            })
        };
        let on_canvas_touch = {
            let canvas_ref = canvas_ref.clone();
            let ui_credit_hitbox = ui_credit_hitbox.clone();
            let ui_credit_hovered = ui_credit_hovered.clone();
            let local_snapshot = local_snapshot.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let begin_drag = begin_drag.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            let puzzle_scale = puzzle_scale;
            Callback::from(move |event: TouchEvent| {
                if event.touches().length() > 1 {
                    return;
                }
                let Some(touch) = touch_from_event(&event, None, true) else {
                    return;
                };
                let touch_id = Some(touch.identifier());
                let Some((x, y)) = touch_event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    true,
                ) else {
                    return;
                };
                let (px, py) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                let credit_hit = ui_credit_hitbox
                    .borrow()
                    .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
                    .unwrap_or(false);
                let mask_atlas_ref = mask_atlas.borrow();
                let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                    if credit_hit {
                        open_credit_url();
                        event.prevent_default();
                    }
                    return;
                };
                let mut order = (*z_order).clone();
                let total = cols * rows;
                if order.len() != total {
                    order = (0..total).collect();
                }
                let piece_hit = {
                    let snapshot = local_snapshot.borrow();
                    pick_piece_at(
                        px,
                        py,
                        snapshot.positions.as_slice(),
                        snapshot.rotations.as_slice(),
                        snapshot.flips.as_slice(),
                        &order,
                        mask_atlas,
                        cols,
                        piece_width,
                        piece_height,
                        mask_pad,
                    )
                };
                if let Some(piece_id) = piece_hit {
                    if *ui_credit_hovered {
                        ui_credit_hovered.set(false);
                    }
                    begin_drag(piece_id, px, py, false, false, false, touch_id);
                    event.prevent_default();
                    return;
                }
                if credit_hit {
                    open_credit_url();
                    event.prevent_default();
                    return;
                }
                event.prevent_default();
            })
        };
        let on_canvas_move = {
            let canvas_ref = canvas_ref.clone();
            let local_snapshot = local_snapshot.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let set_hovered = set_hovered.clone();
            let ui_credit_hitbox = ui_credit_hitbox.clone();
            let ui_credit_hovered = ui_credit_hovered.clone();
            let active_id = active_id.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            let yew_wgpu_enabled = yew_wgpu_enabled;
            let puzzle_scale = puzzle_scale;
            Callback::from(move |event: MouseEvent| {
                if yew_wgpu_enabled && active_id.is_some() {
                    return;
                }
                let Some((x, y)) = event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) else {
                    set_hovered(None);
                    if *ui_credit_hovered {
                        ui_credit_hovered.set(false);
                    }
                    return;
                };
                let (px, py) = workspace_to_puzzle_coords(puzzle_scale, x, y);
                let mut piece_hit = None;
                if let Some(mask_atlas) = mask_atlas.borrow().as_ref() {
                    let snapshot = local_snapshot.borrow();
                    let positions_snapshot = snapshot.positions.as_slice();
                    let rotations_snapshot = snapshot.rotations.as_slice();
                    let flips_snapshot = snapshot.flips.as_slice();
                    let order_snapshot = &*z_order;
                    let total = cols * rows;
                    let fallback_order = if order_snapshot.len() == total {
                        None
                    } else {
                        Some((0..total).collect::<Vec<_>>())
                    };
                    let order = match fallback_order.as_deref() {
                        Some(slice) => slice,
                        None => order_snapshot.as_slice(),
                    };
                    piece_hit = pick_piece_at(
                        px,
                        py,
                        positions_snapshot,
                        rotations_snapshot,
                        flips_snapshot,
                        order,
                        mask_atlas,
                        cols,
                        piece_width,
                        piece_height,
                        mask_pad,
                    );
                }
                if piece_hit.is_some() {
                    if *ui_credit_hovered {
                        ui_credit_hovered.set(false);
                    }
                    set_hovered(piece_hit);
                    return;
                }
                let credit_hit = ui_credit_hitbox
                    .borrow()
                    .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
                    .unwrap_or(false);
                if *ui_credit_hovered != credit_hit {
                    ui_credit_hovered.set(credit_hit);
                }
                if credit_hit {
                    set_hovered(None);
                    return;
                }
                set_hovered(None);
            })
        };
        let on_canvas_leave = {
            let set_hovered = set_hovered.clone();
            let ui_credit_hovered = ui_credit_hovered.clone();
            Callback::from(move |_: MouseEvent| {
                set_hovered(None);
                if *ui_credit_hovered {
                    ui_credit_hovered.set(false);
                }
            })
        };
        let on_context_menu = Callback::from(|event: MouseEvent| {
            event.prevent_default();
        });
        let bounds_inset = 1.0;
        let puzzle_bounds = if menu_visible_value {
            html! {}
        } else {
            html! {
                <rect
                    class="puzzle-bounds"
                    x={fmt_f32(bounds_inset)}
                    y={fmt_f32(bounds_inset)}
                    width={fmt_f32(width_f - 2.0 * bounds_inset)}
                    height={fmt_f32(height_f - 2.0 * bounds_inset)}
                    rx={frame_corner_radius.clone()}
                    ry={frame_corner_radius.clone()}
                />
            }
        };
        let puzzle_transform = format!("scale({})", fmt_f32(puzzle_scale));
        let puzzle_group = html! {
            <g transform={puzzle_transform}>
                {puzzle_bounds}
                {piece_nodes}
            </g>
        };
        let workspace_bounds = html! {
            <rect
                class="workspace-bounds"
                x={fmt_f32(view_min_x)}
                y={fmt_f32(view_min_y)}
                width={fmt_f32(view_width)}
                height={fmt_f32(view_height)}
            />
        };

        let mut svg_class = if show_debug_value {
            "puzzle-image debug".to_string()
        } else {
            "puzzle-image".to_string()
        };
        if !animations_enabled_value {
            svg_class.push_str(" no-anim");
        }
        if fast_render_value {
            svg_class.push_str(" fast-render");
        }
        if solved_value {
            svg_class.push_str(" solved");
        }
        let mut canvas_class = "puzzle-canvas".to_string();
        if active_id_value.is_some() {
            canvas_class.push_str(" dragging");
        } else if hovered_id_value.is_some() {
            canvas_class.push_str(" hover");
        }
        if ui_credit_hovered_value {
            canvas_class.push_str(" ui-link-hover");
        }
        let canvas_node = if yew_wgpu_enabled && board_ready_value {
            let noop_mouse = Callback::from(|_: MouseEvent| {});
            let noop_touch = Callback::from(|_: TouchEvent| {});
            let (canvas_on_down, canvas_on_move, canvas_on_leave, canvas_on_touch) =
                if menu_visible_value {
                    (
                        noop_mouse.clone(),
                        noop_mouse.clone(),
                        noop_mouse.clone(),
                        noop_touch.clone(),
                    )
                } else {
                    (
                        on_canvas_down.clone(),
                        on_canvas_move.clone(),
                        on_canvas_leave.clone(),
                        on_canvas_touch.clone(),
                    )
                };
            html! {
                <canvas
                    class={canvas_class}
                    ref={canvas_ref}
                    width={view_width.round().to_string()}
                    height={view_height.round().to_string()}
                    onmousedown={canvas_on_down}
                    onmousemove={canvas_on_move}
                    onmouseleave={canvas_on_leave}
                    ontouchstart={canvas_on_touch}
                    oncontextmenu={on_context_menu.clone()}
                />
            }
        } else {
            html! {}
        };
        let ui_nodes: Html = if svg_enabled {
            ui_specs
                .iter()
                .map(|spec| {
                    let x = fmt_f32(spec.pos[0]);
                    let y = fmt_f32(spec.pos[1]);
                    let rotation = fmt_f32(spec.rotation_deg);
                    let pivot_x = fmt_f32(spec.pos[0] + spec.rotation_offset[0]);
                    let pivot_y = fmt_f32(spec.pos[1] + spec.rotation_offset[1]);
                    let transform = format!("rotate({} {} {})", rotation, pivot_x, pivot_y);
                    let style = format!(
                        "font-size: {}px; fill: {};",
                        fmt_f32(spec.font_size * 0.925),
                        ui_color_to_css(spec.color)
                    );
                    let class = match spec.id {
                        UiTextId::Title => "ui-text ui-title",
                        UiTextId::Progress => "ui-text ui-progress",
                        UiTextId::Credit => "ui-text ui-credit",
                        UiTextId::Success => "ui-text ui-success",
                        UiTextId::MenuTitle => "ui-text ui-menu-title",
                        UiTextId::MenuSubtitle => "ui-text ui-menu-sub",
                    };
                    let lines: Vec<&str> = spec.text.lines().collect();
                    let line_count = lines.len().max(1) as f32;
                    let total_height = spec.line_height * (line_count - 1.0).max(0.0);
                    let start_dy = -total_height * 0.5;
                    let line_nodes: Html = lines
                        .iter()
                        .enumerate()
                        .map(|(idx, line)| {
                            let dy = if idx == 0 {
                                start_dy
                            } else {
                                spec.line_height
                            };
                            html! {
                                <tspan x={x.clone()} dy={fmt_f32(dy)}>
                                    {(*line).to_string()}
                                </tspan>
                            }
                        })
                        .collect();
                    let text_node = html! {
                        <text
                            class={class}
                            x={x.clone()}
                            y={y.clone()}
                            transform={transform}
                            style={style}
                        >
                            {line_nodes}
                        </text>
                    };
                    if matches!(spec.id, UiTextId::Credit) {
                        html! {
                            <a
                                class="ui-link"
                                href={CREDIT_URL}
                                target="_blank"
                                rel="noopener noreferrer"
                            >
                                {text_node}
                            </a>
                        }
                    } else {
                        text_node
                    }
                })
                .collect()
        } else {
            html! {}
        };

        let svg_node = if svg_enabled {
            html! {
                <svg
                    xmlns="http://www.w3.org/2000/svg"
                    class={svg_class}
                    viewBox={view_box}
                    width={fmt_f32(view_width)}
                    height={fmt_f32(view_height)}
                    preserveAspectRatio="xMidYMid meet"
                    ref={svg_ref}
                    oncontextmenu={on_context_menu}
                >
                    <defs>
                        {back_pattern}
                        {emboss_filter}
                        {mask_defs}
                    </defs>
                    {workspace_bounds}
                    {ui_nodes}
                    {puzzle_group}
                </svg>
            }
        } else {
            html! {}
        };
        (
            html! {
                <>
                    {canvas_node}
                    {svg_node}
                </>
            },
            on_scramble,
            on_solve,
            on_solve_rotation,
            on_unflip,
            false,
        )
    } else {
        (
            html! { <p>{ "Loading puzzle image..." }</p> },
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            true,
        )
    };

    let preview_corner_class = match *preview_corner {
        PreviewCorner::BottomLeft => "corner-bl",
        PreviewCorner::BottomRight => "corner-br",
        PreviewCorner::TopLeft => "corner-tl",
        PreviewCorner::TopRight => "corner-tr",
    };
    let preview_state_class = if preview_revealed_value {
        "preview-revealed"
    } else {
        "preview-hidden"
    };
    let preview_class = format!(
        "preview-box {} {}",
        preview_corner_class, preview_state_class
    );
    let on_preview_toggle = {
        let preview_revealed = preview_revealed.clone();
        Callback::from(move |_| {
            preview_revealed.set(!*preview_revealed);
        })
    };
    let preview_toggle_label = if preview_revealed_value {
        "Hide preview"
    } else {
        "Show preview"
    };
    let on_preview_hide = {
        let preview_revealed = preview_revealed.clone();
        Callback::from(move |_| {
            if *preview_revealed {
                preview_revealed.set(false);
            }
        })
    };
    let on_preview_horizontal = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::BottomRight,
                PreviewCorner::BottomRight => PreviewCorner::BottomLeft,
                PreviewCorner::TopLeft => PreviewCorner::TopRight,
                PreviewCorner::TopRight => PreviewCorner::TopLeft,
            };
            preview_corner.set(next);
        })
    };
    let on_preview_vertical = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::TopLeft,
                PreviewCorner::BottomRight => PreviewCorner::TopRight,
                PreviewCorner::TopLeft => PreviewCorner::BottomLeft,
                PreviewCorner::TopRight => PreviewCorner::BottomRight,
            };
            preview_corner.set(next);
        })
    };
    let preview_arrow_horizontal = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::TopLeft => "preview-arrow preview-arrow-right",
        PreviewCorner::BottomRight | PreviewCorner::TopRight => "preview-arrow preview-arrow-left",
    };
    let preview_arrow_vertical = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::BottomRight => "preview-arrow preview-arrow-up",
        PreviewCorner::TopLeft | PreviewCorner::TopRight => "preview-arrow preview-arrow-down",
    };
    let preview_toggle_slash = if preview_revealed_value {
        html! { <line class="preview-toggle-slash" x1="5" y1="19" x2="19" y2="5" /> }
    } else {
        html! {}
    };
    let preview_box = if show_svg {
        html! {
            <aside class={preview_class}>
            <button
                class="preview-toggle"
                type="button"
                aria-label={preview_toggle_label}
                aria-pressed={if preview_revealed_value { "true" } else { "false" }}
                onclick={on_preview_toggle}
            >
                <svg class="preview-toggle-icon" viewBox="0 0 24 24" aria-hidden="true">
                    <path
                        class="preview-toggle-eye"
                        d="M2 12c2.4-4.2 5.8-6.4 10-6.4s7.6 2.2 10 6.4c-2.4 4.2-5.8 6.4-10 6.4S4.4 16.2 2 12z"
                    />
                    <circle class="preview-toggle-pupil" cx="12" cy="12" r="3.2" />
                    {preview_toggle_slash}
                </svg>
            </button>
            <button
                class={preview_arrow_horizontal}
                type="button"
                aria-label="Move preview horizontally"
                onclick={on_preview_horizontal}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <button
                class={preview_arrow_vertical}
                type="button"
                aria-label="Move preview vertically"
                onclick={on_preview_vertical}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <img src={puzzle_src} alt="preview" onclick={on_preview_hide} />
            </aside>
        }
    } else {
        html! {}
    };
    let controls_panel = if show_dev_panel && show_controls_value {
        html! {
            <aside class="controls">
                <h2>{ "Dev Panel" }</h2>
                <p class={status_class}>{ status_label }</p>
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
                                    onchange={on_share_seed_toggle}
                                />
                            </label>
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
                        </>
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
                <div class="control">
                    <label for="workspace-scale">
                        { "Workspace scale" }
                        <span class="control-value">{ fmt_f32(workspace_scale_value) }</span>
                    </label>
                    <input
                        id="workspace-scale"
                        type="range"
                        min={WORKSPACE_SCALE_MIN.to_string()}
                        max={WORKSPACE_SCALE_MAX.to_string()}
                        step="0.05"
                        value={workspace_scale_value.to_string()}
                        oninput={on_workspace_scale}
                    />
                </div>
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
                { if renderer_kind == RendererKind::Svg {
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
                <hr class="control-separator" />
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
            </aside>
        }
    } else {
        html! {}
    };
    let content_view = if show_svg { content } else { html! {} };
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
            {content_view}
            {preview_box}
            {controls_panel}
        </>
    };
    if show_svg {
        let mut app_class = "app".to_string();
        if multiplayer_disconnected {
            app_class.push_str(" sync-disconnected");
        }
        html! {
            <main class={app_class}>
                {body}
            </main>
        }
    } else {
        html! {
            <div class="dev-panel-root">
                {body}
            </div>
        }
    }
}

pub(crate) fn run_svg() {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Some(document) = window.document() else {
        return;
    };
    let Some(root) = document.get_element_by_id("svg-root") else {
        return;
    };
    let _app_handle =
        yew::Renderer::<App>::with_root_and_props(root, AppProps { mode: AppMode::Svg }).render();
}

pub(crate) fn run_dev_panel() {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Some(document) = window.document() else {
        return;
    };
    let Some(root) = document.get_element_by_id("dev-panel-root") else {
        return;
    };
    let _app_handle =
        yew::Renderer::<App>::with_root_and_props(root, AppProps { mode: AppMode::DevPanel })
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
