//! State and behaviour shared by the wgpu and SVG views: UI text layout and
//! hitboxes, the preview overlay (placement, drag, pinch, momentum), edge
//! auto-pan while dragging, the private-image resolver, piece picking and the
//! debug overlay. Each renderer embeds a [`ViewShell`], implements
//! [`ViewShellHost`] for its drawing-specific callbacks, and keeps only canvas
//! or DOM work in its own module.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::render::{AnimationFrame, request_animation_frame};
use glyphon::cosmic_text::Align;
use glyphon::{Attrs, Buffer, Family, FontSystem, Metrics, Shaping};
use taffy::prelude::*;
use wasm_bindgen_futures::spawn_local;
use web_sys::{Document, Element, PointerEvent};

use crate::app_core::{AppCore, AppSnapshot, ViewRect};
use crate::boot;
use crate::core::*;
use crate::input::{ClickGesture, PointerKind, screen_to_view_coords, workspace_to_puzzle_coords};
use crate::persisted_store;
use crate::puzzle_image::{create_object_url, revoke_object_url};
use crate::renderer::{MaskAtlasData, UiRotationOrigin, UiTextId, UiTextSpec, measure_text_bounds};
use crate::runtime::{CoreAction, GameSyncView, ViewHooks};
use crate::view_runtime::ActiveView;
use crate::web::{now_ms, now_ms_u64};
use heddobureika_core::{ClientId, PuzzleImageRef, PuzzleInfo};
use std::collections::HashMap;
pub(crate) const UI_FONT_BYTES: &[u8] = include_bytes!("../../fonts/chirufont.ttf");
pub(crate) const CREDIT_TEXT: &str = "created by すごいジャン";
pub(crate) const UI_TITLE_TEXT: &str = "ヘッドブレイカー";
pub(crate) const CREDIT_URL: &str = "https://github.com/sugoijan/heddobureika";
pub(crate) const UI_FONT_FAMILY: &str = "KaoriGel";
pub(crate) const UI_CREDIT_FONT_RATIO: f32 = 0.026;
pub(crate) const UI_CREDIT_ROTATION_DEG: f32 = -1.1;
pub(crate) const UI_ART_CREDIT_FONT_RATIO: f32 = 0.022;
pub(crate) const UI_ART_CREDIT_ROTATION_DEG: f32 = -1.1;
/// Spacing between the art credit and the puzzle frame's bottom-right
/// corner (inset from the right edge, gap below the bottom edge), as a
/// fraction of its font size.
pub(crate) const UI_ART_CREDIT_PAD_RATIO: f32 = 0.6;
pub(crate) const UI_MENU_SUB_FONT_RATIO: f32 = 0.028;
pub(crate) const UI_MENU_SUB_ROTATION_DEG: f32 = 1.1;
pub(crate) const UI_MENU_TITLE_FONT_RATIO: f32 = 0.05;
pub(crate) const UI_MENU_TITLE_ROTATION_DEG: f32 = -0.8;
pub(crate) const UI_PROGRESS_FONT_RATIO: f32 = 0.032;
pub(crate) const UI_PROGRESS_ROTATION_DEG: f32 = -89.6;
pub(crate) const UI_SUCCESS_FONT_RATIO: f32 = 0.082;
pub(crate) const UI_SUCCESS_OFFSET_RATIO: f32 = 0.03;
pub(crate) const UI_SUCCESS_ROTATION_DEG: f32 = -1.3;
pub(crate) const UI_TEXT_ALPHA_SCALE: f32 = 0.8;
pub(crate) const UI_TEXT_LAYOUT_SCALE: f32 = 1.08;
pub(crate) const UI_TEXT_LAYOUT_PAD: f32 = 6.0;
pub(crate) const UI_TEXT_HITBOX_PAD: f32 = 8.0;
pub(crate) const UI_TITLE_ROTATION_DEG: f32 = 0.5;
pub(crate) const UI_TITLE_FONT_RATIO: f32 = 0.058;
pub(crate) const PREVIEW_BOX_WIDTH: f32 = 170.0;
pub(crate) const PREVIEW_BOX_MIN_WIDTH: f32 = 120.0;
pub(crate) const PREVIEW_BOX_INIT_FRAC: f32 = 0.24;
pub(crate) const PREVIEW_BOX_MAX_FRAC: f32 = 0.28;
pub(crate) const PREVIEW_BOX_PADDING: f32 = 8.0;
pub(crate) const PREVIEW_BOX_MARGIN: f32 = 16.0;
pub(crate) const PREVIEW_RESIZE_BORDER: f32 = 10.0;
pub(crate) const PREVIEW_MIN_VISIBLE: f32 = 32.0;
pub(crate) const PREVIEW_MIN_VISIBLE_FRAC: f32 = 0.35;
pub(crate) const PREVIEW_MOMENTUM_DECAY: f32 = 8.0;
pub(crate) const PREVIEW_VELOCITY_EPS: f32 = 10.0;
pub(crate) const PREVIEW_VELOCITY_SMOOTH: f32 = 0.25;
pub(crate) const PREVIEW_FIT_PADDING_RATIO: f32 = 0.02;

const UI_DEBUG_FONT_RATIO: f32 = 0.02;

pub(crate) fn build_ui_specs(
    measure: &mut GlyphonMeasureState,
    layout: WorkspaceLayout,
    width: f32,
    height: f32,
    title_text: &str,
    art_credit_text: Option<&str>,
    frame_rect: [f32; 4],
    solved: bool,
    connections_label: &str,
    border_connections_label: &str,
    show_debug: bool,
    backend_label: Option<&str>,
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

        if let Some(text) = art_credit_text {
            // Anchored just below the puzzle frame's bottom-right corner
            // rather than the workspace gutter grid, so it stays visible
            // once the puzzle is complete; apply_taffy_layout skips it.
            let art_size = min_dim * UI_ART_CREDIT_FONT_RATIO;
            let mut spec = UiTextSpec {
                id: UiTextId::ArtCredit,
                text: text.to_string(),
                pos: [0.0, 0.0],
                rotation_deg: UI_ART_CREDIT_ROTATION_DEG,
                rotation_origin: UiRotationOrigin::BottomRight,
                rotation_offset: [0.0, 0.0],
                font_size: art_size,
                line_height: art_size * 1.08,
                color: base_color,
            };
            let (text_width, text_height) = layout_text_bounds(measure, &spec);
            spec.rotation_offset =
                rotation_offset_for(spec.rotation_origin, text_width, text_height);
            let pad = art_size * UI_ART_CREDIT_PAD_RATIO;
            spec.pos = [
                frame_rect[0] + frame_rect[2] - pad - text_width * 0.5,
                frame_rect[1] + frame_rect[3] + pad + text_height * 0.5,
            ];
            specs.push(spec);
        }

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
    if show_debug && !menu_visible {
        if let Some(label) = backend_label {
            let debug_size = min_dim * UI_DEBUG_FONT_RATIO;
            specs.push(UiTextSpec {
                id: UiTextId::Debug,
                text: format!("WGPU backend: {label}"),
                pos: [0.0, 0.0],
                rotation_deg: 0.0,
                rotation_origin: UiRotationOrigin::Center,
                rotation_offset: [0.0, 0.0],
                font_size: debug_size,
                line_height: debug_size * 1.08,
                color: muted_color,
            });
        }
    }

    apply_taffy_layout(measure, layout, width, height, menu_visible, &mut specs);
    specs
}

#[derive(Clone, Copy)]
pub(crate) struct UiHitbox {
    pub(crate) center: [f32; 2],
    pub(crate) half_size: [f32; 2],
    pub(crate) rotation_deg: f32,
}
#[derive(Clone, Copy)]
pub(crate) struct PanState {
    pub(crate) last_x: f32,
    pub(crate) last_y: f32,
    pub(crate) pointer_id: Option<i32>,
}
#[derive(Clone, Copy)]
pub(crate) struct PinchState {
    pub(crate) touch_a: i32,
    pub(crate) touch_b: i32,
    pub(crate) last_distance: f32,
}
#[derive(Clone, Copy)]
pub(crate) struct AutoPanRect {
    pub(crate) left: f32,
    pub(crate) right: f32,
    pub(crate) top: f32,
    pub(crate) bottom: f32,
}
impl AutoPanRect {
    pub(crate) fn contains(&self, x: f32, y: f32) -> bool {
        x >= self.left && x <= self.right && y >= self.top && y <= self.bottom
    }
}
pub(crate) struct AutoPanZones {
    pub(crate) outer: AutoPanRect,
    pub(crate) inner: AutoPanRect,
    pub(crate) outer_inset: f32,
    pub(crate) inner_inset: f32,
}
pub(crate) struct AutoPanState {
    pub(crate) active: bool,
    pub(crate) seen_inner: bool,
    pub(crate) has_pointer: bool,
    pub(crate) last_screen_x: f32,
    pub(crate) last_screen_y: f32,
    pub(crate) last_tick_ms: Option<f64>,
    pub(crate) frame: Option<AnimationFrame>,
}
impl AutoPanState {
    pub(crate) fn new() -> Self {
        Self {
            active: false,
            seen_inner: false,
            has_pointer: false,
            last_screen_x: 0.0,
            last_screen_y: 0.0,
            last_tick_ms: None,
            frame: None,
        }
    }
}
pub(crate) struct DebugOverlay {
    pub(crate) root: Element,
    pub(crate) outer: Element,
    pub(crate) inner: Element,
}
impl DebugOverlay {
    pub(crate) fn new(document: &Document) -> Self {
        let root = document
            .create_element("div")
            .expect("create debug overlay root");
        root.set_class_name("debug-rect-overlay");
        let outer = document
            .create_element("div")
            .expect("create debug outer rect");
        outer.set_class_name("debug-rect debug-rect-outer");
        let inner = document
            .create_element("div")
            .expect("create debug inner rect");
        inner.set_class_name("debug-rect debug-rect-inner");
        let _ = root.append_child(&outer);
        let _ = root.append_child(&inner);
        Self { root, outer, inner }
    }

    pub(crate) fn set_visible(&self, visible: bool) {
        if visible {
            let _ = self.root.remove_attribute("style");
        } else {
            let _ = self.root.set_attribute("style", "display: none;");
        }
    }

    pub(crate) fn set_insets(&self, outer_inset: f32, inner_inset: f32) {
        let outer_style = format!("inset: {}px;", fmt_f32(outer_inset));
        let inner_style = format!("inset: {}px;", fmt_f32(inner_inset));
        let _ = self.outer.set_attribute("style", &outer_style);
        let _ = self.inner.set_attribute("style", &inner_style);
    }
}
pub(crate) struct GlyphonMeasureState {
    pub(crate) font_system: FontSystem,
}
impl GlyphonMeasureState {
    pub(crate) fn new() -> Self {
        let mut font_system = FontSystem::new();
        font_system.db_mut().load_font_data(UI_FONT_BYTES.to_vec());
        Self { font_system }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PreviewResizeHandle {
    Left,
    Right,
    Top,
    Bottom,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PreviewHoverTarget {
    None,
    Body,
    Resize(PreviewResizeHandle),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PreviewDragKind {
    Move,
    Resize,
    Pinch,
}
#[derive(Clone, Copy, Debug)]
pub(crate) enum PreviewDragState {
    Move {
        pointer_id: Option<i32>,
        grab_offset: [f32; 2],
        last_ms: f32,
    },
    Resize {
        pointer_id: Option<i32>,
        handle: PreviewResizeHandle,
        start_pos: [f32; 2],
        start_width: f32,
        last_ms: f32,
    },
    Pinch {
        touch_a: i32,
        touch_b: i32,
        start_distance: f32,
        start_width: f32,
        anchor_rel: [f32; 2],
    },
}
#[derive(Clone, Copy, Debug)]
pub(crate) struct PreviewClickState {
    pub(crate) pointer_id: Option<i32>,
    pub(crate) gesture: ClickGesture,
}
impl PreviewDragState {
    pub(crate) fn pointer_id(&self) -> Option<i32> {
        match self {
            PreviewDragState::Move { pointer_id, .. } => *pointer_id,
            PreviewDragState::Resize { pointer_id, .. } => *pointer_id,
            PreviewDragState::Pinch { .. } => None,
        }
    }

    pub(crate) fn kind(&self) -> PreviewDragKind {
        match self {
            PreviewDragState::Move { .. } => PreviewDragKind::Move,
            PreviewDragState::Resize { .. } => PreviewDragKind::Resize,
            PreviewDragState::Pinch { .. } => PreviewDragKind::Pinch,
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub(crate) struct Rect {
    pub(crate) x: f32,
    pub(crate) y: f32,
    pub(crate) w: f32,
    pub(crate) h: f32,
}
impl Rect {
    pub(crate) fn contains(&self, x: f32, y: f32) -> bool {
        x >= self.x && x <= self.x + self.w && y >= self.y && y <= self.y + self.h
    }

    pub(crate) fn inset(&self, inset: f32) -> Self {
        let inset = inset.max(0.0);
        let w = (self.w - inset * 2.0).max(0.0);
        let h = (self.h - inset * 2.0).max(0.0);
        Self {
            x: self.x + inset,
            y: self.y + inset,
            w,
            h,
        }
    }

    pub(crate) fn center(&self) -> [f32; 2] {
        [self.x + self.w * 0.5, self.y + self.h * 0.5]
    }
}
pub(crate) struct PreviewLayout {
    pub(crate) box_rect: Rect,
    pub(crate) image_rect: Rect,
}
#[derive(Clone, PartialEq)]
pub(crate) struct PuzzleKey {
    pub(crate) image_ref: PuzzleImageRef,
    pub(crate) topology: heddobureika_core::PlayableTopologySnapshot,
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) shape_seed: u32,
}
impl PuzzleKey {
    pub(crate) fn from_info(info: &PuzzleInfo) -> Self {
        Self {
            image_ref: info.image_ref.clone(),
            topology: info.topology.clone(),
            width: info.image_width,
            height: info.image_height,
            shape_seed: info.shape_seed,
        }
    }
}
pub(crate) fn smoothstep(t: f32) -> f32 {
    let t = t.clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}
pub(crate) fn edge_intensity(
    pos: f32,
    inner_min: f32,
    inner_max: f32,
    inner_inset: f32,
) -> (f32, f32) {
    if inner_inset <= 0.0 {
        return (0.0, 0.0);
    }
    if pos < inner_min {
        let dist = (inner_min - pos).min(inner_inset);
        (-1.0, dist / inner_inset)
    } else if pos > inner_max {
        let dist = (pos - inner_max).min(inner_inset);
        (1.0, dist / inner_inset)
    } else {
        (0.0, 0.0)
    }
}
pub(crate) fn workspace_fade_scale(x: f32, y: f32, layout: WorkspaceLayout, view: ViewRect) -> f32 {
    let min_x = layout.view_min_x;
    let min_y = layout.view_min_y;
    let max_x = min_x + layout.view_width;
    let max_y = min_y + layout.view_height;
    let dx = if x < min_x {
        min_x - x
    } else if x > max_x {
        x - max_x
    } else {
        0.0
    };
    let dy = if y < min_y {
        min_y - y
    } else if y > max_y {
        y - max_y
    } else {
        0.0
    };
    let dist = dx.max(dy);
    if dist <= 0.0 {
        return 1.0;
    }
    let falloff = view.width.max(view.height) * 0.2;
    if falloff <= 0.0 {
        return 0.0;
    }
    let t = (dist / falloff).clamp(0.0, 1.0);
    1.0 - smoothstep(t)
}
pub(crate) fn drag_angle_for_group(count: usize) -> f32 {
    let denom = (count.max(1) as f32).sqrt();
    DRAG_ROTATION_DEG / denom
}
pub(crate) fn drag_group_center(
    positions: &[(f32, f32)],
    members: &[usize],
    geometry: &PuzzleRenderGeometry,
) -> Option<(f32, f32)> {
    if members.is_empty() {
        return None;
    }
    let mut sum_x = 0.0;
    let mut sum_y = 0.0;
    let mut count = 0.0;
    for id in members {
        if let (Some(pos), Some(piece)) = (positions.get(*id), geometry.pieces.get(*id)) {
            sum_x += pos.0 + piece.pose_anchor_px[0];
            sum_y += pos.1 + piece.pose_anchor_px[1];
            count += 1.0;
        }
    }
    if count > 0.0 {
        Some((sum_x / count, sum_y / count))
    } else {
        None
    }
}
pub(crate) fn drag_group_position(
    pos: (f32, f32),
    center: (f32, f32),
    scale: f32,
    rotation_deg: f32,
    pose_anchor_px: [f32; 2],
) -> (f32, f32) {
    let piece_center = (pos.0 + pose_anchor_px[0], pos.1 + pose_anchor_px[1]);
    let mut dx = piece_center.0 - center.0;
    let mut dy = piece_center.1 - center.1;
    dx *= scale;
    dy *= scale;
    let (rx, ry) = rotate_vec(dx, dy, rotation_deg);
    let new_center = (center.0 + rx, center.1 + ry);
    (
        new_center.0 - pose_anchor_px[0],
        new_center.1 - pose_anchor_px[1],
    )
}
/// Whether `piece_id` belongs to a group another player currently holds.
/// `ownership` is keyed by group anchor (see `AppSnapshot::piece_group_anchor`).
pub(crate) fn piece_owned_by_other(
    snapshot: &AppSnapshot,
    ownership: &HashMap<u32, ClientId>,
    own_client_id: Option<ClientId>,
    piece_id: usize,
) -> bool {
    let Some(anchor_id) = snapshot.piece_group_anchor.get(piece_id) else {
        return false;
    };
    ownership
        .get(anchor_id)
        .is_some_and(|owner_id| Some(*owner_id) != own_client_id)
}

/// Group ownership by other players for the current sync mode, or `None`
/// when no piece can be owned by someone else (local play / no owners).
pub(crate) fn foreign_ownership(
    sync_view: &dyn GameSyncView,
) -> Option<(Rc<HashMap<u32, ClientId>>, Option<ClientId>)> {
    if matches!(sync_view.mode(), InitMode::Local) {
        return None;
    }
    let ownership = sync_view.ownership_by_anchor();
    (!ownership.is_empty()).then(|| (ownership, sync_view.client_id()))
}
pub(crate) fn pick_piece_at(
    x: f32,
    y: f32,
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    mask_atlas: &MaskAtlasData,
    geometry: &PuzzleRenderGeometry,
    mask_pad: f32,
) -> Option<usize> {
    if geometry.pieces.is_empty() {
        return None;
    }
    let atlas_width = mask_atlas.width as i32;
    let atlas_height = mask_atlas.height as i32;
    for &piece_id in z_order.iter().rev() {
        let Some(piece) = geometry.pieces.get(piece_id) else {
            continue;
        };
        let pos = positions
            .get(piece_id)
            .copied()
            .unwrap_or((piece.bounds_px.x, piece.bounds_px.y));
        let rotation = rotations.get(piece_id).copied().unwrap_or(0.0);
        let flipped = flips.get(piece_id).copied().unwrap_or(false);
        let Some((local_x, local_y)) =
            geometry.hit_test_local_coords(piece.id, (x, y), pos, rotation, flipped)
        else {
            continue;
        };
        let min_x = -mask_pad;
        let min_y = -mask_pad;
        let max_x = piece.bounds_px.width + mask_pad;
        let max_y = piece.bounds_px.height + mask_pad;
        if local_x < min_x || local_y < min_y || local_x > max_x || local_y > max_y {
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
pub(crate) fn estimate_text_width(text: &str, font_size: f32) -> f32 {
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
pub(crate) fn estimate_text_block(spec: &UiTextSpec) -> (f32, f32) {
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
pub(crate) fn measure_text_block(
    measure: &mut GlyphonMeasureState,
    spec: &UiTextSpec,
) -> (f32, f32) {
    let metrics = Metrics::new(spec.font_size, spec.line_height);
    let mut buffer = Buffer::new(&mut measure.font_system, metrics);
    buffer.set_size(None, None);
    let attrs = Attrs::new().family(Family::Name(UI_FONT_FAMILY));
    buffer.set_text(&spec.text, &attrs, Shaping::Advanced, Some(Align::Left));
    buffer.shape_until_scroll(&mut measure.font_system, false);
    let (width, height) = measure_text_bounds(&buffer);
    if width <= 0.0 || height <= 0.0 {
        estimate_text_block(spec)
    } else {
        (width, height)
    }
}
pub(crate) fn layout_text_bounds(
    measure: &mut GlyphonMeasureState,
    spec: &UiTextSpec,
) -> (f32, f32) {
    let (text_width, text_height) = measure_text_block(measure, spec);
    let scaled_width = text_width * UI_TEXT_LAYOUT_SCALE;
    let scaled_height = text_height * UI_TEXT_LAYOUT_SCALE;
    (
        (scaled_width + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
        (scaled_height + UI_TEXT_LAYOUT_PAD * 2.0).max(1.0),
    )
}
pub(crate) fn rotation_offset_for(origin: UiRotationOrigin, width: f32, height: f32) -> [f32; 2] {
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
pub(crate) fn apply_taffy_layout(
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
        grid_template_rows: vec![
            length(top_gutter),
            length(puzzle_height),
            length(bottom_gutter),
        ],
        justify_items: JustifyItems::CENTER,
        align_items: AlignItems::CENTER,
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
            align_items: AlignItems::CENTER,
            justify_content: JustifyContent::CENTER,
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
                UiTextId::Title => (1, 1, false, JustifySelf::START, AlignSelf::START),
                UiTextId::Progress => (3, 1, false, JustifySelf::START, AlignSelf::END),
                UiTextId::Credit => (3, 3, false, JustifySelf::END, AlignSelf::END),
                // Positioned directly against the puzzle frame rect in
                // build_ui_specs; not part of the workspace gutter grid.
                UiTextId::ArtCredit => continue,
                UiTextId::Success => (1, 2, false, JustifySelf::CENTER, AlignSelf::CENTER),
                UiTextId::Debug => (1, 2, false, JustifySelf::END, AlignSelf::START),
                UiTextId::MenuTitle | UiTextId::MenuSubtitle => {
                    (2, 2, false, JustifySelf::CENTER, AlignSelf::CENTER)
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
pub(crate) fn ui_hitbox_for_spec(measure: &mut GlyphonMeasureState, spec: &UiTextSpec) -> UiHitbox {
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
pub(crate) fn point_in_ui_hitbox(x: f32, y: f32, hitbox: UiHitbox) -> bool {
    let dx = x - hitbox.center[0];
    let dy = y - hitbox.center[1];
    let angle = -hitbox.rotation_deg.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();
    let local_x = dx * cos - dy * sin;
    let local_y = dx * sin + dy * cos;
    local_x.abs() <= hitbox.half_size[0] && local_y.abs() <= hitbox.half_size[1]
}
pub(crate) fn ui_scale_alpha(color: [u8; 4], scale: f32) -> [u8; 4] {
    let scaled = (color[3] as f32 * scale).round().clamp(0.0, 255.0) as u8;
    [color[0], color[1], color[2], scaled]
}
pub(crate) fn ui_scaled_alpha(alpha: u8) -> u8 {
    (alpha as f32 * UI_TEXT_ALPHA_SCALE)
        .round()
        .clamp(0.0, 255.0) as u8
}
pub(crate) fn ui_text_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [235, 234, 230, 230]
    } else {
        [20, 20, 20, 220]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}
pub(crate) fn ui_accent_color(is_dark: bool) -> [u8; 4] {
    let base = if is_dark {
        [168, 255, 208, 240]
    } else {
        [24, 120, 62, 230]
    };
    ui_scale_alpha(base, UI_TEXT_ALPHA_SCALE)
}
pub(crate) fn ui_with_alpha(color: [u8; 4], alpha: u8) -> [u8; 4] {
    [color[0], color[1], color[2], alpha]
}
pub(crate) fn preview_cursor_class(target: PreviewHoverTarget) -> Option<&'static str> {
    match target {
        PreviewHoverTarget::None => None,
        PreviewHoverTarget::Body => Some("preview-grab"),
        PreviewHoverTarget::Resize(handle) => Some(match handle {
            PreviewResizeHandle::Left | PreviewResizeHandle::Right => "preview-resize-ew",
            PreviewResizeHandle::Top | PreviewResizeHandle::Bottom => "preview-resize-ns",
            PreviewResizeHandle::TopLeft | PreviewResizeHandle::BottomRight => {
                "preview-resize-nwse"
            }
            PreviewResizeHandle::TopRight | PreviewResizeHandle::BottomLeft => {
                "preview-resize-nesw"
            }
        }),
    }
}
/// Renderer-agnostic view state. Field names match the former per-renderer
/// fields so the drawing code reads the same (`self.shell.preview_pos`).
pub(crate) struct ViewShell {
    pub(crate) core: Rc<AppCore>,
    /// The element whose bounding rect is the viewport (canvas or svg root).
    pub(crate) viewport: Element,
    /// Preview overlay tilt and fit scale; the renderers draw the preview
    /// slightly differently, so each supplies its own.
    pub(crate) tilt_deg: f32,
    pub(crate) max_fit_scale: f32,
    pub(crate) hooks: RefCell<Option<ViewHooks>>,
    pub(crate) multiplayer_active: Cell<bool>,
    pub(crate) mask_atlas: RefCell<Option<Rc<MaskAtlasData>>>,
    pub(crate) last_puzzle: RefCell<Option<PuzzleKey>>,
    pub(crate) ui_measure: RefCell<GlyphonMeasureState>,
    pub(crate) ui_credit_hitbox: RefCell<Option<UiHitbox>>,
    /// Hitbox + target url for the art credit; only set when the current
    /// puzzle's catalog entry has both credit_text and credit_url.
    pub(crate) ui_art_credit_hitbox: RefCell<Option<(UiHitbox, &'static str)>>,
    pub(crate) auto_pan: RefCell<AutoPanState>,
    pub(crate) debug_overlay: RefCell<Option<DebugOverlay>>,
    pub(crate) preview_revealed: Cell<bool>,
    pub(crate) preview_hover: Cell<PreviewHoverTarget>,
    pub(crate) preview_pos: Cell<[f32; 2]>,
    pub(crate) preview_width: Cell<f32>,
    pub(crate) preview_velocity: Cell<[f32; 2]>,
    pub(crate) preview_drag: RefCell<Option<PreviewDragState>>,
    pub(crate) preview_click: RefCell<Option<PreviewClickState>>,
    pub(crate) preview_seeded: Cell<bool>,
    pub(crate) preview_motion_last_ms: Cell<f32>,
    pub(crate) private_image_url: RefCell<Option<String>>,
    pub(crate) private_image_hash: RefCell<Option<String>>,
    pub(crate) private_image_loading: Cell<bool>,
    pub(crate) private_image_error: RefCell<Option<String>>,
}

impl ViewShell {
    pub(crate) fn new(
        core: Rc<AppCore>,
        viewport: Element,
        tilt_deg: f32,
        max_fit_scale: f32,
    ) -> Self {
        Self {
            core,
            viewport,
            tilt_deg,
            max_fit_scale,
            hooks: RefCell::new(None),
            multiplayer_active: Cell::new(false),
            mask_atlas: RefCell::new(None),
            last_puzzle: RefCell::new(None),
            ui_measure: RefCell::new(GlyphonMeasureState::new()),
            ui_credit_hitbox: RefCell::new(None),
            ui_art_credit_hitbox: RefCell::new(None),
            auto_pan: RefCell::new(AutoPanState::new()),
            debug_overlay: RefCell::new(None),
            preview_revealed: Cell::new(false),
            preview_hover: Cell::new(PreviewHoverTarget::None),
            preview_pos: Cell::new([0.0, 0.0]),
            preview_width: Cell::new(PREVIEW_BOX_WIDTH),
            preview_velocity: Cell::new([0.0, 0.0]),
            preview_drag: RefCell::new(None),
            preview_click: RefCell::new(None),
            preview_seeded: Cell::new(false),
            preview_motion_last_ms: Cell::new(0.0),
            private_image_url: RefCell::new(None),
            private_image_hash: RefCell::new(None),
            private_image_loading: Cell::new(false),
            private_image_error: RefCell::new(None),
        }
    }

    /// Resets the preview overlay interaction state (puzzle change).
    pub(crate) fn reset_preview_state(&self) {
        self.preview_drag.borrow_mut().take();
        self.preview_click.borrow_mut().take();
        self.preview_seeded.set(false);
        self.preview_hover.set(PreviewHoverTarget::None);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(0.0);
    }

    pub(crate) fn clear_private_image_cache(&self) {
        if let Some(url) = self.private_image_url.borrow_mut().take() {
            revoke_object_url(&url);
        }
        self.private_image_hash.borrow_mut().take();
        self.private_image_error.borrow_mut().take();
        self.private_image_loading.set(false);
    }

    pub(crate) fn note_private_image_error(&self, hash: &str, message: &str) {
        let mut last = self.private_image_error.borrow_mut();
        if last.as_deref() == Some(hash) {
            return;
        }
        *last = Some(hash.to_string());
        if !self.multiplayer_active.get() {
            boot::fail(
                "image-missing",
                message,
                "Re-upload the image to this browser.",
            );
        }
        self.private_image_loading.set(false);
    }

    pub(crate) fn pointer_kind(event: &PointerEvent) -> PointerKind {
        PointerKind::from_pointer_type(&event.pointer_type())
    }

    pub(crate) fn hit_credit(&self, x: f32, y: f32) -> bool {
        self.ui_credit_hitbox
            .borrow()
            .map(|hitbox| point_in_ui_hitbox(x, y, hitbox))
            .unwrap_or(false)
    }

    /// The art credit's target url when (x, y) hits its hitbox. Art credits
    /// without a url have no hitbox and are never hit.
    pub(crate) fn hit_art_credit(&self, x: f32, y: f32) -> Option<&'static str> {
        self.ui_art_credit_hitbox
            .borrow()
            .and_then(|(hitbox, url)| point_in_ui_hitbox(x, y, hitbox).then_some(url))
    }

    pub(crate) fn set_hooks(&self, hooks: ViewHooks) {
        *self.hooks.borrow_mut() = Some(hooks);
    }

    pub(crate) fn dispatch_action(&self, action: CoreAction) {
        if let Some(hooks) = self.hooks.borrow().as_ref() {
            (hooks.on_action)(action);
        }
    }

    pub(crate) fn set_preview_hover(&self, target: PreviewHoverTarget) -> bool {
        if self.preview_hover.get() == target {
            return false;
        }
        self.preview_hover.set(target);
        true
    }

    pub(crate) fn arm_preview_click(&self, pointer_id: Option<i32>, local_x: f32, local_y: f32) {
        let mut gesture = ClickGesture::new_default();
        gesture.arm(local_x, local_y, now_ms());
        *self.preview_click.borrow_mut() = Some(PreviewClickState {
            pointer_id,
            gesture,
        });
    }

    pub(crate) fn clear_preview_click(&self) {
        self.preview_click.borrow_mut().take();
    }

    pub(crate) fn update_preview_click_movement(
        &self,
        pointer_id: Option<i32>,
        local_x: f32,
        local_y: f32,
    ) {
        let mut click_ref = self.preview_click.borrow_mut();
        let Some(click) = click_ref.as_mut() else {
            return;
        };
        if click.pointer_id != pointer_id {
            return;
        }
        click.gesture.update(local_x, local_y);
    }

    pub(crate) fn consume_preview_click(&self, pointer_id: Option<i32>) -> bool {
        let mut click_ref = self.preview_click.borrow_mut();
        let Some(click) = click_ref.take() else {
            return false;
        };
        if click.pointer_id != pointer_id {
            *click_ref = Some(click);
            return false;
        }
        click.gesture.is_click(now_ms())
    }

    pub(crate) fn preview_aspect(info: &PuzzleInfo) -> f32 {
        if info.image_width > 0 {
            info.image_height as f32 / info.image_width as f32
        } else {
            1.0
        }
    }

    pub(crate) fn preview_frame_max_width(
        &self,
        info: &PuzzleInfo,
        layout: WorkspaceLayout,
        viewport_w: f32,
        viewport_h: f32,
    ) -> Option<f32> {
        if info.image_width == 0 || info.image_height == 0 {
            return None;
        }
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return None;
        }
        let scale = layout.puzzle_scale.max(1.0e-4);
        let frame_w = info.image_width as f32 * scale;
        let frame_h = info.image_height as f32 * scale;
        if !frame_w.is_finite() || !frame_h.is_finite() || frame_w <= 0.0 || frame_h <= 0.0 {
            return None;
        }
        let target_w = layout.view_width.max(1.0) * (1.0 + PREVIEW_FIT_PADDING_RATIO);
        let target_h = layout.view_height.max(1.0) * (1.0 + PREVIEW_FIT_PADDING_RATIO);
        let fit_zoom = (viewport_w / target_w).min(viewport_h / target_h);
        if !fit_zoom.is_finite() || fit_zoom <= 0.0 {
            return None;
        }
        let width = frame_w * fit_zoom * self.max_fit_scale + PREVIEW_BOX_PADDING * 2.0;
        if width.is_finite() && width > 0.0 {
            Some(width)
        } else {
            None
        }
    }

    pub(crate) fn preview_width_bounds(
        &self,
        aspect: f32,
        viewport_w: f32,
        viewport_h: f32,
        frame_max_w: Option<f32>,
    ) -> (f32, f32) {
        let min_w = PREVIEW_BOX_MIN_WIDTH.max(1.0);
        let mut max_w = (viewport_w - PREVIEW_BOX_MARGIN * 2.0).max(min_w);
        if aspect > 0.0 {
            let max_h = (viewport_h - PREVIEW_BOX_MARGIN * 2.0).max(1.0);
            let max_w_from_h =
                (max_h - PREVIEW_BOX_PADDING * 2.0).max(1.0) / aspect + PREVIEW_BOX_PADDING * 2.0;
            if max_w_from_h.is_finite() {
                max_w = max_w.min(max_w_from_h);
            }
        }
        if let Some(frame_max_w) = frame_max_w {
            if frame_max_w.is_finite() && frame_max_w > 0.0 {
                max_w = max_w.min(frame_max_w);
            }
        }
        if max_w < min_w {
            max_w = min_w;
        }
        (min_w, max_w)
    }

    pub(crate) fn clamp_preview_width(
        &self,
        aspect: f32,
        viewport_w: f32,
        viewport_h: f32,
        frame_max_w: Option<f32>,
    ) -> f32 {
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let current = self.preview_width.get();
        let mut next = if current.is_finite() && current > 0.0 {
            current
        } else {
            PREVIEW_BOX_WIDTH
        };
        if next < min_w {
            next = min_w;
        } else if next > max_w {
            next = max_w;
        }
        if (next - current).abs() > f32::EPSILON {
            self.preview_width.set(next);
        }
        next
    }

    pub(crate) fn preview_box_metrics(width: f32, aspect: f32) -> (f32, f32, f32, f32) {
        let aspect = if aspect.is_finite() && aspect > 0.0 {
            aspect
        } else {
            1.0
        };
        let image_w = (width - PREVIEW_BOX_PADDING * 2.0).max(1.0);
        let image_h = (image_w * aspect).max(1.0);
        let box_h = image_h + PREVIEW_BOX_PADDING * 2.0;
        (width, box_h, image_w, image_h)
    }

    pub(crate) fn clamp_preview_position(
        &self,
        pos: [f32; 2],
        box_w: f32,
        box_h: f32,
        viewport_w: f32,
        viewport_h: f32,
    ) -> ([f32; 2], [bool; 2]) {
        let min_dim = box_w.min(box_h).max(1.0);
        let mut min_visible = (min_dim * PREVIEW_MIN_VISIBLE_FRAC).max(PREVIEW_MIN_VISIBLE);
        if min_visible > min_dim {
            min_visible = min_dim;
        }
        let min_x = -box_w + min_visible;
        let max_x = viewport_w - min_visible;
        let min_y = -box_h + min_visible;
        let max_y = viewport_h - min_visible;
        let mut x = pos[0];
        let mut y = pos[1];
        let mut hit_x = false;
        let mut hit_y = false;
        if x < min_x {
            x = min_x;
            hit_x = true;
        } else if x > max_x {
            x = max_x;
            hit_x = true;
        }
        if y < min_y {
            y = min_y;
            hit_y = true;
        } else if y > max_y {
            y = max_y;
            hit_y = true;
        }
        ([x, y], [hit_x, hit_y])
    }

    pub(crate) fn begin_preview_drag(
        &self,
        target: PreviewHoverTarget,
        local_x: f32,
        local_y: f32,
        pointer_id: Option<i32>,
    ) {
        let pos = self.preview_pos.get();
        let now = now_ms();
        let state = match target {
            PreviewHoverTarget::Resize(handle) => PreviewDragState::Resize {
                pointer_id,
                handle,
                start_pos: pos,
                start_width: self.preview_width.get(),
                last_ms: now,
            },
            PreviewHoverTarget::Body => PreviewDragState::Move {
                pointer_id,
                grab_offset: [local_x - pos[0], local_y - pos[1]],
                last_ms: now,
            },
            PreviewHoverTarget::None => return,
        };
        *self.preview_drag.borrow_mut() = Some(state);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now);
    }

    pub(crate) fn end_preview_drag(&self, pointer_id: Option<i32>) -> bool {
        let mut drag_ref = self.preview_drag.borrow_mut();
        let Some(drag) = drag_ref.as_ref() else {
            return false;
        };
        if let Some(id) = pointer_id {
            if drag.pointer_id() != Some(id) {
                return false;
            }
        }
        let was_resize = matches!(drag.kind(), PreviewDragKind::Resize);
        drag_ref.take();
        if was_resize {
            self.preview_velocity.set([0.0, 0.0]);
        }
        self.preview_motion_last_ms.set(now_ms());
        true
    }

    pub(crate) fn preview_unrotate_point(&self, center: [f32; 2], x: f32, y: f32) -> [f32; 2] {
        if self.tilt_deg.abs() <= f32::EPSILON {
            return [x, y];
        }
        let dx = x - center[0];
        let dy = y - center[1];
        let (rx, ry) = rotate_vec(dx, dy, -self.tilt_deg);
        [center[0] + rx, center[1] + ry]
    }

    pub(crate) fn preview_local_point(&self, layout: &PreviewLayout, x: f32, y: f32) -> [f32; 2] {
        self.preview_unrotate_point(layout.box_rect.center(), x, y)
    }

    pub(crate) fn preview_point_inside(&self, layout: &PreviewLayout, x: f32, y: f32) -> bool {
        let [ux, uy] = self.preview_local_point(layout, x, y);
        layout.box_rect.contains(ux, uy)
    }

    pub(crate) fn preview_hit_test(
        &self,
        layout: &PreviewLayout,
        x: f32,
        y: f32,
    ) -> PreviewHoverTarget {
        let [x, y] = self.preview_local_point(layout, x, y);
        if !layout.box_rect.contains(x, y) {
            return PreviewHoverTarget::None;
        }
        let border = PREVIEW_RESIZE_BORDER.max(1.0);
        let left = x - layout.box_rect.x <= border;
        let right = layout.box_rect.x + layout.box_rect.w - x <= border;
        let top = y - layout.box_rect.y <= border;
        let bottom = layout.box_rect.y + layout.box_rect.h - y <= border;
        if left || right || top || bottom {
            let handle = if left && top {
                PreviewResizeHandle::TopLeft
            } else if right && top {
                PreviewResizeHandle::TopRight
            } else if left && bottom {
                PreviewResizeHandle::BottomLeft
            } else if right && bottom {
                PreviewResizeHandle::BottomRight
            } else if left {
                PreviewResizeHandle::Left
            } else if right {
                PreviewResizeHandle::Right
            } else if top {
                PreviewResizeHandle::Top
            } else {
                PreviewResizeHandle::Bottom
            };
            return PreviewHoverTarget::Resize(handle);
        }
        PreviewHoverTarget::Body
    }

    pub(crate) fn update_debug_overlay(&self, snapshot: &AppSnapshot) {
        let overlay_ref = self.debug_overlay.borrow();
        let Some(overlay) = overlay_ref.as_ref() else {
            return;
        };
        if !snapshot.app_settings.show_debug {
            overlay.set_visible(false);
            return;
        }
        let Some(zones) = self.compute_auto_pan_zones(snapshot) else {
            overlay.set_visible(false);
            return;
        };
        overlay.set_visible(true);
        overlay.set_insets(zones.outer_inset, zones.inner_inset);
    }

    pub(crate) fn clear_auto_pan_frame(&self) {
        self.auto_pan.borrow_mut().frame.take();
    }

    pub(crate) fn stop_auto_pan(&self) {
        let mut state = self.auto_pan.borrow_mut();
        state.active = false;
        state.seen_inner = false;
        state.has_pointer = false;
        state.last_tick_ms = None;
        state.frame.take();
    }

    pub(crate) fn pick_piece(&self, x: f32, y: f32, snapshot: &AppSnapshot) -> Option<usize> {
        let assets = self.core.assets()?;
        let mask_atlas = self.mask_atlas.borrow();
        let mask_atlas = mask_atlas.as_ref()?;
        let positions = snapshot.piece_positions_px();
        let rotations = snapshot.piece_rotations_deg();
        pick_piece_at(
            x,
            y,
            &positions,
            &rotations,
            &snapshot.piece_flipped,
            &snapshot.z_order,
            mask_atlas,
            &assets.render_geometry,
            assets.mask_pad,
        )
    }

    pub(crate) fn update_preview_drag(
        &self,
        pointer_id: Option<i32>,
        local_x: f32,
        local_y: f32,
        now_ms: f32,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let mut width = self.preview_width.get().clamp(min_w, max_w);
        self.preview_width.set(width);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mut pos = self.preview_pos.get();
        let mut velocity = self.preview_velocity.get();
        let mut drag_ref = self.preview_drag.borrow_mut();
        let Some(drag) = drag_ref.as_mut() else {
            return false;
        };
        if drag.pointer_id() != pointer_id {
            return false;
        }
        match drag {
            PreviewDragState::Move {
                grab_offset,
                last_ms,
                ..
            } => {
                let half_w = box_w * 0.5;
                let half_h = box_h * 0.5;
                let vx = grab_offset[0] - half_w;
                let vy = grab_offset[1] - half_h;
                let (rvx, rvy) = rotate_vec(vx, vy, self.tilt_deg);
                let target_pos = [local_x - half_w - rvx, local_y - half_h - rvy];
                let (clamped, _) =
                    self.clamp_preview_position(target_pos, box_w, box_h, viewport_w, viewport_h);
                let dt = ((now_ms - *last_ms).max(0.0).min(50.0)) / 1000.0;
                if dt > 0.0 {
                    let inst_vx = (clamped[0] - pos[0]) / dt;
                    let inst_vy = (clamped[1] - pos[1]) / dt;
                    velocity[0] += (inst_vx - velocity[0]) * PREVIEW_VELOCITY_SMOOTH;
                    velocity[1] += (inst_vy - velocity[1]) * PREVIEW_VELOCITY_SMOOTH;
                }
                pos = clamped;
                *last_ms = now_ms;
            }
            PreviewDragState::Resize {
                handle,
                start_pos,
                start_width,
                last_ms,
                ..
            } => {
                let center = [pos[0] + box_w * 0.5, pos[1] + box_h * 0.5];
                let [local_x, local_y] = self.preview_unrotate_point(center, local_x, local_y);
                let (start_box_w, start_box_h, _, _) =
                    Self::preview_box_metrics(*start_width, aspect);
                let left = start_pos[0];
                let top = start_pos[1];
                let right = left + start_box_w;
                let bottom = top + start_box_h;
                let center_x = left + start_box_w * 0.5;
                let center_y = top + start_box_h * 0.5;
                let width_from_height = |height: f32| -> f32 {
                    if aspect > 0.0 {
                        (height - PREVIEW_BOX_PADDING * 2.0) / aspect + PREVIEW_BOX_PADDING * 2.0
                    } else {
                        height
                    }
                };
                let mut width_x = None;
                let mut width_y = None;
                match handle {
                    PreviewResizeHandle::Left => {
                        width_x = Some(right - local_x);
                    }
                    PreviewResizeHandle::Right => {
                        width_x = Some(local_x - left);
                    }
                    PreviewResizeHandle::Top => {
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::Bottom => {
                        width_y = Some(width_from_height(local_y - top));
                    }
                    PreviewResizeHandle::TopLeft => {
                        width_x = Some(right - local_x);
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::TopRight => {
                        width_x = Some(local_x - left);
                        width_y = Some(width_from_height(bottom - local_y));
                    }
                    PreviewResizeHandle::BottomLeft => {
                        width_x = Some(right - local_x);
                        width_y = Some(width_from_height(local_y - top));
                    }
                    PreviewResizeHandle::BottomRight => {
                        width_x = Some(local_x - left);
                        width_y = Some(width_from_height(local_y - top));
                    }
                }
                let mut candidate = match (width_x, width_y) {
                    (Some(wx), Some(wy)) => {
                        if (wx - *start_width).abs() >= (wy - *start_width).abs() {
                            wx
                        } else {
                            wy
                        }
                    }
                    (Some(wx), None) => wx,
                    (None, Some(wy)) => wy,
                    (None, None) => *start_width,
                };
                if !candidate.is_finite() {
                    candidate = *start_width;
                }
                width = candidate.clamp(min_w, max_w);
                self.preview_width.set(width);
                let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
                pos = match handle {
                    PreviewResizeHandle::Left => [right - box_w, center_y - box_h * 0.5],
                    PreviewResizeHandle::Right => [left, center_y - box_h * 0.5],
                    PreviewResizeHandle::Top => [center_x - box_w * 0.5, bottom - box_h],
                    PreviewResizeHandle::Bottom => [center_x - box_w * 0.5, top],
                    PreviewResizeHandle::TopLeft => [right - box_w, bottom - box_h],
                    PreviewResizeHandle::TopRight => [left, bottom - box_h],
                    PreviewResizeHandle::BottomLeft => [right - box_w, top],
                    PreviewResizeHandle::BottomRight => [left, top],
                };
                let (clamped, _) =
                    self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
                pos = clamped;
                velocity = [0.0, 0.0];
                *last_ms = now_ms;
            }
            PreviewDragState::Pinch { .. } => {
                return false;
            }
        }
        self.preview_pos.set(pos);
        self.preview_velocity.set(velocity);
        self.preview_motion_last_ms.set(now_ms);
        true
    }

    pub(crate) fn update_preview_motion(
        &self,
        now_ms: f32,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        if self.preview_drag.borrow().is_some() {
            self.preview_motion_last_ms.set(now_ms);
            return false;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mut velocity = self.preview_velocity.get();
        let speed_sq = velocity[0] * velocity[0] + velocity[1] * velocity[1];
        if speed_sq <= PREVIEW_VELOCITY_EPS * PREVIEW_VELOCITY_EPS {
            self.preview_velocity.set([0.0, 0.0]);
            self.preview_motion_last_ms.set(now_ms);
            return false;
        }
        let last = self.preview_motion_last_ms.get();
        let dt = if last > 0.0 {
            ((now_ms - last).max(0.0).min(50.0)) / 1000.0
        } else {
            0.0
        };
        self.preview_motion_last_ms.set(now_ms);
        if dt <= 0.0 {
            return true;
        }
        let mut pos = self.preview_pos.get();
        pos[0] += velocity[0] * dt;
        pos[1] += velocity[1] * dt;
        let decay = (-PREVIEW_MOMENTUM_DECAY * dt).exp();
        velocity[0] *= decay;
        velocity[1] *= decay;
        let (clamped, hit) = self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        if hit[0] {
            velocity[0] *= 0.2;
        }
        if hit[1] {
            velocity[1] *= 0.2;
        }
        pos = clamped;
        self.preview_pos.set(pos);
        self.preview_velocity.set(velocity);
        velocity[0].abs() > PREVIEW_VELOCITY_EPS || velocity[1].abs() > PREVIEW_VELOCITY_EPS
    }

    pub(crate) fn update_preview_pinch(
        &self,
        screen_a: [f32; 2],
        screen_b: [f32; 2],
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
        now_ms: f32,
        start_distance: f32,
        start_width: f32,
        anchor_rel: [f32; 2],
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let dx = screen_b[0] - screen_a[0];
        let dy = screen_b[1] - screen_a[1];
        let distance = (dx * dx + dy * dy).sqrt();
        if start_distance <= 0.0 || !distance.is_finite() {
            return false;
        }
        let scale = (distance / start_distance).max(0.01);
        let mut width = start_width * scale;
        if !width.is_finite() {
            width = start_width;
        }
        width = width.clamp(min_w, max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let mid_x = (screen_a[0] + screen_b[0]) * 0.5 - canvas_rect.left() as f32;
        let mid_y = (screen_a[1] + screen_b[1]) * 0.5 - canvas_rect.top() as f32;
        let half_w = box_w * 0.5;
        let half_h = box_h * 0.5;
        let vx = (anchor_rel[0] - 0.5) * box_w;
        let vy = (anchor_rel[1] - 0.5) * box_h;
        let (rvx, rvy) = rotate_vec(vx, vy, self.tilt_deg);
        let center_x = mid_x - rvx;
        let center_y = mid_y - rvy;
        let mut pos = [center_x - half_w, center_y - half_h];
        let (clamped, _) = self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        pos = clamped;
        self.preview_width.set(width);
        self.preview_pos.set(pos);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now_ms);
        true
    }

    pub(crate) fn begin_preview_pinch(
        &self,
        pointer_a: i32,
        pointer_b: i32,
        screen_a: [f32; 2],
        screen_b: [f32; 2],
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> bool {
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return false;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return false;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, _, _) = Self::preview_box_metrics(width, aspect);
        let pos = self.preview_pos.get();
        if box_w <= 0.0 || box_h <= 0.0 {
            return false;
        }
        let mid_x = (screen_a[0] + screen_b[0]) * 0.5 - canvas_rect.left() as f32;
        let mid_y = (screen_a[1] + screen_b[1]) * 0.5 - canvas_rect.top() as f32;
        let center = [pos[0] + box_w * 0.5, pos[1] + box_h * 0.5];
        let dx = mid_x - center[0];
        let dy = mid_y - center[1];
        let (ux, uy) = rotate_vec(dx, dy, -self.tilt_deg);
        let mut anchor_rel = [0.5 + ux / box_w, 0.5 + uy / box_h];
        anchor_rel[0] = anchor_rel[0].clamp(0.0, 1.0);
        anchor_rel[1] = anchor_rel[1].clamp(0.0, 1.0);
        let dx = screen_b[0] - screen_a[0];
        let dy = screen_b[1] - screen_a[1];
        let distance = (dx * dx + dy * dy).sqrt();
        if distance <= 0.0 || !distance.is_finite() {
            return false;
        }
        *self.preview_drag.borrow_mut() = Some(PreviewDragState::Pinch {
            touch_a: pointer_a,
            touch_b: pointer_b,
            start_distance: distance,
            start_width: width,
            anchor_rel,
        });
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_motion_last_ms.set(now_ms());
        true
    }

    pub(crate) fn compute_auto_pan_zones(&self, snapshot: &AppSnapshot) -> Option<AutoPanZones> {
        let rect = self.viewport.get_bounding_client_rect();
        let width = rect.width() as f32;
        let height = rect.height() as f32;
        if width <= 0.0 || height <= 0.0 {
            return None;
        }
        let min_dim = width.min(height);
        let max_inset = min_dim * 0.45;
        let outer_ratio = snapshot.view_settings.auto_pan_outer_ratio.max(0.0);
        let inner_ratio = snapshot.view_settings.auto_pan_inner_ratio.max(outer_ratio);
        let outer_inset = (outer_ratio * min_dim).min(max_inset);
        let inner_inset = (inner_ratio * min_dim).min(max_inset).max(outer_inset);
        let left = rect.left() as f32;
        let top = rect.top() as f32;
        let right = left + width;
        let bottom = top + height;
        let outer = AutoPanRect {
            left: left + outer_inset,
            right: right - outer_inset,
            top: top + outer_inset,
            bottom: bottom - outer_inset,
        };
        let inner = AutoPanRect {
            left: left + inner_inset,
            right: right - inner_inset,
            top: top + inner_inset,
            bottom: bottom - inner_inset,
        };
        Some(AutoPanZones {
            outer,
            inner,
            outer_inset,
            inner_inset,
        })
    }

    pub(crate) fn ensure_preview_seeded(
        &self,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
        aspect: f32,
    ) {
        if self.preview_seeded.get() {
            return;
        }
        let Some(info) = snapshot.puzzle_info.as_ref() else {
            return;
        };
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return;
        }
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let (min_w, max_w) = self.preview_width_bounds(aspect, viewport_w, viewport_h, frame_max_w);
        let base_dim = viewport_w.min(viewport_h).max(1.0);
        let mut box_w = (base_dim * PREVIEW_BOX_INIT_FRAC).max(PREVIEW_BOX_MIN_WIDTH);
        box_w = box_w
            .min(viewport_w * PREVIEW_BOX_MAX_FRAC)
            .max(min_w)
            .min(max_w);
        let (_, box_h, _, _) = Self::preview_box_metrics(box_w, aspect);
        let base_x = PREVIEW_BOX_MARGIN;
        let base_y = viewport_h - PREVIEW_BOX_MARGIN - box_h;
        self.preview_pos.set([base_x, base_y]);
        self.preview_width.set(box_w);
        self.preview_velocity.set([0.0, 0.0]);
        self.preview_seeded.set(true);
    }

    pub(crate) fn preview_layout(
        &self,
        snapshot: &AppSnapshot,
        canvas_rect: &web_sys::DomRect,
    ) -> Option<PreviewLayout> {
        let info = snapshot.puzzle_info.as_ref()?;
        let viewport_w = canvas_rect.width() as f32;
        let viewport_h = canvas_rect.height() as f32;
        if viewport_w <= 0.0 || viewport_h <= 0.0 {
            return None;
        }
        let aspect = Self::preview_aspect(info);
        self.ensure_preview_seeded(snapshot, canvas_rect, aspect);
        let frame_max_w =
            self.preview_frame_max_width(info, snapshot.layout, viewport_w, viewport_h);
        let width = self.clamp_preview_width(aspect, viewport_w, viewport_h, frame_max_w);
        let (box_w, box_h, image_w, image_h) = Self::preview_box_metrics(width, aspect);
        let mut pos = self.preview_pos.get();
        let (clamped, _) = self.clamp_preview_position(pos, box_w, box_h, viewport_w, viewport_h);
        if clamped != pos {
            self.preview_pos.set(clamped);
            pos = clamped;
        }
        let box_rect = Rect {
            x: pos[0],
            y: pos[1],
            w: box_w,
            h: box_h,
        };
        let image_rect = Rect {
            x: pos[0] + PREVIEW_BOX_PADDING,
            y: pos[1] + PREVIEW_BOX_PADDING,
            w: image_w,
            h: image_h,
        };
        Some(PreviewLayout {
            box_rect,
            image_rect,
        })
    }

    pub(crate) fn is_current_puzzle(&self, key: &PuzzleKey) -> bool {
        self.last_puzzle.borrow().as_ref() == Some(key)
    }
}

/// Renderer-specific behaviour the shared view logic calls back into. The
/// provided methods are the `Rc<Self>`-based flows (animation frames and
/// async loads) that need to hold the view alive across a callback.
pub(crate) trait ViewShellHost: 'static {
    fn shell(&self) -> &ViewShell;

    /// Schedules a redraw with the latest snapshot.
    fn request_render(self: &Rc<Self>);

    /// A private image resolved to an object URL for `key`; load and draw it.
    fn private_image_ready(self: &Rc<Self>, key: PuzzleKey, url: String);

    fn update_auto_pan(self: &Rc<Self>, screen_x: f32, screen_y: f32, snapshot: &AppSnapshot) {
        let zones = self.shell().compute_auto_pan_zones(snapshot);
        let mut should_start = false;
        let mut should_stop = false;
        {
            let mut state = self.shell().auto_pan.borrow_mut();
            state.has_pointer = true;
            state.last_screen_x = screen_x;
            state.last_screen_y = screen_y;
            if let Some(zones) = zones.as_ref() {
                let inside_outer = zones.outer.contains(screen_x, screen_y);
                let inside_inner = zones.inner.contains(screen_x, screen_y);
                if inside_inner {
                    state.seen_inner = true;
                }
                if state.active {
                    if inside_inner {
                        state.active = false;
                        state.last_tick_ms = None;
                        should_stop = true;
                    }
                } else if state.seen_inner && !inside_outer {
                    state.active = true;
                    state.last_tick_ms = None;
                    should_start = true;
                }
            } else if state.active {
                state.active = false;
                state.last_tick_ms = None;
                should_stop = true;
            }
        }
        if should_start {
            self.ensure_auto_pan_frame();
        } else if should_stop {
            self.shell().clear_auto_pan_frame();
        }
    }

    fn ensure_auto_pan_frame(self: &Rc<Self>) {
        let needs_frame = self.shell().auto_pan.borrow().frame.is_none();
        if !needs_frame {
            return;
        }
        let view = Rc::clone(self);
        let handle = request_animation_frame(move |timestamp| {
            view.auto_pan_frame(timestamp);
        });
        self.shell().auto_pan.borrow_mut().frame = Some(handle);
    }

    fn auto_pan_frame(self: &Rc<Self>, timestamp: f64) {
        if crate::crash::is_aborted() {
            return;
        }
        let (active, seen_inner, has_pointer, screen_x, screen_y, dt_sec) = {
            let mut state = self.shell().auto_pan.borrow_mut();
            state.frame.take();
            let dt_ms = match state.last_tick_ms {
                Some(prev) => (timestamp - prev).max(0.0),
                None => 0.0,
            };
            state.last_tick_ms = Some(timestamp);
            (
                state.active,
                state.seen_inner,
                state.has_pointer,
                state.last_screen_x,
                state.last_screen_y,
                (dt_ms.min(50.0) / 1000.0) as f32,
            )
        };
        if !active || !seen_inner || !has_pointer {
            return;
        }
        let snapshot = self.shell().core.snapshot();
        if snapshot.dragging_members.is_empty() {
            self.shell().stop_auto_pan();
            return;
        }
        let Some(zones) = self.shell().compute_auto_pan_zones(&snapshot) else {
            self.shell().stop_auto_pan();
            return;
        };
        let (dir_x, t_x) = edge_intensity(
            screen_x,
            zones.inner.left,
            zones.inner.right,
            zones.inner_inset,
        );
        let (dir_y, t_y) = edge_intensity(
            screen_y,
            zones.inner.top,
            zones.inner.bottom,
            zones.inner_inset,
        );
        let ease_x = smoothstep(t_x);
        let ease_y = smoothstep(t_y);
        let mut speed_ratio = snapshot.view_settings.auto_pan_speed_ratio.max(0.0);
        if let Some((view_x, view_y)) =
            screen_to_view_coords(screen_x, screen_y, &self.shell().viewport, snapshot.view)
        {
            speed_ratio *= workspace_fade_scale(view_x, view_y, snapshot.layout, snapshot.view);
        }
        let dx_world = dir_x * ease_x * snapshot.view.width * speed_ratio * dt_sec;
        let dy_world = dir_y * ease_y * snapshot.view.height * speed_ratio * dt_sec;
        if dx_world != 0.0 || dy_world != 0.0 {
            self.shell().core.pan_view(dx_world, dy_world);
            let snapshot = self.shell().core.snapshot();
            if snapshot.dragging_members.is_empty() {
                self.shell().stop_auto_pan();
                return;
            }
            if let Some((view_x, view_y)) =
                screen_to_view_coords(screen_x, screen_y, &self.shell().viewport, snapshot.view)
            {
                let (px, py) =
                    workspace_to_puzzle_coords(snapshot.layout.puzzle_scale, view_x, view_y);
                self.shell()
                    .dispatch_action(CoreAction::DragMove { x: px, y: py });
            }
        }
        if self.shell().auto_pan.borrow().active {
            self.ensure_auto_pan_frame();
        }
    }

    fn ensure_private_image_src(self: &Rc<Self>, hash: &str, key: PuzzleKey) -> Option<String> {
        if self.shell().private_image_hash.borrow().as_deref() == Some(hash) {
            if let Some(url) = self.shell().private_image_url.borrow().as_ref() {
                return Some(url.clone());
            }
        }
        if self.shell().private_image_loading.get() {
            return None;
        }
        self.shell().private_image_loading.set(true);
        let hash_value = hash.to_string();
        let view = Rc::clone(self);
        spawn_local(async move {
            let entry = match persisted_store::load_private_image(&hash_value).await {
                Ok(Some(entry)) => entry,
                Ok(None) => {
                    view.shell()
                        .note_private_image_error(&hash_value, "Private image is missing.");
                    return;
                }
                Err(message) => {
                    view.shell().note_private_image_error(&hash_value, &message);
                    return;
                }
            };
            let url = match create_object_url(&entry.bytes, &entry.mime) {
                Ok(url) => url,
                Err(_) => {
                    view.shell()
                        .note_private_image_error(&hash_value, "Failed to prepare private image.");
                    return;
                }
            };
            if !view.shell().is_current_puzzle(&key) {
                revoke_object_url(&url);
                view.shell().private_image_loading.set(false);
                return;
            }
            *view.shell().private_image_url.borrow_mut() = Some(url.clone());
            *view.shell().private_image_hash.borrow_mut() = Some(hash_value.clone());
            view.shell().private_image_loading.set(false);
            let mut updated = entry;
            updated.last_used_at = now_ms_u64();
            let _ = persisted_store::save_private_image(&hash_value, updated).await;
            view.private_image_ready(key, url);
        });
        None
    }

    /// A first-time room joiner misses the local store while the image is still
    /// downloading. `ensure_puzzle_image` only runs once per puzzle, so without
    /// this retry the image never loads and the boot shell never clears.
    fn retry_private_image(self: &Rc<Self>, hash: &str) {
        let missed = self.shell().private_image_error.borrow().as_deref() == Some(hash);
        let key = self.shell().last_puzzle.borrow().clone();
        if let (true, Some(key)) = (missed, key) {
            self.shell().private_image_error.borrow_mut().take();
            self.ensure_private_image_src(hash, key);
        }
        self.request_render();
    }
}

impl<V: ViewShellHost> ActiveView for Rc<V> {
    fn request_render(&self) {
        ViewShellHost::request_render(self);
    }

    fn private_image_stored(&self, hash: &str) {
        self.retry_private_image(hash);
    }
}
