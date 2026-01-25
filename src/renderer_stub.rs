use crate::core::{GridChoice, Piece, PiecePaths};
use std::rc::Rc;
use wasm_bindgen::JsValue;
use web_sys::{HtmlCanvasElement, HtmlImageElement};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum UiTextId {
    Title,
    Progress,
    Credit,
    Success,
    MenuTitle,
    MenuSubtitle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum UiRotationOrigin {
    Center,
    BottomLeft,
    BottomRight,
    TopLeft,
    TopRight,
}

#[derive(Clone, Debug)]
pub(crate) struct UiTextSpec {
    pub(crate) id: UiTextId,
    pub(crate) text: String,
    pub(crate) pos: [f32; 2],
    pub(crate) rotation_deg: f32,
    pub(crate) rotation_origin: UiRotationOrigin,
    pub(crate) rotation_offset: [f32; 2],
    pub(crate) font_size: f32,
    pub(crate) line_height: f32,
    pub(crate) color: [u8; 4],
}

#[derive(Clone, Copy)]
pub(crate) struct Instance {
    pub(crate) pos: [f32; 2],
    pub(crate) size: [f32; 2],
    pub(crate) rotation: f32,
    pub(crate) flip: f32,
    pub(crate) hover: f32,
    pub(crate) drag: f32,
    pub(crate) piece_origin: [f32; 2],
    pub(crate) mask_origin: [f32; 2],
}

#[derive(Clone, Copy)]
pub(crate) struct InstanceBatch {
    pub(crate) start: u32,
    pub(crate) count: u32,
    pub(crate) draw_outline: bool,
}

pub(crate) struct InstanceSet {
    pub(crate) instances: Vec<Instance>,
    pub(crate) batches: Vec<InstanceBatch>,
}

pub(crate) struct MaskAtlasData {
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) pixels: Vec<u8>,
    pub(crate) origins: Vec<[f32; 2]>,
}

pub(crate) fn build_mask_atlas(
    pieces: &[Piece],
    _paths: &[PiecePaths],
    _piece_width: f32,
    _piece_height: f32,
    _grid: GridChoice,
    _padding: f32,
) -> Result<MaskAtlasData, JsValue> {
    Ok(MaskAtlasData {
        width: 0,
        height: 0,
        pixels: Vec::new(),
        origins: vec![[0.0, 0.0]; pieces.len()],
    })
}

pub(crate) struct WgpuRenderer;

fn touch_rotation_origin_variants() {
    let _ = UiRotationOrigin::BottomRight;
    let _ = UiRotationOrigin::TopRight;
}

impl WgpuRenderer {
    pub(crate) async fn new(
        _canvas: HtmlCanvasElement,
        _image: HtmlImageElement,
        _pieces: Vec<Piece>,
        _paths: Vec<PiecePaths>,
        _grid: GridChoice,
        _piece_width: f32,
        _piece_height: f32,
        _view_min_x: f32,
        _view_min_y: f32,
        _view_width: f32,
        _view_height: f32,
        _puzzle_scale: f32,
        _mask_atlas: Rc<MaskAtlasData>,
        _mask_pad: f32,
        _render_scale: f32,
        _is_dark_theme: bool,
    ) -> Result<Self, JsValue> {
        Err(JsValue::from_str(
            "wgpu renderer is only supported on wasm32 targets",
        ))
    }

    pub(crate) fn set_emboss_enabled(&mut self, _enabled: bool) {}

    pub(crate) fn set_font_bytes(&mut self, _bytes: Vec<u8>) {}

    pub(crate) fn set_ui_font_bytes(&mut self, _bytes: Vec<u8>) {}

    pub(crate) fn set_edge_aa(&mut self, _value: f32) {}

    pub(crate) fn set_solved(&mut self, _value: bool) {}

    pub(crate) fn update_instances(&mut self, instances: InstanceSet) {
        let _ = instances.batches.len();
        for batch in &instances.batches {
            let _ = (batch.start, batch.count, batch.draw_outline);
        }
        for inst in &instances.instances {
            let _ = (
                inst.pos,
                inst.size,
                inst.rotation,
                inst.flip,
                inst.hover,
                inst.drag,
                inst.piece_origin,
                inst.mask_origin,
            );
        }
    }

    pub(crate) fn set_show_fps(&mut self, _value: bool) {}

    pub(crate) fn set_ui_texts(&mut self, specs: &[UiTextSpec]) {
        touch_rotation_origin_variants();
        if let Some(spec) = specs.first() {
            let _ = (
                spec.id,
                &spec.text,
                spec.pos,
                spec.rotation_deg,
                spec.rotation_origin,
                spec.rotation_offset,
                spec.font_size,
                spec.line_height,
                spec.color,
            );
        }
    }

    pub(crate) fn set_show_frame(&mut self, _value: bool) {}

    pub(crate) fn render(&mut self) {}
}
