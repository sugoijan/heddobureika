//! Grid-specific jigsaw shaping implementation.

use std::f32::consts::PI;

use crate::edge_compose::{
    append_mapped_local_line_points as compose_append_mapped_local_line_points,
    connection_point_for_side as compose_connection_point_for_side,
    map_segments_to_path as compose_map_segments_to_path, reverse_connector_segments, ConnectorSeg,
};
use crate::edge_host::{
    corner_arc_points, frame_outline_points, warp_point, warp_points, BorderFrameShape,
    EdgeOrientation, HostEdgeFrame, LineWave, WarpField,
};
use crate::edge_profile::{
    build_edge_profile_segments, ConnectorShape, EdgeProfileInput, EdgeShapeStyle,
};
use crate::ids::{BorderEdgeId, EdgeId, FrameEdgeId, PieceId};
use crate::shape::{
    BorderEdgeGeometryMm, EdgeSide, EdgeSideGeometryMm, FrameGeometryMm, GeometryInvariantError,
    InteriorEdgeGeometryMm, PathMm, PathSegMm, PieceEdgeRef, PieceGeometryMm,
    PieceGeometryProvider, PointMm, ShapeAtlasMm, TopologyShaper,
};
use crate::topology::{GridTopology, PuzzleTopology};
use crate::units::LengthMm;

const MAX_LINE_BEND_RATIO: f32 = 0.2;
const TAB_WIDTH_MIN: f32 = 0.2;
const TAB_WIDTH_MAX: f32 = 0.72;
const TAB_WIDTH_RANGE: f32 = 0.16;
const TAB_DEPTH_MIN: f32 = 0.2;
const TAB_DEPTH_MAX: f32 = 1.1;
const TAB_DEPTH_RANGE: f32 = 0.35;
const TAB_SIZE_SCALE_MIN: f32 = 0.1;
const TAB_SIZE_SCALE_MAX: f32 = 0.5;
const TAB_SIZE_MIN_LIMIT: f32 = 0.02;
const TAB_SIZE_MAX_LIMIT: f32 = 0.24;
const JITTER_STRENGTH_MIN: f32 = 0.0;
const JITTER_STRENGTH_MAX: f32 = 0.3;
const JITTER_LEN_BIAS_MIN: f32 = 0.0;
const JITTER_LEN_BIAS_MAX: f32 = 1.0;
const TAB_DEPTH_CAP_MIN: f32 = 0.2;
const TAB_DEPTH_CAP_MAX: f32 = 0.45;
const CURVE_DETAIL_MIN: f32 = 0.5;
const CURVE_DETAIL_MAX: f32 = 3.0;
const SKEW_RANGE_MAX: f32 = 0.2;
const VARIATION_MIN: f32 = 0.0;
const VARIATION_MAX: f32 = 1.0;
const LINE_BEND_MIN: f32 = 0.0;
const CORNER_RADIUS_RATIO: f32 = 0.05;

const SIDE_TOP: usize = 0;
const SIDE_RIGHT: usize = 1;
const SIDE_BOTTOM: usize = 2;
const SIDE_LEFT: usize = 3;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GridShapeSettings {
    pub tab_width: f32,
    pub tab_depth: f32,
    pub tab_size_scale: f32,
    pub tab_size_min: f32,
    pub tab_size_max: f32,
    pub jitter_strength: f32,
    pub jitter_len_bias: f32,
    pub tab_depth_cap: f32,
    pub curve_detail: f32,
    pub skew_range: f32,
    pub variation: f32,
    pub line_bend_ratio: f32,
    /// Connector profile style for interior tab/blank edges.
    pub edge_style: EdgeShapeStyle,
    /// Host-border frame shape used by host-warp previews.
    ///
    /// Piece generation remains topology-bound in this iteration.
    pub border_shape: BorderFrameShape,
    /// Frame rotation (degrees) used for polygon/circle parameterization.
    pub border_rotation_deg: f32,
    /// Inset from puzzle bounds in millimeters.
    pub border_inset_mm: f32,
}

impl Default for GridShapeSettings {
    fn default() -> Self {
        Self {
            tab_width: 0.43,
            tab_depth: 0.98,
            tab_size_scale: 0.25,
            tab_size_min: 0.04,
            tab_size_max: 0.16,
            jitter_strength: 0.13,
            jitter_len_bias: 0.4,
            tab_depth_cap: 0.32,
            curve_detail: 1.4,
            skew_range: 0.18,
            variation: 0.16,
            line_bend_ratio: 0.06,
            edge_style: EdgeShapeStyle::Classic,
            border_shape: BorderFrameShape::Rectangle,
            border_rotation_deg: 0.0,
            border_inset_mm: 0.0,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GridJigsawShaper;

#[derive(Clone, Debug, PartialEq)]
pub struct GridShapeCache {
    pub piece_width: LengthMm,
    pub piece_height: LengthMm,
    pub mask_pad: LengthMm,
    pub atlas: ShapeAtlasMm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GridShapeBuildError {
    Invariant(GeometryInvariantError),
    TopologyMismatch,
    InternalConstruction,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GridHostShapePreview {
    pub width: LengthMm,
    pub height: LengthMm,
    pub border_outline: PathMm,
    pub horizontal_lines: Box<[PathMm]>,
    pub vertical_lines: Box<[PathMm]>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GridEdgeProfileSample {
    pub edge_id: EdgeId,
    pub row: u32,
    pub col: u32,
    pub sign: i8,
    pub path: PathMm,
    pub connection_point: PointMm,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GridEdgeProfilePreview {
    pub horizontal: Box<[GridEdgeProfileSample]>,
    pub vertical: Box<[GridEdgeProfileSample]>,
}

impl PieceGeometryProvider for GridShapeCache {
    fn piece_count(&self) -> u32 {
        self.atlas.pieces.len() as u32
    }

    fn piece_geometry(&self, piece: PieceId) -> &PieceGeometryMm {
        self.atlas
            .pieces
            .get(piece.as_usize())
            .expect("piece id should be valid for grid shape cache")
    }

    fn interior_edge_geometry(&self, edge: EdgeId) -> &InteriorEdgeGeometryMm {
        self.atlas
            .interior_edges
            .get(edge.as_usize())
            .expect("interior edge id should be valid for grid shape cache")
    }

    fn border_edge_geometry(&self, edge: BorderEdgeId) -> &BorderEdgeGeometryMm {
        self.atlas
            .border_edges
            .get(edge.as_usize())
            .expect("border edge id should be valid for grid shape cache")
    }

    fn frame_geometry(&self) -> &FrameGeometryMm {
        &self.atlas.frame
    }

    fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm {
        self.atlas.piece_edge_geometry(piece, edge_index)
    }
}

impl TopologyShaper<GridTopology> for GridJigsawShaper {
    type Settings = GridShapeSettings;
    type Cache = GridShapeCache;
    type Error = GridShapeBuildError;

    fn build_cache(
        &self,
        topology: &GridTopology,
        piece_width: LengthMm,
        piece_height: LengthMm,
        seed: u32,
        settings: &Self::Settings,
    ) -> Result<Self::Cache, Self::Error> {
        let cols = topology.cols().get();
        let rows = topology.rows().get();
        let piece_w = piece_width.as_mm_f32();
        let piece_h = piece_height.as_mm_f32();

        let (horizontal, vertical) = build_edge_maps(rows, cols, seed, settings);
        let (horizontal_waves, vertical_waves) =
            build_line_waves(rows, cols, seed, piece_w, piece_h, settings.line_bend_ratio);
        let warp = WarpField {
            width: piece_w * cols as f32,
            height: piece_h * rows as f32,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };

        let mut piece_sides = Vec::with_capacity(topology.piece_count() as usize);
        for row in 0..rows {
            for col in 0..cols {
                piece_sides.push(build_piece_sides(
                    topology,
                    row,
                    col,
                    piece_w,
                    piece_h,
                    &horizontal,
                    &vertical,
                    &warp,
                    settings,
                ));
            }
        }
        let atlas = build_shape_atlas(topology, rows, cols, piece_sides)?;

        let depth_cap = settings
            .tab_depth_cap
            .clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
        let max_depth = piece_w.max(piece_h) * depth_cap;
        let max_bend = horizontal_waves
            .iter()
            .chain(vertical_waves.iter())
            .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
        let mask_pad = LengthMm::try_new((max_depth + max_bend).ceil()).unwrap_or_default();

        Ok(GridShapeCache {
            piece_width,
            piece_height,
            mask_pad,
            atlas,
        })
    }
}

impl GridJigsawShaper {
    /// Builds a debug preview of host-grid shaping (warp/bend only, no connectors).
    pub fn build_host_shape_preview(
        &self,
        topology: &GridTopology,
        piece_width: LengthMm,
        piece_height: LengthMm,
        seed: u32,
        settings: &GridShapeSettings,
    ) -> GridHostShapePreview {
        let cols = topology.cols().get();
        let rows = topology.rows().get();
        let piece_w = piece_width.as_mm_f32();
        let piece_h = piece_height.as_mm_f32();

        let (horizontal_waves, vertical_waves) =
            build_line_waves(rows, cols, seed, piece_w, piece_h, settings.line_bend_ratio);
        let warp = WarpField {
            width: piece_w * cols as f32,
            height: piece_h * rows as f32,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };

        let h_steps = host_line_steps(cols);
        let v_steps = host_line_steps(rows);
        let frame_steps = host_frame_steps(rows, cols, settings.border_shape);

        let mut horizontal_lines = Vec::with_capacity((rows + 1) as usize);
        for row in 0..=rows {
            let y = row as f32 * piece_h;
            horizontal_lines.push(sample_warped_line(
                (0.0, y),
                (cols as f32 * piece_w, y),
                &warp,
                h_steps,
            ));
        }

        let mut vertical_lines = Vec::with_capacity((cols + 1) as usize);
        for col in 0..=cols {
            let x = col as f32 * piece_w;
            vertical_lines.push(sample_warped_line(
                (x, 0.0),
                (x, rows as f32 * piece_h),
                &warp,
                v_steps,
            ));
        }

        let border_points = frame_outline_points(
            piece_w * cols as f32,
            piece_h * rows as f32,
            settings.border_shape,
            settings.border_rotation_deg,
            settings.border_inset_mm,
            frame_steps,
        );
        let border_warped = warp_points(&border_points, &warp);
        let border_outline = path_from_points_closed(&border_warped);

        GridHostShapePreview {
            width: LengthMm::try_new(piece_w * cols as f32).unwrap_or_default(),
            height: LengthMm::try_new(piece_h * rows as f32).unwrap_or_default(),
            border_outline,
            horizontal_lines: horizontal_lines.into_boxed_slice(),
            vertical_lines: vertical_lines.into_boxed_slice(),
        }
    }

    /// Builds a debug preview of connector-only edge profiles in local edge coordinates.
    pub fn build_edge_profile_preview(
        &self,
        topology: &GridTopology,
        piece_width: LengthMm,
        piece_height: LengthMm,
        seed: u32,
        settings: &GridShapeSettings,
    ) -> GridEdgeProfilePreview {
        let cols = topology.cols().get();
        let rows = topology.rows().get();
        let piece_w = piece_width.as_mm_f32();
        let piece_h = piece_height.as_mm_f32();
        let horizontal_count = rows * cols.saturating_sub(1);

        let (horizontal, vertical) = build_edge_maps(rows, cols, seed, settings);
        let flat_waves = [LineWave {
            amplitude: 0.0,
            skew: 0.0,
        }];
        let flat_warp = WarpField {
            width: 1.0,
            height: 1.0,
            horizontal: &flat_waves,
            vertical: &flat_waves,
        };
        let frame = HostEdgeFrame {
            orientation: EdgeOrientation::Bottom,
            origin: (0.0, 0.0),
            offset: (0.0, 0.0),
        };

        let mut horizontal_samples = Vec::with_capacity((rows.saturating_sub(1) * cols) as usize);
        for row in 1..rows {
            for col in 0..cols {
                let Some(edge) = horizontal[row as usize][col as usize].as_ref() else {
                    continue;
                };
                let sign = edge.tab_side.sign();
                let segments = edge_curve_segments(
                    piece_w,
                    piece_h,
                    Some(edge),
                    sign,
                    settings.tab_depth_cap,
                    settings.edge_style,
                );
                let path = compose_map_segments_to_path((0.0, 0.0), &segments, frame, &flat_warp);
                let cp = compose_connection_point_for_side(
                    (0.0, 0.0),
                    &segments,
                    piece_w,
                    piece_h,
                    frame,
                    &flat_warp,
                );

                horizontal_samples.push(GridEdgeProfileSample {
                    edge_id: EdgeId(horizontal_count + (row - 1) * cols + col),
                    row,
                    col,
                    sign,
                    path,
                    connection_point: point_mm(cp.0, cp.1),
                });
            }
        }

        let mut vertical_samples = Vec::with_capacity((rows * cols.saturating_sub(1)) as usize);
        for row in 0..rows {
            for col in 1..cols {
                let Some(edge) = vertical[row as usize][col as usize].as_ref() else {
                    continue;
                };
                let sign = edge.tab_side.sign();
                let segments = edge_curve_segments(
                    piece_h,
                    piece_w,
                    Some(edge),
                    sign,
                    settings.tab_depth_cap,
                    settings.edge_style,
                );
                let path = compose_map_segments_to_path((0.0, 0.0), &segments, frame, &flat_warp);
                let cp = compose_connection_point_for_side(
                    (0.0, 0.0),
                    &segments,
                    piece_h,
                    piece_w,
                    frame,
                    &flat_warp,
                );

                vertical_samples.push(GridEdgeProfileSample {
                    edge_id: EdgeId(row * cols.saturating_sub(1) + (col - 1)),
                    row,
                    col,
                    sign,
                    path,
                    connection_point: point_mm(cp.0, cp.1),
                });
            }
        }

        GridEdgeProfilePreview {
            horizontal: horizontal_samples.into_boxed_slice(),
            vertical: vertical_samples.into_boxed_slice(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct GridPieceSides {
    sides: [EdgeSideGeometryMm; 4],
    topology_edges: [Option<EdgeId>; 4],
}

#[derive(Clone, Debug)]
struct InteriorEdgeSlots {
    endpoints: (PieceId, PieceId),
    side_a: Option<EdgeSideGeometryMm>,
    side_b: Option<EdgeSideGeometryMm>,
}

fn build_shape_atlas(
    topology: &GridTopology,
    rows: u32,
    cols: u32,
    piece_sides: Vec<GridPieceSides>,
) -> Result<ShapeAtlasMm, GridShapeBuildError> {
    if piece_sides.len() != topology.piece_count() as usize {
        return Err(GridShapeBuildError::TopologyMismatch);
    }

    let mut interior_slots = (0..topology.edge_count())
        .map(|edge| InteriorEdgeSlots {
            endpoints: topology.edge_endpoints(EdgeId(edge)),
            side_a: None,
            side_b: None,
        })
        .collect::<Vec<_>>();

    let frame_count = (2 * (rows + cols)) as usize;
    let mut frame_edges = vec![None; frame_count];
    let mut border_edges = vec![None; frame_count];
    let mut pieces = Vec::with_capacity(piece_sides.len());

    for (piece_idx, piece_side) in piece_sides.into_iter().enumerate() {
        let piece_id = PieceId(piece_idx as u32);
        let row = piece_idx as u32 / cols;
        let col = piece_idx as u32 % cols;
        let mut edge_refs = Vec::with_capacity(piece_side.sides.len());

        for (side_idx, side_geometry) in piece_side.sides.into_iter().enumerate() {
            let maybe_topology_edge = piece_side.topology_edges[side_idx];
            if let Some(topology_edge) = maybe_topology_edge {
                let edge_idx = topology_edge.as_usize();
                let slot = interior_slots
                    .get_mut(edge_idx)
                    .ok_or(GridShapeBuildError::TopologyMismatch)?;

                let side = if slot.endpoints.0 == piece_id {
                    EdgeSide::A
                } else if slot.endpoints.1 == piece_id {
                    EdgeSide::B
                } else {
                    return Err(GridShapeBuildError::TopologyMismatch);
                };

                match side {
                    EdgeSide::A => {
                        if slot.side_a.replace(side_geometry).is_some() {
                            return Err(GridShapeBuildError::InternalConstruction);
                        }
                    }
                    EdgeSide::B => {
                        if slot.side_b.replace(side_geometry).is_some() {
                            return Err(GridShapeBuildError::InternalConstruction);
                        }
                    }
                }
                edge_refs.push(PieceEdgeRef::Interior {
                    edge: topology_edge,
                    side,
                });
            } else {
                let frame_edge = grid_boundary_frame_edge(rows, cols, row, col, side_idx)
                    .ok_or(GridShapeBuildError::InternalConstruction)?;
                let frame_idx = frame_edge.as_usize();
                let border_edge = BorderEdgeId(frame_edge.as_u32());
                let border_idx = border_edge.as_usize();

                if frame_edges.get(frame_idx).is_none() || border_edges.get(border_idx).is_none() {
                    return Err(GridShapeBuildError::InternalConstruction);
                }
                if frame_edges[frame_idx].is_some() || border_edges[border_idx].is_some() {
                    return Err(GridShapeBuildError::InternalConstruction);
                }

                frame_edges[frame_idx] = Some(side_geometry.path.clone());
                border_edges[border_idx] = Some(BorderEdgeGeometryMm {
                    piece: piece_id,
                    side: side_geometry,
                    frame_edge,
                });
                edge_refs.push(PieceEdgeRef::Border { edge: border_edge });
            }
        }

        pieces.push(PieceGeometryMm {
            edges: edge_refs.into_boxed_slice(),
        });
    }

    let mut interior_edges = Vec::with_capacity(interior_slots.len());
    for slot in interior_slots {
        let side_a = slot
            .side_a
            .ok_or(GridShapeBuildError::InternalConstruction)?;
        let side_b = slot
            .side_b
            .ok_or(GridShapeBuildError::InternalConstruction)?;
        interior_edges.push(InteriorEdgeGeometryMm {
            endpoints: slot.endpoints,
            side_a,
            side_b,
        });
    }

    let frame_edges = frame_edges
        .into_iter()
        .map(|edge| edge.ok_or(GridShapeBuildError::InternalConstruction))
        .collect::<Result<Vec<_>, _>>()?;
    let border_edges = border_edges
        .into_iter()
        .map(|edge| edge.ok_or(GridShapeBuildError::InternalConstruction))
        .collect::<Result<Vec<_>, _>>()?;

    let atlas = ShapeAtlasMm {
        pieces: pieces.into_boxed_slice(),
        interior_edges: interior_edges.into_boxed_slice(),
        border_edges: border_edges.into_boxed_slice(),
        frame: FrameGeometryMm {
            edges: frame_edges.into_boxed_slice(),
        },
    };
    atlas
        .validate(topology)
        .map_err(GridShapeBuildError::Invariant)?;
    Ok(atlas)
}

fn build_piece_sides(
    topology: &GridTopology,
    row: u32,
    col: u32,
    piece_width: f32,
    piece_height: f32,
    horizontal: &[Vec<Option<Edge>>],
    vertical: &[Vec<Option<Edge>>],
    warp: &WarpField<'_>,
    settings: &GridShapeSettings,
) -> GridPieceSides {
    let row_usize = row as usize;
    let col_usize = col as usize;
    let rows = horizontal.len().saturating_sub(1);
    let cols = horizontal.first().map(|line| line.len()).unwrap_or(0);

    let top_edge = horizontal[row_usize][col_usize].as_ref();
    let bottom_edge = horizontal[row_usize + 1][col_usize].as_ref();
    let left_edge = vertical[row_usize][col_usize].as_ref();
    let right_edge = vertical[row_usize][col_usize + 1].as_ref();

    let top_sign = top_edge.map(|edge| -edge.tab_side.sign()).unwrap_or(0);
    let right_sign = right_edge.map(|edge| edge.tab_side.sign()).unwrap_or(0);
    let bottom_sign = bottom_edge.map(|edge| edge.tab_side.sign()).unwrap_or(0);
    let left_sign = left_edge.map(|edge| -edge.tab_side.sign()).unwrap_or(0);

    let is_top = row_usize == 0;
    let is_left = col_usize == 0;
    let is_bottom = row_usize + 1 == rows;
    let is_right = col_usize + 1 == cols;

    let mut corner_radius = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
    let max_corner = piece_width.min(piece_height) * 0.45;
    if corner_radius > max_corner {
        corner_radius = max_corner;
    }

    let round_tl = is_top && is_left && corner_radius > 0.0;
    let round_tr = is_top && is_right && corner_radius > 0.0;
    let round_br = is_bottom && is_right && corner_radius > 0.0;
    let round_bl = is_bottom && is_left && corner_radius > 0.0;

    let top_start_trim = if round_tl { corner_radius } else { 0.0 };
    let top_end_trim = if round_tr { corner_radius } else { 0.0 };
    let right_start_trim = if round_tr { corner_radius } else { 0.0 };
    let right_end_trim = if round_br { corner_radius } else { 0.0 };
    let bottom_start_trim = if round_br { corner_radius } else { 0.0 };
    let bottom_end_trim = if round_bl { corner_radius } else { 0.0 };
    let left_start_trim = if round_bl { corner_radius } else { 0.0 };
    let left_end_trim = if round_tl { corner_radius } else { 0.0 };

    let top_is_boundary = top_edge.is_none();
    let right_is_boundary = right_edge.is_none();
    let bottom_is_boundary = bottom_edge.is_none();
    let left_is_boundary = left_edge.is_none();

    let top_start = if top_is_boundary {
        (top_start_trim, 0.0)
    } else {
        (0.0, 0.0)
    };
    let right_start = if right_is_boundary {
        (right_start_trim, 0.0)
    } else {
        (0.0, 0.0)
    };
    let bottom_start = if bottom_is_boundary {
        (piece_width - bottom_start_trim, 0.0)
    } else {
        (piece_width, 0.0)
    };
    let left_start = if left_is_boundary {
        (piece_height - left_start_trim, 0.0)
    } else {
        (piece_height, 0.0)
    };

    let top_segments = if top_is_boundary {
        vec![ConnectorSeg::LineTo {
            to: (piece_width - top_end_trim, 0.0),
        }]
    } else {
        edge_curve_segments(
            piece_width,
            piece_height,
            top_edge,
            top_sign,
            settings.tab_depth_cap,
            settings.edge_style,
        )
    };

    let right_segments = if right_is_boundary {
        vec![ConnectorSeg::LineTo {
            to: (piece_height - right_end_trim, 0.0),
        }]
    } else {
        edge_curve_segments(
            piece_height,
            piece_width,
            right_edge,
            right_sign,
            settings.tab_depth_cap,
            settings.edge_style,
        )
    };

    let bottom_segments = if bottom_is_boundary {
        vec![ConnectorSeg::LineTo {
            to: (bottom_end_trim, 0.0),
        }]
    } else {
        reverse_side_segments(&edge_curve_segments(
            piece_width,
            piece_height,
            bottom_edge,
            bottom_sign,
            settings.tab_depth_cap,
            settings.edge_style,
        ))
    };

    let left_segments = if left_is_boundary {
        vec![ConnectorSeg::LineTo {
            to: (left_end_trim, 0.0),
        }]
    } else {
        reverse_side_segments(&edge_curve_segments(
            piece_height,
            piece_width,
            left_edge,
            left_sign,
            settings.tab_depth_cap,
            settings.edge_style,
        ))
    };

    let offset = (col as f32 * piece_width, row as f32 * piece_height);
    let arc_steps = ((settings
        .curve_detail
        .clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX)
        * 6.0)
        .round() as usize)
        .clamp(4, 24);

    let top_origin = (0.0, 0.0);
    let right_origin = (piece_width, 0.0);
    let bottom_origin = (0.0, piece_height);
    let left_origin = (0.0, 0.0);

    let mut top_path = map_side_path(
        top_start,
        &top_segments,
        EdgeOrientation::Top,
        top_origin,
        offset,
        warp,
    );
    let mut right_path = map_side_path(
        right_start,
        &right_segments,
        EdgeOrientation::Right,
        right_origin,
        offset,
        warp,
    );
    let mut bottom_path = map_side_path(
        bottom_start,
        &bottom_segments,
        EdgeOrientation::Bottom,
        bottom_origin,
        offset,
        warp,
    );
    let mut left_path = map_side_path(
        left_start,
        &left_segments,
        EdgeOrientation::Left,
        left_origin,
        offset,
        warp,
    );

    if round_tr {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            corner_radius,
            corner_radius,
            1.5 * PI,
            2.0 * PI,
            arc_steps,
        );
        append_local_arc_lines(&mut top_path, offset, warp, &arc);
    }

    if round_br {
        let arc = corner_arc_points(
            piece_width - corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.0,
            0.5 * PI,
            arc_steps,
        );
        append_local_arc_lines(&mut right_path, offset, warp, &arc);
    }

    if round_bl {
        let arc = corner_arc_points(
            corner_radius,
            piece_height - corner_radius,
            corner_radius,
            0.5 * PI,
            PI,
            arc_steps,
        );
        append_local_arc_lines(&mut bottom_path, offset, warp, &arc);
    }

    if round_tl {
        let arc = corner_arc_points(
            corner_radius,
            corner_radius,
            corner_radius,
            PI,
            1.5 * PI,
            arc_steps,
        );
        append_local_arc_lines(&mut left_path, offset, warp, &arc);
    }

    let mut stitched_paths = [top_path, right_path, bottom_path, left_path];
    stitch_piece_ring(&mut stitched_paths);
    let [top_path, right_path, bottom_path, left_path] = stitched_paths;

    let top_geometry = EdgeSideGeometryMm {
        path: top_path,
        connection_point: {
            let p = connection_point_for_side(
                top_start,
                &top_segments,
                piece_width,
                piece_height,
                EdgeOrientation::Top,
                top_origin,
                offset,
                warp,
            );
            point_mm(p.0, p.1)
        },
    };
    let right_geometry = EdgeSideGeometryMm {
        path: right_path,
        connection_point: {
            let p = connection_point_for_side(
                right_start,
                &right_segments,
                piece_height,
                piece_width,
                EdgeOrientation::Right,
                right_origin,
                offset,
                warp,
            );
            point_mm(p.0, p.1)
        },
    };
    let bottom_geometry = EdgeSideGeometryMm {
        path: bottom_path,
        connection_point: {
            let p = connection_point_for_side(
                bottom_start,
                &bottom_segments,
                piece_width,
                piece_height,
                EdgeOrientation::Bottom,
                bottom_origin,
                offset,
                warp,
            );
            point_mm(p.0, p.1)
        },
    };
    let left_geometry = EdgeSideGeometryMm {
        path: left_path,
        connection_point: {
            let p = connection_point_for_side(
                left_start,
                &left_segments,
                piece_height,
                piece_width,
                EdgeOrientation::Left,
                left_origin,
                offset,
                warp,
            );
            point_mm(p.0, p.1)
        },
    };

    GridPieceSides {
        sides: [top_geometry, right_geometry, bottom_geometry, left_geometry],
        topology_edges: [
            grid_side_topology_edge(topology, row, col, SIDE_TOP),
            grid_side_topology_edge(topology, row, col, SIDE_RIGHT),
            grid_side_topology_edge(topology, row, col, SIDE_BOTTOM),
            grid_side_topology_edge(topology, row, col, SIDE_LEFT),
        ],
    }
}

fn grid_side_topology_edge(
    topology: &GridTopology,
    row: u32,
    col: u32,
    side: usize,
) -> Option<EdgeId> {
    let cols = topology.cols().get();
    let rows = topology.rows().get();
    let horizontal_count = rows * cols.saturating_sub(1);

    let idx = match side {
        SIDE_TOP if row > 0 => horizontal_count + (row - 1) * cols + col,
        SIDE_RIGHT if col + 1 < cols => row * cols.saturating_sub(1) + col,
        SIDE_BOTTOM if row + 1 < rows => horizontal_count + row * cols + col,
        SIDE_LEFT if col > 0 => row * cols.saturating_sub(1) + (col - 1),
        _ => return None,
    };
    Some(EdgeId(idx))
}

fn grid_boundary_frame_edge(
    rows: u32,
    cols: u32,
    row: u32,
    col: u32,
    side: usize,
) -> Option<FrameEdgeId> {
    let idx = match side {
        SIDE_TOP if row == 0 => col,
        SIDE_RIGHT if col + 1 == cols => cols + row,
        SIDE_BOTTOM if row + 1 == rows => cols + rows + (cols - 1 - col),
        SIDE_LEFT if col == 0 => cols + rows + cols + (rows - 1 - row),
        _ => return None,
    };
    Some(FrameEdgeId(idx))
}

#[derive(Clone, Copy, Debug)]
struct EdgeParams {
    tab_size: f32,
    tab_depth: f32,
    a: f32,
    b: f32,
    c: f32,
    d: f32,
    e: f32,
}

#[derive(Clone, Copy, Debug)]
enum TabSide {
    Tab,
    Blank,
}

impl TabSide {
    fn sign(self) -> i8 {
        match self {
            TabSide::Tab => 1,
            TabSide::Blank => -1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Edge {
    tab_side: TabSide,
    params: EdgeParams,
}

fn splitmix32(mut value: u32) -> u32 {
    value = value.wrapping_add(0x9E37_79B9);
    let mut z = value;
    z = (z ^ (z >> 16)).wrapping_mul(0x85EB_CA6B);
    z = (z ^ (z >> 13)).wrapping_mul(0xC2B2_AE35);
    z ^ (z >> 16)
}

fn rand_unit(seed: u32, salt: u32) -> f32 {
    let mixed = splitmix32(seed ^ salt);
    let top = mixed >> 8;
    top as f32 / ((1u32 << 24) as f32)
}

fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
}

fn jitter_value(
    seed: u32,
    salt: u32,
    base: f32,
    range: f32,
    min: f32,
    max: f32,
    variation: f32,
) -> f32 {
    let jitter = rand_unit(seed, salt) * 2.0 - 1.0;
    let value = base + jitter * range * variation;
    value.clamp(min, max)
}

fn edge_seed(base: u32, orientation: u32, row: u32, col: u32) -> u32 {
    base ^ orientation.wrapping_mul(0x9E37_79B9)
        ^ row.wrapping_mul(0x85EB_CA6B)
        ^ col.wrapping_mul(0xC2B2_AE35)
}

fn edge_from_seed(seed: u32, settings: &GridShapeSettings) -> Edge {
    let variation = settings.variation.clamp(VARIATION_MIN, VARIATION_MAX);

    let tab_size_raw = jitter_value(
        seed,
        0,
        settings.tab_width,
        TAB_WIDTH_RANGE,
        TAB_WIDTH_MIN,
        TAB_WIDTH_MAX,
        variation,
    );
    let tab_depth_raw = jitter_value(
        seed,
        2,
        settings.tab_depth,
        TAB_DEPTH_RANGE,
        TAB_DEPTH_MIN,
        TAB_DEPTH_MAX,
        variation,
    );

    let tab_size_scale = settings
        .tab_size_scale
        .clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
    let tab_size_min = settings
        .tab_size_min
        .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
    let tab_size_max = settings
        .tab_size_max
        .clamp(tab_size_min, TAB_SIZE_MAX_LIMIT);

    let tab_size = (tab_size_raw * tab_size_scale).clamp(tab_size_min, tab_size_max);
    let tab_depth = tab_depth_raw.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);

    let jitter_strength = settings
        .jitter_strength
        .clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
    let jitter_base = (variation * jitter_strength).clamp(0.0, jitter_strength);

    let skew_ratio = (settings.skew_range / SKEW_RANGE_MAX).clamp(0.0, 1.0);
    let jitter_len_bias = settings
        .jitter_len_bias
        .clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);

    let jitter_len = jitter_base * (jitter_len_bias + (1.0 - jitter_len_bias) * skew_ratio);
    let jitter_depth = jitter_base * tab_depth;

    let a = rand_range(seed, 3, -jitter_depth, jitter_depth);
    let b = rand_range(seed, 4, -jitter_len, jitter_len);
    let c = rand_range(seed, 5, -jitter_depth, jitter_depth);
    let d = rand_range(seed, 6, -jitter_len, jitter_len);
    let e = rand_range(seed, 7, -jitter_depth, jitter_depth);

    let tab_side = if rand_unit(seed, 8) < 0.5 {
        TabSide::Tab
    } else {
        TabSide::Blank
    };

    Edge {
        tab_side,
        params: EdgeParams {
            tab_size,
            tab_depth,
            a,
            b,
            c,
            d,
            e,
        },
    }
}

fn build_edge_maps(
    rows: u32,
    cols: u32,
    seed: u32,
    settings: &GridShapeSettings,
) -> (Vec<Vec<Option<Edge>>>, Vec<Vec<Option<Edge>>>) {
    let mut horizontal = vec![vec![None; cols as usize]; (rows + 1) as usize];
    for row in 1..rows {
        for col in 0..cols {
            let edge_seed = edge_seed(seed, 0, row, col);
            horizontal[row as usize][col as usize] = Some(edge_from_seed(edge_seed, settings));
        }
    }

    let mut vertical = vec![vec![None; (cols + 1) as usize]; rows as usize];
    for row in 0..rows {
        for col in 1..cols {
            let edge_seed = edge_seed(seed, 1, row, col);
            vertical[row as usize][col as usize] = Some(edge_from_seed(edge_seed, settings));
        }
    }

    (horizontal, vertical)
}

fn line_wave(seed: u32, axis: u32, index: u32, max_amp: f32) -> LineWave {
    if max_amp == 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }

    let salt = axis.wrapping_mul(0x9E37_79B9) ^ index;
    let amplitude = rand_range(seed ^ 0xB1EB_01DE, salt, -max_amp, max_amp);
    let skew = rand_range(seed ^ 0xA11C_E0DE, salt, -0.6, 0.6);

    LineWave { amplitude, skew }
}

fn build_line_waves(
    rows: u32,
    cols: u32,
    seed: u32,
    piece_width: f32,
    piece_height: f32,
    line_bend_ratio: f32,
) -> (Vec<LineWave>, Vec<LineWave>) {
    let bend_ratio = line_bend_ratio.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
    let max_h = piece_height * bend_ratio;
    let max_v = piece_width * bend_ratio;

    let mut horizontal = Vec::with_capacity((rows + 1) as usize);
    for row in 0..=rows {
        let wave = if row == 0 || row == rows {
            LineWave {
                amplitude: 0.0,
                skew: 0.0,
            }
        } else {
            line_wave(seed, 2, row, max_h)
        };
        horizontal.push(wave);
    }

    let mut vertical = Vec::with_capacity((cols + 1) as usize);
    for col in 0..=cols {
        let wave = if col == 0 || col == cols {
            LineWave {
                amplitude: 0.0,
                skew: 0.0,
            }
        } else {
            line_wave(seed, 3, col, max_v)
        };
        vertical.push(wave);
    }

    (horizontal, vertical)
}

fn edge_curve_segments(
    len: f32,
    depth_base: f32,
    edge: Option<&Edge>,
    tab_sign: i8,
    depth_limit: f32,
    edge_style: EdgeShapeStyle,
) -> Vec<ConnectorSeg> {
    let connector = if tab_sign == 0 {
        None
    } else {
        edge.map(|edge| ConnectorShape {
            tab_size: edge.params.tab_size,
            tab_depth: edge.params.tab_depth,
            a: edge.params.a,
            b: edge.params.b,
            c: edge.params.c,
            d: edge.params.d,
            e: edge.params.e,
        })
    };
    let input = EdgeProfileInput {
        len_mm: len,
        depth_base_mm: depth_base,
        depth_limit_mm: depth_limit.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX),
        sign: tab_sign,
        connector,
    };
    build_edge_profile_segments(edge_style, &input)
}

fn reverse_side_segments(segments: &[ConnectorSeg]) -> Vec<ConnectorSeg> {
    reverse_connector_segments(segments)
}

fn host_edge_frame(
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
) -> HostEdgeFrame {
    HostEdgeFrame {
        orientation,
        origin,
        offset,
    }
}

fn map_side_path(
    start: (f32, f32),
    segments: &[ConnectorSeg],
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
    warp: &WarpField<'_>,
) -> PathMm {
    compose_map_segments_to_path(
        start,
        segments,
        host_edge_frame(orientation, origin, offset),
        warp,
    )
}

fn connection_point_for_side(
    start: (f32, f32),
    segments: &[ConnectorSeg],
    tangent_scale: f32,
    normal_scale: f32,
    orientation: EdgeOrientation,
    origin: (f32, f32),
    offset: (f32, f32),
    warp: &WarpField<'_>,
) -> (f32, f32) {
    compose_connection_point_for_side(
        start,
        segments,
        tangent_scale,
        normal_scale,
        host_edge_frame(orientation, origin, offset),
        warp,
    )
}

fn append_mapped_local_line_points(
    out: &mut Vec<PathSegMm>,
    offset: (f32, f32),
    warp: &WarpField<'_>,
    points: &[(f32, f32)],
) {
    compose_append_mapped_local_line_points(out, offset, warp, points);
}

fn append_local_arc_lines(
    path: &mut PathMm,
    offset: (f32, f32),
    warp: &WarpField<'_>,
    points: &[(f32, f32)],
) {
    if points.len() <= 1 {
        return;
    }
    let mut segs = path.segs.to_vec();
    append_mapped_local_line_points(&mut segs, offset, warp, &points[1..]);
    path.segs = segs.into_boxed_slice();
}

fn host_line_steps(cells: u32) -> usize {
    (cells as usize * 20).clamp(24, 360)
}

fn host_frame_steps(rows: u32, cols: u32, shape: BorderFrameShape) -> usize {
    match shape {
        BorderFrameShape::Rectangle => 4,
        BorderFrameShape::Circle => ((rows.max(cols) as usize) * 56).clamp(48, 720),
        BorderFrameShape::RegularPolygon { sides } => (sides.max(3) as usize).clamp(3, 64),
    }
}

fn sample_warped_line(
    start: (f32, f32),
    end: (f32, f32),
    warp: &WarpField<'_>,
    steps: usize,
) -> PathMm {
    let steps = steps.max(1);
    let (sx, sy) = warp_point(start.0, start.1, warp);
    let mut segs = Vec::with_capacity(steps);

    for i in 1..=steps {
        let t = i as f32 / steps as f32;
        let x = start.0 + (end.0 - start.0) * t;
        let y = start.1 + (end.1 - start.1) * t;
        let (wx, wy) = warp_point(x, y, warp);
        segs.push(PathSegMm::LineTo {
            to: point_mm(wx, wy),
        });
    }

    PathMm::new(point_mm(sx, sy), segs.into_boxed_slice(), false)
}

fn path_from_points_closed(points: &[(f32, f32)]) -> PathMm {
    let start = points.first().copied().unwrap_or((0.0, 0.0));
    let mut segs = Vec::with_capacity(points.len().saturating_sub(1));
    for &(x, y) in points.iter().skip(1) {
        segs.push(PathSegMm::LineTo { to: point_mm(x, y) });
    }
    PathMm::new(point_mm(start.0, start.1), segs.into_boxed_slice(), true)
}

fn stitch_piece_ring(paths: &mut [PathMm; 4]) {
    for idx in 1..paths.len() {
        let prev_end = path_end_point(&paths[idx - 1]);
        paths[idx].start = prev_end;
    }
    let first_start = paths[0].start;
    let last_idx = paths.len() - 1;
    set_path_end_point(&mut paths[last_idx], first_start);
}

fn path_end_point(path: &PathMm) -> PointMm {
    let mut current = path.start;
    for seg in path.segs.iter() {
        current = match seg {
            PathSegMm::LineTo { to } => *to,
            PathSegMm::CubicTo { to, .. } => *to,
        };
    }
    current
}

fn set_path_end_point(path: &mut PathMm, target: PointMm) {
    if let Some(last) = path.segs.last_mut() {
        match last {
            PathSegMm::LineTo { to } => *to = target,
            PathSegMm::CubicTo { to, .. } => *to = target,
        }
    } else {
        path.start = target;
    }
}

fn point_mm(x: f32, y: f32) -> PointMm {
    PointMm {
        x: LengthMm::try_new(x).unwrap_or_default(),
        y: LengthMm::try_new(y).unwrap_or_default(),
    }
}
