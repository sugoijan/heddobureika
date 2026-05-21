//! Shaper for the hexagonal-tiling puzzle topology.
//!
//! Inner pieces are regular flat-top hexagons. Interior edges carry
//! tab/blank profiles via the shared `shape_profiles` machinery. The
//! puzzle frame uses rounded corners via the same `BoundaryArcClassifier`
//! the triangular shaper uses.
//!
//! Outer columns (col 0 and col C-1) absorb the horizontal "extra"
//! left over by the image aspect: their right (or left) half of the
//! hex is shifted out by `(outer_gap_pose - 1.5) * pose_unit` pixels,
//! so they meet inner-column neighbours that sit further away than the
//! standard `1.5*s` column spacing. Corner pieces forced to TAB sign
//! (outward bulge) on every interior edge.
//!
//! Pipeline:
//!
//! 1. Compute the 6 flat-top hex vertices for each piece in pixel coords.
//! 2. Clip each hex polygon against the puzzle rectangle, preserving an
//!    *edge label* per polygon side (either "original hex edge index"
//!    or "border" introduced by the clip).
//! 3. Match each labelled edge to its topology `EdgeId` (if interior)
//!    via the neighbour direction it points to. Edges whose neighbour
//!    is off-grid (e.g. the flat top side of a top-tangent piece) become
//!    border edges.
//! 4. Build the atlas via the shared `ShapeAtlasBuilder`. All edges are
//!    straight `LineTo` paths for the MVP.

use std::collections::HashMap;

use crate::hexagonal_topology::{HexPieceKind, HexagonalTopology};
use crate::ids::{EdgeId, PieceId};
use crate::shape::{
    EdgeSideGeometryMm, GeometryInvariantError, PathMm, PieceGeometryMm, ShapeAtlasMm,
};
use crate::shape_atlas_builder::{PieceEdgeBuilderSpec, ShapeAtlasBuildError, ShapeAtlasBuilder};
use crate::shape_profiles::{
    profiled_path_with_sign, reverse_path, straight_path, BoundaryArcClassifier, Point2,
};
use crate::traits::shaping::{PieceGeometryProvider, TopologyShaper};
use crate::traits::topology::PuzzleTopology;
use crate::units::LengthMm;

const SQRT_3: f32 = 1.732_050_8;
const SQRT_3_OVER_2: f32 = 0.866_025_4;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HexagonalShapeSettings {
    /// Reserved for future use — the MVP shaper renders a sharp
    /// rectangular puzzle frame and ignores this value.
    pub corner_radius_px: f32,
}

impl Default for HexagonalShapeSettings {
    fn default() -> Self {
        Self {
            corner_radius_px: 0.0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HexagonalShapeCache {
    pub piece_width: LengthMm,
    pub piece_height: LengthMm,
    pub mask_pad: LengthMm,
    pub atlas: ShapeAtlasMm,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct HexagonalShaper;

#[derive(Clone, Debug, PartialEq)]
pub enum HexagonalShapeBuildError {
    InternalConstruction,
    Invariant(GeometryInvariantError),
    Atlas(ShapeAtlasBuildError),
}

impl From<ShapeAtlasBuildError> for HexagonalShapeBuildError {
    fn from(err: ShapeAtlasBuildError) -> Self {
        HexagonalShapeBuildError::Atlas(err)
    }
}

impl PieceGeometryProvider for HexagonalShapeCache {
    fn piece_count(&self) -> u32 {
        self.atlas.pieces.len() as u32
    }

    fn piece_geometry(&self, piece: PieceId) -> &PieceGeometryMm {
        &self.atlas.pieces[piece.as_usize()]
    }

    fn interior_edge_geometry(&self, edge: EdgeId) -> &crate::shape::InteriorEdgeGeometryMm {
        &self.atlas.interior_edges[edge.as_usize()]
    }

    fn border_edge_geometry(
        &self,
        edge: crate::ids::BorderEdgeId,
    ) -> &crate::shape::BorderEdgeGeometryMm {
        &self.atlas.border_edges[edge.as_usize()]
    }

    fn frame_geometry(&self) -> &crate::shape::FrameGeometryMm {
        &self.atlas.frame
    }

    fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm {
        let geom = &self.atlas.pieces[piece.as_usize()];
        match geom.edges[edge_index] {
            crate::shape::PieceEdgeRef::Interior { edge, side } => {
                let interior = &self.atlas.interior_edges[edge.as_usize()];
                match side {
                    crate::shape::EdgeSide::A => &interior.side_a,
                    crate::shape::EdgeSide::B => &interior.side_b,
                }
            }
            crate::shape::PieceEdgeRef::Border { edge } => {
                &self.atlas.border_edges[edge.as_usize()].side
            }
        }
    }
}

impl TopologyShaper<HexagonalTopology> for HexagonalShaper {
    type Settings = HexagonalShapeSettings;
    type Cache = HexagonalShapeCache;
    type Error = HexagonalShapeBuildError;

    fn build_cache(
        &self,
        topology: &HexagonalTopology,
        frame_width: LengthMm,
        frame_height: LengthMm,
        seed: u32,
        settings: &Self::Settings,
    ) -> Result<Self::Cache, Self::Error> {
        let atlas = build_hex_atlas(
            topology,
            seed,
            frame_width.as_mm_f32(),
            frame_height.as_mm_f32(),
            settings.corner_radius_px.max(0.0),
        )?;
        atlas
            .validate(topology)
            .map_err(HexagonalShapeBuildError::Invariant)?;
        Ok(HexagonalShapeCache {
            piece_width: frame_width,
            piece_height: frame_height,
            mask_pad: LengthMm::try_new(
                frame_width.as_mm_f32().min(frame_height.as_mm_f32()) * 0.015,
            )
            .unwrap_or_default(),
            atlas,
        })
    }
}

// ---------------------------------------------------------------------
// Atlas construction
// ---------------------------------------------------------------------

type Pt = Point2;

/// Tag for a polygon edge, indicating where it came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HexEdgeTag {
    /// Original side of the hex (index `0..6`). Index 0 = NE side
    /// (V_top_right → V_right), CW from there. The neighbour in this
    /// direction may or may not exist — caller resolves.
    Original(u8),
    /// Cut introduced by clipping against one of the four frame sides.
    Border(FrameSide),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FrameSide {
    Top,
    Right,
    Bottom,
    Left,
}

/// A polygon vertex carrying the tag of the edge *leading into it*
/// (i.e. the edge from the previous vertex to this one).
#[derive(Clone, Copy, Debug)]
struct TaggedVertex {
    pt: Pt,
    in_tag: HexEdgeTag,
}

fn build_hex_atlas(
    topology: &HexagonalTopology,
    seed: u32,
    frame_w: f32,
    frame_h: f32,
    corner_radius_px: f32,
) -> Result<ShapeAtlasMm, HexagonalShapeBuildError> {
    let c = topology.cols().get();
    let outer_gap_pose = topology.outer_gap_pose();
    // Per-axis pose-unit derived from the topology's image extent.
    // In the wider-edges mode (`outer_gap_pose > 1.5`) the image
    // extent is `aspect * (R-1) * √3` on the x axis, which makes
    // `pose_unit_x = pose_unit_y` — inner hexes are exactly regular.
    // In the uniform fallback (clamp to 1.5) the image extent is
    // `(C-1) * 1.5`, so the per-axis values differ: hexes stretched
    // anisotropically, matching the pre-phase-6 behaviour.
    let (extent_x_pose, extent_y_pose) = topology.image_extent_in_pose_units();
    let pose_unit_x = frame_w / extent_x_pose.max(1.0e-3);
    let pose_unit_y = frame_h / extent_y_pose.max(1.0e-3);
    let s_x = pose_unit_x;
    let s_y = pose_unit_y;
    // For tab/blank sizing we want a representative hex side length.
    // Use the geometric mean of the per-axis pose units so the
    // average stays meaningful when the two differ (uniform fallback).
    // Scale up by `HEX_TAB_SCALE` so the tab/blank features are
    // visibly larger than triangular's defaults.
    const HEX_TAB_SCALE: f32 = 1.4;
    let average_edge_len = (pose_unit_x * pose_unit_y).sqrt().max(1.0) * HEX_TAB_SCALE;

    // Build (piece_a, piece_b) → EdgeId lookup, with the canonical
    // ordering `endpoint.0` ≤ `endpoint.1` matching topology storage.
    let mut topology_edges: HashMap<(PieceId, PieceId), EdgeId> = HashMap::new();
    for idx in 0..topology.edge_count() {
        let edge = EdgeId(idx);
        let (a, b) = topology.edge_endpoints(edge);
        topology_edges.insert((a, b), edge);
        topology_edges.insert((b, a), edge);
    }

    // Rounded-corner machinery for the puzzle frame. With a non-zero
    // radius, sharp polygon vertices at rectangle corners are replaced
    // by two inset vertices + an arc edge; the classifier then maps
    // border-edge paths through arc/straight transitions.
    let radius = corner_radius_px
        .max(0.0)
        .min(frame_w * 0.5)
        .min(frame_h * 0.5);
    let arc_classifier = if radius > 0.0 {
        Some(BoundaryArcClassifier::new(frame_w, frame_h, radius))
    } else {
        None
    };

    // First pass: compute each piece's clipped polygon, classify
    // edges, and remember per-piece per-edge metadata.
    let piece_count = topology.piece_count() as usize;
    let mut piece_polygons: Vec<Vec<TaggedVertex>> = Vec::with_capacity(piece_count);
    let mut piece_edge_kinds: Vec<Vec<EdgeKind>> = Vec::with_capacity(piece_count);

    for piece_idx in 0..topology.piece_count() {
        let piece = PieceId(piece_idx);
        let (col, row_in_col) = topology
            .piece_col_row(piece)
            .ok_or(HexagonalShapeBuildError::InternalConstruction)?;
        let _kind = topology
            .piece_kind(piece)
            .ok_or(HexagonalShapeBuildError::InternalConstruction)?;
        let (cx, cy) =
            piece_centre_px(col, row_in_col, c, pose_unit_x, pose_unit_y, outer_gap_pose);
        // delta_pose: by how many pose units the outer column's right
        // half (col 0) or left half (col C-1) is shifted out from a
        // regular hex shape. For uniform layout (`outer_gap_pose=1.5`)
        // this is zero. For wider-edges layout we push the rim vertices
        // outward by `outer_gap_pose - 1.5` pose units (= the same
        // amount in pixels since `pose_unit_x = pose_unit_y` in that
        // mode).
        let delta_pose = (outer_gap_pose - 1.5).max(0.0);
        let stretch_right_px = if col == 0 {
            delta_pose * pose_unit_x
        } else {
            0.0
        };
        let stretch_left_px = if col + 1 == c {
            delta_pose * pose_unit_x
        } else {
            0.0
        };
        let hex = full_hex_polygon(cx, cy, s_x, s_y, stretch_right_px, stretch_left_px);
        let polygon = clip_to_rect(hex, frame_w, frame_h);
        let polygon = dedupe_consecutive(polygon);
        if polygon.len() < 3 {
            return Err(HexagonalShapeBuildError::InternalConstruction);
        }
        let polygon = round_polygon_corners(polygon, frame_w, frame_h, radius);
        let kinds =
            polygon_edge_kinds(topology, col, row_in_col, &polygon, piece, &topology_edges)?;
        piece_polygons.push(polygon);
        piece_edge_kinds.push(kinds);
    }

    // Decide an "owner" piece per topology edge. The owner computes the
    // canonical tab/blank path; the peer takes `reverse_path` of it so
    // the two `side_a` / `side_b` paths agree by construction.
    //
    // Default rule: lower piece-id is owner.
    // Corner override: if either endpoint is a `Corner` piece, the
    // corner is owner and uses a forced `sign=+1`, which guarantees
    // the corner piece has a tab (outward bulge) on every interior
    // edge. The peer is always a non-corner piece and gets the
    // matching blank.
    let edge_owner = compute_edge_owners(topology);

    // Second pass: pre-compute canonical interior paths per topology
    // edge. We walk the OWNER's polygon to find the (start, end)
    // endpoints that match the topology edge.
    let mut interior_paths: HashMap<EdgeId, PathMm> = HashMap::new();
    for piece_idx in 0..topology.piece_count() {
        let polygon = &piece_polygons[piece_idx as usize];
        let kinds = &piece_edge_kinds[piece_idx as usize];
        let piece = PieceId(piece_idx);
        let owner_is_corner = topology.piece_kind(piece) == Some(HexPieceKind::Corner);
        let n = polygon.len();
        for i in 0..n {
            let EdgeKind::Interior { edge, .. } = kinds[i] else {
                continue;
            };
            if edge_owner.get(&edge).copied() != Some(piece) {
                continue;
            }
            let start = polygon[i].pt;
            let end = polygon[(i + 1) % n].pt;
            // sign=-1 makes the bulge land on the -ny side, which is
            // the OUTWARD direction for a visually-CW polygon in
            // screen coords (y-down). That gives the corner piece a
            // tab (outward bulge); the non-corner peer takes
            // `reverse_path` and ends up with the matching blank.
            let forced_sign: Option<i8> = if owner_is_corner { Some(-1) } else { None };
            let path = profiled_path_with_sign(
                start,
                end,
                seed,
                edge,
                average_edge_len,
                frame_w,
                frame_h,
                forced_sign,
            )
            .ok_or(HexagonalShapeBuildError::InternalConstruction)?;
            interior_paths.insert(edge, path);
        }
    }

    // Third pass: emit edge specs per piece. Interior edges resolve to
    // the canonical path (if this piece is owner) or its reverse (if
    // peer). Border edges are straight for now.
    let mut builder = ShapeAtlasBuilder::new(topology);
    for piece_idx in 0..topology.piece_count() {
        let piece = PieceId(piece_idx);
        let polygon = &piece_polygons[piece_idx as usize];
        let kinds = &piece_edge_kinds[piece_idx as usize];
        let n = polygon.len();

        let mut edges: Vec<PieceEdgeBuilderSpec> = Vec::with_capacity(n);
        for i in 0..n {
            let start = polygon[i].pt;
            let end = polygon[(i + 1) % n].pt;
            let kind = kinds[i];
            let spec = match kind {
                EdgeKind::Border => {
                    let path = match &arc_classifier {
                        Some(classifier) => classifier
                            .path_along_boundary(start, end)
                            .ok_or(HexagonalShapeBuildError::InternalConstruction)?,
                        None => straight_path(start, end)
                            .ok_or(HexagonalShapeBuildError::InternalConstruction)?,
                    };
                    PieceEdgeBuilderSpec::Border {
                        side_geometry: EdgeSideGeometryMm {
                            path: path.clone(),
                            connection_point: path.start,
                        },
                        frame_sort_key: frame_sort_key_for(start, end),
                    }
                }
                EdgeKind::Interior { edge, .. } => {
                    let canonical = interior_paths
                        .get(&edge)
                        .ok_or(HexagonalShapeBuildError::InternalConstruction)?;
                    let path = if edge_owner.get(&edge) == Some(&piece) {
                        canonical.clone()
                    } else {
                        reverse_path(canonical)
                    };
                    PieceEdgeBuilderSpec::Interior {
                        edge,
                        side_geometry: EdgeSideGeometryMm {
                            path: path.clone(),
                            connection_point: path.start,
                        },
                    }
                }
            };
            edges.push(spec);
        }
        builder.push_piece(piece, edges)?;
    }

    let atlas = builder.build()?;
    Ok(atlas)
}

/// For every topology edge, return the piece that owns the canonical
/// tab path. If either endpoint is a `Corner`, that piece is the owner
/// (it will use a forced sign=+1 and get a tab). Otherwise the
/// lower-id piece wins (default rule, same as before).
fn compute_edge_owners(topology: &HexagonalTopology) -> HashMap<EdgeId, PieceId> {
    let mut owners = HashMap::new();
    for idx in 0..topology.edge_count() {
        let edge = EdgeId(idx);
        let (a, b) = topology.edge_endpoints(edge);
        let a_corner = topology.piece_kind(a) == Some(HexPieceKind::Corner);
        let b_corner = topology.piece_kind(b) == Some(HexPieceKind::Corner);
        let owner = if a_corner && !b_corner {
            a
        } else if b_corner && !a_corner {
            b
        } else if a.as_u32() <= b.as_u32() {
            a
        } else {
            b
        };
        owners.insert(edge, owner);
    }
    owners
}

#[derive(Clone, Copy, Debug)]
enum EdgeKind {
    Border,
    Interior { edge: EdgeId },
}

/// Drop consecutive vertices that share a pixel position (after the
/// snap-to-grid). Sutherland-Hodgman with on-boundary-counts-as-inside
/// produces zero-length edges where an original vertex lies exactly on
/// the clip line — the intersection point and the next kept vertex
/// coincide. Removing those keeps the polygon's edge ring clean for
/// the atlas-validation step.
fn dedupe_consecutive(poly: Vec<TaggedVertex>) -> Vec<TaggedVertex> {
    let n = poly.len();
    if n < 2 {
        return poly;
    }
    let mut out: Vec<TaggedVertex> = Vec::with_capacity(n);
    for v in poly.into_iter() {
        let same_as_last = out
            .last()
            .map(|last| same_point(last.pt, v.pt))
            .unwrap_or(false);
        if !same_as_last {
            out.push(v);
        }
    }
    // Polygon is closed; also remove trailing duplicates of the head.
    while out.len() > 1 && same_point(out[0].pt, out[out.len() - 1].pt) {
        out.pop();
    }
    out
}

fn same_point(a: Pt, b: Pt) -> bool {
    (a.x - b.x).abs() < 1.0e-3 && (a.y - b.y).abs() < 1.0e-3
}

/// Replace each polygon vertex that lies exactly on a rectangle corner
/// with two new vertices inset by `radius` along the two adjacent
/// sides, leaving a new arc-tagged edge between them. The polygon walk
/// stays CW; the existing vertex's `in_tag` carries to the entry-side
/// inset vertex (it's the same edge that ended a bit earlier), and the
/// exit-side inset vertex picks up a `Border(side)` tag whose
/// downstream effect — straight or arc — is handled by
/// `BoundaryArcClassifier::path_along_boundary`.
fn round_polygon_corners(
    poly: Vec<TaggedVertex>,
    frame_w: f32,
    frame_h: f32,
    radius: f32,
) -> Vec<TaggedVertex> {
    if radius <= 0.0 {
        return poly;
    }
    // For each rectangle corner (CW order): corner coords, entry side
    // (which edge of the rectangle the CW walk arrives on), exit side
    // (which edge it leaves on).
    let corner_table: [(f32, f32, FrameSide, FrameSide); 4] = [
        (0.0, 0.0, FrameSide::Left, FrameSide::Top),
        (frame_w, 0.0, FrameSide::Top, FrameSide::Right),
        (frame_w, frame_h, FrameSide::Right, FrameSide::Bottom),
        (0.0, frame_h, FrameSide::Bottom, FrameSide::Left),
    ];
    let mut out: Vec<TaggedVertex> = Vec::with_capacity(poly.len() + 4);
    for v in poly.into_iter() {
        let match_corner = corner_table
            .iter()
            .find(|(cx, cy, _, _)| (v.pt.x - cx).abs() < 1.0e-3 && (v.pt.y - cy).abs() < 1.0e-3);
        if let Some(&(cx, cy, entry_side, exit_side)) = match_corner {
            let entry_inset = inset_along(cx, cy, entry_side, radius);
            let exit_inset = inset_along(cx, cy, exit_side, radius);
            out.push(TaggedVertex {
                pt: entry_inset,
                in_tag: v.in_tag,
            });
            out.push(TaggedVertex {
                pt: exit_inset,
                in_tag: HexEdgeTag::Border(exit_side),
            });
        } else {
            out.push(v);
        }
    }
    out
}

/// Returns the point on the rectangle at distance `radius` from
/// `(cx, cy)` along `side`. Used by `round_polygon_corners` to inset
/// sharp corner vertices.
fn inset_along(cx: f32, cy: f32, side: FrameSide, radius: f32) -> Pt {
    match side {
        FrameSide::Left | FrameSide::Right => Pt {
            x: cx,
            // Move AWAY from the corner along this vertical side. The
            // direction depends on whether the corner is TL/TR (cy=0,
            // inset downward = +radius) or BL/BR (cy=H, inset upward
            // = -radius).
            y: if cy <= 1.0e-3 {
                cy + radius
            } else {
                cy - radius
            },
        },
        FrameSide::Top | FrameSide::Bottom => Pt {
            x: if cx <= 1.0e-3 {
                cx + radius
            } else {
                cx - radius
            },
            y: cy,
        },
    }
}

fn polygon_edge_kinds(
    topology: &HexagonalTopology,
    col: u32,
    row_in_col: u32,
    polygon: &[TaggedVertex],
    piece: PieceId,
    topology_edges: &HashMap<(PieceId, PieceId), EdgeId>,
) -> Result<Vec<EdgeKind>, HexagonalShapeBuildError> {
    let n = polygon.len();
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let next = polygon[(i + 1) % n];
        let tag = next.in_tag;
        let kind = match tag {
            HexEdgeTag::Original(idx) => {
                if let Some(neighbour) = neighbour_in_direction(topology, col, row_in_col, idx) {
                    let edge = *topology_edges
                        .get(&(piece, neighbour))
                        .ok_or(HexagonalShapeBuildError::InternalConstruction)?;
                    EdgeKind::Interior { edge }
                } else {
                    EdgeKind::Border
                }
            }
            HexEdgeTag::Border(_) => EdgeKind::Border,
        };
        out.push(kind);
    }
    Ok(out)
}

fn piece_centre_px(
    col: u32,
    row_in_col: u32,
    cols: u32,
    pose_unit_x: f32,
    pose_unit_y: f32,
    outer_gap_pose: f32,
) -> (f32, f32) {
    // Canonical pose-unit position uses the piecewise column layout:
    // col 0 at 0, col C-1 at `outer_gap_pose + (C-3)*1.5 + outer_gap_pose`,
    // inner cols at `outer_gap_pose + (col-1)*1.5`. Multiply by the
    // per-axis pose-unit to get pixels.
    let x_pose = crate::hexagonal_topology::col_pose_x(col, cols, outer_gap_pose);
    let y_pose = (row_in_col as f32 + (col % 2) as f32 * 0.5) * SQRT_3;
    (x_pose * pose_unit_x, y_pose * pose_unit_y)
}

/// Snap a point to a 1/128 px grid. Adjacent hex pieces compute their
/// shared vertices via different multiplication chains (`cy - half_y`
/// vs `0 + half_y`); without this snap, float non-associativity makes
/// the two values differ by a few thousandths of a pixel and the
/// atlas ring-contiguity check fails. 1/128 px (~0.008) is well below
/// visual perceptibility but tolerates the accumulated float drift on
/// the largest grids (19x9 hits ~0.001 px diffs at the right frame).
fn snap_pt(p: Pt) -> Pt {
    const SCALE: f32 = 128.0;
    Pt {
        x: (p.x * SCALE).round() / SCALE,
        y: (p.y * SCALE).round() / SCALE,
    }
}

/// Returns the 6 flat-top hex vertices CW from the top-right vertex.
/// `s_x`/`s_y` are the hex side length in pixels (x and y can differ
/// slightly if aspect doesn't match perfectly, but we now always use
/// `s_x = s_y = pose_unit`).
///
/// `stretch_right_px` / `stretch_left_px` shift the right or left
/// half of the hex out from a regular shape by that many pixels, so
/// col 0 / col C-1 pieces meet inner-column neighbours that sit at
/// `outer_gap_pose > 1.5` away. For uniform layout both are zero and
/// the hex is regular.
fn full_hex_polygon(
    cx: f32,
    cy: f32,
    s_x: f32,
    s_y: f32,
    stretch_right_px: f32,
    stretch_left_px: f32,
) -> Vec<TaggedVertex> {
    // For flat-top hex with side `s` in pose units, the vertices in
    // pose-unit-x and pose-unit-y space are:
    //   V0 top-right  : (cx + 0.5,    cy - √3/2)
    //   V1 right      : (cx + 1.0,    cy        )
    //   V2 bottom-right: (cx + 0.5,   cy + √3/2)
    //   V3 bottom-left : (cx - 0.5,   cy + √3/2)
    //   V4 left        : (cx - 1.0,   cy        )
    //   V5 top-left    : (cx - 0.5,   cy - √3/2)
    // Stored CW. We tag each vertex with the edge LEADING INTO it:
    //   into V0 ← E5 = top side       (between V5 and V0, N neighbour)
    //   into V1 ← E0 = NE side        (between V0 and V1)
    //   into V2 ← E1 = SE side        (between V1 and V2)
    //   into V3 ← E2 = bottom side    (between V2 and V3, S neighbour)
    //   into V4 ← E3 = SW side
    //   into V5 ← E4 = NW side
    // Hex of side `s` has top/bottom vertices `(±s/2, ±s·√3/2)` and
    // left/right vertices `(±s, 0)`. In our pose-unit system one
    // pose-unit on each axis equals one hex side, so x offsets are
    // `pose_unit_x * (0.5 or 1)` and y offsets are
    // `pose_unit_y * (√3/2)`.
    let half_x = s_x;
    let quarter_x = s_x * 0.5;
    let half_y = s_y * SQRT_3_OVER_2;
    let dr = stretch_right_px;
    let dl = stretch_left_px;
    let snap = |x: f32, y: f32| snap_pt(Pt { x, y });
    vec![
        TaggedVertex {
            pt: snap(cx + quarter_x + dr, cy - half_y),
            in_tag: HexEdgeTag::Original(5), // top side, N neighbour
        },
        TaggedVertex {
            pt: snap(cx + half_x + dr, cy),
            in_tag: HexEdgeTag::Original(0), // NE side
        },
        TaggedVertex {
            pt: snap(cx + quarter_x + dr, cy + half_y),
            in_tag: HexEdgeTag::Original(1), // SE side
        },
        TaggedVertex {
            pt: snap(cx - quarter_x - dl, cy + half_y),
            in_tag: HexEdgeTag::Original(2), // bottom side, S neighbour
        },
        TaggedVertex {
            pt: snap(cx - half_x - dl, cy),
            in_tag: HexEdgeTag::Original(3), // SW side
        },
        TaggedVertex {
            pt: snap(cx - quarter_x - dl, cy - half_y),
            in_tag: HexEdgeTag::Original(4), // NW side
        },
    ]
}

fn clip_to_rect(poly: Vec<TaggedVertex>, frame_w: f32, frame_h: f32) -> Vec<TaggedVertex> {
    let poly = clip_against_side(poly, FrameSide::Left, frame_w, frame_h);
    let poly = clip_against_side(poly, FrameSide::Right, frame_w, frame_h);
    let poly = clip_against_side(poly, FrameSide::Top, frame_w, frame_h);
    clip_against_side(poly, FrameSide::Bottom, frame_w, frame_h)
}

/// Sutherland-Hodgman clip against one frame side. Vertices carry the
/// edge tag of the edge LEADING IN to them; when an edge crosses the
/// clip line we insert one or two synthetic vertices.
///
/// Convention: the segment from `poly[i-1]` to `poly[i]` carries
/// `poly[i].in_tag`. A "border" segment inserted along the clip line is
/// tagged with `HexEdgeTag::Border(side)`, attached to the vertex that
/// re-enters the keep half-plane.
fn clip_against_side(
    poly: Vec<TaggedVertex>,
    side: FrameSide,
    frame_w: f32,
    frame_h: f32,
) -> Vec<TaggedVertex> {
    if poly.is_empty() {
        return poly;
    }
    let n = poly.len();
    let mut out = Vec::with_capacity(n);
    let mut prev = poly[n - 1];
    let mut prev_inside = inside(prev.pt, side, frame_w, frame_h);
    for &curr in &poly {
        let curr_inside = inside(curr.pt, side, frame_w, frame_h);
        match (prev_inside, curr_inside) {
            (true, true) => {
                out.push(curr);
            }
            (true, false) => {
                // Edge leaves the keep region. Emit the intersection
                // point carrying the original tag (the kept-piece-side
                // ends there).
                let ip = intersect(prev.pt, curr.pt, side, frame_w, frame_h);
                out.push(TaggedVertex {
                    pt: ip,
                    in_tag: curr.in_tag,
                });
            }
            (false, true) => {
                // Edge re-enters. Emit the intersection (border tag) +
                // the current vertex (with its original tag).
                let ip = intersect(prev.pt, curr.pt, side, frame_w, frame_h);
                out.push(TaggedVertex {
                    pt: ip,
                    in_tag: HexEdgeTag::Border(side),
                });
                out.push(curr);
            }
            (false, false) => {}
        }
        prev = curr;
        prev_inside = curr_inside;
    }
    out
}

fn inside(pt: Pt, side: FrameSide, frame_w: f32, frame_h: f32) -> bool {
    const EPS: f32 = 1.0e-3;
    match side {
        FrameSide::Left => pt.x >= -EPS,
        FrameSide::Right => pt.x <= frame_w + EPS,
        FrameSide::Top => pt.y >= -EPS,
        FrameSide::Bottom => pt.y <= frame_h + EPS,
    }
}

fn intersect(a: Pt, b: Pt, side: FrameSide, frame_w: f32, frame_h: f32) -> Pt {
    let (clip, axis_horizontal) = match side {
        FrameSide::Left => (0.0_f32, false),
        FrameSide::Right => (frame_w, false),
        FrameSide::Top => (0.0_f32, true),
        FrameSide::Bottom => (frame_h, true),
    };
    let raw = if axis_horizontal {
        let t = (clip - a.y) / (b.y - a.y);
        Pt {
            x: a.x + t * (b.x - a.x),
            y: clip,
        }
    } else {
        let t = (clip - a.x) / (b.x - a.x);
        Pt {
            x: clip,
            y: a.y + t * (b.y - a.y),
        }
    };
    snap_pt(raw)
}

/// Returns the neighbour `PieceId` for a hex side index (0..6), or
/// `None` if that neighbour is outside the topology grid. Matches the
/// `neighbour_coords` table in `hexagonal_topology.rs`:
///   side 0 = NE, 1 = SE, 2 = S, 3 = SW, 4 = NW, 5 = N
fn neighbour_in_direction(
    topology: &HexagonalTopology,
    col: u32,
    row_in_col: u32,
    side_idx: u8,
) -> Option<PieceId> {
    let c = col as i32;
    let r = row_in_col as i32;
    let (n_col, n_row) = if col % 2 == 0 {
        match side_idx {
            0 => (c + 1, r - 1), // NE
            1 => (c + 1, r),     // SE
            2 => (c, r + 1),     // S
            3 => (c - 1, r),     // SW
            4 => (c - 1, r - 1), // NW
            5 => (c, r - 1),     // N
            _ => return None,
        }
    } else {
        match side_idx {
            0 => (c + 1, r),     // NE
            1 => (c + 1, r + 1), // SE
            2 => (c, r + 1),     // S
            3 => (c - 1, r + 1), // SW
            4 => (c - 1, r),     // NW
            5 => (c, r - 1),     // N
            _ => return None,
        }
    };
    if n_col < 0 || n_row < 0 {
        return None;
    }
    topology.piece_id_at(n_col as u32, n_row as u32)
}

/// Deterministic sort key for border edges. We use the midpoint's
/// perimeter parameter (clockwise from top-left around the rectangle):
/// top side [0, frame_w], right [frame_w, frame_w+frame_h], etc. This
/// makes border edges ordered consistently with the rendered frame
/// outline.
fn frame_sort_key_for(start: Pt, end: Pt) -> f32 {
    // Approximate: use the midpoint and pick the dominant side.
    let mx = (start.x + end.x) * 0.5;
    let my = (start.y + end.y) * 0.5;
    // Without knowing the frame dimensions here we can't fold the
    // perimeter parameter cleanly; using (4 * primary axis + secondary)
    // gives a stable ordering: x increasing on top/bottom, y on
    // left/right. We mostly want determinism, not a specific order.
    mx + my * 1.0e3
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_atlas_3x2_succeeds() {
        let topology = HexagonalTopology::try_new_uniform(3, 2).expect("valid");
        let cache = HexagonalShaper
            .build_cache(
                &topology,
                LengthMm::try_new(450.0).unwrap(),
                LengthMm::try_new(300.0).unwrap(),
                0,
                &HexagonalShapeSettings::default(),
            )
            .expect("atlas");
        assert_eq!(cache.atlas.pieces.len(), 5);
    }

    #[test]
    fn build_atlas_5x3_succeeds() {
        let topology = HexagonalTopology::try_new_uniform(5, 3).expect("valid");
        let cache = HexagonalShaper
            .build_cache(
                &topology,
                LengthMm::try_new(600.0).unwrap(),
                LengthMm::try_new(346.41).unwrap(), // ≈ 600 * √3/3 for regular hexes
                0,
                &HexagonalShapeSettings::default(),
            )
            .expect("atlas");
        assert_eq!(cache.atlas.pieces.len(), 13);
        // 4 corners + 1 top cut + 1 bottom cut + 1 left cut + 1 right
        // cut + 4 tangents = 12 border pieces; piece 6 is interior.
        let interior_count = (0..topology.piece_count() as usize)
            .filter(|&i| topology.is_frame_border_piece(PieceId(i as u32)) == false)
            .count();
        assert_eq!(interior_count, 1);
    }

    #[test]
    fn corner_pieces_always_have_tabs_on_interior_edges() {
        // Sweep through a range of (cols, rows, aspect) and verify
        // every Corner piece's interior edges have a `CubicTo`
        // segment in their path (= tab, not straight line). Catches
        // the regression where `profiled_path_with_sign` fell back
        // to `straight_path` when both signs failed the frame-fit
        // check.
        use crate::hexagonal_topology::HexPieceKind;
        use crate::shape::PathSegMm;
        use crate::traits::topology::PuzzleTopology;
        for (c, r, aspect) in [
            (5, 3, 1.0_f32),
            (7, 4, 1.71),
            (9, 5, 1.71),
            (9, 6, 1.71),
            (11, 5, 1.71),
            (11, 7, 1.0),
            (13, 7, 1.71),
            (15, 9, 1.71),
        ] {
            let topology = HexagonalTopology::try_new(c, r, aspect).expect("valid");
            let h = 700.0_f32;
            let w = aspect * h;
            let cache = HexagonalShaper
                .build_cache(
                    &topology,
                    LengthMm::try_new(w).unwrap(),
                    LengthMm::try_new(h).unwrap(),
                    0,
                    &HexagonalShapeSettings::default(),
                )
                .expect("atlas");
            for piece_idx in 0..topology.piece_count() {
                let piece = PieceId(piece_idx);
                if topology.piece_kind(piece) != Some(HexPieceKind::Corner) {
                    continue;
                }
                let geom = &cache.atlas.pieces[piece.as_usize()];
                for (edge_idx, edge_ref) in geom.edges.iter().enumerate() {
                    if let crate::shape::PieceEdgeRef::Interior { edge, side } = edge_ref {
                        let interior = &cache.atlas.interior_edges[edge.as_usize()];
                        let side_geom = match side {
                            crate::shape::EdgeSide::A => &interior.side_a,
                            crate::shape::EdgeSide::B => &interior.side_b,
                        };
                        let has_curve = side_geom
                            .path
                            .segs
                            .iter()
                            .any(|s| matches!(s, PathSegMm::CubicTo { .. }));
                        assert!(
                            has_curve,
                            "{c}x{r} aspect={aspect} corner piece {piece_idx} \
                             edge {edge_idx} is a straight line (no tab)"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn build_atlas_at_high_piece_counts() {
        // User reported piece counts >= 300 silently fail. Iterate the
        // hex registry resolver for each of the curated targets and
        // confirm the atlas builds.
        for &target in &[150u32, 300, 500, 750, 1000] {
            // Compute (C, R) by aspect from the resolver math.
            let aspect = 1200.0_f32 / 700.0;
            let kind_target_aspect = aspect * 1.154_700_5_f32;
            let init_rows_f = (target as f32 / kind_target_aspect.max(1.0e-3))
                .sqrt()
                .max(2.0);
            let init_rows = init_rows_f.round() as u32;
            let mut init_cols = (init_rows_f * kind_target_aspect + 1.0).round() as u32;
            if init_cols % 2 == 0 {
                init_cols += 1;
            }
            let init_cols = init_cols.max(3);
            let topology =
                HexagonalTopology::try_new(init_cols, init_rows, aspect).unwrap_or_else(|| {
                    panic!("try_new failed for {init_cols}x{init_rows} target={target}")
                });
            let cache = HexagonalShaper
                .build_cache(
                    &topology,
                    LengthMm::try_new(1200.0).unwrap(),
                    LengthMm::try_new(700.0).unwrap(),
                    0,
                    &HexagonalShapeSettings::default(),
                )
                .unwrap_or_else(|e| {
                    panic!(
                        "build_cache failed for target={target} ({init_cols}x{init_rows}): {e:?}"
                    )
                });
            assert!(!cache.atlas.pieces.is_empty());
        }
    }

    #[test]
    fn build_atlas_with_wider_edges_layout() {
        // User's reference case: 1200x700 image, 9x6 hexagonal puzzle
        // (50 pieces). Image aspect 1.714 > uniform-layout target
        // (~1.386 for 9x6), so the topology activates the wider-edges
        // layout. The atlas should still validate and produce 50
        // pieces; the column-0 piece is visibly wider than half a hex
        // side.
        let aspect = 1200.0 / 700.0;
        let topology = HexagonalTopology::try_new(9, 6, aspect).expect("valid");
        assert!(
            topology.outer_gap_pose() > 1.5,
            "wider-edges expected; got outer_gap_pose = {}",
            topology.outer_gap_pose()
        );
        let cache = HexagonalShaper
            .build_cache(
                &topology,
                LengthMm::try_new(1200.0).unwrap(),
                LengthMm::try_new(700.0).unwrap(),
                0,
                &HexagonalShapeSettings::default(),
            )
            .expect("atlas");
        assert_eq!(cache.atlas.pieces.len(), 50);
    }

    #[test]
    fn outer_gap_pose_clamps_to_uniform_when_aspect_too_narrow() {
        // Aspect smaller than the uniform target ⇒ would require
        // thinner edges; topology must fall back to uniform layout
        // (outer_gap_pose = 1.5).
        let topology = HexagonalTopology::try_new(9, 6, 0.5).expect("valid");
        assert_eq!(topology.outer_gap_pose(), 1.5);
    }

    #[test]
    fn build_atlas_with_rounded_corners_succeeds() {
        let topology = HexagonalTopology::try_new_uniform(5, 3).expect("valid");
        let cache = HexagonalShaper
            .build_cache(
                &topology,
                LengthMm::try_new(600.0).unwrap(),
                LengthMm::try_new(346.41).unwrap(),
                0,
                &HexagonalShapeSettings {
                    corner_radius_px: 30.0,
                },
            )
            .expect("atlas with rounded corners");
        assert_eq!(cache.atlas.pieces.len(), 13);
    }

    #[test]
    fn build_atlas_7x4_succeeds() {
        let topology = HexagonalTopology::try_new_uniform(7, 4).expect("valid");
        let cache = HexagonalShaper
            .build_cache(
                &topology,
                LengthMm::try_new(900.0).unwrap(),
                LengthMm::try_new(520.0).unwrap(),
                0,
                &HexagonalShapeSettings::default(),
            )
            .expect("atlas");
        assert_eq!(cache.atlas.pieces.len(), 25);
    }
}
