use crate::ids::{EdgeId, PieceId};
use crate::playable::Pose2;
use crate::shape::{PathMm, PathSegMm, PieceEdgeRef, PointMm, ShapeAtlasMm};
use crate::shape_svg::path_to_svg_d;
use crate::traits::topology::PuzzleTopology;
use crate::units::LengthMm;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RectPx {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

/// Canonical rounded-rect that defines the puzzle's outer frame. Every
/// topology shapes its border pieces against this shape, and every
/// renderer draws its dashed workspace outline from it — so a given
/// puzzle has exactly one corner radius regardless of topology kind.
///
/// `corner_radius_px` is derived from `typical_piece_extent_px` using
/// the same ratio grid has historically used (`CORNER_RADIUS_RATIO =
/// 0.05`), clamped to never exceed 45% of the smallest piece extent.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PuzzleFrameShape {
    pub bounds: RectPx,
    pub corner_radius_px: f32,
}

/// Ratio of the smallest typical piece extent that becomes the puzzle
/// frame corner radius. Matches the historical grid value so existing
/// grid puzzles look unchanged.
pub const PUZZLE_FRAME_CORNER_RADIUS_RATIO: f32 = 0.05;

impl PuzzleFrameShape {
    /// Builds the canonical frame shape for an image of `(image_w,
    /// image_h)` pixels with the supplied `typical_piece_extent_px`
    /// (component-wise median piece bbox size). The corner radius is
    /// `min(typical_x, typical_y) * PUZZLE_FRAME_CORNER_RADIUS_RATIO`,
    /// clamped to never exceed 45% of the smallest extent so the arcs
    /// stay inside the smallest piece.
    pub fn from_image_and_pieces(
        image_w: u32,
        image_h: u32,
        typical_piece_extent_px: [f32; 2],
    ) -> Self {
        let min_extent = typical_piece_extent_px[0].min(typical_piece_extent_px[1]);
        let mut radius = min_extent * PUZZLE_FRAME_CORNER_RADIUS_RATIO;
        let max_radius = min_extent * 0.45;
        if radius > max_radius {
            radius = max_radius;
        }
        if !radius.is_finite() || radius < 0.0 {
            radius = 0.0;
        }
        Self {
            bounds: RectPx {
                x: 0.0,
                y: 0.0,
                width: image_w as f32,
                height: image_h as f32,
            },
            corner_radius_px: radius,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PiecePaths {
    pub outline: String,
    pub edges: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PieceRenderGeometry {
    pub id: PieceId,
    pub bounds_px: RectPx,
    pub pose_anchor_px: [f32; 2],
    pub image_origin_px: [f32; 2],
    pub outline_svg: String,
    pub edge_svgs: Vec<String>,
    /// For each entry in `edge_svgs`, the topology edge id this edge
    /// corresponds to (when the edge sits between this piece and a
    /// neighbour) or `None` (when the edge sits on the puzzle's outer
    /// frame and has no neighbour). The SVG renderer uses this to decide
    /// which edges to draw as part of a joined-group outline.
    pub topology_edges: Vec<Option<EdgeId>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PuzzleRenderGeometry {
    pub image_width: u32,
    pub image_height: u32,
    pub puzzle_bounds_px: RectPx,
    pub pose_unit_px: [f32; 2],
    pub pose_origin_px: [f32; 2],
    pub pieces: Vec<PieceRenderGeometry>,
    pub mask_pad_px: f32,
    /// Component-wise minimum bounding-box size across all pieces, in pixels.
    pub min_piece_extent_px: [f32; 2],
    /// Median bounding-box size across all pieces, in pixels.
    pub typical_piece_extent_px: [f32; 2],
    /// Canonical puzzle frame shape (rounded rect). Renderers read its
    /// `corner_radius_px` to draw the dashed workspace outline; each
    /// topology uses the same value to round its border pieces.
    pub frame_shape: PuzzleFrameShape,
}

impl PuzzleRenderGeometry {
    pub fn compute_piece_extents(pieces: &[PieceRenderGeometry]) -> ([f32; 2], [f32; 2]) {
        if pieces.is_empty() {
            return ([0.0, 0.0], [0.0, 0.0]);
        }
        let mut min_w = f32::INFINITY;
        let mut min_h = f32::INFINITY;
        let mut widths = Vec::with_capacity(pieces.len());
        let mut heights = Vec::with_capacity(pieces.len());
        for piece in pieces {
            min_w = min_w.min(piece.bounds_px.width);
            min_h = min_h.min(piece.bounds_px.height);
            widths.push(piece.bounds_px.width);
            heights.push(piece.bounds_px.height);
        }
        widths.sort_by(|a, b| a.total_cmp(b));
        heights.sort_by(|a, b| a.total_cmp(b));
        let mid = widths.len() / 2;
        ([min_w, min_h], [widths[mid], heights[mid]])
    }

    pub fn piece(&self, id: PieceId) -> Option<&PieceRenderGeometry> {
        self.pieces.get(id.as_usize())
    }

    pub fn pose_to_piece_top_left(&self, id: PieceId, pose: Pose2) -> Option<(f32, f32)> {
        let piece = self.piece(id)?;
        Some((
            self.pose_origin_px[0] + pose.x_mm() * self.pose_unit_px[0] - piece.pose_anchor_px[0],
            self.pose_origin_px[1] + pose.y_mm() * self.pose_unit_px[1] - piece.pose_anchor_px[1],
        ))
    }

    pub fn pixel_to_pose(
        &self,
        id: PieceId,
        top_left_px: (f32, f32),
        rotation_deg: f32,
    ) -> Option<Pose2> {
        let piece = self.piece(id)?;
        if self.pose_unit_px[0] <= 0.0 || self.pose_unit_px[1] <= 0.0 {
            return None;
        }
        let x = (top_left_px.0 + piece.pose_anchor_px[0] - self.pose_origin_px[0])
            / self.pose_unit_px[0];
        let y = (top_left_px.1 + piece.pose_anchor_px[1] - self.pose_origin_px[1])
            / self.pose_unit_px[1];
        Pose2::try_from_mm_degrees(x, y, rotation_deg)
    }

    pub fn piece_image_offset(&self, id: PieceId) -> Option<[f32; 2]> {
        let piece = self.piece(id)?;
        Some([-piece.image_origin_px[0], -piece.image_origin_px[1]])
    }

    pub fn hit_test_local_coords(
        &self,
        id: PieceId,
        world_px: (f32, f32),
        top_left_px: (f32, f32),
        rotation_deg: f32,
        flipped: bool,
    ) -> Option<(f32, f32)> {
        let piece = self.piece(id)?;
        let mut local_x = world_px.0 - top_left_px.0;
        let mut local_y = world_px.1 - top_left_px.1;
        if rotation_deg.abs() > f32::EPSILON {
            let rot = if flipped { rotation_deg } else { -rotation_deg };
            let (rx, ry) = rotate_point(
                local_x,
                local_y,
                piece.pose_anchor_px[0],
                piece.pose_anchor_px[1],
                rot,
            );
            local_x = rx;
            local_y = ry;
        }
        if flipped {
            local_x = 2.0 * piece.pose_anchor_px[0] - local_x;
        }
        Some((local_x, local_y))
    }
}

fn rotate_point(x: f32, y: f32, origin_x: f32, origin_y: f32, angle_deg: f32) -> (f32, f32) {
    let theta = angle_deg.to_radians();
    let (sin, cos) = theta.sin_cos();
    let (dx, dy) = (x - origin_x, y - origin_y);
    (
        origin_x + dx * cos - dy * sin,
        origin_y + dx * sin + dy * cos,
    )
}

fn path_bounds(path: &PathMm) -> Option<RectPx> {
    let mut min_x = path.start.x_mm();
    let mut min_y = path.start.y_mm();
    let mut max_x = min_x;
    let mut max_y = min_y;
    for seg in path.segs.iter() {
        match seg {
            PathSegMm::LineTo { to } => {
                include_point(*to, &mut min_x, &mut min_y, &mut max_x, &mut max_y);
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                include_point(*c1, &mut min_x, &mut min_y, &mut max_x, &mut max_y);
                include_point(*c2, &mut min_x, &mut min_y, &mut max_x, &mut max_y);
                include_point(*to, &mut min_x, &mut min_y, &mut max_x, &mut max_y);
            }
        }
    }
    Some(RectPx {
        x: min_x,
        y: min_y,
        width: (max_x - min_x).max(0.0),
        height: (max_y - min_y).max(0.0),
    })
}

fn include_point(
    point: PointMm,
    min_x: &mut f32,
    min_y: &mut f32,
    max_x: &mut f32,
    max_y: &mut f32,
) {
    let x = point.x_mm();
    let y = point.y_mm();
    *min_x = (*min_x).min(x);
    *min_y = (*min_y).min(y);
    *max_x = (*max_x).max(x);
    *max_y = (*max_y).max(y);
}

fn translate_point(point: PointMm, dx: f32, dy: f32) -> Option<PointMm> {
    PointMm::try_from_mm(point.x_mm() + dx, point.y_mm() + dy)
}

fn translate_path(path: &PathMm, dx: f32, dy: f32) -> Option<PathMm> {
    let start = translate_point(path.start, dx, dy)?;
    let mut segs = Vec::with_capacity(path.segs.len());
    for seg in path.segs.iter() {
        match seg {
            PathSegMm::LineTo { to } => {
                segs.push(PathSegMm::LineTo {
                    to: translate_point(*to, dx, dy)?,
                });
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                segs.push(PathSegMm::CubicTo {
                    c1: translate_point(*c1, dx, dy)?,
                    c2: translate_point(*c2, dx, dy)?,
                    to: translate_point(*to, dx, dy)?,
                });
            }
        }
    }
    Some(PathMm::new(start, segs.into_boxed_slice(), path.closed))
}

/// Builds a topology-agnostic `PuzzleRenderGeometry` by walking the
/// topology's piece geometries and resolving each piece's outline +
/// per-edge SVG strings against the supplied shape cache. Used by each
/// topology's `PuzzleTopology::build_render_geometry` impl.
pub fn build_render_geometry_from_atlas<T, F, G>(
    topology: &T,
    atlas: &ShapeAtlasMm,
    image_width: u32,
    image_height: u32,
    pose_unit_px: [f32; 2],
    pose_origin_px: [f32; 2],
    mask_pad_px: f32,
    frame_shape: PuzzleFrameShape,
    canonical_anchor_px: F,
    atlas_to_global_offset: G,
) -> Option<PuzzleRenderGeometry>
where
    T: PuzzleTopology + ?Sized,
    F: Fn(PieceId) -> Option<(f32, f32)>,
    G: Fn(PieceId) -> (f32, f32),
{
    let total = topology.piece_count();
    let mut pieces = Vec::with_capacity(total as usize);
    for id in 0..total {
        let piece = PieceId(id);
        let raw_outline = atlas.piece_outline(piece);
        // Some shapers (grid) produce piece paths in piece-local coords;
        // others (triangular) produce them in image-global coords. The
        // caller tells us how to translate from atlas coords to global
        // image-space coords. From there `bounds` is the piece's global
        // bbox and `local_outline` is the outline relative to that bbox.
        let (gx, gy) = atlas_to_global_offset(piece);
        let outline = translate_path(&raw_outline, gx, gy)?;
        let bounds = path_bounds(&outline)?;
        let local_outline = translate_path(&outline, -bounds.x, -bounds.y)?;
        let piece_geom = atlas
            .pieces
            .get(piece.as_usize())
            .expect("piece id valid for atlas");
        let mut edge_svgs = Vec::with_capacity(piece_geom.edges.len());
        let mut topology_edges = Vec::with_capacity(piece_geom.edges.len());
        for (idx, edge_ref) in piece_geom.edges.iter().enumerate() {
            let edge = atlas.piece_edge_geometry(piece, idx);
            let local_edge = translate_path(&edge.path, gx - bounds.x, gy - bounds.y)?;
            edge_svgs.push(path_to_svg_d(&local_edge));
            topology_edges.push(match edge_ref {
                PieceEdgeRef::Interior { edge, .. } => Some(*edge),
                PieceEdgeRef::Border { .. } => None,
            });
        }
        let (anchor_x, anchor_y) = canonical_anchor_px(piece)?;
        pieces.push(PieceRenderGeometry {
            id: piece,
            bounds_px: bounds,
            pose_anchor_px: [anchor_x - bounds.x, anchor_y - bounds.y],
            image_origin_px: [bounds.x, bounds.y],
            outline_svg: path_to_svg_d(&local_outline),
            edge_svgs,
            topology_edges,
        });
    }
    let (min_piece_extent_px, typical_piece_extent_px) =
        PuzzleRenderGeometry::compute_piece_extents(&pieces);
    Some(PuzzleRenderGeometry {
        image_width,
        image_height,
        puzzle_bounds_px: RectPx {
            x: 0.0,
            y: 0.0,
            width: image_width as f32,
            height: image_height as f32,
        },
        pose_unit_px,
        pose_origin_px,
        pieces,
        mask_pad_px,
        min_piece_extent_px,
        typical_piece_extent_px,
        frame_shape,
    })
}

/// Helper for topologies that want to declare their image extent in
/// pose-unit terms and have the helper derive pose_unit_px.
pub fn pose_unit_px_from_image(image_w: u32, image_h: u32, extent: (f32, f32)) -> [f32; 2] {
    let (xu, yu) = extent;
    let _ = LengthMm::zero(); // keep import in use
    [image_w as f32 / xu.max(1.0), image_h as f32 / yu.max(1.0)]
}
