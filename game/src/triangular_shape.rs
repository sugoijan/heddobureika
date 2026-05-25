//! Board-space shaping for triangular tessellation puzzles.

use std::collections::HashMap;

use crate::edge_compose::ConnectorSeg;
use crate::edge_profile::{
    build_edge_profile_segments, ConnectorShape, EdgeProfileInput, EdgeShapeStyle,
};
use crate::ids::{BorderEdgeId, EdgeId, FrameEdgeId, PieceId};
use crate::shape::{
    BorderEdgeGeometryMm, EdgeSide, EdgeSideGeometryMm, FrameGeometryMm, GeometryInvariantError,
    InteriorEdgeGeometryMm, PathMm, PathSegMm, PieceEdgeRef, PieceGeometryMm,
    PieceGeometryProvider, PointMm, ShapeAtlasMm, TopologyShaper,
};
use crate::topology::{PuzzleTopology, TriangularTessellationTopology};
use crate::units::LengthMm;

const TAB_DEPTH_LIMIT: f32 = 0.24;
const TAB_WIDTH_AVG_MIN: f32 = 0.075;
const TAB_WIDTH_AVG_MAX: f32 = 0.115;
const TAB_DEPTH_AVG_MIN: f32 = 0.055;
const TAB_DEPTH_AVG_MAX: f32 = 0.075;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct TriangularTessellationShapeSettings {
    /// Radius in pose-pixel units of the rounded corners of the
    /// rectangularised mesh boundary. `0.0` keeps the historical
    /// sharp-rect behaviour; positive values snap mesh boundary
    /// vertices onto a rounded-rect path so the puzzle's outer
    /// outline matches the workspace's dashed frame.
    pub corner_radius_px: f32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TriangularTessellationShaper;

#[derive(Clone, Debug, PartialEq)]
pub struct TriangularTessellationShapeCache {
    /// For triangular puzzles this is the solved frame width.
    pub piece_width: LengthMm,
    /// For triangular puzzles this is the solved frame height.
    pub piece_height: LengthMm,
    pub mask_pad: LengthMm,
    pub atlas: ShapeAtlasMm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TriangularTessellationShapeBuildError {
    Invariant(GeometryInvariantError),
    InternalConstruction,
}

impl PieceGeometryProvider for TriangularTessellationShapeCache {
    fn piece_count(&self) -> u32 {
        self.atlas.pieces.len() as u32
    }

    fn piece_geometry(&self, piece: PieceId) -> &PieceGeometryMm {
        self.atlas
            .pieces
            .get(piece.as_usize())
            .expect("piece id should be valid for triangular shape cache")
    }

    fn interior_edge_geometry(&self, edge: EdgeId) -> &InteriorEdgeGeometryMm {
        self.atlas
            .interior_edges
            .get(edge.as_usize())
            .expect("interior edge id should be valid for triangular shape cache")
    }

    fn border_edge_geometry(&self, edge: BorderEdgeId) -> &BorderEdgeGeometryMm {
        self.atlas
            .border_edges
            .get(edge.as_usize())
            .expect("border edge id should be valid for triangular shape cache")
    }

    fn frame_geometry(&self) -> &FrameGeometryMm {
        &self.atlas.frame
    }

    fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm {
        self.atlas.piece_edge_geometry(piece, edge_index)
    }
}

impl TopologyShaper<TriangularTessellationTopology> for TriangularTessellationShaper {
    type Settings = TriangularTessellationShapeSettings;
    type Cache = TriangularTessellationShapeCache;
    type Error = TriangularTessellationShapeBuildError;

    fn build_cache(
        &self,
        topology: &TriangularTessellationTopology,
        piece_width: LengthMm,
        piece_height: LengthMm,
        seed: u32,
        settings: &Self::Settings,
    ) -> Result<Self::Cache, Self::Error> {
        let atlas = build_triangular_atlas(
            topology,
            piece_width,
            piece_height,
            seed,
            settings.corner_radius_px.max(0.0),
        )?;
        atlas
            .validate(topology)
            .map_err(TriangularTessellationShapeBuildError::Invariant)?;
        Ok(TriangularTessellationShapeCache {
            piece_width,
            piece_height,
            mask_pad: LengthMm::try_new(
                piece_width.as_mm_f32().min(piece_height.as_mm_f32()) * 0.015,
            )
            .unwrap_or_default(),
            atlas,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Point2 {
    x: f32,
    y: f32,
}

#[derive(Clone, Debug)]
struct Mesh {
    vertices: Vec<Point2>,
    faces: Vec<[usize; 3]>,
}

fn build_triangular_atlas(
    topology: &TriangularTessellationTopology,
    frame_width: LengthMm,
    frame_height: LengthMm,
    seed: u32,
    corner_radius_px: f32,
) -> Result<ShapeAtlasMm, TriangularTessellationShapeBuildError> {
    // The topology already carries an exact equilateral lattice in pose
    // units; scale it UNIFORMLY into the frame (pose units are square) and
    // feed it straight to the generic faces→atlas builder. No honeycomb
    // construction, no rectangularization, no relaxation — interior pieces
    // stay exactly equilateral.
    let (verts, faces) = topology.lattice_geometry();
    let (ex, _ey) = topology.pose_extent();
    let scale = if ex > 0.0 {
        frame_width.as_mm_f32() / ex
    } else {
        1.0
    };
    let mut mesh = Mesh {
        vertices: verts
            .iter()
            .map(|&(x, y)| Point2 {
                x: x * scale,
                y: y * scale,
            })
            .collect(),
        faces: faces
            .iter()
            .map(|f| [f[0] as usize, f[1] as usize, f[2] as usize])
            .collect(),
    };
    // Round the four rectangle corners: pull the single mesh vertex sitting on
    // each corner onto that corner's arc midpoint. Because the vertex is shared
    // by both border edges AND the interior hypotenuse meeting there, moving it
    // as one keeps every piece ring closed; the arc↔straight routing in
    // `build_atlas_from_mesh` then draws the rounded edges automatically.
    snap_corner_vertices(
        &mut mesh,
        frame_width.as_mm_f32(),
        frame_height.as_mm_f32(),
        corner_radius_px,
    );
    build_atlas_from_mesh(
        topology,
        &mesh,
        seed,
        frame_width.as_mm_f32(),
        frame_height.as_mm_f32(),
        corner_radius_px,
    )
}

/// Pulls every mesh vertex that lies exactly on a rectangle corner onto that
/// corner's rounded-arc midpoint (the 45° point), so the corner reads as a
/// smooth round rather than a sharp tip. The offset `r·(1 − √2/2)` along each
/// axis lands the vertex exactly on the corner arc of radius `r`, where the
/// `arc_index` classifier picks it up. No-op when `radius <= 0`.
fn snap_corner_vertices(mesh: &mut Mesh, width: f32, height: f32, radius: f32) {
    if radius <= 0.0 {
        return;
    }
    let k = radius * (1.0 - std::f32::consts::FRAC_1_SQRT_2);
    let tol = (radius * 0.08).max(1.0e-3);
    let near = |a: f32, b: f32| (a - b).abs() <= tol;
    for v in mesh.vertices.iter_mut() {
        // `sx`/`sy`: which way to push off each corner-aligned axis (+1 from a
        // min edge, −1 from a max edge); `None` means not on that edge.
        let sx = if near(v.x, 0.0) {
            Some((0.0, 1.0))
        } else if near(v.x, width) {
            Some((width, -1.0))
        } else {
            None
        };
        let sy = if near(v.y, 0.0) {
            Some((0.0, 1.0))
        } else if near(v.y, height) {
            Some((height, -1.0))
        } else {
            None
        };
        if let (Some((cx, sx)), Some((cy, sy))) = (sx, sy) {
            v.x = cx + sx * k;
            v.y = cy + sy * k;
        }
    }
}

fn build_atlas_from_mesh(
    topology: &TriangularTessellationTopology,
    mesh: &Mesh,
    seed: u32,
    frame_width: f32,
    frame_height: f32,
    corner_radius_px: f32,
) -> Result<ShapeAtlasMm, TriangularTessellationShapeBuildError> {
    // Each side of the rounded boundary is `half-arc → straight →
    // half-arc`. Two consecutive boundary vertices that both land on
    // the same corner arc lie on an arc segment of at most 45°; a
    // single cubic Bézier reproduces such an arc with error far below
    // a pixel (max ~0.0002 of the radius for a 45° span), so we emit
    // one cubic per such pair instead of a straight chord. This makes
    // the rounded corners look like proper curves even when the mesh
    // boundary has only one or two vertices per arc.
    let arc_classifier = if corner_radius_px > 0.0 {
        Some(BoundaryArcClassifier::new(
            frame_width,
            frame_height,
            corner_radius_px,
        ))
    } else {
        None
    };
    let piece_count = topology.piece_count() as usize;
    let edge_count = topology.edge_count() as usize;

    let mut topology_edges = HashMap::<(PieceId, PieceId), (EdgeId, EdgeSide)>::new();
    for edge_idx in 0..edge_count {
        let edge = EdgeId(edge_idx as u32);
        let (a, b) = topology.edge_endpoints(edge);
        topology_edges.insert((a, b), (edge, EdgeSide::A));
        topology_edges.insert((b, a), (edge, EdgeSide::B));
    }

    let mut side_owners = HashMap::<(usize, usize), Vec<(PieceId, usize, usize, usize)>>::new();
    for piece_idx in 0..piece_count {
        let piece = PieceId(piece_idx as u32);
        let face = mesh.faces[piece_idx];
        for side_idx in 0..3 {
            let start = face[side_idx];
            let end = face[(side_idx + 1) % 3];
            let key = ordered_edge_key(start, end);
            side_owners
                .entry(key)
                .or_default()
                .push((piece, side_idx, start, end));
        }
    }
    let average_edge_len = average_interior_edge_len(mesh, &side_owners);

    let mut piece_edges = vec![vec![None::<PieceEdgeRef>; 3]; piece_count];
    let mut interior_sides =
        vec![(None::<EdgeSideGeometryMm>, None::<EdgeSideGeometryMm>); edge_count];
    let mut border_edges = Vec::<BorderEdgeGeometryMm>::new();
    let mut frame_edges = Vec::<PathMm>::new();

    for owners in side_owners.values() {
        match owners.as_slice() {
            [(piece, side_idx, start, end)] => {
                let border = BorderEdgeId(border_edges.len() as u32);
                let frame = FrameEdgeId(frame_edges.len() as u32);
                let start_pt = mesh.vertices[*start];
                let end_pt = mesh.vertices[*end];
                let path = match arc_classifier.as_ref() {
                    Some(c) => c.path_along_boundary(start_pt, end_pt)?,
                    None => straight_path(start_pt, end_pt)?,
                };
                let side = side_geometry_from_path(path.clone())?;
                frame_edges.push(path);
                border_edges.push(BorderEdgeGeometryMm {
                    piece: *piece,
                    side: side.clone(),
                    frame_edge: frame,
                });
                piece_edges[piece.as_usize()][*side_idx] =
                    Some(PieceEdgeRef::Border { edge: border });
            }
            [(piece_a, side_idx_a, start_a, end_a), (piece_b, side_idx_b, start_b, end_b)] => {
                let (edge, side_a_label) = topology_edges
                    .get(&(*piece_a, *piece_b))
                    .copied()
                    .ok_or(TriangularTessellationShapeBuildError::InternalConstruction)?;
                let side_b_label = match side_a_label {
                    EdgeSide::A => EdgeSide::B,
                    EdgeSide::B => EdgeSide::A,
                };

                let path_a = profiled_path(
                    mesh.vertices[*start_a],
                    mesh.vertices[*end_a],
                    seed,
                    edge,
                    average_edge_len,
                    frame_width,
                    frame_height,
                )?;
                let path_b = if mesh.vertices[*start_b] == mesh.vertices[*end_a]
                    && mesh.vertices[*end_b] == mesh.vertices[*start_a]
                {
                    reverse_path(&path_a)
                } else {
                    profiled_path(
                        mesh.vertices[*start_b],
                        mesh.vertices[*end_b],
                        seed,
                        edge,
                        average_edge_len,
                        frame_width,
                        frame_height,
                    )?
                };
                let side_a = side_geometry_from_path(path_a)?;
                let side_b = side_geometry_from_path(path_b)?;

                match side_a_label {
                    EdgeSide::A => {
                        interior_sides[edge.as_usize()].0 = Some(side_a);
                        interior_sides[edge.as_usize()].1 = Some(side_b);
                    }
                    EdgeSide::B => {
                        interior_sides[edge.as_usize()].0 = Some(side_b);
                        interior_sides[edge.as_usize()].1 = Some(side_a);
                    }
                }
                piece_edges[piece_a.as_usize()][*side_idx_a] = Some(PieceEdgeRef::Interior {
                    edge,
                    side: side_a_label,
                });
                piece_edges[piece_b.as_usize()][*side_idx_b] = Some(PieceEdgeRef::Interior {
                    edge,
                    side: side_b_label,
                });
            }
            _ => {
                return Err(TriangularTessellationShapeBuildError::InternalConstruction);
            }
        }
    }

    let mut pieces = Vec::<PieceGeometryMm>::with_capacity(piece_count);
    for ring in piece_edges {
        let mut edges = Vec::<PieceEdgeRef>::with_capacity(3);
        for edge in ring {
            edges.push(edge.ok_or(TriangularTessellationShapeBuildError::InternalConstruction)?);
        }
        pieces.push(PieceGeometryMm {
            edges: edges.into_boxed_slice(),
        });
    }

    let mut interior_edges = Vec::with_capacity(edge_count);
    for edge_idx in 0..edge_count {
        let edge = EdgeId(edge_idx as u32);
        let (a, b) = topology.edge_endpoints(edge);
        let (side_a, side_b) = std::mem::take(&mut interior_sides[edge_idx]);
        interior_edges.push(InteriorEdgeGeometryMm {
            endpoints: (a, b),
            side_a: side_a.ok_or(TriangularTessellationShapeBuildError::InternalConstruction)?,
            side_b: side_b.ok_or(TriangularTessellationShapeBuildError::InternalConstruction)?,
        });
    }

    Ok(ShapeAtlasMm {
        pieces: pieces.into_boxed_slice(),
        interior_edges: interior_edges.into_boxed_slice(),
        border_edges: border_edges.into_boxed_slice(),
        frame: FrameGeometryMm {
            edges: frame_edges.into_boxed_slice(),
        },
    })
}

fn average_interior_edge_len(
    mesh: &Mesh,
    side_owners: &HashMap<(usize, usize), Vec<(PieceId, usize, usize, usize)>>,
) -> f32 {
    let mut total = 0.0_f32;
    let mut count = 0_u32;
    for owners in side_owners.values() {
        if let [(_, _, start, end), (_, _, _, _)] = owners.as_slice() {
            total += distance(mesh.vertices[*start], mesh.vertices[*end]);
            count += 1;
        }
    }
    if count == 0 {
        1.0
    } else {
        total / count as f32
    }
}

fn straight_path(
    start: Point2,
    end: Point2,
) -> Result<PathMm, TriangularTessellationShapeBuildError> {
    Ok(PathMm::new(
        point(start)?,
        vec![PathSegMm::LineTo { to: point(end)? }].into_boxed_slice(),
        false,
    ))
}

/// Knows where the four corner arcs of the rounded-rect frame sit and
/// can build a `PathMm` that exactly traces the boundary between two
/// adjacent boundary vertices — whether that's a single arc segment, a
/// single straight chord, or a composite that crosses an arc↔straight
/// transition.
///
/// Without the composite case, a border edge whose endpoints straddle
/// the arc/straight transition would shortcut from "somewhere on the
/// arc" to "somewhere on the straight side" via a chord, and that
/// chord cuts *inside* the rounded boundary — visually the corner
/// "tapers inward" rather than curving smoothly.
#[derive(Clone, Copy, Debug)]
struct BoundaryArcClassifier {
    arcs: [ArcInfo; 4],
    width: f32,
    height: f32,
    radius: f32,
    /// Slack on both the radial distance test and the quadrant check.
    /// Relaxation can nudge boundary vertices by floating-point noise;
    /// a few percent of the radius leaves headroom without admitting
    /// vertices that genuinely lie on a straight section.
    tolerance: f32,
}

#[derive(Clone, Copy, Debug)]
struct ArcInfo {
    center: Point2,
    /// Two transition points where this arc meets the adjacent
    /// straight sections (e.g. TL arc → `(0, r)` and `(r, 0)`).
    transitions: [Point2; 2],
    /// Which corner this is, used for the quadrant containment check.
    corner: Corner,
}

#[derive(Clone, Copy, Debug)]
enum Corner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

#[derive(Clone, Copy, Debug)]
enum FrameSide {
    Top,
    Bottom,
    Left,
    Right,
}

impl BoundaryArcClassifier {
    fn new(width: f32, height: f32, radius: f32) -> Self {
        Self {
            arcs: [
                ArcInfo {
                    center: Point2 {
                        x: radius,
                        y: radius,
                    },
                    transitions: [Point2 { x: 0.0, y: radius }, Point2 { x: radius, y: 0.0 }],
                    corner: Corner::TopLeft,
                },
                ArcInfo {
                    center: Point2 {
                        x: width - radius,
                        y: radius,
                    },
                    transitions: [
                        Point2 {
                            x: width - radius,
                            y: 0.0,
                        },
                        Point2 {
                            x: width,
                            y: radius,
                        },
                    ],
                    corner: Corner::TopRight,
                },
                ArcInfo {
                    center: Point2 {
                        x: width - radius,
                        y: height - radius,
                    },
                    transitions: [
                        Point2 {
                            x: width,
                            y: height - radius,
                        },
                        Point2 {
                            x: width - radius,
                            y: height,
                        },
                    ],
                    corner: Corner::BottomRight,
                },
                ArcInfo {
                    center: Point2 {
                        x: radius,
                        y: height - radius,
                    },
                    transitions: [
                        Point2 {
                            x: radius,
                            y: height,
                        },
                        Point2 {
                            x: 0.0,
                            y: height - radius,
                        },
                    ],
                    corner: Corner::BottomLeft,
                },
            ],
            width,
            height,
            radius,
            tolerance: (radius * 0.08).max(1.0e-3),
        }
    }

    fn in_arc_quadrant(&self, p: Point2, arc: &ArcInfo) -> bool {
        let tol = self.tolerance;
        match arc.corner {
            Corner::TopLeft => p.x <= self.radius + tol && p.y <= self.radius + tol,
            Corner::TopRight => p.x >= self.width - self.radius - tol && p.y <= self.radius + tol,
            Corner::BottomRight => {
                p.x >= self.width - self.radius - tol && p.y >= self.height - self.radius - tol
            }
            Corner::BottomLeft => {
                p.x <= self.radius + tol && p.y >= self.height - self.radius - tol
            }
        }
    }

    /// Returns the arc index `[0..4]` (TL/TR/BR/BL) that this point
    /// sits on, or `None` if it lies on a straight side or off-path.
    fn arc_index(&self, p: Point2) -> Option<usize> {
        for (idx, arc) in self.arcs.iter().enumerate() {
            if !self.in_arc_quadrant(p, arc) {
                continue;
            }
            let dx = p.x - arc.center.x;
            let dy = p.y - arc.center.y;
            let dist = (dx * dx + dy * dy).sqrt();
            if (dist - self.radius).abs() <= self.tolerance {
                return Some(idx);
            }
        }
        None
    }

    /// Builds the path along the rounded boundary from `a` to `b`,
    /// handling arc-arc, straight-straight, and arc↔straight
    /// transitions as composite segments.
    fn path_along_boundary(
        &self,
        a: Point2,
        b: Point2,
    ) -> Result<PathMm, TriangularTessellationShapeBuildError> {
        let arc_a = self.arc_index(a);
        let arc_b = self.arc_index(b);
        match (arc_a, arc_b) {
            (Some(ia), Some(ib)) if ia == ib => {
                // Both on the same arc — single cubic.
                arc_cubic_path(a, b, self.arcs[ia].center)
            }
            (Some(ia), None) => {
                // `a` is on an arc, `b` is on a straight section. The
                // arc/straight transition is whichever of the arc's two
                // endpoints is closer to `b`.
                let transition = nearest_transition(&self.arcs[ia], b);
                self.compose_arc_then_line(a, transition, self.arcs[ia].center, b)
            }
            (None, Some(ib)) => {
                let transition = nearest_transition(&self.arcs[ib], a);
                self.compose_line_then_arc(a, transition, b, self.arcs[ib].center)
            }
            (Some(_), Some(_)) => {
                // Two arcs in one chain edge would mean the edge spans
                // an entire straight section. Mesh boundary chains are
                // dense enough that this shouldn't happen in practice;
                // fall back to a chord if it ever does.
                straight_path(a, b)
            }
            (None, None) => {
                // Both endpoints on straight sections. If they're on the
                // SAME side this is a normal flat edge (chord). If they're
                // on adjacent sides, the edge spans a corner (a corner piece
                // touching two frame edges) and must wrap the rounded corner
                // instead of cutting straight across it.
                match self.corner_arc_between(a, b) {
                    Some(arc_idx) => {
                        let arc = &self.arcs[arc_idx];
                        let t_a = nearest_transition(arc, a);
                        let t_b = if t_a == arc.transitions[0] {
                            arc.transitions[1]
                        } else {
                            arc.transitions[0]
                        };
                        self.compose_line_arc_line(a, t_a, t_b, arc.center, b)
                    }
                    None => straight_path(a, b),
                }
            }
        }
    }

    /// Identifies the corner arc lying between two boundary points that
    /// sit on different (adjacent) frame edges, or `None` when they share
    /// an edge / aren't on a corner-spanning chord. Arc order is fixed by
    /// `new`: TL=0, TR=1, BR=2, BL=3.
    fn corner_arc_between(&self, a: Point2, b: Point2) -> Option<usize> {
        match (self.frame_side(a)?, self.frame_side(b)?) {
            (FrameSide::Top, FrameSide::Left) | (FrameSide::Left, FrameSide::Top) => Some(0),
            (FrameSide::Top, FrameSide::Right) | (FrameSide::Right, FrameSide::Top) => Some(1),
            (FrameSide::Bottom, FrameSide::Right) | (FrameSide::Right, FrameSide::Bottom) => {
                Some(2)
            }
            (FrameSide::Bottom, FrameSide::Left) | (FrameSide::Left, FrameSide::Bottom) => Some(3),
            _ => None,
        }
    }

    /// Which frame edge a (snapped) boundary point lies on.
    fn frame_side(&self, p: Point2) -> Option<FrameSide> {
        let tol = self.tolerance.max(1.0e-3);
        if p.y.abs() <= tol {
            Some(FrameSide::Top)
        } else if (p.y - self.height).abs() <= tol {
            Some(FrameSide::Bottom)
        } else if p.x.abs() <= tol {
            Some(FrameSide::Left)
        } else if (p.x - self.width).abs() <= tol {
            Some(FrameSide::Right)
        } else {
            None
        }
    }

    fn compose_line_arc_line(
        &self,
        start: Point2,
        t_a: Point2,
        t_b: Point2,
        arc_center: Point2,
        end: Point2,
    ) -> Result<PathMm, TriangularTessellationShapeBuildError> {
        let mut segs: Vec<PathSegMm> = Vec::with_capacity(3);
        segs.push(PathSegMm::LineTo { to: point(t_a)? });
        if !push_arc_cubic_seg(&mut segs, t_a, t_b, arc_center)? {
            segs.push(PathSegMm::LineTo { to: point(t_b)? });
        }
        segs.push(PathSegMm::LineTo { to: point(end)? });
        Ok(PathMm::new(point(start)?, segs.into_boxed_slice(), false))
    }

    fn compose_arc_then_line(
        &self,
        arc_start: Point2,
        transition: Point2,
        arc_center: Point2,
        line_end: Point2,
    ) -> Result<PathMm, TriangularTessellationShapeBuildError> {
        let mut segs: Vec<PathSegMm> = Vec::with_capacity(2);
        push_arc_cubic_seg(&mut segs, arc_start, transition, arc_center)?;
        segs.push(PathSegMm::LineTo {
            to: point(line_end)?,
        });
        Ok(PathMm::new(
            point(arc_start)?,
            segs.into_boxed_slice(),
            false,
        ))
    }

    fn compose_line_then_arc(
        &self,
        line_start: Point2,
        transition: Point2,
        arc_end: Point2,
        arc_center: Point2,
    ) -> Result<PathMm, TriangularTessellationShapeBuildError> {
        let mut segs: Vec<PathSegMm> = Vec::with_capacity(2);
        segs.push(PathSegMm::LineTo {
            to: point(transition)?,
        });
        push_arc_cubic_seg(&mut segs, transition, arc_end, arc_center)?;
        Ok(PathMm::new(
            point(line_start)?,
            segs.into_boxed_slice(),
            false,
        ))
    }
}

fn nearest_transition(arc: &ArcInfo, reference: Point2) -> Point2 {
    let d0 = distance(arc.transitions[0], reference);
    let d1 = distance(arc.transitions[1], reference);
    if d0 <= d1 {
        arc.transitions[0]
    } else {
        arc.transitions[1]
    }
}

/// Builds a `PathMm` that traces the short arc from `start` to `end`
/// on the circle centred at `center`. The path is a single cubic
/// Bézier (or a straight chord if the sweep is degenerate).
fn arc_cubic_path(
    start: Point2,
    end: Point2,
    center: Point2,
) -> Result<PathMm, TriangularTessellationShapeBuildError> {
    let mut segs = Vec::with_capacity(1);
    if !push_arc_cubic_seg(&mut segs, start, end, center)? {
        return straight_path(start, end);
    }
    Ok(PathMm::new(point(start)?, segs.into_boxed_slice(), false))
}

/// Appends a single cubic-Bézier `PathSegMm::CubicTo` approximating
/// the short arc from `start` to `end` around `center`. Returns
/// `Ok(true)` if a segment was appended, `Ok(false)` if the sweep is
/// effectively zero (caller should emit a straight line instead).
///
/// Uses the standard `α = (4/3) · tan(sweep/4)` arc-to-Bézier
/// formula. Maximum radial error is ≈ `0.0002 · radius` for sweeps up
/// to 45°, well under a pixel even at large image sizes.
fn push_arc_cubic_seg(
    out: &mut Vec<PathSegMm>,
    start: Point2,
    end: Point2,
    center: Point2,
) -> Result<bool, TriangularTessellationShapeBuildError> {
    let theta1 = (start.y - center.y).atan2(start.x - center.x);
    let theta2 = (end.y - center.y).atan2(end.x - center.x);
    let mut sweep = theta2 - theta1;
    if sweep > std::f32::consts::PI {
        sweep -= 2.0 * std::f32::consts::PI;
    } else if sweep <= -std::f32::consts::PI {
        sweep += 2.0 * std::f32::consts::PI;
    }
    if sweep.abs() <= 1.0e-6 {
        return Ok(false);
    }
    let alpha = (4.0 / 3.0) * (sweep / 4.0).tan();
    // Unit tangent at θ is `(-sin θ, cos θ)` (CCW). When `sweep < 0`,
    // `alpha` flips sign, which flips the control points to the CW
    // side — so the same formula handles both directions.
    let tan_start = Point2 {
        x: -(start.y - center.y),
        y: start.x - center.x,
    };
    let tan_end = Point2 {
        x: -(end.y - center.y),
        y: end.x - center.x,
    };
    let c1 = Point2 {
        x: start.x + alpha * tan_start.x,
        y: start.y + alpha * tan_start.y,
    };
    let c2 = Point2 {
        x: end.x - alpha * tan_end.x,
        y: end.y - alpha * tan_end.y,
    };
    out.push(PathSegMm::CubicTo {
        c1: point(c1)?,
        c2: point(c2)?,
        to: point(end)?,
    });
    Ok(true)
}

fn profiled_path(
    start: Point2,
    end: Point2,
    seed: u32,
    edge: EdgeId,
    average_edge_len: f32,
    frame_width: f32,
    frame_height: f32,
) -> Result<PathMm, TriangularTessellationShapeBuildError> {
    let len = distance(start, end);
    if len <= 1.0e-5 {
        return straight_path(start, end);
    }
    let params = connector_for_edge(seed, edge, len, average_edge_len);
    let preferred_sign = if splitmix32(seed ^ edge.as_u32() ^ 0xD1CE_BA5E) & 1 == 0 {
        1
    } else {
        -1
    };
    for sign in [preferred_sign, -preferred_sign] {
        let input = EdgeProfileInput {
            len_mm: len,
            depth_base_mm: average_edge_len,
            depth_limit_mm: TAB_DEPTH_LIMIT,
            sign,
            connector: Some(params),
        };
        let segments = build_edge_profile_segments(EdgeShapeStyle::Classic, &input);
        if let Ok(Some(path)) =
            path_from_connector_segments_in_frame(start, end, &segments, frame_width, frame_height)
        {
            return Ok(path);
        }
    }
    straight_path(start, end)
}

fn path_from_connector_segments_in_frame(
    start: Point2,
    end: Point2,
    segments: &[ConnectorSeg],
    frame_width: f32,
    frame_height: f32,
) -> Result<Option<PathMm>, TriangularTessellationShapeBuildError> {
    if !connector_segments_fit_frame(start, end, segments, frame_width, frame_height) {
        return Ok(None);
    }
    path_from_connector_segments(start, end, segments).map(Some)
}

fn connector_segments_fit_frame(
    start: Point2,
    end: Point2,
    segments: &[ConnectorSeg],
    frame_width: f32,
    frame_height: f32,
) -> bool {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len <= 1.0e-5 {
        return true;
    }
    let tx = dx / len;
    let ty = dy / len;
    let nx = -ty;
    let ny = tx;
    for segment in segments {
        let points: &[(f32, f32)] = match segment {
            ConnectorSeg::LineTo { to } => std::slice::from_ref(to),
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let candidates = [*c1, *c2, *to];
                for local in candidates {
                    let point = map_local(start, tx, ty, nx, ny, local);
                    if !point_in_frame(point, frame_width, frame_height) {
                        return false;
                    }
                }
                continue;
            }
        };
        for local in points {
            let point = map_local(start, tx, ty, nx, ny, *local);
            if !point_in_frame(point, frame_width, frame_height) {
                return false;
            }
        }
    }
    true
}

fn point_in_frame(point: Point2, frame_width: f32, frame_height: f32) -> bool {
    let epsilon = 0.01;
    point.x >= -epsilon
        && point.y >= -epsilon
        && point.x <= frame_width + epsilon
        && point.y <= frame_height + epsilon
}

fn connector_for_edge(seed: u32, edge: EdgeId, len: f32, average_edge_len: f32) -> ConnectorShape {
    let base = seed ^ edge.as_u32().wrapping_mul(0x9E37_79B9);
    let average = average_edge_len.max(1.0e-5);
    let tab_width = average * rand_range(base, 0, TAB_WIDTH_AVG_MIN, TAB_WIDTH_AVG_MAX);
    let tab_depth = average * rand_range(base, 1, TAB_DEPTH_AVG_MIN, TAB_DEPTH_AVG_MAX);
    let tab_size = (tab_width / len.max(1.0e-5)).clamp(0.04, 0.24);
    let tab_depth = (tab_depth / (average * tab_size).max(1.0e-5)).clamp(0.2, 1.4);
    let jitter_depth = 0.015;
    let jitter_len = (average * 0.025 / len.max(1.0e-5)).clamp(0.0, 0.05);
    ConnectorShape {
        tab_size,
        tab_depth,
        a: rand_range(base, 2, -jitter_depth, jitter_depth),
        b: rand_range(base, 3, -jitter_len, jitter_len),
        c: rand_range(base, 4, -jitter_depth, jitter_depth),
        d: rand_range(base, 5, -jitter_len, jitter_len),
        e: rand_range(base, 6, -jitter_depth, jitter_depth),
    }
}

fn path_from_connector_segments(
    start: Point2,
    end: Point2,
    segments: &[ConnectorSeg],
) -> Result<PathMm, TriangularTessellationShapeBuildError> {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len <= 1.0e-5 {
        return straight_path(start, end);
    }
    let tx = dx / len;
    let ty = dy / len;
    let nx = -ty;
    let ny = tx;
    let mut path_segments = Vec::<PathSegMm>::with_capacity(segments.len());
    for segment in segments {
        match *segment {
            ConnectorSeg::LineTo { to } => {
                path_segments.push(PathSegMm::LineTo {
                    to: point(map_local(start, tx, ty, nx, ny, to))?,
                });
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                path_segments.push(PathSegMm::CubicTo {
                    c1: point(map_local(start, tx, ty, nx, ny, c1))?,
                    c2: point(map_local(start, tx, ty, nx, ny, c2))?,
                    to: point(map_local(start, tx, ty, nx, ny, to))?,
                });
            }
        }
    }
    let mut path = PathMm::new(point(start)?, path_segments.into_boxed_slice(), false);
    set_path_end_point(&mut path, point(end)?);
    Ok(path)
}

fn reverse_path(path: &PathMm) -> PathMm {
    let mut states = Vec::<(PointMm, PathSegMm, PointMm)>::with_capacity(path.segs.len());
    let mut current = path.start;
    for segment in path.segs.iter() {
        let segment_start = current;
        let segment = segment.clone();
        match segment {
            PathSegMm::LineTo { to } => {
                current = to;
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                let _ = (c1, c2);
                current = to;
            }
        }
        states.push((segment_start, segment, current));
    }

    let mut segs = Vec::<PathSegMm>::with_capacity(states.len());
    for (start, segment, _) in states.into_iter().rev() {
        match segment {
            PathSegMm::LineTo { .. } => segs.push(PathSegMm::LineTo { to: start }),
            PathSegMm::CubicTo { c1, c2, .. } => segs.push(PathSegMm::CubicTo {
                c1: c2,
                c2: c1,
                to: start,
            }),
        }
    }
    PathMm::new(current, segs.into_boxed_slice(), false)
}

fn side_geometry_from_path(
    path: PathMm,
) -> Result<EdgeSideGeometryMm, TriangularTessellationShapeBuildError> {
    let sampled = sample_path(&path, 12);
    let mut sx = 0.0;
    let mut sy = 0.0;
    for p in &sampled {
        sx += p.x;
        sy += p.y;
    }
    let denom = sampled.len().max(1) as f32;
    Ok(EdgeSideGeometryMm {
        path,
        connection_point: point(Point2 {
            x: sx / denom,
            y: sy / denom,
        })?,
    })
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

fn sample_path(path: &PathMm, cubic_steps: usize) -> Vec<Point2> {
    let mut out = vec![Point2 {
        x: path.start.x_mm(),
        y: path.start.y_mm(),
    }];
    let mut current = out[0];
    let steps = cubic_steps.max(1);
    for seg in path.segs.iter() {
        match *seg {
            PathSegMm::LineTo { to } => {
                current = Point2 {
                    x: to.x_mm(),
                    y: to.y_mm(),
                };
                out.push(current);
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                let p1 = Point2 {
                    x: c1.x_mm(),
                    y: c1.y_mm(),
                };
                let p2 = Point2 {
                    x: c2.x_mm(),
                    y: c2.y_mm(),
                };
                let p3 = Point2 {
                    x: to.x_mm(),
                    y: to.y_mm(),
                };
                for step in 1..=steps {
                    out.push(cubic_point(current, p1, p2, p3, step as f32 / steps as f32));
                }
                current = p3;
            }
        }
    }
    out
}

fn cubic_point(p0: Point2, p1: Point2, p2: Point2, p3: Point2, t: f32) -> Point2 {
    let u = 1.0 - t;
    let tt = t * t;
    let uu = u * u;
    let uuu = uu * u;
    let ttt = tt * t;
    Point2 {
        x: uuu * p0.x + 3.0 * uu * t * p1.x + 3.0 * u * tt * p2.x + ttt * p3.x,
        y: uuu * p0.y + 3.0 * uu * t * p1.y + 3.0 * u * tt * p2.y + ttt * p3.y,
    }
}

fn map_local(start: Point2, tx: f32, ty: f32, nx: f32, ny: f32, local: (f32, f32)) -> Point2 {
    Point2 {
        x: start.x + tx * local.0 + nx * local.1,
        y: start.y + ty * local.0 + ny * local.1,
    }
}

fn point(value: Point2) -> Result<PointMm, TriangularTessellationShapeBuildError> {
    let x = if value.x > -0.01 && value.x < 0.0 {
        0.0
    } else {
        value.x
    };
    let y = if value.y > -0.01 && value.y < 0.0 {
        0.0
    } else {
        value.y
    };
    PointMm::try_from_mm(x, y).ok_or(TriangularTessellationShapeBuildError::InternalConstruction)
}

fn ordered_edge_key(a: usize, b: usize) -> (usize, usize) {
    if a <= b {
        (a, b)
    } else {
        (b, a)
    }
}

fn distance(a: Point2, b: Point2) -> f32 {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    (dx * dx + dy * dy).sqrt()
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
