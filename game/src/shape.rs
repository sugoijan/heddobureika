//! Canonical renderer-agnostic piece geometry types.

use crate::ids::{BorderEdgeId, EdgeId, FrameEdgeId, PieceId};
pub use crate::traits::shaping::{PieceGeometryProvider, TopologyShaper};
use crate::traits::topology::PuzzleTopology;
use crate::units::LengthMm;

/// Piece-local point in millimeters.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct PointMm {
    pub x: LengthMm,
    pub y: LengthMm,
}

impl PointMm {
    pub fn try_from_mm(x_mm: f32, y_mm: f32) -> Option<Self> {
        Some(Self {
            x: LengthMm::try_new(x_mm)?,
            y: LengthMm::try_new(y_mm)?,
        })
    }

    pub fn x_mm(self) -> f32 {
        self.x.as_mm_f32()
    }

    pub fn y_mm(self) -> f32 {
        self.y.as_mm_f32()
    }
}

/// Path segment in piece-local millimeter coordinates.
#[derive(Clone, Debug, PartialEq)]
pub enum PathSegMm {
    LineTo {
        to: PointMm,
    },
    CubicTo {
        c1: PointMm,
        c2: PointMm,
        to: PointMm,
    },
}

/// Open or closed piece-local path.
#[derive(Clone, Debug, PartialEq)]
pub struct PathMm {
    pub start: PointMm,
    pub segs: Box<[PathSegMm]>,
    pub closed: bool,
}

impl PathMm {
    pub fn new(start: PointMm, segs: Box<[PathSegMm]>, closed: bool) -> Self {
        Self {
            start,
            segs,
            closed,
        }
    }
}

/// Side label for an interior topology edge endpoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeSide {
    A,
    B,
}

/// Piece-local geometry for one directed side of an edge.
#[derive(Clone, Debug, PartialEq)]
pub struct EdgeSideGeometryMm {
    pub path: PathMm,
    pub connection_point: PointMm,
}

/// Canonical atlas entry for one interior topology edge.
#[derive(Clone, Debug, PartialEq)]
pub struct InteriorEdgeGeometryMm {
    pub endpoints: (PieceId, PieceId),
    pub side_a: EdgeSideGeometryMm,
    pub side_b: EdgeSideGeometryMm,
}

/// Canonical atlas entry for one boundary edge owned by one piece.
#[derive(Clone, Debug, PartialEq)]
pub struct BorderEdgeGeometryMm {
    pub piece: PieceId,
    pub side: EdgeSideGeometryMm,
    pub frame_edge: FrameEdgeId,
}

/// Piece-ring edge references in clockwise order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PieceEdgeRef {
    Interior { edge: EdgeId, side: EdgeSide },
    Border { edge: BorderEdgeId },
}

/// Canonical geometry for a single puzzle piece represented as a clockwise
/// closed ring of edge references.
#[derive(Clone, Debug, PartialEq)]
pub struct PieceGeometryMm {
    pub edges: Box<[PieceEdgeRef]>,
}

/// Canonical border frame geometry represented as a clockwise closed ring.
#[derive(Clone, Debug, PartialEq)]
pub struct FrameGeometryMm {
    pub edges: Box<[PathMm]>,
}

/// Canonical shape atlas for all piece and edge geometry.
#[derive(Clone, Debug, PartialEq)]
pub struct ShapeAtlasMm {
    pub pieces: Box<[PieceGeometryMm]>,
    pub interior_edges: Box<[InteriorEdgeGeometryMm]>,
    pub border_edges: Box<[BorderEdgeGeometryMm]>,
    pub frame: FrameGeometryMm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GeometryInvariantError {
    PieceCountMismatch,
    InteriorEdgeCountMismatch,
    InteriorEdgeEndpointsMismatch,
    InteriorEdgeReferenceCountMismatch,
    BorderEdgeRefOutOfRange,
    FrameEdgeRefOutOfRange,
    FrameEdgeUsageMismatch,
    PieceRingEmpty,
    PieceRingNotContiguous,
    PieceRingNotClosed,
}

impl ShapeAtlasMm {
    /// Returns the geometry for one referenced piece edge.
    pub fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm {
        let piece_geom = self
            .pieces
            .get(piece.as_usize())
            .expect("piece id should be valid for atlas");
        let edge_ref = piece_geom
            .edges
            .get(edge_index)
            .expect("edge index should be valid for piece ring");
        self.resolve_piece_edge_ref(*edge_ref)
    }

    /// Derives a closed piece outline from the piece ring.
    pub fn piece_outline(&self, piece: PieceId) -> PathMm {
        let piece_geom = self
            .pieces
            .get(piece.as_usize())
            .expect("piece id should be valid for atlas");
        let first_ref = *piece_geom
            .edges
            .first()
            .expect("piece ring should not be empty");
        let first = self.resolve_piece_edge_ref(first_ref);
        let mut segs = Vec::new();
        segs.extend(first.path.segs.iter().cloned());

        for edge_ref in piece_geom.edges.iter().copied().skip(1) {
            let side = self.resolve_piece_edge_ref(edge_ref);
            segs.extend(side.path.segs.iter().cloned());
        }

        PathMm::new(first.path.start, segs.into_boxed_slice(), true)
    }

    /// Validates atlas invariants against topology and ring continuity rules.
    pub fn validate<T: PuzzleTopology>(&self, topology: &T) -> Result<(), GeometryInvariantError> {
        if self.pieces.len() != topology.piece_count() as usize {
            return Err(GeometryInvariantError::PieceCountMismatch);
        }
        if self.interior_edges.len() != topology.edge_count() as usize {
            return Err(GeometryInvariantError::InteriorEdgeCountMismatch);
        }

        let mut interior_ref_count = vec![0u32; self.interior_edges.len()];
        let mut frame_usage = vec![0u32; self.frame.edges.len()];

        for (edge_idx, edge_geom) in self.interior_edges.iter().enumerate() {
            let edge_id = EdgeId(edge_idx as u32);
            if edge_geom.endpoints != topology.edge_endpoints(edge_id) {
                return Err(GeometryInvariantError::InteriorEdgeEndpointsMismatch);
            }
        }

        for (piece_idx, piece_geom) in self.pieces.iter().enumerate() {
            if piece_geom.edges.is_empty() {
                return Err(GeometryInvariantError::PieceRingEmpty);
            }

            let mut prev_end = None;
            for (edge_index, edge_ref) in piece_geom.edges.iter().copied().enumerate() {
                let side_geom = match edge_ref {
                    PieceEdgeRef::Interior { edge, .. } => {
                        let idx = edge.as_usize();
                        if idx >= self.interior_edges.len() {
                            return Err(GeometryInvariantError::InteriorEdgeReferenceCountMismatch);
                        }
                        interior_ref_count[idx] = interior_ref_count[idx].saturating_add(1);
                        self.resolve_piece_edge_ref(edge_ref)
                    }
                    PieceEdgeRef::Border { edge } => {
                        let idx = edge.as_usize();
                        let border = self
                            .border_edges
                            .get(idx)
                            .ok_or(GeometryInvariantError::BorderEdgeRefOutOfRange)?;
                        let frame_idx = border.frame_edge.as_usize();
                        if frame_idx >= self.frame.edges.len() {
                            return Err(GeometryInvariantError::FrameEdgeRefOutOfRange);
                        }
                        frame_usage[frame_idx] = frame_usage[frame_idx].saturating_add(1);
                        self.resolve_piece_edge_ref(edge_ref)
                    }
                };

                if let Some(prev_end_point) = prev_end {
                    if prev_end_point != side_geom.path.start {
                        let _ = (piece_idx, edge_index);
                        return Err(GeometryInvariantError::PieceRingNotContiguous);
                    }
                }
                prev_end = Some(path_end_point(&side_geom.path));
            }

            let first_start = self.resolve_piece_edge_ref(piece_geom.edges[0]).path.start;
            if prev_end != Some(first_start) {
                return Err(GeometryInvariantError::PieceRingNotClosed);
            }
        }

        if interior_ref_count.iter().any(|count| *count != 2) {
            return Err(GeometryInvariantError::InteriorEdgeReferenceCountMismatch);
        }
        if frame_usage.iter().any(|count| *count != 1) {
            return Err(GeometryInvariantError::FrameEdgeUsageMismatch);
        }

        Ok(())
    }

    pub fn resolve_piece_edge_ref(&self, edge_ref: PieceEdgeRef) -> &EdgeSideGeometryMm {
        match edge_ref {
            PieceEdgeRef::Interior { edge, side } => {
                let geom = self
                    .interior_edges
                    .get(edge.as_usize())
                    .expect("interior edge id should be valid for atlas");
                match side {
                    EdgeSide::A => &geom.side_a,
                    EdgeSide::B => &geom.side_b,
                }
            }
            PieceEdgeRef::Border { edge } => {
                &self
                    .border_edges
                    .get(edge.as_usize())
                    .expect("border edge id should be valid for atlas")
                    .side
            }
        }
    }
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
