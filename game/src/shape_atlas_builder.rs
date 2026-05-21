//! Shared helper for assembling a `ShapeAtlasMm` out of per-piece edge
//! specifications.
//!
//! Both `grid_shape::build_shape_atlas` and `voronoi_topology::build_voronoi_atlas`
//! used to do the same five-step dance:
//!
//! 1. Allocate one slot per interior topology edge with `side_a`/`side_b` pockets.
//! 2. Walk each piece's edge ring; for each edge, either fill the appropriate
//!    interior slot (matching the piece against the topology's stored
//!    endpoints to pick `EdgeSide::A` vs `EdgeSide::B`) or queue it as a
//!    border edge.
//! 3. Sort all queued border edges by a topology-supplied `frame_sort_key`
//!    so the resulting `FrameEdgeId` / `BorderEdgeId` assignment is
//!    deterministic and produces a stable rendering order.
//! 4. Re-walk each piece in input order and build the canonical edge ring
//!    (`PieceGeometryMm.edges`), looking up each border edge's
//!    `BorderEdgeId` from the post-sort assignment.
//! 5. Validate the resulting atlas against the topology.
//!
//! `ShapeAtlasBuilder` codifies that pipeline. Topologies hand the builder
//! per-piece `PieceEdgeBuilderSpec` rings; the builder owns the slot
//! filling and sorting.

use std::collections::HashMap;

use crate::ids::{BorderEdgeId, EdgeId, FrameEdgeId, PieceId};
use crate::shape::{
    BorderEdgeGeometryMm, EdgeSide, EdgeSideGeometryMm, FrameGeometryMm, GeometryInvariantError,
    InteriorEdgeGeometryMm, PieceEdgeRef, PieceGeometryMm, ShapeAtlasMm,
};
use crate::traits::topology::PuzzleTopology;

/// One edge of a piece's canonical edge ring, as supplied by a topology.
#[derive(Clone, Debug, PartialEq)]
pub enum PieceEdgeBuilderSpec {
    /// Edge that joins two pieces. `edge` identifies the topology edge;
    /// the builder figures out whether this piece is endpoint `A` or `B`
    /// from the topology's `edge_endpoints`.
    Interior {
        edge: EdgeId,
        side_geometry: EdgeSideGeometryMm,
    },
    /// Edge that lies on the puzzle's outer frame. `frame_sort_key`
    /// determines the order in which border edges are assigned
    /// `FrameEdgeId` / `BorderEdgeId` — pick a deterministic value
    /// derived from geometry (e.g. position along the frame).
    Border {
        side_geometry: EdgeSideGeometryMm,
        frame_sort_key: f32,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeAtlasBuildError {
    /// `push_piece` was called with a piece id that does not match the
    /// next-expected piece (pieces must be pushed in id order, exactly
    /// once each).
    PieceOutOfOrder { expected: u32, got: u32 },
    /// Fewer pieces were pushed than the topology reports.
    MissingPieces { expected: u32, got: u32 },
    /// More pieces were pushed than the topology reports.
    TooManyPieces { expected: u32 },
    /// An interior-edge spec referred to a topology edge whose endpoints
    /// do not include the pushing piece, or the same side of an interior
    /// edge was filled twice.
    InteriorEdgeMismatch { edge: EdgeId },
    /// An interior topology edge was not referenced by both of its
    /// endpoint pieces (or by neither).
    InteriorEdgeIncomplete { edge: EdgeId },
    /// Topology-level invariant violation surfaced by `ShapeAtlasMm::validate`.
    Invariant(GeometryInvariantError),
}

struct InteriorSlot {
    endpoints: (PieceId, PieceId),
    side_a: Option<EdgeSideGeometryMm>,
    side_b: Option<EdgeSideGeometryMm>,
}

struct BorderEntry {
    piece: PieceId,
    piece_edge_index: usize,
    side_geometry: EdgeSideGeometryMm,
    sort_key: f32,
}

/// Per-piece edge marker. We can't emit the final `PieceEdgeRef::Border`
/// until border ids have been assigned (post-sort), so we record the
/// piece-edge slot now and resolve in `build()`.
#[derive(Clone, Copy)]
enum EdgeMarker {
    Interior { edge: EdgeId, side: EdgeSide },
    Border,
}

pub struct ShapeAtlasBuilder<'a, T: PuzzleTopology + ?Sized> {
    topology: &'a T,
    interior_slots: Vec<InteriorSlot>,
    border_entries: Vec<BorderEntry>,
    piece_markers: Vec<Vec<EdgeMarker>>,
}

impl<'a, T: PuzzleTopology + ?Sized> ShapeAtlasBuilder<'a, T> {
    pub fn new(topology: &'a T) -> Self {
        let interior_slots = (0..topology.edge_count())
            .map(|edge_idx| InteriorSlot {
                endpoints: topology.edge_endpoints(EdgeId(edge_idx)),
                side_a: None,
                side_b: None,
            })
            .collect();
        let piece_count = topology.piece_count() as usize;
        Self {
            topology,
            interior_slots,
            border_entries: Vec::new(),
            piece_markers: Vec::with_capacity(piece_count),
        }
    }

    /// Push the next piece's canonical edge ring. Pieces must be pushed
    /// in `PieceId` order, exactly once each.
    pub fn push_piece(
        &mut self,
        piece: PieceId,
        edges: Vec<PieceEdgeBuilderSpec>,
    ) -> Result<(), ShapeAtlasBuildError> {
        let expected = self.piece_markers.len() as u32;
        if expected >= self.topology.piece_count() {
            return Err(ShapeAtlasBuildError::TooManyPieces {
                expected: self.topology.piece_count(),
            });
        }
        if piece.as_u32() != expected {
            return Err(ShapeAtlasBuildError::PieceOutOfOrder {
                expected,
                got: piece.as_u32(),
            });
        }
        let mut markers = Vec::with_capacity(edges.len());
        for (edge_idx, spec) in edges.into_iter().enumerate() {
            match spec {
                PieceEdgeBuilderSpec::Interior {
                    edge,
                    side_geometry,
                } => {
                    let slot = self
                        .interior_slots
                        .get_mut(edge.as_usize())
                        .ok_or(ShapeAtlasBuildError::InteriorEdgeMismatch { edge })?;
                    let side = if slot.endpoints.0 == piece {
                        EdgeSide::A
                    } else if slot.endpoints.1 == piece {
                        EdgeSide::B
                    } else {
                        return Err(ShapeAtlasBuildError::InteriorEdgeMismatch { edge });
                    };
                    let target = match side {
                        EdgeSide::A => &mut slot.side_a,
                        EdgeSide::B => &mut slot.side_b,
                    };
                    if target.replace(side_geometry).is_some() {
                        return Err(ShapeAtlasBuildError::InteriorEdgeMismatch { edge });
                    }
                    markers.push(EdgeMarker::Interior { edge, side });
                }
                PieceEdgeBuilderSpec::Border {
                    side_geometry,
                    frame_sort_key,
                } => {
                    self.border_entries.push(BorderEntry {
                        piece,
                        piece_edge_index: edge_idx,
                        side_geometry,
                        sort_key: frame_sort_key,
                    });
                    markers.push(EdgeMarker::Border);
                }
            }
        }
        self.piece_markers.push(markers);
        Ok(())
    }

    /// Finalise the atlas: sort border edges, assign frame/border ids,
    /// resolve piece-edge rings, fill interior edges, validate.
    pub fn build(self) -> Result<ShapeAtlasMm, ShapeAtlasBuildError> {
        let expected = self.topology.piece_count();
        if self.piece_markers.len() as u32 != expected {
            return Err(ShapeAtlasBuildError::MissingPieces {
                expected,
                got: self.piece_markers.len() as u32,
            });
        }

        // 1) Stable sort of border entries gives a deterministic
        //    FrameEdgeId / BorderEdgeId assignment.
        let mut border_entries = self.border_entries;
        border_entries.sort_by(|a, b| {
            a.sort_key
                .total_cmp(&b.sort_key)
                .then_with(|| a.piece.cmp(&b.piece))
                .then_with(|| a.piece_edge_index.cmp(&b.piece_edge_index))
        });

        // 2) Walk sorted entries; emit border edges + lookup table from
        //    (piece, piece_edge_index) → BorderEdgeId.
        let mut border_lookup: HashMap<(PieceId, usize), BorderEdgeId> =
            HashMap::with_capacity(border_entries.len());
        let mut frame_edges = Vec::with_capacity(border_entries.len());
        let mut border_edges = Vec::with_capacity(border_entries.len());
        for (idx, entry) in border_entries.into_iter().enumerate() {
            let frame_edge = FrameEdgeId(idx as u32);
            let border_edge = BorderEdgeId(idx as u32);
            border_lookup.insert((entry.piece, entry.piece_edge_index), border_edge);
            frame_edges.push(entry.side_geometry.path.clone());
            border_edges.push(BorderEdgeGeometryMm {
                piece: entry.piece,
                side: entry.side_geometry,
                frame_edge,
            });
        }

        // 3) Resolve each piece's canonical edge ring.
        let mut pieces = Vec::with_capacity(self.piece_markers.len());
        for (piece_idx, markers) in self.piece_markers.into_iter().enumerate() {
            let piece = PieceId(piece_idx as u32);
            let mut refs = Vec::with_capacity(markers.len());
            for (edge_idx, marker) in markers.into_iter().enumerate() {
                match marker {
                    EdgeMarker::Interior { edge, side } => {
                        refs.push(PieceEdgeRef::Interior { edge, side });
                    }
                    EdgeMarker::Border => {
                        let border = *border_lookup.get(&(piece, edge_idx)).expect(
                            "border lookup must contain every marked border edge by construction",
                        );
                        refs.push(PieceEdgeRef::Border { edge: border });
                    }
                }
            }
            pieces.push(PieceGeometryMm {
                edges: refs.into_boxed_slice(),
            });
        }

        // 4) Interior edges: every slot must have both sides filled.
        let mut interior_edges = Vec::with_capacity(self.interior_slots.len());
        for (idx, slot) in self.interior_slots.into_iter().enumerate() {
            let edge = EdgeId(idx as u32);
            let side_a = slot
                .side_a
                .ok_or(ShapeAtlasBuildError::InteriorEdgeIncomplete { edge })?;
            let side_b = slot
                .side_b
                .ok_or(ShapeAtlasBuildError::InteriorEdgeIncomplete { edge })?;
            interior_edges.push(InteriorEdgeGeometryMm {
                endpoints: slot.endpoints,
                side_a,
                side_b,
            });
        }

        let atlas = ShapeAtlasMm {
            pieces: pieces.into_boxed_slice(),
            interior_edges: interior_edges.into_boxed_slice(),
            border_edges: border_edges.into_boxed_slice(),
            frame: FrameGeometryMm {
                edges: frame_edges.into_boxed_slice(),
            },
        };
        atlas
            .validate(self.topology)
            .map_err(ShapeAtlasBuildError::Invariant)?;
        Ok(atlas)
    }
}
