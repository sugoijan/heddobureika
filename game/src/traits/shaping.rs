//! Shape provider and topology-shaper behavior contracts.

use crate::ids::{BorderEdgeId, EdgeId, PieceId};
use crate::shape::{
    BorderEdgeGeometryMm, EdgeSideGeometryMm, FrameGeometryMm, InteriorEdgeGeometryMm,
    PieceGeometryMm,
};
use crate::traits::topology::PuzzleTopology;
use crate::units::LengthMm;

/// Immutable piece-geometry provider.
pub trait PieceGeometryProvider {
    fn piece_count(&self) -> u32;
    fn piece_geometry(&self, piece: PieceId) -> &PieceGeometryMm;
    fn interior_edge_geometry(&self, edge: EdgeId) -> &InteriorEdgeGeometryMm;
    fn border_edge_geometry(&self, edge: BorderEdgeId) -> &BorderEdgeGeometryMm;
    fn frame_geometry(&self) -> &FrameGeometryMm;
    fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm;
}

/// Trait for topology-specific shapers.
pub trait TopologyShaper<T: PuzzleTopology> {
    type Settings;
    type Cache: PieceGeometryProvider;
    type Error;

    fn build_cache(
        &self,
        topology: &T,
        piece_width: LengthMm,
        piece_height: LengthMm,
        seed: u32,
        settings: &Self::Settings,
    ) -> Result<Self::Cache, Self::Error>;
}
