//! Topology behavior contract.

use crate::ids::{EdgeId, PieceId};
use crate::rotation_step::{next_step_canonical, StepDirection};
use crate::topology::{RelativePose, TopologySpec};
use crate::units::AngleDeg;

/// Kinds of frame-snap policy supported by a topology.
///
/// Frame snap is the feature that pulls a group toward the puzzle's outer
/// frame (corners/edges) when it lands close enough. Each topology decides
/// whether the concept applies and which corners/edges count.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrameSnapKind {
    /// This topology does not participate in frame snap.
    None,
    /// Rectangular grid frame snap (corners + edges + complete-group identity).
    Grid,
}

/// Describes puzzle piece/edge relationships without requiring a specific
/// geometry implementation inside core state containers.
pub trait PuzzleTopology {
    /// Transport-friendly topology identity for snapshots.
    fn topology_spec(&self) -> TopologySpec {
        TopologySpec::Unknown {
            piece_count: self.piece_count(),
            edge_count: self.edge_count(),
        }
    }

    /// Total number of pieces in this puzzle.
    fn piece_count(&self) -> u32;

    /// Total number of topology edges that can be activated as joins.
    fn edge_count(&self) -> u32;

    /// Endpoints for a topology edge.
    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId);

    /// Expected relative placement between two pieces when joined.
    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose;

    /// Piece-local rotational symmetry angles in degrees, excluding `0`.
    ///
    /// Contract:
    /// - values are interpreted modulo 360;
    /// - `0` is implicit and always considered valid by the shared solver;
    /// - slices may be non-canonical (unsorted, duplicates, out-of-range).
    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg];

    /// Topology-defined clockwise discrete step rotation.
    ///
    /// Default behavior uses `symmetry_angles(piece)` and shared rotation-step
    /// solver logic.
    fn step_rotation_cw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        next_step_canonical(
            self.symmetry_angles(piece),
            current,
            rotation_snap_tolerance,
            StepDirection::Cw,
        )
    }

    /// Topology-defined counter-clockwise discrete step rotation.
    ///
    /// Default behavior uses `symmetry_angles(piece)` and shared rotation-step
    /// solver logic.
    fn step_rotation_ccw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        next_step_canonical(
            self.symmetry_angles(piece),
            current,
            rotation_snap_tolerance,
            StepDirection::Ccw,
        )
    }

    /// Identifies the kind of frame-snap policy this topology participates
    /// in. Defaults to `None`; topologies that want frame-snap behavior
    /// (currently only `GridTopology`) override.
    ///
    /// `PlayableState::apply_topology_frame_snap` reads this to dispatch to
    /// the right snap-target computation.
    fn frame_snap_kind(&self) -> FrameSnapKind {
        FrameSnapKind::None
    }
}
