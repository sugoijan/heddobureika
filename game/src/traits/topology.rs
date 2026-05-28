//! Topology behavior contract.

use std::any::Any;

use crate::ids::{EdgeId, PieceId};
use crate::playable::Pose2;
use crate::render_geometry::PuzzleRenderGeometry;
use crate::rotation_step::{next_step_canonical, StepDirection, SymmetryStrength};
use crate::topology::{RelativePose, TopologySpec};
use crate::units::{AngleDeg, LengthMm};

/// Inclusive piece-center frame bounds in canonical pose units.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FrameBounds {
    pub min_x: f32,
    pub min_y: f32,
    pub max_x: f32,
    pub max_y: f32,
}

/// How a topology's pose-unit lattice is laid out in image pixels — the single
/// source of truth shared by the client renderer and the server (worker) so
/// both agree on where pieces sit. A pose-mm point `p` maps to the pixel
/// `origin_px + p * pose_unit_px`, and back via `(pixel - origin_px) /
/// pose_unit_px`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ImagePlacement {
    /// Pose-unit → pixel scale per axis.
    pub pose_unit_px: [f32; 2],
    /// Top-left pixel of the puzzle frame within the image.
    pub origin_px: [f32; 2],
    /// Puzzle frame size in pixels (`pose_extent * pose_unit_px`). For
    /// stretch-to-fill topologies this equals the image; for cropping ones
    /// (triangular) it is a centred, letterboxed sub-rect.
    pub frame_px: [f32; 2],
}

/// An outer feature of a piece, used by the universal frame-snap
/// solver. All coordinates are piece-local pose units (relative to the
/// piece anchor, BEFORE rotation), and the solver lifts them to world
/// coords via the piece's current pose with aspect-aware rotation.
///
/// The solver evaluates each feature independently against the puzzle
/// frame: rotation correction + perpendicular distance for
/// `BorderEdge`, point distance for `CornerAttachment`. Surviving
/// constraints are aggregated into a single group-pose correction, so a
/// piece with two `BorderEdge`s on perpendicular frame sides snaps both
/// axes the same way a 2-piece group with one such edge each would.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PieceOuterFeature {
    /// A straight outer segment of the piece. Matched to whichever of
    /// the four frame sides (top/right/bottom/left) it is currently
    /// most parallel to and closest to. Constrains the perpendicular
    /// axis only (1D position constraint + rotation constraint).
    BorderEdge { p1: (f32, f32), p2: (f32, f32) },
    /// A piece-local point that attaches to one of the four frame
    /// corners. Constrains both axes (2D position constraint). Topology
    /// declares these explicitly — they are NOT inferred from geometry
    /// because the puzzle frame's rounded corners clip true geometric
    /// vertices that would otherwise meet the corner.
    CornerAttachment { point: (f32, f32) },
}

/// Describes puzzle piece/edge relationships without requiring a specific
/// geometry implementation inside core state containers.
pub trait PuzzleTopology {
    /// Transport-friendly topology identity for snapshots. Each concrete
    /// topology implementation is expected to override this with its own
    /// `SerializableTopology::to_spec`. The default returns the placeholder
    /// `unknown` spec, which cannot be round-tripped back to a topology.
    fn to_spec(&self) -> TopologySpec {
        TopologySpec::unknown()
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

    /// Whether this piece's `symmetry_angles` represent strict geometric
    /// symmetries (`Strong`) or heuristic per-piece rotation hints
    /// (`Weak`). Used by group-level rotation aggregation to avoid
    /// collapsing the rotation set when a heuristic piece joins a
    /// constrained one. See [`SymmetryStrength`] for the semantics.
    ///
    /// Default: `Strong`. Topologies with irregular per-piece geometry
    /// (e.g. Voronoi interior cells) should override to return `Weak` for
    /// those pieces.
    fn symmetry_strength(&self, _piece: PieceId) -> SymmetryStrength {
        SymmetryStrength::Strong
    }

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

    /// Optional canonical frame bounds for topology-neutral frame snap.
    fn frame_bounds(&self) -> Option<FrameBounds> {
        None
    }

    /// Whether this piece belongs to the puzzle's outer frame.
    /// Optional optimisation hint — the universal snap solver doesn't
    /// require this to be accurate (a piece with no outer features is
    /// equivalent to one for which `is_frame_border_piece` returns
    /// false).
    fn is_frame_border_piece(&self, _piece: PieceId) -> bool {
        false
    }

    /// Visits the outer features of this piece in piece-local pose
    /// units. The default emits nothing — topologies whose pieces touch
    /// the frame override this. See `PieceOuterFeature` for semantics.
    fn visit_outer_features(&self, _piece: PieceId, _visitor: &mut dyn FnMut(PieceOuterFeature)) {}

    /// Anchor used for complete-group identity snap.
    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        None
    }

    /// For topologies parameterised by `(cols, rows)` — the grid and the
    /// triangular tessellation today, possibly more in future — returns
    /// the underlying dimensions for display purposes (catalog picker
    /// labels, debug HUD). Topologies without a `cols × rows`
    /// parameterisation should return `None`; non-display callers should
    /// not read this.
    fn dims_hint(&self) -> Option<(u32, u32)> {
        None
    }

    /// The canonical piece-center position in pose-mm units. Multiplying
    /// the returned coordinates by `pose_unit_px` (= `image_dim /
    /// image_extent_in_pose_units`) gives the piece's anchor in pixels.
    /// Every concrete topology should override this — the default `None`
    /// is only a no-op fallback so render-geometry helpers can ask
    /// generically.
    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        let _ = piece;
        None
    }

    /// Frame extent used by the universal frame-snap solver: the
    /// outer rectangle that piece `BorderEdge` midpoints and
    /// `CornerAttachment` points are matched against. For grid and
    /// Voronoi this equals `image_extent_in_pose_units()`. For the
    /// triangular tessellation it's `(cols, 2*rows)` — the bottom
    /// half-row's anchors sit at `y = 2*rows`, NOT at
    /// `image_extent_in_pose_units().1 = 2*rows + 1`.
    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.image_extent_in_pose_units()
    }

    /// How many pose-mm units the topology spans on each axis when laid
    /// out across the puzzle image. Consumers use this to derive
    /// `pose_unit_px = image_dim / extent` for any topology without
    /// hard-coding grid math (`image_w / cols`).
    ///
    /// Default: derived from `frame_bounds() + typical_piece_extent_mm`
    /// (the bounds give the range of canonical piece positions; adding a
    /// half-extent on each end accounts for the piece extending past its
    /// own canonical position). Topologies with a tighter native concept
    /// of "span" should override.
    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        let bounds = self.frame_bounds().unwrap_or(FrameBounds {
            min_x: 0.0,
            min_y: 0.0,
            max_x: 0.0,
            max_y: 0.0,
        });
        let (typical_x, typical_y) = self.typical_piece_extent_mm();
        let extent_x = (bounds.max_x - bounds.min_x).max(0.0) + typical_x.as_mm_f32();
        let extent_y = (bounds.max_y - bounds.min_y).max(0.0) + typical_y.as_mm_f32();
        let extent_x = if extent_x > 0.0 { extent_x } else { 1.0 };
        let extent_y = if extent_y > 0.0 { extent_y } else { 1.0 };
        (extent_x, extent_y)
    }

    /// Pixel placement of the pose-unit lattice within the image (see
    /// [`ImagePlacement`]). The default STRETCHES the pose extent to fill the
    /// whole image (origin `[0, 0]`, frame = image), matching grid/hex/voronoi.
    /// Topologies that keep pieces undistorted by cropping/letterboxing
    /// (triangular, which uses a uniform scale and a centred frame) override
    /// this. Both the renderer and the worker call it, so they never disagree.
    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        let (ex, ey) = self.image_extent_in_pose_units();
        let w = image_width as f32;
        let h = image_height as f32;
        ImagePlacement {
            pose_unit_px: [
                if ex > 0.0 { w / ex } else { 1.0 },
                if ey > 0.0 { h / ey } else { 1.0 },
            ],
            origin_px: [0.0, 0.0],
            frame_px: [w, h],
        }
    }

    /// Approximate piece extent in pose-mm units, used by consumers that
    /// need a generic "how big is this piece" value (UX tolerances,
    /// safety-bound insets, etc.).
    ///
    /// The default walks edges incident on the piece and uses the largest
    /// per-axis component of `expected_relative_pose`. Topologies whose
    /// pieces have a more well-defined extent (e.g. grid cells span one
    /// pose unit per axis) may override this with a closed-form answer.
    fn piece_extent_mm(&self, piece: PieceId) -> (LengthMm, LengthMm) {
        let edge_count = self.edge_count();
        let mut dx: f32 = 0.0;
        let mut dy: f32 = 0.0;
        for idx in 0..edge_count {
            let edge = EdgeId(idx);
            let (a, b) = self.edge_endpoints(edge);
            let (other, neighbor_of) = if a == piece {
                (b, true)
            } else if b == piece {
                (a, true)
            } else {
                (PieceId(0), false)
            };
            if !neighbor_of {
                continue;
            }
            let rel = self.expected_relative_pose(piece, other);
            dx = dx.max(rel.dx.as_mm_f32().abs());
            dy = dy.max(rel.dy.as_mm_f32().abs());
        }
        if dx <= 0.0 {
            dx = 1.0;
        }
        if dy <= 0.0 {
            dy = 1.0;
        }
        (
            LengthMm::try_new(dx).unwrap_or(LengthMm::zero()),
            LengthMm::try_new(dy).unwrap_or(LengthMm::zero()),
        )
    }

    /// Typical piece extent in pose-mm units. Used by UX heuristics that
    /// want a single per-puzzle "piece size" estimate (click slop,
    /// rubber-band radius, scramble margins). The default takes the median
    /// per-axis extent across all pieces; topologies with uniform pieces
    /// should override to return that uniform extent directly.
    fn typical_piece_extent_mm(&self) -> (LengthMm, LengthMm) {
        let total = self.piece_count();
        if total == 0 {
            return (LengthMm::zero(), LengthMm::zero());
        }
        let mut xs: Vec<f32> = Vec::with_capacity(total as usize);
        let mut ys: Vec<f32> = Vec::with_capacity(total as usize);
        for idx in 0..total {
            let (px, py) = self.piece_extent_mm(PieceId(idx));
            xs.push(px.as_mm_f32());
            ys.push(py.as_mm_f32());
        }
        xs.sort_by(|a, b| a.total_cmp(b));
        ys.sort_by(|a, b| a.total_cmp(b));
        let mid = xs.len() / 2;
        let median_x = xs[mid];
        let median_y = ys[mid];
        (
            LengthMm::try_new(median_x).unwrap_or(LengthMm::zero()),
            LengthMm::try_new(median_y).unwrap_or(LengthMm::zero()),
        )
    }

    /// Builds the topology-agnostic render geometry (per-piece outlines,
    /// edge SVGs, bounds, pose anchors) used by the renderers. The default
    /// implementation returns `None`; concrete topologies override this
    /// with their shaper-driven implementation.
    ///
    /// `settings` is a topology-owned, type-erased shape settings blob.
    /// Each topology's implementation downcasts it to its expected concrete
    /// type and returns `None` if the type does not match.
    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn Any,
    ) -> Option<PuzzleRenderGeometry> {
        let _ = (image_width, image_height, shape_seed, settings);
        None
    }
}
