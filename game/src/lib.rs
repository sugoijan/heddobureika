//! Experimental bottom-up game model crate.
//!
//! This crate intentionally exposes a skeletal API for iterative design:
//! - `LogicalState`: topology/connectivity progress model.
//! - `PlayableState`: authoritative gameplay model and action application.
//! - `InteractableState`: projection/cache for rendering and interaction.
//!
//! Behavior is intentionally minimal in this first pass; the goal is to lock
//! boundaries and contracts before implementing full mechanics.

pub mod delta;
pub mod edge_compose;
pub mod edge_host;
pub mod edge_profile;
pub mod grid_puzzle;
pub mod grid_shape;
pub mod hexagonal_shape;
pub mod hexagonal_topology;
pub mod ids;
pub mod logical;
pub mod playable;
pub mod projection;
pub mod render_geometry;
pub mod rotation_step;
pub mod rules;
pub mod shape;
pub mod shape_atlas_builder;
pub mod shape_profiles;
pub mod shape_svg;
pub mod snap;
pub mod snapshot;
pub mod topology;
pub mod traits;
pub mod triangular_lattice;
pub mod triangular_shape;
pub mod units;
pub mod update;
pub mod voronoi_topology;

pub use delta::{IdList, PlayableDelta};
pub use edge_compose::ConnectorSeg;
pub use edge_host::{BorderFrameShape, EdgeOrientation, HostEdgeFrame, LineWave, WarpField};
pub use edge_profile::{
    ConnectorShape, EdgeProfileInput, EdgeShapeStyle, OffsetCircleEdgeProfile, TabBlankEdgeProfile,
    TrapezoidEdgeProfile,
};
pub use grid_puzzle::GridPuzzleDefinition;
pub use grid_shape::{
    GridEdgeProfilePreview, GridEdgeProfileSample, GridHostShapePreview, GridJigsawShaper,
    GridShapeBuildError, GridShapeCache, GridShapeSettings,
};
pub use hexagonal_shape::{
    HexagonalShapeBuildError, HexagonalShapeCache, HexagonalShapeSettings, HexagonalShaper,
};
pub use hexagonal_topology::{HexPieceKind, HexagonalTopology};
pub use ids::{BorderEdgeId, EdgeId, FrameEdgeId, GroupId, PieceId};
pub use logical::{
    GroupSlot, LogicalInvariantError, LogicalMerge, LogicalState, LogicalStateSummary, PieceSlot,
};
pub use playable::{
    FlipState, PlayableAction, PlayableInvariantError, PlayableState, PlayableStateSummary, Pose2,
    Position2, RestrictedPlayableAction, SolveStage,
};
pub use projection::{
    IdentityPhysicalState, InteractableState, PhysicalProjection, ProjectionScratch, VisualState,
};
pub use render_geometry::{
    PiecePaths, PieceRenderGeometry, PuzzleFrameShape, PuzzleRenderGeometry, RectPx,
    PUZZLE_FRAME_CORNER_RADIUS_RATIO,
};
pub use rotation_step::{
    canonicalize_symmetry_angles, group_symmetry_angles, intersect_symmetry_angles,
    next_step_canonical, next_step_target, StepDirection, StepRotationTarget, SymmetryStrength,
};
pub use rules::PlayRules;
pub use shape::{
    BorderEdgeGeometryMm, EdgeSide, EdgeSideGeometryMm, FrameGeometryMm, GeometryInvariantError,
    InteriorEdgeGeometryMm, PathMm, PathSegMm, PieceEdgeRef, PieceGeometryMm, PointMm,
    ShapeAtlasMm,
};
pub use shape_atlas_builder::{PieceEdgeBuilderSpec, ShapeAtlasBuildError, ShapeAtlasBuilder};
pub use shape_svg::{
    cache_to_svg_paths, frame_to_svg_paths, path_to_svg_d, piece_to_svg_paths, SvgPiecePaths,
};
pub use snap::{ActionId, JoinSite, MergePolicy, SnapCandidate, SnapProposal, SnapRejectionReason};
pub use snapshot::{
    PlayableSnapshot, PlayableSnapshotError, RestoredPlayableState, SnapshotEnvelope,
    PLAYABLE_SNAPSHOT_VERSION,
};
pub use topology::{
    build_topology_from_spec, GenericPlayableState, GenericTopology, GridTopology, RelativePose,
    SerializableTopology, TopologySpec, TrianglePieceKind, TriangularTessellationTopology,
};
pub use traits::edge_profile::EdgeProfileStrategy;
pub use traits::shaping::{PieceGeometryProvider, TopologyShaper};
pub use traits::topology::{FrameBounds, PieceOuterFeature, PuzzleTopology};
pub use triangular_lattice::{TriDirection, TriLattice};
pub use triangular_shape::{
    TriangularTessellationShapeBuildError, TriangularTessellationShapeCache,
    TriangularTessellationShapeSettings, TriangularTessellationShaper,
};
pub use units::{AngleDeg, Dpi, LengthMm};
pub use update::{
    AppliedProposal, GroupMergeUpdate, GroupPoseUpdate, PlayableUpdateBatch,
    ProposalApplyRejection, ProposalApplyStatus,
};
pub use voronoi_topology::VoronoiTopology;

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug)]
    struct DummyTopology {
        pieces: u32,
        edges: u32,
    }

    impl PuzzleTopology for DummyTopology {
        fn piece_count(&self) -> u32 {
            self.pieces
        }

        fn edge_count(&self) -> u32 {
            self.edges
        }

        fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
            let idx = edge.as_u32() % self.pieces.max(1);
            (PieceId(idx), PieceId(idx))
        }

        fn expected_relative_pose(&self, _a: PieceId, _b: PieceId) -> RelativePose {
            RelativePose::default()
        }

        fn symmetry_angles(&self, _piece: PieceId) -> &[AngleDeg] {
            static ANGLES: std::sync::OnceLock<Box<[AngleDeg]>> = std::sync::OnceLock::new();
            ANGLES
                .get_or_init(|| {
                    vec![
                        AngleDeg::try_new(90.0).expect("finite"),
                        AngleDeg::try_new(180.0).expect("finite"),
                        AngleDeg::try_new(270.0).expect("finite"),
                    ]
                    .into_boxed_slice()
                })
                .as_ref()
        }
    }

    #[test]
    fn skeleton_construction_and_projection_compile() {
        let topo = DummyTopology {
            pieces: 4,
            edges: 3,
        };
        let logical = LogicalState::new(topo);
        let mut playable = PlayableState::new(logical, PlayRules::default());

        let delta = playable.apply_action(PlayableAction::TranslateGroup {
            group: GroupId(0),
            drop_pos: Position2::try_from_mm(12.0, 4.0).expect("finite"),
        });
        assert_eq!(delta.revision, 1);
        assert!(delta.dirty_groups.contains(&GroupId(0)));

        let mut interactable = InteractableState::rebuild_from(&playable);
        let mut scratch = ProjectionScratch::with_capacity(playable.piece_count());
        interactable.apply_delta(&playable, &delta, &mut scratch);
        assert_eq!(interactable.piece_world_pose().len(), 4);
    }

    #[test]
    fn delta_lists_can_be_reused() {
        let mut delta = PlayableDelta::for_revision(7);
        delta.dirty_groups.push(GroupId(2));
        delta.dirty_pieces.push(PieceId(5));
        delta.clear_keep_revision(8);
        assert_eq!(delta.revision, 8);
        assert!(delta.dirty_groups.is_empty());
        assert!(delta.dirty_pieces.is_empty());
    }

    #[test]
    fn z_order_iterators_exist() {
        let topo = DummyTopology {
            pieces: 3,
            edges: 0,
        };
        let logical = LogicalState::new(topo);
        let playable = PlayableState::new(logical, PlayRules::default());

        let asc: Vec<_> = playable.iter_z_asc().collect();
        let desc: Vec<_> = playable.iter_z_desc().collect();
        assert_eq!(asc, vec![GroupId(0), GroupId(1), GroupId(2)]);
        assert_eq!(desc, vec![GroupId(2), GroupId(1), GroupId(0)]);
    }

    #[test]
    fn restricted_detach_action_compiles_and_marks_membership_change() {
        let topo = DummyTopology {
            pieces: 4,
            edges: 0,
        };
        let logical = LogicalState::new(topo);
        let mut playable = PlayableState::new(logical, PlayRules::default());

        // Simulate piece 1 belonging to group 0 before detach.
        playable.logical.pieces[1].group = GroupId(0);
        playable.logical.groups[0].size = 2;
        playable.logical.groups[1].size = 0;
        playable.logical.groups[1].alive = false;

        let delta =
            playable.apply_restricted_action(RestrictedPlayableAction::DetachPieceAsGroup {
                piece: PieceId(1),
                target_pose: Pose2::try_from_mm_degrees(20.0, 30.0, 45.0).expect("finite"),
                target_flip: FlipState::Flipped,
            });

        assert!(delta.membership_changed);
        assert_eq!(playable.logical.group_of(PieceId(1)), Some(GroupId(1)));
        assert_eq!(playable.flip_of(GroupId(1)), Some(FlipState::Flipped));
    }
}
