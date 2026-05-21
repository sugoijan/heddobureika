use std::sync::Arc;

use heddobureika_game::{
    build_topology_from_spec, GenericTopology, GridTopology, LogicalState, PlayRules,
    PlayableState, PuzzleTopology, TopologySpec, TriangularTessellationTopology,
};

#[test]
fn arc_dyn_topology_works_with_playable_state() {
    let topology: GenericTopology = Arc::new(GridTopology::try_new(2, 1).expect("valid grid"));
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    assert_eq!(playable.piece_count(), 2);
    assert_eq!(playable.logical.edge_count(), 1);
}

#[test]
fn descriptors_build_supported_topologies() {
    let grid =
        build_topology_from_spec(&TopologySpec::grid(3, 2)).expect("grid descriptor should build");
    assert_eq!(grid.piece_count(), 6);
    assert_eq!(grid.edge_count(), 7);

    let triangular = build_topology_from_spec(&TopologySpec::triangular_tessellation(3, 2))
        .expect("triangular descriptor should build");
    assert_eq!(triangular.piece_count(), 15);
    assert_eq!(triangular.edge_count(), 17);
}

#[test]
fn concrete_topologies_report_descriptors() {
    let grid = GridTopology::try_new(2, 3).expect("valid grid");
    assert_eq!(grid.to_spec(), TopologySpec::grid(2, 3));

    let triangular = TriangularTessellationTopology::example_3x2();
    assert_eq!(
        triangular.to_spec(),
        TopologySpec::triangular_tessellation(3, 2)
    );
}
