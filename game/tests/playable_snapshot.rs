use heddobureika_game::{
    GridTopology, LogicalState, MergePolicy, PlayRules, PlayableAction, PlayableSnapshot,
    PlayableSnapshotError, PlayableState, Position2, PuzzleTopology, SnapshotEnvelope,
    TopologySpec, TriangularTessellationTopology,
};

#[test]
fn playable_snapshot_round_trips_authoritative_state() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable =
        PlayableState::new(LogicalState::new(topology.clone()), PlayRules::default());
    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.0, 0.0).expect("finite"),
        },
        Some(heddobureika_game::ActionId(11)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.revision_after, 1);

    let snapshot = PlayableSnapshot::from_playable(&playable, Some(heddobureika_game::PieceId(1)));
    assert_eq!(snapshot.topology, TopologySpec::grid(2, 1));
    snapshot
        .validate_for_topology(&topology)
        .expect("snapshot should validate against its topology");

    let restored = snapshot
        .restore(topology)
        .expect("snapshot should restore into playable state");

    assert_eq!(restored.revision, playable.revision);
    assert_eq!(
        restored.logical.edge_active_slice(),
        playable.logical.edge_active_slice()
    );
    assert_eq!(
        restored.logical.piece_groups().collect::<Vec<_>>(),
        playable.logical.piece_groups().collect::<Vec<_>>()
    );
    assert_eq!(restored.group_pose.to_vec(), playable.group_pose.to_vec());
    assert_eq!(restored.group_flip.to_vec(), playable.group_flip.to_vec());
    assert_eq!(restored.z_order, playable.z_order);
    assert_eq!(restored.rules, playable.rules);
    restored
        .validate()
        .expect("restored state should preserve invariants");
}

#[test]
fn playable_snapshot_restores_known_grid_topology_from_spec() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let snapshot = PlayableSnapshot::from_playable(&playable, None);

    let restored = snapshot
        .restore_from_spec()
        .expect("grid snapshot should restore from spec");

    assert_eq!(restored.logical.piece_count(), 2);
    assert_eq!(restored.logical.edge_count(), 1);
}

#[test]
fn playable_snapshot_records_triangular_topology_descriptor() {
    let topology = TriangularTessellationTopology::example_3x2();
    let expected_pieces = topology.piece_count();
    let expected_edges = topology.edge_count();
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let snapshot = PlayableSnapshot::from_playable(&playable, None);

    // `example_3x2` is a horizontal (3 lines, 5 points) lattice.
    assert_eq!(
        snapshot.topology,
        TopologySpec::triangular_tessellation(3, 5)
    );
    assert_eq!(snapshot.topology_piece_count, expected_pieces);
    assert_eq!(snapshot.topology_edge_count, expected_edges);

    let restored = snapshot
        .restore_from_spec()
        .expect("triangular snapshot should restore from spec");
    assert_eq!(restored.logical.piece_count() as u32, expected_pieces);
    assert_eq!(restored.logical.edge_count() as u32, expected_edges);
}

#[test]
fn snapshot_envelope_records_current_version() {
    let topology = GridTopology::try_new(1, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let envelope = SnapshotEnvelope::from_playable(&playable, None);

    envelope
        .validate_version()
        .expect("fresh envelope should use current version");
}

#[test]
fn snapshot_validation_rejects_active_edge_across_groups() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology.clone()), PlayRules::default());
    let mut snapshot = PlayableSnapshot::from_playable(&playable, None);
    snapshot.edge_active[0] = true;

    let err = snapshot
        .validate_for_topology(&topology)
        .expect_err("active edge must require matching piece groups");

    assert!(matches!(
        err,
        PlayableSnapshotError::Logical(
            heddobureika_game::LogicalInvariantError::ActiveEdgeAcrossGroups { .. }
        )
    ));
}

#[test]
fn snapshot_restore_rejects_wrong_topology_with_same_counts() {
    let horizontal = GridTopology::try_new(2, 1).expect("valid grid");
    let vertical = GridTopology::try_new(1, 2).expect("valid grid");
    assert_eq!(horizontal.piece_count(), vertical.piece_count());
    assert_eq!(horizontal.edge_count(), vertical.edge_count());

    let playable = PlayableState::new(LogicalState::new(horizontal), PlayRules::default());
    let snapshot = PlayableSnapshot::from_playable(&playable, None);

    let err = match snapshot.restore(vertical) {
        Ok(_) => panic!("same-count topology with different local poses should reject"),
        Err(err) => err,
    };

    assert!(matches!(
        err,
        PlayableSnapshotError::TopologySpecMismatch {
            snapshot,
            expected,
        } if snapshot == TopologySpec::grid(2, 1)
            && expected == TopologySpec::grid(1, 2)
    ));
}

#[test]
fn snapshot_validation_rejects_mutated_topology_descriptor_counts() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology.clone()), PlayRules::default());
    let mut snapshot = PlayableSnapshot::from_playable(&playable, None);
    snapshot.topology = TopologySpec::grid(3, 1);

    let err = snapshot
        .validate_for_topology(&topology)
        .expect_err("mutated topology spec should reject");

    assert!(matches!(
        err,
        PlayableSnapshotError::TopologySpecMismatch {
            snapshot,
            expected,
        } if snapshot == TopologySpec::grid(3, 1)
            && expected == TopologySpec::grid(2, 1)
    ));
}

#[test]
fn snapshot_validation_rejects_bad_lengths() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology.clone()), PlayRules::default());
    let mut snapshot = PlayableSnapshot::from_playable(&playable, None);
    snapshot.piece_group.pop();

    let err = snapshot
        .validate_for_topology(&topology)
        .expect_err("truncated piece groups should reject");

    assert!(matches!(
        err,
        PlayableSnapshotError::PieceGroupLenMismatch {
            expected: 2,
            actual: 1
        }
    ));
}

#[test]
fn snapshot_validation_rejects_missing_alive_group_in_z_order() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology.clone()), PlayRules::default());
    let mut snapshot = PlayableSnapshot::from_playable(&playable, None);
    snapshot
        .z_order
        .retain(|group| *group != heddobureika_game::GroupId(1));

    let err = snapshot
        .validate_for_topology(&topology)
        .expect_err("z-order must include every alive group");

    assert!(matches!(
        err,
        PlayableSnapshotError::ZOrderMissingAliveGroup {
            group: heddobureika_game::GroupId(1)
        }
    ));
}

#[test]
fn snapshot_validation_rejects_dead_group_in_z_order() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology.clone());
    assert!(logical.activate_edge(heddobureika_game::EdgeId(0)));
    let playable = PlayableState::new(logical, PlayRules::default());
    let mut snapshot = PlayableSnapshot::from_playable(&playable, None);
    snapshot.z_order.push(heddobureika_game::GroupId(1));

    let err = snapshot
        .validate_for_topology(&topology)
        .expect_err("z-order must not include dead groups");

    assert!(matches!(
        err,
        PlayableSnapshotError::ZOrderDeadGroup {
            group: heddobureika_game::GroupId(1)
        }
    ));
}
