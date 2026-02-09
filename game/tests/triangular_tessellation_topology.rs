use std::collections::BTreeSet;

use heddobureika_game::{
    AngleDeg, EdgeId, GroupId, LogicalState, PlayRules, PlayableAction, PlayableState,
    PuzzleTopology, TrianglePieceKind, TriangularTessellationTopology,
};

#[test]
fn triangular_3x2_counts_match_expected_15_pieces() {
    let topo = TriangularTessellationTopology::example_3x2();
    assert_eq!(topo.piece_row_count(), 5);
    assert_eq!(topo.piece_count(), 15);
    assert_eq!(topo.regular_piece_count(), 9);
    assert_eq!(topo.half_piece_count(), 6);
    assert_eq!(topo.edge_count(), 17);
}

#[test]
fn triangular_3x2_piece_kinds_match_rows() {
    let topo = TriangularTessellationTopology::example_3x2();

    for col in 0..3 {
        let top = topo.top_half_piece_id(col).expect("valid");
        let bottom = topo.bottom_half_piece_id(col).expect("valid");
        assert_eq!(topo.piece_kind(top), Some(TrianglePieceKind::HalfRegular));
        assert_eq!(
            topo.piece_kind(bottom),
            Some(TrianglePieceKind::HalfRegular)
        );
    }

    for row in 1..4 {
        for col in 0..3 {
            let piece = topo.regular_piece_id(row, col).expect("valid");
            assert_eq!(topo.piece_kind(piece), Some(TrianglePieceKind::Regular));
        }
    }
}

#[test]
fn triangular_step_rotation_respects_piece_kind() {
    let topo = TriangularTessellationTopology::example_3x2();
    let tolerance = AngleDeg::try_new(5.0).expect("finite");

    let regular = topo.regular_piece_id(1, 0).expect("valid regular");
    let half = topo.top_half_piece_id(0).expect("valid half");

    let regular_next =
        topo.step_rotation_cw(regular, AngleDeg::try_new(58.0).expect("finite"), tolerance);
    // Near 60 (within half tolerance), so "next 60-step" is 120.
    assert_approx(regular_next.as_degrees_f32(), 120.0);

    let half_next =
        topo.step_rotation_cw(half, AngleDeg::try_new(88.0).expect("finite"), tolerance);
    // Near 90 (within half tolerance), so "next 90-step" is 180.
    assert_approx(half_next.as_degrees_f32(), 180.0);
}

#[test]
fn triangular_playable_step_rotation_uses_kind_specific_steps() {
    let topo = TriangularTessellationTopology::example_3x2();
    let logical = LogicalState::new(topo.clone());
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let regular_piece = topo.regular_piece_id(1, 1).expect("valid");
    let half_piece = topo.top_half_piece_id(1).expect("valid");

    let regular_group = GroupId(regular_piece.as_u32());
    let half_group = GroupId(half_piece.as_u32());

    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group: regular_group,
        drop_rotation: AngleDeg::try_new(58.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCw {
        group: regular_group,
    });
    let regular_rot = playable
        .pose_of(regular_group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(regular_rot, 120.0);

    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group: half_group,
        drop_rotation: AngleDeg::try_new(88.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCw { group: half_group });
    let half_rot = playable
        .pose_of(half_group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(half_rot, 180.0);
}

#[test]
fn triangular_playable_group_step_rotation_uses_group_symmetry_intersection() {
    let topo = TriangularTessellationTopology::example_3x2();
    let regular_piece = topo.regular_piece_id(1, 1).expect("valid");
    let half_piece = topo.top_half_piece_id(1).expect("valid");

    let mut logical = LogicalState::new(topo.clone());
    let mut join_edge = None;
    for edge in 0..topo.edge_count() {
        let endpoints = topo.edge_endpoints(EdgeId(edge));
        if endpoints == (regular_piece, half_piece) || endpoints == (half_piece, regular_piece) {
            join_edge = Some(EdgeId(edge));
            break;
        }
    }
    let edge = join_edge.expect("regular and half piece should share one edge");
    assert!(logical.activate_edge(edge));

    let mut playable = PlayableState::new(logical, PlayRules::default());
    let group = playable
        .logical
        .group_of(regular_piece)
        .expect("group should exist");
    assert_eq!(playable.logical.group_of(half_piece), Some(group));

    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group,
        drop_rotation: AngleDeg::try_new(58.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCw { group });
    let cw_rot = playable
        .pose_of(group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(cw_rot, 180.0);

    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group,
        drop_rotation: AngleDeg::try_new(58.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCcw { group });
    let ccw_rot = playable
        .pose_of(group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(ccw_rot, 0.0);
}

#[test]
fn triangular_3x2_piece_adjacency_matches_expected_connections() {
    let topo = TriangularTessellationTopology::example_3x2();
    assert_eq!(topo.piece_count(), 15);

    let mut adjacency = vec![BTreeSet::<u32>::new(); topo.piece_count() as usize];
    for edge in 0..topo.edge_count() {
        let (a, b) = topo.edge_endpoints(EdgeId(edge));
        adjacency[a.as_usize()].insert(b.as_u32());
        adjacency[b.as_usize()].insert(a.as_u32());
    }

    let actual: Vec<Vec<u32>> = adjacency
        .into_iter()
        .map(|set| set.into_iter().collect::<Vec<_>>())
        .collect();

    let expected = vec![
        vec![1, 3],      // 0
        vec![0, 4],      // 1
        vec![5],         // 2
        vec![0, 6],      // 3
        vec![1, 5, 7],   // 4
        vec![2, 4, 8],   // 5
        vec![3, 7, 9],   // 6
        vec![4, 6, 10],  // 7
        vec![5, 11],     // 8
        vec![6, 12],     // 9
        vec![7, 11, 13], // 10
        vec![8, 10, 14], // 11
        vec![9, 13],     // 12
        vec![10, 12],    // 13
        vec![11],        // 14
    ];

    assert_eq!(actual, expected);
}

#[test]
fn triangular_3x2_degree_distribution_matches_expected() {
    let topo = TriangularTessellationTopology::example_3x2();
    let mut degree_buckets = [0u32; 4];

    let mut adjacency = vec![BTreeSet::<u32>::new(); topo.piece_count() as usize];
    for edge in 0..topo.edge_count() {
        let (a, b) = topo.edge_endpoints(EdgeId(edge));
        adjacency[a.as_usize()].insert(b.as_u32());
        adjacency[b.as_usize()].insert(a.as_u32());
    }

    for neighbors in adjacency {
        let d = neighbors.len();
        assert!(d <= 3, "unexpected degree {d}");
        degree_buckets[d] += 1;
    }

    assert_eq!(degree_buckets[1], 2);
    assert_eq!(degree_buckets[2], 7);
    assert_eq!(degree_buckets[3], 6);
}

#[test]
fn triangular_debug_dot_graph_contains_expected_nodes_edges_and_labels() {
    let topo = TriangularTessellationTopology::example_3x2();
    let dot = topo.debug_dot_graph();

    assert!(dot.starts_with("graph triangular_tessellation {"));
    assert!(dot.contains("p0 [label=\"0\\nhalf\\nr0c0\""));
    assert!(dot.contains("p4 [label=\"4\\nregular\\nr1c1\""));
    assert!(dot.contains("p14 [label=\"14\\nhalf\\nr4c2\""));

    // A few explicit expected edges from the canonical adjacency map.
    assert!(dot.contains("p0 -- p1;"));
    assert!(dot.contains("p0 -- p3;"));
    assert!(dot.contains("p10 -- p13;"));
    assert!(dot.contains("p11 -- p14;"));
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-3,
        "expected {expected}, got {actual}"
    );
}
