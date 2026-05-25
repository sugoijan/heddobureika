use std::collections::BTreeSet;

use heddobureika_game::{
    AngleDeg, EdgeId, PieceId, PuzzleTopology, TriDirection, TrianglePieceKind,
    TriangularTessellationTopology,
};

fn find_kind(topo: &TriangularTessellationTopology, kind: TrianglePieceKind) -> Option<PieceId> {
    (0..topo.piece_count())
        .map(PieceId)
        .find(|&p| topo.piece_kind(p) == Some(kind))
}

#[test]
fn lattice_topology_has_consistent_structure_both_directions() {
    for direction in [TriDirection::Horizontal, TriDirection::Vertical] {
        let topo = TriangularTessellationTopology::try_new_directed(direction, 4, 6)
            .expect("valid topology");
        let n = topo.piece_count();
        assert!(n > 0, "{direction:?}: no pieces");

        // Edges: valid endpoints, no self-loops, degree <= 3, symmetric.
        let mut adjacency = vec![BTreeSet::<u32>::new(); n as usize];
        for e in 0..topo.edge_count() {
            let (a, b) = topo.edge_endpoints(EdgeId(e));
            assert!(
                a.as_u32() < n && b.as_u32() < n,
                "{direction:?}: edge out of range"
            );
            assert_ne!(a, b, "{direction:?}: self-loop edge");
            adjacency[a.as_usize()].insert(b.as_u32());
            adjacency[b.as_usize()].insert(a.as_u32());
        }
        for (p, neigh) in adjacency.iter().enumerate() {
            assert!(
                neigh.len() <= 3,
                "{direction:?}: piece {p} has degree {}",
                neigh.len()
            );
        }

        // Both kinds present; border classification matches is_frame_border_piece.
        assert!(find_kind(&topo, TrianglePieceKind::Regular).is_some());
        assert!(find_kind(&topo, TrianglePieceKind::Border).is_some());
        for p in (0..n).map(PieceId) {
            let is_border = topo.piece_kind(p) == Some(TrianglePieceKind::Border);
            assert_eq!(
                is_border,
                topo.is_frame_border_piece(p),
                "{direction:?}: piece {} kind/border mismatch",
                p.as_u32()
            );
        }

        // Canonical centroids are distinct.
        let mut seen = std::collections::HashSet::new();
        for p in (0..n).map(PieceId) {
            let (x, y) = topo.canonical_position_in_pose_units(p).expect("centroid");
            let key = ((x * 1000.0) as i32, (y * 1000.0) as i32);
            assert!(
                seen.insert(key),
                "{direction:?}: duplicate centroid for {}",
                p.as_u32()
            );
        }
    }
}

#[test]
fn regular_pieces_rotate_in_60_steps_borders_in_90() {
    let topo = TriangularTessellationTopology::try_new(4, 6).expect("topology");
    let tolerance = AngleDeg::try_new(5.0).expect("finite");

    let regular = find_kind(&topo, TrianglePieceKind::Regular).expect("a regular piece");
    let border = find_kind(&topo, TrianglePieceKind::Border).expect("a border piece");

    // Regular (equilateral) interior pieces step by 60°: 58° -> 120°.
    let regular_next =
        topo.step_rotation_cw(regular, AngleDeg::try_new(58.0).expect("finite"), tolerance);
    assert_approx(regular_next.as_degrees_f32(), 120.0);

    // Border fillers step like the grid (90°): 88° -> 180°.
    let border_next =
        topo.step_rotation_cw(border, AngleDeg::try_new(88.0).expect("finite"), tolerance);
    assert_approx(border_next.as_degrees_f32(), 180.0);
}

#[test]
fn debug_dot_graph_is_well_formed() {
    let topo = TriangularTessellationTopology::try_new(3, 5).expect("topology");
    let dot = topo.debug_dot_graph();
    assert!(dot.starts_with("graph triangular_tessellation {"));
    assert!(dot.contains("regular") || dot.contains("border"));
    assert!(dot.trim_end().ends_with('}'));
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-3,
        "expected {expected}, got {actual}"
    );
}
