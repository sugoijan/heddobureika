use heddobureika_game::{
    EdgeId, LengthMm, PathMm, PathSegMm, PieceGeometryProvider, PointMm, PuzzleTopology,
    TopologyShaper, TriangularTessellationShapeSettings, TriangularTessellationShaper,
    TriangularTessellationTopology,
};

const EPS: f32 = 0.75;

#[test]
fn triangular_shaper_validates_atlas_invariants() {
    let topology = TriangularTessellationTopology::example_3x2();
    let shaper = TriangularTessellationShaper;
    let cache = shaper
        .build_cache(
            &topology,
            LengthMm::try_new(100.0).expect("finite width"),
            LengthMm::try_new(86.0).expect("finite height"),
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("triangular cache should build");

    assert_eq!(cache.piece_count(), topology.piece_count());
    assert_eq!(
        cache.atlas.interior_edges.len(),
        topology.edge_count() as usize
    );
    cache
        .atlas
        .validate(&topology)
        .expect("triangular atlas should validate");
}

#[test]
fn triangular_interior_edge_sides_match_in_board_space() {
    let topology = TriangularTessellationTopology::example_3x2();
    let cache = triangular_cache(&topology, 320.0, 180.0);

    for edge_idx in 0..topology.edge_count() {
        let edge = cache.interior_edge_geometry(EdgeId(edge_idx));
        let a = sample_path(&edge.side_a.path, 8);
        let b = sample_path(&edge.side_b.path, 8);
        assert_eq!(a.len(), b.len(), "edge {edge_idx} sample count");
        for (idx, pa) in a.iter().enumerate() {
            let pb = b[b.len() - 1 - idx];
            assert!(
                distance(*pa, pb) <= 0.01,
                "edge {edge_idx} sample {idx} mismatch: {pa:?} vs {pb:?}"
            );
        }
    }
}

#[test]
fn triangular_shapes_cover_source_frame_for_3x2_and_larger() {
    assert_triangular_coverage(3, 5);
    assert_triangular_coverage(4, 6);
}

#[test]
fn triangular_interior_edges_use_profiled_tabs() {
    let topology = TriangularTessellationTopology::example_3x2();
    let cache = triangular_cache(&topology, 320.0, 180.0);

    let curved_edges = (0..topology.edge_count())
        .filter(|edge_idx| {
            let edge = cache.interior_edge_geometry(EdgeId(*edge_idx));
            edge.side_a
                .path
                .segs
                .iter()
                .any(|seg| matches!(seg, PathSegMm::CubicTo { .. }))
        })
        .count();

    assert!(
        curved_edges > 0,
        "triangular interior edges should include curved tab/blank segments"
    );
}

fn triangular_cache(
    topology: &TriangularTessellationTopology,
    width: f32,
    height: f32,
) -> heddobureika_game::TriangularTessellationShapeCache {
    let shaper = TriangularTessellationShaper;
    shaper
        .build_cache(
            topology,
            LengthMm::try_new(width).expect("finite width"),
            LengthMm::try_new(height).expect("finite height"),
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("triangular cache should build")
}

fn assert_triangular_coverage(lines: u32, points: u32) {
    let topology = TriangularTessellationTopology::try_new(lines, points).expect("topology");
    // Frame must match the lattice's pose-extent aspect so the uniform scale
    // fills it exactly (otherwise the mesh fills only one axis).
    let (ex, ey) = topology.pose_extent();
    let scale = 60.0_f32;
    let width = ex * scale;
    let height = ey * scale;
    let cache = triangular_cache(&topology, width, height);
    let outlines = (0..topology.piece_count())
        .map(|piece| {
            flatten_path(
                &cache.atlas.piece_outline(heddobureika_game::PieceId(piece)),
                10,
            )
        })
        .collect::<Vec<_>>();

    let (min_x, min_y, max_x, max_y) = aggregate_bounds(&outlines);
    assert!((min_x - 0.0).abs() <= EPS, "min x {min_x}");
    assert!((min_y - 0.0).abs() <= EPS, "min y {min_y}");
    assert!((max_x - width).abs() <= EPS, "max x {max_x}");
    assert!((max_y - height).abs() <= EPS, "max y {max_y}");

    for y_idx in 0..13 {
        for x_idx in 0..17 {
            let x = ((x_idx as f32 + 0.37) / 17.0) * width;
            let y = ((y_idx as f32 + 0.41) / 13.0) * height;
            let covered = outlines
                .iter()
                .filter(|outline| point_in_polygon((x, y), outline))
                .count();
            assert_eq!(
                covered, 1,
                "point ({x:.2}, {y:.2}) should be covered by exactly one piece"
            );
        }
    }

    for outline in &outlines {
        for &(x, y) in outline {
            assert!(
                x >= -EPS && x <= width + EPS,
                "outline x outside frame: {x}"
            );
            assert!(
                y >= -EPS && y <= height + EPS,
                "outline y outside frame: {y}"
            );
        }
    }
}

fn aggregate_bounds(polygons: &[Vec<(f32, f32)>]) -> (f32, f32, f32, f32) {
    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;
    for polygon in polygons {
        for &(x, y) in polygon {
            min_x = min_x.min(x);
            min_y = min_y.min(y);
            max_x = max_x.max(x);
            max_y = max_y.max(y);
        }
    }
    (min_x, min_y, max_x, max_y)
}

fn sample_path(path: &PathMm, cubic_steps: usize) -> Vec<(f32, f32)> {
    flatten_path(path, cubic_steps)
}

fn flatten_path(path: &PathMm, cubic_steps: usize) -> Vec<(f32, f32)> {
    let mut out = vec![point_tuple(path.start)];
    let mut current = point_tuple(path.start);
    let steps = cubic_steps.max(1);
    for seg in path.segs.iter() {
        match *seg {
            PathSegMm::LineTo { to } => {
                current = point_tuple(to);
                out.push(current);
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                let p1 = point_tuple(c1);
                let p2 = point_tuple(c2);
                let p3 = point_tuple(to);
                for step in 1..=steps {
                    out.push(cubic_point(current, p1, p2, p3, step as f32 / steps as f32));
                }
                current = p3;
            }
        }
    }
    out
}

fn point_tuple(point: PointMm) -> (f32, f32) {
    (point.x_mm(), point.y_mm())
}

fn cubic_point(
    p0: (f32, f32),
    p1: (f32, f32),
    p2: (f32, f32),
    p3: (f32, f32),
    t: f32,
) -> (f32, f32) {
    let u = 1.0 - t;
    let tt = t * t;
    let uu = u * u;
    let uuu = uu * u;
    let ttt = tt * t;
    (
        uuu * p0.0 + 3.0 * uu * t * p1.0 + 3.0 * u * tt * p2.0 + ttt * p3.0,
        uuu * p0.1 + 3.0 * uu * t * p1.1 + 3.0 * u * tt * p2.1 + ttt * p3.1,
    )
}

fn point_in_polygon(point: (f32, f32), polygon: &[(f32, f32)]) -> bool {
    let (x, y) = point;
    let mut inside = false;
    for idx in 0..polygon.len() {
        let (x1, y1) = polygon[idx];
        let (x2, y2) = polygon[(idx + 1) % polygon.len()];
        if ((y1 > y) != (y2 > y)) && x < (x2 - x1) * (y - y1) / (y2 - y1) + x1 {
            inside = !inside;
        }
    }
    inside
}

fn distance(a: (f32, f32), b: (f32, f32)) -> f32 {
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    (dx * dx + dy * dy).sqrt()
}
