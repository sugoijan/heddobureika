//! Every interior triangular piece must be a (near-)exact equilateral
//! triangle. With the lattice construction the interior is regular by
//! construction, so the tolerance here is tight; only the border filler
//! pieces (which touch a frame edge) are allowed to be irregular.

use heddobureika_game::{
    LengthMm, PieceId, ShapeAtlasMm, TopologyShaper, TriDirection,
    TriangularTessellationShapeSettings, TriangularTessellationShaper,
    TriangularTessellationTopology,
};

/// Allowed deviation of each inner-piece corner from 60°. The lattice is
/// exact; this only absorbs f32 noise through the scale + atlas build.
const ANGLE_TOLERANCE_DEG: f32 = 0.5;

/// Builds the shape atlas exactly as `build_render_geometry` does: the
/// lattice is scaled uniformly to fit the image, centred.
fn build_atlas(
    direction: TriDirection,
    lines: u32,
    points: u32,
    image_w: f32,
    image_h: f32,
) -> (TriangularTessellationTopology, ShapeAtlasMm) {
    let topology = TriangularTessellationTopology::try_new_directed(direction, lines, points)
        .expect("valid triangular topology");
    let (ex, ey) = topology.pose_extent();
    let scale = (image_w / ex).min(image_h / ey);
    let shaper = TriangularTessellationShaper;
    let atlas = shaper
        .build_cache(
            &topology,
            LengthMm::try_new(ex * scale).expect("frame width"),
            LengthMm::try_new(ey * scale).expect("frame height"),
            0,
            &TriangularTessellationShapeSettings {
                corner_radius_px: 0.0,
            },
        )
        .expect("triangular cache")
        .atlas;
    (topology, atlas)
}

/// The three corner points of a (triangular) piece, in ring order.
fn piece_corners(atlas: &ShapeAtlasMm, piece: PieceId) -> Vec<(f32, f32)> {
    let ring = &atlas.pieces[piece.as_usize()].edges;
    (0..ring.len())
        .map(|i| {
            let start = atlas.piece_edge_geometry(piece, i).path.start;
            (start.x_mm(), start.y_mm())
        })
        .collect()
}

fn corner_angles(corners: &[(f32, f32)]) -> Vec<f32> {
    let n = corners.len();
    (0..n)
        .map(|i| {
            let prev = corners[(i + n - 1) % n];
            let here = corners[i];
            let next = corners[(i + 1) % n];
            let u = (prev.0 - here.0, prev.1 - here.1);
            let v = (next.0 - here.0, next.1 - here.1);
            let dot = u.0 * v.0 + u.1 * v.1;
            let mag = (u.0 * u.0 + u.1 * u.1).sqrt() * (v.0 * v.0 + v.1 * v.1).sqrt();
            if mag <= f32::EPSILON {
                return 0.0;
            }
            (dot / mag).clamp(-1.0, 1.0).acos().to_degrees()
        })
        .collect()
}

fn worst_angle_error(angles: &[f32]) -> f32 {
    angles
        .iter()
        .map(|a| (a - 60.0).abs())
        .fold(0.0_f32, f32::max)
}

/// Border (filler) pieces are those owning a frame edge.
fn border_piece_ids(atlas: &ShapeAtlasMm) -> Vec<u32> {
    let mut ids: Vec<u32> = atlas
        .border_edges
        .iter()
        .map(|e| e.piece.as_u32())
        .collect();
    ids.sort_unstable();
    ids.dedup();
    ids
}

#[test]
fn inner_pieces_are_regular_equilateral_triangles() {
    // Representative aspects/sizes (incl. zoe-samurai's 4096x2194) in both
    // directions.
    let cases = [
        (TriDirection::Horizontal, 6u32, 8u32, 4096.0_f32, 2194.0_f32),
        (TriDirection::Vertical, 6, 8, 4096.0, 2194.0),
        (TriDirection::Horizontal, 5, 5, 600.0, 400.0),
        (TriDirection::Vertical, 4, 10, 900.0, 1600.0),
        (TriDirection::Horizontal, 8, 4, 1280.0, 720.0),
    ];
    for (direction, lines, points, w, h) in cases {
        let (_topo, atlas) = build_atlas(direction, lines, points, w, h);
        let borders = border_piece_ids(&atlas);
        let mut offenders: Vec<(u32, f32, Vec<f32>)> = Vec::new();
        let mut inner_count = 0usize;
        for idx in 0..atlas.pieces.len() as u32 {
            if borders.contains(&idx) {
                continue;
            }
            let corners = piece_corners(&atlas, PieceId(idx));
            if corners.len() != 3 {
                continue;
            }
            inner_count += 1;
            let angles = corner_angles(&corners);
            let err = worst_angle_error(&angles);
            if err > ANGLE_TOLERANCE_DEG {
                offenders.push((idx, err, angles));
            }
        }
        assert!(
            inner_count > 0,
            "{direction:?} {lines}x{points} @ {w}x{h}: no inner pieces"
        );
        offenders.sort_by(|a, b| b.1.total_cmp(&a.1));
        assert!(
            offenders.is_empty(),
            "{direction:?} {lines}x{points} @ {w}x{h}: {} of {inner_count} inner pieces not \
             regular (tol {ANGLE_TOLERANCE_DEG}°). Worst: {:?}",
            offenders.len(),
            offenders.iter().take(5).collect::<Vec<_>>()
        );
    }
}
