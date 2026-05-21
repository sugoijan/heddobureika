//! Topology-agnostic regression tests for `PuzzleTopology::build_render_geometry`.
//!
//! These tests pin down two behaviours that broke during the Phase 9
//! refactor when the grid shaper switched to the canonical
//! `GridJigsawShaper` pipeline:
//!
//! 1. Each piece's `image_origin_px` (the per-piece offset into the
//!    puzzle image used by the renderer to sample the right slice) must
//!    be unique per piece and tile the puzzle bounds. Symptom of
//!    breakage: every piece samples from the puzzle's top-left corner.
//! 2. Each piece's `bounds_px` must sit inside the puzzle bounds at the
//!    piece's intended row/column. Symptom of breakage: scrambling
//!    pushes every piece outside the workspace because the bbox starts
//!    near (0,0) for every piece.

use heddobureika_game::{
    build_topology_from_spec, GridShapeSettings, PuzzleTopology, TopologySpec,
    TriangularTessellationShapeSettings,
};

const IMAGE_W: u32 = 600;
const IMAGE_H: u32 = 400;

#[test]
fn grid_render_geometry_assigns_distinct_image_origins_per_piece() {
    let topology = build_topology_from_spec(&TopologySpec::grid(3, 2)).expect("grid topology");
    let geom = topology
        .build_render_geometry(IMAGE_W, IMAGE_H, 0, &GridShapeSettings::default())
        .expect("render geometry");
    assert_eq!(geom.pieces.len(), 6);
    let piece_w = IMAGE_W as f32 / 3.0;
    let piece_h = IMAGE_H as f32 / 2.0;
    for (id, piece) in geom.pieces.iter().enumerate() {
        let col = (id % 3) as f32;
        let row = (id / 3) as f32;
        let expected_x = col * piece_w;
        let expected_y = row * piece_h;
        // `image_origin_px` is the piece's top-left in image-space pixels.
        // It must equal the bbox top-left and reflect the piece's actual
        // position in the puzzle image — never (0,0) for non-(0,0) pieces.
        assert!(
            (piece.image_origin_px[0] - expected_x).abs() <= piece_w * 0.5,
            "piece {} image_origin_px.x = {} expected near {} (piece_w {})",
            id,
            piece.image_origin_px[0],
            expected_x,
            piece_w
        );
        assert!(
            (piece.image_origin_px[1] - expected_y).abs() <= piece_h * 0.5,
            "piece {} image_origin_px.y = {} expected near {} (piece_h {})",
            id,
            piece.image_origin_px[1],
            expected_y,
            piece_h
        );
    }
}

#[test]
fn grid_render_geometry_bbox_tiles_the_image_bounds() {
    let topology = build_topology_from_spec(&TopologySpec::grid(3, 2)).expect("grid topology");
    let geom = topology
        .build_render_geometry(IMAGE_W, IMAGE_H, 0, &GridShapeSettings::default())
        .expect("render geometry");
    // Every piece's bbox must be inside an expanded puzzle bounds
    // (allowing for tab extrusion via mask_pad). The bbox centres must
    // be distinct — at minimum one piece per grid cell.
    let mut centres = Vec::new();
    let pad = geom.mask_pad_px;
    for piece in &geom.pieces {
        let cx = piece.bounds_px.x + piece.bounds_px.width * 0.5;
        let cy = piece.bounds_px.y + piece.bounds_px.height * 0.5;
        assert!(
            cx >= -pad && cx <= IMAGE_W as f32 + pad,
            "piece {} centre.x = {} outside image bounds (pad {})",
            piece.id.as_u32(),
            cx,
            pad
        );
        assert!(
            cy >= -pad && cy <= IMAGE_H as f32 + pad,
            "piece {} centre.y = {} outside image bounds (pad {})",
            piece.id.as_u32(),
            cy,
            pad
        );
        centres.push((cx, cy));
    }
    for i in 0..centres.len() {
        for j in (i + 1)..centres.len() {
            let (xi, yi) = centres[i];
            let (xj, yj) = centres[j];
            let dx = (xi - xj).abs();
            let dy = (yi - yj).abs();
            assert!(
                dx > 1.0 || dy > 1.0,
                "pieces {} and {} share centre near ({}, {})",
                i,
                j,
                xi,
                yi
            );
        }
    }
}

#[test]
fn grid_pose_to_piece_top_left_round_trips_each_piece_to_its_image_origin() {
    use heddobureika_game::{PieceId, Pose2};
    let topology = build_topology_from_spec(&TopologySpec::grid(3, 2)).expect("grid topology");
    let geom = topology
        .build_render_geometry(IMAGE_W, IMAGE_H, 0, &GridShapeSettings::default())
        .expect("render geometry");
    // At each piece's canonical pose (col+0.5, row+0.5 in pose units),
    // `pose_to_piece_top_left` should land the piece's top-left at its
    // image_origin_px. Symptom of the prior bug: every piece landed at
    // (0,0) regardless of pose.
    for (id, piece) in geom.pieces.iter().enumerate() {
        let col = (id % 3) as f32;
        let row = (id / 3) as f32;
        let pose = Pose2::try_from_mm_degrees(col + 0.5, row + 0.5, 0.0).expect("finite pose");
        let (top_left_x, top_left_y) = geom
            .pose_to_piece_top_left(PieceId(id as u32), pose)
            .expect("pose result");
        assert!(
            (top_left_x - piece.image_origin_px[0]).abs() <= 1.0,
            "piece {} top_left.x = {} expected {}",
            id,
            top_left_x,
            piece.image_origin_px[0]
        );
        assert!(
            (top_left_y - piece.image_origin_px[1]).abs() <= 1.0,
            "piece {} top_left.y = {} expected {}",
            id,
            top_left_y,
            piece.image_origin_px[1]
        );
    }
}

#[test]
fn triangular_render_geometry_spreads_pieces_across_the_image() {
    let topology = build_topology_from_spec(&TopologySpec::triangular_tessellation(3, 2))
        .expect("triangular topology");
    let geom = topology
        .build_render_geometry(
            IMAGE_W,
            IMAGE_H,
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("render geometry");
    // 3x2 triangular tessellation has 9 regular + 6 half = 15 pieces.
    assert_eq!(geom.pieces.len(), 15);
    // Triangular half-pieces at corners can share an image_origin
    // (their bboxes overlap), so we don't require strict uniqueness.
    // What we *do* require: piece centres span both axes — if every
    // piece lived at the top-left we'd have width/height clustered at
    // 0, which is the symptom of the grid Phase 9 regression.
    let mut xs: Vec<f32> = Vec::new();
    let mut ys: Vec<f32> = Vec::new();
    for piece in &geom.pieces {
        xs.push(piece.bounds_px.x + piece.bounds_px.width * 0.5);
        ys.push(piece.bounds_px.y + piece.bounds_px.height * 0.5);
    }
    let max_x = xs.iter().cloned().fold(f32::MIN, f32::max);
    let max_y = ys.iter().cloned().fold(f32::MIN, f32::max);
    assert!(
        max_x > IMAGE_W as f32 * 0.5,
        "no triangular piece reaches the right half (max x = {})",
        max_x
    );
    assert!(
        max_y > IMAGE_H as f32 * 0.5,
        "no triangular piece reaches the bottom half (max y = {})",
        max_y
    );
}

// ---- Frame-shape consistency / corner rounding -----------------------------

const FRAME_IMAGE_W: u32 = 800;
const FRAME_IMAGE_H: u32 = 600;

#[test]
fn frame_shape_populated_and_positive_across_topologies() {
    let cases: &[(&str, TopologySpec)] = &[
        ("grid", TopologySpec::grid(8, 6)),
        ("triangular", TopologySpec::triangular_tessellation(4, 3)),
        ("voronoi", TopologySpec::voronoi(48, 1, 4.0 / 3.0)),
    ];
    for (label, spec) in cases {
        let topology = build_topology_from_spec(spec).unwrap_or_else(|| panic!("{label} topology"));
        let geom = topology
            .build_render_geometry(
                FRAME_IMAGE_W,
                FRAME_IMAGE_H,
                0,
                &GridShapeSettings::default(),
            )
            .unwrap_or_else(|| panic!("{label} render geometry"));
        let frame = geom.frame_shape;
        assert_eq!(frame.bounds.width, FRAME_IMAGE_W as f32, "{label} bounds.w");
        assert_eq!(
            frame.bounds.height, FRAME_IMAGE_H as f32,
            "{label} bounds.h"
        );
        assert!(
            frame.corner_radius_px > 0.0,
            "{label} corner_radius_px = {} should be positive",
            frame.corner_radius_px
        );
        assert!(
            frame.corner_radius_px.is_finite(),
            "{label} corner_radius_px is not finite"
        );
        // Radius should be substantially smaller than the image — a
        // sanity bound around 10% catches the prior Voronoi-only formula
        // that produced ~12% of the image dimension.
        assert!(
            frame.corner_radius_px < (FRAME_IMAGE_W.min(FRAME_IMAGE_H) as f32) * 0.10,
            "{label} corner_radius_px = {} is suspiciously large for {}x{}",
            frame.corner_radius_px,
            FRAME_IMAGE_W,
            FRAME_IMAGE_H
        );
    }
}

/// Returns the (global) coordinates of each `M` / `L` command in
/// `piece.outline_svg`, in image-space pixels.
fn collect_piece_vertices(piece: &heddobureika_game::PieceRenderGeometry) -> Vec<(f32, f32)> {
    let mut out = Vec::new();
    let origin_x = piece.image_origin_px[0];
    let origin_y = piece.image_origin_px[1];
    let mut tokens = piece.outline_svg.split_ascii_whitespace();
    while let Some(tok) = tokens.next() {
        if tok == "M" || tok == "L" {
            if let (Some(xs), Some(ys)) = (tokens.next(), tokens.next()) {
                if let (Ok(x), Ok(y)) = (xs.parse::<f32>(), ys.parse::<f32>()) {
                    out.push((origin_x + x, origin_y + y));
                }
            }
        }
    }
    out
}

/// Returns the signed radial distance from a point to the rounded-rect
/// boundary (positive outside, negative inside, zero on the path).
fn rounded_rect_signed_distance(p: (f32, f32), w: f32, h: f32, r: f32) -> f32 {
    let (x, y) = p;
    // Identify which region the point falls into. We treat the
    // rounded-rect boundary as the locus where:
    //   - on straight sides: x ∈ {0, w} for the verticals, y ∈ {0, h}
    //     for the horizontals, with the orthogonal coord in [r, w-r]
    //     or [r, h-r] respectively;
    //   - on arcs: |distance to corner-arc center| == r, inside the
    //     corresponding `(0..r, 0..r)` quadrant.
    if x >= r && x <= w - r {
        // In the horizontal band; closest boundary is top or bottom.
        return (y - 0.0).abs().min((y - h).abs());
    }
    if y >= r && y <= h - r {
        // In the vertical band; closest boundary is left or right.
        return (x - 0.0).abs().min((x - w).abs());
    }
    // Otherwise we're in a corner cell; closest is the corner arc.
    let (cx, cy) = if x < r && y < r {
        (r, r)
    } else if x > w - r && y < r {
        (w - r, r)
    } else if x > w - r && y > h - r {
        (w - r, h - r)
    } else {
        (r, h - r)
    };
    let dx = x - cx;
    let dy = y - cy;
    let d = (dx * dx + dy * dy).sqrt();
    d - r
}

/// Samples roughly evenly-spaced points along an SVG path string in
/// piece-local coords. Each `L` command becomes one straight segment;
/// each `C` command becomes a cubic Bézier sampled at `bezier_steps`
/// internal parameter values.
fn sample_svg_path_local(svg: &str, bezier_steps: usize) -> Vec<(f32, f32)> {
    let mut out = Vec::new();
    let mut current = (0.0_f32, 0.0_f32);
    let mut tokens = svg.split_ascii_whitespace().peekable();
    while let Some(tok) = tokens.next() {
        match tok {
            "M" => {
                let x = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let y = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                current = (x, y);
                out.push(current);
            }
            "L" => {
                let x = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let y = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let to = (x, y);
                // Sample a few interior points along the line.
                let line_steps = 4;
                for step in 1..line_steps {
                    let t = step as f32 / line_steps as f32;
                    out.push((
                        current.0 + (to.0 - current.0) * t,
                        current.1 + (to.1 - current.1) * t,
                    ));
                }
                out.push(to);
                current = to;
            }
            "C" => {
                let c1x = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let c1y = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let c2x = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let c2y = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let x = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let y = tokens
                    .next()
                    .and_then(|s| s.parse::<f32>().ok())
                    .unwrap_or(0.0);
                let to = (x, y);
                for step in 1..=bezier_steps {
                    let t = step as f32 / bezier_steps as f32;
                    let mt = 1.0 - t;
                    let bx = mt * mt * mt * current.0
                        + 3.0 * mt * mt * t * c1x
                        + 3.0 * mt * t * t * c2x
                        + t * t * t * to.0;
                    let by = mt * mt * mt * current.1
                        + 3.0 * mt * mt * t * c1y
                        + 3.0 * mt * t * t * c2y
                        + t * t * t * to.1;
                    out.push((bx, by));
                }
                current = to;
            }
            _ => {}
        }
    }
    out
}

#[test]
fn triangular_corner_outlines_hug_rounded_rect_within_tight_margin() {
    // For every border edge of every triangular piece, sample many
    // points along the actual emitted path and confirm each sample
    // lies within a tight pixel margin of the rounded-rect boundary.
    // Three failure modes this catches:
    //  - chord between arc-vertex and straight-section vertex cutting
    //    inside (the "tapering inward" symptom);
    //  - cubic Bézier control points placed incorrectly;
    //  - wrong arc center (e.g. corner-quadrant mis-assignment).
    let topology = build_topology_from_spec(&TopologySpec::triangular_tessellation(4, 3))
        .expect("triangular topology");
    let geom = topology
        .build_render_geometry(
            FRAME_IMAGE_W,
            FRAME_IMAGE_H,
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("render geometry");
    let frame = geom.frame_shape;
    let radius = frame.corner_radius_px;
    assert!(radius > 0.0);

    // Tight tolerance: the cubic-Bézier arc approximation has max
    // radial error ~0.0002·r over a 45° sweep; our sub-segments are
    // shorter so the actual error is lower. Allow 1% of the radius
    // plus a tiny absolute floor to absorb float noise in the SVG
    // round-trip.
    let tol = (radius * 0.01).max(0.05);
    let w = FRAME_IMAGE_W as f32;
    let h = FRAME_IMAGE_H as f32;

    let mut sampled = 0usize;
    let mut corner_samples = 0usize;
    for piece in &geom.pieces {
        for (idx, edge_svg) in piece.edge_svgs.iter().enumerate() {
            if piece.topology_edges[idx].is_some() {
                continue; // skip interior edges
            }
            let origin_x = piece.image_origin_px[0];
            let origin_y = piece.image_origin_px[1];
            for (lx, ly) in sample_svg_path_local(edge_svg, 12) {
                let gx = origin_x + lx;
                let gy = origin_y + ly;
                let d = rounded_rect_signed_distance((gx, gy), w, h, radius);
                assert!(
                    d.abs() <= tol,
                    "border edge sample ({:.3}, {:.3}) on piece {} is {:.3} away from the \
                     rounded rect (radius {:.3}, tol {:.3})",
                    gx,
                    gy,
                    piece.id.as_u32(),
                    d,
                    radius,
                    tol
                );
                sampled += 1;
                // Track samples that actually fall in a corner-cell so
                // we know we exercised the arc path, not just the
                // straight sides.
                let in_corner = (gx < radius && gy < radius)
                    || (gx > w - radius && gy < radius)
                    || (gx > w - radius && gy > h - radius)
                    || (gx < radius && gy > h - radius);
                if in_corner {
                    corner_samples += 1;
                }
            }
        }
    }
    assert!(sampled > 100, "expected many samples, got {}", sampled);
    assert!(
        corner_samples >= 12,
        "expected the test to actually exercise the corner arcs — got {} corner samples",
        corner_samples
    );
}

#[test]
fn triangular_corner_outlines_use_cubic_arcs_not_chords() {
    // The boundary builder emits a single cubic Bézier per pair of
    // consecutive boundary vertices that land on the same corner arc;
    // a straight `L` chord would coarsely undershoot the arc. So at
    // least one piece's outline should contain a `C` command for the
    // optimisation to be in effect. Guard against a regression to
    // chord-only output.
    let topology = build_topology_from_spec(&TopologySpec::triangular_tessellation(4, 3))
        .expect("triangular topology");
    let geom = topology
        .build_render_geometry(
            FRAME_IMAGE_W,
            FRAME_IMAGE_H,
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("render geometry");
    let cubic_count: usize = geom
        .pieces
        .iter()
        .flat_map(|piece| piece.edge_svgs.iter())
        .filter(|svg| svg.split_ascii_whitespace().any(|tok| tok == "C"))
        .count();
    assert!(
        cubic_count > 0,
        "expected at least one CubicTo on a corner arc edge — found none"
    );
}

#[test]
fn triangular_corner_outlines_stay_inside_rounded_frame() {
    // With `rectangularize_mesh` snapping the mesh boundary onto a
    // rounded-rect path, no outline vertex of any triangular piece
    // should land inside the corner-exclusion region (the small
    // triangular area between a rect corner and the rounded arc).
    let topology = build_topology_from_spec(&TopologySpec::triangular_tessellation(4, 3))
        .expect("triangular topology");
    let geom = topology
        .build_render_geometry(
            FRAME_IMAGE_W,
            FRAME_IMAGE_H,
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("render geometry");
    let frame = geom.frame_shape;
    let radius = frame.corner_radius_px;
    assert!(radius > 0.0, "triangular radius should be positive");
    let corners: [[f32; 2]; 4] = [
        [0.0, 0.0],
        [FRAME_IMAGE_W as f32, 0.0],
        [0.0, FRAME_IMAGE_H as f32],
        [FRAME_IMAGE_W as f32, FRAME_IMAGE_H as f32],
    ];
    // Vertices on the arc itself satisfy
    // `(lx - radius)^2 + (ly - radius)^2 <= radius^2` (where lx/ly are
    // the absolute distances from the rect corner). The chord-based
    // approximation introduces a small inward error, so we allow a
    // generous slack proportional to the chord length.
    let slack = radius * radius * 0.5;
    for piece in geom.pieces.iter() {
        for corner in &corners {
            for (vx, vy) in collect_piece_vertices(piece) {
                let lx = (vx - corner[0]).abs();
                let ly = (vy - corner[1]).abs();
                if lx >= radius * 0.999 || ly >= radius * 0.999 {
                    continue;
                }
                let center_dist_sq = (lx - radius) * (lx - radius) + (ly - radius) * (ly - radius);
                assert!(
                    center_dist_sq <= radius * radius + slack,
                    "piece {} has outline vertex ({:.2}, {:.2}) inside corner exclusion region near {:?} (radius {})",
                    piece.id.as_u32(),
                    vx,
                    vy,
                    corner,
                    radius
                );
            }
        }
    }
}

#[test]
fn frame_shape_corner_radius_is_comparable_across_topologies() {
    // Same image, same target piece count → frame radius should land
    // within the same order of magnitude across topology kinds. Before
    // the unification, Voronoi's radius was 2-3× grid's; this guards
    // against that regression.
    let grid = build_topology_from_spec(&TopologySpec::grid(10, 10))
        .expect("grid")
        .build_render_geometry(1000, 1000, 0, &GridShapeSettings::default())
        .expect("grid geom");
    let voronoi = build_topology_from_spec(&TopologySpec::voronoi(100, 1, 1.0))
        .expect("voronoi")
        .build_render_geometry(1000, 1000, 0, &GridShapeSettings::default())
        .expect("voronoi geom");
    let tri = build_topology_from_spec(&TopologySpec::triangular_tessellation(7, 7))
        .expect("triangular")
        .build_render_geometry(
            1000,
            1000,
            0,
            &TriangularTessellationShapeSettings::default(),
        )
        .expect("triangular geom");
    let g = grid.frame_shape.corner_radius_px;
    let v = voronoi.frame_shape.corner_radius_px;
    let t = tri.frame_shape.corner_radius_px;
    // All three should be within a 2x factor of each other. Grid is the
    // canonical reference at min(piece_w, piece_h) * 0.05 = 5px here.
    for (label, value) in [("voronoi", v), ("triangular", t)] {
        let ratio = (value / g).max(g / value);
        assert!(
            ratio < 2.0,
            "{label} radius {} vs grid radius {} differ by more than 2x",
            value,
            g
        );
    }
}
