//! Realistic regression test for click-pivot unflip, exercising the same
//! casting path the UI uses (`hit_test_local_coords`, via
//! `piece_contains_world_point`) against the real piece outline.
//!
//! Property: any world point that hits a flipped piece must STILL hit that
//! piece after the piece is unflipped using that point as the flip pivot.
//! This is what makes "click a flipped piece to unflip, then immediately
//! grab and drag it" work — the point under the cursor stays on the piece.
//!
//! Geometrically this holds because a flip is a horizontal mirror; pivoting
//! it about the vertical line through the click point leaves that point
//! fixed, and a shape's slice along its own mirror axis maps to itself.
//! We assert it through the actual hit test (rotation + aspect included),
//! and re-test each piece with many points by restoring state each time.

use heddobureika_game::{
    build_topology_from_spec, FlipState, GroupId, LogicalState, PieceId, PlayRules, PlayableAction,
    PlayableState, Pose2, Position2, PuzzleRenderGeometry, PuzzleTopology,
    TriangularTessellationTopology,
};

// Non-square on purpose so pose-unit x and y differ (aspect != 1).
const IMAGE_W: u32 = 600;
const IMAGE_H: u32 = 400;
// Rotations to exercise, including a non-cardinal angle.
const ROTATIONS: [f32; 3] = [0.0, 90.0, 37.0];
// Cap points tested per (piece, rotation) so the sweep stays fast.
const MAX_POINTS_PER_CASE: usize = 12;

fn px_to_pose(geom: &PuzzleRenderGeometry, x: f32, y: f32) -> Option<Position2> {
    let [ox, oy] = geom.pose_origin_px;
    let [ux, uy] = geom.pose_unit_px;
    if ux <= 0.0 || uy <= 0.0 {
        return None;
    }
    Position2::try_from_mm((x - ox) / ux, (y - oy) / uy)
}

/// World-pixel points that hit the given (flipped) piece at `pose`, found by
/// sweeping a grid around the piece's rendered location.
fn hitting_points(
    geom: &PuzzleRenderGeometry,
    piece: PieceId,
    pose: Pose2,
    flipped: bool,
) -> Vec<(f32, f32)> {
    let Some(geo_piece) = geom.piece(piece) else {
        return Vec::new();
    };
    let Some(top_left) = geom.pose_to_piece_top_left(piece, pose) else {
        return Vec::new();
    };
    // Anchor world position; the rendered (possibly rotated) piece sits
    // within ~one bbox diagonal of it.
    let anchor = (
        top_left.0 + geo_piece.pose_anchor_px[0],
        top_left.1 + geo_piece.pose_anchor_px[1],
    );
    let reach = geo_piece.bounds_px.width + geo_piece.bounds_px.height;
    let step = (reach / 24.0).max(2.0);
    // Only keep points that are robustly interior (a ~1.5px margin in all
    // four directions). The unflip puts the *same* material point back under
    // the cursor, but `point_in_polygon` is fragile for points that sit
    // exactly on an edge / the reflection axis at a triangle tip — sampling
    // strictly-interior points keeps the assertion about geometry, not
    // floating-point edge coincidences.
    let m = 1.5_f32;
    let inside = |x: f32, y: f32| {
        geom.piece_contains_world_point(piece, (x, y), top_left, pose.rotation_degrees(), flipped)
    };
    let mut hits = Vec::new();
    let mut y = anchor.1 - reach;
    while y <= anchor.1 + reach {
        let mut x = anchor.0 - reach;
        while x <= anchor.0 + reach {
            if inside(x, y)
                && inside(x + m, y)
                && inside(x - m, y)
                && inside(x, y + m)
                && inside(x, y - m)
            {
                hits.push((x, y));
            }
            x += step;
        }
        y += step;
    }
    hits
}

#[test]
fn unflipping_about_click_point_keeps_that_point_on_the_piece() {
    let spec = TriangularTessellationTopology::new_spec(5, 3);
    let topology = build_topology_from_spec(&spec).expect("triangular topology");
    let geom = topology
        .build_render_geometry(IMAGE_W, IMAGE_H, 0, &())
        .expect("render geometry");

    let mut base = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    if geom.pose_unit_px[0] > 0.0 && geom.pose_unit_px[1] > 0.0 {
        base.set_piece_aspect_ratio(geom.pose_unit_px[1] / geom.pose_unit_px[0]);
    }

    let total = base.piece_count() as u32;
    // Park each piece at a fixed, in-frame-ish pose so its render location is
    // well-defined; the exact spot doesn't matter to the property.
    let park = (2.5_f32, 3.0_f32);

    let mut total_points_checked = 0usize;
    for id in 0..total {
        let piece = PieceId(id);
        let group = GroupId(id);
        for &theta in ROTATIONS.iter() {
            let pose = Pose2::try_from_mm_degrees(park.0, park.1, theta).expect("pose");

            // Flipped starting state for this case.
            let mut flipped_state = base.clone();
            flipped_state.group_pose[group.as_usize()] = pose;
            flipped_state.group_flip[group.as_usize()] = FlipState::Flipped;

            let hits = hitting_points(&geom, piece, pose, true);
            assert!(
                !hits.is_empty(),
                "no hitting points found for piece {id} at rotation {theta}"
            );

            let stride = (hits.len() / MAX_POINTS_PER_CASE).max(1);
            for world in hits.iter().step_by(stride) {
                let pivot = px_to_pose(&geom, world.0, world.1).expect("pivot in pose units");

                // Restore the flipped state, then unflip about this click point.
                let mut state = flipped_state.clone();
                let _ = state.apply_action(PlayableAction::UnflipGroup {
                    group,
                    pivot: Some(pivot),
                });
                assert_eq!(state.flip_of(group), Some(FlipState::Normal));

                let new_pose = state.pose_of(group).expect("pose after unflip");
                let new_top_left = geom
                    .pose_to_piece_top_left(piece, new_pose)
                    .expect("top left after unflip");
                assert!(
                    geom.piece_contains_world_point(
                        piece,
                        *world,
                        new_top_left,
                        new_pose.rotation_degrees(),
                        false,
                    ),
                    "piece {id} @ rot {theta}: click point {world:?} fell off the piece after unflip \
                     (pose {pose:?} -> {new_pose:?})"
                );
                total_points_checked += 1;
            }
        }
    }
    assert!(
        total_points_checked > 100,
        "expected a broad sweep; only checked {total_points_checked} points"
    );
}

#[test]
fn unflip_without_pivot_leaves_pose_unchanged() {
    // Non-interactive unflip (no pivot) must not move the group — preserves
    // the historical reflect-about-anchor behaviour.
    let spec = TriangularTessellationTopology::new_spec(3, 2);
    let topology = build_topology_from_spec(&spec).expect("triangular topology");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let group = GroupId(6);
    playable.group_pose[group.as_usize()] =
        Pose2::try_from_mm_degrees(4.0, 5.0, 30.0).expect("pose");
    playable.group_flip[group.as_usize()] = FlipState::Flipped;
    let before = playable.pose_of(group).expect("pose");

    let _ = playable.apply_action(PlayableAction::UnflipGroup { group, pivot: None });
    let after = playable.pose_of(group).expect("pose");
    assert_eq!(before, after, "pivot-less unflip must not move the group");
}

#[test]
fn flipping_a_multi_piece_group_is_a_no_op() {
    // Only individual pieces may be flipped; a joined group must never enter
    // the flipped state.
    use heddobureika_game::{MergePolicy, RestrictedPlayableAction};

    let topology = build_topology_from_spec(&heddobureika_game::TopologySpec::grid(2, 1))
        .expect("grid topology");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    // Join the two pieces into one group by snapping piece 1 onto piece 0's
    // canonical neighbour pose.
    playable.group_pose[1] = Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("pose");
    let proposal = playable.probe_snaps(
        GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).unwrap(),
    );
    let _ = playable.apply_proposal(&proposal, MergePolicy::KeepFixedGroup);
    let group = playable.logical.group_of(PieceId(0)).expect("group");
    assert!(
        playable.logical.members_of(group).take(2).count() == 2,
        "precondition: pieces joined into a multi-piece group"
    );

    let _ = playable
        .apply_restricted_action(RestrictedPlayableAction::FlipGroup { group, pivot: None });
    assert_eq!(
        playable.flip_of(group),
        Some(FlipState::Normal),
        "a multi-piece group must not become flipped"
    );
}
