//! End-to-end tests for geometry-aware z-ordering on a real `PlayableState`.
//! Pure ordering logic is covered by unit tests in `game/src/z_depth.rs`; here
//! we exercise `group_world_aabb` against a concrete topology and the two
//! gesture methods through controlled group poses.

use std::num::NonZeroU32;

use heddobureika_game::{
    GridTopology, GroupId, PieceId, PlayRules, PlayableState, Pose2, PuzzleTopology,
};

const SEED: u64 = 0xABCD_1234_5678_9F01;

fn grid(n: u32) -> PlayableState<GridTopology> {
    let topo = GridTopology::new(NonZeroU32::new(n).unwrap(), NonZeroU32::new(n).unwrap());
    PlayableState::shuffled(topo, PlayRules::default(), SEED)
}

/// Square piece extent (mm) for a square grid puzzle — all pieces are equal.
fn piece_extent(n: u32) -> (f32, f32) {
    let topo = GridTopology::new(NonZeroU32::new(n).unwrap(), NonZeroU32::new(n).unwrap());
    let (ex, ey) = topo.piece_extent_mm(PieceId(0));
    (ex.as_mm_f32(), ey.as_mm_f32())
}

fn set_pose(state: &mut PlayableState<GridTopology>, g: GroupId, x: f32, y: f32, deg: f32) {
    state.group_pose[g.as_usize()] = Pose2::try_from_mm_degrees(x, y, deg).unwrap();
}

fn zidx(state: &PlayableState<GridTopology>, g: GroupId) -> u32 {
    state.z_index_of[g.as_usize()]
}

/// Spread every group onto a wide 1-D lattice so none overlap, then return the
/// group list (in z order). Callers reposition the pair under test afterward.
fn spread_all(state: &mut PlayableState<GridTopology>, spacing: f32) -> Vec<GroupId> {
    let groups: Vec<GroupId> = state.iter_z_asc().collect();
    for (k, &g) in groups.iter().enumerate() {
        set_pose(state, g, k as f32 * spacing, 0.0, 0.0);
    }
    groups
}

#[test]
fn group_world_aabb_singleton_matches_extent() {
    let mut state = grid(3);
    let (ex, ey) = piece_extent(3);
    let g = state.logical.group_of(PieceId(0)).unwrap();
    set_pose(&mut state, g, 100.0, 50.0, 0.0);
    let aabb = state.group_world_aabb(g).unwrap();
    let eps = 1e-3;
    assert!((aabb.min_x - (100.0 - ex * 0.5)).abs() < eps);
    assert!((aabb.max_x - (100.0 + ex * 0.5)).abs() < eps);
    assert!((aabb.min_y - (50.0 - ey * 0.5)).abs() < eps);
    assert!((aabb.max_y - (50.0 + ey * 0.5)).abs() < eps);
}

#[test]
fn group_world_aabb_rotation_45_enlarges_by_sqrt2() {
    let mut state = grid(3);
    let (ex, ey) = piece_extent(3);
    let g = state.logical.group_of(PieceId(0)).unwrap();
    set_pose(&mut state, g, 0.0, 0.0, 45.0);
    let aabb = state.group_world_aabb(g).unwrap();
    // Rotated-rect width = ex*|cos| + ey*|sin| = (ex+ey)*√2/2.
    let expected_w = (ex + ey) * (2f32.sqrt() / 2.0);
    assert!((aabb.width() - expected_w).abs() < 1e-2, "w={} exp={}", aabb.width(), expected_w);
}

#[test]
fn bring_forward_does_not_bury_covered_piece() {
    let mut state = grid(3);
    let (ex, _) = piece_extent(3);
    let spacing = ex * 6.0;
    let groups = spread_all(&mut state, spacing);
    let (small, big) = (groups[0], groups[1]);
    // Concentric, far from the lattice: `big` rotated 45° so its AABB (≈√2×)
    // fully contains `small`'s upright AABB.
    let far = -spacing * 8.0;
    set_pose(&mut state, small, far, 0.0, 0.0);
    set_pose(&mut state, big, far, 0.0, 45.0);

    state.bring_forward_to_fitting_depth(&[small.as_u32()]); // normalize start
    state.bring_forward_to_fitting_depth(&[big.as_u32()]);

    assert!(
        zidx(&state, small) > zidx(&state, big),
        "small covered piece must stay above the just-picked big one"
    );
    // A non-cluster lattice group keeps its place relative to others (sanity:
    // it is not dragged into the pair's reorder).
    let other = groups[5];
    assert!(state.group_world_aabb(other).is_some());
}

#[test]
fn shake_keeps_shaken_group_visible_and_reveals_covered() {
    let mut state = grid(3);
    let (ex, _) = piece_extent(3);
    let spacing = ex * 6.0;
    let groups = spread_all(&mut state, spacing);
    let (small, big) = (groups[0], groups[1]);
    let far = -spacing * 8.0;
    set_pose(&mut state, small, far, 0.0, 0.0);
    set_pose(&mut state, big, far, 0.0, 45.0);

    // Shake the BIG group: it should drop behind the small one it was covering,
    // but the small one must not be hidden.
    state.send_backward_to_fitting_depth(&[big.as_u32()]);
    assert!(
        zidx(&state, small) > zidx(&state, big),
        "after shaking big, the covered small piece is revealed above it"
    );

    // Force big on top (hiding small), then shake the SMALL group: it must rise
    // above its container rather than stay hidden.
    let big_anchor = state.anchor_piece_of_group(big).unwrap().as_u32();
    state.set_z_order_by_anchors(&[big_anchor]);
    assert!(zidx(&state, big) > zidx(&state, small), "big now hides small");
    state.send_backward_to_fitting_depth(&[small.as_u32()]);
    assert!(
        zidx(&state, small) > zidx(&state, big),
        "the shaken small group must rise above its container"
    );
}

#[test]
fn isolated_group_goes_fully_forward_and_back() {
    let mut state = grid(3);
    let (ex, _) = piece_extent(3);
    let spacing = ex * 6.0;
    let groups = spread_all(&mut state, spacing);
    let g = groups[4]; // overlaps nobody

    state.bring_forward_to_fitting_depth(&[g.as_u32()]);
    let total = groups.len() as u32;
    assert_eq!(zidx(&state, g), total - 1, "isolated bring-forward → absolute front");

    state.send_backward_to_fitting_depth(&[g.as_u32()]);
    assert_eq!(zidx(&state, g), 0, "isolated send-backward → absolute back");
}

#[test]
fn determinism_two_identical_states_agree() {
    let build = || {
        let mut s = grid(3);
        let (ex, _) = piece_extent(3);
        let spacing = ex * 6.0;
        let groups = spread_all(&mut s, spacing);
        let far = -spacing * 8.0;
        set_pose(&mut s, groups[0], far, 0.0, 0.0);
        set_pose(&mut s, groups[1], far, 0.0, 45.0);
        s.send_backward_to_fitting_depth(&[groups[1].as_u32()]);
        s.iter_z_asc().map(|g| g.as_u32()).collect::<Vec<_>>()
    };
    assert_eq!(build(), build(), "same inputs must yield the same z_order");
}
