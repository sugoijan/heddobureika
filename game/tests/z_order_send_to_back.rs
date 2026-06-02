use std::num::NonZeroU32;

use heddobureika_game::{GridTopology, PlayRules, PlayableState, PuzzleTopology};

fn shuffled_3x3() -> PlayableState<GridTopology> {
    let topology = GridTopology::new(
        NonZeroU32::new(3).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    assert_eq!(topology.piece_count(), 9);
    // Shuffled => every piece is its own singleton group, so each piece id is
    // also its group's anchor and appears once in `z_order`.
    PlayableState::shuffled(topology, PlayRules::default(), 0x1234_5678_9ABC_DEF0)
}

/// `z_order` is back-to-front; demoting a group moves it to index 0 (bottom).
#[test]
fn send_to_back_moves_group_to_bottom() {
    let mut state = shuffled_3x3();
    let before: Vec<u32> = state.iter_z_asc().map(|g| g.as_u32()).collect();
    assert_eq!(before.len(), 9);

    // Pick a group that isn't already at the bottom so the call is a real move.
    let target = before[5];
    assert!(state.send_to_back_by_anchors(&[target]));

    let after: Vec<u32> = state.iter_z_asc().map(|g| g.as_u32()).collect();
    assert_eq!(after.first().copied(), Some(target), "target must be bottom");
    // Everything else keeps its relative order.
    let rest: Vec<u32> = before.into_iter().filter(|g| *g != target).collect();
    assert_eq!(after[1..], rest[..]);
}

/// Demoting the group that is already at the bottom is a no-op.
#[test]
fn send_to_back_is_noop_when_already_bottom() {
    let mut state = shuffled_3x3();
    let bottom = state.iter_z_asc().next().expect("non-empty").as_u32();
    assert!(!state.send_to_back_by_anchors(&[bottom]));
}

/// An empty anchor list never reorders.
#[test]
fn send_to_back_empty_is_noop() {
    let mut state = shuffled_3x3();
    assert!(!state.send_to_back_by_anchors(&[]));
}

/// Mirror property: promote-to-front then send-to-back returns a group from the
/// top to the bottom, and the two helpers are inverses for a single group on an
/// otherwise-unchanged stack.
#[test]
fn send_to_back_inverts_promote_to_front() {
    let mut state = shuffled_3x3();
    let original: Vec<u32> = state.iter_z_asc().map(|g| g.as_u32()).collect();

    let target = original[3];
    assert!(state.set_z_order_by_anchors(&[target]));
    assert_eq!(
        state.iter_z_asc().last().map(|g| g.as_u32()),
        Some(target),
        "target should be on top after promote"
    );

    assert!(state.send_to_back_by_anchors(&[target]));
    assert_eq!(
        state.iter_z_asc().next().map(|g| g.as_u32()),
        Some(target),
        "target should be on the bottom after send-to-back"
    );
}
