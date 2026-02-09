use std::num::NonZeroU32;

use heddobureika_game::{
    AngleDeg, GridTopology, GroupId, LogicalState, PlayRules, PlayableAction, PlayableState,
};

#[test]
fn step_rotate_cw_skips_nearby_lattice_angle() {
    let topology = GridTopology::new(
        NonZeroU32::new(2).expect("non-zero"),
        NonZeroU32::new(1).expect("non-zero"),
    );
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let group = GroupId(0);
    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group,
        drop_rotation: AngleDeg::try_new(88.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCw { group });

    let rotation = playable
        .pose_of(group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(rotation, 180.0);
}

#[test]
fn step_rotate_cw_moves_to_next_lattice_when_not_nearby() {
    let topology = GridTopology::new(
        NonZeroU32::new(2).expect("non-zero"),
        NonZeroU32::new(1).expect("non-zero"),
    );
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let group = GroupId(0);
    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group,
        drop_rotation: AngleDeg::try_new(86.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCw { group });

    let rotation = playable
        .pose_of(group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(rotation, 90.0);
}

#[test]
fn step_rotate_ccw_skips_nearby_lattice_angle() {
    let topology = GridTopology::new(
        NonZeroU32::new(2).expect("non-zero"),
        NonZeroU32::new(1).expect("non-zero"),
    );
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let group = GroupId(0);
    let _ = playable.apply_action(PlayableAction::RotateGroupTo {
        group,
        drop_rotation: AngleDeg::try_new(2.0).expect("finite"),
    });
    let _ = playable.apply_action(PlayableAction::StepRotateGroupCcw { group });

    let rotation = playable
        .pose_of(group)
        .map(|pose| pose.rotation_degrees())
        .unwrap_or(0.0);
    assert_approx(rotation, 270.0);
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-3,
        "expected {expected}, got {actual}"
    );
}
