use std::num::NonZeroU32;

use heddobureika_game::{
    EdgeId, FlipState, GridTopology, PieceId, PlayRules, PlayableAction, PlayableState, Position2,
    PuzzleTopology, SolveStage,
};

#[test]
fn grid_9x9_solved_shuffled_and_progression_story() {
    let topology = GridTopology::new(
        NonZeroU32::new(9).expect("non-zero"),
        NonZeroU32::new(9).expect("non-zero"),
    );
    assert_eq!(topology.piece_count(), 81);
    assert_eq!(topology.edge_count(), 144);

    // Solved baseline.
    let solved = PlayableState::solved(topology.clone(), PlayRules::default());
    let solved_summary = solved.summary();
    assert_eq!(solved_summary.stage, SolveStage::Solved);
    assert!(solved_summary.solved);
    assert_eq!(solved_summary.logical.group_count, 1);
    assert_eq!(solved_summary.logical.active_edges, 144);

    // Randomly shuffled baseline.
    let mut shuffled = PlayableState::shuffled(
        topology.clone(),
        PlayRules::default(),
        0x9A3E_0D21_55AA_F00D,
    );
    let shuffled_summary = shuffled.summary();
    assert_eq!(shuffled_summary.stage, SolveStage::Shuffled);
    assert!(!shuffled_summary.solved);
    assert_eq!(shuffled_summary.logical.group_count, 81);
    assert_eq!(shuffled_summary.logical.active_edges, 0);
    assert!(has_visible_shuffle(&shuffled));

    // Step 1: normalize each singleton group orientation/flip and place each
    // group at its canonical piece position.
    for id in 0..topology.piece_count() {
        let group = heddobureika_game::GroupId(id);

        if shuffled.flip_of(group) == Some(FlipState::Flipped) {
            let _ = shuffled.apply_action(PlayableAction::UnflipGroup { group, pivot: None });
        }

        // Rotations are shuffled in 90-degree steps, so <=4 clockwise steps
        // always reaches 0.
        for _ in 0..4 {
            let rot = shuffled
                .pose_of(group)
                .map(|pose| pose.rotation_degrees())
                .unwrap_or(0.0);
            if approx_zero(rot) {
                break;
            }
            let _ = shuffled.apply_action(PlayableAction::StepRotateGroupCw { group });
        }

        let piece = PieceId(id);
        let (x, y) = topology
            .canonical_position_mm(piece)
            .expect("piece should be valid for 9x9 grid");
        let _ = shuffled.apply_action(PlayableAction::TranslateGroup {
            group,
            drop_pos: Position2 { x, y },
        });
    }

    let normalized_summary = shuffled.summary();
    assert_eq!(normalized_summary.stage, SolveStage::Shuffled);
    assert!(!normalized_summary.solved);
    assert_eq!(normalized_summary.logical.active_edges, 0);
    assert_eq!(normalized_summary.logical.group_count, 81);

    // Step 2: edge-by-edge joins move the state into in-progress, then solved.
    assert!(shuffled.logical.activate_edge(EdgeId(0)));
    let in_progress = shuffled.summary();
    assert_eq!(in_progress.stage, SolveStage::InProgress);
    assert!(!in_progress.solved);
    assert_eq!(in_progress.logical.active_edges, 1);
    assert_eq!(in_progress.logical.group_count, 80);

    for edge in 1..topology.edge_count() {
        let changed = shuffled.logical.activate_edge(EdgeId(edge));
        assert!(changed);
    }

    let solved_after_steps = shuffled.summary();
    assert_eq!(solved_after_steps.stage, SolveStage::Solved);
    assert!(solved_after_steps.solved);
    assert_eq!(
        solved_after_steps.logical.active_edges,
        topology.edge_count()
    );
    assert_eq!(solved_after_steps.logical.group_count, 1);
}

fn has_visible_shuffle(state: &PlayableState<GridTopology>) -> bool {
    for idx in 0..state.piece_count() {
        let group = heddobureika_game::GroupId(idx as u32);
        if state.flip_of(group) == Some(FlipState::Flipped) {
            return true;
        }

        let Some(pose) = state.pose_of(group) else {
            continue;
        };
        if !approx_zero(pose.x_mm())
            || !approx_zero(pose.y_mm())
            || !approx_zero(pose.rotation_degrees())
        {
            return true;
        }
    }
    false
}

fn approx_zero(value: f32) -> bool {
    value.abs() <= 1.0e-4
}
