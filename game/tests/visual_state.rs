use heddobureika_game::{
    EdgeId, GridTopology, LogicalState, MergePolicy, PlayRules, PlayableAction, PlayableState,
    Pose2, Position2, ProjectionScratch, VisualState,
};

#[test]
fn visual_state_starts_from_authoritative_projection() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let playable = PlayableState::new(logical, PlayRules::default());

    let visual = VisualState::rebuild_from(&playable);

    assert_eq!(
        visual.piece_visual_pose(),
        visual.authoritative_piece_pose()
    );
    assert_eq!(
        visual.piece_target_pose(),
        visual.authoritative_piece_pose()
    );
    assert_approx(visual.authoritative_piece_pose()[1].x_mm(), 1.0);
}

#[test]
fn visual_prediction_does_not_affect_snap_probe() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let mut visual = VisualState::rebuild_from(&playable);
    assert!(visual.set_piece_visual_pose(
        heddobureika_game::PieceId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    ));

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(5.0, 0.0, 0.0).expect("finite"),
    );

    assert!(proposal.activate_edges.is_empty());
    assert_eq!(playable.logical.active_edge_count(), 0);
}

#[test]
fn visual_state_apply_delta_updates_targets_but_preserves_current_visual_pose() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let mut visual = VisualState::rebuild_from(&playable);
    assert!(visual.set_piece_visual_pose(
        heddobureika_game::PieceId(1),
        Pose2::try_from_mm_degrees(20.0, 0.0, 0.0).expect("finite"),
    ));

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(5.0, 0.0).expect("finite"),
        },
        None,
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(
        batch.proposal.status,
        heddobureika_game::ProposalApplyStatus::ActionOnly
    );

    let mut scratch = ProjectionScratch::with_capacity(playable.piece_count());
    visual.apply_delta(&playable, &batch.delta, &mut scratch);

    assert_approx(visual.authoritative_piece_pose()[1].x_mm(), 5.0);
    assert_approx(visual.piece_target_pose()[1].x_mm(), 5.0);
    assert_approx(visual.piece_visual_pose()[1].x_mm(), 20.0);
}

#[test]
fn visual_state_steps_toward_targets_and_can_snap_back() {
    let topology = GridTopology::try_new(1, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let mut visual = VisualState::rebuild_from(&playable);
    assert!(visual.set_piece_visual_pose(
        heddobureika_game::PieceId(0),
        Pose2::try_from_mm_degrees(10.0, 0.0, 350.0).expect("finite"),
    ));
    assert!(visual.set_piece_target_pose(
        heddobureika_game::PieceId(0),
        Pose2::try_from_mm_degrees(0.0, 0.0, 10.0).expect("finite"),
    ));

    visual.step_toward_targets(0.5);

    assert_approx(visual.piece_visual_pose()[0].x_mm(), 5.0);
    assert_approx(visual.piece_visual_pose()[0].rotation_degrees(), 360.0);

    assert!(visual.snap_piece_to_authoritative(heddobureika_game::PieceId(0)));
    assert_eq!(
        visual.piece_visual_pose()[0],
        visual.authoritative_piece_pose()[0]
    );
    assert_eq!(
        visual.piece_target_pose()[0],
        visual.authoritative_piece_pose()[0]
    );
}

#[test]
fn visual_state_resets_when_piece_count_changes() {
    let topology_one = GridTopology::try_new(1, 1).expect("valid grid");
    let topology_two = GridTopology::try_new(2, 1).expect("valid grid");
    let playable_one = PlayableState::new(LogicalState::new(topology_one), PlayRules::default());
    let playable_two = PlayableState::new(LogicalState::new(topology_two), PlayRules::default());
    let mut visual = VisualState::rebuild_from(&playable_one);
    let delta = heddobureika_game::PlayableDelta::for_revision(1);
    let mut scratch = ProjectionScratch::with_capacity(playable_two.piece_count());

    visual.apply_delta(&playable_two, &delta, &mut scratch);

    assert_eq!(visual.piece_visual_pose().len(), 2);
    assert_eq!(
        visual.piece_visual_pose(),
        visual.authoritative_piece_pose()
    );
}

#[test]
fn identity_physical_state_mirrors_authoritative_poses() {
    use heddobureika_game::{
        IdentityPhysicalState, InteractableState, PhysicalProjection, PieceId,
    };

    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[1] = Pose2::try_from_mm_degrees(7.0, 3.0, 0.0).expect("finite");

    let authoritative = InteractableState::rebuild_from(&playable);
    let mut physical = IdentityPhysicalState::from_authoritative(&authoritative);

    assert_eq!(physical.piece_physical_pose(PieceId(1)).x_mm(), 7.0);

    playable.group_pose[1] = Pose2::try_from_mm_degrees(9.5, 3.0, 0.0).expect("finite");
    let refreshed = InteractableState::rebuild_from(&playable);
    physical.refresh_dirty(&refreshed, &[PieceId(1)]);
    physical.step(0.016);
    assert_eq!(physical.piece_physical_pose(PieceId(1)).x_mm(), 9.5);
    assert_eq!(physical.piece_physical_pose(PieceId(0)).x_mm(), 0.0);
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-4,
        "expected {expected}, got {actual}"
    );
}
