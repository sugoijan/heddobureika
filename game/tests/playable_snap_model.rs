use heddobureika_game::{
    ActionId, EdgeId, FlipState, GridTopology, InteractableState, LogicalState, MergePolicy,
    PlayRules, PlayableAction, PlayableState, Pose2, Position2, ProjectionScratch,
    ProposalApplyRejection, ProposalApplyStatus, RestrictedPlayableAction, SnapRejectionReason,
};

#[test]
fn logical_deactivation_rebuilds_groups_and_validates() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);

    assert!(logical.activate_edge(EdgeId(0)));
    assert!(logical.activate_edge(EdgeId(1)));
    assert_eq!(logical.group_count(), 1);
    logical.validate().expect("joined state should be valid");

    assert!(logical.deactivate_edge(EdgeId(0)));
    logical.validate().expect("split state should be valid");
    assert_eq!(logical.group_count(), 2);
    assert_ne!(
        logical.group_of(heddobureika_game::PieceId(0)),
        logical.group_of(heddobureika_game::PieceId(1))
    );
    assert_eq!(
        logical.group_of(heddobureika_game::PieceId(1)),
        logical.group_of(heddobureika_game::PieceId(2))
    );
}

#[test]
fn projection_uses_piece_local_pose_within_joined_group() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());

    playable.group_pose[0] = Pose2::try_from_mm_degrees(10.0, 20.0, 0.0).expect("finite");
    let interactable = InteractableState::rebuild_from(&playable);
    let poses = interactable.piece_world_pose();

    assert_approx(poses[0].x_mm(), 10.0);
    assert_approx(poses[0].y_mm(), 20.0);
    assert_approx(poses[1].x_mm(), 11.0);
    assert_approx(poses[1].y_mm(), 20.0);
}

#[test]
fn snap_probe_returns_best_non_mutating_grid_join() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let proposed = Pose2::try_from_mm_degrees(1.05, 0.0, 0.0).expect("finite");

    let proposal = playable.probe_snaps(heddobureika_game::GroupId(1), proposed);

    assert_eq!(proposal.base_revision, 0);
    assert_eq!(proposal.activate_edges, vec![EdgeId(0)]);
    assert_eq!(proposal.fixed_group, Some(heddobureika_game::GroupId(0)));
    assert_approx(proposal.desired_pose.x_mm(), 1.0);
    assert_eq!(playable.logical.active_edge_count(), 0);
}

#[test]
fn snap_probe_ranks_best_candidate_by_distance() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[2] = Pose2::try_from_mm_degrees(2.15, 0.0, 0.0).expect("finite");

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.05, 0.0, 0.0).expect("finite"),
    );

    assert_eq!(proposal.candidates.len(), 2);
    assert_eq!(proposal.activate_edges, vec![EdgeId(0)]);
    assert!(proposal.candidates[0].distance_mm < proposal.candidates[1].distance_mm);
}

#[test]
fn snap_probe_tie_breaks_by_edge_and_piece_ids() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[2] = Pose2::try_from_mm_degrees(2.0, 0.0, 0.0).expect("finite");

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );

    assert_eq!(proposal.candidates.len(), 2);
    assert_eq!(proposal.candidates[0].site.edge, EdgeId(0));
    assert_eq!(proposal.candidates[1].site.edge, EdgeId(1));
    assert_eq!(proposal.activate_edges, vec![EdgeId(0)]);
}

#[test]
fn snap_probe_applies_rotation_tolerance() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let near = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 4.0).expect("finite"),
    );
    assert_eq!(near.activate_edges, vec![EdgeId(0)]);

    let far = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 8.0).expect("finite"),
    );
    assert!(far.activate_edges.is_empty());
    assert!(far
        .rejections
        .iter()
        .any(|(_, reason)| *reason == SnapRejectionReason::RotationMismatch));
}

#[test]
fn snap_probe_ignores_rotation_mismatch_when_rotation_disabled() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut rules = PlayRules::default();
    rules.rotation_enabled = false;
    let mut playable = PlayableState::new(LogicalState::new(topology), rules);
    playable.group_pose[0] = Pose2::try_from_mm_degrees(0.0, 0.0, 90.0).expect("finite");

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );

    assert_eq!(proposal.activate_edges, vec![EdgeId(0)]);
    assert!(!proposal
        .rejections
        .iter()
        .any(|(_, reason)| *reason == SnapRejectionReason::RotationMismatch));
}

#[test]
fn snap_probe_rejects_flipped_fixed_group() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_flip[0] = FlipState::Flipped;

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );

    assert!(proposal.activate_edges.is_empty());
    assert!(proposal
        .rejections
        .iter()
        .any(|(_, reason)| *reason == SnapRejectionReason::FlippedGroup));
}

#[test]
fn applying_snap_proposal_preserves_fixed_group_pose_and_cascades_delta() {
    // After joining all 2x1 pieces into a single group, the identity
    // frame-snap fires and pulls the group anchor to `(0.5, 0.5)` — the
    // workspace top-left target in piece-center pose units.
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let proposed = Pose2::try_from_mm_degrees(1.05, 0.0, 0.0).expect("finite");
    let proposal = playable.probe_snaps(heddobureika_game::GroupId(1), proposed);

    let delta = playable.apply_proposal(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(delta.revision, 1);
    assert!(delta.membership_changed);
    assert!(delta.dirty_edges.contains(&EdgeId(0)));
    assert_eq!(playable.logical.group_count(), 1);
    assert_eq!(
        playable.logical.group_of(heddobureika_game::PieceId(1)),
        Some(heddobureika_game::GroupId(0))
    );
    let fixed_pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("fixed group should exist");
    assert_approx(fixed_pose.x_mm(), 0.5);
    assert_approx(fixed_pose.y_mm(), 0.5);
}

#[test]
fn applying_multi_piece_mover_preserves_higher_id_fixed_group_world_pose() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());
    playable.group_pose[2] = Pose2::try_from_mm_degrees(2.0, 0.0, 0.0).expect("finite");

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(0),
        Pose2::try_from_mm_degrees(0.05, 0.0, 0.0).expect("finite"),
    );
    assert_eq!(proposal.activate_edges, vec![EdgeId(1)]);
    assert_eq!(proposal.fixed_group, Some(heddobureika_game::GroupId(2)));

    let batch = playable.apply_proposal_with_batch(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(playable.logical.group_count(), 1);
    assert_eq!(
        playable.logical.group_of(heddobureika_game::PieceId(0)),
        Some(heddobureika_game::GroupId(2))
    );
    // Complete 3x1 group identity-snaps to anchor pose `(0.5, 0.5)`, so
    // pieces 0/1/2 sit at x = 0.5/1.5/2.5 respectively.
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(0))
            .expect("piece 0 pose")
            .x_mm(),
        0.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(1))
            .expect("piece 1 pose")
            .x_mm(),
        1.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(2))
            .expect("piece 2 pose")
            .x_mm(),
        2.5,
    );
    playable
        .validate()
        .expect("multi-piece fixed merge should preserve invariants");
}

#[test]
fn applying_singleton_mover_to_multi_piece_fixed_group_preserves_fixed_pose() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(2),
        Pose2::try_from_mm_degrees(2.05, 0.0, 0.0).expect("finite"),
    );
    assert_eq!(proposal.activate_edges, vec![EdgeId(1)]);
    assert_eq!(proposal.fixed_group, Some(heddobureika_game::GroupId(0)));

    let batch = playable.apply_proposal_with_batch(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(playable.logical.group_count(), 1);
    assert_eq!(
        playable.logical.group_of(heddobureika_game::PieceId(2)),
        Some(heddobureika_game::GroupId(0))
    );
    // Complete 3x1 group identity-snaps to anchor pose `(0.5, 0.5)`.
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(0))
            .expect("piece 0 pose")
            .x_mm(),
        0.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(2))
            .expect("piece 2 pose")
            .x_mm(),
        2.5,
    );
    playable
        .validate()
        .expect("singleton to multi-piece fixed merge should preserve invariants");
}

#[test]
fn proposal_batch_reports_accepted_metadata_and_updates() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let mut proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );
    proposal.action_id = Some(ActionId(42));

    let batch = playable.apply_proposal_with_batch(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(batch.delta.revision, 1);
    assert_eq!(batch.proposal.action_id, Some(ActionId(42)));
    assert_eq!(batch.proposal.base_revision, 0);
    assert_eq!(batch.proposal.applied_revision, 1);
    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(batch.proposal.rejection, None);
    assert!(!batch.proposal.rebased);
    assert_eq!(batch.proposal.activated_edges, vec![EdgeId(0)]);
    assert_eq!(batch.proposal.merged_groups.len(), 1);
    assert_eq!(
        batch.proposal.merged_groups[0].keep,
        heddobureika_game::GroupId(0)
    );
    assert_eq!(
        batch.proposal.merged_groups[0].absorbed,
        heddobureika_game::GroupId(1)
    );
    assert!(batch
        .proposal
        .final_group_poses
        .iter()
        .any(|update| update.group == heddobureika_game::GroupId(0)));
}

#[test]
fn proposal_batch_reports_rejection_without_advancing_revision() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(5.0, 0.0, 0.0).expect("finite"),
    );

    let batch = playable.apply_proposal_with_batch(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 0);
    assert_eq!(playable.revision, 0);
    assert_eq!(batch.proposal.status, ProposalApplyStatus::Rejected);
    assert!(matches!(
        batch.proposal.rejection,
        Some(ProposalApplyRejection::CandidateRejected(
            SnapRejectionReason::OutsideSnapDistance
        ))
    ));
    assert_eq!(playable.logical.active_edge_count(), 0);
}

#[test]
fn proposal_batch_reports_noop_when_no_join_site_exists() {
    let topology = GridTopology::try_new(1, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(0),
        Pose2::try_from_mm_degrees(3.0, 4.0, 0.0).expect("finite"),
    );

    let batch = playable.apply_proposal_with_batch(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 0);
    assert_eq!(playable.revision, 0);
    assert_eq!(batch.proposal.status, ProposalApplyStatus::Noop);
    assert_eq!(
        batch.proposal.rejection,
        Some(ProposalApplyRejection::NoCandidate)
    );
    assert_eq!(
        playable.pose_of(heddobureika_game::GroupId(0)),
        Some(Pose2::default())
    );
}

#[test]
fn snapped_action_translate_joins_and_emits_batch() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.05, 0.0).expect("finite"),
        },
        Some(ActionId(7)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(batch.proposal.action_id, Some(ActionId(7)));
    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(batch.proposal.activated_edges, vec![EdgeId(0)]);
    assert_eq!(playable.logical.group_count(), 1);
    assert_eq!(playable.logical.active_edge_count(), 1);
}

#[test]
fn snapped_action_transform_sets_pose_and_joins() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.05, 0.0).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::try_new(4.0).expect("finite"),
        },
        Some(ActionId(8)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(playable.logical.active_edge_count(), 1);
    // After join, complete 2x1 group identity-snaps to `(0.5, 0.5)`.
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("merged group pose should exist");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
    assert_approx(pose.rotation_degrees(), 0.0);
}

#[test]
fn snapped_action_without_candidate_applies_action_only() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(5.0, -2.0).expect("finite"),
        },
        Some(ActionId(9)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    assert_eq!(batch.proposal.rejection, None);
    assert!(batch.proposal.activated_edges.is_empty());
    assert_eq!(playable.logical.active_edge_count(), 0);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(1))
        .expect("group pose should exist");
    assert_approx(pose.x_mm(), 5.0);
    assert_approx(pose.y_mm(), -2.0);
}

#[test]
fn transform_action_frame_snaps_single_corner_piece() {
    // Mirrors what `drag_end` does for a singleton corner piece dropped
    // near the workspace top-left: pose `(0.5, 0.5)` is the workspace
    // top-left target (piece-center convention in piece-count units).
    // Frame snap should pull it to exactly `(0.5, 0.5, 0°)`.
    let topology = GridTopology::try_new(5, 5).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.58, 0.53).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(101)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("group pose");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
    assert_approx(pose.rotation_degrees(), 0.0);
}

#[test]
fn transform_action_frame_snaps_single_edge_piece() {
    // Singleton top-edge piece (col=2, row=0 in a 5x5 grid) dropped near
    // the top edge (y ≈ 0.5 in piece-center pose units): only the Y axis
    // should snap (to 0.5), X is preserved.
    let topology = GridTopology::try_new(5, 5).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(2),
            drop_pos: Position2::try_from_mm(2.85, 0.58).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(102)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(2))
        .expect("group pose");
    assert_approx(pose.x_mm(), 2.85);
    assert_approx(pose.y_mm(), 0.5);
}

#[test]
fn snapped_action_frame_snaps_incomplete_corner_group() {
    // Top-left corner piece (group 0) dropped near workspace TL pose
    // `(0.5, 0.5)`. Frame snap pulls it exactly there.
    let topology = GridTopology::try_new(2, 2).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.58, 0.53).expect("finite"),
        },
        Some(ActionId(10)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    assert_eq!(playable.logical.active_edge_count(), 0);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("group pose should exist");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
}

#[test]
fn snapped_action_frame_snaps_incomplete_edge_group() {
    // Top-edge piece group dropped near the top edge (y ≈ 0.5). Only y
    // axis snaps to 0.5; x is preserved.
    let topology = GridTopology::try_new(3, 3).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.85, 0.58).expect("finite"),
        },
        Some(ActionId(11)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    assert_eq!(playable.logical.active_edge_count(), 0);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(1))
        .expect("group pose should exist");
    assert_approx(pose.x_mm(), 1.85);
    assert_approx(pose.y_mm(), 0.5);
}

#[test]
fn snapped_action_frame_snaps_border_group_to_identity() {
    // Border ring drop near workspace TL pose `(0.5, 0.5)` triggers
    // identity frame snap. Corner piece 8 (col=2, row=2) lands at
    // `(2.5, 2.5)` — the canonical solved pose-center for that cell.
    let topology = GridTopology::try_new(3, 3).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    activate_3x3_border_ring(&mut logical);
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.57, 0.54).expect("finite"),
        },
        Some(ActionId(12)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    assert_eq!(playable.logical.group_count(), 2);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("border group pose should exist");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(8))
            .expect("piece 8 pose")
            .x_mm(),
        2.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(8))
            .expect("piece 8 pose")
            .y_mm(),
        2.5,
    );
}

#[test]
fn final_join_complete_snaps_offset_grid_to_identity() {
    // Even with the pre-existing group offset far from workspace TL, the
    // final-join identity snap (forced for complete groups within
    // `2*snap_distance`) pulls anchor to `(0.5, 0.5)`.
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[0] = Pose2::try_from_mm_degrees(10.0, 3.0, 0.0).expect("finite");

    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(11.05, 3.0).expect("finite"),
        },
        Some(ActionId(13)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert!(batch.delta.solved_changed);
    assert!(playable.is_solved());
    assert_eq!(playable.logical.active_edge_count(), 1);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("solved group pose should exist");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
    assert_approx(pose.rotation_degrees(), 0.0);
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(1))
            .expect("piece pose")
            .x_mm(),
        1.5,
    );
}

#[test]
fn solved_state_survives_completed_group_transform() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::solved(topology, PlayRules::default());

    assert!(playable.is_solved());
    assert_eq!(
        playable.summary().stage,
        heddobureika_game::SolveStage::Solved
    );

    let delta = playable.apply_action(PlayableAction::TransformGroupTo {
        group: heddobureika_game::GroupId(0),
        drop_pos: Position2::try_from_mm(10.0, 4.0).expect("finite"),
        drop_rotation: heddobureika_game::AngleDeg::try_new(90.0).expect("finite"),
    });

    assert!(!delta.solved_changed);
    assert!(playable.is_solved());
    assert_eq!(
        playable.summary().stage,
        heddobureika_game::SolveStage::Solved
    );
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("solved group pose should exist");
    assert_approx(pose.x_mm(), 10.0);
    assert_approx(pose.y_mm(), 4.0);
    assert_approx(pose.rotation_degrees(), 90.0);
}

#[test]
fn legacy_apply_action_still_does_not_snap() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let delta = playable.apply_action(PlayableAction::TranslateGroup {
        group: heddobureika_game::GroupId(1),
        drop_pos: Position2::try_from_mm(1.0, 0.0).expect("finite"),
    });

    assert_eq!(delta.revision, 1);
    assert_eq!(playable.logical.active_edge_count(), 0);
    assert_eq!(playable.logical.group_count(), 2);
}

#[test]
fn applying_snap_proposal_cascades_aligned_neighbors() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[2] = Pose2::try_from_mm_degrees(2.0, 0.0, 0.0).expect("finite");
    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );

    let delta = playable.apply_proposal(&proposal, MergePolicy::KeepFixedGroup);

    assert_eq!(playable.logical.active_edge_count(), 2);
    assert_eq!(playable.logical.group_count(), 1);
    assert!(delta.dirty_edges.contains(&EdgeId(0)));
    assert!(delta.dirty_edges.contains(&EdgeId(1)));
    playable
        .validate()
        .expect("cascade should leave playable state valid");
}

#[test]
fn applying_stale_proposal_rebases_against_current_connectivity() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_pose[1] = Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite");
    playable.group_pose[2] = Pose2::try_from_mm_degrees(2.0, 0.0, 0.0).expect("finite");
    let stale = playable.probe_snaps(
        heddobureika_game::GroupId(2),
        Pose2::try_from_mm_degrees(2.0, 0.0, 0.0).expect("finite"),
    );
    assert_eq!(stale.fixed_group, Some(heddobureika_game::GroupId(1)));

    let _ = playable.apply_restricted_action(RestrictedPlayableAction::FlipGroup {
        group: heddobureika_game::GroupId(2),
    });
    let first = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );
    let _ = playable.apply_proposal(&first, MergePolicy::KeepFixedGroup);
    let _ = playable.apply_action(heddobureika_game::PlayableAction::UnflipGroup {
        group: heddobureika_game::GroupId(2),
    });

    let batch = playable.apply_proposal_with_batch(&stale, MergePolicy::KeepFixedGroup);
    let delta = batch.delta;

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Rebased);
    assert!(batch.proposal.rebased);
    assert_eq!(batch.proposal.base_revision, stale.base_revision);
    assert!(delta.dirty_edges.contains(&EdgeId(1)));
    assert_eq!(playable.logical.active_edge_count(), 2);
    assert_eq!(playable.logical.group_count(), 1);
    playable
        .validate()
        .expect("rebased proposal should leave playable state valid");
}

#[test]
fn stale_proposal_rejected_when_candidate_disappears_after_rebase() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let stale = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );
    assert_eq!(stale.activate_edges, vec![EdgeId(0)]);

    let _ = playable.apply_action(PlayableAction::TranslateGroup {
        group: heddobureika_game::GroupId(0),
        drop_pos: Position2::try_from_mm(20.0, 0.0).expect("finite"),
    });

    let batch = playable.apply_proposal_with_batch(&stale, MergePolicy::KeepFixedGroup);

    assert_eq!(batch.revision_before, 1);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(batch.proposal.status, ProposalApplyStatus::Rejected);
    assert!(batch.proposal.rebased);
    assert!(matches!(
        batch.proposal.rejection,
        Some(ProposalApplyRejection::CandidateRejected(
            SnapRejectionReason::OutsideSnapDistance
        ))
    ));
    assert_eq!(playable.logical.active_edge_count(), 0);
}

#[test]
fn snap_probe_rejects_flipped_groups() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    playable.group_flip[1] = heddobureika_game::FlipState::Flipped;
    let proposed = Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite");

    let proposal = playable.probe_snaps(heddobureika_game::GroupId(1), proposed);

    assert!(proposal.activate_edges.is_empty());
    assert!(proposal
        .rejections
        .iter()
        .any(|(_, reason)| *reason == SnapRejectionReason::FlippedGroup));
}

#[test]
fn delta_projection_refreshes_local_piece_pose_after_merge() {
    // After the complete-group snap, identity frame snap places piece 0 at
    // pose-center `(0.5, ...)` and piece 1 at `(1.5, ...)`.
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let mut visual = InteractableState::rebuild_from(&playable);
    let proposal = playable.probe_snaps(
        heddobureika_game::GroupId(1),
        Pose2::try_from_mm_degrees(1.0, 0.0, 0.0).expect("finite"),
    );
    let delta = playable.apply_proposal(&proposal, MergePolicy::KeepFixedGroup);
    let mut scratch = ProjectionScratch::with_capacity(playable.piece_count());

    visual.apply_delta(&playable, &delta, &mut scratch);

    assert_approx(visual.piece_world_pose()[0].x_mm(), 0.5);
    assert_approx(visual.piece_world_pose()[1].x_mm(), 1.5);
}

#[test]
fn restricted_detach_deactivates_incident_edges_and_preserves_invariants() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());

    let delta = playable.apply_restricted_action(RestrictedPlayableAction::DetachPieceAsGroup {
        piece: heddobureika_game::PieceId(1),
        target_pose: Pose2::try_from_mm_degrees(4.0, 5.0, 90.0).expect("finite"),
        target_flip: heddobureika_game::FlipState::Normal,
    });

    assert!(delta.membership_changed);
    assert!(delta.dirty_edges.contains(&EdgeId(0)));
    assert_eq!(playable.logical.is_edge_active(EdgeId(0)), Some(false));
    assert_eq!(playable.logical.group_count(), 2);
    playable
        .validate()
        .expect("detach should leave playable state valid");
}

#[test]
fn restricted_detach_reanchors_remaining_group_when_first_piece_splits_off() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::solved(topology, PlayRules::default());

    let delta = playable.apply_restricted_action(RestrictedPlayableAction::DetachPieceAsGroup {
        piece: heddobureika_game::PieceId(0),
        target_pose: Pose2::try_from_mm_degrees(10.0, 4.0, 0.0).expect("finite"),
        target_flip: heddobureika_game::FlipState::Normal,
    });

    assert!(delta.membership_changed);
    assert!(delta.dirty_edges.contains(&EdgeId(0)));
    assert_eq!(playable.logical.group_count(), 2);
    assert_eq!(
        playable.logical.group_of(heddobureika_game::PieceId(1)),
        Some(heddobureika_game::GroupId(1))
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(0))
            .expect("piece 0 pose")
            .x_mm(),
        10.0,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(1))
            .expect("piece 1 pose")
            .x_mm(),
        1.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(2))
            .expect("piece 2 pose")
            .x_mm(),
        2.5,
    );
}

#[test]
fn restricted_detach_reanchors_all_components_when_middle_piece_splits_group() {
    let topology = GridTopology::try_new(3, 1).expect("valid grid");
    let mut playable = PlayableState::solved(topology, PlayRules::default());

    let delta = playable.apply_restricted_action(RestrictedPlayableAction::DetachPieceAsGroup {
        piece: heddobureika_game::PieceId(1),
        target_pose: Pose2::try_from_mm_degrees(10.0, 4.0, 0.0).expect("finite"),
        target_flip: heddobureika_game::FlipState::Normal,
    });

    assert!(delta.membership_changed);
    assert!(delta.dirty_edges.contains(&EdgeId(0)));
    assert!(delta.dirty_edges.contains(&EdgeId(1)));
    assert_eq!(playable.logical.group_count(), 3);
    assert_eq!(
        playable.logical.group_of(heddobureika_game::PieceId(2)),
        Some(heddobureika_game::GroupId(2))
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(0))
            .expect("piece 0 pose")
            .x_mm(),
        0.5,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(1))
            .expect("piece 1 pose")
            .x_mm(),
        10.0,
    );
    assert_approx(
        playable
            .piece_world_pose(heddobureika_game::PieceId(2))
            .expect("piece 2 pose")
            .x_mm(),
        2.5,
    );
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-4,
        "expected {expected}, got {actual}"
    );
}

fn activate_3x3_border_ring(logical: &mut LogicalState<GridTopology>) {
    for edge in [0, 1, 4, 5, 6, 8, 9, 11] {
        assert!(logical.activate_edge(EdgeId(edge)));
    }
}

/// With non-square pieces (W != H), a 90°-rotated group must place adjacent
/// pieces so their edges align in pixel space — i.e. the canonical offset is
/// rotated isotropically in pixels, not anisotropically in piece-count units.
///
/// For a 2x1 grid with `aspect = piece_height / piece_width`, the second
/// piece's canonical (1, 0) offset under a 90° rotation should land at
/// `(0, 1/aspect)` in piece-count mm: that converts to `(0, piece_width_px)`
/// in pixels, matching the rotated piece's pixel height of `piece_width_px`.
#[test]
fn rotated_group_pose_is_aspect_aware() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());
    // Aspect for piece_width_px = 300, piece_height_px = 175.
    let aspect = 175.0_f32 / 300.0_f32;
    playable.set_piece_aspect_ratio(aspect);

    playable.group_pose[0] = Pose2::try_from_mm_degrees(0.0, 0.0, 90.0).expect("finite");

    let p0 = playable
        .piece_world_pose(heddobureika_game::PieceId(0))
        .expect("piece 0 pose");
    let p1 = playable
        .piece_world_pose(heddobureika_game::PieceId(1))
        .expect("piece 1 pose");

    assert_approx(p0.x_mm(), 0.0);
    assert_approx(p0.y_mm(), 0.0);
    // Without the aspect fix, p1.y_mm() would be 1.0; with the fix it must be
    // 1/aspect so that `p1.y_mm() * piece_height_px == piece_width_px`.
    assert_approx(p1.x_mm(), 0.0);
    assert_approx(p1.y_mm(), 1.0 / aspect);

    // The rendering boundary scales y by piece_height_px. The pixel
    // separation between piece centers must equal `piece_width_px` so the
    // 90°-rotated pieces (each of pixel height `piece_width_px`) line up.
    let piece_width_px = 300.0_f32;
    let piece_height_px = 175.0_f32;
    let p1_y_px = p1.y_mm() * piece_height_px;
    assert_approx(p1_y_px, piece_width_px);
}

/// Square pieces (aspect == 1) must produce the same rotated pose as before
/// the aspect fix — `(0, 1)` for a unit offset under 90°. Guards against the
/// helper inadvertently affecting the legacy square-piece behavior.
#[test]
fn rotated_group_pose_unchanged_for_square_pieces() {
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    assert!(logical.activate_edge(EdgeId(0)));
    let mut playable = PlayableState::new(logical, PlayRules::default());
    // Square pieces (default aspect = 1.0).
    playable.group_pose[0] = Pose2::try_from_mm_degrees(0.0, 0.0, 90.0).expect("finite");

    let p1 = playable
        .piece_world_pose(heddobureika_game::PieceId(1))
        .expect("piece 1 pose");
    assert_approx(p1.x_mm(), 0.0);
    assert_approx(p1.y_mm(), 1.0);
}

/// User-facing regression: dropping the top-left corner piece visibly at the
/// workspace top-left (pose `(0.5, 0.5)` in piece-center pose-units) must
/// trigger the corner frame snap. The previous `(0, 0)` target placed the
/// snap zone half outside the workspace frame, so dropping inside the frame
/// could never snap.
#[test]
fn corner_frame_snap_target_is_inside_workspace_frame() {
    let topology = GridTopology::try_new(5, 5).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    // Drop the top-left corner piece at the workspace top-left
    // (pose offset just inside the snap zone of `(0.5, 0.5)`).
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.52, 0.48).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(201)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("group pose");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 0.5);
}

/// Same idea for the bottom-right corner: dropping piece (cols-1, rows-1)
/// near the workspace BR corner pose `(cols-0.5, rows-0.5)` must snap.
#[test]
fn corner_frame_snap_target_is_inside_workspace_frame_bottom_right() {
    let topology = GridTopology::try_new(5, 5).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    // Piece 24 (col=4, row=4) — the bottom-right corner. Its solved
    // pose-center is `(4.5, 4.5)`; drop just inside the snap zone.
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(24),
            drop_pos: Position2::try_from_mm(4.45, 4.55).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(202)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(24))
        .expect("group pose");
    assert_approx(pose.x_mm(), 4.5);
    assert_approx(pose.y_mm(), 4.5);
}
