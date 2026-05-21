use heddobureika_game::{
    ActionId, EdgeId, FlipState, GridTopology, HexagonalTopology, InteractableState, LogicalState,
    MergePolicy, PieceOuterFeature, PlayRules, PlayableAction, PlayableState, Pose2, Position2,
    ProjectionScratch, ProposalApplyRejection, ProposalApplyStatus, PuzzleTopology,
    RestrictedPlayableAction, SnapRejectionReason, TriangularTessellationTopology, VoronoiTopology,
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

/// Regression: 90°-rotated top-edge piece on a non-square grid must snap so
/// its (rotated) outer edge aligns with the right frame edge in pixel space.
///
/// In a 5x3 grid with `aspect = piece_height / piece_width = 0.5`, the
/// top-edge piece `(col=2, row=0)` has its outer edge midpoint at local
/// offset `(0, -0.5)` in pose units. Rotating that by 90° via the
/// aspect-aware rotation yields `(0.25, 0)` in pose units (the rotated
/// piece is `aspect` pose-units tall = 0.5 in pose units, so its half-extent
/// is 0.25). Snapping to the right edge therefore places the piece center at
/// `x = cols - 0.25 = 4.75`, with the rotated piece-edge landing on
/// `x = 5.0` (the frame edge). The Y axis is free.
#[test]
fn transform_action_frame_snap_aligns_rotated_edge_on_non_square_grid() {
    let topology = GridTopology::try_new(5, 3).expect("valid grid");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    // piece_width_px = 300, piece_height_px = 150 → aspect = 0.5.
    playable.set_piece_aspect_ratio(0.5);

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(2),
            drop_pos: Position2::try_from_mm(4.70, 1.00).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::try_new(90.0).expect("finite"),
        },
        Some(ActionId(301)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(2))
        .expect("group pose");
    assert_approx(pose.x_mm(), 4.75);
    assert_approx(pose.y_mm(), 1.00);
    assert_approx(pose.rotation_degrees(), 90.0);
}

/// Regression: triangular-tessellation half-row edge pieces must snap to the
/// top frame edge. Prior to the topology-frame-snap implementation,
/// `is_frame_border_piece` was the default `false` for triangular pieces and
/// no frame snap targets were emitted at all.
///
/// 3x2 triangular tessellation: piece_row 0 is a half-row at canonical
/// `y = 0`. PieceId(1) is at `(1.0, 0.0)`. Dropping it just below the top
/// edge — but far enough from neighbor canonical positions to avoid a
/// join-snap — should pull only the Y axis to 0; X is preserved.
#[test]
fn transform_action_frame_snaps_triangular_top_edge_half_piece() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.25, 0.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(302)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(1))
        .expect("group pose");
    assert_approx(pose.x_mm(), 1.25);
    assert_approx(pose.y_mm(), 0.0);
}

/// Regression: triangular corner half-triangle (PieceId(0): piece_row=0,
/// col=0) must snap as a *corner* (both axes constrained), not just an edge.
/// The rounded notch in the frame's TL corner makes only this piece fit, so
/// it should snap to `(0, 0)` from any drag inside the snap zone.
#[test]
fn transform_action_frame_snaps_triangular_corner_half_triangle() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.07, 0.06).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(303)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("group pose");
    assert_approx(pose.x_mm(), 0.0);
    assert_approx(pose.y_mm(), 0.0);
}

/// Regression: triangular bottom-right corner half-triangle must also corner-
/// snap. The bottom-right canonical position for 3x2 is `(cols-1, 2*rows)`
/// = `(2, 4)`. PieceId(14) sits there (last piece in piece_row 4, col=2).
#[test]
fn transform_action_frame_snaps_triangular_corner_bottom_right_half_triangle() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(14),
            drop_pos: Position2::try_from_mm(1.95, 3.96).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(304)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(14))
        .expect("group pose");
    assert_approx(pose.x_mm(), 2.0);
    assert_approx(pose.y_mm(), 4.0);
}

/// Regression: triangular regular-row piece in the first column should snap
/// to the *left* frame edge (single-axis X). PieceId(3) is at piece_row=1,
/// col=0 → canonical `(0.5, 1.0)`. Dropping it just inside the left edge —
/// far enough from canonical to avoid a vertical-edge join with the
/// half-corner above — pulls X to `0.5`, Y is preserved.
#[test]
fn transform_action_frame_snaps_triangular_left_edge_regular_piece() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(3),
            drop_pos: Position2::try_from_mm(0.55, 1.30).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(305)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(3))
        .expect("group pose");
    assert_approx(pose.x_mm(), 0.5);
    assert_approx(pose.y_mm(), 1.30);
}

/// Regression: triangular top-edge half-piece rotated 180° must snap
/// its flat side to the *visual* bottom frame. The visual frame for a
/// 3x2 triangular tessellation runs from `y = 0` to `y = piece_rows
/// = 5`, NOT to `y = 4` (the bottom half-row anchor). Top-half-row
/// pieces have their anchor on the flat side, so the anchor itself
/// must land on the visual bottom at `y = 5` for the rotated piece to
/// fit there.
#[test]
fn transform_action_frame_snaps_triangular_top_edge_rotated_to_bottom() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());

    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.25, 4.95).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::try_new(180.0).expect("finite"),
        },
        Some(ActionId(310)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(1))
        .expect("group pose");
    assert_approx(pose.y_mm(), 5.0);
}

/// Symmetric regression: triangular bottom-edge half-piece rotated
/// 180° must snap its flat side to the visual top frame at `y = 0`.
/// Bottom-half-row pieces have their flat side one pose unit BELOW
/// the anchor, so the rotated piece's anchor lands one unit BELOW the
/// flat side (i.e., at `y = 1` for snap to top).
#[test]
fn transform_action_frame_snaps_triangular_bottom_edge_rotated_to_top() {
    let topology = TriangularTessellationTopology::try_new(3, 2).expect("valid triangular");
    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    // Piece 13: piece_row=4 (bottom half-row), col=1, canonical (1, 4).
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(13),
            drop_pos: Position2::try_from_mm(1.25, 1.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::try_new(180.0).expect("finite"),
        },
        Some(ActionId(311)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(13))
        .expect("group pose");
    assert_approx(pose.y_mm(), 1.0);
}

/// Voronoi smoke test: the universal frame-snap solver pulls a corner
/// cell to its canonical site from a small drop offset (both axes).
/// Locates a corner cell by inspecting the topology's outer features —
/// any cell with at least one `CornerAttachment` qualifies.
#[test]
fn transform_action_frame_snaps_voronoi_corner_cell() {
    let topology = VoronoiTopology::try_new(12, 7, 1.0).expect("valid voronoi");
    // Find a piece whose outer features include a CornerAttachment.
    let corner_piece = (0..topology.piece_count())
        .map(heddobureika_game::PieceId)
        .find(|piece| {
            let mut has_corner = false;
            topology.visit_outer_features(*piece, &mut |f| {
                if matches!(f, PieceOuterFeature::CornerAttachment { .. }) {
                    has_corner = true;
                }
            });
            has_corner
        })
        .expect("voronoi must have at least one corner cell");
    let (site_x, site_y) = topology
        .canonical_position_in_pose_units(corner_piece)
        .expect("site");

    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let group = heddobureika_game::GroupId(corner_piece.as_u32());
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group,
            drop_pos: Position2::try_from_mm(site_x + 0.05, site_y + 0.06).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(401)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable.pose_of(group).expect("group pose");
    assert_approx(pose.x_mm(), site_x);
    assert_approx(pose.y_mm(), site_y);
}

/// Voronoi smoke test: an edge cell with only `BorderEdge` features
/// (no CornerAttachment) snaps along ONE axis (perpendicular to the
/// matching frame side) while the other axis is left at the drop
/// position. Selects the first such cell from the topology.
#[test]
fn transform_action_frame_snaps_voronoi_edge_cell_single_axis() {
    let topology = VoronoiTopology::try_new(16, 7, 1.0).expect("valid voronoi");
    let edge_piece = (0..topology.piece_count())
        .map(heddobureika_game::PieceId)
        .find(|piece| {
            let mut border = 0usize;
            let mut corner = 0usize;
            topology.visit_outer_features(*piece, &mut |f| match f {
                PieceOuterFeature::BorderEdge { .. } => border += 1,
                PieceOuterFeature::CornerAttachment { .. } => corner += 1,
            });
            corner == 0 && border >= 1
        })
        .expect("voronoi must have an edge-only border cell");
    let (site_x, site_y) = topology
        .canonical_position_in_pose_units(edge_piece)
        .expect("site");

    let mut playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    let group = heddobureika_game::GroupId(edge_piece.as_u32());
    // Tiny drift on both axes; only the axis the BorderEdge constrains
    // (perpendicular to the frame side it sits on) snaps back.
    let drop_x = site_x + 0.05;
    let drop_y = site_y + 0.05;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group,
            drop_pos: Position2::try_from_mm(drop_x, drop_y).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(402)),
        MergePolicy::KeepFixedGroup,
    );

    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable.pose_of(group).expect("group pose");
    // Exactly one of x/y must have snapped back to the site; the other
    // axis remains at the drop position (within float tolerance).
    let x_snapped = (pose.x_mm() - site_x).abs() < 1.0e-4;
    let y_snapped = (pose.y_mm() - site_y).abs() < 1.0e-4;
    let x_drift = (pose.x_mm() - drop_x).abs() < 1.0e-4;
    let y_drift = (pose.y_mm() - drop_y).abs() < 1.0e-4;
    assert!(
        (x_snapped && y_drift) || (y_snapped && x_drift),
        "expected exactly one axis to snap: pose=({}, {}), drop=({}, {}), site=({}, {})",
        pose.x_mm(),
        pose.y_mm(),
        drop_x,
        drop_y,
        site_x,
        site_y,
    );
}

// ---------------------------------------------------------------------
// Hexagonal-tiling snap tests.
//
// Reference layout (5x3): 13 pieces, snap_frame_extent = (6, 2√3).
//   piece 0  : TL corner       anchor (0, 0)
//   piece 1  : left edge cut   anchor (0, √3)
//   piece 2  : BL corner       anchor (0, 2√3)
//   piece 3  : top tangent     anchor (1.5, √3/2)
//   piece 4  : bottom tangent  anchor (1.5, 1.5·√3)
//   piece 5  : top edge cut    anchor (3, 0)
//   piece 6  : interior        anchor (3, √3)
//   piece 7  : bottom edge cut anchor (3, 2√3)
//   piece 8  : top tangent     anchor (4.5, √3/2)
//   piece 9  : bottom tangent  anchor (4.5, 1.5·√3)
//   piece 10 : TR corner       anchor (6, 0)
//   piece 11 : right edge cut  anchor (6, √3)
//   piece 12 : BR corner       anchor (6, 2√3)
// ---------------------------------------------------------------------

const HEX_SQRT_3: f32 = 1.732_050_8;

fn hex_topology_5x3_playable() -> PlayableState<HexagonalTopology> {
    let topology = HexagonalTopology::try_new_uniform(5, 3).expect("valid hex");
    PlayableState::new(LogicalState::new(topology), PlayRules::default())
}

#[test]
fn transform_action_frame_snaps_hexagonal_tl_corner() {
    let mut playable = hex_topology_5x3_playable();
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(0),
            drop_pos: Position2::try_from_mm(0.07, 0.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(601)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(0))
        .expect("pose");
    assert_approx(pose.x_mm(), 0.0);
    assert_approx(pose.y_mm(), 0.0);
}

#[test]
fn transform_action_frame_snaps_hexagonal_tr_corner() {
    let mut playable = hex_topology_5x3_playable();
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(10),
            drop_pos: Position2::try_from_mm(5.93, 0.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(602)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(10))
        .expect("pose");
    assert_approx(pose.x_mm(), 6.0);
    assert_approx(pose.y_mm(), 0.0);
}

#[test]
fn transform_action_frame_snaps_hexagonal_br_corner() {
    let mut playable = hex_topology_5x3_playable();
    let extent_y = 2.0 * HEX_SQRT_3;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(12),
            drop_pos: Position2::try_from_mm(5.93, extent_y - 0.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(603)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(12))
        .expect("pose");
    assert_approx(pose.x_mm(), 6.0);
    assert_approx(pose.y_mm(), extent_y);
}

#[test]
fn transform_action_frame_snaps_hexagonal_bl_corner() {
    let mut playable = hex_topology_5x3_playable();
    let extent_y = 2.0 * HEX_SQRT_3;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(2),
            drop_pos: Position2::try_from_mm(0.07, extent_y - 0.05).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(604)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(2))
        .expect("pose");
    assert_approx(pose.x_mm(), 0.0);
    assert_approx(pose.y_mm(), extent_y);
}

#[test]
fn transform_action_frame_snaps_hexagonal_top_edge_cut() {
    let mut playable = hex_topology_5x3_playable();
    // Piece 5: top edge cut piece, canonical (3, 0). Drop slightly off
    // canonical to verify only the y axis snaps (the horizontal
    // BorderEdge sits on the top frame, perpendicular = y).
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(5),
            drop_pos: Position2::try_from_mm(3.0, 0.07).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(605)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(5))
        .expect("pose");
    assert_approx(pose.x_mm(), 3.0);
    assert_approx(pose.y_mm(), 0.0);
}

#[test]
fn transform_action_frame_snaps_hexagonal_bottom_edge_cut() {
    let mut playable = hex_topology_5x3_playable();
    // Piece 7: bottom edge cut piece, canonical (3, 2√3).
    let extent_y = 2.0 * HEX_SQRT_3;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(7),
            drop_pos: Position2::try_from_mm(3.0, extent_y - 0.07).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(606)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(7))
        .expect("pose");
    assert_approx(pose.x_mm(), 3.0);
    assert_approx(pose.y_mm(), extent_y);
}

#[test]
fn transform_action_frame_snaps_hexagonal_left_edge_cut() {
    let mut playable = hex_topology_5x3_playable();
    // Piece 1: left edge cut, canonical (0, √3). Drop with shifted y
    // so the join distance to the N-neighbour (piece 0 at world (0,0),
    // join target = canonical (0, √3)) exceeds the join tolerance
    // (0.2 * √3 ≈ 0.346) while still inside the frame-snap zone (x
    // within 0.2 of 0).
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(0.07, 0.5).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(607)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(1))
        .expect("pose");
    assert_approx(pose.x_mm(), 0.0);
    assert_approx(pose.y_mm(), 0.5);
}

#[test]
fn transform_action_frame_snaps_hexagonal_right_edge_cut() {
    let mut playable = hex_topology_5x3_playable();
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(11),
            drop_pos: Position2::try_from_mm(5.93, 0.5).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(608)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(11))
        .expect("pose");
    assert_approx(pose.x_mm(), 6.0);
    assert_approx(pose.y_mm(), 0.5);
}

#[test]
fn transform_action_frame_snaps_hexagonal_top_tangent() {
    let mut playable = hex_topology_5x3_playable();
    // Piece 3: top tangent, canonical (1.5, √3/2). BorderEdge piece-
    // local y = -√3/2, so world midpoint y = anchor.y - √3/2 must hit
    // y = 0 (top frame). Drop x shifted away from canonical to clear
    // the NW-neighbour (piece 0) join zone.
    let drop_y = HEX_SQRT_3 / 2.0 + 0.07;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(3),
            drop_pos: Position2::try_from_mm(0.95, drop_y).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(609)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(3))
        .expect("pose");
    assert_approx(pose.x_mm(), 0.95);
    assert_approx(pose.y_mm(), HEX_SQRT_3 / 2.0);
}

#[test]
fn transform_action_frame_snaps_hexagonal_bottom_tangent() {
    let mut playable = hex_topology_5x3_playable();
    // Piece 4: bottom tangent, canonical (1.5, 1.5·√3). BorderEdge
    // piece-local y = +√3/2 ⇒ world midpoint y = anchor.y + √3/2,
    // snaps to y = 2√3 (bottom frame).
    let anchor_y = 1.5 * HEX_SQRT_3;
    let drop_y = anchor_y - 0.07;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(4),
            drop_pos: Position2::try_from_mm(0.95, drop_y).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::zero(),
        },
        Some(ActionId(610)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(4))
        .expect("pose");
    assert_approx(pose.x_mm(), 0.95);
    assert_approx(pose.y_mm(), anchor_y);
}

#[test]
fn transform_action_frame_snaps_hexagonal_top_edge_rotated_to_bottom() {
    let mut playable = hex_topology_5x3_playable();
    // Top edge cut piece 5 rotated 180°: BorderEdge stays at piece-
    // local y = 0, world midpoint stays at anchor. Drop near bottom
    // frame; snap pulls anchor y to extent_y.
    let extent_y = 2.0 * HEX_SQRT_3;
    let batch = playable.apply_action_with_snap(
        PlayableAction::TransformGroupTo {
            group: heddobureika_game::GroupId(5),
            drop_pos: Position2::try_from_mm(3.0, extent_y - 0.07).expect("finite"),
            drop_rotation: heddobureika_game::AngleDeg::try_new(180.0).expect("finite"),
        },
        Some(ActionId(611)),
        MergePolicy::KeepFixedGroup,
    );
    assert_eq!(batch.proposal.status, ProposalApplyStatus::ActionOnly);
    let pose = playable
        .pose_of(heddobureika_game::GroupId(5))
        .expect("pose");
    assert_approx(pose.x_mm(), 3.0);
    assert_approx(pose.y_mm(), extent_y);
    assert_approx(pose.rotation_degrees(), 180.0);
}
