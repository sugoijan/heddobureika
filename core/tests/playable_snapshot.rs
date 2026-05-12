use heddobureika_core::{
    decode, encode, GameRules, PlayableGameSnapshot, PlayableGameSnapshotError,
    PlayableGameStateSnapshot, PuzzleImageRef, PuzzleInfo, PLAYABLE_GAME_SNAPSHOT_VERSION,
};
use heddobureika_game::{
    ActionId, GridTopology, LogicalState, MergePolicy, PlayRules, PlayableAction, PlayableState,
    Position2, ProposalApplyStatus, RestoredPlayableState, RestrictedPlayableAction,
};

#[test]
fn playable_game_state_snapshot_round_trips_through_core_codec() {
    let topology = GridTopology::try_new(2, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());
    let batch = playable.apply_action_with_snap(
        PlayableAction::TranslateGroup {
            group: heddobureika_game::GroupId(1),
            drop_pos: Position2::try_from_mm(1.0, 0.0).expect("finite position"),
        },
        Some(ActionId(9)),
        heddobureika_game::MergePolicy::KeepFixedGroup,
    );
    assert_eq!(
        batch.proposal.status,
        heddobureika_game::ProposalApplyStatus::Accepted
    );

    let snapshot = PlayableGameStateSnapshot::from_playable(&playable, Some(1));
    let bytes = encode(&snapshot).expect("snapshot should encode");
    let decoded = decode::<PlayableGameStateSnapshot>(&bytes).expect("snapshot should decode");

    assert_eq!(decoded, snapshot);
    assert_eq!(decoded.revision, playable.revision);
    assert_eq!(decoded.focused_piece, Some(1));

    let RestoredPlayableState::Grid(restored) = decoded
        .restore_from_spec()
        .expect("snapshot should restore")
    else {
        panic!("expected restored grid state");
    };
    assert_eq!(restored.revision, playable.revision);
    assert_eq!(restored.logical.active_edge_count(), 1);
    assert_eq!(restored.logical.group_count(), 1);
}

#[test]
fn playable_game_snapshot_wraps_puzzle_metadata_and_revision() {
    let topology = GridTopology::try_new(1, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let mut playable = PlayableState::new(logical, PlayRules::default());
    playable.revision = 17;
    let puzzle = puzzle_info(1, 1);

    let snapshot = PlayableGameSnapshot::from_playable(
        puzzle.clone(),
        GameRules::default(),
        99,
        &playable,
        None,
    );
    let bytes = encode(&snapshot).expect("snapshot should encode");
    let decoded = decode::<PlayableGameSnapshot>(&bytes).expect("snapshot should decode");

    assert_eq!(decoded.version, PLAYABLE_GAME_SNAPSHOT_VERSION);
    assert_eq!(decoded.seq, 17);
    assert_eq!(decoded.puzzle.label, puzzle.label);
    assert_eq!(decoded.scramble_nonce, 99);
    let RestoredPlayableState::Grid(restored) = decoded
        .restore_playable_from_spec()
        .expect("snapshot should restore")
    else {
        panic!("expected restored grid state");
    };
    assert_eq!(restored.revision, 17);
    assert_eq!(restored.logical.piece_count(), 1);
}

#[test]
fn playable_game_snapshot_applies_snapping_action_in_place() {
    let topology = GridTopology::try_new(2, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let playable = PlayableState::new(logical, PlayRules::default());
    let mut snapshot = PlayableGameSnapshot::from_playable(
        puzzle_info(2, 1),
        GameRules::default(),
        7,
        &playable,
        None,
    );

    let batch = snapshot
        .apply_action_with_snap(
            PlayableAction::TranslateGroup {
                group: heddobureika_game::GroupId(1),
                drop_pos: Position2::try_from_mm(1.0, 0.0).expect("finite position"),
            },
            Some(ActionId(21)),
            MergePolicy::KeepFixedGroup,
        )
        .expect("action should apply");

    assert_eq!(batch.proposal.status, ProposalApplyStatus::Accepted);
    assert_eq!(batch.proposal.action_id, Some(ActionId(21)));
    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(snapshot.seq, 1);
    assert_eq!(snapshot.state.revision, 1);
    let RestoredPlayableState::Grid(restored) = snapshot
        .restore_playable_from_spec()
        .expect("snapshot should restore")
    else {
        panic!("expected restored grid state");
    };
    assert_eq!(restored.logical.active_edge_count(), 1);
    assert_eq!(restored.logical.group_count(), 1);
}

#[test]
fn playable_game_snapshot_applies_action_only_without_joining() {
    let topology = GridTopology::try_new(2, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let playable = PlayableState::new(logical, PlayRules::default());
    let mut snapshot = PlayableGameSnapshot::from_playable(
        puzzle_info(2, 1),
        GameRules::default(),
        8,
        &playable,
        None,
    );

    let batch = snapshot
        .apply_action_only(
            PlayableAction::TranslateGroup {
                group: heddobureika_game::GroupId(1),
                drop_pos: Position2::try_from_mm(1.0, 0.0).expect("finite position"),
            },
            Some(ActionId(22)),
        )
        .expect("action should apply");

    assert_eq!(batch.proposal.action_id, Some(ActionId(22)));
    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(snapshot.seq, 1);
    let RestoredPlayableState::Grid(restored) = snapshot
        .restore_playable_from_spec()
        .expect("snapshot should restore")
    else {
        panic!("expected restored grid state");
    };
    assert_eq!(restored.logical.active_edge_count(), 0);
    assert_eq!(restored.logical.group_count(), 2);
}

#[test]
fn playable_game_snapshot_applies_restricted_action_in_place() {
    let topology = GridTopology::try_new(1, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let playable = PlayableState::new(logical, PlayRules::default());
    let mut snapshot = PlayableGameSnapshot::from_playable(
        puzzle_info(1, 1),
        GameRules::default(),
        9,
        &playable,
        None,
    );

    let batch = snapshot
        .apply_restricted_action(
            RestrictedPlayableAction::FlipGroup {
                group: heddobureika_game::GroupId(0),
            },
            Some(ActionId(23)),
        )
        .expect("restricted action should apply");

    assert_eq!(batch.proposal.action_id, Some(ActionId(23)));
    assert_eq!(batch.revision_before, 0);
    assert_eq!(batch.revision_after, 1);
    assert_eq!(snapshot.seq, 1);
    assert_eq!(snapshot.state.group_flip, vec![true]);
}

#[test]
fn playable_game_state_snapshot_rejects_invalid_pose_values() {
    let topology = GridTopology::try_new(1, 1).expect("valid topology");
    let logical = LogicalState::new(topology);
    let playable = PlayableState::new(logical, PlayRules::default());
    let mut snapshot = PlayableGameStateSnapshot::from_playable(&playable, None);
    snapshot.group_pose[0].x_mm = f32::NAN;

    assert!(matches!(
        snapshot.restore_from_spec(),
        Err(PlayableGameSnapshotError::InvalidPose)
    ));
}

fn puzzle_info(cols: u32, rows: u32) -> PuzzleInfo {
    PuzzleInfo {
        label: "test".to_string(),
        image_ref: PuzzleImageRef::BuiltIn {
            slug: "test".to_string(),
        },
        rows,
        cols,
        shape_seed: 1,
        image_width: cols * 100,
        image_height: rows * 100,
    }
}
