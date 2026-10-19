use super::*;
use heddobureika_core::{
    EdgeId, FlipState, GenericPlayableState, GridTopology, LogicalState, PieceId, PlayRules,
    PlayableState, Pose2, PuzzleTopology,
};
use heddobureika_core::{GameRules, PuzzleImageRef, PuzzleInfo, TopologySpec};

/// The server's geometry must use the SAME pose→pixel placement the client
/// renders with, or scattered/clamped pieces would land in different spots
/// in multiplayer than in single-player. For triangular (uniform scale +
/// letterboxed centred frame) this used to diverge from the worker's old
/// stretch-to-fill model.
#[test]
fn room_geometry_matches_client_placement_for_triangular() {
    let spec = heddobureika_core::TopologySpec::triangular_tessellation(4, 5);
    let topology = heddobureika_core::build_topology_from_spec(&spec).expect("topology");
    let (w, h) = (1200u32, 700u32);
    let ratio = GameRules::default().workspace_padding_ratio;
    let geom = RoomGeometry::from_topology(&topology, w, h, ratio).expect("room geometry");
    let render = topology
        .build_render_geometry(
            w,
            h,
            0,
            &heddobureika_core::TriangularTessellationShapeSettings::default(),
        )
        .expect("render geometry");

    let approx = |a: f32, b: f32| (a - b).abs() < 1.0e-3;
    assert!(
        approx(geom.pose_unit_x, render.pose_unit_px[0])
            && approx(geom.pose_unit_y, render.pose_unit_px[1]),
        "pose units diverge: worker {:?} vs client {:?}",
        (geom.pose_unit_x, geom.pose_unit_y),
        render.pose_unit_px,
    );
    assert!(
        approx(geom.origin_x, render.pose_origin_px[0])
            && approx(geom.origin_y, render.pose_origin_px[1]),
        "frame origin diverges: worker {:?} vs client {:?}",
        (geom.origin_x, geom.origin_y),
        render.pose_origin_px,
    );
    // Triangular: square pose units (uniform scale) and a letterboxed,
    // non-zero centred origin — the exact properties the old worker lacked.
    assert!(
        approx(geom.pose_unit_x, geom.pose_unit_y),
        "triangular pose units should be square"
    );
    assert!(
        geom.origin_x > 0.0 || geom.origin_y > 0.0,
        "triangular frame should be letterboxed (centred origin)"
    );
}

/// Per-piece fixture data used by the 2x1 test builder.
struct LegacyFixture {
    positions: Vec<(f32, f32)>,
    rotations: Vec<f32>,
    flips: Vec<bool>,
    connections: Vec<[bool; 4]>,
    group_order: Vec<u32>,
    scramble_nonce: u32,
}

/// Restores the playable state from a `PlayableGameSnapshot` so tests
/// can read per-piece world poses, flips, and connectivity directly
/// without going through a legacy projection helper.
fn restore_grid(snapshot: &PlayableGameSnapshot) -> GenericPlayableState {
    snapshot
        .restore_playable_from_spec()
        .expect("restore playable")
}

fn piece_position_px(playable: &GenericPlayableState, piece: u32) -> (f32, f32) {
    let pose = playable
        .piece_world_pose(PieceId(piece))
        .expect("piece world pose");
    (pose.x_mm() * 100.0 - 50.0, pose.y_mm() * 100.0 - 50.0)
}

fn piece_rotation_deg(playable: &GenericPlayableState, piece: u32) -> f32 {
    playable
        .piece_world_pose(PieceId(piece))
        .expect("piece world pose")
        .rotation_degrees()
}

fn piece_flipped(playable: &GenericPlayableState, piece: u32) -> bool {
    let group = playable
        .logical
        .group_of(PieceId(piece))
        .expect("piece group");
    playable.flip_of(group) == Some(FlipState::Flipped)
}

fn group_order_anchors(playable: &GenericPlayableState) -> Vec<u32> {
    playable
        .iter_z_asc()
        .filter_map(|group| playable.anchor_piece_of_group(group))
        .map(|piece| piece.as_u32())
        .collect()
}

fn edge_active_between(playable: &GenericPlayableState, a: u32, b: u32) -> bool {
    let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
    for edge_idx in 0..playable.logical.edge_count() {
        let edge = EdgeId(edge_idx as u32);
        if playable.logical.is_edge_active(edge) != Some(true) {
            continue;
        }
        let (e_a, e_b) = playable.logical.topology.edge_endpoints(edge);
        let (e_lo, e_hi) = if e_a.as_u32() <= e_b.as_u32() {
            (e_a.as_u32(), e_b.as_u32())
        } else {
            (e_b.as_u32(), e_a.as_u32())
        };
        if e_lo == lo && e_hi == hi {
            return true;
        }
    }
    false
}

#[test]
fn bridge_action_id_is_deterministic_and_uses_client_sequence_when_present() {
    let client = ClientId::from(42);

    let from_client_seq = bridge_action_id(client, Some(7), 100);
    assert_eq!(from_client_seq, bridge_action_id(client, Some(7), 200));
    assert_ne!(from_client_seq, bridge_action_id(client, Some(8), 100));
    assert_ne!(
        from_client_seq,
        bridge_action_id(ClientId::from(43), Some(7), 100)
    );

    let from_room_seq = bridge_action_id(client, None, 100);
    assert_eq!(from_room_seq, bridge_action_id(client, Some(0), 100));
    assert_ne!(from_room_seq, bridge_action_id(client, None, 101));
    assert_ne!(from_room_seq.0, 0);
}

#[test]
fn bridge_live_move_updates_connected_group_positions_without_snapping() {
    // Start from the canonical solved state (pose `(0.5, 0.5)` →
    // piece 0 top-left at pixel `(0, 0)`) and move to a target far
    // enough from `(0.5, 0.5)` to stay outside the frame snap radius.
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(0.0, 0.0), (100.0, 0.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false, true, false, false], [false; 4]],
        group_order: vec![0],
        scramble_nonce: 7,
    }))
    .expect("live puzzle");
    let action_id = ActionId(11);

    let batch = apply_bridge_live_move(&mut live, 0, playable_position((200.0, 250.0)), action_id)
        .expect("move should apply");

    assert_eq!(batch.proposal.action_id, Some(action_id));
    assert_eq!(batch.revision_before, 5);
    assert_eq!(batch.revision_after, 6);
    assert_eq!(batch.delta.revision, 6);
    assert_eq!(live.snapshot.seq, 6);
    assert_eq!(live.snapshot.scramble_nonce, 7);
    let restored = restore_grid(&live.snapshot);
    let p0 = piece_position_px(&restored, 0);
    let p1 = piece_position_px(&restored, 1);
    assert_approx(p0.0, 200.0);
    assert_approx(p0.1, 250.0);
    assert_approx(p1.0, 300.0);
    assert_approx(p1.1, 250.0);
    assert_approx(piece_rotation_deg(&restored, 0), 0.0);
    assert_approx(piece_rotation_deg(&restored, 1), 0.0);
    assert!(edge_active_between(&restored, 0, 1));
}

#[test]
fn bridge_live_transform_rotates_connected_group_without_snapping() {
    // Same baseline as the live-move test: start at the canonical pose
    // `(0.5, 0.5)` and transform to a far target so no frame snap fires.
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(0.0, 0.0), (100.0, 0.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false, true, false, false], [false; 4]],
        group_order: vec![0],
        scramble_nonce: 8,
    }))
    .expect("live puzzle");
    let action_id = ActionId(12);

    let batch =
        apply_bridge_live_transform(&mut live, 0, playable_pose((200.0, 250.0), 90.0), action_id)
            .expect("transform should apply");

    assert_eq!(batch.proposal.action_id, Some(action_id));
    assert_eq!(batch.revision_before, 5);
    assert_eq!(batch.revision_after, 6);
    assert_eq!(batch.delta.revision, 6);
    assert_eq!(live.snapshot.seq, 6);
    let restored = restore_grid(&live.snapshot);
    let p0 = piece_position_px(&restored, 0);
    let p1 = piece_position_px(&restored, 1);
    assert_approx(p0.0, 200.0);
    assert_approx(p0.1, 250.0);
    assert_approx(p1.0, 200.0);
    assert_approx(p1.1, 350.0);
    assert_approx(piece_rotation_deg(&restored, 0), 90.0);
    assert_approx(piece_rotation_deg(&restored, 1), 90.0);
    assert_eq!(group_order_anchors(&restored), vec![0]);
}

#[test]
fn bridge_finalize_keeps_fixed_group_pose_and_snaps_neighbor() {
    // Two singletons positioned so piece 0 is already at the canonical
    // workspace TL (pose `(0.5, 0.5)`) and piece 1 sits slightly past
    // its solved neighbor pose. Finalize joins them; the completed
    // group then identity-snaps to anchor pose `(0.5, 0.5)` →
    // piece 0 TL `(0, 0)`, piece 1 TL `(100, 0)`.
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(0.0, 0.0), (105.0, 0.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false; 4]; 2],
        group_order: vec![0, 1],
        scramble_nonce: 9,
    }))
    .expect("live puzzle");
    let action_id = ActionId(13);

    let batch = apply_bridge_finalize(
        &mut live,
        1,
        Some(playable_position((105.0, 0.0))),
        Some(0.0),
        action_id,
    )
    .expect("finalize should apply");

    assert_eq!(batch.proposal.action_id, Some(action_id));
    assert_eq!(batch.revision_before, 5);
    assert_eq!(batch.revision_after, 6);
    assert_eq!(batch.delta.revision, 6);
    assert_eq!(live.snapshot.seq, 6);
    let restored = restore_grid(&live.snapshot);
    assert!(edge_active_between(&restored, 0, 1));
    let p0 = piece_position_px(&restored, 0);
    let p1 = piece_position_px(&restored, 1);
    assert_approx(p0.0, 0.0);
    assert_approx(p0.1, 0.0);
    assert_approx(p1.0, 100.0);
    assert_approx(p1.1, 0.0);
    assert_eq!(group_order_anchors(&restored), vec![0]);
}

#[test]
fn bridge_flip_updates_singleton_with_playable_snapshot() {
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(-50.0, -50.0), (50.0, -50.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false; 4]; 2],
        group_order: vec![0, 1],
        scramble_nonce: 10,
    }))
    .expect("live puzzle");
    let action_id = ActionId(14);

    let batch = apply_bridge_flip(&mut live, 1, true, None, action_id).expect("flip should apply");

    assert_eq!(batch.proposal.action_id, Some(action_id));
    assert_eq!(batch.revision_before, 5);
    assert_eq!(batch.revision_after, 6);
    assert_eq!(batch.delta.revision, 6);
    assert_eq!(live.snapshot.seq, 6);
    assert_eq!(live.snapshot.scramble_nonce, 10);
    let restored = restore_grid(&live.snapshot);
    assert!(!piece_flipped(&restored, 0));
    assert!(piece_flipped(&restored, 1));
    assert!(!edge_active_between(&restored, 0, 1));
    assert_eq!(group_order_anchors(&restored), vec![0, 1]);
    let p1 = piece_position_px(&restored, 1);
    assert_approx(p1.0, 50.0);
    assert_approx(p1.1, -50.0);
}

#[test]
fn bridge_detach_splits_piece_without_moving_original_group() {
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(20.0, 30.0), (120.0, 30.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false, true, false, false], [false; 4]],
        group_order: vec![0],
        scramble_nonce: 11,
    }))
    .expect("live puzzle");
    let action_id = ActionId(15);

    let batch = apply_bridge_detach(&mut live, 1, action_id).expect("detach should apply");

    assert_eq!(batch.proposal.action_id, Some(action_id));
    assert_eq!(batch.revision_before, 5);
    assert_eq!(batch.revision_after, 6);
    assert_eq!(batch.delta.revision, 6);
    assert_eq!(live.snapshot.seq, 6);
    let restored = restore_grid(&live.snapshot);
    assert!(!edge_active_between(&restored, 0, 1));
    assert_eq!(group_order_anchors(&restored), vec![0, 1]);
    let p0 = piece_position_px(&restored, 0);
    let p1 = piece_position_px(&restored, 1);
    assert_approx(p0.0, 20.0);
    assert_approx(p0.1, 30.0);
    assert_approx(p1.0, 120.0);
    assert_approx(p1.1, 30.0);
}

#[test]
fn playable_update_message_carries_group_changes_and_revision() {
    let mut live = RoomLivePuzzle::from_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(-50.0, -50.0), (50.0, -50.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: vec![[false, true, false, false], [false; 4]],
        group_order: vec![0],
        scramble_nonce: 24,
    }))
    .expect("live puzzle");
    let action_id = ActionId(15);
    let batch = apply_bridge_live_move(&mut live, 0, playable_position((10.0, 20.0)), action_id)
        .expect("move should apply");

    let msg = playable_update_msg_from_batch(
        &live.snapshot,
        &batch,
        PlayableRoomUpdateKind::ActionOnly,
        Some(ClientId::from(7)),
        Some(99),
    );

    let ServerMsg::PlayableUpdate {
        seq,
        update,
        source,
        client_seq,
    } = msg
    else {
        panic!("expected playable update");
    };
    assert_eq!(seq, 6);
    assert_eq!(source, Some(ClientId::from(7)));
    assert_eq!(client_seq, Some(99));
    assert_eq!(update.kind, PlayableRoomUpdateKind::ActionOnly);
    assert_eq!(update.action_id, Some(action_id.0));
    assert_eq!(update.revision_before, 5);
    assert_eq!(update.revision_after, 6);
    assert!(!update.group_changes.is_empty());
}

#[test]
fn control_update_message_carries_ownership_without_room_update() {
    let msg = control_update_msg(
        12,
        RoomControlUpdate::Ownership {
            group_anchor: 3,
            owner: Some(ClientId::from(9)),
            reason: OwnershipReason::Granted,
        },
        Some(ClientId::from(9)),
        None,
    );

    let ServerMsg::ControlUpdate {
        seq,
        update,
        source,
        client_seq,
    } = msg
    else {
        panic!("expected control update");
    };
    assert_eq!(seq, 12);
    assert_eq!(source, Some(ClientId::from(9)));
    assert_eq!(client_seq, None);
    assert!(matches!(
        update,
        RoomControlUpdate::Ownership {
            group_anchor: 3,
            owner: Some(owner),
            reason: OwnershipReason::Granted,
        } if owner == ClientId::from(9)
    ));
}

fn snapshot_2x1(state: LegacyFixture) -> PlayableGameSnapshot {
    let puzzle = PuzzleInfo {
        label: "test".to_string(),
        image_ref: PuzzleImageRef::BuiltIn {
            slug: "test".to_string(),
        },
        topology: TopologySpec::grid(2, 1).into(),
        shape_seed: 1,
        image_width: 200,
        image_height: 100,
    };
    let scramble_nonce = state.scramble_nonce;
    let topology = GridTopology::try_new(2, 1).expect("valid grid");
    let mut logical = LogicalState::new(topology);
    // 2x1 grid has exactly one horizontal edge (id 0) between piece 0 and 1.
    let edge_between_0_and_1 = state.connections[0][heddobureika_core::DIR_RIGHT]
        || state.connections[1][heddobureika_core::DIR_LEFT];
    if edge_between_0_and_1 {
        assert!(logical.activate_edge(EdgeId(0)));
    }
    let mut playable = PlayableState::new(logical, PlayRules::default());
    for group in playable.logical.active_group_ids().collect::<Vec<_>>() {
        let anchor = playable
            .anchor_piece_of_group(group)
            .expect("group has anchor");
        let idx = anchor.as_usize();
        let pose = Pose2::try_from_mm_degrees(
            (state.positions[idx].0 + 50.0) / 100.0,
            (state.positions[idx].1 + 50.0) / 100.0,
            state.rotations[idx],
        )
        .expect("finite pose");
        playable.group_pose[group.as_usize()] = pose;
        playable.group_flip[group.as_usize()] = if state.flips[idx] {
            FlipState::Flipped
        } else {
            FlipState::Normal
        };
    }
    // Apply group_order if it differs from the default (anchor-ascending).
    if !state.group_order.is_empty() {
        let mut z: Vec<heddobureika_core::GroupId> = Vec::new();
        for anchor_id in &state.group_order {
            if let Some(group) = playable.logical.group_of(PieceId(*anchor_id)) {
                if !z.contains(&group) {
                    z.push(group);
                }
            }
        }
        for group in playable.logical.active_group_ids() {
            if !z.contains(&group) {
                z.push(group);
            }
        }
        playable.z_order = z;
        playable.rebuild_z_indices_from_snapshot();
    }
    let mut snapshot = PlayableGameSnapshot::from_playable(
        puzzle,
        GameRules::default(),
        scramble_nonce,
        &playable,
        None,
    );
    set_playable_snapshot_seq(&mut snapshot, 5);
    snapshot
}

fn playable_position(pos: (f32, f32)) -> PlayablePositionSnapshot {
    PlayablePositionSnapshot {
        x_mm: (pos.0 + 50.0) / 100.0,
        y_mm: (pos.1 + 50.0) / 100.0,
    }
}

fn playable_pose(pos: (f32, f32), rot_deg: f32) -> PlayablePoseSnapshot {
    let drop_pos = playable_position(pos);
    PlayablePoseSnapshot {
        x_mm: drop_pos.x_mm,
        y_mm: drop_pos.y_mm,
        rotation_deg: rot_deg,
    }
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-4,
        "expected {expected}, got {actual}"
    );
}

fn snapshot_for_topology(spec: TopologySpec, image_w: u32, image_h: u32) -> PlayableGameSnapshot {
    let topology = heddobureika_core::build_topology_from_spec(&spec).expect("buildable topology");
    let logical = LogicalState::new(topology);
    let playable = PlayableState::new(logical, PlayRules::default());
    let puzzle = PuzzleInfo {
        label: "test".to_string(),
        image_ref: PuzzleImageRef::BuiltIn {
            slug: "test".to_string(),
        },
        topology: spec.into(),
        shape_seed: 1,
        image_width: image_w,
        image_height: image_h,
    };
    let mut snapshot =
        PlayableGameSnapshot::from_playable(puzzle, GameRules::default(), 0, &playable, None);
    set_playable_snapshot_seq(&mut snapshot, 5);
    snapshot
}

#[test]
fn every_topology_gets_per_piece_bounds_geometry() {
    // Regression: the bounds geometry used to be grid-only, so every
    // action handler bailed for non-grid topologies and no move/flip/etc.
    // processed on triangular/hexagonal/Voronoi rooms. Geometry is now
    // built from each topology's per-piece extents, so it's present for
    // all of them — and the action still applies.
    for spec in [
        TopologySpec::grid(3, 2),
        TopologySpec::triangular_tessellation(3, 2),
        TopologySpec::hexagonal(5, 4, 1.5),
        TopologySpec::voronoi(40, 1, 1.5),
    ] {
        let tag = spec.tag.clone();
        let live = RoomLivePuzzle::from_snapshot(snapshot_for_topology(spec, 600, 400))
            .expect("live puzzle");
        let geometry = live
            .geometry
            .as_ref()
            .unwrap_or_else(|| panic!("{tag} should have bounds geometry"));
        assert_eq!(
            geometry.piece_half_w.len(),
            live.snapshot.state.topology_piece_count as usize,
            "{tag} has a half-extent per piece"
        );
        assert!(
            geometry.piece_half_w.iter().all(|w| *w > 0.0),
            "{tag} piece extents are positive"
        );
    }

    // The action itself applies on a non-grid topology.
    let triangular = snapshot_for_topology(TopologySpec::triangular_tessellation(3, 2), 600, 400);
    let mut live = RoomLivePuzzle::from_snapshot(triangular).expect("live puzzle");
    let batch = apply_bridge_flip(&mut live, 0, true, None, ActionId(7)).expect("flip applies");
    assert_eq!(batch.proposal.action_id, Some(ActionId(7)));
    assert_eq!(batch.revision_after, batch.revision_before + 1);
    assert_eq!(live.snapshot.seq, batch.revision_after);
}

#[test]
fn bridge_flip_honors_client_supplied_drop_pose() {
    // The click-pivot adjustment is computed client-side; the server must
    // reproduce it by applying the post-flip pose carried on the wire,
    // not by recomputing the pre-flip world pose.
    let triangular = snapshot_for_topology(TopologySpec::triangular_tessellation(3, 2), 600, 400);
    let mut live = RoomLivePuzzle::from_snapshot(triangular).expect("live puzzle");
    let group = heddobureika_game::GroupId(0);
    let pivoted = PlayablePoseSnapshot {
        x_mm: 4.25,
        y_mm: 1.75,
        rotation_deg: 0.0,
    };
    let _ = apply_bridge_flip(&mut live, 0, true, Some(pivoted.clone()), ActionId(9))
        .expect("flip applies");
    let pose = live.playable.pose_of(group).expect("group pose");
    assert!(
        (pose.x_mm() - pivoted.x_mm).abs() <= 1.0e-3
            && (pose.y_mm() - pivoted.y_mm).abs() <= 1.0e-3,
        "server must adopt the wire drop_pose ({pivoted:?}) but got {pose:?}"
    );
    assert_eq!(live.playable.flip_of(group), Some(FlipState::Flipped));
}

#[test]
fn group_in_bounds_accepts_centered_and_rejects_offscreen() {
    let live =
        RoomLivePuzzle::from_snapshot(snapshot_for_topology(TopologySpec::grid(3, 2), 600, 400))
            .expect("live puzzle");
    let geometry = live.geometry.as_ref().expect("geometry");
    let members = [0usize];
    // The image center in pose-mm (extent 3x2 → center ~ (1.5, 1.0)) sits
    // comfortably inside the workspace.
    assert!(group_in_bounds(
        &live.snapshot,
        geometry,
        &members,
        0,
        (1.5, 1.0),
        0.0
    ));
    // A wildly off-screen anchor is rejected.
    assert!(!group_in_bounds(
        &live.snapshot,
        geometry,
        &members,
        0,
        (100.0, 100.0),
        0.0
    ));
}
fn set_playable_snapshot_seq(snapshot: &mut PlayableGameSnapshot, seq: u64) {
    snapshot.seq = seq;
    snapshot.state.revision = seq;
}

fn authority(connected: bool) -> SessionAuthority {
    let mut authority = SessionAuthority::new();
    assert!(authority.set_snapshot(snapshot_2x1(LegacyFixture {
        positions: vec![(0.0, 0.0), (100.0, 0.0)],
        rotations: vec![0.0, 0.0],
        flips: vec![false, false],
        connections: if connected {
            vec![[false, true, false, false], [false; 4]]
        } else {
            vec![[false; 4]; 2]
        },
        group_order: if connected { vec![0] } else { vec![0, 1] },
        scramble_nonce: 1,
    })));
    authority
}

fn snapshot_bytes(authority: &SessionAuthority) -> Vec<u8> {
    heddobureika_core::encode(authority.snapshot().expect("initialised")).unwrap()
}

fn event_seq(event: &ServerMsg) -> u64 {
    match event {
        ServerMsg::ControlUpdate { seq, .. } | ServerMsg::PlayableUpdate { seq, .. } => *seq,
        _ => panic!("unexpected authority event"),
    }
}

fn assert_ownership(
    event: &ServerMsg,
    anchor: u32,
    owner: Option<ClientId>,
    reason: OwnershipReason,
) {
    assert!(matches!(event, ServerMsg::ControlUpdate {
        update: RoomControlUpdate::Ownership { group_anchor, owner: actual, reason: actual_reason }, ..
    } if *group_anchor == anchor && *actual == owner && *actual_reason == reason));
}

#[test]
fn competing_players_cannot_steal_or_mutate_an_owned_group() {
    let mut session = authority(true);
    let a = ClientId(1);
    let b = ClientId(2);
    let selected = session
        .apply(a, ClientMsg::Select { piece_id: 1 }, 100)
        .unwrap();
    assert_eq!(selected.events.len(), 2);
    assert_ownership(&selected.events[0], 0, Some(a), OwnershipReason::Granted);
    assert!(matches!(&selected.events[1], ServerMsg::ControlUpdate {
        update: RoomControlUpdate::GroupOrder { order }, ..
    } if *order == vec![0]));
    assert_eq!(
        event_seq(&selected.events[0]),
        event_seq(&selected.events[1])
    );
    let before = snapshot_bytes(&session);
    let claims: Vec<_> = session.ownerships().collect();
    // These go through the public command entry point, rather than bypassing
    // ownership checks with the lower-level playable engine.
    for command in [
        ClientMsg::Select { piece_id: 0 },
        ClientMsg::Move {
            piece_id: 0,
            drop_pos: playable_position((20.0, 10.0)),
            client_seq: 1,
            base_revision: 0,
        },
        ClientMsg::Transform {
            piece_id: 0,
            drop_pose: playable_pose((20.0, 10.0), 90.0),
            client_seq: 2,
            base_revision: 0,
        },
        ClientMsg::Place {
            piece_id: 0,
            drop_pose: playable_pose((20.0, 10.0), 0.0),
            client_seq: 3,
            base_revision: 0,
        },
        ClientMsg::Rotate {
            piece_id: 0,
            drop_rotation_deg: 90.0,
            base_revision: 0,
        },
        ClientMsg::Detach {
            piece_id: 1,
            base_revision: 0,
        },
        ClientMsg::SendToBack { piece_id: 0 },
        ClientMsg::Release { piece_id: 0 },
    ] {
        assert!(session.apply(b, command, 200).is_none());
        assert_eq!(snapshot_bytes(&session), before);
        assert_eq!(session.ownerships().collect::<Vec<_>>(), claims);
    }
}

#[test]
fn selecting_another_group_releases_the_previous_claim_before_granting() {
    let mut session = authority(false);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let previous_seq = session.snapshot().unwrap().seq;
    let selected = session
        .apply(a, ClientMsg::Select { piece_id: 1 }, 200)
        .unwrap();
    assert_eq!(selected.events.len(), 3);
    assert_ownership(&selected.events[0], 0, None, OwnershipReason::AutoRelease);
    assert_ownership(&selected.events[1], 1, Some(a), OwnershipReason::Granted);
    assert_eq!(event_seq(&selected.events[0]), previous_seq + 1);
    assert!(event_seq(&selected.events[1]) > event_seq(&selected.events[0]));
    assert_eq!(
        event_seq(&selected.events[1]),
        event_seq(&selected.events[2])
    );
    assert_eq!(
        session.ownerships().collect::<Vec<_>>(),
        vec![Ownership {
            owner_id: a,
            anchor_id: 1,
            since_ms: 200,
        }]
    );
    // The released group is immediately available to the other player.
    assert!(
        session
            .apply(ClientId(2), ClientMsg::Select { piece_id: 0 }, 201)
            .is_some()
    );
}

#[test]
fn final_place_releases_before_snap_at_the_same_revision() {
    let mut session = authority(false);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 1 }, 100)
        .unwrap();
    let revision = session.snapshot().unwrap().seq;
    let placed = session
        .apply(
            a,
            ClientMsg::Place {
                piece_id: 1,
                drop_pose: playable_pose((105.0, 0.0), 0.0),
                client_seq: 77,
                base_revision: 0,
            },
            200,
        )
        .unwrap();
    assert!(placed.full_state);
    assert_eq!(placed.events.len(), 2);
    assert_ownership(&placed.events[0], 1, None, OwnershipReason::Released);
    let ServerMsg::PlayableUpdate {
        update,
        source,
        client_seq,
        seq,
    } = &placed.events[1]
    else {
        panic!("expected snap after release");
    };
    assert_eq!(*source, Some(a));
    assert_eq!(*client_seq, Some(77));
    assert_eq!(update.kind, PlayableRoomUpdateKind::Snap);
    assert_eq!(update.revision_before, revision);
    assert_eq!(update.revision_after, *seq);
    assert_eq!(event_seq(&placed.events[0]), *seq);
    let snapshot = session.snapshot().unwrap();
    assert_eq!(snapshot.seq, *seq);
    assert_eq!(snapshot.state.revision, *seq);
    assert!(edge_active_between(&restore_grid(snapshot), 0, 1));
    assert_eq!(session.ownerships().count(), 0);
}

#[test]
fn detach_moves_claim_to_the_detached_piece_and_emits_one_revision() {
    let mut session = authority(true);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let detached = session
        .apply(
            a,
            ClientMsg::Detach {
                piece_id: 1,
                base_revision: 0,
            },
            200,
        )
        .unwrap();
    assert!(detached.full_state);
    assert_eq!(detached.events.len(), 3);
    assert!(
        matches!(&detached.events[0], ServerMsg::PlayableUpdate { update, .. }
        if update.kind == PlayableRoomUpdateKind::RestrictedAction)
    );
    assert_ownership(&detached.events[1], 0, None, OwnershipReason::Released);
    assert_ownership(&detached.events[2], 1, Some(a), OwnershipReason::Granted);
    let seq = session.snapshot().unwrap().seq;
    assert!(detached.events.iter().all(|event| event_seq(event) == seq));
    assert_eq!(
        session.ownerships().collect::<Vec<_>>(),
        vec![Ownership {
            owner_id: a,
            anchor_id: 1,
            since_ms: 200,
        }]
    );
    assert!(!edge_active_between(
        &restore_grid(session.snapshot().unwrap()),
        0,
        1
    ));
    assert!(
        session
            .apply(ClientId(2), ClientMsg::Select { piece_id: 0 }, 201)
            .is_some()
    );
    assert!(
        session
            .apply(ClientId(2), ClientMsg::Select { piece_id: 1 }, 202)
            .is_none()
    );
}

#[test]
fn singleton_flip_requires_ownership_then_releases_after_the_update() {
    let mut session = authority(false);
    let a = ClientId(1);
    let command = ClientMsg::Flip {
        piece_id: 0,
        flipped: true,
        drop_pose: playable_pose((20.0, 30.0), 90.0),
        base_revision: 0,
    };
    let before = snapshot_bytes(&session);
    assert!(session.apply(a, command.clone(), 0).is_none());
    assert_eq!(snapshot_bytes(&session), before);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let flipped = session.apply(a, command, 200).unwrap();
    assert!(!flipped.full_state);
    assert_eq!(flipped.events.len(), 2);
    assert!(matches!(
        &flipped.events[0],
        ServerMsg::PlayableUpdate { .. }
    ));
    assert_ownership(&flipped.events[1], 0, None, OwnershipReason::Released);
    assert_eq!(event_seq(&flipped.events[0]), event_seq(&flipped.events[1]));
    assert_eq!(session.ownerships().count(), 0);
    let restored = restore_grid(session.snapshot().unwrap());
    assert!(piece_flipped(&restored, 0));
    assert_approx(piece_position_px(&restored, 0).0, 20.0);
    assert_approx(piece_position_px(&restored, 0).1, 30.0);
    assert_approx(piece_rotation_deg(&restored, 0), 90.0);
}

#[test]
fn moves_refresh_the_lease_and_expiry_uses_injected_time() {
    let mut session = authority(false);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    assert_eq!(session.next_ownership_expiry(), Some(30_100));
    let moved = session
        .apply(
            a,
            ClientMsg::Move {
                piece_id: 0,
                drop_pos: playable_position((20.0, 10.0)),
                client_seq: 1,
                base_revision: 0,
            },
            5_000,
        )
        .unwrap();
    assert!(!moved.full_state);
    assert_eq!(session.next_ownership_expiry(), Some(35_000));
    assert!(session.expire_ownership(34_999).is_empty());
    let seq = session.snapshot().unwrap().seq;
    let expired = session.expire_ownership(35_000);
    assert_eq!(expired.len(), 1);
    assert_ownership(&expired[0], 0, None, OwnershipReason::Timeout);
    assert_eq!(event_seq(&expired[0]), seq + 1);
    assert!(matches!(
        &expired[0],
        ServerMsg::ControlUpdate { source: None, .. }
    ));
    assert!(session.expire_ownership(35_001).is_empty());
    assert_eq!(session.next_ownership_expiry(), None);
    assert_eq!(session.snapshot().unwrap().state.revision, seq + 1);
}

#[test]
fn disconnect_release_and_snapshot_replacement_clear_claims() {
    let mut session = authority(false);
    let saved = session.snapshot().unwrap().clone();
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let event = session
        .release_client(a, OwnershipReason::Released)
        .unwrap();
    assert_ownership(&event, 0, None, OwnershipReason::Released);
    assert!(matches!(event, ServerMsg::ControlUpdate { source: Some(source), .. } if source == a));
    assert!(
        session
            .release_client(a, OwnershipReason::Released)
            .is_none()
    );
    session
        .apply(a, ClientMsg::Select { piece_id: 1 }, 200)
        .unwrap();
    assert!(session.set_snapshot(saved));
    assert_eq!(session.ownerships().count(), 0);
    assert_eq!(session.next_ownership_expiry(), None);
    assert_eq!(session.snapshot().unwrap().seq, 5);
    session.clear();
    assert!(session.snapshot().is_none());
    assert!(
        session
            .apply(a, ClientMsg::Select { piece_id: 0 }, 300)
            .is_none()
    );
}

#[test]
fn invalid_and_out_of_bounds_commands_do_not_change_the_puzzle() {
    let mut session = authority(false);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let before = snapshot_bytes(&session);
    for command in [
        ClientMsg::Select { piece_id: u32::MAX },
        ClientMsg::Transform {
            piece_id: 0,
            drop_pose: PlayablePoseSnapshot {
                x_mm: 100_000.0,
                y_mm: 100_000.0,
                rotation_deg: 0.0,
            },
            client_seq: 1,
            base_revision: 0,
        },
        ClientMsg::Move {
            piece_id: 0,
            drop_pos: PlayablePositionSnapshot {
                x_mm: f32::NAN,
                y_mm: 0.0,
            },
            client_seq: 2,
            base_revision: 0,
        },
        ClientMsg::Place {
            piece_id: 0,
            drop_pose: PlayablePoseSnapshot {
                x_mm: 0.0,
                y_mm: 0.0,
                rotation_deg: f32::INFINITY,
            },
            client_seq: 3,
            base_revision: 0,
        },
    ] {
        assert!(session.apply(a, command, 200).is_none());
        assert_eq!(snapshot_bytes(&session), before);
        assert_eq!(session.ownerships().next().unwrap().owner_id, a);
    }
}

#[test]
fn command_path_supports_all_topologies_and_preserves_sequence_invariants() {
    for spec in [
        TopologySpec::grid(3, 2),
        TopologySpec::triangular_tessellation(3, 2),
        TopologySpec::hexagonal(5, 4, 1.5),
        TopologySpec::voronoi(40, 1, 1.5),
    ] {
        let tag = spec.tag.clone();
        let snapshot = snapshot_for_topology(spec, 600, 400);
        let pose = snapshot
            .restore_playable_from_spec()
            .unwrap()
            .piece_world_pose(PieceId(0))
            .unwrap();
        let mut session = SessionAuthority::new();
        assert!(session.set_snapshot(snapshot), "{tag}");
        let a = ClientId(1);
        session
            .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
            .unwrap();
        let selected_revision = session.snapshot().unwrap().seq;
        let moved = session
            .apply(
                a,
                ClientMsg::Transform {
                    piece_id: 0,
                    drop_pose: PlayablePoseSnapshot {
                        x_mm: pose.x_mm(),
                        y_mm: pose.y_mm(),
                        rotation_deg: 15.0,
                    },
                    client_seq: 9,
                    base_revision: 0,
                },
                200,
            )
            .unwrap_or_else(|| panic!("{tag} transform"));
        let ServerMsg::PlayableUpdate {
            seq,
            update,
            client_seq,
            ..
        } = &moved.events[0]
        else {
            panic!("playable update");
        };
        assert_eq!(update.revision_before, selected_revision, "{tag}");
        assert_eq!(*seq, update.revision_after, "{tag}");
        assert_eq!(*client_seq, Some(9));
        assert_eq!(session.snapshot().unwrap().state.revision, *seq);
        session
            .snapshot()
            .unwrap()
            .restore_playable_from_spec()
            .unwrap()
            .validate()
            .unwrap();
    }
}

#[test]
fn rotate_finalizes_the_owned_group_and_release_is_idempotent() {
    let mut session = authority(true);
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 0 }, 100)
        .unwrap();
    let rotated = session
        .apply(
            a,
            ClientMsg::Rotate {
                piece_id: 0,
                drop_rotation_deg: 90.0,
                base_revision: 0,
            },
            200,
        )
        .unwrap();
    assert!(rotated.full_state);
    assert_eq!(rotated.events.len(), 2);
    assert_ownership(&rotated.events[0], 0, None, OwnershipReason::Released);
    assert_eq!(event_seq(&rotated.events[0]), event_seq(&rotated.events[1]));
    let restored = restore_grid(session.snapshot().unwrap());
    assert_approx(piece_rotation_deg(&restored, 0), 90.0);
    assert_approx(piece_rotation_deg(&restored, 1), 90.0);
    let before = snapshot_bytes(&session);
    assert!(
        session
            .apply(a, ClientMsg::Release { piece_id: 0 }, 201)
            .is_none()
    );
    assert_eq!(snapshot_bytes(&session), before);
}

#[test]
fn send_to_back_reorders_overlapping_groups_without_releasing_ownership() {
    let mut session = authority(false);
    let mut snapshot = session.snapshot().unwrap().clone();
    // Force overlap so the geometry-aware order must change.
    snapshot.state.group_pose[1] = snapshot.state.group_pose[0];
    assert!(session.set_snapshot(snapshot));
    let a = ClientId(1);
    session
        .apply(a, ClientMsg::Select { piece_id: 1 }, 100)
        .unwrap();
    let before = session.snapshot().unwrap().seq;
    let reordered = session
        .apply(a, ClientMsg::SendToBack { piece_id: 1 }, 200)
        .unwrap();
    assert!(!reordered.full_state);
    assert_eq!(reordered.events.len(), 1);
    assert!(matches!(&reordered.events[0], ServerMsg::ControlUpdate {
        seq, source: Some(source), update: RoomControlUpdate::GroupOrder { order }, ..
    } if *seq > before && *source == a && *order == vec![1, 0]));
    assert_eq!(session.ownerships().next().unwrap().owner_id, a);
    assert_eq!(
        group_order_anchors(&restore_grid(session.snapshot().unwrap())),
        vec![1, 0]
    );
    assert!(
        session
            .apply(a, ClientMsg::SendToBack { piece_id: 1 }, 201)
            .is_none()
    );
}

#[test]
fn p2p_finalisation_cannot_merge_another_players_held_group() {
    let mut session = authority(false);
    let host = ClientId(1);
    let guest = ClientId(2);
    session
        .apply_exclusive(host, ClientMsg::Select { piece_id: 0 }, 0)
        .unwrap();
    session
        .apply_exclusive(guest, ClientMsg::Select { piece_id: 1 }, 0)
        .unwrap();
    let before = snapshot_bytes(&session);
    let command = ClientMsg::Place {
        piece_id: 1,
        drop_pose: playable_pose((105.0, 0.0), 0.0),
        client_seq: 1,
        base_revision: 0,
    };
    assert!(session.apply_exclusive(guest, command.clone(), 1).is_none());
    assert_eq!(snapshot_bytes(&session), before);
    session
        .release_client(host, OwnershipReason::Released)
        .unwrap();
    assert!(session.apply_exclusive(guest, command, 2).is_some());
    assert!(edge_active_between(
        &restore_grid(session.snapshot().unwrap()),
        0,
        1
    ));
}
