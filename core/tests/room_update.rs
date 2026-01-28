use heddobureika_core::apply_room_update_to_snapshot;
use heddobureika_core::game::{set_connection, DIR_RIGHT};
use heddobureika_core::{PuzzleStateSnapshot, RoomUpdate};

fn build_state(cols: usize, rows: usize) -> PuzzleStateSnapshot {
    let total = cols * rows;
    let mut positions = Vec::with_capacity(total);
    for row in 0..rows {
        for col in 0..cols {
            positions.push((col as f32 * 100.0, row as f32 * 100.0));
        }
    }
    PuzzleStateSnapshot {
        positions,
        rotations: vec![0.0; total],
        flips: vec![false; total],
        connections: vec![[false; 4]; total],
        group_order: (0..total as u32).collect(),
        scramble_nonce: 0,
    }
}

#[test]
fn apply_group_transform_updates_positions() {
    let cols = 2usize;
    let rows = 1usize;
    let piece_width = 100.0;
    let piece_height = 100.0;
    let mut state = build_state(cols, rows);
    set_connection(&mut state.connections, 0, DIR_RIGHT, true, cols, rows);

    let update = RoomUpdate::GroupTransform {
        anchor_id: 0,
        pos: (10.0, 20.0),
        rot_deg: 0.0,
    };
    let applied =
        apply_room_update_to_snapshot(&update, &mut state, cols, rows, piece_width, piece_height);
    assert!(applied);
    assert_eq!(state.positions[0], (10.0, 20.0));
    assert_eq!(state.positions[1], (110.0, 20.0));
}

#[test]
fn apply_group_order_filters_invalid() {
    let mut state = build_state(2, 1);
    let update = RoomUpdate::GroupOrder {
        order: vec![0, 99, 1],
    };
    let applied =
        apply_room_update_to_snapshot(&update, &mut state, 2, 1, 100.0, 100.0);
    assert!(applied);
    assert_eq!(state.group_order, vec![0, 1]);
}

#[test]
fn apply_connections_replaces_state() {
    let mut state = build_state(2, 1);
    let mut next = vec![[false; 4]; 2];
    next[0][DIR_RIGHT] = true;
    let update = RoomUpdate::Connections { connections: next.clone() };
    let applied =
        apply_room_update_to_snapshot(&update, &mut state, 2, 1, 100.0, 100.0);
    assert!(applied);
    assert_eq!(state.connections, next);
}

#[test]
fn apply_flip_updates_state() {
    let mut state = build_state(2, 1);
    let update = RoomUpdate::Flip {
        piece_id: 1,
        flipped: true,
    };
    let applied =
        apply_room_update_to_snapshot(&update, &mut state, 2, 1, 100.0, 100.0);
    assert!(applied);
    assert_eq!(state.flips[1], true);
}
