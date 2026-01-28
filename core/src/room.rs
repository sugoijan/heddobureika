use crate::game::{collect_group, piece_local_offset, rotate_vec};
use crate::protocol::RoomUpdate;
use crate::snapshot::PuzzleStateSnapshot;

pub fn apply_room_update_to_snapshot(
    update: &RoomUpdate,
    state: &mut PuzzleStateSnapshot,
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
) -> bool {
    let total = cols * rows;
    match update {
        RoomUpdate::Ownership { .. } => false,
        RoomUpdate::GroupOrder { order } => {
            let filtered: Vec<u32> = order
                .iter()
                .copied()
                .filter(|id| (*id as usize) < total)
                .collect();
            state.group_order = filtered;
            true
        }
        RoomUpdate::Connections { connections } => {
            if connections.len() != total {
                return false;
            }
            state.connections = connections.clone();
            true
        }
        RoomUpdate::Flip { piece_id, flipped } => {
            let id = *piece_id as usize;
            if id >= total {
                return false;
            }
            if let Some(slot) = state.flips.get_mut(id) {
                *slot = *flipped;
                true
            } else {
                false
            }
        }
        RoomUpdate::GroupTransform {
            anchor_id,
            pos,
            rot_deg,
        } => {
            let anchor = *anchor_id as usize;
            if anchor >= total {
                return false;
            }
            let mut members = collect_group(&state.connections, anchor, cols, rows);
            if members.is_empty() {
                members.push(anchor);
            }
            members.sort_unstable();
            if members[0] != anchor {
                return false;
            }
            for id in members {
                let (dx, dy) = piece_local_offset(id, anchor, cols, piece_width, piece_height);
                let (rx, ry) = rotate_vec(dx, dy, *rot_deg);
                if let Some(slot) = state.positions.get_mut(id) {
                    *slot = (pos.0 + rx, pos.1 + ry);
                }
                if let Some(slot) = state.rotations.get_mut(id) {
                    *slot = *rot_deg;
                }
            }
            true
        }
    }
}
