use base64::engine::general_purpose::STANDARD;
use base64::Engine;

use crate::app_core::{AppCore, AppSnapshot};
use crate::core::{
    build_group_order_from_piece_order, build_piece_order_from_groups, groups_from_connections,
    LOCAL_GAME_KEY,
};
use heddobureika_core::{
    decode, encode, GameRules, GameSnapshot, PuzzleStateSnapshot, GAME_SNAPSHOT_VERSION,
};

pub(crate) fn build_game_snapshot_from_app(snapshot: &AppSnapshot) -> Option<GameSnapshot> {
    let info = snapshot.puzzle_info.as_ref()?.clone();
    let cols = info.cols as usize;
    let rows = info.rows as usize;
    let total = cols * rows;
    if total == 0 {
        return None;
    }
    if snapshot.positions.len() != total
        || snapshot.rotations.len() != total
        || snapshot.flips.len() != total
        || snapshot.connections.len() != total
    {
        return None;
    }
    let piece_order = if snapshot.z_order.len() == total {
        snapshot.z_order.clone()
    } else {
        (0..total).collect()
    };
    let anchor_of = anchor_of_from_connections(&snapshot.connections, cols, rows);
    let group_order = build_group_order_from_piece_order(&piece_order, &anchor_of);
    let group_order_u32: Vec<u32> = group_order
        .into_iter()
        .filter_map(|id| u32::try_from(id).ok())
        .collect();
    let state = PuzzleStateSnapshot {
        positions: snapshot.positions.clone(),
        rotations: snapshot.rotations.clone(),
        flips: snapshot.flips.clone(),
        connections: snapshot.connections.clone(),
        group_order: group_order_u32,
        scramble_nonce: snapshot.scramble_nonce,
    };
    Some(GameSnapshot {
        version: GAME_SNAPSHOT_VERSION,
        seq: 0,
        rules: GameRules::default(),
        puzzle: info,
        state,
    })
}

pub(crate) enum ApplySnapshotResult {
    Applied,
    NotReady,
    Mismatch,
}

pub(crate) fn apply_game_snapshot_to_core(
    snapshot: &GameSnapshot,
    core: &AppCore,
    current: &AppSnapshot,
) -> ApplySnapshotResult {
    let Some(info) = current.puzzle_info.as_ref() else {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore not ready (puzzle info)");
        }
        return ApplySnapshotResult::NotReady;
    };
    if info.image_src != snapshot.puzzle.image_src {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch image_src");
        }
        return ApplySnapshotResult::Mismatch;
    }
    if info.image_width != snapshot.puzzle.image_width
        || info.image_height != snapshot.puzzle.image_height
    {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch image dims");
        }
        return ApplySnapshotResult::Mismatch;
    }
    if info.cols != snapshot.puzzle.cols || info.rows != snapshot.puzzle.rows {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch grid");
        }
        return ApplySnapshotResult::Mismatch;
    }
    if info.shape_seed != snapshot.puzzle.shape_seed {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch shape seed");
        }
        return ApplySnapshotResult::Mismatch;
    }
    let cols = snapshot.puzzle.cols as usize;
    let rows = snapshot.puzzle.rows as usize;
    let total = cols * rows;
    if total == 0 {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore not ready (empty grid)");
        }
        return ApplySnapshotResult::NotReady;
    }
    if snapshot.state.positions.len() != total
        || snapshot.state.rotations.len() != total
        || snapshot.state.flips.len() != total
        || snapshot.state.connections.len() != total
    {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch state lengths");
        }
        return ApplySnapshotResult::Mismatch;
    }
    let group_order = filter_group_order(&snapshot.state.group_order, total);
    let anchor_of = anchor_of_from_connections(&snapshot.state.connections, cols, rows);
    let piece_order = build_piece_order_from_groups(&group_order, &anchor_of);
    core.apply_snapshot(
        snapshot.state.positions.clone(),
        snapshot.state.rotations.clone(),
        snapshot.state.flips.clone(),
        snapshot.state.connections.clone(),
        piece_order,
        snapshot.state.scramble_nonce,
    );
    #[cfg(target_arch = "wasm32")]
    {
        gloo::console::log!("local snapshot: restore applied");
    }
    ApplySnapshotResult::Applied
}

pub(crate) fn load_local_snapshot() -> Option<GameSnapshot> {
    #[cfg(target_arch = "wasm32")]
    {
        gloo::console::log!("local snapshot: load");
        let window = web_sys::window()?;
        let storage = window.local_storage().ok()??;
        let raw = storage.get_item(LOCAL_GAME_KEY).ok()??;
        if raw.is_empty() {
            gloo::console::log!("local snapshot: empty value");
            return None;
        }
        let bytes = STANDARD.decode(raw.as_bytes()).ok()?;
        let snapshot = decode::<GameSnapshot>(&bytes)?;
        if snapshot.version != GAME_SNAPSHOT_VERSION {
            gloo::console::log!(
                "local snapshot: version mismatch",
                snapshot.version,
                GAME_SNAPSHOT_VERSION
            );
            return None;
        }
        gloo::console::log!("local snapshot: loaded");
        Some(snapshot)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        None
    }
}

pub(crate) fn save_local_snapshot(snapshot: &GameSnapshot) {
    #[cfg(target_arch = "wasm32")]
    {
        gloo::console::log!("local snapshot: save");
        let Some(bytes) = encode(snapshot) else {
            gloo::console::log!("local snapshot: encode failed");
            return;
        };
        let raw = STANDARD.encode(bytes);
        let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
        else {
            gloo::console::log!("local snapshot: storage unavailable");
            return;
        };
        if storage.set_item(LOCAL_GAME_KEY, &raw).is_err() {
            gloo::console::log!("local snapshot: storage set failed");
        } else {
            gloo::console::log!("local snapshot: saved");
        }
    }
}

pub(crate) fn clear_local_snapshot() {
    #[cfg(target_arch = "wasm32")]
    {
        let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
        else {
            return;
        };
        let _ = storage.remove_item(LOCAL_GAME_KEY);
    }
}

fn anchor_of_from_connections(connections: &[[bool; 4]], cols: usize, rows: usize) -> Vec<usize> {
    let total = cols * rows;
    let mut anchor_of = vec![0usize; total];
    let groups = groups_from_connections(connections, cols, rows);
    for group in groups {
        if group.is_empty() {
            continue;
        }
        let anchor = group[0];
        for id in group {
            if id < total {
                anchor_of[id] = anchor;
            }
        }
    }
    anchor_of
}

fn filter_group_order(group_order: &[u32], total: usize) -> Vec<usize> {
    group_order
        .iter()
        .filter_map(|id| usize::try_from(*id).ok())
        .filter(|id| *id < total)
        .collect()
}
