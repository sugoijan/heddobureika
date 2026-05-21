use crate::app_core::{AppCore, AppSnapshot};
use crate::game_state::AppGameState;
use heddobureika_core::{PlayableGameSnapshot, PuzzleTopology};

pub(crate) fn build_playable_snapshot_from_app(
    snapshot: &AppSnapshot,
) -> Option<PlayableGameSnapshot> {
    let game = snapshot.game.as_ref()?;
    Some(game.to_snapshot())
}

pub(crate) enum ApplySnapshotResult {
    Applied,
    NotReady,
    Mismatch,
}

pub(crate) fn apply_playable_snapshot_to_core(
    snapshot: &PlayableGameSnapshot,
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
    if info.image_ref != snapshot.puzzle.image_ref {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch image ref");
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
    if info.topology != snapshot.puzzle.topology {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch topology descriptor");
        }
        return ApplySnapshotResult::Mismatch;
    }
    if let Some(game) = current.game.as_ref() {
        let snapshot_topology: heddobureika_core::TopologySpec =
            snapshot.state.topology.clone().into();
        if game.playable.logical.topology.to_spec() != snapshot_topology {
            #[cfg(target_arch = "wasm32")]
            {
                gloo::console::log!("local snapshot: restore mismatch topology");
            }
            return ApplySnapshotResult::Mismatch;
        }
    }
    if info.shape_seed != snapshot.puzzle.shape_seed {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch shape seed");
        }
        return ApplySnapshotResult::Mismatch;
    }
    let total = snapshot.puzzle.piece_count() as usize;
    if total == 0 {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore not ready (empty topology)");
        }
        return ApplySnapshotResult::NotReady;
    }
    let Ok(game) = AppGameState::from_snapshot(snapshot.clone()) else {
        #[cfg(target_arch = "wasm32")]
        {
            gloo::console::log!("local snapshot: restore mismatch playable state");
        }
        return ApplySnapshotResult::Mismatch;
    };
    core.install_game(game, false);
    #[cfg(target_arch = "wasm32")]
    {
        gloo::console::log!("local snapshot: restore applied");
    }
    ApplySnapshotResult::Applied
}

pub(crate) fn load_local_snapshot() -> Option<PlayableGameSnapshot> {
    #[cfg(target_arch = "wasm32")]
    {
        crate::persisted_store::snapshot()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        None
    }
}

pub(crate) fn save_local_snapshot(snapshot: &PlayableGameSnapshot) {
    #[cfg(not(target_arch = "wasm32"))]
    let _ = snapshot;

    #[cfg(target_arch = "wasm32")]
    {
        crate::persisted_store::set_snapshot(Some(snapshot.clone()));
    }
}

pub(crate) fn clear_local_snapshot() {
    #[cfg(target_arch = "wasm32")]
    {
        crate::persisted_store::set_snapshot(None);
    }
}
