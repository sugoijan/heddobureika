#![cfg_attr(not(target_arch = "wasm32"), allow(dead_code, unused_variables))]

mod app_builder;
mod app_core;
mod app_router;
mod app_runtime;
mod boot;
mod boot_runtime;
mod core;
mod crash;
mod favicon;
mod flip_anim;
mod game_state;
mod idb;
mod input;
mod local_snapshot;
mod model;
#[cfg(any(target_arch = "wasm32", test))]
mod multiplayer_bridge;
mod multiplayer_game_sync;
mod multiplayer_identity;
mod multiplayer_sync;
#[cfg(feature = "experimental-p2p")]
mod p2p_progress;
/// Stand-ins for builds without P2P so the UI needs no feature gates.
#[cfg(not(feature = "experimental-p2p"))]
mod p2p_progress {
    pub(crate) fn has_saved_progress() -> bool {
        false
    }

    pub(crate) fn load_error() -> Option<String> {
        None
    }
}
#[cfg(feature = "experimental-p2p")]
mod p2p_sync;
mod persisted;
mod persisted_store;
mod puzzle_image;
#[cfg(target_arch = "wasm32")]
mod renderer;
mod rotation_anim;
mod runtime;
mod sync_runtime;
#[cfg(target_arch = "wasm32")]
mod view_common;
mod view_runtime;
mod web;

#[cfg(target_arch = "wasm32")]
mod wgpu_app;

#[cfg(target_arch = "wasm32")]
mod svg_app;

#[cfg(feature = "dev-panel-yew")]
mod yew_app;

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        app_builder::run();
    }
}
