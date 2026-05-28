#![cfg_attr(not(target_arch = "wasm32"), allow(dead_code, unused_variables))]

mod app_builder;
mod app_core;
mod app_router;
mod app_runtime;
mod boot;
mod boot_runtime;
mod core;
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
mod persisted;
mod persisted_store;
mod puzzle_image;
#[cfg(target_arch = "wasm32")]
mod renderer;
#[cfg(not(target_arch = "wasm32"))]
#[path = "renderer_stub.rs"]
mod renderer;
mod rotation_anim;
mod runtime;
mod sync_runtime;
mod view_runtime;

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
