mod app_builder;
mod app_core;
mod app_router;
mod app_runtime;
mod boot;
mod boot_runtime;
mod view_runtime;
mod core;
mod input;
mod idb;
mod local_snapshot;
mod model;
mod persisted;
mod persisted_store;
#[cfg(target_arch = "wasm32")]
mod multiplayer_bridge;
mod multiplayer_game_sync;
mod multiplayer_identity;
mod sync_runtime;
mod multiplayer_sync;
mod runtime;
#[cfg(target_arch = "wasm32")]
mod renderer;
#[cfg(not(target_arch = "wasm32"))]
#[path = "renderer_stub.rs"]
mod renderer;

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
