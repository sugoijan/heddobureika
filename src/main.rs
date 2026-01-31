mod app_builder;
mod app_core;
mod app_router;
mod app_runtime;
mod core;
mod local_snapshot;
mod model;
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

#[cfg(any(feature = "backend-yew", feature = "dev-panel-yew"))]
mod yew_app;

#[cfg(feature = "backend-yew")]
mod svg_view;

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        app_builder::run();
    }
}
