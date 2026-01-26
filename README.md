# ヘッドブレイカー

> heddobureikā -> head breaker -> jigsaw puzzle

A silly web-based jigsaw puzzle demo.

![screenshot](./screenshot.png)

Current puzzles:

- `zoe-potter`: [source art here](https://x.com/zoe_IRIAM/status/1933655531409060301)
- `zoe-samurai`: [source art here](https://x.com/zoe_IRIAM/status/1907209022752878958)
- `raora-by-noy`: [source art here](https://x.com/Ururuka_Noy/status/1880853322875421148)

## Dev

Requirements:

- Rust toolchain (cargo)
- `just`
- `trunk` (for frontend dev)
- `wrangler` (for worker dev)
- `worker-build` (`cargo install worker-build`)

Dev tasks live in `justfile` (run `just --list`). Common flow:

- `just dev-vars` (create `.dev.vars`)
- `just web-dev` (run the frontend locally)
- `just worker-dev` (run the worker locally via Miniflare)
- `just room-create` (admin-activate a room)
  - If `.dev.vars` exists, `ADMIN_TOKEN` is loaded from it automatically.
- `just mp-test` (integration test; requires `worker-dev` running and admin token set)

Frontend multiplayer config (optional):

- Add `#room=ROOM_ID` to the URL to auto-join a room.
- `just web-dev` sets `HEDDOBUREIKA_WS_BASE` from `ROOM_WS_BASE_URL` so the dev WS endpoint matches your worker.
- You can still override `HEDDOBUREIKA_WS_BASE` manually (e.g. via your shell or `.env`) if needed. It must end with `/ws`.

## Disclaimer

Made with a lot of help from `codex-cli` (using the `gpt-5.2-codex` model).

## License

For now any file in this repo (except for the puzzle arts) is licensed as [CC BY-NC-SA 4.0](./LICENSE), that may change in the future.
