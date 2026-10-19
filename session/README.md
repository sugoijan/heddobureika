# Shared puzzle authority

This crate extracts the existing server's puzzle and ownership logic into portable
Rust. The Cloudflare worker and experimental P2P runtime use it for server and
browser hosts. The same authority can serve a future native desktop host.

`SessionAuthority` holds an optional canonical puzzle and its group claims. Supply
an initial snapshot with `set_snapshot`, then serially call `apply` with an
authenticated `ClientId`, a gameplay `ClientMsg`, and the current time in
milliseconds. The caller owns authentication and admission; a claimed client ID
from an untrusted message is not sufficient.

An accepted command returns `CommandEffects` containing ordered `ServerMsg`
events and the existing full-state scheduling hint. `None` preserves the server's
current ignored-command behaviour. Initialization, asset requests, and ping stay
with the adapter. The caller performs all I/O after applying the command.

Use `next_ownership_expiry` to schedule a timer and `expire_ownership` to produce
timeout events. Disconnect grace periods and live connection checks belong to the
adapter; call `release_client` when a disconnected player's claim should end.
Keep the time base consistent across command application and expiry.

`snapshot` exposes the authoritative persistence/transport snapshot read-only.
`set_snapshot` replaces the puzzle and clears all claims; if restoration fails,
it returns false and leaves the authority empty, preserving the worker's previous
restore behaviour. `clear` ends the puzzle and removes claims.

Compatibility details retained from the server:

- Multiple control and playable events can share one revision. Preserve event
  order and type; do not deduplicate events solely by revision.
- The snapshot sequence, playable revision, and state revision advance together.
- `base_revision` remains ignored, as it is by the existing server protocol.
- Group claims expire after 30 seconds; move/transform attempts by the owner
  refresh their activity timestamp according to the existing server behaviour.
- `apply` preserves the legacy snap and ownership policies. `apply_exclusive`
  previews finalisation and rejects merges into another player's held group;
  the P2P runtime uses this stricter policy while the worker keeps using `apply`.

The P2P crate handles admission, connection generations, command IDs, and drag
claim generations before accepting peer input. Transport and executor types stay
outside this crate's public API. The host's predicted UI state remains separate
from this canonical state.

Validation from the repository root:

```sh
cargo test -p heddobureika-session -p heddobureika-worker --lib
cargo check -p heddobureika-session -p heddobureika-worker --target wasm32-unknown-unknown
```

Worker-backed regression tests additionally require a running local worker and
test credentials. Run `cargo test -p heddobureika-cli --test multiplayer_sync -- --test-threads=1`
with `ADMIN_TOKEN` and `ROOM_WS_BASE_URL` pointing to that worker. The CLI upload
test runs with `pnpm test:integration:cli`. Run the multiplayer tests sequentially
against a debug worker: concurrent private-image conversion can exceed the
recording test's response deadline. This also occurs with the original worker.
