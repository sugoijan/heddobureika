# Experimental P2P transport

`heddobureika-p2p` connects one host and one guest using iroh 1.3. Both players'
commands run through `heddobureika-session`; the host supplies canonical state.
The app exposes this as **P2P (experimental)** under advanced host settings.
Server remains the default. Enable the app's `experimental-p2p` Cargo feature
to include the transport and selector.

## Runtime contract

Create an endpoint with `bind`, construct a `Host` or `Guest`, and drive its
`run` future while consuming the returned event receiver. Keep its `Handle` to
send commands and close the session. The library uses a runtime abstraction that
supports both Wasm and Tokio. The game topology is currently not `Send`; a native
host should drive `Host::run` on a Tokio `LocalSet` on the game thread. Network
tasks stay separate from the serial authority queue.

Host from a validated catalog snapshot. Publish the invite from `Event::Invite`
after the home relay is online; `Host::invite` also supports native direct-address
test harnesses. Invites contain a protocol version, session ID, endpoint address,
and random capability secret. They belong in the URL fragment and must not be
logged. Debug output redacts the invite. The native endpoint and browser endpoint
use the same ALPN and framing; browser connections currently use relays.

The host binds the guest slot to the authenticated endpoint identity. A different
identity cannot take the slot, even after the first guest disconnects. To invite
someone else, end the session and host again. The browser stores guest identity
in session storage so a tab reload can rejoin the same host. `HostIdentity`
contains the private endpoint key, original invite/capability, room ID and
admitted guest key. Never expose this record or log its contents. Debug output
redacts its secrets. Rebind with `identity.key()` and use `Host::resume` to
preserve the room across restarts. Existing ownership claims are not restored.

`Event::Checkpoint` carries host-confirmed progress at most four times per
second when its revision changes, initially, and on shutdown. It is host-only
and contains a `PlayableGameSnapshot`, with no credentials or ownership claims.
Storage belongs to the adapter: the browser atomically saves `HostIdentity`
with the checkpoint, while a future desktop adapter can store them in a local
file. For a new persistent host, wait for `wait_online`, create `Host::new`,
commit `host.identity()` with `host.snapshot()`, then call
`host.require_persistence()` before running it. `Event::PersistRoom` commits a
new guest's admission before Welcome; acknowledge `committed` only after the
record commits. A failed admission save closes the host instead of admitting a
guest whose identity has not been saved. `Host::new` remains available for
ephemeral/native harnesses that do not require durable admission.

`Handle::checkpoint` captures state in the authority loop. For renderer reloads,
`Handle::prepare_reload` first pauses mutations and disconnects the guest, so
commands cannot be acknowledged after the final checkpoint. The host lock is
kept until close/reload, and failed saves can retry the barrier. A renderer
change waits for both this checkpoint and its
transaction, then commits renderer preferences before reloading. The browser
holds a Web Lock throughout hosting and uses a tab-scoped active-room marker
to resume after reload. Explicit leave clears that marker. The shared invite
remains byte-for-byte stable; endpoint address lookup handles routing changes.
Consumers should drain events through shutdown to receive the final checkpoint.
`validate_snapshot` is available to check a saved puzzle before offering restore.

Frames use checked rkyv encoding with a four-byte big-endian length and a 4 MiB
limit. The handshake has a deadline and the number of pending handshakes is
bounded. One reliable bidirectional stream preserves command/event order.
Queues are bounded; an overloaded connection closes rather than silently losing
a placement or release. Drag coalescing is a future performance improvement.

Each connection has a generation. Commands have monotonic IDs and carry the ID
of their selecting interaction. Duplicate IDs, old claims, and messages from a
replaced connection cannot mutate the current session. The host checks ownership
again for every mutation and previews snapping to avoid merging another player's
held group. The host player uses the same checks.

If a second connection uses the admitted guest identity (for example, a copied
browser tab), it replaces the old connection. The displaced guest stops retrying
and reports that the session opened elsewhere. Ordinary connection loss still
retries, so normal guest reloads and host restarts keep working.

Browser guests coordinate before binding their saved identity. A BroadcastChannel
asks the previous tab to stop, and an exclusive Web Lock stays held until that
tab's transport and endpoint have closed. The new tab then acquires the lock and
opens its endpoint. This also works when relay replacement prevents delivery of
the transport's terminal close reason. Cancelling a pending join cancels its lock
request and handoff messages. A handoff that cannot finish within ten seconds
fails without opening a competing endpoint. Both APIs must be available to join.

`Event::Messages` contains ordered authoritative events. Several event types can
share a puzzle revision; preserve their order. `Event::Reset` clears optimistic
state and installs a full snapshot and ownership map. Rejected mutations reset
only the requesting player. Initial join and reconnect also use a reset. Catalog
slug and logical dimensions must match, and the topology must restore correctly.
Catalogs should use the same underlying art; content-hash verification is not
implemented in this milestone.

The app commits an ownership-only revision when a later revision arrives. This
keeps its playable shadow aligned after releases and lease expiry while allowing
an ownership release and its snap to share a revision. Snapshot/Welcome resets
discard deferred revisions from the previous connection.

A guest pauses input during connection loss and retries the same host with a
fresh snapshot and command sequence. Buffered commands from the dead connection
are discarded. The host continues locally, releases disconnected guest claims
after five seconds, and expires idle claims after the authority's normal timeout.
There is no automatic host promotion or server fallback.

Opening a replacement P2P invite in an existing tab restarts the app through
the normal route parser. Fragment-only navigation must replace the old
transport and discard its queued input, just as opening the link in a new tab
does.

## Two-session reload check

1. Serve a P2P-enabled build and open the host at `http://127.0.0.1:8080`.
   Choose Online, check that Server is selected by default, then host with P2P.
2. Open its invite in a second tab at `http://localhost:8080` to isolate the
   guest's browser storage. Move two pieces as the guest and verify both moves
   on the host. Wait for "Progress saved on this device".
3. Reload the host. Verify that it automatically hosts the same room and its
   invite is identical. The guest must reconnect without navigating or rejoining.
4. Change the host renderer and then the guest renderer. Check that both rejoin
   the same room and can still move pieces. Finish with both using WGPU.
5. Check that all confirmed moves survived and the host reports a committed
   save. Leave explicitly, reload, and verify that hosting does not resume.
   Starting a new host from saved progress should create a different invite.

SVG rendering allows comparing every piece's outer and surface transforms
between tabs; hover outlines and viewport transforms are local UI state.

## Build and validation

```sh
trunk serve --features experimental-p2p
cargo check -p heddobureika --target wasm32-unknown-unknown
cargo check -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p
cargo test -p heddobureika-p2p -p heddobureika-session
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 WASM_BINDGEN_TEST_TIMEOUT=90 cargo test -p heddobureika-p2p --target wasm32-unknown-unknown browser_relay_roundtrip
```

The browser test connects two real Wasm endpoints through the public relay and
compares confirmed movement updates. Native socket tests cover command replay,
claim generations, third-player rejection, invalid invites, and reconnecting with
the admitted identity. App tests additionally exercise the host selector and the
guest invite route through the real app's state bridge. Run these app tests one
at a time because their global app runtimes are shared:

```sh
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_host_setting
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_guest_route
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_guest_lock_cancel
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_guest_handoff
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_progress_commits
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_progress_rejects
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_progress_save_failure
GECKODRIVER=geckodriver WASM_BINDGEN_TEST_HEADLESS=1 cargo test -p heddobureika --target wasm32-unknown-unknown --features experimental-p2p p2p_room_identity
```

Wasm crypto compilation needs a Wasm-capable C compiler and LLVM archiver.
`scripts/wasm-ar.sh` selects `llvm-ar` from PATH or the active Rust toolchain;
install `rustup component add llvm-tools` if neither exists. An explicit
`AR_wasm32_unknown_unknown` environment variable overrides this selection.
If a previous macOS build produced empty `ring` archives with the system
archiver, run `cargo clean -p ring --target wasm32-unknown-unknown` once.

Public relays are suitable for experiments and hobby use and may be rate-limited.
The browser integration test needs outbound network access. See the upstream
[browser support](https://docs.iroh.computer/languages/wasm-browser) and
[public relay policy](https://docs.iroh.computer/iroh-services/relays/public).

Host identity/progress is saved under `p2p-host-room.v1` in the existing
snapshot store, separately from local-play and server-room records. The older
`p2p-host-progress.v1` record remains readable as progress-only recovery data.
No database upgrade is needed. Writes are serialized and coalesced, and save
success waits for the transaction commit. Corrupt, incompatible, and
missing-catalog saves are not offered for restore or automatic resume. The
experimental feature can be disabled without deleting either record. Clearing
browser data removes the saved room and its identity. This milestone does not
transfer private images, implement a desktop shell, or migrate authority when
the host leaves.
