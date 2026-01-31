set dotenv-load
set shell := ["zsh", "-cu"]

ADMIN_TOKEN := env_var_or_default("ADMIN_TOKEN", env_var_or_default("ROOM_ADMIN_TOKEN", ""))
#ROOM_WS_BASE_URL := env_var_or_default("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws")
WRANGLER_LOG_PATH := env_var_or_default("WRANGLER_LOG_PATH", ".wrangler/logs")
TRUNK_PORT := env_var_or_default("TRUNK_PORT", "8081")
CADDY_CONFIG := env_var_or_default("CADDY_CONFIG", "Caddyfile.dev")

# Show available tasks
default:
    @just --list

# Create a local .dev.vars file for wrangler dev
dev-vars:
    @cp -n .dev.vars.example .dev.vars
    @echo "Created .dev.vars if it didn't exist. Update ADMIN_TOKEN inside it."

# Run the worker locally (no Cloudflare login required)
wrangler-dev:
    @WRANGLER_LOG_PATH="{{WRANGLER_LOG_PATH}}" npx --no-install wrangler dev --local --host 0.0.0.0 --persist-to .wrangler/state --show-interactive-dev-session=false

# Run the frontend locally (requires trunk)
trunk-serve:
    @mkdir -p .wrangler
    @trunk serve --port {{TRUNK_PORT}}

# Run Caddy dev proxy (serves on :8080 by default)
caddy-run:
    @caddy run --config {{CADDY_CONFIG}}

# Run worker + frontend + proxy together
[parallel]
dev: wrangler-dev trunk-serve caddy-run

# Build the worker (requires worker-build)
worker-build:
    @cd worker && worker-build --release

# Cargo-check the worker (wasm target)
worker-check:
    @cargo check -p heddobureika-worker --target wasm32-unknown-unknown

# Cargo-check the admin CLI (native target)
cli-check:
    @cargo check -p heddobureika-cli

# Check the web app
app-check:
    @cargo check -p heddobureika --target wasm32-unknown-unknown

# Run all checks (worker uses wasm target, CLI uses native)
check: worker-check cli-check app-check

# Run wasm-bindgen tests in the browser via cargo runner
wasm-test browser="firefox":
    @case "{{browser}}" in \
      firefox) GECKODRIVER=geckodriver cargo test -p heddobureika --target wasm32-unknown-unknown ;; \
      chrome) CHROMEDRIVER=chromedriver cargo test -p heddobureika --target wasm32-unknown-unknown ;; \
      safari) SAFARIDRIVER=safaridriver cargo test -p heddobureika --target wasm32-unknown-unknown ;; \
      *) echo "unknown browser '{{browser}}' (use firefox|chrome|safari)"; exit 2 ;; \
    esac

# Run a single wasm smoke test to verify the runner works
wasm-test-smoke browser="firefox":
    @case "{{browser}}" in \
      firefox) GECKODRIVER=geckodriver cargo test -p heddobureika --target wasm32-unknown-unknown wasm_smoke ;; \
      chrome) CHROMEDRIVER=chromedriver cargo test -p heddobureika --target wasm32-unknown-unknown wasm_smoke ;; \
      safari) SAFARIDRIVER=safaridriver cargo test -p heddobureika --target wasm32-unknown-unknown wasm_smoke ;; \
      *) echo "unknown browser '{{browser}}' (use firefox|chrome|safari)"; exit 2 ;; \
    esac

# Run only the multiplayer wasm test with output enabled
wasm-test-mp browser="firefox":
    @case "{{browser}}" in \
      firefox) GECKODRIVER=geckodriver cargo test -p heddobureika --target wasm32-unknown-unknown multiplayer_warns_when_image_missing -- --nocapture ;; \
      chrome) CHROMEDRIVER=chromedriver cargo test -p heddobureika --target wasm32-unknown-unknown multiplayer_warns_when_image_missing -- --nocapture ;; \
      safari) SAFARIDRIVER=safaridriver cargo test -p heddobureika --target wasm32-unknown-unknown multiplayer_warns_when_image_missing -- --nocapture ;; \
      *) echo "unknown browser '{{browser}}' (use firefox|chrome|safari)"; exit 2 ;; \
    esac

# Integration test: requires worker running and ADMIN_TOKEN/ROOM_ADMIN_TOKEN set
mp-test:
    @cargo test -p heddobureika-cli --test multiplayer_sync

# Create/activate a room via the admin CLI
create-room *args:
    @if [ -f .dev.vars ]; then \
      set -a; source .dev.vars; set +a; \
    fi; \
    token="${ADMIN_TOKEN:-${ROOM_ADMIN_TOKEN:-}}"; \
    if [ -z "$token" ]; then \
      echo "Missing admin token. Set ADMIN_TOKEN (or ROOM_ADMIN_TOKEN) or use .dev.vars."; \
      exit 1; \
    fi; \
    base_url="${ROOM_WS_BASE_URL:-ws://127.0.0.1:8787/ws}"; \
    set -- {{args}}; \
    if printf ' %s ' "$@" | grep -q -- ' --base-url '; then \
      cargo run -p heddobureika-cli -- rooms create --admin-token "$token" "$@"; \
    else \
      cargo run -p heddobureika-cli -- rooms create --admin-token "$token" --base-url "$base_url" "$@"; \
    fi
