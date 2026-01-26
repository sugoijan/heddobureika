set dotenv-load
set shell := ["zsh", "-cu"]

ADMIN_TOKEN := env_var_or_default("ROOM_ADMIN_TOKEN", "")
ROOM_WS_BASE_URL := env_var_or_default("ROOM_WS_BASE_URL", "ws://127.0.0.1:8787/ws")
WRANGLER_LOG_PATH := env_var_or_default("WRANGLER_LOG_PATH", ".wrangler/logs")

# Show available tasks
default:
    @just --list

# Create a local .dev.vars file for wrangler dev
dev-vars:
    @cp -n .dev.vars.example .dev.vars
    @echo "Created .dev.vars if it didn't exist. Update ADMIN_TOKEN inside it."

# Run the worker locally (no Cloudflare login required)
worker-dev:
    @WRANGLER_LOG_PATH="{{WRANGLER_LOG_PATH}}" wrangler dev --local --persist-to .wrangler/state

# Run the frontend locally (requires trunk)
web-dev:
    @trunk serve

# Build the worker (requires worker-build)
worker-build:
    @cd worker && worker-build --release

# Cargo-check the worker (wasm target)
worker-check:
    @cargo check -p heddobureika-worker --target wasm32-unknown-unknown

# Cargo-check the admin CLI (native target)
cli-check:
    @cargo check -p heddobureika-cli

# Check the web app (requires trunk)
app-check:
    @trunk build

# Run all checks (worker uses wasm target, CLI uses native)
all-check:
    @just worker-check
    @just cli-check
    @just app-check

# Create/activate a room via the admin CLI
room-create token=ADMIN_TOKEN base_url=ROOM_WS_BASE_URL *args:
    @if [ -z "{{token}}" ]; then echo "Missing admin token. Set ROOM_ADMIN_TOKEN or pass token=..."; exit 1; fi
    @cargo run -p heddobureika-cli -- rooms create --admin-token "{{token}}" --base-url "{{base_url}}" {{args}}
