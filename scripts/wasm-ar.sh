#!/bin/sh
set -eu

if command -v llvm-ar >/dev/null 2>&1; then
    exec llvm-ar "$@"
fi

wasm_ar_host="$(rustc -vV | sed -n 's/^host: //p')"
wasm_ar_tool="$(rustc --print sysroot)/lib/rustlib/$wasm_ar_host/bin/llvm-ar"
if [ -x "$wasm_ar_tool" ]; then
    exec "$wasm_ar_tool" "$@"
fi

echo 'Wasm crypto builds need llvm-ar. Install rustup component add llvm-tools, or set AR_wasm32_unknown_unknown to an LLVM archiver.' >&2
exit 1
