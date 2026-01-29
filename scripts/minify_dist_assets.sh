#!/usr/bin/env sh
set -eu

dist_dir="${TRUNK_DIST_DIR:-dist}"
profile="${TRUNK_PROFILE:-${TRUNK_BUILD_PROFILE:-}}"

if [ ! -d "$dist_dir" ]; then
  exit 0
fi

css_files=$(find "$dist_dir" -maxdepth 1 -type f -name "*.css" 2>/dev/null || true)
if [ -n "$css_files" ]; then
  echo "$css_files" | while IFS= read -r css_file; do
    if [ -n "$css_file" ]; then
      npx --no-install lightningcss --minify "$css_file" -o "$css_file"
    fi
  done
fi

case "$profile" in
  release|Release|prod|production) ;; # ok
  *) exit 0 ;;
esac

js_files=$(find "$dist_dir" -maxdepth 1 -type f -name "*.js" 2>/dev/null || true)
if [ -n "$js_files" ]; then
  echo "$js_files" | while IFS= read -r js_file; do
    if [ -n "$js_file" ]; then
      npx --no-install terser --compress --mangle --module "$js_file" -o "$js_file"
    fi
  done
fi
