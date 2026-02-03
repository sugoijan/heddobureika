#!/usr/bin/env sh
set -eu

dist_dir="${TRUNK_DIST_DIR:-dist}"
stage_dir="${TRUNK_STAGING_DIR:-}"

if [ -z "$stage_dir" ] && [ -d "$dist_dir/.stage" ]; then
  stage_dir="$dist_dir/.stage"
fi

if [ -n "$stage_dir" ]; then
  dist_dir="$stage_dir"
fi
profile="${TRUNK_PROFILE:-${TRUNK_BUILD_PROFILE:-}}"

case "$profile" in
  release|Release|prod|production) ;;
  *) exit 0 ;;
esac

if [ ! -d "$dist_dir" ]; then
  exit 0
fi

css_files=$(find "$dist_dir" -maxdepth 1 -type f -name "*.css" 2>/dev/null || true)
if [ -n "$css_files" ]; then
  echo "$css_files" | while IFS= read -r css_file; do
    if [ -n "$css_file" ]; then
      pnpm exec lightningcss --minify "$css_file" -o "$css_file"
    fi
  done
fi

js_files=$(find "$dist_dir" -maxdepth 1 -type f -name "*.js" 2>/dev/null || true)
if [ -n "$js_files" ]; then
  echo "$js_files" | while IFS= read -r js_file; do
    if [ -n "$js_file" ]; then
      pnpm exec terser --compress --mangle --module "$js_file" -o "$js_file"
    fi
  done
fi

index_file="$dist_dir/index.html"
if [ -f "$index_file" ]; then
  tmp_file="$index_file.tmp"
  pnpm exec html-minifier-terser \
    --collapse-whitespace \
    --remove-comments \
    --remove-redundant-attributes \
    --remove-script-type-attributes \
    --remove-style-link-type-attributes \
    --use-short-doctype \
    --collapse-boolean-attributes \
    --remove-optional-tags \
    --minify-css true \
    --minify-js true \
    -o "$tmp_file" \
    "$index_file"
  mv "$tmp_file" "$index_file"

  if command -v pnpm >/dev/null 2>&1; then
    pnpm exec node - "$dist_dir" "$index_file" <<'NODE'
const fs = require("fs");
const path = require("path");
const crypto = require("crypto");

const distDir = process.argv[2];
const indexFile = process.argv[3];
const text = fs.readFileSync(indexFile, "utf8");

const pattern = /(href|src)="([^"]+)"([^>]*?)\bintegrity="(sha384-[^"]+)"/gi;
const publicUrlRaw = process.env.TRUNK_BUILD_PUBLIC_URL || process.env.TRUNK_PUBLIC_URL || "";
let publicUrlHost = "";
let basePath = "/";

if (publicUrlRaw) {
  if (
    publicUrlRaw.startsWith("http://") ||
    publicUrlRaw.startsWith("https://") ||
    publicUrlRaw.startsWith("//")
  ) {
    const normalized = publicUrlRaw.startsWith("//")
      ? `https:${publicUrlRaw}`
      : publicUrlRaw;
    try {
      const parsed = new URL(normalized);
      publicUrlHost = parsed.host;
      basePath = parsed.pathname || "/";
    } catch {
      basePath = "/";
    }
  } else {
    basePath = publicUrlRaw;
  }
}

if (!basePath.startsWith("/")) {
  basePath = `/${basePath}`;
}
if (!basePath.endsWith("/")) {
  basePath = `${basePath}/`;
}

const sriFor = (filePath) =>
  `sha384-${crypto.createHash("sha384").update(fs.readFileSync(filePath)).digest("base64")}`;

const toAssetPath = (url) => {
  if (url.startsWith("data:") || url.startsWith("blob:")) {
    return null;
  }

  let assetPath = "";
  const cleanedUrl = url.split("?", 1)[0].split("#", 1)[0];

  if (
    cleanedUrl.startsWith("http://") ||
    cleanedUrl.startsWith("https://") ||
    cleanedUrl.startsWith("//")
  ) {
    if (!publicUrlHost) {
      return null;
    }
    const normalized = cleanedUrl.startsWith("//")
      ? `https:${cleanedUrl}`
      : cleanedUrl;
    let parsed;
    try {
      parsed = new URL(normalized);
    } catch {
      return null;
    }
    if (parsed.host !== publicUrlHost) {
      return null;
    }
    assetPath = parsed.pathname;
  } else if (cleanedUrl.startsWith("/")) {
    assetPath = cleanedUrl;
  } else if (cleanedUrl.startsWith("./")) {
    assetPath = cleanedUrl.slice(1);
  } else if (cleanedUrl.startsWith("../")) {
    return null;
  } else {
    assetPath = `/${cleanedUrl}`;
  }

  let normalizedPath = assetPath;
  if (!normalizedPath.startsWith("/")) {
    normalizedPath = `/${normalizedPath}`;
  }

  if (basePath === "/") {
    normalizedPath = normalizedPath.replace(/^\/+/, "");
  } else if (normalizedPath.startsWith(basePath)) {
    normalizedPath = normalizedPath.slice(basePath.length);
  } else {
    return null;
  }

  return normalizedPath || null;
};

const replaceFn = (match, attr, url, between) => {
  const assetPath = toAssetPath(url);
  if (!assetPath) {
    return match;
  }
  const filePath = path.join(distDir, assetPath);
  if (!fs.existsSync(filePath) || !fs.statSync(filePath).isFile()) {
    return match;
  }
  return `${attr}="${url}"${between}integrity="${sriFor(filePath)}"`;
};

const newText = text.replace(pattern, replaceFn);
if (newText !== text) {
  fs.writeFileSync(indexFile, newText, "utf8");
}
NODE
  else
    echo "warning: pnpm not found; skipping SRI update" >&2
  fi
fi
