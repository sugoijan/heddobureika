use std::collections::HashSet;
use std::env;
use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;

#[derive(Deserialize)]
struct CatalogFile {
    default_slug: Option<String>,
    default_src: Option<String>,
    puzzles: Vec<PuzzleEntry>,
}

#[derive(Deserialize)]
struct PuzzleEntry {
    label: String,
    slug: String,
    src: String,
    width: u32,
    height: u32,
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("missing CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir.parent().unwrap_or(&manifest_dir);
    let env_path = workspace_root.join(".env");
    let env_local_path = workspace_root.join(".env.local");

    let _ = dotenvy::from_filename(&env_local_path);
    let _ = dotenvy::from_filename(&env_path);

    println!("cargo:rerun-if-env-changed=PUZZLE_CATALOG_PATH");
    println!("cargo:rerun-if-changed={}", env_path.display());
    println!("cargo:rerun-if-changed={}", env_local_path.display());

    let catalog_path = resolve_catalog_path(workspace_root);
    println!("cargo:rerun-if-changed={}", catalog_path.display());

    let contents = fs::read_to_string(&catalog_path).unwrap_or_else(|err| {
        panic!(
            "failed to read puzzle catalog at {}: {err}",
            catalog_path.display()
        )
    });

    let catalog: CatalogFile = toml::from_str(&contents).unwrap_or_else(|err| {
        panic!(
            "failed to parse puzzle catalog at {}: {err}",
            catalog_path.display()
        )
    });

    if catalog.puzzles.is_empty() {
        panic!("puzzle catalog {} has no entries", catalog_path.display());
    }

    validate_entries(&catalog.puzzles, &catalog_path);

    let default_slug = catalog
        .default_slug
        .as_deref()
        .unwrap_or(catalog.puzzles[0].slug.as_str());
    let default_entry = catalog
        .puzzles
        .iter()
        .find(|entry| entry.slug == default_slug)
        .unwrap_or_else(|| {
            panic!(
                "default_slug '{}' not found in {}",
                default_slug,
                catalog_path.display()
            )
        });

    let default_src = catalog
        .default_src
        .as_deref()
        .unwrap_or(default_entry.src.as_str());
    if default_src != default_entry.src {
        panic!(
            "default_src '{}' does not match src '{}' for default_slug '{}' in {}",
            default_src,
            default_entry.src,
            default_slug,
            catalog_path.display()
        );
    }

    let mut output = String::new();
    writeln!(
        &mut output,
        "pub const DEFAULT_PUZZLE_SLUG: &str = {};",
        rust_string(default_slug)
    )
    .unwrap();
    writeln!(
        &mut output,
        "pub const DEFAULT_PUZZLE_SRC: &str = {};",
        rust_string(default_src)
    )
    .unwrap();
    writeln!(&mut output).unwrap();
    writeln!(
        &mut output,
        "pub const PUZZLE_CATALOG: &[PuzzleCatalogEntry] = &["
    )
    .unwrap();

    for entry in &catalog.puzzles {
        let src_value = if entry.slug == default_slug {
            "DEFAULT_PUZZLE_SRC".to_string()
        } else {
            rust_string(&entry.src)
        };
        writeln!(&mut output, "    PuzzleCatalogEntry {{").unwrap();
        writeln!(&mut output, "        label: {},", rust_string(&entry.label)).unwrap();
        writeln!(&mut output, "        slug: {},", rust_string(&entry.slug)).unwrap();
        writeln!(&mut output, "        src: {},", src_value).unwrap();
        writeln!(&mut output, "        width: {},", entry.width).unwrap();
        writeln!(&mut output, "        height: {},", entry.height).unwrap();
        writeln!(&mut output, "    }},").unwrap();
    }

    writeln!(&mut output, "];").unwrap();

    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("missing OUT_DIR"));
    let out_path = out_dir.join("puzzle_catalog.rs");
    fs::write(&out_path, output).unwrap_or_else(|err| {
        panic!("failed to write {}: {err}", out_path.display())
    });
}

fn resolve_catalog_path(workspace_root: &Path) -> PathBuf {
    let env_value = env::var("PUZZLE_CATALOG_PATH").ok();
    let raw_path = match env_value {
        Some(value) if !value.trim().is_empty() => PathBuf::from(value),
        _ => workspace_root.join("puzzles/catalog.toml"),
    };
    if raw_path.is_relative() {
        workspace_root.join(raw_path)
    } else {
        raw_path
    }
}

fn rust_string(value: &str) -> String {
    format!("{:?}", value)
}

fn validate_entries(entries: &[PuzzleEntry], catalog_path: &Path) {
    let mut slugs = HashSet::new();
    let mut srcs = HashSet::new();

    for entry in entries {
        if entry.slug.trim().is_empty() {
            panic!("puzzle slug cannot be empty in {}", catalog_path.display());
        }
        if entry.src.trim().is_empty() {
            panic!("puzzle src cannot be empty in {}", catalog_path.display());
        }
        if entry.width == 0 || entry.height == 0 {
            panic!(
                "puzzle '{}' has invalid dimensions in {}",
                entry.slug,
                catalog_path.display()
            );
        }
        if !slugs.insert(entry.slug.clone()) {
            panic!(
                "duplicate puzzle slug '{}' in {}",
                entry.slug,
                catalog_path.display()
            );
        }
        if !srcs.insert(entry.src.clone()) {
            panic!(
                "duplicate puzzle src '{}' in {}",
                entry.src,
                catalog_path.display()
            );
        }
    }
}
