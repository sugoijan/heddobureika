//! `gen-icons`: regenerate the committed web icons in `seo/` from the
//! Sketch source document. Local-only tooling: it drives the
//! `sketchtool` CLI bundled with Sketch.app, so it runs on a designer's
//! machine — the generated icons are committed and the web build never
//! needs Sketch.
//!
//! Exports two artboards from the Apple icon template:
//! - the favicon artboard becomes `favicon.svg` and `favicon.ico`
//!   (16/32/48 px), and
//! - the iOS icon artboard becomes `apple-touch-icon.png` (180 px).
//!
//! Note: exports render the artboards as saved — keep the template's
//! "Icon Grid" guide overlays hidden in the icon masters before saving,
//! or the guides end up baked into the icons.

use std::collections::BTreeMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, anyhow, bail};
use clap::Args;
use image::codecs::ico::{IcoEncoder, IcoFrame};
use image::imageops::FilterType;
use image::{DynamicImage, ExtendedColorType};

const DEFAULT_SKETCHTOOL: &str =
    "/Applications/Sketch.app/Contents/Resources/sketchtool/bin/sketchtool";
const ICO_SIZES: [u32; 3] = [48, 32, 16];
const APPLE_TOUCH_SIZE: u32 = 180;

#[derive(Args, Debug)]
pub struct GenIconsArgs {
    /// Sketch source document.
    #[arg(long, default_value = "art/icon.sketch")]
    sketch: PathBuf,

    /// Output directory for the generated, committed icons.
    #[arg(long, default_value = "seo")]
    out: PathBuf,

    /// Artboard rendered into favicon.svg and favicon.ico.
    #[arg(long, default_value = "x / Icons / Favicon")]
    favicon_item: String,

    /// Artboard rendered into apple-touch-icon.png.
    #[arg(long, default_value = "x / Icons / iOS & iPadOS")]
    touch_item: String,

    /// Best-effort mode for build hooks: exit successfully without
    /// regenerating when sketchtool is unavailable, and skip work when
    /// the generated icons are already newer than the Sketch document.
    #[arg(long)]
    auto: bool,
}

pub fn gen_icons(args: &GenIconsArgs) -> Result<()> {
    let tool = match locate_sketchtool() {
        Ok(tool) => tool,
        Err(err) if args.auto => {
            println!("gen-icons: skipped ({err})");
            return Ok(());
        }
        Err(err) => return Err(err),
    };
    if args.auto && icons_up_to_date(args)? {
        println!("gen-icons: up to date");
        return Ok(());
    }
    let work = PathBuf::from("target/gen-icons");
    fs::create_dir_all(&work).context("creating target/gen-icons")?;
    fs::create_dir_all(&args.out)
        .with_context(|| format!("creating {}", args.out.display()))?;

    let artboards = artboard_ids_by_name(&tool, &args.sketch)?;
    let favicon_id = artboards.get(&args.favicon_item).ok_or_else(|| {
        anyhow!(
            "artboard `{}` not found in {} (have: {})",
            args.favicon_item,
            args.sketch.display(),
            artboards.keys().cloned().collect::<Vec<_>>().join(", ")
        )
    })?;
    let touch_id = artboards.get(&args.touch_item).ok_or_else(|| {
        anyhow!(
            "artboard `{}` not found in {}",
            args.touch_item,
            args.sketch.display()
        )
    })?;

    let svg_export = export_artboard(&tool, &args.sketch, favicon_id, "svg", None, &work)?;
    let svg = fs::read_to_string(&svg_export)
        .with_context(|| format!("reading {}", svg_export.display()))?
        .replace(
            &format!("<title>{favicon_id}</title>"),
            "<title>favicon</title>",
        );
    let favicon_svg = args.out.join("favicon.svg");
    fs::write(&favicon_svg, svg)?;
    println!("wrote {}", favicon_svg.display());

    let favicon_png = export_artboard(&tool, &args.sketch, favicon_id, "png", Some(2), &work)?;
    let favicon_src = image::open(&favicon_png)
        .with_context(|| format!("decoding {}", favicon_png.display()))?;
    let favicon_ico = args.out.join("favicon.ico");
    write_ico(&favicon_src, &favicon_ico)?;
    println!("wrote {}", favicon_ico.display());

    let touch_png = export_artboard(&tool, &args.sketch, touch_id, "png", Some(2), &work)?;
    let touch_src = image::open(&touch_png)
        .with_context(|| format!("decoding {}", touch_png.display()))?;
    let touch_out = args.out.join("apple-touch-icon.png");
    touch_src
        .resize_exact(APPLE_TOUCH_SIZE, APPLE_TOUCH_SIZE, FilterType::Lanczos3)
        .save(&touch_out)
        .with_context(|| format!("writing {}", touch_out.display()))?;
    println!("wrote {}", touch_out.display());

    Ok(())
}

/// True when every generated icon exists and is newer than the Sketch
/// document, meaning regeneration would be a no-op.
fn icons_up_to_date(args: &GenIconsArgs) -> Result<bool> {
    let sketch_mtime = fs::metadata(&args.sketch)
        .and_then(|meta| meta.modified())
        .with_context(|| format!("reading mtime of {}", args.sketch.display()))?;
    for name in ["favicon.svg", "favicon.ico", "apple-touch-icon.png"] {
        let Ok(meta) = fs::metadata(args.out.join(name)) else {
            return Ok(false);
        };
        if meta.modified().map(|m| m < sketch_mtime).unwrap_or(true) {
            return Ok(false);
        }
    }
    Ok(true)
}

fn locate_sketchtool() -> Result<PathBuf> {
    if let Ok(path) = std::env::var("SKETCHTOOL") {
        let path = PathBuf::from(path);
        if !path.exists() {
            bail!("SKETCHTOOL points at {}, which does not exist", path.display());
        }
        return Ok(path);
    }
    let default = PathBuf::from(DEFAULT_SKETCHTOOL);
    if default.exists() {
        return Ok(default);
    }
    bail!(
        "sketchtool not found at {DEFAULT_SKETCHTOOL}; install Sketch or set \
         SKETCHTOOL=/path/to/sketchtool"
    )
}

/// Maps artboard names to their stable IDs via `sketchtool metadata`.
fn artboard_ids_by_name(tool: &Path, sketch: &Path) -> Result<BTreeMap<String, String>> {
    let output = Command::new(tool)
        .arg("metadata")
        .arg(sketch)
        .output()
        .with_context(|| format!("running {} metadata", tool.display()))?;
    if !output.status.success() {
        bail!(
            "sketchtool metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    let value: serde_json::Value =
        serde_json::from_slice(&output.stdout).context("parsing sketchtool metadata JSON")?;
    let mut map = BTreeMap::new();
    let pages = value["pagesAndArtboards"]
        .as_object()
        .ok_or_else(|| anyhow!("metadata JSON has no pagesAndArtboards"))?;
    for page in pages.values() {
        let Some(artboards) = page["artboards"].as_object() else {
            continue;
        };
        for (id, artboard) in artboards {
            if let Some(name) = artboard["name"].as_str() {
                map.insert(name.to_string(), id.clone());
            }
        }
    }
    Ok(map)
}

fn export_artboard(
    tool: &Path,
    sketch: &Path,
    artboard_id: &str,
    format: &str,
    scale: Option<u32>,
    out_dir: &Path,
) -> Result<PathBuf> {
    let mut cmd = Command::new(tool);
    cmd.arg("export")
        .arg("artboards")
        .arg(sketch)
        .arg(format!("--items={artboard_id}"))
        .arg(format!("--formats={format}"))
        .arg(format!("--output={}", out_dir.display()))
        .arg("--overwriting=YES")
        .arg("--use-id-for-name=YES");
    if let Some(scale) = scale {
        cmd.arg(format!("--scales={scale}"));
    }
    let output = cmd
        .output()
        .with_context(|| format!("running {} export artboards", tool.display()))?;
    if !output.status.success() {
        bail!(
            "sketchtool export failed for {artboard_id}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Exports are named `<id>.<ext>` or `<id>@<N>x.<ext>` depending on
    // scale; resolve whichever the running sketchtool produced.
    let mut candidates = vec![out_dir.join(format!("{artboard_id}.{format}"))];
    if let Some(scale) = scale {
        candidates.insert(0, out_dir.join(format!("{artboard_id}@{scale}x.{format}")));
    }
    candidates
        .into_iter()
        .find(|p| p.exists())
        .ok_or_else(|| anyhow!("sketchtool reported success but no {format} export found for {artboard_id}"))
}

fn write_ico(source: &DynamicImage, out: &Path) -> Result<()> {
    let resized = ICO_SIZES
        .map(|size| source.resize_exact(size, size, FilterType::Lanczos3).to_rgba8());
    let frames = resized
        .iter()
        .zip(ICO_SIZES)
        .map(|(img, size)| {
            IcoFrame::as_png(img.as_raw(), size, size, ExtendedColorType::Rgba8)
                .context("building ico frame")
        })
        .collect::<Result<Vec<_>>>()?;
    let mut bytes = Vec::new();
    IcoEncoder::new(Cursor::new(&mut bytes))
        .encode_images(&frames)
        .context("encoding favicon.ico")?;
    fs::write(out, bytes).with_context(|| format!("writing {}", out.display()))?;
    Ok(())
}
