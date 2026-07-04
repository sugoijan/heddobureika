//! `export-pieces`: render pieces of a generated hexagonal puzzle as
//! SVG files, one file per seed. Intended to produce base shapes for
//! icon/favicon design work: every piece is its own closed `<path>`
//! (grouped per neighbour ring) in shared puzzle coordinates, so the
//! seams between pieces match exactly when imported into a vector
//! editor such as Sketch.

use std::collections::BTreeSet;
use std::fmt::Write as _;
use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result, anyhow, bail};
use clap::Args;
use heddobureika_game::hexagonal_shape::{HexagonalShapeSettings, HexagonalShaper};
use heddobureika_game::hexagonal_topology::HexagonalTopology;
use heddobureika_game::ids::{EdgeId, PieceId};
use heddobureika_game::shape::{PathSegMm, PieceGeometryProvider};
use heddobureika_game::shape_svg::piece_to_svg_paths;
use heddobureika_game::traits::shaping::TopologyShaper;
use heddobureika_game::traits::topology::PuzzleTopology;
use heddobureika_game::units::LengthMm;

#[derive(Args, Debug)]
pub struct ExportPiecesArgs {
    /// Hexagonal grid columns (must be odd, >= 3).
    #[arg(long, default_value_t = 5)]
    cols: u32,

    /// Hexagonal grid rows (>= 2).
    #[arg(long, default_value_t = 4)]
    rows: u32,

    /// Puzzle frame height in millimeters; width is derived from the
    /// aspect ratio.
    #[arg(long, default_value_t = 100.0)]
    height_mm: f32,

    /// Image aspect ratio (W/H). Defaults to the aspect for which the
    /// hex layout is exactly uniform (regular hexagons).
    #[arg(long)]
    aspect: Option<f32>,

    /// Shape seeds to export, one SVG file per seed.
    #[arg(long, value_delimiter = ',', default_values_t = [1u32, 2, 3, 4, 5, 6, 7, 8])]
    seeds: Vec<u32>,

    /// Focus piece as `<col>,<row-in-col>`. Defaults to `0,0`, the
    /// top-left corner piece.
    #[arg(long, default_value = "0,0")]
    piece: String,

    /// Number of neighbour rings around the focus piece to include
    /// (0 = just the focus piece).
    #[arg(long, default_value_t = 2)]
    rings: u32,

    /// Output directory for the SVG files.
    #[arg(long, default_value = "art/pieces")]
    out: PathBuf,
}

pub fn export_pieces(args: &ExportPiecesArgs) -> Result<()> {
    let aspect = match args.aspect {
        Some(aspect) => aspect,
        None => HexagonalTopology::uniform_aspect_for(args.cols, args.rows)
            .ok_or_else(|| anyhow!("no uniform aspect for {}x{} (need odd cols >= 3, rows >= 2)", args.cols, args.rows))?,
    };
    let topology = HexagonalTopology::try_new(args.cols, args.rows, aspect)
        .ok_or_else(|| anyhow!("invalid hex topology {}x{} (cols must be odd)", args.cols, args.rows))?;

    let frame_h = LengthMm::try_new(args.height_mm)
        .ok_or_else(|| anyhow!("invalid --height-mm {}", args.height_mm))?;
    let frame_w = LengthMm::try_new(args.height_mm * aspect)
        .ok_or_else(|| anyhow!("invalid derived width for aspect {aspect}"))?;

    let (col, row) = parse_piece(&args.piece)?;
    let focus = topology
        .piece_id_at(col, row)
        .ok_or_else(|| anyhow!("no piece at col {col}, row {row} in a {}x{} grid", args.cols, args.rows))?;

    fs::create_dir_all(&args.out)
        .with_context(|| format!("creating {}", args.out.display()))?;

    for &seed in &args.seeds {
        let svg = export_one(&topology, frame_w, frame_h, seed, focus, args.rings)?;
        let path = args.out.join(format!(
            "hex{}x{}_c{col}r{row}_seed{seed}.svg",
            args.cols, args.rows
        ));
        fs::write(&path, svg).with_context(|| format!("writing {}", path.display()))?;
        println!("wrote {}", path.display());
    }
    Ok(())
}

fn parse_piece(spec: &str) -> Result<(u32, u32)> {
    let Some((col, row)) = spec.split_once(',') else {
        bail!("--piece must be `<col>,<row>`, got `{spec}`");
    };
    Ok((
        col.trim().parse().context("piece col")?,
        row.trim().parse().context("piece row")?,
    ))
}

fn export_one(
    topology: &HexagonalTopology,
    frame_w: LengthMm,
    frame_h: LengthMm,
    seed: u32,
    focus: PieceId,
    rings: u32,
) -> Result<String> {
    let cache = HexagonalShaper
        .build_cache(
            topology,
            frame_w,
            frame_h,
            seed,
            &HexagonalShapeSettings::default(),
        )
        .map_err(|err| anyhow!("building shape cache: {err:?}"))?;

    // Focus piece first, then one group per neighbour ring.
    let mut groups: Vec<(String, Vec<PieceId>)> = vec![("focus".to_string(), vec![focus])];
    let mut seen: BTreeSet<PieceId> = BTreeSet::from([focus]);
    let mut frontier: Vec<PieceId> = vec![focus];
    for ring in 1..=rings {
        let mut next: BTreeSet<PieceId> = neighbours_of(topology, &frontier);
        next.retain(|p| !seen.contains(p));
        if next.is_empty() {
            break;
        }
        seen.extend(next.iter().copied());
        frontier = next.iter().copied().collect();
        groups.push((format!("ring{ring}"), frontier.clone()));
    }

    let all: Vec<PieceId> = seen.iter().copied().collect();
    let (min_x, min_y, max_x, max_y) = bounds(&cache, &all);
    let pad = 2.0;
    let (vx, vy) = (min_x - pad, min_y - pad);
    let (vw, vh) = (max_x - min_x + 2.0 * pad, max_y - min_y + 2.0 * pad);

    let mut svg = String::new();
    writeln!(
        svg,
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{vx:.3} {vy:.3} {vw:.3} {vh:.3}\" width=\"{vw:.1}mm\" height=\"{vh:.1}mm\">"
    )?;
    for (name, pieces) in &groups {
        writeln!(svg, "  <g id=\"{name}\">")?;
        for piece in pieces {
            let (col, row) = topology
                .piece_col_row(*piece)
                .ok_or_else(|| anyhow!("piece {piece:?} has no col/row"))?;
            let d = piece_to_svg_paths(&cache, *piece).outline;
            writeln!(
                svg,
                "    <path id=\"piece-c{col}-r{row}\" d=\"{d}\" fill=\"none\" stroke=\"#000\" stroke-width=\"0.5\"/>"
            )?;
        }
        writeln!(svg, "  </g>")?;
    }
    svg.push_str("</svg>\n");
    Ok(svg)
}

fn neighbours_of(topology: &HexagonalTopology, of: &[PieceId]) -> BTreeSet<PieceId> {
    let mut out = BTreeSet::new();
    for idx in 0..topology.edge_count() {
        let (a, b) = topology.edge_endpoints(EdgeId(idx));
        if of.contains(&a) {
            out.insert(b);
        }
        if of.contains(&b) {
            out.insert(a);
        }
    }
    for p in of {
        out.remove(p);
    }
    out
}

fn bounds(cache: &impl PieceGeometryProvider, pieces: &[PieceId]) -> (f32, f32, f32, f32) {
    let (mut min_x, mut min_y) = (f32::MAX, f32::MAX);
    let (mut max_x, mut max_y) = (f32::MIN, f32::MIN);
    let mut visit = |x: f32, y: f32| {
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
    };
    for piece in pieces {
        let geom = cache.piece_geometry(*piece);
        for idx in 0..geom.edges.len() {
            let edge = cache.piece_edge_geometry(*piece, idx);
            visit(edge.path.start.x_mm(), edge.path.start.y_mm());
            for seg in edge.path.segs.iter() {
                match seg {
                    PathSegMm::LineTo { to } => visit(to.x_mm(), to.y_mm()),
                    PathSegMm::CubicTo { c1, c2, to } => {
                        visit(c1.x_mm(), c1.y_mm());
                        visit(c2.x_mm(), c2.y_mm());
                        visit(to.x_mm(), to.y_mm());
                    }
                }
            }
        }
    }
    (min_x, min_y, max_x, max_y)
}
