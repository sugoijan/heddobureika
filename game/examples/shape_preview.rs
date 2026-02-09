use std::fmt::Write;
use std::num::NonZeroU32;

use heddobureika_game::{
    path_to_svg_d, GridPuzzleDefinition, GridShapeSettings, LengthMm, PieceEdgeRef,
    PieceGeometryProvider, PieceId,
};

fn main() {
    let mut args = std::env::args();
    let _bin = args.next();

    let Some(cols_str) = args.next() else {
        print_usage_and_exit();
    };
    let Some(rows_str) = args.next() else {
        print_usage_and_exit();
    };
    let Some(piece_w_str) = args.next() else {
        print_usage_and_exit();
    };
    let Some(piece_h_str) = args.next() else {
        print_usage_and_exit();
    };
    let Some(seed_str) = args.next() else {
        print_usage_and_exit();
    };

    let cols = parse_nz("cols", &cols_str);
    let rows = parse_nz("rows", &rows_str);
    let piece_w = parse_f32("piece_width_mm", &piece_w_str);
    let piece_h = parse_f32("piece_height_mm", &piece_h_str);
    let seed = parse_u32("seed", &seed_str);

    let out_path = args.next();
    if args.next().is_some() {
        print_usage_and_exit();
    }

    let puzzle = GridPuzzleDefinition::new(
        cols,
        rows,
        LengthMm::try_new(piece_w).unwrap_or_else(|| {
            eprintln!("piece_width_mm must be finite");
            std::process::exit(2);
        }),
        LengthMm::try_new(piece_h).unwrap_or_else(|| {
            eprintln!("piece_height_mm must be finite");
            std::process::exit(2);
        }),
        seed,
        GridShapeSettings::default(),
    )
    .unwrap_or_else(|err| {
        eprintln!("failed to build grid shape cache: {err:?}");
        std::process::exit(1);
    });

    let svg = build_svg(&puzzle);

    if let Some(path) = out_path {
        std::fs::write(&path, svg).unwrap_or_else(|err| {
            eprintln!("failed to write output {}: {err}", path);
            std::process::exit(1);
        });
        eprintln!("wrote {path}");
    } else {
        println!("{svg}");
    }
}

fn build_svg(puzzle: &GridPuzzleDefinition) -> String {
    let cols = puzzle.topology.cols().get();
    let rows = puzzle.topology.rows().get();
    let piece_w = puzzle.piece_width.as_mm_f32();
    let piece_h = puzzle.piece_height.as_mm_f32();

    let margin = puzzle.shape_cache.mask_pad.as_mm_f32() + 4.0;
    let scene_w = cols as f32 * piece_w + margin * 2.0;
    let scene_h = rows as f32 * piece_h + margin * 2.0;

    // Render at 4x scale while keeping mm-like viewBox units.
    let canvas_w = scene_w * 4.0;
    let canvas_h = scene_h * 4.0;

    let mut svg = String::with_capacity((puzzle.shape_cache.piece_count() as usize) * 1200 + 1024);
    let _ = writeln!(
        svg,
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{}\" height=\"{}\" viewBox=\"0 0 {} {}\">",
        fmt_f32(canvas_w),
        fmt_f32(canvas_h),
        fmt_f32(scene_w),
        fmt_f32(scene_h)
    );
    let _ = writeln!(
        svg,
        "  <rect x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" fill=\"#fcfcfa\"/>",
        fmt_f32(scene_w),
        fmt_f32(scene_h)
    );

    let legend_x = 4.0;
    let legend_y = 5.0;
    let _ = writeln!(
        svg,
        "  <text x=\"{}\" y=\"{}\" font-size=\"3\" fill=\"#111\">boundary edge</text>",
        fmt_f32(legend_x + 8.0),
        fmt_f32(legend_y)
    );
    let _ = writeln!(
        svg,
        "  <path d=\"M {} {} L {} {}\" stroke=\"#d62828\" stroke-width=\"0.6\" fill=\"none\"/>",
        fmt_f32(legend_x),
        fmt_f32(legend_y - 1.0),
        fmt_f32(legend_x + 6.0),
        fmt_f32(legend_y - 1.0)
    );
    let _ = writeln!(
        svg,
        "  <text x=\"{}\" y=\"{}\" font-size=\"3\" fill=\"#111\">interior edge</text>",
        fmt_f32(legend_x + 8.0),
        fmt_f32(legend_y + 4.5)
    );
    let _ = writeln!(
        svg,
        "  <path d=\"M {} {} L {} {}\" stroke=\"#0077b6\" stroke-width=\"0.6\" fill=\"none\"/>",
        fmt_f32(legend_x),
        fmt_f32(legend_y + 3.5),
        fmt_f32(legend_x + 6.0),
        fmt_f32(legend_y + 3.5)
    );
    let _ = writeln!(
        svg,
        "  <text x=\"{}\" y=\"{}\" font-size=\"3\" fill=\"#111\">connection point</text>",
        fmt_f32(legend_x + 8.0),
        fmt_f32(legend_y + 9.0)
    );
    let _ = writeln!(
        svg,
        "  <circle cx=\"{}\" cy=\"{}\" r=\"0.85\" fill=\"#111\"/>",
        fmt_f32(legend_x + 3.0),
        fmt_f32(legend_y + 8.0)
    );

    for piece in 0..puzzle.shape_cache.piece_count() {
        let piece_id = PieceId(piece);
        let geom = puzzle.shape_cache.piece_geometry(piece_id);
        let (row, col) = puzzle
            .topology
            .piece_row_col(piece_id)
            .expect("row/col should exist");

        let tx = margin + col as f32 * piece_w;
        let ty = margin + row as f32 * piece_h;
        let fill = piece_fill(piece);

        let _ = writeln!(
            svg,
            "  <g transform=\"translate({} {})\">",
            fmt_f32(tx),
            fmt_f32(ty)
        );

        let outline_d = path_to_svg_d(&puzzle.shape_cache.atlas.piece_outline(piece_id));
        let _ = writeln!(
            svg,
            "    <path d=\"{}\" fill=\"{}\" fill-opacity=\"0.75\" stroke=\"#222\" stroke-width=\"0.45\"/>",
            outline_d,
            fill
        );

        for (edge_idx, edge_ref) in geom.edges.iter().enumerate() {
            let edge = puzzle.shape_cache.piece_edge_geometry(piece_id, edge_idx);
            let edge_d = path_to_svg_d(&edge.path);
            let stroke = if matches!(edge_ref, PieceEdgeRef::Interior { .. }) {
                "#0077b6"
            } else {
                "#d62828"
            };
            let _ = writeln!(
                svg,
                "    <path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-width=\"0.35\"/>",
                edge_d, stroke
            );
            let _ = writeln!(
                svg,
                "    <circle cx=\"{}\" cy=\"{}\" r=\"0.60\" fill=\"{}\" stroke=\"#111\" stroke-width=\"0.08\"/>",
                fmt_f32(edge.connection_point.x_mm()),
                fmt_f32(edge.connection_point.y_mm()),
                stroke
            );
        }

        let _ = writeln!(
            svg,
            "    <text x=\"{}\" y=\"{}\" font-size=\"3.2\" text-anchor=\"middle\" dominant-baseline=\"middle\" fill=\"#111\">{}</text>",
            fmt_f32(piece_w * 0.5),
            fmt_f32(piece_h * 0.5),
            piece
        );

        let _ = writeln!(svg, "  </g>");
    }

    svg.push_str("</svg>\n");
    svg
}

fn piece_fill(piece: u32) -> &'static str {
    const PALETTE: [&str; 8] = [
        "#ffe8d6", "#d8f3dc", "#dbe7ff", "#fef9c3", "#fce7f3", "#e2e8f0", "#f3e8ff", "#dcfce7",
    ];
    PALETTE[(piece as usize) % PALETTE.len()]
}

fn parse_nz(name: &str, raw: &str) -> NonZeroU32 {
    let value = parse_u32(name, raw);
    NonZeroU32::new(value).unwrap_or_else(|| {
        eprintln!("{name} must be > 0");
        std::process::exit(2);
    })
}

fn parse_u32(name: &str, raw: &str) -> u32 {
    raw.parse::<u32>().unwrap_or_else(|_| {
        eprintln!("invalid {name}: {raw}");
        std::process::exit(2);
    })
}

fn parse_f32(name: &str, raw: &str) -> f32 {
    raw.parse::<f32>().unwrap_or_else(|_| {
        eprintln!("invalid {name}: {raw}");
        std::process::exit(2);
    })
}

fn fmt_f32(value: f32) -> String {
    format!("{value:.3}")
}

fn print_usage_and_exit() -> ! {
    eprintln!(
        "usage: cargo run -p heddobureika-game --example shape_preview -- <cols> <rows> <piece_width_mm> <piece_height_mm> <seed> [output.svg]"
    );
    std::process::exit(2);
}
