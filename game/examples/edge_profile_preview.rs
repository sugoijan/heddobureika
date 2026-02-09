use std::fmt::Write;
use std::num::NonZeroU32;

use heddobureika_game::{
    path_to_svg_d, GridEdgeProfileSample, GridJigsawShaper, GridShapeSettings, GridTopology,
    LengthMm, PathMm, PathSegMm,
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

    let piece_w = LengthMm::try_new(piece_w).unwrap_or_else(|| {
        eprintln!("piece_width_mm must be finite");
        std::process::exit(2);
    });
    let piece_h = LengthMm::try_new(piece_h).unwrap_or_else(|| {
        eprintln!("piece_height_mm must be finite");
        std::process::exit(2);
    });

    let topology = GridTopology::new(cols, rows);
    let shaper = GridJigsawShaper;
    let settings = GridShapeSettings::default();
    let preview = shaper.build_edge_profile_preview(&topology, piece_w, piece_h, seed, &settings);
    let svg = build_svg(&preview.horizontal, &preview.vertical);

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

fn build_svg(horizontal: &[GridEdgeProfileSample], vertical: &[GridEdgeProfileSample]) -> String {
    let outer = 6.0;
    let panel_pad = 6.0;
    let section_gap = 8.0;
    let (h_section_w, h_section_h) = section_size(horizontal, 6);
    let (v_section_w, v_section_h) = section_size(vertical, 6);
    let panel_w = h_section_w.max(v_section_w) + panel_pad * 2.0;
    let panel_h = panel_pad * 2.0 + h_section_h + section_gap + v_section_h;
    let scene_w = outer * 2.0 + panel_w;
    let scene_h = outer * 2.0 + panel_h;
    let canvas_w = scene_w * 4.0;
    let canvas_h = scene_h * 4.0;

    let mut svg = String::with_capacity(48_000);
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
        "  <rect x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" fill=\"#f8fafc\"/>",
        fmt_f32(scene_w),
        fmt_f32(scene_h)
    );
    let _ = writeln!(
        svg,
        "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"#ffffff\" stroke=\"#cbd5e1\" stroke-width=\"0.35\"/>",
        fmt_f32(outer),
        fmt_f32(outer),
        fmt_f32(panel_w),
        fmt_f32(panel_h)
    );

    let section_x = outer + panel_pad;
    let mut section_y = outer + panel_pad;
    render_section(
        &mut svg,
        section_x,
        section_y,
        "Horizontal topology edges",
        horizontal,
        6,
    );
    section_y += h_section_h + section_gap;
    render_section(
        &mut svg,
        section_x,
        section_y,
        "Vertical topology edges",
        vertical,
        6,
    );

    svg.push_str("</svg>\n");
    svg
}

fn render_section(
    svg: &mut String,
    x: f32,
    y: f32,
    title: &str,
    samples: &[GridEdgeProfileSample],
    cols_max: usize,
) {
    let title_h = 5.0;
    let tile_gap = 3.0;
    let tile_label_h = 4.0;
    let tile_inner = 2.0;
    let (content_w, content_h) = section_content_size(samples);
    let tile_w = content_w + tile_inner * 2.0;
    let tile_h = content_h + tile_inner * 2.0 + tile_label_h;
    let cols = samples.len().max(1).min(cols_max);

    let _ = writeln!(
        svg,
        "  <text x=\"{}\" y=\"{}\" font-size=\"3.0\" fill=\"#0f172a\">{}</text>",
        fmt_f32(x),
        fmt_f32(y + 3.7),
        title
    );

    for (idx, sample) in samples.iter().enumerate() {
        let row = idx / cols;
        let col = idx % cols;
        let tx = x + col as f32 * (tile_w + tile_gap);
        let ty = y + title_h + row as f32 * (tile_h + tile_gap);
        let bounds = sample_bounds(sample);
        let sx = tx + tile_inner - bounds.min_x + (content_w - (bounds.max_x - bounds.min_x)) * 0.5;
        let sy = ty + tile_label_h + tile_inner - bounds.min_y
            + (content_h - (bounds.max_y - bounds.min_y)) * 0.5;
        let color = if sample.sign >= 0 {
            "#1d4ed8"
        } else {
            "#b91c1c"
        };
        let (start, end) = path_start_end(&sample.path);

        let _ = writeln!(
            svg,
            "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"#f8fafc\" stroke=\"#dbe2ea\" stroke-width=\"0.25\"/>",
            fmt_f32(tx),
            fmt_f32(ty),
            fmt_f32(tile_w),
            fmt_f32(tile_h)
        );
        let _ = writeln!(
            svg,
            "  <text x=\"{}\" y=\"{}\" font-size=\"2.2\" fill=\"#334155\">e{} r{} c{}</text>",
            fmt_f32(tx + 1.2),
            fmt_f32(ty + 3.0),
            sample.edge_id.as_u32(),
            sample.row,
            sample.col
        );
        let _ = writeln!(
            svg,
            "  <g transform=\"translate({} {})\">",
            fmt_f32(sx),
            fmt_f32(sy)
        );
        let _ = writeln!(
            svg,
            "    <path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"#94a3b8\" stroke-width=\"0.18\" stroke-dasharray=\"0.7 0.5\"/>",
            fmt_f32(start.0),
            fmt_f32(0.0),
            fmt_f32(end.0),
            fmt_f32(0.0)
        );
        let _ = writeln!(
            svg,
            "    <path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-width=\"0.45\"/>",
            path_to_svg_d(&sample.path),
            color
        );
        let _ = writeln!(
            svg,
            "    <circle cx=\"{}\" cy=\"{}\" r=\"0.55\" fill=\"{}\" stroke=\"#0f172a\" stroke-width=\"0.12\"/>",
            fmt_f32(sample.connection_point.x_mm()),
            fmt_f32(sample.connection_point.y_mm()),
            color
        );
        let _ = writeln!(svg, "  </g>");
    }
}

fn section_size(samples: &[GridEdgeProfileSample], cols_max: usize) -> (f32, f32) {
    let title_h = 5.0;
    let tile_gap = 3.0;
    let tile_label_h = 4.0;
    let tile_inner = 2.0;
    let (content_w, content_h) = section_content_size(samples);
    let tile_w = content_w + tile_inner * 2.0;
    let tile_h = content_h + tile_inner * 2.0 + tile_label_h;
    let cols = samples.len().max(1).min(cols_max);
    let rows = samples.len().div_ceil(cols).max(1);
    let width = cols as f32 * tile_w + (cols.saturating_sub(1) as f32) * tile_gap;
    let height = title_h + rows as f32 * tile_h + (rows.saturating_sub(1) as f32) * tile_gap;
    (width, height)
}

fn section_content_size(samples: &[GridEdgeProfileSample]) -> (f32, f32) {
    let mut max_w = 1.0_f32;
    let mut max_h = 1.0_f32;
    for sample in samples {
        let bounds = sample_bounds(sample);
        max_w = max_w.max((bounds.max_x - bounds.min_x).max(1.0));
        max_h = max_h.max((bounds.max_y - bounds.min_y).max(1.0));
    }
    (max_w, max_h)
}

fn sample_bounds(sample: &GridEdgeProfileSample) -> Bounds {
    let mut bounds = path_bounds(&sample.path);
    bounds.include(
        sample.connection_point.x_mm(),
        sample.connection_point.y_mm(),
    );
    bounds
}

#[derive(Clone, Copy)]
struct Bounds {
    min_x: f32,
    min_y: f32,
    max_x: f32,
    max_y: f32,
}

impl Bounds {
    fn include(&mut self, x: f32, y: f32) {
        self.min_x = self.min_x.min(x);
        self.min_y = self.min_y.min(y);
        self.max_x = self.max_x.max(x);
        self.max_y = self.max_y.max(y);
    }
}

fn path_bounds(path: &PathMm) -> Bounds {
    let mut bounds = Bounds {
        min_x: path.start.x_mm(),
        min_y: path.start.y_mm(),
        max_x: path.start.x_mm(),
        max_y: path.start.y_mm(),
    };
    for seg in path.segs.iter() {
        match seg {
            PathSegMm::LineTo { to } => bounds.include(to.x_mm(), to.y_mm()),
            PathSegMm::CubicTo { c1, c2, to } => {
                bounds.include(c1.x_mm(), c1.y_mm());
                bounds.include(c2.x_mm(), c2.y_mm());
                bounds.include(to.x_mm(), to.y_mm());
            }
        }
    }
    bounds
}

fn path_start_end(path: &PathMm) -> ((f32, f32), (f32, f32)) {
    let start = (path.start.x_mm(), path.start.y_mm());
    let mut end = start;
    for seg in path.segs.iter() {
        match seg {
            PathSegMm::LineTo { to } => end = (to.x_mm(), to.y_mm()),
            PathSegMm::CubicTo { to, .. } => end = (to.x_mm(), to.y_mm()),
        }
    }
    (start, end)
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
        "usage: cargo run -p heddobureika-game --example edge_profile_preview -- <cols> <rows> <piece_width_mm> <piece_height_mm> <seed> [output.svg]"
    );
    std::process::exit(2);
}
