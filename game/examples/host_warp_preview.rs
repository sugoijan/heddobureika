use std::fmt::Write;
use std::num::NonZeroU32;

use heddobureika_game::{
    path_to_svg_d, BorderFrameShape, GridJigsawShaper, GridShapeSettings, GridTopology, LengthMm,
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

    let opt_a = args.next();
    let opt_b = args.next();
    if args.next().is_some() {
        print_usage_and_exit();
    }

    let (shape, out_path) = parse_optional_shape_and_output(opt_a, opt_b);
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
    let mut settings = GridShapeSettings::default();
    settings.border_shape = shape;
    settings.border_rotation_deg = 0.0;
    settings.border_inset_mm = 0.0;

    let host = shaper.build_host_shape_preview(&topology, piece_w, piece_h, seed, &settings);
    let svg = build_svg(&host, shape);

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

fn build_svg(host: &heddobureika_game::GridHostShapePreview, shape: BorderFrameShape) -> String {
    let margin = 6.0;
    let width = host.width.as_mm_f32();
    let height = host.height.as_mm_f32();
    let scene_w = width + margin * 2.0;
    let scene_h = height + margin * 2.0;
    let canvas_w = scene_w * 4.0;
    let canvas_h = scene_h * 4.0;

    let mut svg = String::with_capacity(24_000);
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
        "  <text x=\"{}\" y=\"{}\" font-size=\"3.2\" fill=\"#0f172a\">host warp preview ({})</text>",
        fmt_f32(2.0),
        fmt_f32(4.6),
        frame_shape_label(shape)
    );

    let _ = writeln!(
        svg,
        "  <g transform=\"translate({} {})\">",
        fmt_f32(margin),
        fmt_f32(margin)
    );
    let _ = writeln!(
        svg,
        "    <rect x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" fill=\"#ffffff\" stroke=\"#d0d7de\" stroke-width=\"0.25\"/>",
        fmt_f32(width),
        fmt_f32(height)
    );

    let h_last = host.horizontal_lines.len().saturating_sub(1);
    for (idx, path) in host.horizontal_lines.iter().enumerate() {
        let boundary = idx == 0 || idx == h_last;
        let stroke = if boundary { "#1f2937" } else { "#2563eb" };
        let _ = writeln!(
            svg,
            "    <path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-width=\"{}\"/>",
            path_to_svg_d(path),
            stroke,
            if boundary { "0.45" } else { "0.30" }
        );
    }

    let v_last = host.vertical_lines.len().saturating_sub(1);
    for (idx, path) in host.vertical_lines.iter().enumerate() {
        let boundary = idx == 0 || idx == v_last;
        let stroke = if boundary { "#111827" } else { "#16a34a" };
        let _ = writeln!(
            svg,
            "    <path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-width=\"{}\"/>",
            path_to_svg_d(path),
            stroke,
            if boundary { "0.45" } else { "0.30" }
        );
    }

    let _ = writeln!(
        svg,
        "    <path d=\"{}\" fill=\"none\" stroke=\"#b91c1c\" stroke-width=\"0.55\"/>",
        path_to_svg_d(&host.border_outline)
    );
    let _ = writeln!(svg, "  </g>");
    svg.push_str("</svg>\n");
    svg
}

fn parse_optional_shape_and_output(
    opt_a: Option<String>,
    opt_b: Option<String>,
) -> (BorderFrameShape, Option<String>) {
    match (opt_a, opt_b) {
        (None, None) => (BorderFrameShape::Rectangle, None),
        (Some(a), None) => {
            if let Some(shape) = parse_frame_shape(&a) {
                (shape, None)
            } else {
                (BorderFrameShape::Rectangle, Some(a))
            }
        }
        (Some(a), Some(b)) => {
            let shape = parse_frame_shape(&a).unwrap_or_else(|| {
                eprintln!("invalid frame_shape: {a}");
                std::process::exit(2);
            });
            (shape, Some(b))
        }
        (None, Some(_)) => {
            print_usage_and_exit();
        }
    }
}

fn parse_frame_shape(raw: &str) -> Option<BorderFrameShape> {
    let lower = raw.to_ascii_lowercase();
    match lower.as_str() {
        "rectangle" | "rect" => Some(BorderFrameShape::Rectangle),
        "circle" => Some(BorderFrameShape::Circle),
        "triangle" => Some(BorderFrameShape::RegularPolygon { sides: 3 }),
        "hexagon" => Some(BorderFrameShape::RegularPolygon { sides: 6 }),
        "octagon" => Some(BorderFrameShape::RegularPolygon { sides: 8 }),
        _ => {
            if let Some(rest) = lower.strip_prefix("poly:") {
                let sides = rest.parse::<u8>().ok()?;
                if sides >= 3 {
                    return Some(BorderFrameShape::RegularPolygon { sides });
                }
            }
            None
        }
    }
}

fn frame_shape_label(shape: BorderFrameShape) -> String {
    match shape {
        BorderFrameShape::Rectangle => "rectangle".to_string(),
        BorderFrameShape::Circle => "circle".to_string(),
        BorderFrameShape::RegularPolygon { sides } => format!("poly:{sides}"),
    }
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
        "usage: cargo run -p heddobureika-game --example host_warp_preview -- <cols> <rows> <piece_width_mm> <piece_height_mm> <seed> [frame_shape] [output.svg]"
    );
    eprintln!("frame_shape: rectangle | circle | triangle | hexagon | octagon | poly:N");
    std::process::exit(2);
}
