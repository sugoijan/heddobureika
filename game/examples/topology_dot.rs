use std::num::NonZeroU32;

use heddobureika_game::{GridTopology, TriangularTessellationTopology};

fn main() {
    let mut args = std::env::args();
    let _bin = args.next();

    let Some(kind) = args.next() else {
        print_usage_and_exit();
    };
    let Some(cols_str) = args.next() else {
        print_usage_and_exit();
    };
    let Some(rows_str) = args.next() else {
        print_usage_and_exit();
    };

    let cols = parse_nz("cols", &cols_str);
    let rows = parse_nz("rows", &rows_str);

    let dot = match kind.as_str() {
        "grid" => GridTopology::new(cols, rows).debug_dot_graph(),
        "tri" | "triangular" => TriangularTessellationTopology::new(cols, rows).debug_dot_graph(),
        _ => {
            eprintln!("unknown topology kind: {kind}");
            print_usage_and_exit();
        }
    };

    println!("{dot}");
}

fn parse_nz(name: &str, raw: &str) -> NonZeroU32 {
    let value = raw.parse::<u32>().unwrap_or_else(|_| {
        eprintln!("invalid {name}: {raw}");
        std::process::exit(2);
    });

    NonZeroU32::new(value).unwrap_or_else(|| {
        eprintln!("{name} must be > 0");
        std::process::exit(2);
    })
}

fn print_usage_and_exit() -> ! {
    eprintln!(
        "usage: cargo run -p heddobureika-game --example topology_dot -- <grid|tri> <cols> <rows>"
    );
    std::process::exit(2);
}
