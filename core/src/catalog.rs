#[derive(Clone, Copy, Debug)]
pub struct PuzzleCatalogEntry {
    pub label: &'static str,
    pub slug: &'static str,
    pub src: &'static str,
    pub width: u32,
    pub height: u32,
}

pub const DEFAULT_PUZZLE_SLUG: &str = "zoe-samurai";
pub const DEFAULT_PUZZLE_SRC: &str = "puzzles/zoe-samurai.jpg";

pub const PUZZLE_CATALOG: &[PuzzleCatalogEntry] = &[
    PuzzleCatalogEntry {
        label: "Zoe Samurai",
        slug: "zoe-samurai",
        src: DEFAULT_PUZZLE_SRC,
        width: 4096,
        height: 2194,
    },
    PuzzleCatalogEntry {
        label: "Zoe Potter",
        slug: "zoe-potter",
        src: "puzzles/zoe-potter.jpg",
        width: 1200,
        height: 840,
    },
    PuzzleCatalogEntry {
        label: "Raora by Noy",
        slug: "raora-by-noy",
        src: "puzzles/raora-by-noy.avif",
        width: 2429,
        height: 2833,
    },
];

pub fn puzzle_by_slug(slug: &str) -> Option<&'static PuzzleCatalogEntry> {
    let trimmed = slug.trim();
    PUZZLE_CATALOG
        .iter()
        .find(|entry| entry.slug.eq_ignore_ascii_case(trimmed))
}

pub fn puzzle_by_src(src: &str) -> Option<&'static PuzzleCatalogEntry> {
    PUZZLE_CATALOG.iter().find(|entry| entry.src == src)
}

pub fn puzzle_by_label(label: &str) -> Option<&'static PuzzleCatalogEntry> {
    let trimmed = label.trim();
    PUZZLE_CATALOG
        .iter()
        .find(|entry| entry.label.eq_ignore_ascii_case(trimmed))
}

pub fn logical_image_size(width: u32, height: u32, max_dim: u32) -> (u32, u32) {
    let max_axis = width.max(height).max(1);
    let logical_max = max_dim.max(1);
    let scale = if max_axis > logical_max {
        logical_max as f64 / max_axis as f64
    } else {
        1.0
    };
    let logical_w = ((width as f64) * scale).round().max(1.0) as u32;
    let logical_h = ((height as f64) * scale).round().max(1.0) as u32;
    (logical_w, logical_h)
}
