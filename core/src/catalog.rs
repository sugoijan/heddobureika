#[derive(Clone, Copy, Debug)]
pub struct PuzzleCatalogEntry {
    pub label: &'static str,
    pub slug: &'static str,
    pub src: &'static str,
    pub width: u32,
    pub height: u32,
}

include!(concat!(env!("OUT_DIR"), "/puzzle_catalog.rs"));

pub const BLANK_WHITE_SLUG: &str = "blank-white";
pub const BLANK_BLACK_SLUG: &str = "blank-black";

/// Synthetic, solid-color "puzzles" used to stress-test the visual effects
/// (emboss / shadow / etc.) at the extremes. They are deliberately NOT part of
/// [`PUZZLE_CATALOG`] — so they never appear in the catalog listing or the
/// random first-boot pick — but can be selected explicitly and round-trip
/// through snapshots and multiplayer like any other built-in slug. Their `src`
/// is intentionally empty: the renderer synthesizes a flat image from the slug
/// rather than fetching a file.
pub const BLANK_PUZZLES: &[PuzzleCatalogEntry] = &[
    PuzzleCatalogEntry {
        label: "Blank (white)",
        slug: BLANK_WHITE_SLUG,
        src: "",
        width: 1024,
        height: 1024,
    },
    PuzzleCatalogEntry {
        label: "Blank (black)",
        slug: BLANK_BLACK_SLUG,
        src: "",
        width: 1024,
        height: 1024,
    },
];

/// Looks up a blank test puzzle by slug. Returns `None` for ordinary catalog
/// slugs (use [`puzzle_by_slug`] for those).
pub fn blank_puzzle_by_slug(slug: &str) -> Option<&'static PuzzleCatalogEntry> {
    let trimmed = slug.trim();
    BLANK_PUZZLES
        .iter()
        .find(|entry| entry.slug.eq_ignore_ascii_case(trimmed))
}

/// Whether `slug` names one of the synthetic blank test puzzles.
pub fn is_blank_slug(slug: &str) -> bool {
    blank_puzzle_by_slug(slug).is_some()
}

/// Resolves a built-in slug to its catalog or blank entry. Blanks are included
/// so saved games / hash routes that pin a blank slug restore correctly; the
/// catalog listing and random pick still iterate [`PUZZLE_CATALOG`] directly,
/// so blanks stay out of those.
pub fn puzzle_by_slug(slug: &str) -> Option<&'static PuzzleCatalogEntry> {
    let trimmed = slug.trim();
    PUZZLE_CATALOG
        .iter()
        .chain(BLANK_PUZZLES.iter())
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
