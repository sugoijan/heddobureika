//! Registry of selectable topologies with their user-facing
//! piece-count pickers.
//!
//! Each topology declares one `TopologyKind` entry. The UI iterates
//! `available_topologies()` to render the topology dropdown and reads
//! `piece_count_choices` / `resolve_target` to drive the piece-count
//! picker for whatever topology is active. Adding a new topology means
//! implementing it in `game/`, adding a match arm in
//! `build_topology_from_spec`, and adding one entry to `REGISTRY` below
//! — no other app changes.

use heddobureika_game::{
    GridTopology, HexagonalTopology, PuzzleTopology, SerializableTopology, TopologySpec,
    TriDirection, TriangularTessellationTopology, VoronoiTopology,
};

use crate::grid::{
    build_grid_choices, clamp_custom_piece_count, grid_choice_label, nearest_valid_grid,
    DEFAULT_TARGET_COUNT, FALLBACK_GRID, TARGET_PIECE_COUNTS,
};

/// One discrete piece-count option the UI can offer the user. Wraps a
/// fully resolved `TopologySpec` so the caller doesn't have to know how
/// the topology parameterises itself.
#[derive(Clone, Debug, PartialEq)]
pub struct PieceCountChoice {
    /// What the user (or the friendly-count list) asked for.
    pub target_count: u32,
    /// How many pieces the topology actually produces for this choice.
    /// May equal or differ from `target_count` (grid often differs;
    /// Voronoi matches exactly).
    pub actual_count: u32,
    /// User-facing label, e.g. "100 pieces (10x10)" or "100 pieces".
    pub label: String,
    /// Fully resolved spec, ready to feed `build_topology_from_spec`.
    pub spec: TopologySpec,
}

/// Static description of a selectable topology family.
///
/// Function-pointer fields rather than dyn-trait methods because every
/// implementation lives in static memory and the registry is a `const`
/// slice. The trade-off: closures that capture aren't possible, but
/// no topology needs that.
#[derive(Clone, Copy)]
pub struct TopologyKind {
    /// Machine name. Matches `SerializableTopology::TAG`.
    pub tag: &'static str,
    /// User-facing name in the topology dropdown.
    pub display_name: &'static str,
    /// Default piece count when the user first selects this topology.
    pub default_target_count: u32,
    /// `true` for topologies whose layout varies with a seed — the UI
    /// shows a "Regenerate" button to bump the seed.
    pub supports_regenerate: bool,
    /// Discrete user-friendly piece counts the UI offers in a `<select>`.
    pub piece_count_choices: fn(image_w: u32, image_h: u32) -> Vec<PieceCountChoice>,
    /// Resolve an arbitrary user-typed count to the nearest feasible spec.
    pub resolve_target:
        fn(target: u32, image_w: u32, image_h: u32, seed: u32) -> Option<PieceCountChoice>,
    /// Short HUD label for an existing spec (e.g. "10x10"). Used by
    /// debug overlays. Returning `None` falls back to the display name.
    pub spec_label: fn(spec: &TopologySpec) -> Option<String>,
    /// Rebuilds the spec for a fresh image size when the user swaps the
    /// puzzle image. Grid is aspect-independent and returns the spec
    /// unchanged; aspect-dependent topologies (triangular, hexagonal,
    /// Voronoi) re-fit their layout to the new dimensions while preserving
    /// piece count. Implementations should be infallible — if anything
    /// goes wrong, return the input spec untouched so saved games stay
    /// loadable.
    pub rebuild_for_image: fn(spec: &TopologySpec, image_w: u32, image_h: u32) -> TopologySpec,
}

const REGISTRY: &[TopologyKind] = &[GRID_KIND, TRIANGULAR_KIND, HEXAGONAL_KIND, VORONOI_KIND];

/// Returns the static registry of selectable topologies. Order is the
/// order they appear in the UI dropdown.
pub fn available_topologies() -> &'static [TopologyKind] {
    REGISTRY
}

/// Looks up a registry entry by tag. The legacy `voronoi_canary` tag
/// resolves to the same entry as `voronoi`, so old saved games map
/// onto the new kind transparently.
pub fn topology_kind_for_tag(tag: &str) -> Option<&'static TopologyKind> {
    let resolved = if tag == VoronoiTopology::legacy_tag() {
        VoronoiTopology::TAG
    } else {
        tag
    };
    REGISTRY.iter().find(|kind| kind.tag == resolved)
}

fn image_aspect_ratio(image_w: u32, image_h: u32) -> f32 {
    if image_h == 0 {
        1.0
    } else {
        (image_w as f32 / image_h as f32).max(f32::EPSILON)
    }
}

// ---- Grid -----------------------------------------------------------------

const GRID_KIND: TopologyKind = TopologyKind {
    tag: <GridTopology as SerializableTopology>::TAG,
    display_name: "Grid",
    default_target_count: DEFAULT_TARGET_COUNT,
    supports_regenerate: false,
    piece_count_choices: grid_piece_count_choices,
    resolve_target: grid_resolve_target,
    spec_label: grid_spec_label,
    rebuild_for_image: identity_rebuild,
};

fn identity_rebuild(spec: &TopologySpec, _image_w: u32, _image_h: u32) -> TopologySpec {
    spec.clone()
}

fn grid_piece_count_choices(image_w: u32, image_h: u32) -> Vec<PieceCountChoice> {
    let mut choices = build_grid_choices(image_w, image_h);
    if choices.is_empty() {
        choices.push(FALLBACK_GRID);
    }
    choices
        .into_iter()
        .map(|choice| PieceCountChoice {
            target_count: choice.target_count,
            actual_count: choice.actual_count,
            label: grid_choice_label(&choice),
            spec: TopologySpec::grid(choice.cols, choice.rows),
        })
        .collect()
}

fn grid_resolve_target(
    target: u32,
    image_w: u32,
    image_h: u32,
    _seed: u32,
) -> Option<PieceCountChoice> {
    let target = clamp_custom_piece_count(target.max(1));
    let choice = nearest_valid_grid(image_w, image_h, target).unwrap_or(FALLBACK_GRID);
    Some(PieceCountChoice {
        target_count: choice.target_count,
        actual_count: choice.actual_count,
        label: grid_choice_label(&choice),
        spec: TopologySpec::grid(choice.cols, choice.rows),
    })
}

fn grid_spec_label(spec: &TopologySpec) -> Option<String> {
    if spec.tag != <GridTopology as SerializableTopology>::TAG {
        return None;
    }
    let topology = GridTopology::read_payload(&spec.payload)?;
    Some(format!(
        "{}x{}",
        topology.cols().get(),
        topology.rows().get()
    ))
}

// ---- Triangular -----------------------------------------------------------

const TRIANGULAR_KIND: TopologyKind = TopologyKind {
    tag: <TriangularTessellationTopology as SerializableTopology>::TAG,
    display_name: "Triangular tessellation",
    default_target_count: DEFAULT_TARGET_COUNT,
    supports_regenerate: false,
    piece_count_choices: triangular_piece_count_choices,
    resolve_target: triangular_resolve_target,
    spec_label: triangular_spec_label,
    rebuild_for_image: triangular_rebuild_for_image,
};

/// The triangular layout is aspect-dependent (its `(lines, points)` and
/// chosen direction fit the image), so re-resolve for a new image while
/// preserving the current piece count.
fn triangular_rebuild_for_image(spec: &TopologySpec, image_w: u32, image_h: u32) -> TopologySpec {
    let Some(existing) = TriangularTessellationTopology::read_payload(&spec.payload) else {
        return spec.clone();
    };
    let count = existing.piece_count().max(1);
    triangular_resolve_target(count, image_w, image_h, 0)
        .map(|choice| choice.spec)
        .unwrap_or_else(|| spec.clone())
}

fn triangular_piece_count_choices(image_w: u32, image_h: u32) -> Vec<PieceCountChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| triangular_resolve_target(*target, image_w, image_h, 0))
        .collect()
}

/// Resolves a target piece count to the best triangular tessellation. For each
/// guide-line direction it finds the `(lines, points)` that best hits the
/// target count, then auto-picks the direction whose layout deviates least
/// from the image's aspect ratio (lowest letterbox waste — `waste` is a pure,
/// monotonic function of aspect mismatch). Count breaks only near-ties in
/// aspect, so the chosen direction always honours the image's proportions.
fn triangular_resolve_target(
    target: u32,
    image_w: u32,
    image_h: u32,
    _seed: u32,
) -> Option<PieceCountChoice> {
    if target == 0 || image_w == 0 || image_h == 0 {
        return None;
    }
    let target = clamp_custom_piece_count(target.max(1));
    let w = image_w as f32;
    let h = image_h as f32;

    let mut best: Option<(TriDirection, u32, u32, u32, f32)> = None; // dir, lines, points, count, waste
    for &direction in &[TriDirection::Horizontal, TriDirection::Vertical] {
        if let Some((lines, points, count, waste)) = best_triangular_params(direction, target, w, h)
        {
            let take = match best {
                None => true,
                Some((_, _, _, best_count, best_waste)) => {
                    // Lower waste == closer to the image aspect. Only when the
                    // two directions match the aspect about equally (within 1%
                    // of image area) does the closer piece count decide.
                    if (waste - best_waste).abs() > 0.01 {
                        waste < best_waste
                    } else {
                        diff_abs(count, target) < diff_abs(best_count, target)
                    }
                }
            };
            if take {
                best = Some((direction, lines, points, count, waste));
            }
        }
    }

    let (direction, lines, points, actual_count, _waste) = best?;
    let dir_label = match direction {
        TriDirection::Horizontal => "H",
        TriDirection::Vertical => "V",
    };
    Some(PieceCountChoice {
        target_count: target,
        actual_count,
        label: if actual_count == target {
            format!("{target} pieces ({dir_label} {lines}x{points})")
        } else {
            format!("{target} pieces ({dir_label} {lines}x{points}, actual {actual_count})")
        },
        spec: TopologySpec::triangular_tessellation_directed(direction, lines, points),
    })
}

/// Row height of a unit-side equilateral triangle (`√3/2`); mirrors
/// `triangular_lattice::TRI_ROW_HEIGHT`. Kept local so the resolver stays
/// pure arithmetic and never builds a lattice.
const TRI_ROW_HEIGHT: f32 = 0.866_025_4;

/// Searches `(lines, points)` for one direction, returning the best
/// `(lines, points, count, waste)`. The internal score balances count error
/// and wasted (letterbox) area so the representative hits the target count
/// well; `waste` is reported separately so the caller can pick the direction
/// that deviates least from the image aspect. Uses the lattice's closed-form
/// `count` and `extent` (NOT a built lattice) — this runs on every UI render,
/// so it must stay cheap arithmetic like the grid resolver.
fn best_triangular_params(
    direction: TriDirection,
    target: u32,
    w: f32,
    h: f32,
) -> Option<(u32, u32, u32, f32)> {
    // Closed forms (see `triangular_lattice`): for `(lines, points)`,
    //   count  = lines·(2·points − 1)
    //   extent = (points−1, lines·h)   [Horizontal]
    //          = (lines·h, points−1)   [Vertical]
    // with h = √3/2. The equilateral aspect wants points−1 ≈ lines·h·aspect
    // (and the transpose for vertical), giving lines ≈ √(target/(√3·a)).
    const SQRT_3: f32 = 1.732_050_8;
    let aspect = (w / h).max(f32::EPSILON);
    let a = match direction {
        TriDirection::Horizontal => aspect,
        TriDirection::Vertical => 1.0 / aspect,
    };
    let lines_est = (target as f32 / (SQRT_3 * a).max(0.05))
        .sqrt()
        .round()
        .max(1.0) as i32;

    let mut best: Option<(u32, u32, u32, f32, f32)> = None; // lines, points, count, waste, score
    for d_lines in -3i32..=3 {
        let lines = (lines_est + d_lines).max(1) as u32;
        let points_est = (target as f32 / lines as f32 / 2.0).round().max(2.0) as i32;
        for d_points in -3i32..=4 {
            let points = (points_est + d_points).max(2) as u32;
            let count = lines * (2 * points - 1);
            if count == 0 {
                continue;
            }
            let (ex, ey) = match direction {
                TriDirection::Horizontal => ((points - 1) as f32, lines as f32 * TRI_ROW_HEIGHT),
                TriDirection::Vertical => (lines as f32 * TRI_ROW_HEIGHT, (points - 1) as f32),
            };
            if ex <= 0.0 || ey <= 0.0 {
                continue;
            }
            let scale = (w / ex).min(h / ey);
            let covered = (ex * scale) * (ey * scale);
            let waste = 1.0 - (covered / (w * h)).clamp(0.0, 1.0);
            let count_rel = diff_abs(count, target) as f32 / target as f32;
            let score = count_rel.powi(2) + 2.0 * waste.powi(2);
            if best.map(|(_, _, _, _, s)| score < s).unwrap_or(true) {
                best = Some((lines, points, count, waste, score));
            }
        }
    }
    best.map(|(lines, points, count, waste, _score)| (lines, points, count, waste))
}

fn diff_abs(a: u32, b: u32) -> u32 {
    if a >= b {
        a - b
    } else {
        b - a
    }
}

fn triangular_spec_label(spec: &TopologySpec) -> Option<String> {
    if spec.tag != <TriangularTessellationTopology as SerializableTopology>::TAG {
        return None;
    }
    let topology = TriangularTessellationTopology::read_payload(&spec.payload)?;
    let dir = match topology.direction() {
        TriDirection::Horizontal => "H",
        TriDirection::Vertical => "V",
    };
    Some(format!(
        "{} {}x{}",
        dir,
        topology.lines().get(),
        topology.points().get()
    ))
}

// ---- Hexagonal ------------------------------------------------------------

const HEXAGONAL_KIND: TopologyKind = TopologyKind {
    tag: <HexagonalTopology as SerializableTopology>::TAG,
    display_name: "Hexagonal tiling",
    default_target_count: DEFAULT_TARGET_COUNT,
    supports_regenerate: false,
    piece_count_choices: hexagonal_piece_count_choices,
    resolve_target: hexagonal_resolve_target,
    spec_label: hexagonal_spec_label,
    rebuild_for_image: hexagonal_rebuild_for_image,
};

fn hexagonal_rebuild_for_image(spec: &TopologySpec, image_w: u32, image_h: u32) -> TopologySpec {
    // The hex spec stores its `(C, R)` AND the image aspect (so the
    // outer-column stretch can be computed at construction). When the
    // user swaps images, re-derive aspect from the new dimensions and
    // re-emit the spec; `(C, R)` carry over unchanged.
    let Some(existing) = HexagonalTopology::read_payload(&spec.payload) else {
        return spec.clone();
    };
    let aspect = image_aspect_ratio(image_w, image_h);
    TopologySpec::hexagonal(existing.cols().get(), existing.rows().get(), aspect)
}

fn hexagonal_piece_count_choices(image_w: u32, image_h: u32) -> Vec<PieceCountChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| hexagonal_resolve_target(*target, image_w, image_h, 0))
        .collect()
}

/// Hexagonal tiling has `piece_count = (2 * R * C - C + 1) / 2` where
/// `C` (cols) is required odd. The pose-unit extent is `((C-1) * 1.5,
/// (R-1) * √3)`. For near-regular interior hexes the image stretch
/// should be uniform:
///
/// ```text
/// image_w / ((C-1) * 1.5) ≈ image_h / ((R-1) * √3)
/// → (C - 1) / (R - 1) ≈ aspect * √3 / 1.5 ≈ 1.1547 * aspect
/// ```
///
/// We pick the (cols, rows) pair (C odd, ≥ 3; R ≥ 1) that minimises a
/// weighted blend of count-error and aspect-error, just like the
/// triangular resolver.
fn hexagonal_resolve_target(
    target: u32,
    image_w: u32,
    image_h: u32,
    _seed: u32,
) -> Option<PieceCountChoice> {
    if target == 0 || image_w == 0 || image_h == 0 {
        return None;
    }
    let target = clamp_custom_piece_count(target.max(1));
    let aspect = image_aspect_ratio(image_w, image_h);
    let target_pose_aspect = hexagonal_target_pose_aspect(aspect);

    // Approximate from the two equations:
    //   (C-1)/(R-1) ≈ target_pose_aspect
    //   target ≈ R*C (for large R, C; the boundary correction is small)
    // ⇒ R ≈ √(target / target_pose_aspect)
    let init_rows_f = (target as f32 / target_pose_aspect.max(1.0e-3))
        .sqrt()
        .max(2.0);
    let init_rows = init_rows_f.round() as u32;
    let init_cols_f = init_rows_f * target_pose_aspect + 1.0;
    let mut init_cols = init_cols_f.round() as u32;
    if init_cols % 2 == 0 {
        init_cols += 1;
    }
    let init_cols = init_cols.max(3);

    let mut best: Option<(u32, u32, u32)> = None;
    let mut best_score = f32::INFINITY;
    for d_rows in -5i32..=5 {
        let rows = (init_rows as i32 + d_rows).max(1) as u32;
        for d_cols in -5i32..=5 {
            // Keep cols odd and ≥ 3.
            let raw_cols = init_cols as i32 + d_cols * 2;
            let cols = raw_cols.max(3) as u32;
            if cols % 2 == 0 {
                continue;
            }
            let count = (cols * (2 * rows - 1) + 1) / 2;
            if count == 0 {
                continue;
            }
            let rel_err = diff_abs(count, target) as f32 / target as f32;
            let pose_aspect = (cols as f32 - 1.0) / (rows as f32 - 1.0).max(1.0);
            // Direct "shape distortion" metric, in pose units. Two
            // failure modes — both bad, both penalised here:
            //
            // (a) Wider-edges mode (pose_aspect <= target): inner
            //     hexes regular but outer columns visibly stretched
            //     by `outer_gap_pose - 1.5` pose units. For the user's
            //     50@1200x700 case, 9x6 gives `outer_gap = 2.92` —
            //     a ~95% stretch over inner spacing.
            //
            // (b) Uniform fallback (pose_aspect > target): no outer
            //     stretch but inner hexes anisotropically stretched.
            //     Distortion measured by how far `pose_aspect` is
            //     above `target_pose_aspect` (= log ratio).
            //
            // We pick the dominant distortion of the two modes (a
            // candidate only suffers one) and weight it as a squared
            // penalty. This shifts the resolver's preference toward
            // (C, R) pairs whose layout looks clean even at the cost
            // of some piece-count deviation, which matches how the
            // user perceives "good shapes" vs "stretched edges".
            let shape_distortion = if pose_aspect <= target_pose_aspect {
                // outer-gap stretch in pose units (= delta_pose).
                const SQRT_3: f32 = 1.732_050_8;
                let outer_gap =
                    (aspect * (rows as f32 - 1.0) * SQRT_3 - (cols as f32 - 3.0) * 1.5) * 0.5;
                (outer_gap - 1.5).max(0.0)
            } else {
                // Anisotropy: log-ratio of pose_aspect vs target.
                // Multiplied by 10 to bring its scale roughly in line
                // with outer-gap stretch (a 1.5-unit gap is "large";
                // a 0.15 log-ratio anisotropy is also "large").
                (pose_aspect.ln() - target_pose_aspect.ln()).abs() * 10.0
            };
            let score = rel_err.powi(2) + 0.15 * shape_distortion.powi(2);
            if score < best_score {
                best_score = score;
                best = Some((cols, rows, count));
            }
        }
    }

    let (cols, rows, actual_count) = best?;
    Some(PieceCountChoice {
        target_count: target,
        actual_count,
        label: if actual_count == target {
            format!("{} pieces ({}x{})", target, cols, rows)
        } else {
            format!(
                "{} pieces ({}x{}, actual {})",
                target, cols, rows, actual_count
            )
        },
        spec: TopologySpec::hexagonal(cols, rows, aspect),
    })
}

/// Target `(C-1) / (R-1)` ratio that produces near-regular interior
/// hexes for the given image aspect.
fn hexagonal_target_pose_aspect(aspect: f32) -> f32 {
    const SQRT_3_OVER_ONE_POINT_FIVE: f32 = 1.154_700_5; // √3 / 1.5
    (aspect * SQRT_3_OVER_ONE_POINT_FIVE).max(f32::EPSILON)
}

fn hexagonal_spec_label(spec: &TopologySpec) -> Option<String> {
    if spec.tag != <HexagonalTopology as SerializableTopology>::TAG {
        return None;
    }
    let topology = HexagonalTopology::read_payload(&spec.payload)?;
    Some(format!(
        "{}x{}",
        topology.cols().get(),
        topology.rows().get()
    ))
}

// ---- Voronoi --------------------------------------------------------------

const VORONOI_KIND: TopologyKind = TopologyKind {
    tag: <VoronoiTopology as SerializableTopology>::TAG,
    display_name: "Voronoi",
    default_target_count: DEFAULT_TARGET_COUNT,
    supports_regenerate: true,
    piece_count_choices: voronoi_piece_count_choices,
    resolve_target: voronoi_resolve_target,
    spec_label: voronoi_spec_label,
    rebuild_for_image: voronoi_rebuild_for_image,
};

fn voronoi_rebuild_for_image(spec: &TopologySpec, image_w: u32, image_h: u32) -> TopologySpec {
    let Some(existing) = VoronoiTopology::read_payload(&spec.payload) else {
        // Try the pre-rename canary tag — saved games against the older
        // format still round-trip into a valid VoronoiTopology by way
        // of `read_legacy_payload`.
        if spec.tag == VoronoiTopology::legacy_tag() {
            if let Some(legacy) = VoronoiTopology::read_legacy_payload(&spec.payload) {
                let aspect = image_aspect_ratio(image_w, image_h);
                return TopologySpec::voronoi(legacy.piece_count(), legacy.seed(), aspect);
            }
        }
        return spec.clone();
    };
    let aspect = image_aspect_ratio(image_w, image_h);
    TopologySpec::voronoi(existing.piece_count(), existing.seed(), aspect)
}

fn voronoi_piece_count_choices(image_w: u32, image_h: u32) -> Vec<PieceCountChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .map(|target| {
            voronoi_resolve_target(*target, image_w, image_h, 1)
                .expect("voronoi can hit any clamped target")
        })
        .collect()
}

fn voronoi_resolve_target(
    target: u32,
    image_w: u32,
    image_h: u32,
    seed: u32,
) -> Option<PieceCountChoice> {
    let target = clamp_custom_piece_count(target.max(1));
    let aspect = image_aspect_ratio(image_w, image_h);
    Some(PieceCountChoice {
        target_count: target,
        actual_count: target,
        label: format!("{} pieces", target),
        spec: TopologySpec::voronoi(target, seed, aspect),
    })
}

fn voronoi_spec_label(spec: &TopologySpec) -> Option<String> {
    if spec.tag != <VoronoiTopology as SerializableTopology>::TAG {
        return None;
    }
    let topology = VoronoiTopology::read_payload(&spec.payload)?;
    Some(format!("{} pieces", topology.piece_count()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use heddobureika_game::{build_topology_from_spec, PuzzleTopology};

    #[test]
    fn registry_contains_all_kinds() {
        let tags: Vec<&str> = available_topologies().iter().map(|k| k.tag).collect();
        assert_eq!(
            tags,
            vec!["grid", "triangular_tessellation", "hexagonal", "voronoi"]
        );
    }

    #[test]
    fn hexagonal_resolve_target_returns_odd_cols() {
        for (w, h, target) in [
            (1000u32, 1000u32, 100u32),
            (1600, 900, 200),
            (900, 1600, 200),
            (1920, 1080, 500),
        ] {
            let choice = (HEXAGONAL_KIND.resolve_target)(target, w, h, 0).expect("hex");
            let topology = HexagonalTopology::read_payload(&choice.spec.payload).expect("read");
            assert_eq!(topology.cols().get() % 2, 1, "cols must be odd");
        }
    }

    #[test]
    fn legacy_voronoi_canary_tag_maps_to_voronoi() {
        let kind = topology_kind_for_tag("voronoi_canary").expect("voronoi");
        assert_eq!(kind.tag, "voronoi");
    }

    #[test]
    fn each_kind_resolves_default_target_into_buildable_topology() {
        for kind in available_topologies() {
            let choice = (kind.resolve_target)(kind.default_target_count, 1600, 900, 1)
                .unwrap_or_else(|| panic!("{} resolve_target", kind.tag));
            let topology = build_topology_from_spec(&choice.spec)
                .unwrap_or_else(|| panic!("{} build_topology_from_spec", kind.tag));
            assert!(topology.piece_count() > 0, "{} produced 0 pieces", kind.tag);
        }
    }

    #[test]
    fn hexagonal_resolve_target_keeps_outer_stretch_bounded() {
        // User-reported case: 1200x700 image at 50 pieces. The old
        // resolver picked `9x6` because it hit the count exactly,
        // but the resulting `outer_gap_pose = 2.92` made the
        // outer-column pieces visibly ~2x wider than inner ones. The
        // shape-distortion-aware scoring trades a bit of count
        // accuracy for a much cleaner layout (lower outer stretch or
        // mild uniform-fallback anisotropy).
        let choice = (HEXAGONAL_KIND.resolve_target)(50, 1200, 700, 0).expect("hex");
        let topology = HexagonalTopology::read_payload(&choice.spec.payload).expect("read");
        let outer_stretch = (topology.outer_gap_pose() - 1.5).max(0.0);
        assert!(
            outer_stretch < 1.0,
            "outer stretch should stay below 1 pose unit; got {} for {}x{} ({})",
            outer_stretch,
            topology.cols().get(),
            topology.rows().get(),
            choice.actual_count,
        );
    }

    #[test]
    fn hexagonal_resolve_target_handles_high_piece_counts() {
        // User reported targets >= 300 silently failing. Exercise the
        // full registry → spec → topology path for each curated count
        // at a typical landscape aspect.
        for &target in &[150u32, 300, 500, 750, 1000, 1500, 2000, 3000, 5000] {
            let choice = (HEXAGONAL_KIND.resolve_target)(target, 1200, 700, 0)
                .unwrap_or_else(|| panic!("resolve target={target}"));
            let topology = build_topology_from_spec(&choice.spec)
                .unwrap_or_else(|| panic!("build_topology_from_spec target={target}"));
            assert!(
                topology.piece_count() > 0,
                "target {target} produced 0 pieces"
            );
        }
    }

    #[test]
    fn voronoi_resolve_target_hits_exact_count() {
        let choice = (VORONOI_KIND.resolve_target)(257, 1280, 720, 1).expect("voronoi");
        assert_eq!(choice.actual_count, 257);
    }

    #[test]
    fn grid_piece_count_choices_are_non_empty() {
        let choices = (GRID_KIND.piece_count_choices)(1000, 800);
        assert!(!choices.is_empty());
        for choice in &choices {
            let topology = build_topology_from_spec(&choice.spec).expect("buildable");
            assert_eq!(topology.piece_count(), choice.actual_count);
        }
    }

    #[test]
    fn triangular_resolve_target_low_waste_across_aspects() {
        // The chosen lattice should fill nearly all of the image (small
        // letterbox/crop), confirming the resolver picked a direction +
        // (lines, points) whose natural extent hugs the image aspect.
        for (w, h, target) in [
            (1000u32, 1000u32, 100u32),
            (1000, 1000, 500),
            (1600, 900, 300),
            (900, 1600, 300),
            (1920, 1080, 1000),
            (4096, 2194, 50),
        ] {
            let choice = (TRIANGULAR_KIND.resolve_target)(target, w, h, 0).expect("triangular");
            let topology = TriangularTessellationTopology::read_payload(&choice.spec.payload)
                .expect("triangular topology");
            let (ex, ey) = topology.pose_extent();
            let scale = (w as f32 / ex).min(h as f32 / ey);
            let covered = (ex * scale) * (ey * scale) / (w as f32 * h as f32);
            assert!(
                covered > 0.88,
                "triangular {w}x{h} target={target}: only covers {:.3} of the image",
                covered
            );
        }
    }

    #[test]
    fn triangular_resolve_target_round_trips_into_buildable_topology() {
        for (w, h) in [(800u32, 600u32), (300, 900), (1600, 400)] {
            let choice = (TRIANGULAR_KIND.resolve_target)(300, w, h, 0).expect("triangular");
            let topology = build_topology_from_spec(&choice.spec).expect("buildable");
            assert_eq!(topology.piece_count(), choice.actual_count);
            let diff = diff_abs(choice.actual_count, 300);
            assert!(
                diff < 120,
                "triangular target=300 actual={} for ({},{})",
                choice.actual_count,
                w,
                h
            );
        }
    }

    #[test]
    fn triangular_resolve_picks_min_aspect_deviation_direction() {
        // The auto-pick must choose the guide-line direction whose layout
        // deviates least from the image aspect (lowest letterbox waste),
        // within the near-tie margin where piece count decides.
        for (w, h, target) in [
            (1600u32, 900u32, 150u32),
            (900, 1600, 150),
            (1000, 1000, 200),
            (2000, 700, 80),
            (700, 2000, 80),
        ] {
            let (wf, hf) = (w as f32, h as f32);
            let (_, _, _, h_waste) =
                best_triangular_params(TriDirection::Horizontal, target, wf, hf).expect("h");
            let (_, _, _, v_waste) =
                best_triangular_params(TriDirection::Vertical, target, wf, hf).expect("v");
            let min_waste = h_waste.min(v_waste);

            let choice = (TRIANGULAR_KIND.resolve_target)(target, w, h, 0).expect("triangular");
            let topo =
                TriangularTessellationTopology::read_payload(&choice.spec.payload).expect("read");
            let chosen_waste = match topo.direction() {
                TriDirection::Horizontal => h_waste,
                TriDirection::Vertical => v_waste,
            };
            assert!(
                chosen_waste <= min_waste + 0.01 + 1.0e-4,
                "{w}x{h} target={target}: chose {:?} (waste {chosen_waste:.4}), \
                 min available {min_waste:.4}",
                topo.direction()
            );
        }
    }

    #[test]
    fn triangular_rebuild_refits_layout_when_aspect_flips() {
        // Switching art from landscape to portrait must re-fit the lattice to
        // the new aspect (the bug: the old spec was carried over and covered
        // the new image poorly). Resolve for landscape, then rebuild for
        // portrait and confirm the refit covers the portrait image well —
        // which the carried-over landscape spec would not.
        let landscape = (TRIANGULAR_KIND.resolve_target)(120, 1600, 900, 0).expect("landscape");

        let coverage = |spec: &TopologySpec, w: u32, h: u32| -> f32 {
            let topo = TriangularTessellationTopology::read_payload(&spec.payload).expect("read");
            let (ex, ey) = topo.pose_extent();
            let scale = (w as f32 / ex).min(h as f32 / ey);
            (ex * scale) * (ey * scale) / (w as f32 * h as f32)
        };

        let (pw, ph) = (900u32, 1600u32);
        let carried_over = coverage(&landscape.spec, pw, ph);
        let refit_spec = (TRIANGULAR_KIND.rebuild_for_image)(&landscape.spec, pw, ph);
        let refit = coverage(&refit_spec, pw, ph);

        assert!(
            refit > 0.85,
            "refit should hug the portrait image; covered {refit:.3}"
        );
        assert!(
            refit > carried_over + 0.1,
            "refit ({refit:.3}) should cover much better than the carried-over \
             landscape spec ({carried_over:.3})"
        );
        // Piece count is preserved across the refit (within the discrete grid).
        let before = build_topology_from_spec(&landscape.spec)
            .unwrap()
            .piece_count();
        let after = build_topology_from_spec(&refit_spec).unwrap().piece_count();
        assert!(
            diff_abs(before, after) <= before / 5 + 2,
            "piece count drifted too much on refit: {before} -> {after}"
        );
    }

    #[test]
    fn spec_labels_route_to_the_right_kind() {
        let grid = TopologySpec::grid(5, 4);
        assert_eq!(
            (topology_kind_for_tag(&grid.tag).unwrap().spec_label)(&grid).as_deref(),
            Some("5x4")
        );
        let tri = TopologySpec::triangular_tessellation(3, 2);
        assert_eq!(
            (topology_kind_for_tag(&tri.tag).unwrap().spec_label)(&tri).as_deref(),
            Some("H 3x2")
        );
        let vor = TopologySpec::voronoi(80, 1, 1.0);
        assert_eq!(
            (topology_kind_for_tag(&vor.tag).unwrap().spec_label)(&vor).as_deref(),
            Some("80 pieces")
        );
    }
}
