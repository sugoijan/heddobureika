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
    TriangularTessellationTopology, VoronoiTopology,
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
    /// puzzle image. Topologies whose identity is aspect-independent
    /// (grid, triangular) return the spec unchanged; aspect-dependent
    /// topologies (Voronoi) re-resolve their layout against the new
    /// dimensions. Implementations should be infallible — if anything
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
    rebuild_for_image: identity_rebuild,
};

fn triangular_piece_count_choices(image_w: u32, image_h: u32) -> Vec<PieceCountChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| triangular_resolve_target(*target, image_w, image_h, 0))
        .collect()
}

/// Triangular tessellation has `piece_count = cols * (2*rows + 1)`.
///
/// **Aspect target.** The canonical (un-stretched) mesh occupies roughly
/// `1.5 * cols` units horizontally and `(sqrt(3)/2) * piece_rows` units
/// vertically. `rectangularize_mesh` in `triangular_shape.rs` stretches
/// this canonical mesh to fit the image rectangle, so pieces remain
/// near-equilateral only when the stretch is uniform:
///
/// ```text
/// image_w / (1.5 * cols) ≈ image_h / ((sqrt(3)/2) * piece_rows)
/// → cols / piece_rows ≈ aspect / sqrt(3)
/// ```
///
/// We solve for `cols * piece_rows = target` with the above ratio,
/// giving `piece_rows = sqrt(target * sqrt(3) / aspect)` and
/// `cols = target / piece_rows`. This avoids the "thin sliver" pieces
/// that `cols/piece_rows ≈ aspect` would produce.
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
    let aspect = image_aspect_ratio(image_w, image_h);
    let target_pose_aspect = triangular_target_pose_aspect(aspect);

    // Initial guess: count = cols * piece_rows ≈ piece_rows^2 * target_pose_aspect.
    let piece_rows_f = (target as f32 / target_pose_aspect).sqrt().max(3.0);
    let init_piece_rows = piece_rows_f.round() as u32;
    // piece_rows = 2 * rows + 1 → rows = (piece_rows - 1) / 2.
    let init_rows = ((init_piece_rows.saturating_sub(1)) / 2).max(1);

    // Combined score per candidate: relative-count-error squared plus
    // aspect-error (log-ratio distance) squared, weighted so that a
    // ~5% count miss costs roughly the same as a ~0.16 log-distance
    // aspect miss. Without this, the resolver would lock onto whichever
    // (cols, rows) hit the count exactly and ignore aspect entirely —
    // producing thin pieces for square-ish images.
    let mut best: Option<(u32, u32, u32)> = None; // (cols, rows, count)
    let mut best_score = f32::INFINITY;
    for d_rows in -5i32..=5 {
        let rows = (init_rows as i32 + d_rows).max(1) as u32;
        let piece_rows = 2 * rows + 1;
        // Try cols around the formula-suggested value.
        let cols_centre = ((target as f32 / piece_rows as f32).round() as i32).max(1);
        for d_cols in -5i32..=5 {
            let cols = (cols_centre + d_cols).max(1) as u32;
            let count = cols.saturating_mul(piece_rows);
            if count == 0 {
                continue;
            }
            let rel_err = diff_abs(count, target) as f32 / target as f32;
            let count_term = rel_err.powi(2);
            let aspect_term = aspect_err(cols, piece_rows, target_pose_aspect).powi(2);
            let score = count_term + 0.5 * aspect_term;
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
        spec: TopologySpec::triangular_tessellation(cols, rows),
    })
}

fn diff_abs(a: u32, b: u32) -> u32 {
    if a >= b {
        a - b
    } else {
        b - a
    }
}

/// Distance between the proposed `cols/piece_rows` ratio and a target
/// pose-aspect, in log space (so over- and under-shoot are weighted
/// symmetrically).
fn aspect_err(cols: u32, piece_rows: u32, target_pose_aspect: f32) -> f32 {
    let pose_aspect = cols as f32 / piece_rows as f32;
    (pose_aspect.ln() - target_pose_aspect.ln()).abs()
}

/// Target `cols / piece_rows` ratio that produces near-equilateral
/// interior triangles for the given image aspect. The canonical
/// triangular mesh is `1.5 * cols` wide and `(sqrt(3)/2) * piece_rows`
/// tall; uniform stretch to the image rectangle keeps triangles
/// equilateral when `cols/piece_rows = aspect / sqrt(3)`.
fn triangular_target_pose_aspect(aspect: f32) -> f32 {
    const SQRT_3: f32 = 1.732_050_8;
    (aspect / SQRT_3).max(f32::EPSILON)
}

fn triangular_spec_label(spec: &TopologySpec) -> Option<String> {
    if spec.tag != <TriangularTessellationTopology as SerializableTopology>::TAG {
        return None;
    }
    let topology = TriangularTessellationTopology::read_payload(&spec.payload)?;
    let (cols, rows) = (topology.cols().get(), topology.rows().get());
    Some(format!("{}x{}", cols, rows))
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
    fn triangular_resolve_target_favours_near_equilateral_pieces() {
        // For each (image_w, image_h, target), the resolved (cols, rows)
        // should land near the equilateral-aspect target — i.e. the
        // pose-aspect `cols / piece_rows` should be within a tight band
        // around `aspect / sqrt(3)`. Without the fix this used to come
        // in at ≈ `aspect`, producing thin pieces (e.g. a square image
        // got cols/piece_rows ≈ 1 instead of ≈ 0.577).
        for (w, h, target) in [
            (1000u32, 1000u32, 100u32),
            (1000, 1000, 500),
            (1600, 900, 300),
            (900, 1600, 300),
            (1920, 1080, 1000),
        ] {
            let aspect = w as f32 / h as f32;
            let target_pose_aspect = triangular_target_pose_aspect(aspect);
            let choice = (TRIANGULAR_KIND.resolve_target)(target, w, h, 0).expect("triangular");
            let spec_payload = choice.spec.payload.clone();
            let topology = TriangularTessellationTopology::read_payload(&spec_payload)
                .expect("triangular topology");
            let cols = topology.cols().get() as f32;
            let piece_rows = (2 * topology.rows().get() + 1) as f32;
            let actual_pose_aspect = cols / piece_rows;
            // log-ratio distance must be small (under ~0.30 ≈ 35% off).
            let err = (actual_pose_aspect.ln() - target_pose_aspect.ln()).abs();
            assert!(
                err < 0.30,
                "pose-aspect {} far from equilateral target {} for {}x{} target={}",
                actual_pose_aspect,
                target_pose_aspect,
                w,
                h,
                target
            );
        }
    }

    #[test]
    fn triangular_resolve_target_round_trips_into_buildable_topology() {
        // Mid-sized target across a few aspects.
        for (w, h) in [(800u32, 600u32), (300, 900), (1600, 400)] {
            let choice = (TRIANGULAR_KIND.resolve_target)(300, w, h, 0).expect("triangular");
            let topology = build_topology_from_spec(&choice.spec).expect("buildable");
            assert_eq!(topology.piece_count(), choice.actual_count);
            // Should be roughly near the target.
            let diff = diff_abs(choice.actual_count, 300);
            assert!(
                diff < 100,
                "triangular target=300 actual={} for ({},{})",
                choice.actual_count,
                w,
                h
            );
        }
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
            Some("3x2")
        );
        let vor = TopologySpec::voronoi(80, 1, 1.0);
        assert_eq!(
            (topology_kind_for_tag(&vor.tag).unwrap().spec_label)(&vor).as_deref(),
            Some("80 pieces")
        );
    }
}
