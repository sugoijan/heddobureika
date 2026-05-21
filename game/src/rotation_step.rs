//! Shared step-rotation solver for piece and group symmetry sets.

use crate::units::AngleDeg;

const SYMMETRY_EPSILON_DEG: f32 = 1.0e-3;

/// Strength of a piece's `symmetry_angles` for the purpose of group-level
/// rotation aggregation.
///
/// `Strong` angles are real geometric symmetries (e.g. a grid square is
/// 4-fold symmetric, a Voronoi frame piece is treated as such by
/// convention) — when present in a group they constrain the group's
/// rotation steps directly. `Weak` angles are heuristic per-piece
/// suggestions (e.g. edge-angle equalities of an irregular Voronoi
/// interior cell) — in a mixed group they are dropped (so a corner piece
/// joining doesn't suddenly collapse the rotation set to the correct
/// answer), and in an all-`Weak` group they're aggregated into a
/// well-spread subset (see [`group_symmetry_angles`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymmetryStrength {
    Strong,
    Weak,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StepDirection {
    Cw,
    Ccw,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StepRotationTarget {
    pub canonical: AngleDeg,
    pub directional: AngleDeg,
}

/// Canonicalizes a symmetry-angle list into sorted unique angles in `[0, 360)`.
///
/// - `0` is always injected.
/// - input angles may be unsorted, duplicated, or out-of-range.
pub fn canonicalize_symmetry_angles(raw: &[AngleDeg]) -> Vec<AngleDeg> {
    let mut values = Vec::with_capacity(raw.len() + 1);
    values.push(0.0);
    for angle in raw {
        values.push(normalize_for_symmetry(angle.as_degrees_f32()));
    }
    values.sort_by(|a, b| a.total_cmp(b));

    let mut dedup = Vec::with_capacity(values.len());
    for value in values {
        if dedup
            .last()
            .map(|last: &f32| (value - *last).abs() <= SYMMETRY_EPSILON_DEG)
            .unwrap_or(false)
        {
            continue;
        }
        dedup.push(value);
    }

    if dedup
        .first()
        .copied()
        .map(|value| value.abs() > SYMMETRY_EPSILON_DEG)
        .unwrap_or(true)
    {
        dedup.insert(0, 0.0);
    }

    dedup
        .into_iter()
        .map(|value| {
            AngleDeg::try_new(value).expect("canonicalized symmetry angle should be finite")
        })
        .collect()
}

/// Intersects two symmetry-angle sets.
///
/// Inputs may be non-canonical; the result is canonicalized and includes `0`.
pub fn intersect_symmetry_angles(lhs: &[AngleDeg], rhs: &[AngleDeg]) -> Vec<AngleDeg> {
    let left = canonicalize_symmetry_angles(lhs);
    let right = canonicalize_symmetry_angles(rhs);

    let left_values = left
        .iter()
        .map(|angle| angle.as_degrees_f32())
        .collect::<Vec<_>>();
    let right_values = right
        .iter()
        .map(|angle| angle.as_degrees_f32())
        .collect::<Vec<_>>();

    let mut out_values = Vec::new();
    let mut i = 0usize;
    let mut j = 0usize;

    while i < left_values.len() && j < right_values.len() {
        let a = left_values[i];
        let b = right_values[j];
        if (a - b).abs() <= SYMMETRY_EPSILON_DEG {
            out_values.push(a);
            i += 1;
            j += 1;
        } else if a < b {
            i += 1;
        } else {
            j += 1;
        }
    }

    if out_values.is_empty() {
        out_values.push(0.0);
    }
    if !out_values
        .iter()
        .any(|value| value.abs() <= SYMMETRY_EPSILON_DEG)
    {
        out_values.insert(0, 0.0);
    }

    out_values
        .into_iter()
        .map(|value| AngleDeg::try_new(value).expect("intersection angle should be finite"))
        .collect()
}

pub fn next_step_target(
    allowed_angles: &[AngleDeg],
    current: AngleDeg,
    rotation_snap_tolerance: AngleDeg,
    direction: StepDirection,
) -> StepRotationTarget {
    let canonical_allowed = canonicalize_symmetry_angles(allowed_angles);
    let allowed_values = canonical_allowed
        .iter()
        .map(|angle| angle.as_degrees_f32())
        .collect::<Vec<_>>();

    let current_raw = current.as_degrees_f32();
    let current_norm = normalize_degrees(current_raw);
    let half_tolerance = rotation_snap_tolerance.as_degrees_f32().abs() * 0.5;

    let (nearest, nearest_diff) = nearest_allowed_angle(&allowed_values, current_norm);
    let base = if nearest_diff <= half_tolerance {
        unwrap_nearest_congruent(nearest, current_raw)
    } else {
        current_raw
    };

    let directional_value = match direction {
        StepDirection::Cw => next_cw_directional(&allowed_values, base),
        StepDirection::Ccw => next_ccw_directional(&allowed_values, base),
    };
    let canonical_value = normalize_degrees(directional_value);

    StepRotationTarget {
        canonical: AngleDeg::try_new(canonical_value).expect("canonical target should be finite"),
        directional: AngleDeg::try_new(directional_value)
            .expect("directional target should be finite"),
    }
}

pub fn next_step_canonical(
    allowed_angles: &[AngleDeg],
    current: AngleDeg,
    rotation_snap_tolerance: AngleDeg,
    direction: StepDirection,
) -> AngleDeg {
    next_step_target(allowed_angles, current, rotation_snap_tolerance, direction).canonical
}

fn nearest_allowed_angle(allowed: &[f32], current_norm: f32) -> (f32, f32) {
    let mut nearest = 0.0_f32;
    let mut nearest_diff = f32::INFINITY;
    for &allowed_angle in allowed {
        let diff = shortest_angular_distance(current_norm, allowed_angle).abs();
        if diff < nearest_diff {
            nearest = allowed_angle;
            nearest_diff = diff;
        }
    }
    (nearest, nearest_diff)
}

fn next_cw_directional(allowed: &[f32], base: f32) -> f32 {
    let mut best = f32::INFINITY;
    for &angle in allowed {
        let turns = ((base - angle) / 360.0).floor() + 1.0;
        let candidate = angle + turns * 360.0;
        if candidate > base + SYMMETRY_EPSILON_DEG && candidate < best {
            best = candidate;
        }
    }
    if best.is_finite() {
        best
    } else {
        base + 360.0
    }
}

fn next_ccw_directional(allowed: &[f32], base: f32) -> f32 {
    let mut best = f32::NEG_INFINITY;
    for &angle in allowed {
        let turns = ((base - angle) / 360.0).ceil() - 1.0;
        let candidate = angle + turns * 360.0;
        if candidate < base - SYMMETRY_EPSILON_DEG && candidate > best {
            best = candidate;
        }
    }
    if best.is_finite() {
        best
    } else {
        base - 360.0
    }
}

fn normalize_for_symmetry(value: f32) -> f32 {
    let normalized = normalize_degrees(value);
    if normalized <= SYMMETRY_EPSILON_DEG || (360.0 - normalized) <= SYMMETRY_EPSILON_DEG {
        0.0
    } else {
        normalized
    }
}

fn normalize_degrees(mut angle: f32) -> f32 {
    angle %= 360.0;
    if angle < 0.0 {
        angle += 360.0;
    }
    angle
}

fn shortest_angular_distance(from: f32, to: f32) -> f32 {
    let mut diff = normalize_degrees(to - from);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

fn unwrap_nearest_congruent(canonical: f32, reference: f32) -> f32 {
    let turns = ((reference - canonical) / 360.0).round();
    canonical + turns * 360.0
}

/// Combines per-piece symmetry angle sets into a group's effective rotation
/// steps.
///
/// Rules:
/// - Empty input → canonical `[0°]`.
/// - Singleton group → pass the piece's raw `symmetry_angles` straight through
///   (canonicalized). No strong/weak filtering for solo pieces.
/// - At least one [`SymmetryStrength::Strong`] member → intersect the strong
///   members' angle sets; weak members are dropped.
/// - All members [`SymmetryStrength::Weak`] → drop `0°` from each set,
///   compute `N = round(mean(#non-trivial angles))`, take the union of
///   non-trivial angles, then greedily pick `N` of them maximizing the
///   minimum circular distance to all already-selected angles (with `0°`
///   anchored). Canonicalization re-adds `0°` at the end.
///
/// Corollary of the all-weak rule: if every weak piece exposes the same
/// non-trivial set `S`, the result is `S` (plus the implicit `0°`).
pub fn group_symmetry_angles(pieces: &[(&[AngleDeg], SymmetryStrength)]) -> Vec<AngleDeg> {
    match pieces.len() {
        0 => return canonicalize_symmetry_angles(&[]),
        1 => return canonicalize_symmetry_angles(pieces[0].0),
        _ => {}
    }

    let strong_count = pieces
        .iter()
        .filter(|(_, s)| *s == SymmetryStrength::Strong)
        .count();
    if strong_count > 0 {
        let mut iter = pieces
            .iter()
            .filter(|(_, s)| *s == SymmetryStrength::Strong);
        let (first, _) = iter.next().expect("strong_count > 0");
        let mut result = canonicalize_symmetry_angles(first);
        for (angles, _) in iter {
            result = intersect_symmetry_angles(&result, angles);
        }
        return result;
    }

    aggregate_weak_symmetry(pieces)
}

fn aggregate_weak_symmetry(pieces: &[(&[AngleDeg], SymmetryStrength)]) -> Vec<AngleDeg> {
    // Per-piece non-trivial angles (0° stripped, normalized to [0, 360)).
    let mut total_non_trivial: usize = 0;
    let mut union: Vec<f32> = Vec::new();
    for (angles, _) in pieces {
        let mut count_here = 0usize;
        for angle in angles.iter() {
            let value = normalize_for_symmetry(angle.as_degrees_f32());
            if value.abs() <= SYMMETRY_EPSILON_DEG {
                continue;
            }
            count_here += 1;
            union.push(value);
        }
        total_non_trivial += count_here;
    }

    if total_non_trivial == 0 {
        return canonicalize_symmetry_angles(&[]);
    }

    // N = round(mean(#non-trivial per piece)).
    let mean = total_non_trivial as f32 / pieces.len() as f32;
    let n = (mean + 0.5).floor() as usize;
    if n == 0 {
        return canonicalize_symmetry_angles(&[]);
    }

    union.sort_by(|a, b| a.total_cmp(b));
    let mut deduped: Vec<f32> = Vec::with_capacity(union.len());
    for value in union {
        if deduped
            .last()
            .map(|last| (value - *last).abs() <= SYMMETRY_EPSILON_DEG)
            .unwrap_or(false)
        {
            continue;
        }
        deduped.push(value);
    }

    // Greedy max-min-gap selection with 0° anchored.
    let mut selected: Vec<f32> = vec![0.0];
    let mut available: Vec<f32> = deduped;
    let pick = n.min(available.len());
    for _ in 0..pick {
        let mut best_idx: Option<usize> = None;
        let mut best_gap = f32::NEG_INFINITY;
        for (idx, candidate) in available.iter().enumerate() {
            let min_gap = selected
                .iter()
                .map(|s| circular_angle_distance(*candidate, *s))
                .fold(f32::INFINITY, f32::min);
            if min_gap > best_gap + SYMMETRY_EPSILON_DEG {
                best_gap = min_gap;
                best_idx = Some(idx);
            }
        }
        let Some(idx) = best_idx else {
            break;
        };
        let chosen = available.remove(idx);
        selected.push(chosen);
    }

    let angles: Vec<AngleDeg> = selected
        .into_iter()
        .filter(|value| value.abs() > SYMMETRY_EPSILON_DEG)
        .filter_map(AngleDeg::try_new)
        .collect();
    canonicalize_symmetry_angles(&angles)
}

fn circular_angle_distance(a: f32, b: f32) -> f32 {
    let mut diff = (a - b).abs() % 360.0;
    if diff > 180.0 {
        diff = 360.0 - diff;
    }
    diff
}

#[cfg(test)]
mod tests {
    use super::*;

    fn deg(values: &[f32]) -> Vec<AngleDeg> {
        values
            .iter()
            .map(|v| AngleDeg::try_new(*v).expect("finite"))
            .collect()
    }

    fn values(angles: &[AngleDeg]) -> Vec<f32> {
        let mut out: Vec<f32> = angles
            .iter()
            .map(|angle| (angle.as_degrees_f32() * 10.0).round() / 10.0)
            .collect();
        out.sort_by(|a, b| a.total_cmp(b));
        out
    }

    #[test]
    fn group_symmetry_singleton_passes_through() {
        let a = deg(&[90.0, 180.0, 270.0]);
        let pieces = [(a.as_slice(), SymmetryStrength::Strong)];
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 90.0, 180.0, 270.0]);

        let b = deg(&[37.0, 154.0]);
        let pieces = [(b.as_slice(), SymmetryStrength::Weak)];
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 37.0, 154.0]);
    }

    #[test]
    fn group_symmetry_strong_dominates_weak() {
        let strong = deg(&[90.0, 180.0, 270.0]);
        let weak = deg(&[21.0, 87.0, 174.0, 218.0]);
        let pieces = [
            (strong.as_slice(), SymmetryStrength::Strong),
            (weak.as_slice(), SymmetryStrength::Weak),
        ];
        // Weak set is fully dropped; strong piece's [90, 180, 270] survives.
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 90.0, 180.0, 270.0]);
    }

    #[test]
    fn group_symmetry_intersects_two_strong() {
        let a = deg(&[60.0, 120.0, 180.0, 240.0, 300.0]);
        let b = deg(&[180.0]);
        let pieces = [
            (a.as_slice(), SymmetryStrength::Strong),
            (b.as_slice(), SymmetryStrength::Strong),
        ];
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 180.0]);
    }

    #[test]
    fn group_symmetry_all_weak_with_identical_sets_returns_same_set() {
        // Corollary: if every weak piece exposes the same non-trivial
        // set S, the aggregated group set equals S (plus the implicit 0°).
        let set = deg(&[60.0, 200.0, 290.0]);
        let pieces = [
            (set.as_slice(), SymmetryStrength::Weak),
            (set.as_slice(), SymmetryStrength::Weak),
            (set.as_slice(), SymmetryStrength::Weak),
        ];
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 60.0, 200.0, 290.0]);
    }

    #[test]
    fn group_symmetry_all_weak_180_only_returns_180() {
        let only_180 = deg(&[180.0]);
        let pieces = [
            (only_180.as_slice(), SymmetryStrength::Weak),
            (only_180.as_slice(), SymmetryStrength::Weak),
            (only_180.as_slice(), SymmetryStrength::Weak),
        ];
        let result = group_symmetry_angles(&pieces);
        assert_eq!(values(&result), vec![0.0, 180.0]);
    }

    #[test]
    fn group_symmetry_all_weak_picks_well_spread_subset() {
        // Two weak pieces, three angles each, fully disjoint. Mean
        // count = 3, so N = 3. Result should pick 3 well-spread angles
        // from the 6-angle union.
        let a = deg(&[40.0, 100.0, 260.0]);
        let b = deg(&[150.0, 200.0, 310.0]);
        let pieces = [
            (a.as_slice(), SymmetryStrength::Weak),
            (b.as_slice(), SymmetryStrength::Weak),
        ];
        let result = group_symmetry_angles(&pieces);
        let vs = values(&result);
        assert_eq!(vs.len(), 4, "expected 0° + 3 picked angles, got {vs:?}");
        assert_eq!(vs[0], 0.0);
        // Every picked angle must come from the union.
        let union = [40.0, 100.0, 150.0, 200.0, 260.0, 310.0];
        for value in vs.iter().skip(1) {
            assert!(
                union.iter().any(|u| (u - value).abs() < 0.5),
                "{value} not from union",
            );
        }
        // Spread: every adjacent pair (circular) should be at least
        // half of the ideal even-spacing gap (360°/4 = 90°). The greedy
        // max-min-gap selection from a constrained union won't always
        // hit 90° but is well above the "all clumped together" failure
        // mode (which would have min-gap near zero).
        let mut sorted = vs.clone();
        sorted.push(sorted[0] + 360.0);
        let min_gap = sorted
            .windows(2)
            .map(|w| w[1] - w[0])
            .fold(f32::INFINITY, f32::min);
        assert!(
            min_gap >= 45.0,
            "well-spread expected, but min circular gap is {min_gap} (angles {vs:?})",
        );
    }

    #[test]
    fn group_symmetry_empty_input_is_zero() {
        let result = group_symmetry_angles(&[]);
        assert_eq!(values(&result), vec![0.0]);
    }
}
