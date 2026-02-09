//! Shared step-rotation solver for piece and group symmetry sets.

use crate::units::AngleDeg;

const SYMMETRY_EPSILON_DEG: f32 = 1.0e-3;

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
