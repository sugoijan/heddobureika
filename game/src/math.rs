//! Small numeric helpers shared by the topology/shape generators, the
//! gameplay model and the downstream crates: a deterministic PRNG, angle
//! wrapping, planar rotation and cubic Bézier evaluation.

/// SplitMix32 bit mixer used as the deterministic hash behind every seeded
/// random choice (shape jitter, scramble layout, Voronoi sites).
pub fn splitmix32(mut value: u32) -> u32 {
    value = value.wrapping_add(0x9E37_79B9);
    let mut z = value;
    z = (z ^ (z >> 16)).wrapping_mul(0x85EB_CA6B);
    z = (z ^ (z >> 13)).wrapping_mul(0xC2B2_AE35);
    z ^ (z >> 16)
}

/// Deterministic uniform sample in `[0, 1)` for `(seed, salt)`.
pub fn rand_unit(seed: u32, salt: u32) -> f32 {
    let mixed = splitmix32(seed ^ salt);
    (mixed >> 8) as f32 / ((1u32 << 24) as f32)
}

/// Deterministic uniform sample in `[min, max)` for `(seed, salt)`.
pub fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
}

/// Wraps an angle in degrees into `[0, 360)`.
pub fn normalize_angle(angle: f32) -> f32 {
    let wrapped = angle % 360.0;
    if wrapped < 0.0 {
        wrapped + 360.0
    } else {
        wrapped
    }
}

/// Signed shortest angular difference `to - from`, wrapped to `[-180, 180]`.
pub fn shortest_angle_delta(from: f32, to: f32) -> f32 {
    let mut delta = (to - from) % 360.0;
    if delta < -180.0 {
        delta += 360.0;
    } else if delta > 180.0 {
        delta -= 360.0;
    }
    delta
}

/// Whether two angles in degrees are within `tolerance` of each other,
/// modulo full turns.
pub fn angle_matches(a: f32, b: f32, tolerance: f32) -> bool {
    shortest_angle_delta(b, a).abs() <= tolerance
}

/// Rotates the vector `(x, y)` by `angle_deg` about the origin.
pub fn rotate_vec(x: f32, y: f32, angle_deg: f32) -> (f32, f32) {
    let (sin, cos) = angle_deg.to_radians().sin_cos();
    (x * cos - y * sin, x * sin + y * cos)
}

/// Rotates the point `(x, y)` by `angle_deg` about `(origin_x, origin_y)`.
pub fn rotate_point(x: f32, y: f32, origin_x: f32, origin_y: f32, angle_deg: f32) -> (f32, f32) {
    let (rx, ry) = rotate_vec(x - origin_x, y - origin_y, angle_deg);
    (origin_x + rx, origin_y + ry)
}

/// Evaluates the cubic Bézier `p0 → p1 → p2 → p3` at parameter `t`.
pub fn cubic_point(
    p0: (f32, f32),
    p1: (f32, f32),
    p2: (f32, f32),
    p3: (f32, f32),
    t: f32,
) -> (f32, f32) {
    let u = 1.0 - t;
    let tt = t * t;
    let uu = u * u;
    let uuu = uu * u;
    let ttt = tt * t;
    (
        uuu * p0.0 + 3.0 * uu * t * p1.0 + 3.0 * u * tt * p2.0 + ttt * p3.0,
        uuu * p0.1 + 3.0 * uu * t * p1.1 + 3.0 * u * tt * p2.1 + ttt * p3.1,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shortest_delta_wraps_both_ways() {
        assert_eq!(shortest_angle_delta(350.0, 10.0), 20.0);
        assert_eq!(shortest_angle_delta(10.0, 350.0), -20.0);
        assert_eq!(shortest_angle_delta(0.0, 180.0), 180.0);
        assert_eq!(shortest_angle_delta(0.0, -180.0), -180.0);
    }

    #[test]
    fn normalize_wraps_negative_angles() {
        assert_eq!(normalize_angle(-90.0), 270.0);
        assert_eq!(normalize_angle(720.0), 0.0);
        assert!(angle_matches(359.5, 0.0, 1.0));
        assert!(!angle_matches(0.0, 180.0, 179.0));
    }

    #[test]
    fn rotate_point_keeps_origin_fixed() {
        let (x, y) = rotate_point(3.0, 4.0, 3.0, 4.0, 123.0);
        assert_eq!((x, y), (3.0, 4.0));
        let (x, y) = rotate_point(2.0, 1.0, 1.0, 1.0, 90.0);
        assert!((x - 1.0).abs() < 1e-6 && (y - 2.0).abs() < 1e-6);
    }
}
