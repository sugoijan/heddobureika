//! Spring-based rotation animation for the WGPU renderer (optional, off by
//! default). Pure, renderer-agnostic math so it can be unit-tested natively.
//!
//! A "click to rotate" turns a group from one rigid pose to another. Any planar
//! rigid motion with a non-zero rotation is a pure rotation about a unique fixed
//! point — its *pole*. Because the game rotates a group about the clicked point,
//! that pole **is** the click point. To make the animation look natural the
//! clicked point must stay put for the whole transition, so we:
//!
//! 1. recover the pole from the current displayed pose and the new target pose,
//! 2. spring only the rotation angle toward the target, and
//! 3. *derive* the group's displayed centroid from the pole + angle.
//!
//! This keeps the pole exactly fixed at every intermediate frame (no drift),
//! while still reaching the target pose precisely. Spring state persists across
//! frames, so re-rotating mid-flight carries the current velocity into the new
//! target (and the new pole) without a jump. Pure translations (e.g. drop snaps
//! with no rotation) fall back to springing the centroid directly.

use std::f32::consts::TAU;

/// Tunable spring feel. `response` is roughly the time (seconds) to reach the
/// target; `damping` is the damping ratio (1.0 critical, <1.0 bouncy).
#[derive(Clone, Copy, Debug)]
pub(crate) struct SpringParams {
    pub(crate) response: f32,
    pub(crate) damping: f32,
}

/// Below this rotation magnitude (degrees) the motion is treated as a pure
/// translation, avoiding an ill-conditioned (far-away) pole.
const ROT_MODE_EPS_DEG: f32 = 0.5;
/// A target differing from the tracked one by more than these is a new action.
const TARGET_POS_EPS: f32 = 0.01;
const TARGET_ANG_EPS: f32 = 0.01;
/// Settle thresholds: once the displayed pose and velocity are this close to the
/// target the transform becomes the identity and the group stops animating.
const POS_EPS: f32 = 0.05;
const ANG_EPS: f32 = 0.05;
const POS_VEL_EPS: f32 = 0.5;
const ANG_VEL_EPS: f32 = 0.5;

/// Per-group spring state, keyed by canonical group anchor in the caller. When
/// the displayed pose matches the target the rendered output is identical to the
/// un-animated path.
#[derive(Clone, Copy, Debug)]
pub(crate) struct GroupAnim {
    disp_cx: f32,
    disp_cy: f32,
    /// Displayed angle, kept *continuous* (may exceed 360) so the spring can be
    /// driven the long way round when a forced direction demands it.
    disp_angle: f32,
    /// Continuous angle the spring travels toward; equals `disp_angle + delta`
    /// where `delta` honors the requested rotation direction (see `step`).
    goal_angle: f32,
    vel_cx: f32,
    vel_cy: f32,
    vel_angle: f32,
    tgt_cx: f32,
    tgt_cy: f32,
    tgt_angle: f32,
    pivot_x: f32,
    pivot_y: f32,
    rotating: bool,
}

impl GroupAnim {
    /// Creates a state already settled at the given target pose (no animation).
    pub(crate) fn settled(cx: f32, cy: f32, angle: f32) -> Self {
        Self {
            disp_cx: cx,
            disp_cy: cy,
            disp_angle: angle,
            goal_angle: angle,
            vel_cx: 0.0,
            vel_cy: 0.0,
            vel_angle: 0.0,
            tgt_cx: cx,
            tgt_cy: cy,
            tgt_angle: angle,
            pivot_x: cx,
            pivot_y: cy,
            rotating: false,
        }
    }

    /// Forces the displayed pose to the target instantly (used for groups being
    /// dragged, which must track the cursor live).
    pub(crate) fn snap_to(&mut self, cx: f32, cy: f32, angle: f32) {
        *self = Self::settled(cx, cy, angle);
    }

    /// Reconfigures the motion when the authoritative target changed, preserving
    /// velocity. `dir_hint` forces the rotation direction (`> 0` clockwise /
    /// increasing angle, `< 0` counter-clockwise, `0` = shortest path); it lets a
    /// local click-to-rotate animate the way the user clicked even when that is
    /// the long way round. A rotation re-derives the fixed pole from the *current
    /// displayed pose* so re-targeting mid-flight never jumps; a pure translation
    /// clears the rotation mode.
    fn retarget_if_changed(&mut self, cx: f32, cy: f32, angle: f32, dir_hint: f32) {
        let changed = shortest_angle_delta(self.tgt_angle, angle).abs() > TARGET_ANG_EPS
            || (self.tgt_cx - cx).abs() > TARGET_POS_EPS
            || (self.tgt_cy - cy).abs() > TARGET_POS_EPS;
        if !changed {
            return;
        }
        let delta = directed_delta(self.disp_angle, angle, dir_hint);
        self.goal_angle = self.disp_angle + delta;
        // The pole is direction-independent (`R(delta) == R(delta - 360)`), so a
        // forced long-way-round rotation keeps the same fixed point.
        match rotation_pole((self.disp_cx, self.disp_cy), delta, (cx, cy)) {
            Some((px, py)) => {
                self.pivot_x = px;
                self.pivot_y = py;
                self.rotating = true;
                // Position is derived from the pole while rotating; drop any
                // stale linear velocity so settling isn't blocked.
                self.vel_cx = 0.0;
                self.vel_cy = 0.0;
            }
            None => self.rotating = false,
        }
        self.tgt_cx = cx;
        self.tgt_cy = cy;
        self.tgt_angle = angle;
    }

    /// Advances one frame toward the (possibly new) target. Returns `true` while
    /// still animating, `false` once settled (identity transform). `dir_hint`
    /// forces the rotation direction on a re-target (see `retarget_if_changed`).
    pub(crate) fn step(
        &mut self,
        cx: f32,
        cy: f32,
        angle: f32,
        params: SpringParams,
        dt: f32,
        dir_hint: f32,
    ) -> bool {
        self.retarget_if_changed(cx, cy, angle, dir_hint);
        if dt > 0.0 {
            // Spring the *continuous* angle toward the continuous goal (linear,
            // not shortest-path) so a forced direction is honored.
            damped_spring(
                &mut self.disp_angle,
                &mut self.vel_angle,
                self.goal_angle,
                params,
                dt,
                false,
            );
            if self.rotating {
                // Keep the pole fixed: the displayed centroid is the target
                // centroid rotated by the remaining offset about the pole.
                let (dx, dy) = rotate_about(
                    (cx, cy),
                    (self.pivot_x, self.pivot_y),
                    self.disp_angle - angle,
                );
                self.disp_cx = dx;
                self.disp_cy = dy;
            } else {
                damped_spring(&mut self.disp_cx, &mut self.vel_cx, cx, params, dt, false);
                damped_spring(&mut self.disp_cy, &mut self.vel_cy, cy, params, dt, false);
            }
        }
        let settled = (self.disp_cx - cx).abs() < POS_EPS
            && (self.disp_cy - cy).abs() < POS_EPS
            && self.vel_cx.abs() < POS_VEL_EPS
            && self.vel_cy.abs() < POS_VEL_EPS
            && (self.disp_angle - self.goal_angle).abs() < ANG_EPS
            && self.vel_angle.abs() < ANG_VEL_EPS;
        if settled {
            self.disp_cx = cx;
            self.disp_cy = cy;
            // Re-bind to the wrapped authoritative angle so it stays bounded.
            self.disp_angle = angle;
            self.goal_angle = angle;
            self.vel_cx = 0.0;
            self.vel_cy = 0.0;
            self.vel_angle = 0.0;
            self.rotating = false;
            return false;
        }
        true
    }

    /// Rigid offset to apply to the group's members for the current frame:
    /// `(phi_deg, (c_d_x, c_d_y))`, where `phi` is the rotation about the target
    /// centroid and `c_d` is the displayed centroid. Feed into
    /// [`apply_group_transform`].
    pub(crate) fn offset(&self, angle: f32) -> (f32, (f32, f32)) {
        (self.disp_angle - angle, (self.disp_cx, self.disp_cy))
    }
}

/// Signed shortest angular difference `to - from`, wrapped to `[-180, 180]` deg.
pub(crate) fn shortest_angle_delta(from: f32, to: f32) -> f32 {
    let mut delta = (to - from) % 360.0;
    if delta < -180.0 {
        delta += 360.0;
    } else if delta > 180.0 {
        delta -= 360.0;
    }
    delta
}

/// Signed angle to travel from `from` to `to`. With `hint == 0` this is the
/// shortest path (`[-180, 180]`); with `hint > 0` it is forced positive
/// (clockwise / increasing angle, `[0, 360)`) and with `hint < 0` forced
/// negative (`(-360, 0]`) — so a click-to-rotate animates the way the user
/// clicked even when that is the long way around.
fn directed_delta(from: f32, to: f32, hint: f32) -> f32 {
    let short = shortest_angle_delta(from, to);
    // No hint, or no real rotation (a translation/snap): take the shortest path
    // so a stale hint can never turn a ~0° change into a full 360° spin.
    if hint == 0.0 || short.abs() < ROT_MODE_EPS_DEG {
        return short;
    }
    let pos = (to - from).rem_euclid(360.0);
    if hint > 0.0 {
        pos
    } else {
        pos - 360.0
    }
}

/// Rotates `point` about `pivot` by `angle_deg`.
fn rotate_about(point: (f32, f32), pivot: (f32, f32), angle_deg: f32) -> (f32, f32) {
    let (s, c) = angle_deg.to_radians().sin_cos();
    let ex = point.0 - pivot.0;
    let ey = point.1 - pivot.1;
    (c * ex - s * ey + pivot.0, s * ex + c * ey + pivot.1)
}

/// Fixed point (pole) of the planar rigid motion that maps `from_centroid` to
/// `to_centroid` while rotating by `dtheta_deg`. Returns `None` for a ~pure
/// translation, where no finite pole exists.
fn rotation_pole(
    from_centroid: (f32, f32),
    dtheta_deg: f32,
    to_centroid: (f32, f32),
) -> Option<(f32, f32)> {
    if dtheta_deg.abs() < ROT_MODE_EPS_DEG {
        return None;
    }
    let (s, c) = dtheta_deg.to_radians().sin_cos();
    // P = (I - R)^-1 (to - R*from); det(I - R) = 2(1 - cos).
    let det = 2.0 * (1.0 - c);
    if det.abs() < 1e-9 {
        return None;
    }
    let rfx = c * from_centroid.0 - s * from_centroid.1;
    let rfy = s * from_centroid.0 + c * from_centroid.1;
    let vx = to_centroid.0 - rfx;
    let vy = to_centroid.1 - rfy;
    let inv = 1.0 / det;
    let px = inv * ((1.0 - c) * vx - s * vy);
    let py = inv * (s * vx + (1.0 - c) * vy);
    Some((px, py))
}

/// Per-member render transform: rotate `point` about the target centroid `c_t`
/// by `phi_deg`, then translate so the centroid lands at the displayed centroid
/// `c_d`. With `c_d` derived from the pole this keeps the pole fixed exactly;
/// at settle (`phi = 0`, `c_d = c_t`) it is the identity.
pub(crate) fn apply_group_transform(
    point: (f32, f32),
    c_t: (f32, f32),
    c_d: (f32, f32),
    phi_deg: f32,
) -> (f32, f32) {
    let (s, c) = phi_deg.to_radians().sin_cos();
    let ex = point.0 - c_t.0;
    let ey = point.1 - c_t.1;
    (c * ex - s * ey + c_d.0, s * ex + c * ey + c_d.1)
}

/// Damped-spring integrator (semi-implicit Euler, sub-stepped for stability at
/// stiff `response`). Preserves velocity across calls so re-targeting stays
/// smooth. When `angular`, the error uses the shortest angular path.
pub(crate) fn damped_spring(
    x: &mut f32,
    vel: &mut f32,
    target: f32,
    params: SpringParams,
    dt: f32,
    angular: bool,
) {
    let omega = TAU / params.response.max(1e-3);
    let steps = ((omega * dt / 0.2).ceil() as i32).clamp(1, 256);
    let sub = dt / steps as f32;
    let k = omega * omega;
    let damping = 2.0 * params.damping * omega;
    for _ in 0..steps {
        let err = if angular {
            shortest_angle_delta(*x, target)
        } else {
            target - *x
        };
        let acc = k * err - damping * *vel;
        *vel += acc * sub;
        *x += *vel * sub;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn approx(a: (f32, f32), b: (f32, f32), eps: f32) -> bool {
        (a.0 - b.0).abs() <= eps && (a.1 - b.1).abs() <= eps
    }

    #[test]
    fn rotation_pole_recovers_known_pivot() {
        let pivot = (137.0, -42.0);
        let from = (10.0, 20.0);
        for dtheta in [15.0_f32, 60.0, 90.0, -120.0, 175.0] {
            let to = rotate_about(from, pivot, dtheta);
            let got = rotation_pole(from, dtheta, to).expect("rotation has a pole");
            assert!(
                approx(got, pivot, 1e-2),
                "dtheta={dtheta}: pole {got:?} != {pivot:?}"
            );
        }
    }

    #[test]
    fn pure_translation_has_no_pole() {
        assert!(rotation_pole((0.0, 0.0), 0.0, (50.0, 50.0)).is_none());
    }

    /// The clicked point (the rotation pole) must stay fixed for the WHOLE
    /// animation, including far from the group centroid — the case that drifted
    /// with independent translation/angle springs.
    #[test]
    fn pivot_stays_fixed_throughout_rotation() {
        let params = SpringParams {
            response: 0.18,
            damping: 1.0,
        };
        // Pivot far from the group centroid to make any drift obvious.
        let pivot = (400.0, 250.0);
        let start_centroid = (60.0, 80.0);
        let target_angle = 90.0;
        let target_centroid = rotate_about(start_centroid, pivot, target_angle);

        let mut anim = GroupAnim::settled(start_centroid.0, start_centroid.1, 0.0);
        let dt = 1.0 / 60.0;
        let mut animated = false;
        let mut max_err = 0.0_f32;
        for _ in 0..600 {
            let still = anim.step(
                target_centroid.0,
                target_centroid.1,
                target_angle,
                params,
                dt,
                0.0,
            );
            let (phi, c_d) = anim.offset(target_angle);
            // The pole, expressed in the target config, must map back onto
            // itself under the rendered transform at every frame.
            let mapped = apply_group_transform(pivot, target_centroid, c_d, phi);
            let err = ((mapped.0 - pivot.0).powi(2) + (mapped.1 - pivot.1).powi(2)).sqrt();
            max_err = max_err.max(err);
            if !still {
                break;
            }
            animated = true;
        }
        assert!(animated, "expected the group to animate toward the target");
        assert!(
            max_err <= 0.05,
            "pivot drifted during animation: max_err={max_err}"
        );
        // And it must actually arrive: settled => identity transform.
        let (phi, c_d) = anim.offset(target_angle);
        assert!(phi.abs() <= ANG_EPS, "did not settle in angle: phi={phi}");
        assert!(
            approx(c_d, target_centroid, POS_EPS),
            "did not settle in position: {c_d:?} != {target_centroid:?}"
        );
    }

    /// Fixed point of the render transform `D(p) = R(phi)(p - c_t) + c_d`,
    /// i.e. `(I - R)^-1 (c_d - R*c_t)`. Used by the test to read back the pole
    /// the animation is actually rotating about on a given frame.
    fn transform_fixed_point(c_t: (f32, f32), c_d: (f32, f32), phi_deg: f32) -> (f32, f32) {
        let (s, c) = phi_deg.to_radians().sin_cos();
        let det = 2.0 * (1.0 - c);
        let rcx = c * c_t.0 - s * c_t.1;
        let rcy = s * c_t.0 + c * c_t.1;
        let vx = c_d.0 - rcx;
        let vy = c_d.1 - rcy;
        let inv = 1.0 / det;
        (
            inv * ((1.0 - c) * vx - s * vy),
            inv * (s * vx + (1.0 - c) * vy),
        )
    }

    /// Re-rotating before the first animation settles keeps a single pole fixed
    /// for the new leg (no drift) and still converges (no jank / runaway). When
    /// interrupted mid-flight the pole is that of the *current displayed pose →
    /// new target*, so we read it back from the render transform rather than
    /// assuming the raw second click point.
    #[test]
    fn pivot_fixed_after_midflight_retarget() {
        let params = SpringParams {
            response: 0.2,
            damping: 1.0,
        };
        let pivot_a = (300.0, 120.0);
        let start = (50.0, 40.0);
        let angle_a = 90.0;
        let target_a = rotate_about(start, pivot_a, angle_a);

        let mut anim = GroupAnim::settled(start.0, start.1, 0.0);
        let dt = 1.0 / 60.0;
        // Animate partway toward the first target (still mid-flight).
        for _ in 0..6 {
            anim.step(target_a.0, target_a.1, angle_a, params, dt, 0.0);
        }

        // Second rotation: the game rotates the authoritative pose (target_a)
        // about a new click point by another 60 degrees.
        let pivot_b = (-80.0, 360.0);
        let angle_b = angle_a + 60.0;
        let target_b = rotate_about(target_a, pivot_b, 60.0);

        // First frame of the new leg establishes the pole we should hold.
        anim.step(target_b.0, target_b.1, angle_b, params, dt, 0.0);
        let (phi0, c_d0) = anim.offset(angle_b);
        let pole = transform_fixed_point(target_b, c_d0, phi0);

        let mut max_err = 0.0_f32;
        for _ in 0..600 {
            let still = anim.step(target_b.0, target_b.1, angle_b, params, dt, 0.0);
            let (phi, c_d) = anim.offset(angle_b);
            let mapped = apply_group_transform(pole, target_b, c_d, phi);
            let err = ((mapped.0 - pole.0).powi(2) + (mapped.1 - pole.1).powi(2)).sqrt();
            max_err = max_err.max(err);
            if !still {
                break;
            }
        }
        assert!(
            max_err <= 0.05,
            "pole drifted after mid-flight retarget: max_err={max_err}"
        );
    }

    /// A clockwise click whose target's *shortest* path is counter-clockwise
    /// must still animate clockwise when the direction is forced. Here the
    /// target is +200° (shortest = -160°); with a `+` hint the displayed angle
    /// must sweep upward (clockwise) and never take the short way.
    #[test]
    fn forced_direction_takes_the_long_way() {
        let params = SpringParams {
            response: 0.18,
            damping: 1.0,
        };
        let pivot = (200.0, 100.0);
        let start = (40.0, 30.0);
        let target_angle = 200.0; // shortest path from 0 would be -160
        let target = rotate_about(start, pivot, 200.0);

        // Forced clockwise (hint > 0): disp_angle climbs 0 -> ~200, never negative.
        let mut cw = GroupAnim::settled(start.0, start.1, 0.0);
        let mut min_disp = f32::INFINITY;
        let mut max_disp = f32::NEG_INFINITY;
        for _ in 0..600 {
            let still = cw.step(target.0, target.1, target_angle, params, 1.0 / 60.0, 1.0);
            min_disp = min_disp.min(cw.disp_angle);
            max_disp = max_disp.max(cw.disp_angle);
            if !still {
                break;
            }
        }
        assert!(
            min_disp > -1.0,
            "clockwise hint must not go counter-clockwise (min_disp={min_disp})"
        );
        assert!(
            max_disp > 150.0,
            "clockwise hint should sweep the long way (max_disp={max_disp})"
        );

        // No hint (0.0): the spring takes the shortest path (counter-clockwise),
        // i.e. disp_angle goes negative — confirming the hint changed behavior.
        let mut shortest = GroupAnim::settled(start.0, start.1, 0.0);
        let mut min_short = f32::INFINITY;
        for _ in 0..600 {
            let still = shortest.step(target.0, target.1, target_angle, params, 1.0 / 60.0, 0.0);
            min_short = min_short.min(shortest.disp_angle);
            if !still {
                break;
            }
        }
        assert!(
            min_short < -100.0,
            "without a hint the shortest path is counter-clockwise (min_short={min_short})"
        );
    }
}
