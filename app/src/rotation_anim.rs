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

use std::collections::HashMap;
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
    /// the long way round.
    ///
    /// `pivot` is the rotation's true fixed point (the click point) when known.
    /// When supplied it is used verbatim, so the animation pivots about exactly
    /// that point regardless of where the displayed pose currently sits — this
    /// matters when a just-committed move is still settling, since the displayed
    /// centroid then lags the authoritative pose and a *recovered* pole would be
    /// wrong (the group would swing in from its pre-move position). When `None`
    /// (e.g. a network rotation) the pole is recovered from the current displayed
    /// pose, which also keeps a mid-flight re-target from jumping. A pure
    /// translation (negligible rotation) clears the rotation mode either way.
    fn retarget_if_changed(
        &mut self,
        cx: f32,
        cy: f32,
        angle: f32,
        dir_hint: f32,
        pivot: Option<(f32, f32)>,
    ) {
        let changed = shortest_angle_delta(self.tgt_angle, angle).abs() > TARGET_ANG_EPS
            || (self.tgt_cx - cx).abs() > TARGET_POS_EPS
            || (self.tgt_cy - cy).abs() > TARGET_POS_EPS;
        if !changed {
            return;
        }
        let delta = directed_delta(self.disp_angle, angle, dir_hint);
        self.goal_angle = self.disp_angle + delta;
        // A negligible rotation has no usable pole — treat it as a translation.
        // Otherwise pivot about the supplied click point when known, else recover
        // the pole from the displayed pose. The pole is direction-independent
        // (`R(delta) == R(delta - 360)`), so a forced long-way-round rotation
        // keeps the same fixed point.
        let pole = if delta.abs() < ROT_MODE_EPS_DEG {
            None
        } else {
            pivot.or_else(|| rotation_pole((self.disp_cx, self.disp_cy), delta, (cx, cy)))
        };
        match pole {
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
    /// forces the rotation direction and `pivot` supplies the exact fixed point
    /// on a re-target (see `retarget_if_changed`).
    pub(crate) fn step(
        &mut self,
        cx: f32,
        cy: f32,
        angle: f32,
        params: SpringParams,
        dt: f32,
        dir_hint: f32,
        pivot: Option<(f32, f32)>,
    ) -> bool {
        self.retarget_if_changed(cx, cy, angle, dir_hint, pivot);
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

    /// The target (authoritative) reference position this state is tracking —
    /// the reference point against which `disp`/`tgt` are expressed. Used as
    /// `old_ref` when carrying this state to a new anchor (see
    /// [`Self::rebase_reference`]).
    pub(crate) fn tracked_reference(&self) -> (f32, f32) {
        (self.tgt_cx, self.tgt_cy)
    }

    /// Re-express the displayed pose against a new reference point *without*
    /// changing the rendered transform. Used when a merge moves a group's spring
    /// state to a new canonical-anchor key: `old_ref`/`new_ref` are the old and
    /// new anchor pieces' target (authoritative) reference positions.
    ///
    /// `c_d` is the displayed position of the reference point, so the new
    /// reference's displayed position under the current transform is
    /// `c_d' = R(phi)·(new_ref − old_ref) + c_d`. The pivot is an absolute world
    /// point (and the angle/velocity state is reference-independent), so they
    /// carry over untouched; only the reference-relative translation moves.
    pub(crate) fn rebase_reference(&mut self, old_ref: (f32, f32), new_ref: (f32, f32)) {
        let phi = self.disp_angle - self.tgt_angle;
        let (s, c) = phi.to_radians().sin_cos();
        let ex = new_ref.0 - old_ref.0;
        let ey = new_ref.1 - old_ref.1;
        self.disp_cx += c * ex - s * ey;
        self.disp_cy += s * ex + c * ey;
        self.tgt_cx = new_ref.0;
        self.tgt_cy = new_ref.1;
    }
}

/// Match each previous frame's spring-state group to its primary successor in
/// the current frame by membership overlap, returning `previous_anchor ->
/// current_anchor` for the entries that should be relocated or kept (others are
/// dropped because they were absorbed into another group's entry). Each previous
/// entry proposes to the current group sharing the most of its former members;
/// each current group accepts the proposer with the largest overlap. This tracks
/// group identity across both merges (an entry follows the bulk of its members)
/// and splits (an entry follows the largest fragment) without depending on
/// `HashMap` iteration order. `has_entry(a)` reports whether previous anchor `a`
/// actually has spring state worth relocating. Ties break toward the lower
/// anchor so the result is deterministic.
///
/// `prev[i]`/`cur[i]` are piece `i`'s canonical group anchor in the previous and
/// current frame; out-of-range indices fall back to the piece's own id (its
/// singleton-group anchor).
pub(crate) fn rotate_group_succession(
    total: usize,
    prev: &[u32],
    cur: &[u32],
    has_entry: impl Fn(u32) -> bool,
) -> HashMap<u32, u32> {
    use std::collections::hash_map::Entry;
    // Shared member count between each previous entry and each current group.
    let mut overlap: HashMap<(u32, u32), usize> = HashMap::new();
    for id in 0..total {
        let prev_a = prev.get(id).copied().unwrap_or(id as u32);
        if !has_entry(prev_a) {
            continue;
        }
        let cur_a = cur.get(id).copied().unwrap_or(id as u32);
        *overlap.entry((prev_a, cur_a)).or_default() += 1;
    }
    // Each previous entry proposes to its best (max-overlap) successor.
    let mut best_for_prev: HashMap<u32, (u32, usize)> = HashMap::new();
    for (&(prev_a, cur_a), &n) in &overlap {
        match best_for_prev.entry(prev_a) {
            Entry::Occupied(mut o) => {
                let (b_cur, b_n) = *o.get();
                if n > b_n || (n == b_n && cur_a < b_cur) {
                    o.insert((cur_a, n));
                }
            }
            Entry::Vacant(v) => {
                v.insert((cur_a, n));
            }
        }
    }
    // Each current group accepts the proposer with the largest overlap.
    let mut accepted: HashMap<u32, (u32, usize)> = HashMap::new();
    for (&prev_a, &(cur_a, n)) in &best_for_prev {
        match accepted.entry(cur_a) {
            Entry::Occupied(mut o) => {
                let (b_prev, b_n) = *o.get();
                if n > b_n || (n == b_n && prev_a < b_prev) {
                    o.insert((prev_a, n));
                }
            }
            Entry::Vacant(v) => {
                v.insert((prev_a, n));
            }
        }
    }
    let mut dest = HashMap::with_capacity(accepted.len());
    for (cur_a, (prev_a, _)) in accepted {
        dest.insert(prev_a, cur_a);
    }
    dest
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
                None,
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
            anim.step(target_a.0, target_a.1, angle_a, params, dt, 0.0, None);
        }

        // Second rotation: the game rotates the authoritative pose (target_a)
        // about a new click point by another 60 degrees.
        let pivot_b = (-80.0, 360.0);
        let angle_b = angle_a + 60.0;
        let target_b = rotate_about(target_a, pivot_b, 60.0);

        // First frame of the new leg establishes the pole we should hold.
        anim.step(target_b.0, target_b.1, angle_b, params, dt, 0.0, None);
        let (phi0, c_d0) = anim.offset(angle_b);
        let pole = transform_fixed_point(target_b, c_d0, phi0);

        let mut max_err = 0.0_f32;
        for _ in 0..600 {
            let still = anim.step(target_b.0, target_b.1, angle_b, params, dt, 0.0, None);
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

    /// Carrying a group's spring state to a new canonical-anchor key (on a
    /// merge) must not move anything on screen: rendering any probe point under
    /// the rebased state against the *new* reference must match the rendering
    /// under the old state against the *old* reference. Uses a mid-flight
    /// rotation so the reference change is exercised with a non-zero `phi`.
    #[test]
    fn rebase_reference_preserves_rendered_transform() {
        let params = SpringParams {
            response: 0.2,
            damping: 1.0,
        };
        let pivot = (300.0, 120.0);
        let start = (60.0, 80.0);
        let target_angle = 90.0;
        let target = rotate_about(start, pivot, target_angle);

        let mut anim = GroupAnim::settled(start.0, start.1, 0.0);
        let dt = 1.0 / 60.0;
        // Mid-flight: displayed angle strictly between 0 and 90 => non-zero phi.
        for _ in 0..5 {
            anim.step(target.0, target.1, target_angle, params, dt, 0.0, None);
        }

        // Render a probe point under the current (old-reference) transform.
        let probe = (220.0, 160.0);
        let old_ref = anim.tracked_reference();
        let (phi0, c_d0) = anim.offset(target_angle);
        let before = apply_group_transform(probe, old_ref, c_d0, phi0);

        // The merge re-keys this group onto a different anchor piece whose
        // target reference is `new_ref`. Carry the state across.
        let new_ref = (140.0, 200.0);
        anim.rebase_reference(old_ref, new_ref);

        // The same probe, rendered against the new reference, must not move.
        let (phi1, c_d1) = anim.offset(target_angle);
        let after = apply_group_transform(probe, new_ref, c_d1, phi1);

        assert!(
            approx(before, after, 1e-3),
            "rebase moved the rendered transform: {before:?} != {after:?}"
        );
        // And the state now tracks the new reference.
        assert!(
            approx(anim.tracked_reference(), new_ref, 1e-3),
            "rebase did not adopt the new reference"
        );
    }

    /// All previous anchors have spring state (the common case in the renderer,
    /// where every group gets an entry).
    fn all_live(_: u32) -> bool {
        true
    }

    /// An unchanged grouping maps every entry to itself — nothing is relocated
    /// or dropped.
    #[test]
    fn succession_keeps_unchanged_groups() {
        // Two groups: {0,1} anchor 0, {2,3} anchor 2.
        let prev = [0u32, 0, 2, 2];
        let cur = [0u32, 0, 2, 2];
        let dest = rotate_group_succession(4, &prev, &cur, all_live);
        assert_eq!(dest.get(&0), Some(&0));
        assert_eq!(dest.get(&2), Some(&2));
        assert_eq!(dest.len(), 2);
    }

    /// Merge of two groups into one (anchor = the lower id): the entry follows
    /// the bulk of its members; the absorbed group's entry is dropped.
    #[test]
    fn succession_merge_follows_majority() {
        // Prev: {0,1} anchor 0, {2,3} anchor 2. Now all one group, anchor 0.
        let prev = [0u32, 0, 2, 2];
        let cur = [0u32, 0, 0, 0];
        let dest = rotate_group_succession(4, &prev, &cur, all_live);
        // Both previous entries' best successor is current anchor 0; the one with
        // the larger overlap wins. They tie here (2 each), so the lower previous
        // anchor (0) wins and 2 is dropped.
        assert_eq!(dest.get(&0), Some(&0));
        assert_eq!(dest.get(&2), None);
        assert_eq!(dest.len(), 1);
    }

    /// Merge where a *larger* group attaches to a *lower-id* loose piece: the
    /// big group's animation must move onto the new (lower) anchor key rather
    /// than be lost to the loose piece's settled entry.
    #[test]
    fn succession_merge_into_lower_loose_piece() {
        // Prev: lone piece 1 (anchor 1), group {5,6,7} anchor 5.
        // Now merged into one group whose anchor is the min => 1.
        let prev = [0u32, 1, 2, 3, 4, 5, 5, 5];
        let cur = [0u32, 1, 2, 3, 4, 1, 1, 1];
        let dest = rotate_group_succession(8, &prev, &cur, all_live);
        // The big group (prev anchor 5, overlap 3) wins key 1 over the loose
        // piece (prev anchor 1, overlap 1).
        assert_eq!(dest.get(&5), Some(&1));
        assert_eq!(dest.get(&1), None);
    }

    /// Detach of a non-anchor piece: the remnant keeps its key in place and the
    /// detached singleton gets no carried entry (it will seed `settled`).
    #[test]
    fn succession_detach_non_anchor_keeps_remnant() {
        // Prev: {0,1,2} anchor 0. Detach piece 2 => {0,1} anchor 0, {2} anchor 2.
        let prev = [0u32, 0, 0];
        let cur = [0u32, 0, 2];
        let dest = rotate_group_succession(3, &prev, &cur, all_live);
        assert_eq!(dest.get(&0), Some(&0)); // remnant keeps its state in place
        assert!(!dest.values().any(|&c| c == 2)); // nothing carried to the lone piece
        assert_eq!(dest.len(), 1);
    }

    /// Detach of the anchor (min) piece: the entry follows the larger remnant
    /// (re-keyed to its new anchor); the detached former-anchor piece gets none.
    #[test]
    fn succession_detach_anchor_follows_larger_remnant() {
        // Prev: {0,1,2} anchor 0. Detach piece 0 => {0} anchor 0, {1,2} anchor 1.
        let prev = [0u32, 0, 0];
        let cur = [0u32, 1, 1];
        let dest = rotate_group_succession(3, &prev, &cur, all_live);
        // Entry 0's best successor is current anchor 1 (overlap 2 > 1).
        assert_eq!(dest.get(&0), Some(&1));
        // Current group {0} (the detached former anchor) receives no entry.
        assert!(!dest.values().any(|&c| c == 0));
        assert_eq!(dest.len(), 1);
    }

    /// Only previous anchors that actually have spring state are considered;
    /// groups without an entry never produce a relocation.
    #[test]
    fn succession_ignores_anchors_without_entries() {
        let prev = [0u32, 0, 2, 2];
        let cur = [0u32, 0, 0, 0];
        // Pretend only anchor 2 has an entry.
        let dest = rotate_group_succession(4, &prev, &cur, |a| a == 2);
        assert_eq!(dest.get(&2), Some(&0));
        assert_eq!(dest.get(&0), None);
        assert_eq!(dest.len(), 1);
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
            let still = cw.step(target.0, target.1, target_angle, params, 1.0 / 60.0, 1.0, None);
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
            let still =
                shortest.step(target.0, target.1, target_angle, params, 1.0 / 60.0, 0.0, None);
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

    /// Starting a rotation while a just-committed move is still settling: the
    /// displayed centroid lags the authoritative (post-move) pose. With the true
    /// click pivot supplied, the animation rotates about that exact point — it
    /// never swings in from the stale pre-move position — so the click point
    /// stays fixed for the whole transition. (Without the pivot the recovered
    /// pole would latch onto the lagging centroid and the click would drift.)
    #[test]
    fn supplied_pivot_holds_through_lagging_move() {
        let params = SpringParams {
            response: 0.18,
            damping: 1.0,
        };
        let pre_move = (50.0, 60.0);
        let post_move = (400.0, 120.0); // a large move, only partly animated
        let click = (260.0, 300.0);
        let rot_delta = -90.0; // counter-clockwise (the reported direction)
        let dt = 1.0 / 60.0;

        // Settle at the pre-move pose, then start the move and step only a few
        // frames so the displayed centroid is still far from `post_move`.
        let mut anim = GroupAnim::settled(pre_move.0, pre_move.1, 0.0);
        for _ in 0..3 {
            anim.step(post_move.0, post_move.1, 0.0, params, dt, 0.0, None);
        }
        let (_, lagging) = anim.offset(0.0);
        let lag = ((lagging.0 - post_move.0).powi(2) + (lagging.1 - post_move.1).powi(2)).sqrt();
        assert!(
            lag > 100.0,
            "precondition: the move should still be lagging (disp={lagging:?})"
        );

        // Now rotate the *post-move* group about the click point by `rot_delta`,
        // supplying the exact pivot the way a local click-to-rotate does.
        let target_angle = rot_delta;
        let target_centroid = rotate_about(post_move, click, rot_delta);
        let mut max_err = 0.0_f32;
        let mut animated = false;
        for _ in 0..600 {
            let still = anim.step(
                target_centroid.0,
                target_centroid.1,
                target_angle,
                params,
                dt,
                -1.0,
                Some(click),
            );
            let (phi, c_d) = anim.offset(target_angle);
            let mapped = apply_group_transform(click, target_centroid, c_d, phi);
            let err = ((mapped.0 - click.0).powi(2) + (mapped.1 - click.1).powi(2)).sqrt();
            max_err = max_err.max(err);
            if !still {
                break;
            }
            animated = true;
        }
        assert!(animated, "expected the group to animate the rotation");
        assert!(
            max_err <= 0.05,
            "click pivot drifted (animation latched onto the lagging pose): max_err={max_err}"
        );
        // And it must arrive exactly: settled => identity at the target.
        let (phi, c_d) = anim.offset(target_angle);
        assert!(phi.abs() <= ANG_EPS, "did not settle in angle: phi={phi}");
        assert!(
            approx(c_d, target_centroid, POS_EPS),
            "did not settle in position: {c_d:?} != {target_centroid:?}"
        );
    }
}
