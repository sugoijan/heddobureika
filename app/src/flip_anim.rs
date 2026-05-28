//! Spring-based flip animation for the WGPU renderer (optional, off by default,
//! shares the `rotate_anim` gate and spring params). Pure, renderer-agnostic so
//! it can be unit-tested natively.
//!
//! Flipping a piece is a 3D rotation about a vertical axis. We track a scalar
//! `progress ∈ [0,1]` (0 = front, 1 = back) and spring it toward the target
//! (the authoritative flip state) using the same damped spring as the rotation
//! animation, so a flip→unflip interrupted mid-flight carries its velocity into
//! the reversal without a jump. The renderer turns `progress` into a 180°·
//! progress rotation about the piece's anchor; at the endpoints (0 or 1) it
//! reduces to the existing instant front/back rendering.

use crate::rotation_anim::{damped_spring, SpringParams};

/// Settle thresholds: once `progress` and its velocity are this close to the
/// target the flip is considered finished and snaps to the exact endpoint.
const PROGRESS_EPS: f32 = 0.002;
const VEL_EPS: f32 = 0.02;

/// Per-piece (keyed by group anchor) flip spring state.
#[derive(Clone, Copy, Debug)]
pub(crate) struct FlipAnim {
    progress: f32,
    vel: f32,
}

impl FlipAnim {
    /// Creates a state already settled at `target` (0.0 or 1.0).
    pub(crate) fn settled(target: f32) -> Self {
        Self {
            progress: target,
            vel: 0.0,
        }
    }

    /// Advances one frame toward `target` (0.0 or 1.0). Returns `true` while
    /// still animating, `false` once settled (snapped to the endpoint).
    pub(crate) fn step(&mut self, target: f32, params: SpringParams, dt: f32) -> bool {
        if dt > 0.0 {
            damped_spring(&mut self.progress, &mut self.vel, target, params, dt, false);
        }
        if (self.progress - target).abs() < PROGRESS_EPS && self.vel.abs() < VEL_EPS {
            self.progress = target;
            self.vel = 0.0;
            return false;
        }
        true
    }

    /// Current flip progress (0 = front, 1 = back). May briefly overshoot
    /// outside `[0,1]` with an underdamped spring; the renderer handles that
    /// gracefully (a slight over-rotate bounce).
    pub(crate) fn progress(&self) -> f32 {
        self.progress
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn params() -> SpringParams {
        SpringParams {
            response: 0.18,
            damping: 1.0,
        }
    }

    fn run_to_settle(anim: &mut FlipAnim, target: f32) -> usize {
        let dt = 1.0 / 60.0;
        for frame in 0..1000 {
            if !anim.step(target, params(), dt) {
                return frame;
            }
        }
        panic!("flip did not settle");
    }

    #[test]
    fn eases_front_to_back_and_settles() {
        let mut anim = FlipAnim::settled(0.0);
        // First step should move and report still-animating.
        assert!(anim.step(1.0, params(), 1.0 / 60.0));
        assert!(anim.progress() > 0.0 && anim.progress() < 1.0);
        run_to_settle(&mut anim, 1.0);
        assert_eq!(anim.progress(), 1.0);
        // Settled => not animating.
        assert!(!anim.step(1.0, params(), 1.0 / 60.0));
    }

    #[test]
    fn eases_back_to_front() {
        let mut anim = FlipAnim::settled(1.0);
        run_to_settle(&mut anim, 0.0);
        assert_eq!(anim.progress(), 0.0);
    }

    #[test]
    fn settled_state_is_identity() {
        // Stepping toward the current target never moves a settled flip.
        let mut anim = FlipAnim::settled(1.0);
        assert!(!anim.step(1.0, params(), 1.0 / 60.0));
        assert_eq!(anim.progress(), 1.0);
    }

    #[test]
    fn midflight_reverse_is_continuous() {
        let mut anim = FlipAnim::settled(0.0);
        // Partway toward back, building up velocity.
        for _ in 0..6 {
            anim.step(1.0, params(), 1.0 / 60.0);
        }
        let p_before = anim.progress();
        let v_before = anim.vel;
        assert!(p_before > 0.0 && p_before < 1.0);
        assert!(v_before > 0.0, "should be moving toward back");
        // Reverse the target with a tiny time step. Velocity (and position) must
        // be carried over — the spring integrates from the current momentum, it
        // does NOT reset to zero or teleport. Over a tiny dt the change is the
        // bounded accel*dt, so the velocity stays essentially what it was.
        anim.step(0.0, params(), 1.0e-4);
        assert!(
            (anim.progress() - p_before).abs() < 1.0e-3,
            "position not continuous across retarget"
        );
        assert!(
            (anim.vel - v_before).abs() < 0.2,
            "velocity jumped on retarget (v_before={v_before}, v_after={})",
            anim.vel
        );
        assert!(anim.vel > 0.0, "momentum should still carry toward back");
        // It eventually settles back at the front.
        run_to_settle(&mut anim, 0.0);
        assert_eq!(anim.progress(), 0.0);
    }
}
