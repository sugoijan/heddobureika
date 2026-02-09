//! Gameplay tolerance and rule knobs used by playable state logic.

use crate::units::AngleDeg;

/// High-level tunables for gameplay action evaluation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PlayRules {
    pub snap_distance_ratio: f32,
    pub rotation_snap_tolerance: AngleDeg,
    pub frame_snap_ratio: f32,
    pub rotation_enabled: bool,
}

impl Default for PlayRules {
    fn default() -> Self {
        Self {
            snap_distance_ratio: 0.2,
            rotation_snap_tolerance: AngleDeg::try_new(5.0).expect("5.0 should be finite"),
            frame_snap_ratio: 1.0,
            rotation_enabled: true,
        }
    }
}
