//! Domain units used by game topology/state APIs.

use std::num::NonZeroU32;

use typed_floats::tf32::NonNaNFinite;

/// Finite scalar backing for unit newtypes.
pub type FiniteScalar = NonNaNFinite;

/// Pixels-per-inch density used to map pixels to millimeters.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Dpi(NonZeroU32);

impl Dpi {
    pub fn new(value: NonZeroU32) -> Self {
        Self(value)
    }

    pub fn try_new(value: u32) -> Option<Self> {
        Some(Self(NonZeroU32::new(value)?))
    }

    pub fn get(self) -> u32 {
        self.0.get()
    }
}

/// Linear distance in millimeters.
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(transparent)]
pub struct LengthMm(FiniteScalar);

impl LengthMm {
    pub fn try_new(mm: f32) -> Option<Self> {
        Some(Self(FiniteScalar::try_from(mm).ok()?))
    }

    pub fn zero() -> Self {
        Self::try_new(0.0).expect("0.0 must be finite")
    }

    pub fn from_px(px: f32, dpi: Dpi) -> Option<Self> {
        let mm = px * 25.4 / dpi.get() as f32;
        Self::try_new(mm)
    }

    pub fn as_mm_f32(self) -> f32 {
        self.0.into()
    }

    pub fn to_px_f32(self, dpi: Dpi) -> f32 {
        self.as_mm_f32() * dpi.get() as f32 / 25.4
    }
}

impl Default for LengthMm {
    fn default() -> Self {
        Self::zero()
    }
}

/// Angle in degrees.
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(transparent)]
pub struct AngleDeg(FiniteScalar);

impl AngleDeg {
    pub fn try_new(degrees: f32) -> Option<Self> {
        Some(Self(FiniteScalar::try_from(degrees).ok()?))
    }

    pub fn zero() -> Self {
        Self::try_new(0.0).expect("0.0 must be finite")
    }

    pub fn quarter_turn() -> Self {
        Self::try_new(90.0).expect("90.0 must be finite")
    }

    pub fn as_degrees_f32(self) -> f32 {
        self.0.into()
    }

    pub fn normalized(self) -> Self {
        let mut angle = self.as_degrees_f32() % 360.0;
        if angle < 0.0 {
            angle += 360.0;
        }
        Self::try_new(angle).expect("normalized angle should remain finite")
    }

    pub fn add_degrees(self, delta_degrees: f32) -> Option<Self> {
        Self::try_new(self.as_degrees_f32() + delta_degrees)
    }
}

impl Default for AngleDeg {
    fn default() -> Self {
        Self::zero()
    }
}
