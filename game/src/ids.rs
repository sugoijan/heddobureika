//! Stable opaque identifiers used across logical/playable/projection layers.

macro_rules! define_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[repr(transparent)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn as_u32(self) -> u32 {
                self.0
            }

            pub const fn as_usize(self) -> usize {
                self.0 as usize
            }
        }

        impl From<u32> for $name {
            fn from(value: u32) -> Self {
                Self(value)
            }
        }

        impl From<$name> for u32 {
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl From<$name> for usize {
            fn from(value: $name) -> Self {
                value.as_usize()
            }
        }
    };
}

define_id!(PieceId);
define_id!(GroupId);
define_id!(EdgeId);
define_id!(BorderEdgeId);
define_id!(FrameEdgeId);
