//! Change tracking contract between `PlayableState` and projection layers.

use smallvec::SmallVec;

use crate::ids::{EdgeId, GroupId, PieceId};

/// Short-lived ID list used by delta payloads.
pub type IdList<T, const N: usize> = SmallVec<[T; N]>;

/// Dirty IDs + pull style delta.
///
/// Projection layers should use the dirty sets to decide what to refresh, then
/// pull current values from `PlayableState`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PlayableDelta {
    pub revision: u64,
    pub dirty_groups: IdList<GroupId, 8>,
    pub dirty_pieces: IdList<PieceId, 16>,
    pub dirty_edges: IdList<EdgeId, 16>,
    /// Edges whose `edge_active` flipped from true to false during this
    /// batch. Subset of `dirty_edges`; used by the wire encoder to ship
    /// explicit deactivations so clients can keep `edge_active` consistent
    /// with the piece→group map (otherwise a future `detach_piece` rebuild
    /// would resurrect stale active edges).
    pub deactivated_edges: IdList<EdgeId, 8>,
    pub z_order_changed: bool,
    pub membership_changed: bool,
    pub solved_changed: bool,
}

impl PlayableDelta {
    pub fn for_revision(revision: u64) -> Self {
        Self {
            revision,
            ..Self::default()
        }
    }

    pub fn clear_keep_revision(&mut self, revision: u64) {
        self.revision = revision;
        self.dirty_groups.clear();
        self.dirty_pieces.clear();
        self.dirty_edges.clear();
        self.deactivated_edges.clear();
        self.z_order_changed = false;
        self.membership_changed = false;
        self.solved_changed = false;
    }
}
