//! Projection/cache layer derived from `PlayableState`.

use crate::delta::PlayableDelta;
use crate::ids::{GroupId, PieceId};
use crate::playable::{PlayableState, Pose2};
use crate::topology::PuzzleTopology;

/// Reusable buffers for projection updates.
pub struct ProjectionScratch {
    pub dirty_groups: Vec<GroupId>,
}

impl ProjectionScratch {
    pub fn with_capacity(piece_capacity: usize) -> Self {
        Self {
            dirty_groups: Vec::with_capacity(piece_capacity),
        }
    }
}

/// Render/input-oriented cache derived from playable state.
///
/// This state can include additional visual-only data (offsets, smoothing),
/// but it should never be authoritative for gameplay rules.
pub struct InteractableState {
    pub piece_world_pose: Box<[Pose2]>,
    pub piece_group_cache: Box<[GroupId]>,
    pub z_sorted_pieces: Vec<PieceId>,
    pub piece_visual_offset: Box<[[f32; 2]]>,
    pub piece_visual_rot_offset: Box<[f32]>,
}

impl InteractableState {
    pub fn rebuild_from<T: PuzzleTopology>(playable: &PlayableState<T>) -> Self {
        let piece_count = playable.piece_count();
        let mut state = Self {
            piece_world_pose: vec![Pose2::default(); piece_count].into_boxed_slice(),
            piece_group_cache: playable
                .logical
                .piece_groups()
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            z_sorted_pieces: Vec::with_capacity(piece_count),
            piece_visual_offset: vec![[0.0, 0.0]; piece_count].into_boxed_slice(),
            piece_visual_rot_offset: vec![0.0; piece_count].into_boxed_slice(),
        };
        state.rebuild_z_sorted_pieces(playable);
        state.refresh_all_piece_pose(playable);
        state
    }

    /// Applies a dirty-id delta, then pulls the latest values from playable.
    pub fn apply_delta<T: PuzzleTopology>(
        &mut self,
        playable: &PlayableState<T>,
        delta: &PlayableDelta,
        scratch: &mut ProjectionScratch,
    ) {
        let piece_count = playable.piece_count();
        if self.piece_world_pose.len() != piece_count {
            *self = Self::rebuild_from(playable);
            return;
        }

        if delta.membership_changed {
            self.piece_group_cache = playable
                .logical
                .piece_groups()
                .collect::<Vec<_>>()
                .into_boxed_slice();
        }

        scratch.dirty_groups.clear();
        scratch
            .dirty_groups
            .extend(delta.dirty_groups.iter().copied());

        for group in scratch.dirty_groups.iter().copied() {
            self.refresh_group_piece_pose(playable, group);
        }

        for piece in delta.dirty_pieces.iter().copied() {
            if let Some(group) = playable.logical.group_of(piece) {
                self.refresh_one_piece_pose(playable, piece, group);
            }
        }

        if delta.z_order_changed || delta.membership_changed {
            self.rebuild_z_sorted_pieces(playable);
        }
    }

    pub fn piece_world_pose(&self) -> &[Pose2] {
        &self.piece_world_pose
    }

    fn refresh_all_piece_pose<T: PuzzleTopology>(&mut self, playable: &PlayableState<T>) {
        for group in playable.iter_z_asc() {
            self.refresh_group_piece_pose(playable, group);
        }
    }

    fn refresh_group_piece_pose<T: PuzzleTopology>(
        &mut self,
        playable: &PlayableState<T>,
        group: GroupId,
    ) {
        let Some(group_pose) = playable.pose_of(group) else {
            return;
        };

        for piece in playable.logical.members_of(group) {
            let idx = piece.as_usize();
            if let Some(out_pose) = self.piece_world_pose.get_mut(idx) {
                // In this initial skeleton, every member uses group anchor pose.
                *out_pose = group_pose;
            }
            if let Some(out_group) = self.piece_group_cache.get_mut(idx) {
                *out_group = group;
            }
        }
    }

    fn refresh_one_piece_pose<T: PuzzleTopology>(
        &mut self,
        playable: &PlayableState<T>,
        piece: PieceId,
        group: GroupId,
    ) {
        let idx = piece.as_usize();
        if let (Some(group_pose), Some(out_pose)) =
            (playable.pose_of(group), self.piece_world_pose.get_mut(idx))
        {
            *out_pose = group_pose;
        }
        if let Some(out_group) = self.piece_group_cache.get_mut(idx) {
            *out_group = group;
        }
    }

    fn rebuild_z_sorted_pieces<T: PuzzleTopology>(&mut self, playable: &PlayableState<T>) {
        self.z_sorted_pieces.clear();
        self.z_sorted_pieces.reserve(playable.piece_count());

        for group in playable.iter_z_asc() {
            self.z_sorted_pieces
                .extend(playable.logical.members_of(group));
        }
    }
}
