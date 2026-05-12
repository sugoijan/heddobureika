//! Projection/cache layer derived from `PlayableState`.

use crate::delta::PlayableDelta;
use crate::ids::{GroupId, PieceId};
use crate::playable::{PlayableState, Pose2};
use crate::topology::PuzzleTopology;
use crate::units::{AngleDeg, LengthMm};

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
#[derive(Clone)]
pub struct InteractableState {
    pub piece_world_pose: Box<[Pose2]>,
    pub piece_group_cache: Box<[GroupId]>,
    pub z_sorted_pieces: Vec<PieceId>,
    pub piece_visual_offset: Box<[[f32; 2]]>,
    pub piece_visual_rot_offset: Box<[f32]>,
}

/// Render-only visual state derived from authoritative playable state.
///
/// `authoritative` is the pull-style projection cache used for hit testing and
/// rule-neutral rendering data. `piece_visual_pose` and `piece_target_pose` are
/// lossy render-only values for prediction/smoothing and must never feed
/// authoritative snap probing.
#[derive(Clone)]
pub struct VisualState {
    pub authoritative: InteractableState,
    pub piece_visual_pose: Box<[Pose2]>,
    pub piece_target_pose: Box<[Pose2]>,
}

/// Projection layer for an optional physics shadow between authoritative
/// `InteractableState` and render-only `VisualState`.
///
/// The conceptual stack (most-exact → most-approximate):
/// 1. `LogicalState` — group membership and active edges.
/// 2. `PlayableState` — exact poses, authoritative.
/// 3. `InteractableState` — pull cache of derived piece world poses.
/// 4. *Physical state* (this trait) — optional realistic imperfections
///    (slight pose jitter, frictional drift) introduced by a physics engine.
/// 5. `VisualState` — render + transient drag preview.
///
/// Implementations are responsible for staying consistent with the
/// authoritative input: a `refresh_from_authoritative` call replaces the
/// shadow state, `step` advances any time-based simulation, and
/// `piece_physical_pose` returns the post-physics pose for a single piece.
///
/// Implementors should treat this as a write-only sink for authoritative
/// data: the physical layer never feeds back into snap probing or merge
/// decisions.
pub trait PhysicalProjection {
    /// Replaces the shadow state with values from `authoritative`. Called
    /// after any authoritative apply that mutated piece poses.
    fn refresh_from_authoritative(&mut self, authoritative: &InteractableState);

    /// Refreshes only the pieces named in `dirty` from `authoritative`. The
    /// default implementation falls back to `refresh_from_authoritative` —
    /// implementations that can do incremental refresh should override.
    fn refresh_dirty(&mut self, authoritative: &InteractableState, dirty: &[PieceId]) {
        let _ = dirty;
        self.refresh_from_authoritative(authoritative);
    }

    /// Advances any time-based simulation by `dt_seconds`. Implementations
    /// without time-based behavior may make this a no-op.
    fn step(&mut self, dt_seconds: f32);

    /// Returns the post-physics pose for a piece. The default returns the
    /// authoritative pose unmodified; implementations override to inject
    /// jitter, drift, or other physics-driven offsets.
    fn piece_physical_pose(&self, piece: PieceId) -> Pose2;
}

/// No-op `PhysicalProjection` implementation that mirrors the authoritative
/// `InteractableState` exactly. Use this as a default or for tests.
#[derive(Clone, Debug, Default)]
pub struct IdentityPhysicalState {
    poses: Box<[Pose2]>,
}

impl IdentityPhysicalState {
    pub fn with_capacity(piece_count: usize) -> Self {
        Self {
            poses: vec![Pose2::default(); piece_count].into_boxed_slice(),
        }
    }

    pub fn from_authoritative(authoritative: &InteractableState) -> Self {
        Self {
            poses: authoritative.piece_world_pose.clone(),
        }
    }

    pub fn poses(&self) -> &[Pose2] {
        &self.poses
    }
}

impl PhysicalProjection for IdentityPhysicalState {
    fn refresh_from_authoritative(&mut self, authoritative: &InteractableState) {
        if self.poses.len() != authoritative.piece_world_pose.len() {
            self.poses = authoritative.piece_world_pose.clone();
        } else {
            self.poses.clone_from_slice(&authoritative.piece_world_pose);
        }
    }

    fn refresh_dirty(&mut self, authoritative: &InteractableState, dirty: &[PieceId]) {
        if self.poses.len() != authoritative.piece_world_pose.len() {
            self.poses = authoritative.piece_world_pose.clone();
            return;
        }
        for piece in dirty.iter().copied() {
            let idx = piece.as_usize();
            if let (Some(out), Some(src)) = (
                self.poses.get_mut(idx),
                authoritative.piece_world_pose.get(idx),
            ) {
                *out = *src;
            }
        }
    }

    fn step(&mut self, _dt_seconds: f32) {}

    fn piece_physical_pose(&self, piece: PieceId) -> Pose2 {
        self.poses
            .get(piece.as_usize())
            .copied()
            .unwrap_or_default()
    }
}

impl VisualState {
    pub fn rebuild_from<T: PuzzleTopology>(playable: &PlayableState<T>) -> Self {
        let authoritative = InteractableState::rebuild_from(playable);
        let poses = authoritative.piece_world_pose.to_vec().into_boxed_slice();
        Self {
            authoritative,
            piece_visual_pose: poses.clone(),
            piece_target_pose: poses,
        }
    }

    pub fn apply_delta<T: PuzzleTopology>(
        &mut self,
        playable: &PlayableState<T>,
        delta: &PlayableDelta,
        scratch: &mut ProjectionScratch,
    ) {
        let old_len = self.authoritative.piece_world_pose.len();
        self.authoritative.apply_delta(playable, delta, scratch);
        let new_len = self.authoritative.piece_world_pose.len();

        if old_len != new_len || self.piece_visual_pose.len() != new_len {
            self.reset_visual_to_authoritative();
            return;
        }

        self.sync_targets_to_authoritative();
    }

    pub fn authoritative_piece_pose(&self) -> &[Pose2] {
        self.authoritative.piece_world_pose()
    }

    pub fn piece_visual_pose(&self) -> &[Pose2] {
        &self.piece_visual_pose
    }

    pub fn piece_target_pose(&self) -> &[Pose2] {
        &self.piece_target_pose
    }

    pub fn set_piece_visual_pose(&mut self, piece: PieceId, pose: Pose2) -> bool {
        let Some(out) = self.piece_visual_pose.get_mut(piece.as_usize()) else {
            return false;
        };
        *out = pose;
        true
    }

    pub fn set_piece_target_pose(&mut self, piece: PieceId, pose: Pose2) -> bool {
        let Some(out) = self.piece_target_pose.get_mut(piece.as_usize()) else {
            return false;
        };
        *out = pose;
        true
    }

    pub fn sync_targets_to_authoritative(&mut self) {
        if self.piece_target_pose.len() != self.authoritative.piece_world_pose.len() {
            self.reset_visual_to_authoritative();
            return;
        }
        self.piece_target_pose
            .copy_from_slice(&self.authoritative.piece_world_pose);
    }

    pub fn reset_visual_to_authoritative(&mut self) {
        let poses = self
            .authoritative
            .piece_world_pose
            .to_vec()
            .into_boxed_slice();
        self.piece_visual_pose = poses.clone();
        self.piece_target_pose = poses;
    }

    pub fn snap_piece_to_authoritative(&mut self, piece: PieceId) -> bool {
        let idx = piece.as_usize();
        let Some(authoritative) = self.authoritative.piece_world_pose.get(idx).copied() else {
            return false;
        };
        if let Some(visual) = self.piece_visual_pose.get_mut(idx) {
            *visual = authoritative;
        }
        if let Some(target) = self.piece_target_pose.get_mut(idx) {
            *target = authoritative;
        }
        true
    }

    pub fn step_toward_targets(&mut self, amount: f32) {
        let amount = amount.clamp(0.0, 1.0);
        for (visual, target) in self
            .piece_visual_pose
            .iter_mut()
            .zip(self.piece_target_pose.iter().copied())
        {
            *visual = lerp_pose(*visual, target, amount);
        }
    }
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
        for piece in playable.logical.members_of(group) {
            let idx = piece.as_usize();
            if let Some(out_pose) = self.piece_world_pose.get_mut(idx) {
                if let Some(piece_pose) = playable.piece_world_pose(piece) {
                    *out_pose = piece_pose;
                }
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
        if let Some(out_pose) = self.piece_world_pose.get_mut(idx) {
            if let Some(piece_pose) = playable.piece_world_pose(piece) {
                *out_pose = piece_pose;
            }
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

fn lerp_pose(from: Pose2, to: Pose2, amount: f32) -> Pose2 {
    let x = lerp(from.x_mm(), to.x_mm(), amount);
    let y = lerp(from.y_mm(), to.y_mm(), amount);
    let rotation = from.rotation_degrees()
        + shortest_angle_delta(from.rotation_degrees(), to.rotation_degrees()) * amount;
    Pose2 {
        x: LengthMm::try_new(x).unwrap_or(to.x),
        y: LengthMm::try_new(y).unwrap_or(to.y),
        rotation: AngleDeg::try_new(rotation).unwrap_or(to.rotation),
    }
}

fn lerp(from: f32, to: f32, amount: f32) -> f32 {
    from + (to - from) * amount
}

fn shortest_angle_delta(from: f32, to: f32) -> f32 {
    let mut delta = (to - from) % 360.0;
    if delta < -180.0 {
        delta += 360.0;
    } else if delta > 180.0 {
        delta -= 360.0;
    }
    delta
}
