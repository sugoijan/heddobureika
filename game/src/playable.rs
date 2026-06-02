//! Authoritative gameplay state layer built on top of `LogicalState`.

use crate::delta::PlayableDelta;
use crate::ids::{EdgeId, GroupId, PieceId};
use crate::logical::{LogicalInvariantError, LogicalState, LogicalStateSummary};
use crate::rotation_step::{group_symmetry_angles, next_step_canonical, StepDirection};
use crate::rules::PlayRules;
use crate::snap::{angle_matches, ActionId, MergePolicy, SnapProposal};
use crate::topology::{PieceOuterFeature, PuzzleTopology};
use crate::units::{AngleDeg, LengthMm};
use crate::z_depth::{reorder_for_fitting_depth, Aabb, Gesture};
use crate::update::{
    AppliedProposal, GroupMergeUpdate, GroupPoseUpdate, PlayableUpdateBatch,
    ProposalApplyRejection, ProposalApplyStatus,
};

/// World-space XY position.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Position2 {
    pub x: LengthMm,
    pub y: LengthMm,
}

impl Position2 {
    pub fn try_from_mm(x_mm: f32, y_mm: f32) -> Option<Self> {
        Some(Self {
            x: LengthMm::try_new(x_mm)?,
            y: LengthMm::try_new(y_mm)?,
        })
    }

    pub fn x_mm(self) -> f32 {
        self.x.as_mm_f32()
    }

    pub fn y_mm(self) -> f32 {
        self.y.as_mm_f32()
    }
}

impl Default for Position2 {
    fn default() -> Self {
        Self {
            x: LengthMm::zero(),
            y: LengthMm::zero(),
        }
    }
}

/// World-space pose for a group anchor transform.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pose2 {
    pub x: LengthMm,
    pub y: LengthMm,
    pub rotation: AngleDeg,
}

impl Pose2 {
    pub fn try_from_mm_degrees(x_mm: f32, y_mm: f32, rotation_degrees: f32) -> Option<Self> {
        Some(Self {
            x: LengthMm::try_new(x_mm)?,
            y: LengthMm::try_new(y_mm)?,
            rotation: AngleDeg::try_new(rotation_degrees)?,
        })
    }

    pub fn x_mm(self) -> f32 {
        self.x.as_mm_f32()
    }

    pub fn y_mm(self) -> f32 {
        self.y.as_mm_f32()
    }

    pub fn rotation_degrees(self) -> f32 {
        self.rotation.as_degrees_f32()
    }
}

impl Default for Pose2 {
    fn default() -> Self {
        Self {
            x: LengthMm::zero(),
            y: LengthMm::zero(),
            rotation: AngleDeg::zero(),
        }
    }
}

/// Group flip state.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum FlipState {
    #[default]
    Normal,
    Flipped,
}

/// Unrestricted gameplay actions.
///
/// Each action represents one atomic authoritative mutation from the
/// perspective of external callers. Internal join checks and join cascades are
/// part of the same atomic apply operation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PlayableAction {
    TranslateGroup {
        group: GroupId,
        drop_pos: Position2,
    },
    TransformGroupTo {
        group: GroupId,
        drop_pos: Position2,
        drop_rotation: AngleDeg,
    },
    RotateGroupTo {
        group: GroupId,
        drop_rotation: AngleDeg,
    },
    StepRotateGroupCw {
        group: GroupId,
    },
    StepRotateGroupCcw {
        group: GroupId,
    },
    UnflipGroup {
        group: GroupId,
        /// World-space point to pivot the flip about (the cursor position
        /// when the unflip comes from a click), so that point stays fixed.
        /// `None` reflects about the group anchor (no pose change).
        pivot: Option<Position2>,
    },
}

/// Restricted/admin actions.
///
/// These are intentionally separated from unrestricted gameplay actions so the
/// caller can enforce game mode policies and achievement gating.
#[derive(Clone, Debug, PartialEq)]
pub enum RestrictedPlayableAction {
    FlipGroup {
        group: GroupId,
        /// World-space point to pivot the flip about (the cursor position
        /// when the flip comes from a click), so that point stays fixed.
        /// `None` reflects about the group anchor (no pose change).
        pivot: Option<Position2>,
    },
    /// Detaches one piece into its own singleton group and immediately applies
    /// the requested pose/flip to that detached group.
    DetachPieceAsGroup {
        piece: PieceId,
        target_pose: Pose2,
        target_flip: FlipState,
    },
    /// Reorders the live `z_order` so that the groups identified by the given
    /// anchor piece ids come last (top of stack), preserving the relative
    /// order of the remaining alive groups. Anchor ids that don't resolve to
    /// alive groups are silently ignored. No-op if the order would not
    /// change.
    SetGroupOrder { anchors: Vec<u32> },
}

/// High-level progress stage for a playable state snapshot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolveStage {
    Shuffled,
    InProgress,
    Solved,
}

/// Summary of playable state progress.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PlayableStateSummary {
    pub revision: u64,
    pub logical: LogicalStateSummary,
    pub stage: SolveStage,
    pub solved: bool,
}

/// Authoritative playable-state invariant violation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlayableInvariantError {
    Logical(LogicalInvariantError),
    GroupPoseLen {
        expected: usize,
        actual: usize,
    },
    GroupFlipLen {
        expected: usize,
        actual: usize,
    },
    PieceLocalPoseLen {
        expected: usize,
        actual: usize,
    },
    ZIndexLen {
        expected: usize,
        actual: usize,
    },
    DeadGroupInZOrder {
        group: GroupId,
    },
    AliveGroupMissingZOrder {
        group: GroupId,
    },
    DuplicateGroupInZOrder {
        group: GroupId,
    },
    ZIndexMismatch {
        group: GroupId,
        expected: u32,
        actual: u32,
    },
}

impl From<LogicalInvariantError> for PlayableInvariantError {
    fn from(value: LogicalInvariantError) -> Self {
        Self::Logical(value)
    }
}

/// Authoritative gameplay layer.
///
/// Z-order is canonical in this state:
/// - `z_order` stores active groups from back to front.
/// - `z_index_of` stores O(1) index lookup into `z_order`.
///
/// Merge policy contract (to be implemented in later iterations):
/// when two groups merge, the merged group keeps the higher-z group's position.
pub struct PlayableState<T: PuzzleTopology> {
    pub logical: LogicalState<T>,
    pub group_pose: Box<[Pose2]>,
    pub group_flip: Box<[FlipState]>,
    pub piece_local_pose: Box<[Pose2]>,
    pub z_order: Vec<GroupId>,
    pub z_index_of: Box<[u32]>,
    pub rules: PlayRules,
    pub revision: u64,
    pub scratch_groups: Vec<GroupId>,
    /// Aspect ratio piece_height / piece_width used to rotate piece-count
    /// offsets in pixel-isotropic space. 1.0 = square pieces; not persisted
    /// in PlayableSnapshot, callers re-set it after restore.
    piece_aspect_ratio: f32,
}

impl<T: PuzzleTopology + Clone> Clone for PlayableState<T> {
    fn clone(&self) -> Self {
        Self {
            logical: self.logical.clone(),
            group_pose: self.group_pose.clone(),
            group_flip: self.group_flip.clone(),
            piece_local_pose: self.piece_local_pose.clone(),
            z_order: self.z_order.clone(),
            z_index_of: self.z_index_of.clone(),
            rules: self.rules,
            revision: self.revision,
            scratch_groups: self.scratch_groups.clone(),
            piece_aspect_ratio: self.piece_aspect_ratio,
        }
    }
}

impl<T: PuzzleTopology> PlayableState<T> {
    pub fn solved(topology: T, rules: PlayRules) -> Self {
        // Anchor the assembled group at the topology's canonical frame anchor
        // so the solved puzzle lines up with the frame outline — every
        // topology reports its own aligned pose, so there's no per-topology
        // special case here. Read it before `topology` is moved into the
        // solved logical state; the placeholder `unknown` topology has no
        // anchor and falls back to the origin.
        let identity = topology
            .identity_frame_anchor()
            .map(|(_, pose)| pose)
            .unwrap_or_default();
        let mut state = Self::new(LogicalState::solved(topology), rules);
        if let Some(slot) = state.group_pose.first_mut() {
            *slot = identity;
        }
        state
    }

    pub fn shuffled(topology: T, rules: PlayRules, seed: u64) -> Self {
        let mut state = Self::new(LogicalState::shuffled(topology), rules);
        state.shuffle_in_place(seed);
        state
    }

    pub fn new(logical: LogicalState<T>, rules: PlayRules) -> Self {
        let count = logical.piece_count();
        let z_order = logical.active_group_ids().collect::<Vec<_>>();
        let mut z_index_of = vec![u32::MAX; count].into_boxed_slice();
        for (idx, group) in z_order.iter().copied().enumerate() {
            if let Some(slot) = z_index_of.get_mut(group.as_usize()) {
                *slot = idx as u32;
            }
        }

        let piece_local_pose = build_piece_local_poses(&logical.topology);

        Self {
            logical,
            group_pose: vec![Pose2::default(); count].into_boxed_slice(),
            group_flip: vec![FlipState::Normal; count].into_boxed_slice(),
            piece_local_pose,
            z_order,
            z_index_of,
            rules,
            revision: 0,
            scratch_groups: Vec::with_capacity(count),
            piece_aspect_ratio: 1.0,
        }
    }

    pub fn piece_aspect_ratio(&self) -> f32 {
        self.piece_aspect_ratio
    }

    pub fn set_piece_aspect_ratio(&mut self, aspect: f32) {
        if aspect.is_finite() && aspect > 0.0 {
            self.piece_aspect_ratio = aspect;
        }
    }

    pub fn piece_count(&self) -> usize {
        self.logical.piece_count()
    }

    pub fn summary(&self) -> PlayableStateSummary {
        let logical = self.logical.summary();
        let stage = self.solve_stage();
        PlayableStateSummary {
            revision: self.revision,
            logical,
            stage,
            solved: stage == SolveStage::Solved,
        }
    }

    pub fn solve_stage(&self) -> SolveStage {
        if self.is_solved() {
            return SolveStage::Solved;
        }
        if self.logical.active_edge_count() == 0 && self.logical.group_count() == self.piece_count()
        {
            return SolveStage::Shuffled;
        }
        SolveStage::InProgress
    }

    pub fn is_solved(&self) -> bool {
        self.logical.is_solved()
    }

    pub fn validate(&self) -> Result<(), PlayableInvariantError> {
        self.logical.validate()?;
        let piece_count = self.logical.piece_count();
        let group_count = self.logical.groups.len();

        if self.group_pose.len() != group_count {
            return Err(PlayableInvariantError::GroupPoseLen {
                expected: group_count,
                actual: self.group_pose.len(),
            });
        }
        if self.group_flip.len() != group_count {
            return Err(PlayableInvariantError::GroupFlipLen {
                expected: group_count,
                actual: self.group_flip.len(),
            });
        }
        if self.piece_local_pose.len() != piece_count {
            return Err(PlayableInvariantError::PieceLocalPoseLen {
                expected: piece_count,
                actual: self.piece_local_pose.len(),
            });
        }
        if self.z_index_of.len() != group_count {
            return Err(PlayableInvariantError::ZIndexLen {
                expected: group_count,
                actual: self.z_index_of.len(),
            });
        }

        let mut seen = vec![false; group_count];
        for (idx, group) in self.z_order.iter().copied().enumerate() {
            let group_idx = group.as_usize();
            if group_idx >= group_count
                || !self
                    .logical
                    .groups
                    .get(group_idx)
                    .map(|slot| slot.alive)
                    .unwrap_or(false)
            {
                return Err(PlayableInvariantError::DeadGroupInZOrder { group });
            }
            if seen[group_idx] {
                return Err(PlayableInvariantError::DuplicateGroupInZOrder { group });
            }
            seen[group_idx] = true;
            let actual = self.z_index_of[group_idx];
            if actual != idx as u32 {
                return Err(PlayableInvariantError::ZIndexMismatch {
                    group,
                    expected: idx as u32,
                    actual,
                });
            }
        }

        for (group_idx, slot) in self.logical.groups.iter().copied().enumerate() {
            if slot.alive && !seen[group_idx] {
                return Err(PlayableInvariantError::AliveGroupMissingZOrder {
                    group: GroupId(group_idx as u32),
                });
            }
        }

        Ok(())
    }

    pub fn iter_z_asc(&self) -> impl Iterator<Item = GroupId> + '_ {
        self.z_order.iter().copied()
    }

    pub fn iter_z_desc(&self) -> impl Iterator<Item = GroupId> + '_ {
        self.z_order.iter().rev().copied()
    }

    pub fn pose_of(&self, group: GroupId) -> Option<Pose2> {
        self.group_pose.get(group.as_usize()).copied()
    }

    pub fn flip_of(&self, group: GroupId) -> Option<FlipState> {
        self.group_flip.get(group.as_usize()).copied()
    }

    pub fn piece_local_pose_of(&self, piece: PieceId) -> Option<Pose2> {
        self.piece_local_pose.get(piece.as_usize()).copied()
    }

    pub fn anchor_piece_of_group(&self, group: GroupId) -> Option<PieceId> {
        self.logical.members_of(group).min()
    }

    pub fn piece_world_pose(&self, piece: PieceId) -> Option<Pose2> {
        let group = self.logical.group_of(piece)?;
        let group_pose = self.pose_of(group)?;
        self.piece_world_pose_with_group_pose(group, group_pose, piece)
    }

    pub fn piece_world_pose_with_group_pose(
        &self,
        group: GroupId,
        group_pose: Pose2,
        piece: PieceId,
    ) -> Option<Pose2> {
        let anchor = self.anchor_piece_of_group(group)?;
        let anchor_local = self.piece_local_pose_of(anchor)?;
        let piece_local = self.piece_local_pose_of(piece)?;
        let mut dx = piece_local.x_mm() - anchor_local.x_mm();
        let dy = piece_local.y_mm() - anchor_local.y_mm();
        if self.flip_of(group) == Some(FlipState::Flipped) {
            dx = -dx;
        }
        let (rx, ry) = rotate_offset_with_aspect(
            dx,
            dy,
            group_pose.rotation_degrees(),
            self.piece_aspect_ratio,
        );
        let rotation = AngleDeg::try_new(
            group_pose.rotation_degrees() + piece_local.rotation_degrees()
                - anchor_local.rotation_degrees(),
        )?;
        Pose2::try_from_mm_degrees(
            group_pose.x_mm() + rx,
            group_pose.y_mm() + ry,
            rotation.as_degrees_f32(),
        )
    }

    pub fn group_pose_to_place_piece(
        &self,
        group: GroupId,
        piece: PieceId,
        target_piece_pose: Pose2,
        group_rotation: AngleDeg,
    ) -> Option<Pose2> {
        let anchor = self.anchor_piece_of_group(group)?;
        let anchor_local = self.piece_local_pose_of(anchor)?;
        let piece_local = self.piece_local_pose_of(piece)?;
        let mut dx = piece_local.x_mm() - anchor_local.x_mm();
        let dy = piece_local.y_mm() - anchor_local.y_mm();
        if self.flip_of(group) == Some(FlipState::Flipped) {
            dx = -dx;
        }
        let (rx, ry) = rotate_offset_with_aspect(
            dx,
            dy,
            group_rotation.as_degrees_f32(),
            self.piece_aspect_ratio,
        );
        Pose2::try_from_mm_degrees(
            target_piece_pose.x_mm() - rx,
            target_piece_pose.y_mm() - ry,
            group_rotation.as_degrees_f32(),
        )
    }

    /// Sets a group's flip state, optionally pivoting the reflection about a
    /// world-space point (the cursor position when the toggle comes from a
    /// click) so that point stays fixed under the cursor.
    ///
    /// A flip renders as a screen-space horizontal mirror (`scale(-1, 1)`)
    /// applied *after* the group's rotation, so a piece-local point maps to
    /// `world = A(P) + S·Rot(p − c₀)`, where `A(P)` is the anchor's world
    /// position (affine in the pose `P`), `Rot` is the group rotation and
    /// `S` is the screen-axis mirror. Pinning a world point `W` across a
    /// toggle requires `A(P') = W − S·(W − A(P))`; the `Rot(p − c₀)` term
    /// cancels, so the correction is independent of rotation and aspect and
    /// reduces to `P'ₓ = 2·pivotₓ − Pₓ` with `P'_y = P_y` in pose-mm units.
    ///
    /// With `pivot == None` the pose is left untouched (reflect about the
    /// group anchor) — used for non-interactive toggles. No-op when the
    /// group is already in `target`.
    fn set_group_flip(
        &mut self,
        group: GroupId,
        target: FlipState,
        pivot: Option<Position2>,
        delta: &mut PlayableDelta,
    ) {
        let Some(current) = self.flip_of(group) else {
            return;
        };
        if current == target {
            return;
        }
        // Only individual pieces may be flipped — a multi-piece group is
        // never put into the flipped state. (Flipped pieces are also barred
        // from snapping/joining, so a group can't become flipped by merging
        // either.) This matches the server's singleton-only flip check and
        // keeps the click-pivot adjustment, which is exact for singletons,
        // the only case it ever runs on.
        if target == FlipState::Flipped && self.logical.members_of(group).take(2).count() != 1 {
            return;
        }
        if let (Some(pivot), Some(pose)) = (pivot, self.pose_of(group)) {
            // Mirror is screen-x-aligned, so only the x coordinate moves and
            // only the pivot's x matters; the conversion to pose units cancels
            // the per-axis pixel scale, leaving no rotation/aspect dependence.
            let new_x = 2.0 * pivot.x_mm() - pose.x_mm();
            if let Some(pose_mut) = self.group_pose.get_mut(group.as_usize()) {
                if let Some(updated) =
                    Pose2::try_from_mm_degrees(new_x, pose_mut.y_mm(), pose_mut.rotation_degrees())
                {
                    *pose_mut = updated;
                }
            }
        }
        if let Some(group_flip) = self.group_flip.get_mut(group.as_usize()) {
            *group_flip = target;
        }
        mark_group_dirty(self, group, delta);
    }

    pub fn probe_snaps(&self, mover_group: GroupId, proposed_pose: Pose2) -> SnapProposal {
        crate::snap::probe_snaps(self, mover_group, proposed_pose)
    }

    pub fn rebase_proposal(&self, proposal: &SnapProposal) -> SnapProposal {
        let mut rebased = self.probe_snaps(proposal.mover_group, proposal.proposed_pose);
        rebased.action_id = proposal.action_id;
        rebased
    }

    pub fn apply_proposal(
        &mut self,
        proposal: &SnapProposal,
        policy: MergePolicy,
    ) -> PlayableDelta {
        self.apply_proposal_with_batch(proposal, policy).delta
    }

    /// Applies an unrestricted gameplay action without consulting the snap
    /// probe. Used for live-move/transform updates where the authoritative
    /// snap evaluation is deferred to the finalize call (drop/release).
    pub fn apply_action_only(
        &mut self,
        action: PlayableAction,
        action_id: Option<ActionId>,
    ) -> PlayableUpdateBatch {
        let revision_before = self.revision;
        let Some((group, _)) = self.proposed_pose_for_action(action) else {
            return rejected_action_batch(
                action_id,
                revision_before,
                group_from_action(action),
                ProposalApplyRejection::MoverGroupMissing,
            );
        };

        if !self.group_is_alive(group) {
            return rejected_action_batch(
                action_id,
                revision_before,
                group,
                ProposalApplyRejection::MoverGroupMissing,
            );
        }

        self.apply_action_only_batch(action, action_id, group)
    }

    /// Applies a restricted/admin action and returns a fully populated
    /// `PlayableUpdateBatch` with an `AppliedProposal` describing the result.
    /// Status is always `ActionOnly` (restricted actions never run snap probes).
    pub fn apply_restricted_action_batch(
        &mut self,
        action: RestrictedPlayableAction,
        action_id: Option<ActionId>,
    ) -> PlayableUpdateBatch {
        let revision_before = self.revision;
        let mover_group = match &action {
            RestrictedPlayableAction::FlipGroup { group, .. } => *group,
            RestrictedPlayableAction::DetachPieceAsGroup { piece, .. } => GroupId(piece.as_u32()),
            RestrictedPlayableAction::SetGroupOrder { anchors } => anchors
                .first()
                .copied()
                .and_then(|anchor| self.logical.group_of(PieceId(anchor)))
                .unwrap_or(GroupId(0)),
        };
        let delta = self.apply_restricted_action(action);
        let final_group_poses = touched_group_poses(self, &delta);
        let deactivated_edges = delta.deactivated_edges.iter().copied().collect();
        let mut applied = AppliedProposal {
            action_id,
            base_revision: revision_before,
            applied_revision: self.revision,
            status: ProposalApplyStatus::ActionOnly,
            rejection: None,
            rebased: false,
            mover_group,
            fixed_group: None,
            activated_edges: Vec::new(),
            deactivated_edges,
            merged_groups: Vec::new(),
            final_group_poses,
        };
        if delta.revision == revision_before {
            applied.status = ProposalApplyStatus::Noop;
        }

        PlayableUpdateBatch {
            revision_before,
            revision_after: self.revision,
            delta,
            proposal: applied,
        }
    }

    pub fn apply_action_with_snap(
        &mut self,
        action: PlayableAction,
        action_id: Option<ActionId>,
        policy: MergePolicy,
    ) -> PlayableUpdateBatch {
        let revision_before = self.revision;
        let Some((group, proposed_pose)) = self.proposed_pose_for_action(action) else {
            return rejected_action_batch(
                action_id,
                revision_before,
                group_from_action(action),
                ProposalApplyRejection::MoverGroupMissing,
            );
        };

        if !self.group_is_alive(group) {
            return rejected_action_batch(
                action_id,
                revision_before,
                group,
                ProposalApplyRejection::MoverGroupMissing,
            );
        }

        if action_can_probe_for_snap(action) {
            let mut proposal = self.probe_snaps(group, proposed_pose);
            proposal.action_id = action_id;
            if !proposal.activate_edges.is_empty() {
                return self.apply_proposal_with_batch(&proposal, policy);
            }
        }

        self.apply_action_only_batch(action, action_id, group)
    }

    pub fn apply_proposal_with_batch(
        &mut self,
        proposal: &SnapProposal,
        policy: MergePolicy,
    ) -> PlayableUpdateBatch {
        let revision_before = self.revision;
        let original_base_revision = proposal.base_revision;
        let proposal = self.rebase_proposal(proposal);
        let rebased = original_base_revision != revision_before;
        let mut delta = PlayableDelta::for_revision(revision_before);
        let mut applied = AppliedProposal {
            action_id: proposal.action_id,
            base_revision: original_base_revision,
            applied_revision: revision_before,
            status: ProposalApplyStatus::Noop,
            rejection: None,
            rebased,
            mover_group: proposal.mover_group,
            fixed_group: proposal.fixed_group,
            activated_edges: Vec::new(),
            deactivated_edges: Vec::new(),
            merged_groups: Vec::new(),
            final_group_poses: Vec::new(),
        };

        if proposal.activate_edges.is_empty() {
            applied.status = if proposal.rejections.is_empty() {
                ProposalApplyStatus::Noop
            } else {
                ProposalApplyStatus::Rejected
            };
            applied.rejection = proposal_apply_rejection(&proposal);
            return PlayableUpdateBatch {
                revision_before,
                revision_after: revision_before,
                delta,
                proposal: applied,
            };
        }

        if self
            .logical
            .groups
            .get(proposal.mover_group.as_usize())
            .map(|slot| slot.alive)
            .unwrap_or(false)
        {
            if let Some(pose) = self.group_pose.get_mut(proposal.mover_group.as_usize()) {
                *pose = proposal.desired_pose;
                mark_group_dirty(self, proposal.mover_group, &mut delta);
            }
        } else {
            applied.status = ProposalApplyStatus::Rejected;
            applied.rejection = Some(ProposalApplyRejection::MoverGroupMissing);
            return PlayableUpdateBatch {
                revision_before,
                revision_after: revision_before,
                delta,
                proposal: applied,
            };
        }

        let was_solved = self.is_solved();
        self.revision = self.revision.wrapping_add(1);
        delta.revision = self.revision;
        applied.applied_revision = self.revision;

        let mut last_keep = proposal.mover_group;
        for edge in proposal.activate_edges.iter().copied() {
            let keep = match policy {
                MergePolicy::KeepMoverGroup => proposal.mover_group,
                MergePolicy::KeepFixedGroup => proposal.fixed_group.unwrap_or(proposal.mover_group),
                MergePolicy::KeepHigherZ => self
                    .higher_z_group_for_edge(edge)
                    .unwrap_or(proposal.mover_group),
                MergePolicy::KeepLowerGroupId => self
                    .lower_group_for_edge(edge)
                    .unwrap_or(proposal.mover_group),
            };
            let post_merge_pose = self.merged_anchor_pose_for_edge(edge, keep);
            if let Some(merge) = self.logical.activate_edge_prefer_group(edge, keep) {
                if merge.edge_changed {
                    delta.dirty_edges.push(edge);
                    applied.activated_edges.push(edge);
                }
                last_keep = merge.keep;
                if let Some(absorbed) = merge.absorbed {
                    if let (Some(pose), Some(group_pose)) = (
                        post_merge_pose,
                        self.group_pose.get_mut(merge.keep.as_usize()),
                    ) {
                        *group_pose = pose;
                    }
                    applied.merged_groups.push(GroupMergeUpdate {
                        keep: merge.keep,
                        absorbed,
                    });
                    self.remove_group_from_z_order(absorbed);
                    delta.z_order_changed = true;
                    delta.membership_changed = true;
                    mark_group_dirty(self, merge.keep, &mut delta);
                    mark_group_dirty(self, absorbed, &mut delta);
                }
            }
        }

        self.cascade_aligned_joins(last_keep, &mut delta, &mut applied);
        if let Some(final_group) = self.current_group_for(last_keep) {
            self.apply_topology_frame_snap(final_group, !was_solved, &mut delta);
        }
        delta.solved_changed = was_solved != self.is_solved();
        applied.status = if applied.activated_edges.is_empty() {
            ProposalApplyStatus::Noop
        } else if rebased {
            ProposalApplyStatus::Rebased
        } else {
            ProposalApplyStatus::Accepted
        };
        applied.final_group_poses = touched_group_poses(self, &delta);

        PlayableUpdateBatch {
            revision_before,
            revision_after: self.revision,
            delta,
            proposal: applied,
        }
    }

    /// Applies an unrestricted action's pose mutation only — no snap probe,
    /// no join cascade. Returns a minimal `PlayableDelta` with the dirty
    /// group/piece set.
    ///
    /// Callers that want snap-aware behavior should use
    /// `apply_action_with_snap` or `apply_action_only` (the batch-returning
    /// variant). This method is preserved for tests and direct callers that
    /// explicitly want raw pose mutation.
    pub fn apply_action(&mut self, action: PlayableAction) -> PlayableDelta {
        self.revision = self.revision.wrapping_add(1);
        let mut delta = PlayableDelta::for_revision(self.revision);

        let anchor_group = match action {
            PlayableAction::TranslateGroup { group, drop_pos } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.x = drop_pos.x;
                    pose.y = drop_pos.y;
                    mark_group_dirty(self, group, &mut delta);
                }
                group
            }
            PlayableAction::TransformGroupTo {
                group,
                drop_pos,
                drop_rotation,
            } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.x = drop_pos.x;
                    pose.y = drop_pos.y;
                    pose.rotation = drop_rotation;
                    mark_group_dirty(self, group, &mut delta);
                }
                group
            }
            PlayableAction::RotateGroupTo {
                group,
                drop_rotation,
            } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.rotation = drop_rotation;
                    mark_group_dirty(self, group, &mut delta);
                }
                group
            }
            PlayableAction::StepRotateGroupCw { group } => {
                if let Some(next_rot) = self.next_step_rotation(group, true) {
                    if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                        pose.rotation = next_rot;
                        mark_group_dirty(self, group, &mut delta);
                    }
                }
                group
            }
            PlayableAction::StepRotateGroupCcw { group } => {
                if let Some(next_rot) = self.next_step_rotation(group, false) {
                    if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                        pose.rotation = next_rot;
                        mark_group_dirty(self, group, &mut delta);
                    }
                }
                group
            }
            PlayableAction::UnflipGroup { group, pivot } => {
                self.set_group_flip(group, FlipState::Normal, pivot, &mut delta);
                group
            }
        };

        self.apply_atomic_join_and_cascade(anchor_group, &mut delta);
        delta
    }

    /// Applies a restricted/admin action's primary mutation. No snap probe,
    /// no cascade. See `apply_action` for the rationale.
    pub fn apply_restricted_action(&mut self, action: RestrictedPlayableAction) -> PlayableDelta {
        self.revision = self.revision.wrapping_add(1);
        let mut delta = PlayableDelta::for_revision(self.revision);

        let anchor_group = match action {
            RestrictedPlayableAction::FlipGroup { group, pivot } => {
                self.set_group_flip(group, FlipState::Flipped, pivot, &mut delta);
                group
            }
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece,
                target_pose,
                target_flip,
            } => self.detach_piece_as_group(piece, target_pose, target_flip, &mut delta),
            RestrictedPlayableAction::SetGroupOrder { anchors } => {
                if self.set_z_order_by_anchors(&anchors) {
                    delta.z_order_changed = true;
                }
                anchors
                    .first()
                    .copied()
                    .and_then(|anchor| self.logical.group_of(PieceId(anchor)))
                    .unwrap_or(GroupId(0))
            }
        };

        self.apply_atomic_join_and_cascade(anchor_group, &mut delta);
        delta
    }

    /// Reorders `z_order` so that the groups containing the given anchor
    /// pieces come last (i.e. on top). Anchors that don't resolve to alive
    /// groups are silently skipped. Returns `true` when the order changed,
    /// `false` when the order would be unchanged.
    pub fn set_z_order_by_anchors(&mut self, anchors: &[u32]) -> bool {
        if anchors.is_empty() {
            return false;
        }
        let before = self.z_order.clone();
        let mut promoted = Vec::with_capacity(anchors.len());
        for anchor in anchors {
            let Some(group) = self.logical.group_of(PieceId(*anchor)) else {
                continue;
            };
            if !before.contains(&group) || promoted.contains(&group) {
                continue;
            }
            promoted.push(group);
        }
        let mut next = Vec::with_capacity(before.len());
        for group in &before {
            if !promoted.contains(group) {
                next.push(*group);
            }
        }
        next.extend(promoted);
        if next == before {
            return false;
        }
        self.z_order = next;
        self.rebuild_z_indices_from_snapshot();
        true
    }

    /// Reorders `z_order` so that the groups containing the given anchor pieces
    /// come first (i.e. on the bottom). Anchors that don't resolve to alive
    /// groups are silently skipped. The demoted groups keep their relative
    /// order. Returns `true` when the order changed, `false` otherwise. Mirror
    /// of [`set_z_order_by_anchors`](Self::set_z_order_by_anchors).
    pub fn send_to_back_by_anchors(&mut self, anchors: &[u32]) -> bool {
        if anchors.is_empty() {
            return false;
        }
        let before = self.z_order.clone();
        let mut demoted = Vec::with_capacity(anchors.len());
        for anchor in anchors {
            let Some(group) = self.logical.group_of(PieceId(*anchor)) else {
                continue;
            };
            if !before.contains(&group) || demoted.contains(&group) {
                continue;
            }
            demoted.push(group);
        }
        let mut next = Vec::with_capacity(before.len());
        next.extend(demoted.iter().copied());
        for group in &before {
            if !demoted.contains(group) {
                next.push(*group);
            }
        }
        if next == before {
            return false;
        }
        self.z_order = next;
        self.rebuild_z_indices_from_snapshot();
        true
    }

    /// World-space axis-aligned bounding box (pose-mm) of a group: the union of
    /// its members' rotated bounding rectangles. `None` if the group has no
    /// placeable members. Deterministic from topology + poses only, so client
    /// and server agree. See [`crate::z_depth`].
    pub fn group_world_aabb(&self, group: GroupId) -> Option<Aabb> {
        let mut aabb = Aabb::empty();
        let mut any = false;
        for piece in self.logical.members_of(group) {
            let Some(pose) = self.piece_world_pose(piece) else {
                continue;
            };
            let (ex, ey) = self.logical.topology.piece_extent_mm(piece);
            let hx = ex.as_mm_f32() * 0.5;
            let hy = ey.as_mm_f32() * 0.5;
            // Rotated-rectangle AABB half-extents.
            let (sin, cos) = pose.rotation_degrees().to_radians().sin_cos();
            let ax = hx * cos.abs() + hy * sin.abs();
            let ay = hx * sin.abs() + hy * cos.abs();
            aabb = aabb.union(Aabb::from_center_half(pose.x_mm(), pose.y_mm(), ax, ay));
            any = true;
        }
        if any {
            Some(aabb)
        } else {
            None
        }
    }

    /// Drag-start gesture: bring the group containing `anchors[0]` as far toward
    /// the front as possible without (near-)completely hiding any overlapping
    /// piece (those it would mostly cover are lifted above it). Geometry-aware
    /// replacement for [`set_z_order_by_anchors`](Self::set_z_order_by_anchors)
    /// at the drag-start call site. Returns `true` when the order changed.
    pub fn bring_forward_to_fitting_depth(&mut self, anchors: &[u32]) -> bool {
        self.apply_fitting_depth(anchors, Gesture::BringForward)
    }

    /// Shake gesture: send the group containing `anchors[0]` to its "fitting
    /// depth" — as far back as possible to reveal pieces under it, but never so
    /// far that it becomes (near-)completely hidden — and re-sort overlapping
    /// pieces so none is left hidden behind a larger one. Geometry-aware
    /// replacement for [`send_to_back_by_anchors`](Self::send_to_back_by_anchors)
    /// at the shake call site. Returns `true` when the order changed.
    pub fn send_backward_to_fitting_depth(&mut self, anchors: &[u32]) -> bool {
        self.apply_fitting_depth(anchors, Gesture::SendBackward)
    }

    fn apply_fitting_depth(&mut self, anchors: &[u32], gesture: Gesture) -> bool {
        let Some(&anchor) = anchors.first() else {
            return false;
        };
        let Some(group) = self.logical.group_of(PieceId(anchor)) else {
            return false;
        };
        let order = self.z_order.clone();
        let Some(g_pos) = order.iter().position(|&gid| gid == group) else {
            return false;
        };
        let aabbs: Vec<Aabb> = order
            .iter()
            .map(|&gid| self.group_world_aabb(gid).unwrap_or_else(Aabb::empty))
            .collect();
        let Some(new_order) = reorder_for_fitting_depth(&order, &aabbs, g_pos, gesture) else {
            return false;
        };
        self.z_order = new_order;
        self.rebuild_z_indices_from_snapshot();
        true
    }

    pub fn next_step_rotation(&self, group: GroupId, clockwise: bool) -> Option<AngleDeg> {
        let current = self.pose_of(group)?.rotation;
        let topology = &self.logical.topology;
        let members: Vec<PieceId> = self.logical.members_of(group).collect();
        if members.is_empty() {
            return None;
        }
        let entries: Vec<(&[AngleDeg], _)> = members
            .iter()
            .map(|piece| {
                (
                    topology.symmetry_angles(*piece),
                    topology.symmetry_strength(*piece),
                )
            })
            .collect();
        let group_angles = group_symmetry_angles(&entries);

        let direction = if clockwise {
            StepDirection::Cw
        } else {
            StepDirection::Ccw
        };
        Some(next_step_canonical(
            &group_angles,
            current,
            self.rules.rotation_snap_tolerance,
            direction,
        ))
    }

    fn proposed_pose_for_action(&self, action: PlayableAction) -> Option<(GroupId, Pose2)> {
        match action {
            PlayableAction::TranslateGroup { group, drop_pos } => {
                let mut pose = self.pose_of(group)?;
                pose.x = drop_pos.x;
                pose.y = drop_pos.y;
                Some((group, pose))
            }
            PlayableAction::TransformGroupTo {
                group,
                drop_pos,
                drop_rotation,
            } => {
                let mut pose = self.pose_of(group)?;
                pose.x = drop_pos.x;
                pose.y = drop_pos.y;
                pose.rotation = drop_rotation;
                Some((group, pose))
            }
            PlayableAction::RotateGroupTo {
                group,
                drop_rotation,
            } => {
                let mut pose = self.pose_of(group)?;
                pose.rotation = drop_rotation;
                Some((group, pose))
            }
            PlayableAction::StepRotateGroupCw { group } => {
                let mut pose = self.pose_of(group)?;
                pose.rotation = self.next_step_rotation(group, true)?;
                Some((group, pose))
            }
            PlayableAction::StepRotateGroupCcw { group } => {
                let mut pose = self.pose_of(group)?;
                pose.rotation = self.next_step_rotation(group, false)?;
                Some((group, pose))
            }
            PlayableAction::UnflipGroup { group, .. } => Some((group, self.pose_of(group)?)),
        }
    }

    fn group_is_alive(&self, group: GroupId) -> bool {
        self.logical
            .groups
            .get(group.as_usize())
            .map(|slot| slot.alive)
            .unwrap_or(false)
    }

    fn apply_action_only_batch(
        &mut self,
        action: PlayableAction,
        action_id: Option<ActionId>,
        group: GroupId,
    ) -> PlayableUpdateBatch {
        let revision_before = self.revision;
        let was_solved = self.is_solved();
        self.revision = self.revision.wrapping_add(1);
        let mut delta = PlayableDelta::for_revision(self.revision);

        match action {
            PlayableAction::TranslateGroup { group, drop_pos } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.x = drop_pos.x;
                    pose.y = drop_pos.y;
                    mark_group_dirty(self, group, &mut delta);
                }
            }
            PlayableAction::TransformGroupTo {
                group,
                drop_pos,
                drop_rotation,
            } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.x = drop_pos.x;
                    pose.y = drop_pos.y;
                    pose.rotation = drop_rotation;
                    mark_group_dirty(self, group, &mut delta);
                }
            }
            PlayableAction::RotateGroupTo {
                group,
                drop_rotation,
            } => {
                if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                    pose.rotation = drop_rotation;
                    mark_group_dirty(self, group, &mut delta);
                }
            }
            PlayableAction::StepRotateGroupCw { group } => {
                if let Some(next_rot) = self.next_step_rotation(group, true) {
                    if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                        pose.rotation = next_rot;
                        mark_group_dirty(self, group, &mut delta);
                    }
                }
            }
            PlayableAction::StepRotateGroupCcw { group } => {
                if let Some(next_rot) = self.next_step_rotation(group, false) {
                    if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                        pose.rotation = next_rot;
                        mark_group_dirty(self, group, &mut delta);
                    }
                }
            }
            PlayableAction::UnflipGroup { group, pivot } => {
                self.set_group_flip(group, FlipState::Normal, pivot, &mut delta);
            }
        }

        self.apply_topology_frame_snap(group, !was_solved, &mut delta);
        delta.solved_changed = was_solved != self.is_solved();
        let final_group_poses = touched_group_poses(self, &delta);
        PlayableUpdateBatch {
            revision_before,
            revision_after: self.revision,
            delta,
            proposal: AppliedProposal {
                action_id,
                base_revision: revision_before,
                applied_revision: self.revision,
                status: ProposalApplyStatus::ActionOnly,
                rejection: None,
                rebased: false,
                mover_group: group,
                fixed_group: None,
                activated_edges: Vec::new(),
                deactivated_edges: Vec::new(),
                merged_groups: Vec::new(),
                final_group_poses,
            },
        }
    }

    fn shuffle_in_place(&mut self, mut seed: u64) {
        for group_idx in 0..self.group_pose.len() {
            if !self
                .logical
                .groups
                .get(group_idx)
                .map(|slot| slot.alive)
                .unwrap_or(false)
            {
                continue;
            }

            let offset_x_mm = rand_range(&mut seed, -12.0, 12.0);
            let offset_y_mm = rand_range(&mut seed, -12.0, 12.0);
            let rotation_step = (next_u32(&mut seed) & 0b11) as f32;
            let rotation_degrees = rotation_step * 90.0;
            let flipped = (next_u32(&mut seed) & 0b11) == 0;

            if let Some(pose) = self.group_pose.get_mut(group_idx) {
                if let Some(x) = LengthMm::try_new(offset_x_mm) {
                    pose.x = x;
                }
                if let Some(y) = LengthMm::try_new(offset_y_mm) {
                    pose.y = y;
                }
                if let Some(rotation) = AngleDeg::try_new(rotation_degrees) {
                    pose.rotation = rotation;
                }
            }
            if let Some(group_flip) = self.group_flip.get_mut(group_idx) {
                *group_flip = if flipped {
                    FlipState::Flipped
                } else {
                    FlipState::Normal
                };
            }
        }
    }

    fn detach_piece_as_group(
        &mut self,
        piece: PieceId,
        target_pose: Pose2,
        target_flip: FlipState,
        delta: &mut PlayableDelta,
    ) -> GroupId {
        let piece_idx = piece.as_usize();
        let new_group = GroupId(piece.as_u32());

        let Some(old_group) = self.logical.group_of(piece) else {
            return new_group;
        };
        let old_pose = self.pose_of(old_group).unwrap_or_default();
        let old_flip = self.flip_of(old_group).unwrap_or_default();
        let old_members = self.logical.members_of(old_group).collect::<Vec<_>>();
        let old_world_poses = old_members
            .iter()
            .copied()
            .filter_map(|member| self.piece_world_pose(member).map(|pose| (member, pose)))
            .collect::<Vec<_>>();
        let deactivated_edges = self.active_incident_edges(piece).collect::<Vec<_>>();
        let connectivity_changed = self.logical.detach_piece(piece);

        if connectivity_changed {
            delta.membership_changed = true;
            delta.dirty_edges.extend(deactivated_edges.iter().copied());
            delta
                .deactivated_edges
                .extend(deactivated_edges.iter().copied());
            let mut touched_groups = Vec::<GroupId>::new();
            for member in old_members.iter().copied() {
                if let Some(group) = self.logical.group_of(member) {
                    if !touched_groups.contains(&group) {
                        let anchor = self.anchor_piece_of_group(group).unwrap_or(member);
                        let anchor_pose = old_world_poses
                            .iter()
                            .find_map(|(old_member, pose)| (*old_member == anchor).then_some(*pose))
                            .unwrap_or(old_pose);
                        if let Some(pose) = self.group_pose.get_mut(group.as_usize()) {
                            *pose = anchor_pose;
                        }
                        if let Some(group_flip) = self.group_flip.get_mut(group.as_usize()) {
                            *group_flip = old_flip;
                        }
                        mark_group_dirty(self, group, delta);
                        touched_groups.push(group);
                    }
                    if group != old_group {
                        delta.dirty_pieces.push(member);
                    }
                }
            }
            if self.sync_z_order_to_logical() {
                delta.z_order_changed = true;
            }
        }

        let Some(piece_slot) = self.logical.pieces.get_mut(piece_idx) else {
            return new_group;
        };
        let current_group = piece_slot.group;

        if current_group != new_group {
            if let Some(current_slot) = self.logical.groups.get_mut(current_group.as_usize()) {
                if current_slot.size > 0 {
                    current_slot.size -= 1;
                }
                if current_slot.size == 0 {
                    current_slot.alive = false;
                }
            }

            if let Some(new_slot) = self.logical.groups.get_mut(new_group.as_usize()) {
                new_slot.alive = true;
                new_slot.size = 1;
            }

            piece_slot.group = new_group;
            delta.membership_changed = true;
        }

        if let Some(pose) = self.group_pose.get_mut(new_group.as_usize()) {
            *pose = target_pose;
        }
        if let Some(group_flip) = self.group_flip.get_mut(new_group.as_usize()) {
            *group_flip = target_flip;
        }

        if current_group != new_group || connectivity_changed {
            if self.sync_z_order_to_logical() {
                delta.z_order_changed = true;
            }

            mark_group_dirty(self, current_group, delta);
        }

        mark_group_dirty(self, new_group, delta);
        delta.dirty_pieces.push(piece);
        new_group
    }

    fn remove_group_from_z_order(&mut self, group: GroupId) {
        let index = self
            .z_index_of
            .get(group.as_usize())
            .copied()
            .unwrap_or(u32::MAX);
        if index == u32::MAX {
            return;
        }
        let index = index as usize;
        if index < self.z_order.len() {
            self.z_order.remove(index);
            self.rebuild_z_indices();
        }
    }

    fn rebuild_z_indices(&mut self) {
        for index in &mut self.z_index_of {
            *index = u32::MAX;
        }
        for (idx, group) in self.z_order.iter().copied().enumerate() {
            if let Some(slot) = self.z_index_of.get_mut(group.as_usize()) {
                *slot = idx as u32;
            }
        }
    }

    /// Recomputes `z_index_of` from the current `z_order` ordering. Callers
    /// that mutate `z_order` directly (snapshot restore, wire-update apply)
    /// must invoke this to keep the O(1) lookup in sync.
    pub fn rebuild_z_indices_from_snapshot(&mut self) {
        self.rebuild_z_indices();
    }

    fn sync_z_order_to_logical(&mut self) -> bool {
        let before = self.z_order.clone();
        self.z_order.retain(|group| {
            self.logical
                .groups
                .get(group.as_usize())
                .map(|slot| slot.alive)
                .unwrap_or(false)
        });

        for (idx, slot) in self.logical.groups.iter().copied().enumerate() {
            let group = GroupId(idx as u32);
            if slot.alive && !self.z_order.contains(&group) {
                self.z_order.push(group);
            }
        }

        self.rebuild_z_indices();
        self.z_order != before
    }

    /// Hook for any post-action work that should happen as part of the same
    /// atomic apply step. Currently a no-op: `apply_action` and
    /// `apply_restricted_action` deliberately do **not** auto-snap or cascade
    /// — that's what `apply_action_with_snap` / `apply_proposal_with_batch`
    /// are for. The two action paths cover distinct use cases:
    ///
    /// - `apply_action` / `apply_restricted_action` (this path): raw pose
    ///   mutation only. Used during live drag / mid-gesture updates where the
    ///   caller doesn't want pieces to teleport to neighbor alignments.
    /// - `apply_action_with_snap`: probes snap candidates from the proposed
    ///   pose and, on a match, mutates to the snap-corrected pose and
    ///   activates joins. Used on drop / commit.
    ///
    /// This split is intentional and tested. Don't auto-cascade here without
    /// also rewriting the legacy tests and the worker's drag-time paths.
    fn apply_atomic_join_and_cascade(
        &mut self,
        _anchor_group: GroupId,
        _delta: &mut PlayableDelta,
    ) {
    }

    fn higher_z_group_for_edge(&self, edge: EdgeId) -> Option<GroupId> {
        let (a, b) = self.logical.topology.edge_endpoints(edge);
        let ga = self.logical.group_of(a)?;
        let gb = self.logical.group_of(b)?;
        if ga == gb {
            return Some(ga);
        }
        let za = self.z_index_of.get(ga.as_usize()).copied().unwrap_or(0);
        let zb = self.z_index_of.get(gb.as_usize()).copied().unwrap_or(0);
        Some(if za >= zb { ga } else { gb })
    }

    fn lower_group_for_edge(&self, edge: EdgeId) -> Option<GroupId> {
        let (a, b) = self.logical.topology.edge_endpoints(edge);
        let ga = self.logical.group_of(a)?;
        let gb = self.logical.group_of(b)?;
        Some(if ga <= gb { ga } else { gb })
    }

    fn activate_stable_internal_edges(&mut self, group: GroupId, delta: &mut PlayableDelta) {
        let edge_count = self.logical.edge_count();
        for edge_idx in 0..edge_count {
            let edge = EdgeId(edge_idx as u32);
            if self.logical.is_edge_active(edge) == Some(true) {
                continue;
            }
            let (a, b) = self.logical.topology.edge_endpoints(edge);
            if self.logical.group_of(a) == Some(group)
                && self.logical.group_of(b) == Some(group)
                && self
                    .logical
                    .activate_edge_prefer_group(edge, group)
                    .is_some()
            {
                delta.dirty_edges.push(edge);
            }
        }
    }

    fn cascade_aligned_joins(
        &mut self,
        keep_group: GroupId,
        delta: &mut PlayableDelta,
        applied: &mut AppliedProposal,
    ) {
        let Some(mut keep_group) = self.current_group_for(keep_group) else {
            return;
        };

        loop {
            let mut merged = false;
            let edge_count = self.logical.edge_count();

            for edge_idx in 0..edge_count {
                let edge = EdgeId(edge_idx as u32);
                if self.logical.is_edge_active(edge) == Some(true) {
                    continue;
                }

                let (a, b) = self.logical.topology.edge_endpoints(edge);
                let Some(group_a) = self.logical.group_of(a) else {
                    continue;
                };
                let Some(group_b) = self.logical.group_of(b) else {
                    continue;
                };
                if group_a == group_b {
                    continue;
                }

                let (fixed_piece, moving_piece, moving_group) = if group_a == keep_group {
                    (a, b, group_b)
                } else if group_b == keep_group {
                    (b, a, group_a)
                } else {
                    continue;
                };

                if !self.edge_is_aligned_for_join(fixed_piece, moving_piece, moving_group) {
                    continue;
                }

                let post_merge_pose = self.merged_anchor_pose_for_edge(edge, keep_group);
                if let Some(merge) = self.logical.activate_edge_prefer_group(edge, keep_group) {
                    if merge.edge_changed {
                        delta.dirty_edges.push(edge);
                        applied.activated_edges.push(edge);
                    }
                    keep_group = merge.keep;
                    if let Some(absorbed) = merge.absorbed {
                        if let (Some(pose), Some(group_pose)) = (
                            post_merge_pose,
                            self.group_pose.get_mut(merge.keep.as_usize()),
                        ) {
                            *group_pose = pose;
                        }
                        applied.merged_groups.push(GroupMergeUpdate {
                            keep: merge.keep,
                            absorbed,
                        });
                        self.remove_group_from_z_order(absorbed);
                        delta.z_order_changed = true;
                        delta.membership_changed = true;
                        mark_group_dirty(self, merge.keep, delta);
                        mark_group_dirty(self, absorbed, delta);
                    }
                    merged = true;
                    break;
                }
            }

            if !merged {
                break;
            }
        }

        self.activate_stable_internal_edges(keep_group, delta);
    }

    /// Applies topology-neutral frame snap hooks. Returns `true` when the
    /// group's pose was adjusted.
    fn apply_topology_frame_snap(
        &mut self,
        group: GroupId,
        complete_snap: bool,
        delta: &mut PlayableDelta,
    ) -> bool {
        let Some(group) = self.current_group_for(group) else {
            return false;
        };
        let Some(target_pose) = self.frame_snap_pose(group, complete_snap) else {
            return false;
        };
        let Some(pose) = self.group_pose.get_mut(group.as_usize()) else {
            return false;
        };
        if pose_approx_eq(*pose, target_pose) {
            return false;
        }

        *pose = target_pose;
        mark_group_dirty(self, group, delta);
        true
    }

    fn frame_snap_pose(&self, group: GroupId, complete_snap: bool) -> Option<Pose2> {
        if self.flip_of(group) == Some(FlipState::Flipped) {
            return None;
        }
        let members = self.logical.members_of(group).collect::<Vec<_>>();
        if members.is_empty() {
            return None;
        }

        let snap_distance =
            self.rules.snap_distance_ratio.abs() * self.rules.frame_snap_ratio.abs();
        let total = self.logical.piece_count();
        let is_complete_group = members.len() == total;

        // Completing the puzzle force-snaps to canonical (UX: once all
        // pieces are joined we don't want the assembly drifting). Uses
        // the topology-supplied identity anchor — the universal solver
        // would also handle this, but the identity path's `force` flag
        // is what lets the user "finish" by dropping the assembly near,
        // not exactly at, the solved pose.
        if is_complete_group {
            return self.identity_frame_candidate(group, complete_snap, snap_distance * 2.0);
        }

        if snap_distance <= 0.0 {
            return None;
        }
        self.universal_frame_snap_pose(group, snap_distance)
    }

    /// Universal frame-snap solver: walks every member piece's outer
    /// features (border-edge segments + corner-attachment points in
    /// piece-local pose units), lifts them to world coords via the
    /// group's current pose, and aggregates per-feature corrections
    /// into a single (Δrot, Δx, Δy) for the whole group.
    ///
    /// Per-feature contracts:
    /// - `BorderEdge`: parallel-snap to the closest of the four frame
    ///   sides; contributes a rotation correction Δθ and a 1-axis
    ///   position correction along the perpendicular. Rejected if Δθ
    ///   exceeds the rotation tolerance or perpendicular distance
    ///   exceeds `snap_distance`.
    /// - `CornerAttachment`: point-snap to the closest of the four
    ///   frame corners; contributes a 2-axis position correction.
    ///   Rejected if distance exceeds `snap_distance`. No rotation
    ///   correction (BorderEdges drive rotation; CornerAttachments
    ///   live alongside them).
    ///
    /// Multiple features behave the same whether they come from one
    /// piece or N pieces in a group — each is evaluated independently
    /// and surviving constraints union by axis (BorderEdges add 1 axis,
    /// CornerAttachments add 2). If a `BorderEdge` exists anywhere and
    /// none survive the rotation check, the group does not snap.
    fn universal_frame_snap_pose(&self, group: GroupId, snap_distance: f32) -> Option<Pose2> {
        let group_pose = self.pose_of(group)?;
        let anchor = self.anchor_piece_of_group(group)?;
        let anchor_local = self.piece_local_pose_of(anchor)?;
        let flipped = self.flip_of(group) == Some(FlipState::Flipped);
        let flip_sign: f32 = if flipped { -1.0 } else { 1.0 };
        let aspect = self.piece_aspect_ratio;
        let (extent_x, extent_y) = self.logical.topology.snap_frame_extent_in_pose_units();
        let rot_tol = self.rules.rotation_snap_tolerance.as_degrees_f32().abs();

        // Collect features in group-anchor-local coords.
        let mut features: Vec<LocalSnapFeature> = Vec::new();
        for piece in self.logical.members_of(group) {
            let Some(piece_local) = self.piece_local_pose_of(piece) else {
                continue;
            };
            let dxp = (piece_local.x_mm() - anchor_local.x_mm()) * flip_sign;
            let dyp = piece_local.y_mm() - anchor_local.y_mm();
            let map_pt = |(px, py): (f32, f32)| (dxp + px * flip_sign, dyp + py);
            self.logical
                .topology
                .visit_outer_features(piece, &mut |f| match f {
                    PieceOuterFeature::BorderEdge { p1, p2 } => {
                        features.push(LocalSnapFeature::BorderEdge {
                            p1: map_pt(p1),
                            p2: map_pt(p2),
                        });
                    }
                    PieceOuterFeature::CornerAttachment { point } => {
                        features.push(LocalSnapFeature::CornerAttachment {
                            point: map_pt(point),
                        });
                    }
                });
        }
        if features.is_empty() {
            return None;
        }

        // Pass 1: per-BorderEdge angular correction to nearest axis.
        let group_rot = group_pose.rotation_degrees();
        let mut rotation_corrections: Vec<f32> = Vec::new();
        let mut accepted_border_idx: Vec<usize> = Vec::new();
        let mut has_any_border_edge = false;
        for (idx, f) in features.iter().enumerate() {
            if let LocalSnapFeature::BorderEdge { p1, p2 } = f {
                has_any_border_edge = true;
                let w1 = rotate_offset_with_aspect(p1.0, p1.1, group_rot, aspect);
                let w2 = rotate_offset_with_aspect(p2.0, p2.1, group_rot, aspect);
                let dx = w2.0 - w1.0;
                let dy = w2.1 - w1.1;
                if dx.hypot(dy) < 1.0e-6 {
                    continue;
                }
                let angle = dy.atan2(dx).to_degrees();
                let delta = rotation_correction_to_axis(angle);
                if delta.abs() > rot_tol {
                    continue;
                }
                rotation_corrections.push(delta);
                accepted_border_idx.push(idx);
            }
        }
        if has_any_border_edge && rotation_corrections.is_empty() {
            return None;
        }
        let rot_delta = if rotation_corrections.is_empty() {
            0.0_f32
        } else {
            rotation_corrections.iter().sum::<f32>() / rotation_corrections.len() as f32
        };
        let new_rot = group_rot + rot_delta;

        // Pass 2: post-rotation position corrections per surviving feature.
        let mut x_corr: Vec<f32> = Vec::new();
        let mut y_corr: Vec<f32> = Vec::new();
        for (idx, f) in features.iter().enumerate() {
            match f {
                LocalSnapFeature::BorderEdge { p1, p2 } => {
                    if !accepted_border_idx.contains(&idx) {
                        continue;
                    }
                    let w1 = rotate_offset_with_aspect(p1.0, p1.1, new_rot, aspect);
                    let w2 = rotate_offset_with_aspect(p2.0, p2.1, new_rot, aspect);
                    let dx = w2.0 - w1.0;
                    let dy = w2.1 - w1.1;
                    if dx.hypot(dy) < 1.0e-6 {
                        continue;
                    }
                    let mid_x = group_pose.x_mm() + (w1.0 + w2.0) * 0.5;
                    let mid_y = group_pose.y_mm() + (w1.1 + w2.1) * 0.5;
                    let horizontal = dx.abs() >= dy.abs();
                    if horizontal {
                        let target_y = if (mid_y).abs() <= (mid_y - extent_y).abs() {
                            0.0
                        } else {
                            extent_y
                        };
                        let d = target_y - mid_y;
                        if d.abs() > snap_distance {
                            continue;
                        }
                        y_corr.push(d);
                    } else {
                        let target_x = if (mid_x).abs() <= (mid_x - extent_x).abs() {
                            0.0
                        } else {
                            extent_x
                        };
                        let d = target_x - mid_x;
                        if d.abs() > snap_distance {
                            continue;
                        }
                        x_corr.push(d);
                    }
                }
                LocalSnapFeature::CornerAttachment { point } => {
                    let w = rotate_offset_with_aspect(point.0, point.1, new_rot, aspect);
                    let wx = group_pose.x_mm() + w.0;
                    let wy = group_pose.y_mm() + w.1;
                    let corners = [
                        (0.0_f32, 0.0_f32),
                        (extent_x, 0.0),
                        (extent_x, extent_y),
                        (0.0, extent_y),
                    ];
                    let mut best: Option<((f32, f32), f32)> = None;
                    for c in corners.iter() {
                        let d = (wx - c.0).hypot(wy - c.1);
                        if best.is_none_or(|(_, bd)| d < bd) {
                            best = Some((*c, d));
                        }
                    }
                    if let Some((c, d)) = best {
                        if d > snap_distance {
                            continue;
                        }
                        x_corr.push(c.0 - wx);
                        y_corr.push(c.1 - wy);
                    }
                }
            }
        }

        let avg = |v: &[f32]| -> f32 {
            if v.is_empty() {
                0.0
            } else {
                v.iter().sum::<f32>() / v.len() as f32
            }
        };
        let dx = avg(&x_corr);
        let dy = avg(&y_corr);
        let rotation_changed = rot_delta.abs() > 1.0e-5;
        if x_corr.is_empty() && y_corr.is_empty() && !rotation_changed {
            return None;
        }

        let new_pose =
            Pose2::try_from_mm_degrees(group_pose.x_mm() + dx, group_pose.y_mm() + dy, new_rot)?;
        let bounds = self
            .logical
            .topology
            .frame_bounds()
            .unwrap_or(crate::topology::FrameBounds {
                min_x: 0.0,
                min_y: 0.0,
                max_x: extent_x,
                max_y: extent_y,
            });
        let frame_slop = snap_distance * 0.25;
        if !self.group_fits_frame(group, new_pose, bounds, frame_slop) {
            return None;
        }
        Some(new_pose)
    }

    fn identity_frame_candidate(
        &self,
        group: GroupId,
        force: bool,
        snap_distance: f32,
    ) -> Option<Pose2> {
        let (anchor, target_piece_pose) = self.logical.topology.identity_frame_anchor()?;
        if !force && !self.rotation_matches_frame_target(group, target_piece_pose.rotation) {
            return None;
        }

        if self.logical.group_of(anchor) != Some(group) {
            return None;
        }
        let current_anchor = self.piece_world_pose(anchor)?;
        let distance = (current_anchor.x_mm() - target_piece_pose.x_mm())
            .hypot(current_anchor.y_mm() - target_piece_pose.y_mm());
        if !force && distance > snap_distance {
            return None;
        }
        self.group_pose_to_place_piece(group, anchor, target_piece_pose, target_piece_pose.rotation)
    }

    fn group_fits_frame(
        &self,
        group: GroupId,
        group_pose: Pose2,
        bounds: crate::topology::FrameBounds,
        slop: f32,
    ) -> bool {
        // Use the full puzzle frame extent rather than canonical
        // piece-center bounds: a non-square piece rotated 90° lands its
        // center outside `[0.5, cols-0.5]` (canonical-rotation bounds) but
        // still entirely inside the puzzle frame. The topology already
        // produced a valid snap target; this check exists to catch
        // multi-piece groups whose other members would land far outside
        // the puzzle area when the anchor moves to the snap pose.
        let (extent_x, extent_y) = self.logical.topology.image_extent_in_pose_units();
        let min_x = bounds.min_x.min(0.0) - slop;
        let min_y = bounds.min_y.min(0.0) - slop;
        let max_x = bounds.max_x.max(extent_x) + slop;
        let max_y = bounds.max_y.max(extent_y) + slop;
        self.logical.members_of(group).all(|piece| {
            self.piece_world_pose_with_group_pose(group, group_pose, piece)
                .map(|pose| {
                    pose.x_mm() >= min_x
                        && pose.x_mm() <= max_x
                        && pose.y_mm() >= min_y
                        && pose.y_mm() <= max_y
                })
                .unwrap_or(false)
        })
    }

    fn rotation_matches_frame_target(&self, group: GroupId, target_rotation: AngleDeg) -> bool {
        if !self.rules.rotation_enabled {
            return true;
        }
        let Some(pose) = self.pose_of(group) else {
            return false;
        };
        angle_matches(
            pose.rotation_degrees(),
            target_rotation.as_degrees_f32(),
            self.rules.rotation_snap_tolerance.as_degrees_f32().abs(),
        )
    }

    fn merged_anchor_pose_for_edge(&self, edge: EdgeId, keep: GroupId) -> Option<Pose2> {
        let (a, b) = self.logical.topology.edge_endpoints(edge);
        let group_a = self.logical.group_of(a)?;
        let group_b = self.logical.group_of(b)?;
        if group_a == group_b {
            return self.pose_of(group_a);
        }
        if keep != group_a && keep != group_b {
            return None;
        }

        let anchor = self
            .logical
            .members_of(group_a)
            .chain(self.logical.members_of(group_b))
            .min()?;
        self.piece_world_pose(anchor)
    }

    fn edge_is_aligned_for_join(
        &self,
        fixed_piece: PieceId,
        moving_piece: PieceId,
        moving_group: GroupId,
    ) -> bool {
        if self.flip_of(moving_group) == Some(FlipState::Flipped) {
            return false;
        }
        let Some(fixed_group) = self.logical.group_of(fixed_piece) else {
            return false;
        };
        if self.flip_of(fixed_group) == Some(FlipState::Flipped) {
            return false;
        }

        let Some(fixed_pose) = self.piece_world_pose(fixed_piece) else {
            return false;
        };
        let Some(moving_pose) = self.piece_world_pose(moving_piece) else {
            return false;
        };

        let rel = self
            .logical
            .topology
            .expected_relative_pose(fixed_piece, moving_piece);
        let (expected_dx, expected_dy) = rotate_offset_with_aspect(
            rel.dx.as_mm_f32(),
            rel.dy.as_mm_f32(),
            fixed_pose.rotation_degrees(),
            self.piece_aspect_ratio,
        );
        let expected_x = fixed_pose.x_mm() + expected_dx;
        let expected_y = fixed_pose.y_mm() + expected_dy;
        let distance_mm = (moving_pose.x_mm() - expected_x).hypot(moving_pose.y_mm() - expected_y);
        let edge_len = rel.dx.as_mm_f32().hypot(rel.dy.as_mm_f32()).max(1.0);
        let max_distance = self.rules.snap_distance_ratio.abs() * edge_len;
        if distance_mm > max_distance {
            return false;
        }

        if self.rules.rotation_enabled {
            let expected_rotation = fixed_pose.rotation_degrees() + rel.drot.as_degrees_f32();
            angle_matches(
                moving_pose.rotation_degrees(),
                expected_rotation,
                self.rules.rotation_snap_tolerance.as_degrees_f32().abs(),
            )
        } else {
            true
        }
    }

    fn current_group_for(&self, group: GroupId) -> Option<GroupId> {
        if self
            .logical
            .groups
            .get(group.as_usize())
            .map(|slot| slot.alive)
            .unwrap_or(false)
        {
            return Some(group);
        }

        let piece = PieceId(group.as_u32());
        self.logical.group_of(piece)
    }

    fn active_incident_edges(&self, piece: PieceId) -> impl Iterator<Item = EdgeId> + '_ {
        (0..self.logical.edge_count()).filter_map(move |edge_idx| {
            let edge = EdgeId(edge_idx as u32);
            if self.logical.is_edge_active(edge) != Some(true) {
                return None;
            }
            let (a, b) = self.logical.topology.edge_endpoints(edge);
            (a == piece || b == piece).then_some(edge)
        })
    }
}

/// A piece's outer feature in group-anchor-local coords (already
/// flattened across piece position offsets and flip).
#[derive(Clone, Copy, Debug)]
enum LocalSnapFeature {
    BorderEdge { p1: (f32, f32), p2: (f32, f32) },
    CornerAttachment { point: (f32, f32) },
}

/// Smallest-magnitude Δ such that `angle + Δ` lands on a multiple of
/// 90°. Returned in degrees, in (-45, 45].
fn rotation_correction_to_axis(angle: f32) -> f32 {
    let m = angle.rem_euclid(90.0);
    if m > 45.0 {
        90.0 - m
    } else {
        -m
    }
}

fn mark_group_dirty<T: PuzzleTopology>(
    playable: &PlayableState<T>,
    group: GroupId,
    delta: &mut PlayableDelta,
) {
    delta.dirty_groups.push(group);
    for piece in playable.logical.members_of(group) {
        delta.dirty_pieces.push(piece);
    }
}

fn proposal_apply_rejection(proposal: &SnapProposal) -> Option<ProposalApplyRejection> {
    if proposal.rejections.is_empty() {
        return Some(ProposalApplyRejection::NoCandidate);
    }

    let reason = proposal
        .rejections
        .iter()
        .map(|(_, reason)| *reason)
        .find(|reason| *reason == crate::snap::SnapRejectionReason::MoverGroupMissing)
        .or_else(|| proposal.rejections.first().map(|(_, reason)| *reason))?;

    match reason {
        crate::snap::SnapRejectionReason::MoverGroupMissing => {
            Some(ProposalApplyRejection::MoverGroupMissing)
        }
        other => Some(ProposalApplyRejection::CandidateRejected(other)),
    }
}

fn group_from_action(action: PlayableAction) -> GroupId {
    match action {
        PlayableAction::TranslateGroup { group, .. }
        | PlayableAction::TransformGroupTo { group, .. }
        | PlayableAction::RotateGroupTo { group, .. }
        | PlayableAction::StepRotateGroupCw { group }
        | PlayableAction::StepRotateGroupCcw { group }
        | PlayableAction::UnflipGroup { group, .. } => group,
    }
}

fn action_can_probe_for_snap(action: PlayableAction) -> bool {
    matches!(
        action,
        PlayableAction::TranslateGroup { .. }
            | PlayableAction::TransformGroupTo { .. }
            | PlayableAction::RotateGroupTo { .. }
            | PlayableAction::StepRotateGroupCw { .. }
            | PlayableAction::StepRotateGroupCcw { .. }
    )
}

fn rejected_action_batch(
    action_id: Option<ActionId>,
    revision: u64,
    group: GroupId,
    rejection: ProposalApplyRejection,
) -> PlayableUpdateBatch {
    PlayableUpdateBatch {
        revision_before: revision,
        revision_after: revision,
        delta: PlayableDelta::for_revision(revision),
        proposal: AppliedProposal {
            action_id,
            base_revision: revision,
            applied_revision: revision,
            status: ProposalApplyStatus::Rejected,
            rejection: Some(rejection),
            rebased: false,
            mover_group: group,
            fixed_group: None,
            activated_edges: Vec::new(),
            deactivated_edges: Vec::new(),
            merged_groups: Vec::new(),
            final_group_poses: Vec::new(),
        },
    }
}

fn touched_group_poses<T: PuzzleTopology>(
    playable: &PlayableState<T>,
    delta: &PlayableDelta,
) -> Vec<GroupPoseUpdate> {
    let mut poses = Vec::with_capacity(delta.dirty_groups.len());
    for group in delta.dirty_groups.iter().copied() {
        if poses
            .iter()
            .any(|update: &GroupPoseUpdate| update.group == group)
        {
            continue;
        }
        if let Some(pose) = playable.pose_of(group) {
            poses.push(GroupPoseUpdate { group, pose });
        }
    }
    poses
}

fn next_u32(seed: &mut u64) -> u32 {
    let mut x = *seed;
    if x == 0 {
        x = 0xA5A5_A5A5_1234_5678;
    }
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    *seed = x;
    (x >> 16) as u32
}

fn rand_unit(seed: &mut u64) -> f32 {
    let value = next_u32(seed);
    value as f32 / (u32::MAX as f32)
}

fn rand_range(seed: &mut u64, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed)
}

fn approx_zero(value: f32) -> bool {
    value.abs() <= 1.0e-4
}

fn pose_approx_eq(a: Pose2, b: Pose2) -> bool {
    approx_zero(a.x_mm() - b.x_mm())
        && approx_zero(a.y_mm() - b.y_mm())
        && angle_matches(a.rotation_degrees(), b.rotation_degrees(), 1.0e-4)
}

fn build_piece_local_poses<T: PuzzleTopology>(topology: &T) -> Box<[Pose2]> {
    let piece_count = topology.piece_count() as usize;
    let mut poses = vec![Pose2::default(); piece_count];
    let mut visited = vec![false; piece_count];
    let mut queue = Vec::<PieceId>::with_capacity(piece_count);

    for start_idx in 0..piece_count {
        if visited[start_idx] {
            continue;
        }

        visited[start_idx] = true;
        queue.clear();
        queue.push(PieceId(start_idx as u32));
        let mut cursor = 0;

        while cursor < queue.len() {
            let current = queue[cursor];
            cursor += 1;
            let current_pose = poses[current.as_usize()];

            for edge_idx in 0..topology.edge_count() {
                let (a, b) = topology.edge_endpoints(EdgeId(edge_idx));
                let (next, rel) = if a == current {
                    (b, topology.expected_relative_pose(a, b))
                } else if b == current {
                    (a, topology.expected_relative_pose(b, a))
                } else {
                    continue;
                };
                let next_idx = next.as_usize();
                if next_idx >= piece_count || visited[next_idx] {
                    continue;
                }

                let next_rotation = current_pose.rotation_degrees() + rel.drot.as_degrees_f32();
                if let Some(next_pose) = Pose2::try_from_mm_degrees(
                    current_pose.x_mm() + rel.dx.as_mm_f32(),
                    current_pose.y_mm() + rel.dy.as_mm_f32(),
                    next_rotation,
                ) {
                    poses[next_idx] = next_pose;
                }
                visited[next_idx] = true;
                queue.push(next);
            }
        }
    }

    poses.into_boxed_slice()
}

pub(crate) fn rotate_vec_mm(x: f32, y: f32, degrees: f32) -> (f32, f32) {
    let radians = degrees.to_radians();
    let (sin, cos) = radians.sin_cos();
    (x * cos - y * sin, x * sin + y * cos)
}

/// Rotates a piece-count offset (where x is in piece-width units and y is
/// in piece-height units) by `degrees`, taking piece aspect into account so
/// the result corresponds to a real isotropic rotation in pixel space.
///
/// `aspect` is `piece_height / piece_width`. For square pieces (`aspect = 1`)
/// this is equivalent to `rotate_vec_mm`. For non-square pieces it produces
/// a piece-count offset such that the anisotropic px conversion
/// `(x * piece_width, y * piece_height)` recovers the pixel position one
/// would get by rotating the canonical pixel offset directly.
pub(crate) fn rotate_offset_with_aspect(x: f32, y: f32, degrees: f32, aspect: f32) -> (f32, f32) {
    if !aspect.is_finite() || aspect <= 0.0 || (aspect - 1.0).abs() < f32::EPSILON {
        return rotate_vec_mm(x, y, degrees);
    }
    let radians = degrees.to_radians();
    let (sin, cos) = radians.sin_cos();
    // Convert (x, y) to a pixel-isotropic frame where 1 unit on both axes
    // equals 1 piece-width: y is scaled by `aspect`. Rotate there, then
    // convert back to piece-count units.
    let yp = y * aspect;
    let rx = x * cos - yp * sin;
    let ry = x * sin + yp * cos;
    (rx, ry / aspect)
}
