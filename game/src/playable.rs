//! Authoritative gameplay state layer built on top of `LogicalState`.

use crate::delta::PlayableDelta;
use crate::ids::{GroupId, PieceId};
use crate::logical::{LogicalState, LogicalStateSummary};
use crate::rotation_step::{intersect_symmetry_angles, next_step_canonical, StepDirection};
use crate::rules::PlayRules;
use crate::topology::PuzzleTopology;
use crate::units::{AngleDeg, LengthMm};

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
    },
}

/// Restricted/admin actions.
///
/// These are intentionally separated from unrestricted gameplay actions so the
/// caller can enforce game mode policies and achievement gating.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RestrictedPlayableAction {
    FlipGroup {
        group: GroupId,
    },
    /// Detaches one piece into its own singleton group and immediately applies
    /// the requested pose/flip to that detached group.
    DetachPieceAsGroup {
        piece: PieceId,
        target_pose: Pose2,
        target_flip: FlipState,
    },
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
    pub z_order: Vec<GroupId>,
    pub z_index_of: Box<[u32]>,
    pub rules: PlayRules,
    pub revision: u64,
    pub scratch_groups: Vec<GroupId>,
}

impl<T: PuzzleTopology> PlayableState<T> {
    pub fn solved(topology: T, rules: PlayRules) -> Self {
        Self::new(LogicalState::solved(topology), rules)
    }

    pub fn shuffled(topology: T, rules: PlayRules, seed: u64) -> Self {
        let mut state = Self::new(LogicalState::shuffled(topology), rules);
        state.shuffle_in_place(seed);
        state
    }

    pub fn new(logical: LogicalState<T>, rules: PlayRules) -> Self {
        let count = logical.piece_count();
        let z_order = (0..count).map(|id| GroupId(id as u32)).collect::<Vec<_>>();
        let z_index_of = (0..count)
            .map(|id| id as u32)
            .collect::<Vec<_>>()
            .into_boxed_slice();

        Self {
            logical,
            group_pose: vec![Pose2::default(); count].into_boxed_slice(),
            group_flip: vec![FlipState::Normal; count].into_boxed_slice(),
            z_order,
            z_index_of,
            rules,
            revision: 0,
            scratch_groups: Vec::with_capacity(count),
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
        if !self.logical.is_solved() {
            return false;
        }
        let Some(group) = self.logical.active_group_ids().next() else {
            return false;
        };
        let Some(pose) = self.pose_of(group) else {
            return false;
        };
        let pose_is_identity = approx_zero(pose.x_mm())
            && approx_zero(pose.y_mm())
            && approx_zero(pose.rotation_degrees());
        let unflipped = self.flip_of(group) == Some(FlipState::Normal);
        pose_is_identity && unflipped
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

    /// Applies an unrestricted action atomically.
    ///
    /// Atomic contract:
    /// 1) Apply direct state mutation from the action.
    /// 2) Run snap-join checks.
    /// 3) Run join cascade until a stable state.
    /// 4) Emit one externally visible delta.
    ///
    /// Steps (2) and (3) are currently placeholders in this skeleton.
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
            PlayableAction::UnflipGroup { group } => {
                if let Some(group_flip) = self.group_flip.get_mut(group.as_usize()) {
                    *group_flip = FlipState::Normal;
                    mark_group_dirty(self, group, &mut delta);
                }
                group
            }
        };

        self.apply_atomic_join_and_cascade(anchor_group, &mut delta);
        delta
    }

    /// Applies a restricted/admin action atomically.
    ///
    /// Same atomic contract as `apply_action`.
    pub fn apply_restricted_action(&mut self, action: RestrictedPlayableAction) -> PlayableDelta {
        self.revision = self.revision.wrapping_add(1);
        let mut delta = PlayableDelta::for_revision(self.revision);

        let anchor_group = match action {
            RestrictedPlayableAction::FlipGroup { group } => {
                if let Some(group_flip) = self.group_flip.get_mut(group.as_usize()) {
                    *group_flip = FlipState::Flipped;
                    mark_group_dirty(self, group, &mut delta);
                }
                group
            }
            RestrictedPlayableAction::DetachPieceAsGroup {
                piece,
                target_pose,
                target_flip,
            } => self.detach_piece_as_group(piece, target_pose, target_flip, &mut delta),
        };

        self.apply_atomic_join_and_cascade(anchor_group, &mut delta);
        delta
    }

    fn next_step_rotation(&self, group: GroupId, clockwise: bool) -> Option<AngleDeg> {
        let current = self.pose_of(group)?.rotation;
        let mut members = self.logical.members_of(group);
        let first = members.next()?;
        let mut group_angles = self.logical.topology.symmetry_angles(first).to_vec();
        for piece in members {
            group_angles = intersect_symmetry_angles(
                &group_angles,
                self.logical.topology.symmetry_angles(piece),
            );
        }

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

        let Some(piece_slot) = self.logical.pieces.get_mut(piece_idx) else {
            return new_group;
        };
        let old_group = piece_slot.group;

        if old_group != new_group {
            if let Some(old_slot) = self.logical.groups.get_mut(old_group.as_usize()) {
                if old_slot.size > 0 {
                    old_slot.size -= 1;
                }
                if old_slot.size == 0 {
                    old_slot.alive = false;
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

        if old_group != new_group {
            if self
                .logical
                .groups
                .get(old_group.as_usize())
                .map(|slot| !slot.alive)
                .unwrap_or(false)
            {
                self.remove_group_from_z_order(old_group);
                delta.z_order_changed = true;
            }

            if self.append_group_to_z_order_if_missing(new_group) {
                delta.z_order_changed = true;
            }

            mark_group_dirty(self, old_group, delta);
        }

        mark_group_dirty(self, new_group, delta);
        delta.dirty_pieces.push(piece);
        new_group
    }

    fn append_group_to_z_order_if_missing(&mut self, group: GroupId) -> bool {
        if self
            .z_index_of
            .get(group.as_usize())
            .copied()
            .unwrap_or(u32::MAX)
            != u32::MAX
        {
            return false;
        }
        self.z_order.push(group);
        self.rebuild_z_indices();
        true
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

    fn apply_atomic_join_and_cascade(
        &mut self,
        _anchor_group: GroupId,
        _delta: &mut PlayableDelta,
    ) {
        // Placeholder in the skeleton crate: future implementation will run
        // snap checks and join cascades as part of the same atomic apply step.
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
