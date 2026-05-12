//! Non-mutating snap probing and proposal contracts.

use crate::ids::{EdgeId, GroupId, PieceId};
use crate::playable::{rotate_offset_with_aspect, FlipState, PlayableState, Pose2};
use crate::topology::PuzzleTopology;

/// Transport-agnostic action identifier carried by proposals.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ActionId(pub u64);

/// Topology site considered for a join.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct JoinSite {
    pub edge: EdgeId,
    pub mover_piece: PieceId,
    pub fixed_piece: PieceId,
    pub fixed_group: GroupId,
}

/// Reason a potential join site was rejected while probing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SnapRejectionReason {
    MoverGroupMissing,
    SameGroup,
    FlippedGroup,
    RotationMismatch,
    OutsideSnapDistance,
    InvalidTopology,
}

/// Candidate join with the mover pose needed to satisfy the topology edge.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SnapCandidate {
    pub site: JoinSite,
    pub desired_mover_pose: Pose2,
    pub distance_mm: f32,
}

/// Non-mutating result of probing a dropped/proposed group pose.
#[derive(Clone, Debug, PartialEq)]
pub struct SnapProposal {
    pub action_id: Option<ActionId>,
    pub base_revision: u64,
    pub mover_group: GroupId,
    pub proposed_pose: Pose2,
    pub desired_pose: Pose2,
    pub fixed_group: Option<GroupId>,
    pub candidates: Vec<SnapCandidate>,
    pub activate_edges: Vec<EdgeId>,
    pub rejections: Vec<(EdgeId, SnapRejectionReason)>,
}

/// Group preservation rule for applying a proposal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MergePolicy {
    KeepMoverGroup,
    KeepFixedGroup,
    KeepHigherZ,
    KeepLowerGroupId,
}

pub fn probe_snaps<T: PuzzleTopology>(
    playable: &PlayableState<T>,
    mover_group: GroupId,
    proposed_pose: Pose2,
) -> SnapProposal {
    let mut proposal = SnapProposal {
        action_id: None,
        base_revision: playable.revision,
        mover_group,
        proposed_pose,
        desired_pose: proposed_pose,
        fixed_group: None,
        candidates: Vec::new(),
        activate_edges: Vec::new(),
        rejections: Vec::new(),
    };

    let mover_alive = playable
        .logical
        .groups
        .get(mover_group.as_usize())
        .map(|slot| slot.alive)
        .unwrap_or(false);
    if !mover_alive {
        proposal
            .rejections
            .push((EdgeId(0), SnapRejectionReason::MoverGroupMissing));
        return proposal;
    }

    if playable.flip_of(mover_group) == Some(FlipState::Flipped) {
        for edge_idx in 0..playable.logical.edge_count() {
            proposal
                .rejections
                .push((EdgeId(edge_idx as u32), SnapRejectionReason::FlippedGroup));
        }
        return proposal;
    }

    for edge_idx in 0..playable.logical.edge_count() {
        let edge = EdgeId(edge_idx as u32);
        if playable.logical.is_edge_active(edge) == Some(true) {
            continue;
        }

        let (a, b) = playable.logical.topology.edge_endpoints(edge);
        let Some(group_a) = playable.logical.group_of(a) else {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::InvalidTopology));
            continue;
        };
        let Some(group_b) = playable.logical.group_of(b) else {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::InvalidTopology));
            continue;
        };

        let (mover_piece, fixed_piece, fixed_group) = if group_a == mover_group {
            if group_b == mover_group {
                proposal
                    .rejections
                    .push((edge, SnapRejectionReason::SameGroup));
                continue;
            }
            (a, b, group_b)
        } else if group_b == mover_group {
            (b, a, group_a)
        } else {
            continue;
        };

        if playable.flip_of(fixed_group) == Some(FlipState::Flipped) {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::FlippedGroup));
            continue;
        }

        let Some(candidate) = candidate_for_edge(
            playable,
            edge,
            mover_group,
            mover_piece,
            fixed_piece,
            fixed_group,
            proposed_pose,
        ) else {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::InvalidTopology));
            continue;
        };

        let Some(fixed_pose) = playable.piece_world_pose(fixed_piece) else {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::InvalidTopology));
            continue;
        };
        let rel = playable
            .logical
            .topology
            .expected_relative_pose(mover_piece, fixed_piece);
        let expected_mover_rotation = fixed_pose.rotation_degrees() - rel.drot.as_degrees_f32();
        if playable.rules.rotation_enabled
            && !angle_matches(
                proposed_pose.rotation_degrees(),
                expected_mover_rotation,
                playable
                    .rules
                    .rotation_snap_tolerance
                    .as_degrees_f32()
                    .abs(),
            )
        {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::RotationMismatch));
            continue;
        }

        let edge_len = rel.dx.as_mm_f32().hypot(rel.dy.as_mm_f32()).max(1.0);
        let max_distance = playable.rules.snap_distance_ratio.abs() * edge_len;
        if candidate.distance_mm > max_distance {
            proposal
                .rejections
                .push((edge, SnapRejectionReason::OutsideSnapDistance));
            continue;
        }

        proposal.candidates.push(candidate);
    }

    proposal.candidates.sort_by(|a, b| {
        a.distance_mm
            .total_cmp(&b.distance_mm)
            .then_with(|| a.site.edge.cmp(&b.site.edge))
            .then_with(|| a.site.mover_piece.cmp(&b.site.mover_piece))
            .then_with(|| a.site.fixed_piece.cmp(&b.site.fixed_piece))
            .then_with(|| a.site.fixed_group.cmp(&b.site.fixed_group))
    });
    if let Some(best) = proposal.candidates.first().copied() {
        proposal.desired_pose = best.desired_mover_pose;
        proposal.fixed_group = Some(best.site.fixed_group);
        proposal.activate_edges.push(best.site.edge);
    }

    proposal
}

fn candidate_for_edge<T: PuzzleTopology>(
    playable: &PlayableState<T>,
    edge: EdgeId,
    mover_group: GroupId,
    mover_piece: PieceId,
    fixed_piece: PieceId,
    fixed_group: GroupId,
    proposed_pose: Pose2,
) -> Option<SnapCandidate> {
    let moving_piece_at_proposed =
        playable.piece_world_pose_with_group_pose(mover_group, proposed_pose, mover_piece)?;
    let fixed_piece_pose = playable.piece_world_pose(fixed_piece)?;
    let rel = playable
        .logical
        .topology
        .expected_relative_pose(mover_piece, fixed_piece);
    let (expected_dx, expected_dy) = rotate_offset_with_aspect(
        rel.dx.as_mm_f32(),
        rel.dy.as_mm_f32(),
        proposed_pose.rotation_degrees(),
        playable.piece_aspect_ratio(),
    );
    let target_mover_piece = Pose2::try_from_mm_degrees(
        fixed_piece_pose.x_mm() - expected_dx,
        fixed_piece_pose.y_mm() - expected_dy,
        proposed_pose.rotation_degrees(),
    )?;
    let desired_mover_pose = playable.group_pose_to_place_piece(
        mover_group,
        mover_piece,
        target_mover_piece,
        proposed_pose.rotation,
    )?;
    let distance_mm = (moving_piece_at_proposed.x_mm() - target_mover_piece.x_mm())
        .hypot(moving_piece_at_proposed.y_mm() - target_mover_piece.y_mm());

    Some(SnapCandidate {
        site: JoinSite {
            edge,
            mover_piece,
            fixed_piece,
            fixed_group,
        },
        desired_mover_pose,
        distance_mm,
    })
}

pub(crate) fn angle_matches(a: f32, b: f32, tolerance: f32) -> bool {
    let mut diff = (a - b) % 360.0;
    if diff < -180.0 {
        diff += 360.0;
    } else if diff > 180.0 {
        diff -= 360.0;
    }
    diff.abs() <= tolerance
}
