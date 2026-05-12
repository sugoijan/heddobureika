//! Game state wrapper used by the app.
//!
//! `AppGameState` owns the authoritative `PlayableState<GridTopology>` plus
//! the derived `VisualState` projection cache. It is the live in-memory state
//! the multiplayer bridge mutates as wire updates arrive.
//!
//! The `playable.revision` and `seq` are kept in sync: `seq` is the server's
//! room sequence; `revision` is the live `PlayableState` revision which the
//! authoritative apply path increments. For locally-initialised state both
//! start at the same value and stay paired through every apply.

use std::fmt;

use heddobureika_core::{
    EdgeId, FlipState, GameRules, GridTopology, GroupId, LogicalState, PieceId, PlayableDelta,
    PlayableGameSnapshot, PlayableGameSnapshotError, PlayableRoomUpdate, PlayableState, Pose2,
    ProjectionScratch, PuzzleInfo, PuzzleTopology, RestoredPlayableState, VisualState,
};

/// Live app-side game state. Wraps the canonical `PlayableState<GridTopology>`
/// and a `VisualState` projection cache.
#[derive(Clone)]
pub(crate) struct AppGameState {
    pub puzzle: PuzzleInfo,
    pub rules: GameRules,
    pub scramble_nonce: u32,
    pub seq: u64,
    pub focused_piece: Option<u32>,
    pub playable: PlayableState<GridTopology>,
    pub visual: VisualState,
}

#[derive(Debug)]
pub(crate) enum AppGameStateError {
    UnsupportedTopology,
    Snapshot(PlayableGameSnapshotError),
}

impl fmt::Display for AppGameStateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedTopology => f.write_str("unsupported puzzle topology"),
            Self::Snapshot(err) => write!(f, "invalid playable snapshot: {err:?}"),
        }
    }
}

impl From<PlayableGameSnapshotError> for AppGameStateError {
    fn from(value: PlayableGameSnapshotError) -> Self {
        Self::Snapshot(value)
    }
}

fn piece_aspect_ratio_from_puzzle(puzzle: &PuzzleInfo) -> f32 {
    if puzzle.cols == 0 || puzzle.rows == 0 || puzzle.image_width == 0 || puzzle.image_height == 0 {
        return 1.0;
    }
    let piece_width = puzzle.image_width as f32 / puzzle.cols as f32;
    let piece_height = puzzle.image_height as f32 / puzzle.rows as f32;
    if piece_width <= 0.0 {
        return 1.0;
    }
    piece_height / piece_width
}

impl AppGameState {
    /// Builds an `AppGameState` from a complete `PlayableGameSnapshot` (the
    /// transport DTO used for full-state refreshes and persistence).
    pub fn from_snapshot(snapshot: PlayableGameSnapshot) -> Result<Self, AppGameStateError> {
        let restored = snapshot.restore_playable_from_spec()?;
        let mut playable = match restored {
            RestoredPlayableState::Grid(playable) => playable,
            RestoredPlayableState::TriangularTessellation(_) => {
                return Err(AppGameStateError::UnsupportedTopology);
            }
        };
        playable.set_piece_aspect_ratio(piece_aspect_ratio_from_puzzle(&snapshot.puzzle));
        let visual = VisualState::rebuild_from(&playable);
        Ok(Self {
            puzzle: snapshot.puzzle,
            rules: snapshot.rules,
            scramble_nonce: snapshot.scramble_nonce,
            seq: snapshot.seq,
            focused_piece: snapshot.state.focused_piece,
            playable,
            visual,
        })
    }

    /// Builds an `AppGameState` for a freshly-scrambled puzzle: no edges
    /// active (each piece is its own singleton group), with the given
    /// per-piece pixel-coord poses applied to each group.
    pub fn scrambled(
        puzzle: PuzzleInfo,
        rules: GameRules,
        scramble_nonce: u32,
        positions: &[(f32, f32)],
        rotations: &[f32],
        flips: &[bool],
        piece_order: &[usize],
        piece_width: f32,
        piece_height: f32,
    ) -> Result<Self, AppGameStateError> {
        let topology = GridTopology::try_new(puzzle.cols, puzzle.rows)
            .ok_or(AppGameStateError::UnsupportedTopology)?;
        let total = topology.piece_count() as usize;
        if positions.len() != total
            || rotations.len() != total
            || flips.len() != total
            || piece_width <= 0.0
            || piece_height <= 0.0
        {
            return Err(AppGameStateError::UnsupportedTopology);
        }
        let play_rules = rules.to_play_rules()?;
        let logical = LogicalState::new(topology);
        let mut playable = PlayableState::new(logical, play_rules);
        playable.set_piece_aspect_ratio(piece_height / piece_width);
        for idx in 0..total {
            let piece = PieceId(idx as u32);
            let Some(group) = playable.logical.group_of(piece) else {
                return Err(AppGameStateError::UnsupportedTopology);
            };
            let mm_x = (positions[idx].0 + piece_width * 0.5) / piece_width;
            let mm_y = (positions[idx].1 + piece_height * 0.5) / piece_height;
            let Some(pose) = Pose2::try_from_mm_degrees(mm_x, mm_y, rotations[idx]) else {
                return Err(AppGameStateError::UnsupportedTopology);
            };
            if let Some(slot) = playable.group_pose.get_mut(group.as_usize()) {
                *slot = pose;
            }
            if let Some(slot) = playable.group_flip.get_mut(group.as_usize()) {
                *slot = if flips[idx] {
                    FlipState::Flipped
                } else {
                    FlipState::Normal
                };
            }
        }
        // Each singleton group's GroupId equals its piece id, so the
        // per-piece scrambled order doubles as the group z-order.
        playable.z_order = piece_order
            .iter()
            .filter_map(|p| u32::try_from(*p).ok())
            .map(GroupId)
            .collect();
        playable.rebuild_z_indices_from_snapshot();
        let visual = VisualState::rebuild_from(&playable);
        Ok(Self {
            puzzle,
            rules,
            scramble_nonce,
            seq: playable.revision,
            focused_piece: None,
            playable,
            visual,
        })
    }

    /// Builds an `AppGameState` for the fully-solved puzzle: every edge
    /// active, the single resulting group anchored at the origin pose with
    /// no rotation or flip.
    pub fn solved(puzzle: PuzzleInfo, rules: GameRules) -> Result<Self, AppGameStateError> {
        let topology = GridTopology::try_new(puzzle.cols, puzzle.rows)
            .ok_or(AppGameStateError::UnsupportedTopology)?;
        let play_rules = rules.to_play_rules()?;
        let mut logical = LogicalState::new(topology);
        logical.activate_all_edges();
        logical
            .validate()
            .map_err(|_| AppGameStateError::UnsupportedTopology)?;
        let mut playable = PlayableState::new(logical, play_rules);
        playable.set_piece_aspect_ratio(piece_aspect_ratio_from_puzzle(&puzzle));
        // Pose stores the piece-CENTER in piece-count units, so the solved
        // anchor (piece 0) sits at `(0.5, 0.5)` to land its top-left pixel
        // at the workspace top-left.
        let origin = Pose2::try_from_mm_degrees(0.5, 0.5, 0.0)
            .ok_or(AppGameStateError::UnsupportedTopology)?;
        for pose in playable.group_pose.iter_mut() {
            *pose = origin;
        }
        for flip in playable.group_flip.iter_mut() {
            *flip = FlipState::Normal;
        }
        playable
            .validate()
            .map_err(|_| AppGameStateError::UnsupportedTopology)?;
        let visual = VisualState::rebuild_from(&playable);
        Ok(Self {
            puzzle,
            rules,
            scramble_nonce: 0,
            seq: playable.revision,
            focused_piece: None,
            playable,
            visual,
        })
    }

    /// Applies a wire `PlayableRoomUpdate` to the live state.
    ///
    /// On `seq`/`revision` mismatch (stale update), this is a no-op and
    /// returns `false`. On success it advances `revision`, `seq`, and
    /// refreshes the `visual` projection from the dirty-id delta in the
    /// wire update.
    pub fn apply_wire_update(&mut self, update: &PlayableRoomUpdate, seq: u64) -> bool {
        let revision_before = self.playable.revision;
        if !update.apply_to_playable(&mut self.playable) {
            return false;
        }
        self.seq = seq;
        let mut delta = PlayableDelta::for_revision(self.playable.revision);
        for change in &update.group_changes {
            delta.dirty_groups.push(GroupId(change.group));
        }
        for change in &update.piece_changes {
            delta.dirty_pieces.push(PieceId(change.piece));
        }
        for edge in &update.activated_edges {
            delta.dirty_edges.push(EdgeId(*edge));
        }
        delta.z_order_changed = update.z_order_changed;
        delta.membership_changed = update.membership_changed;
        delta.solved_changed = update.solved_changed;
        let mut scratch = ProjectionScratch::with_capacity(self.playable.piece_count());
        self.visual
            .apply_delta(&self.playable, &delta, &mut scratch);
        for change in &update.piece_changes {
            self.visual
                .snap_piece_to_authoritative(PieceId(change.piece));
        }
        let _ = revision_before;
        true
    }

    /// Rebuilds the `visual` projection cache from `playable` from scratch.
    /// Used after large mutations like reordering z_order independently of
    /// the wire update path.
    pub fn rebuild_visual(&mut self) {
        self.visual = VisualState::rebuild_from(&self.playable);
    }

    /// Encodes the live state into a transport-ready snapshot.
    pub fn to_snapshot(&self) -> PlayableGameSnapshot {
        let mut snapshot = PlayableGameSnapshot::from_playable(
            self.puzzle.clone(),
            self.rules,
            self.scramble_nonce,
            &self.playable,
            self.focused_piece,
        );
        snapshot.seq = self.seq;
        snapshot.state.revision = self.playable.revision;
        snapshot
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use heddobureika_core::{PlayableAction, PlayableRoomUpdateKind, Position2, PuzzleImageRef};

    #[test]
    fn wire_update_snaps_dirty_visual_pose_to_authoritative_pose() {
        let puzzle = PuzzleInfo {
            label: "test".to_string(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: "test".to_string(),
            },
            rows: 1,
            cols: 2,
            shape_seed: 1,
            image_width: 200,
            image_height: 100,
        };
        let positions = [(0.0, 0.0), (100.0, 0.0)];
        let rotations = [0.0, 0.0];
        let flips = [false, false];
        let order = [0, 1];
        let mut game = AppGameState::scrambled(
            puzzle,
            GameRules::default(),
            1,
            &positions,
            &rotations,
            &flips,
            &order,
            100.0,
            100.0,
        )
        .expect("scrambled app game");
        let stale_pose = Pose2::try_from_mm_degrees(20.0, 0.5, 0.0).expect("finite pose");
        assert!(game.visual.set_piece_visual_pose(PieceId(1), stale_pose));

        let mut server_playable = game.playable.clone();
        let group = server_playable
            .logical
            .group_of(PieceId(1))
            .expect("piece group");
        let batch = server_playable.apply_action_only(
            PlayableAction::TranslateGroup {
                group,
                drop_pos: Position2::try_from_mm(5.0, 0.5).expect("finite position"),
            },
            None,
        );
        let update = PlayableRoomUpdate::from_batch_and_playable(
            PlayableRoomUpdateKind::ActionOnly,
            &batch,
            &server_playable,
        );

        assert!(game.apply_wire_update(&update, update.revision_after));
        assert_eq!(
            game.visual.piece_visual_pose()[1],
            game.visual.authoritative_piece_pose()[1]
        );
        assert_eq!(game.visual.piece_visual_pose()[1].x_mm(), 5.0);
    }
}
