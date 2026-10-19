//! Portable puzzle authority shared by server and player-hosted sessions.
//!
//! Callers serialise commands, supply authenticated client IDs and time, and
//! perform I/O from the returned effects. This crate has no platform runtime,
//! network, authentication, storage, or wall-clock dependency.

use heddobureika_core::game::{
    compute_workspace_layout, rotate_vec, safety_corrections_after_detach,
};
use heddobureika_core::{
    ActionId, ClientId, ClientMsg, GameBridgeError, MergePolicy, OwnershipReason,
    PlayableGameSnapshot, PlayablePoseSnapshot, PlayablePositionSnapshot, PlayableRoomUpdate,
    PlayableRoomUpdateKind, PlayableUpdateBatch, RoomControlUpdate, ServerMsg,
};
use heddobureika_game::{AngleDeg, GroupId, PlayableAction};
use std::collections::HashMap;

/// The current server lease duration. Adapters choose when to drive expiry.
pub const OWNERSHIP_TIMEOUT_MS: i64 = 30_000;

/// An accepted command's ordered effects. Several events may share a revision.
#[derive(Debug)]
pub struct CommandEffects {
    pub events: Vec<ServerMsg>,
    /// Preserve the server's full-state checkpoint scheduling for final actions.
    pub full_state: bool,
}
impl CommandEffects {
    fn new(events: Vec<ServerMsg>, full_state: bool) -> Self {
        Self { events, full_state }
    }
}

/// Group ownership, exposed read-only through `SessionAuthority::ownerships`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ownership {
    pub owner_id: ClientId,
    pub anchor_id: u32,
    pub since_ms: i64,
}

#[derive(Clone, Default)]
pub struct SessionAuthority {
    live: Option<RoomLivePuzzle>,
    owners_by_anchor: HashMap<u32, Ownership>,
    owner_by_client: HashMap<ClientId, u32>,
}

impl SessionAuthority {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn snapshot(&self) -> Option<&PlayableGameSnapshot> {
        self.live.as_ref().map(|live| &live.snapshot)
    }

    /// Replace the puzzle and clear claims. Invalid snapshots leave it empty,
    /// matching the server's existing snapshot restore behaviour.
    pub fn set_snapshot(&mut self, snapshot: PlayableGameSnapshot) -> bool {
        self.live = RoomLivePuzzle::from_snapshot(snapshot);
        self.owners_by_anchor.clear();
        self.owner_by_client.clear();
        self.live.is_some()
    }

    pub fn clear(&mut self) {
        *self = Self::new();
    }

    pub fn ownerships(&self) -> impl Iterator<Item = Ownership> + '_ {
        self.owners_by_anchor.values().copied()
    }

    /// Apply a gameplay command with the existing server semantics. `None`
    /// means ignored (invalid, unauthorised, no-op, or adapter-owned command).
    /// Init, assets and ping are handled by adapters. `base_revision` is
    /// deliberately ignored, as in the existing server protocol.
    pub fn apply(
        &mut self,
        client_id: ClientId,
        command: ClientMsg,
        now: i64,
    ) -> Option<CommandEffects> {
        match command {
            ClientMsg::Select { piece_id } => self.select(client_id, piece_id, now),
            ClientMsg::SendToBack { piece_id } => self.send_to_back(client_id, piece_id),
            ClientMsg::Move {
                piece_id,
                drop_pos,
                client_seq,
                ..
            } => self.move_group(client_id, piece_id, drop_pos, client_seq, now),
            ClientMsg::Transform {
                piece_id,
                drop_pose,
                client_seq,
                ..
            } => self.transform(client_id, piece_id, drop_pose, client_seq, now),
            ClientMsg::Flip {
                piece_id,
                flipped,
                drop_pose,
                ..
            } => self.flip(client_id, piece_id, flipped, drop_pose),
            ClientMsg::Detach { piece_id, .. } => self.detach(client_id, piece_id, now),
            ClientMsg::Rotate {
                piece_id,
                drop_rotation_deg,
                ..
            } => self.finalize(client_id, piece_id, None, Some(drop_rotation_deg), None),
            ClientMsg::Place {
                piece_id,
                drop_pose,
                client_seq,
                ..
            } => self.finalize(
                client_id,
                piece_id,
                Some(PlayablePositionSnapshot {
                    x_mm: drop_pose.x_mm,
                    y_mm: drop_pose.y_mm,
                }),
                Some(drop_pose.rotation_deg),
                Some(client_seq),
            ),
            ClientMsg::Release { piece_id } => {
                if self.owners_by_anchor.get(&piece_id)?.owner_id != client_id {
                    return None;
                }
                self.release_client(client_id, OwnershipReason::Released)
                    .map(|event| CommandEffects::new(vec![event], false))
            }
            ClientMsg::Init { .. } | ClientMsg::AssetRequest { .. } | ClientMsg::Ping { .. } => {
                None
            }
        }
    }

    /// P2P finalisation cannot merge or move another player's held group.
    /// Preview only structural final actions so ordinary drags stay cheap.
    /// The existing server continues to use `apply` with its legacy policy.
    pub fn apply_exclusive(
        &mut self,
        actor: ClientId,
        command: ClientMsg,
        now: i64,
    ) -> Option<CommandEffects> {
        if !matches!(command, ClientMsg::Place { .. } | ClientMsg::Rotate { .. }) {
            return self.apply(actor, command, now);
        }
        let snapshot = self.snapshot()?;
        let held: Vec<_> = self
            .ownerships()
            .filter(|owner| owner.owner_id != actor)
            .map(|owner| {
                (
                    owner.anchor_id as usize,
                    playable_group_members(snapshot, owner.anchor_id as usize),
                )
            })
            .collect();
        let mut candidate = self.clone();
        let effects = candidate.apply(actor, command, now)?;
        for (anchor, members) in held {
            if playable_group_members(candidate.snapshot()?, anchor) != members {
                return None;
            }
        }
        *self = candidate;
        Some(effects)
    }

    /// Release a disconnected player's claim. Connection grace periods and
    /// whether another socket is still alive are adapter responsibilities.
    pub fn release_client(
        &mut self,
        client_id: ClientId,
        reason: OwnershipReason,
    ) -> Option<ServerMsg> {
        let anchor = self.owner_by_client.remove(&client_id)?;
        self.owners_by_anchor.remove(&anchor);
        let seq = self.bump_seq();
        Some(control_update_msg(
            seq,
            RoomControlUpdate::Ownership {
                group_anchor: anchor,
                owner: None,
                reason,
            },
            Some(client_id),
            None,
        ))
    }

    /// Expire leases using caller-supplied time; no clock or timer is required.
    pub fn expire_ownership(&mut self, now: i64) -> Vec<ServerMsg> {
        let expired: Vec<_> = self
            .ownerships()
            .filter(|owner| now.saturating_sub(owner.since_ms) >= OWNERSHIP_TIMEOUT_MS)
            .collect();
        expired
            .into_iter()
            .map(|owner| {
                self.owners_by_anchor.remove(&owner.anchor_id);
                self.owner_by_client.remove(&owner.owner_id);
                let seq = self.bump_seq();
                control_update_msg(
                    seq,
                    RoomControlUpdate::Ownership {
                        group_anchor: owner.anchor_id,
                        owner: None,
                        reason: OwnershipReason::Timeout,
                    },
                    None,
                    None,
                )
            })
            .collect()
    }

    pub fn next_ownership_expiry(&self) -> Option<i64> {
        self.ownerships()
            .map(|owner| owner.since_ms.saturating_add(OWNERSHIP_TIMEOUT_MS))
            .min()
    }

    fn bump_seq(&mut self) -> u64 {
        if let Some(live) = self.live.as_mut() {
            let next = live.snapshot.seq.saturating_add(1);
            live.set_seq(next);
            next
        } else {
            0
        }
    }
    fn select(&mut self, client_id: ClientId, piece_id: u32, now: i64) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                return None;
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;

            if let Some(existing) = inner.owners_by_anchor.get(&anchor_id) {
                if existing.owner_id != client_id {
                    return None;
                }
            }

            let mut pending_updates = Vec::new();
            if let Some(prev_anchor) = inner.owner_by_client.get(&client_id).copied() {
                if prev_anchor != anchor_id {
                    inner.owner_by_client.remove(&client_id);
                    inner.owners_by_anchor.remove(&prev_anchor);
                    let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
                    runtime_snapshot.set_seq(seq);
                    pending_updates.push(control_update_msg(
                        seq,
                        RoomControlUpdate::Ownership {
                            group_anchor: prev_anchor,
                            owner: None,
                            reason: OwnershipReason::AutoRelease,
                        },
                        Some(client_id),
                        None,
                    ));
                }
            }

            inner.owners_by_anchor.insert(
                anchor_id,
                Ownership {
                    owner_id: client_id,
                    anchor_id,
                    since_ms: now,
                },
            );
            inner.owner_by_client.insert(client_id, anchor_id);

            // Bring the selected group toward the front to its geometry-aware
            // fitting depth (matches the client's optimistic reorder), then
            // broadcast the resulting full order.
            let _ = runtime_snapshot.bring_forward(anchor_id);
            let group_order = playable_group_order_anchors(&runtime_snapshot.snapshot);

            let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
            runtime_snapshot.set_seq(seq);
            let update_msg = control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: Some(client_id),
                    reason: OwnershipReason::Granted,
                },
                Some(client_id),
                None,
            );
            let group_order_update = control_update_msg(
                seq,
                RoomControlUpdate::GroupOrder { order: group_order },
                Some(client_id),
                None,
            );

            (pending_updates, update_msg, group_order_update)
        };
        let (mut events, ownership, order) = value;
        events.extend([ownership, order]);
        Some(CommandEffects::new(events, false))
    }

    fn send_to_back(&mut self, client_id: ClientId, piece_id: u32) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                return None;
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }

            // Send the group to its geometry-aware fitting depth (matches the
            // client's optimistic reorder); skip the broadcast if nothing moved.
            if !runtime_snapshot.send_backward(anchor_id) {
                return None;
            }
            let group_order = playable_group_order_anchors(&runtime_snapshot.snapshot);

            let seq = runtime_snapshot.snapshot.seq.saturating_add(1);
            runtime_snapshot.set_seq(seq);
            let group_order_update = control_update_msg(
                seq,
                RoomControlUpdate::GroupOrder { order: group_order },
                Some(client_id),
                None,
            );

            group_order_update
        };
        Some(CommandEffects::new(vec![value], false))
    }

    fn move_group(
        &mut self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pos: PlayablePositionSnapshot,
        client_seq: u64,
        now: i64,
    ) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }
            if let Some(owner) = inner.owners_by_anchor.get_mut(&anchor_id) {
                owner.since_ms = now;
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                return None;
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                return None;
            }
            // A move keeps the group's current rotation; project members about
            // the proposed drop center and reject only if the whole group would
            // leave the workspace.
            let out_of_bounds = if let Some(geometry) = runtime_snapshot.geometry.as_ref() {
                let group = runtime_snapshot
                    .snapshot
                    .state
                    .piece_group
                    .get(anchor_id_usize)
                    .map(|g| *g as usize)
                    .unwrap_or(0);
                let anchor_rot = runtime_snapshot
                    .snapshot
                    .state
                    .group_pose
                    .get(group)
                    .map(|pose| pose.rotation_deg)
                    .unwrap_or(0.0);
                !group_in_bounds(
                    &runtime_snapshot.snapshot,
                    geometry,
                    &members,
                    anchor_id_usize,
                    (drop_pos.x_mm, drop_pos.y_mm),
                    anchor_rot,
                )
            } else {
                false
            };
            if out_of_bounds {
                return None;
            }

            let action_id =
                bridge_action_id(client_id, Some(client_seq), runtime_snapshot.snapshot.seq);
            let batch =
                match apply_bridge_live_move(runtime_snapshot, anchor_id, drop_pos, action_id) {
                    Ok(batch) => batch,
                    Err(_) => {
                        return None;
                    }
                };

            let update = Some(playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::ActionOnly,
                Some(client_id),
                Some(client_seq).filter(|value| *value != 0),
            ));

            update
        };
        Some(CommandEffects::new(vec![value?], false))
    }

    fn transform(
        &mut self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pose: PlayablePoseSnapshot,
        client_seq: u64,
        now: i64,
    ) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }
            if let Some(owner) = inner.owners_by_anchor.get_mut(&anchor_id) {
                owner.since_ms = now;
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                return None;
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                return None;
            }

            // A transform sets the group's rotation to the drop pose's; project
            // members about the proposed center + rotation.
            let out_of_bounds = if let Some(geometry) = runtime_snapshot.geometry.as_ref() {
                !group_in_bounds(
                    &runtime_snapshot.snapshot,
                    geometry,
                    &members,
                    anchor_id_usize,
                    (drop_pose.x_mm, drop_pose.y_mm),
                    drop_pose.rotation_deg,
                )
            } else {
                false
            };
            if out_of_bounds {
                return None;
            }

            let action_id =
                bridge_action_id(client_id, Some(client_seq), runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_live_transform(
                runtime_snapshot,
                anchor_id,
                drop_pose,
                action_id,
            ) {
                Ok(batch) => batch,
                Err(_) => {
                    return None;
                }
            };

            let update = Some(playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::ActionOnly,
                Some(client_id),
                Some(client_seq).filter(|value| *value != 0),
            ));

            update
        };
        Some(CommandEffects::new(vec![value?], false))
    }

    fn flip(
        &mut self,
        client_id: ClientId,
        piece_id: u32,
        flipped: bool,
        drop_pose: PlayablePoseSnapshot,
    ) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                return None;
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;
            if members.len() != 1 || anchor_id != piece_id {
                return None;
            }

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }

            let action_id = bridge_action_id(client_id, None, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_flip(
                runtime_snapshot,
                piece_id,
                flipped,
                Some(drop_pose),
                action_id,
            ) {
                Ok(batch) => batch,
                Err(_) => {
                    return None;
                }
            };

            inner.owner_by_client.remove(&client_id);
            inner.owners_by_anchor.remove(&anchor_id);

            let flip_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::RestrictedAction,
                Some(client_id),
                None,
            );
            let ownership_update = control_update_msg(
                runtime_snapshot.snapshot.seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                Some(client_id),
                None,
            );

            Some((flip_update, ownership_update))
        };
        let (flip, ownership) = value?;
        Some(CommandEffects::new(vec![flip, ownership], false))
    }

    fn detach(&mut self, client_id: ClientId, piece_id: u32, now: i64) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if piece_id as usize >= total {
                return None;
            }

            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, piece_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id = members[0] as u32;
            if members.len() == 1 && anchor_id == piece_id {
                return None;
            }

            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }

            let action_id = bridge_action_id(client_id, None, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_detach(runtime_snapshot, piece_id, action_id) {
                Ok(batch) => batch,
                Err(_) => {
                    return None;
                }
            };

            inner.owner_by_client.insert(client_id, piece_id);
            if anchor_id != piece_id {
                inner.owners_by_anchor.remove(&anchor_id);
            }
            inner.owners_by_anchor.insert(
                piece_id,
                Ownership {
                    owner_id: client_id,
                    anchor_id: piece_id,
                    since_ms: now,
                },
            );

            let detach_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::RestrictedAction,
                Some(client_id),
                None,
            );
            let seq = runtime_snapshot.snapshot.seq;
            let mut ownership_updates = Vec::new();
            if anchor_id != piece_id {
                ownership_updates.push(control_update_msg(
                    seq,
                    RoomControlUpdate::Ownership {
                        group_anchor: anchor_id,
                        owner: None,
                        reason: OwnershipReason::Released,
                    },
                    Some(client_id),
                    None,
                ));
            }
            ownership_updates.push(control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: piece_id,
                    owner: Some(client_id),
                    reason: OwnershipReason::Granted,
                },
                Some(client_id),
                None,
            ));

            Some((detach_update, ownership_updates))
        };
        let (detach, ownership) = value?;
        let mut events = vec![detach];
        events.extend(ownership);
        Some(CommandEffects::new(events, true))
    }

    fn finalize(
        &mut self,
        client_id: ClientId,
        anchor_id: u32,
        drop_pos: Option<PlayablePositionSnapshot>,
        rot_deg: Option<f32>,
        client_seq: Option<u64>,
    ) -> Option<CommandEffects> {
        let value = {
            let inner = self;
            let runtime_snapshot = inner.live.as_mut()?;
            let owns_anchor = inner
                .owners_by_anchor
                .get(&anchor_id)
                .map(|owner| owner.owner_id == client_id)
                .unwrap_or(false);
            if !owns_anchor {
                return None;
            }
            let total = runtime_snapshot.snapshot.state.topology_piece_count as usize;
            if anchor_id as usize >= total {
                return None;
            }
            let mut members =
                match playable_group_members(&runtime_snapshot.snapshot, anchor_id as usize) {
                    Some(members) => members,
                    None => {
                        return None;
                    }
                };
            members.sort_unstable();
            let anchor_id_usize = members[0];
            if anchor_id_usize as u32 != anchor_id {
                return None;
            }

            let action_id = bridge_action_id(client_id, client_seq, runtime_snapshot.snapshot.seq);
            let batch = match apply_bridge_finalize(
                runtime_snapshot,
                anchor_id,
                drop_pos,
                rot_deg,
                action_id,
            ) {
                Ok(batch) => batch,
                Err(_) => {
                    return None;
                }
            };

            let released_anchor = inner.owner_by_client.remove(&client_id);
            if let Some(released_anchor) = released_anchor {
                inner.owners_by_anchor.remove(&released_anchor);
            }
            let playable_update = playable_update_msg_from_batch(
                &runtime_snapshot.snapshot,
                &batch,
                PlayableRoomUpdateKind::Snap,
                Some(client_id),
                client_seq.filter(|value| *value != 0),
            );

            (playable_update, released_anchor)
        };
        let (playable_update, released_anchor) = value;
        let mut events = Vec::new();
        if let Some(anchor_id) = released_anchor {
            let seq = match &playable_update {
                ServerMsg::PlayableUpdate { seq, .. } => *seq,
                _ => unreachable!(),
            };
            events.push(control_update_msg(
                seq,
                RoomControlUpdate::Ownership {
                    group_anchor: anchor_id,
                    owner: None,
                    reason: OwnershipReason::Released,
                },
                Some(client_id),
                client_seq.filter(|value| *value != 0),
            ));
        }
        events.push(playable_update);
        Some(CommandEffects::new(events, true))
    }
}

/// In-memory live puzzle state for a session authority.
///
/// `playable` is the authoritative live state — every action mutates it
/// directly. `snapshot` is the transport/persistence shadow; we resync
/// `snapshot.state` from `playable` whenever we are about to broadcast a
/// `ServerMsg::State` or persist to KV. Other snapshot fields (`puzzle`,
/// `rules`, `scramble_nonce`, `seq`) are owned by the snapshot and copied
/// or read in place.
#[derive(Clone)]
struct RoomLivePuzzle {
    snapshot: PlayableGameSnapshot,
    playable: heddobureika_game::GenericPlayableState,
    /// Cached pixel geometry for the bounds clamp. `None` only when the
    /// puzzle is degenerate (zero image size / pieces). Recomputed only here,
    /// since a live puzzle's topology, image size, and rules never change
    /// once set — actions just move pieces.
    geometry: Option<RoomGeometry>,
}

impl RoomLivePuzzle {
    fn from_snapshot(snapshot: PlayableGameSnapshot) -> Option<Self> {
        let playable = snapshot.restore_playable_from_spec().ok()?;
        let geometry = RoomGeometry::from_topology(
            &playable.logical.topology,
            snapshot.puzzle.image_width,
            snapshot.puzzle.image_height,
            snapshot.rules.workspace_padding_ratio,
        );
        Some(Self {
            snapshot,
            playable,
            geometry,
        })
    }

    fn sync_snapshot_from_playable(&mut self) {
        let focused_piece = self.snapshot.state.focused_piece;
        self.snapshot.state = heddobureika_core::PlayableGameStateSnapshot::from_playable(
            &self.playable,
            focused_piece,
        );
        self.snapshot.seq = self.snapshot.state.revision;
    }

    /// Bumps the wire sequence number used for ordering room messages.
    ///
    /// All three of `snapshot.seq`, `state.revision`, and `playable.revision`
    /// are kept identical so that the next action's wire
    /// `revision_before` matches whatever the client has tracked from the
    /// latest control or playable update. Clients must mirror this by
    /// advancing their local `playable.revision` on every control update too.
    fn set_seq(&mut self, seq: u64) {
        self.snapshot.seq = seq;
        self.snapshot.state.revision = seq;
        self.playable.revision = seq;
    }

    /// Drag-start reorder: bring the group containing `anchor` toward the front
    /// to its geometry-aware "fitting depth" (see [`heddobureika_game::z_depth`]).
    /// Deterministic, so it matches the originating client's optimistic result.
    fn bring_forward(&mut self, anchor: u32) -> bool {
        let changed = self.playable.bring_forward_to_fitting_depth(&[anchor]);
        if changed {
            self.sync_snapshot_from_playable();
        }
        changed
    }

    /// Shake reorder: send the group containing `anchor` to its geometry-aware
    /// "fitting depth" (as far back as possible without hiding it). Deterministic.
    fn send_backward(&mut self, anchor: u32) -> bool {
        let changed = self.playable.send_backward_to_fitting_depth(&[anchor]);
        if changed {
            self.sync_snapshot_from_playable();
        }
        changed
    }
}

fn bridge_action_id(client_id: ClientId, client_seq: Option<u64>, room_seq: u64) -> ActionId {
    let sequence = client_seq.filter(|value| *value != 0).unwrap_or(room_seq);
    let mixed = mix_bridge_action_id(
        client_id.as_u64().wrapping_add(0x9e37_79b9_7f4a_7c15)
            ^ sequence.wrapping_mul(0xbf58_476d_1ce4_e5b9),
    );
    ActionId(if mixed == 0 { 1 } else { mixed })
}

fn mix_bridge_action_id(mut value: u64) -> u64 {
    value ^= value >> 30;
    value = value.wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value ^= value >> 27;
    value = value.wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

fn apply_bridge_live_move(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pos: PlayablePositionSnapshot,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let action = PlayableAction::TranslateGroup {
        group,
        drop_pos: drop_pos.to_position()?,
    };
    let batch = live.playable.apply_action_only(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_live_transform(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pose: PlayablePoseSnapshot,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let pose = drop_pose.to_pose()?;
    let action = PlayableAction::TransformGroupTo {
        group,
        drop_pos: heddobureika_game::Position2 {
            x: pose.x,
            y: pose.y,
        },
        drop_rotation: pose.rotation,
    };
    let batch = live.playable.apply_action_only(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_finalize(
    live: &mut RoomLivePuzzle,
    anchor_id: u32,
    drop_pos: Option<PlayablePositionSnapshot>,
    rot_deg: Option<f32>,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let group = playable_group_for_anchor(&live.playable, anchor_id)?;
    let action = match (drop_pos, rot_deg) {
        (Some(drop_pos), Some(rot_deg)) => {
            let drop_rotation =
                AngleDeg::try_new(rot_deg).ok_or(GameBridgeError::InvalidActionPose)?;
            PlayableAction::TransformGroupTo {
                group,
                drop_pos: drop_pos.to_position()?,
                drop_rotation,
            }
        }
        (Some(drop_pos), None) => PlayableAction::TranslateGroup {
            group,
            drop_pos: drop_pos.to_position()?,
        },
        (None, Some(rot_deg)) => {
            let drop_rotation =
                AngleDeg::try_new(rot_deg).ok_or(GameBridgeError::InvalidActionPose)?;
            PlayableAction::RotateGroupTo {
                group,
                drop_rotation,
            }
        }
        (None, None) => PlayableAction::RotateGroupTo {
            group,
            drop_rotation: live
                .playable
                .pose_of(group)
                .ok_or(GameBridgeError::InvalidPieceId {
                    piece_id: anchor_id,
                })?
                .rotation,
        },
    };
    let batch =
        live.playable
            .apply_action_with_snap(action, Some(action_id), MergePolicy::KeepFixedGroup);
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn playable_group_for_anchor(
    playable: &heddobureika_game::GenericPlayableState,
    anchor_id: u32,
) -> Result<GroupId, GameBridgeError> {
    playable
        .logical
        .group_of(heddobureika_game::PieceId(anchor_id))
        .ok_or(GameBridgeError::InvalidPieceId {
            piece_id: anchor_id,
        })
}

fn apply_bridge_flip(
    live: &mut RoomLivePuzzle,
    piece_id: u32,
    flipped: bool,
    drop_pose: Option<PlayablePoseSnapshot>,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let piece = heddobureika_game::PieceId(piece_id);
    if piece.as_usize() >= live.playable.piece_count() {
        return Err(GameBridgeError::InvalidPieceId { piece_id });
    }
    // The client computes the click-pivoted post-flip pose and sends it, so
    // the authoritative state reproduces that exact pose. Fall back to the
    // current world pose (reflect-about-anchor) when none is supplied.
    let target_pose = match drop_pose.and_then(|pose| pose.to_pose().ok()) {
        Some(pose) => pose,
        None => live
            .playable
            .piece_world_pose(piece)
            .ok_or(GameBridgeError::InvalidPieceId { piece_id })?,
    };
    let target_flip = if flipped {
        heddobureika_game::FlipState::Flipped
    } else {
        heddobureika_game::FlipState::Normal
    };
    let action = heddobureika_game::RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let batch = live
        .playable
        .apply_restricted_action_batch(action, Some(action_id));
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn apply_bridge_detach(
    live: &mut RoomLivePuzzle,
    piece_id: u32,
    action_id: ActionId,
) -> Result<PlayableUpdateBatch, GameBridgeError> {
    let piece = heddobureika_game::PieceId(piece_id);
    if piece.as_usize() >= live.playable.piece_count() {
        return Err(GameBridgeError::InvalidPieceId { piece_id });
    }
    let group = live
        .playable
        .logical
        .group_of(piece)
        .ok_or(GameBridgeError::InvalidPieceId { piece_id })?;
    let target_pose = live
        .playable
        .piece_world_pose(piece)
        .ok_or(GameBridgeError::InvalidPieceId { piece_id })?;
    let target_flip = live.playable.flip_of(group).unwrap_or_default();
    // Capture the original group's members before the detach so we can
    // visit every component the detach may produce and apply safety
    // corrections.
    let original_members: Vec<heddobureika_game::PieceId> =
        live.playable.logical.members_of(group).collect();
    let action = heddobureika_game::RestrictedPlayableAction::DetachPieceAsGroup {
        piece,
        target_pose,
        target_flip,
    };
    let mut batch = live
        .playable
        .apply_restricted_action_batch(action, Some(action_id));
    // Force-move each resulting group whose anchor sits outside its applicable
    // safety bound (loose for singletons, tight for multi-piece). The
    // corrections are merged into the detach batch's dirty-group set so the
    // wire update carries the post-correction poses atomically — a client
    // receiving the update sees the final state directly, no intermediate
    // "unsafe" pose flash.
    let puzzle = live.snapshot.puzzle.clone();
    let rules = live.snapshot.rules;
    let placement = heddobureika_core::build_topology_from_spec(&puzzle.to_spec())
        .map(|t| t.image_placement(puzzle.image_width, puzzle.image_height))
        .unwrap_or(heddobureika_game::ImagePlacement {
            pose_unit_px: [1.0, 1.0],
            origin_px: [0.0, 0.0],
            frame_px: [puzzle.image_width as f32, puzzle.image_height as f32],
        });
    let corrections = safety_corrections_after_detach(
        &live.playable,
        &original_members,
        &puzzle,
        &rules,
        placement,
    );
    for (corr_group, drop_pos) in corrections {
        let correction_batch = live.playable.apply_action_only(
            PlayableAction::TranslateGroup {
                group: corr_group,
                drop_pos,
            },
            None,
        );
        for dirty_group in correction_batch.delta.dirty_groups.iter().copied() {
            if !batch.delta.dirty_groups.contains(&dirty_group) {
                batch.delta.dirty_groups.push(dirty_group);
            }
        }
        batch.revision_after = correction_batch.revision_after;
    }
    live.sync_snapshot_from_playable();
    Ok(batch)
}

fn playable_update_msg_from_batch(
    snapshot: &PlayableGameSnapshot,
    batch: &PlayableUpdateBatch,
    kind: PlayableRoomUpdateKind,
    source: Option<ClientId>,
    client_seq: Option<u64>,
) -> ServerMsg {
    ServerMsg::PlayableUpdate {
        seq: snapshot.seq,
        update: PlayableRoomUpdate::from_batch_and_state(kind, batch, &snapshot.state),
        source,
        client_seq,
    }
}

fn control_update_msg(
    seq: u64,
    update: RoomControlUpdate,
    source: Option<ClientId>,
    client_seq: Option<u64>,
) -> ServerMsg {
    ServerMsg::ControlUpdate {
        seq,
        update,
        source,
        client_seq,
    }
}

/// Topology-agnostic pixel geometry for the authoritative bounds clamp.
///
/// Derived from the topology's [`heddobureika_game::ImagePlacement`] — the SAME mapping the client
/// renderer uses — so the server and client never disagree on where pieces sit.
/// A pose-mm point `p` maps to the pixel `origin + p * pose_unit`. `pose_unit`
/// is per-cell pixel size for stretch-to-fill topologies (grid/hex/voronoi) and
/// a uniform scale for cropping ones (triangular); `origin` is the frame's
/// top-left (non-zero only when letterboxed). `piece_half_*` hold each piece's
/// half bounding-box in pixels, and `view_*` is the workspace rect in pixels.
///
/// Built once per puzzle (cached on `RoomLivePuzzle`) — actions only move
/// pieces, never change the topology, image size, or rules it derives from.
#[derive(Clone)]
struct RoomGeometry {
    pose_unit_x: f32,
    pose_unit_y: f32,
    origin_x: f32,
    origin_y: f32,
    view_min_x: f32,
    view_max_x: f32,
    view_min_y: f32,
    view_max_y: f32,
    piece_half_w: Vec<f32>,
    piece_half_h: Vec<f32>,
}

impl RoomGeometry {
    fn from_topology(
        topology: &heddobureika_game::GenericTopology,
        image_width: u32,
        image_height: u32,
        workspace_padding_ratio: f32,
    ) -> Option<Self> {
        if image_width == 0 || image_height == 0 {
            return None;
        }
        let total = topology.piece_count() as usize;
        if total == 0 {
            return None;
        }
        let placement = topology.image_placement(image_width, image_height);
        let [pose_unit_x, pose_unit_y] = placement.pose_unit_px;
        let [origin_x, origin_y] = placement.origin_px;
        let [frame_w, frame_h] = placement.frame_px;
        if pose_unit_x <= 0.0 || pose_unit_y <= 0.0 || frame_w <= 0.0 || frame_h <= 0.0 {
            return None;
        }
        let layout = compute_workspace_layout(
            origin_x,
            origin_y,
            frame_w,
            frame_h,
            workspace_padding_ratio,
        );
        let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
        let view_min_x = layout.view_min_x / puzzle_scale;
        let view_min_y = layout.view_min_y / puzzle_scale;
        let view_max_x = view_min_x + layout.view_width / puzzle_scale;
        let view_max_y = view_min_y + layout.view_height / puzzle_scale;
        let mut piece_half_w = Vec::with_capacity(total);
        let mut piece_half_h = Vec::with_capacity(total);
        for idx in 0..total as u32 {
            let (ex, ey) = topology.piece_extent_mm(heddobureika_game::PieceId(idx));
            piece_half_w.push(ex.as_mm_f32() * pose_unit_x * 0.5);
            piece_half_h.push(ey.as_mm_f32() * pose_unit_y * 0.5);
        }
        Some(Self {
            pose_unit_x,
            pose_unit_y,
            origin_x,
            origin_y,
            view_min_x,
            view_max_x,
            view_min_y,
            view_max_y,
            piece_half_w,
            piece_half_h,
        })
    }
}

fn playable_group_members(snapshot: &PlayableGameSnapshot, piece_id: usize) -> Option<Vec<usize>> {
    let total = snapshot.state.topology_piece_count as usize;
    if piece_id >= total || snapshot.state.piece_group.len() != total {
        return None;
    }
    let group = *snapshot.state.piece_group.get(piece_id)?;
    let mut members = snapshot
        .state
        .piece_group
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, candidate)| (candidate == group).then_some(idx))
        .collect::<Vec<_>>();
    members.sort_unstable();
    (!members.is_empty()).then_some(members)
}

fn playable_group_anchor(snapshot: &PlayableGameSnapshot, group: u32) -> Option<usize> {
    snapshot
        .state
        .piece_group
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, candidate)| (candidate == group).then_some(idx))
        .min()
}

fn playable_group_order_anchors(snapshot: &PlayableGameSnapshot) -> Vec<u32> {
    let mut order = Vec::with_capacity(snapshot.state.z_order.len());
    for group in snapshot.state.z_order.iter().copied() {
        let Some(anchor) = playable_group_anchor(snapshot, group) else {
            continue;
        };
        let anchor = anchor as u32;
        if !order.contains(&anchor) {
            order.push(anchor);
        }
    }
    order
}

/// Authoritative bounds clamp for a proposed group move/transform.
///
/// Projects every member piece to its pixel center for the proposed anchor
/// pose, then accepts the placement when at least one piece center stays
/// inside the workspace (inset by that piece's own half extent — and a full
/// extent for multi-piece groups, matching the original grid heuristic). The
/// rejection condition is "the whole group would leave the workspace".
///
/// Works for any topology: `pose_unit_*` scale pose-mm to pixels (equal to a
/// grid cell's pixel size for grids, so grid behaviour is preserved) and
/// `piece_half_*` carry each piece's true bounding box.
fn group_in_bounds(
    snapshot: &PlayableGameSnapshot,
    geometry: &RoomGeometry,
    members: &[usize],
    anchor_id: usize,
    anchor_center_mm: (f32, f32),
    anchor_rot_deg: f32,
) -> bool {
    let Some(group) = snapshot
        .state
        .piece_group
        .get(anchor_id)
        .map(|g| *g as usize)
    else {
        return false;
    };
    let Some(anchor_local) = snapshot.state.piece_local_pose.get(anchor_id).copied() else {
        return false;
    };
    let flipped = snapshot
        .state
        .group_flip
        .get(group)
        .copied()
        .unwrap_or(false);
    let multi = members.len() > 1;
    let anchor_px = (
        geometry.origin_x + anchor_center_mm.0 * geometry.pose_unit_x,
        geometry.origin_y + anchor_center_mm.1 * geometry.pose_unit_y,
    );
    for &id in members {
        let Some(piece_local) = snapshot.state.piece_local_pose.get(id).copied() else {
            continue;
        };
        // Scale the local offset to pixels first, THEN rotate — the same
        // aspect-aware order the renderer uses (pose units are not square).
        let mut dx = (piece_local.x_mm - anchor_local.x_mm) * geometry.pose_unit_x;
        let dy = (piece_local.y_mm - anchor_local.y_mm) * geometry.pose_unit_y;
        if flipped {
            dx = -dx;
        }
        let (rx, ry) = rotate_vec(dx, dy, anchor_rot_deg);
        let center_x = anchor_px.0 + rx;
        let center_y = anchor_px.1 + ry;
        let half_w = geometry
            .piece_half_w
            .get(id)
            .copied()
            .unwrap_or(geometry.pose_unit_x * 0.5);
        let half_h = geometry
            .piece_half_h
            .get(id)
            .copied()
            .unwrap_or(geometry.pose_unit_y * 0.5);
        let inset_w = if multi { half_w * 3.0 } else { half_w };
        let inset_h = if multi { half_h * 3.0 } else { half_h };
        let (mut min_x, mut max_x) = (geometry.view_min_x + inset_w, geometry.view_max_x - inset_w);
        if max_x < min_x {
            let mid = (geometry.view_min_x + geometry.view_max_x) * 0.5;
            min_x = mid;
            max_x = mid;
        }
        let (mut min_y, mut max_y) = (geometry.view_min_y + inset_h, geometry.view_max_y - inset_h);
        if max_y < min_y {
            let mid = (geometry.view_min_y + geometry.view_max_y) * 0.5;
            min_y = mid;
            max_y = mid;
        }
        if center_x >= min_x && center_x <= max_x && center_y >= min_y && center_y <= max_y {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests;
