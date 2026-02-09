use super::*;

#[derive(clap::Subcommand)]
pub(super) enum BotCommand {
    Run {
        #[command(flatten)]
        room: RoomArgs,
        #[arg(long, default_value_t = 60)]
        duration_secs: u64,
        #[arg(long)]
        imitate_from: Option<PathBuf>,
        #[arg(long, default_value_t = 0)]
        net_in_delay_ms: u64,
        #[arg(long, default_value_t = 0)]
        net_out_delay_ms: u64,
        #[arg(long, default_value_t = 0)]
        net_jitter_ms: u64,
        #[arg(long)]
        seed: Option<u64>,
        #[arg(long, default_value_t = 384)]
        think_min_ms: u64,
        #[arg(long, default_value_t = 1901)]
        think_max_ms: u64,
        #[arg(long, default_value_t = 220)]
        drag_min_ms: u64,
        #[arg(long, default_value_t = 1500)]
        drag_max_ms: u64,
        #[arg(long, default_value_t = 5)]
        tick_min_ms: u64,
        #[arg(long, default_value_t = 21)]
        tick_max_ms: u64,
        #[arg(long, default_value_t = 0.0)]
        transform_rate: f32,
        #[arg(long, default_value_t = 0.005)]
        conflict_rate: f32,
        #[arg(long, default_value_t = 1.0)]
        jitter_px: f32,
        #[arg(long, default_value_t = 60.0)]
        distance_min_px: f32,
        #[arg(long, default_value_t = 260.0)]
        distance_max_px: f32,
    },
}

pub(super) async fn run(command: BotCommand) -> Result<(), Box<dyn std::error::Error>> {
    match command {
        BotCommand::Run {
            room,
            duration_secs,
            imitate_from,
            net_in_delay_ms,
            net_out_delay_ms,
            net_jitter_ms,
            seed,
            think_min_ms,
            think_max_ms,
            drag_min_ms,
            drag_max_ms,
            tick_min_ms,
            tick_max_ms,
            transform_rate,
            conflict_rate,
            jitter_px,
            distance_min_px,
            distance_max_px,
        } => {
            let config = BotRunConfig {
                duration_secs,
                think_min_ms,
                think_max_ms,
                drag_min_ms,
                drag_max_ms,
                tick_min_ms,
                tick_max_ms,
                transform_rate,
                conflict_rate,
                jitter_px,
                distance_min_px,
                distance_max_px,
            };
            let net = BotNetConfig {
                inbound_ms: net_in_delay_ms,
                outbound_ms: net_out_delay_ms,
                jitter_ms: net_jitter_ms,
            };
            run_bot(room, config, net, seed, imitate_from).await
        }
    }
}

#[derive(Debug)]
struct BotState {
    client_id: Option<ClientId>,
    snapshot: Option<GameSnapshot>,
    ownership_by_anchor: HashMap<u32, ClientId>,
    last_seq: u64,
}

impl BotState {
    fn new() -> Self {
        Self {
            client_id: None,
            snapshot: None,
            ownership_by_anchor: HashMap::new(),
            last_seq: 0,
        }
    }

    fn apply_server_msg(&mut self, msg: &ServerMsg) -> Result<(), Box<dyn std::error::Error>> {
        match msg {
            ServerMsg::Welcome { client_id, .. } => {
                self.client_id = *client_id;
            }
            ServerMsg::State { seq, snapshot } => {
                self.last_seq = *seq;
                self.snapshot = Some(snapshot.clone());
                self.ownership_by_anchor.clear();
            }
            ServerMsg::Update { seq, update, .. } => {
                if *seq <= self.last_seq {
                    return Ok(());
                }
                self.last_seq = *seq;
                match update {
                    RoomUpdate::Ownership {
                        anchor_id,
                        owner,
                        reason: _,
                    } => {
                        if let Some(owner_id) = owner {
                            self.ownership_by_anchor.insert(*anchor_id, *owner_id);
                        } else {
                            self.ownership_by_anchor.remove(anchor_id);
                        }
                    }
                    _ => {
                        if let Some(snapshot) = self.snapshot.as_mut() {
                            let cols = snapshot.puzzle.cols as usize;
                            let rows = snapshot.puzzle.rows as usize;
                            if cols > 0 && rows > 0 {
                                let piece_width = snapshot.puzzle.image_width as f32 / cols as f32;
                                let piece_height =
                                    snapshot.puzzle.image_height as f32 / rows as f32;
                                let _ = apply_room_update_to_snapshot(
                                    update,
                                    &mut snapshot.state,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                );
                                snapshot.seq = *seq;
                            }
                        }
                    }
                }
            }
            ServerMsg::Error { code, message } => {
                return Err(err_msg(format!("server error {code}: {message}")));
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct BotRunConfig {
    duration_secs: u64,
    think_min_ms: u64,
    think_max_ms: u64,
    drag_min_ms: u64,
    drag_max_ms: u64,
    tick_min_ms: u64,
    tick_max_ms: u64,
    transform_rate: f32,
    conflict_rate: f32,
    jitter_px: f32,
    distance_min_px: f32,
    distance_max_px: f32,
}

#[derive(Clone, Copy)]
struct BotNetConfig {
    inbound_ms: u64,
    outbound_ms: u64,
    jitter_ms: u64,
}

#[derive(Clone, Copy, Debug)]
struct BotGeometry {
    cols: usize,
    rows: usize,
    piece_width: f32,
    piece_height: f32,
    center_min_x: f32,
    center_max_x: f32,
    center_min_y: f32,
    center_max_y: f32,
    snap_distance: f32,
    rotation_snap_tolerance: f32,
    rotation_enabled: bool,
}

#[derive(Clone, Debug)]
struct SolveCandidate {
    anchor_id: u32,
    members: Vec<usize>,
    target_pos: (f32, f32),
    target_rot: f32,
    border_priority: bool,
    score: f32,
}

#[derive(Clone, Copy, Debug)]
struct DragPlan {
    anchor_id: u32,
    target_pos: (f32, f32),
    target_rot: f32,
    solver_directed: bool,
}

#[derive(Clone, Copy, Debug)]
enum BotActionPlan {
    Drag(DragPlan),
    Flip { piece_id: u32 },
}
#[derive(Clone, Debug)]
struct ImitationProfile {
    think_min_ms: u64,
    think_max_ms: u64,
    tick_min_ms: u64,
    tick_max_ms: u64,
    transform_rate: f32,
    conflict_rate: f32,
    long_updates_per_drag_early: f32,
    long_updates_per_drag_late: f32,
    speed_px_per_sec_early: f32,
    speed_px_per_sec_late: f32,
    short_drag_rate_early: f32,
    short_drag_rate_late: f32,
    same_piece_retry_early: f32,
    same_piece_retry_late: f32,
}

impl ImitationProfile {
    fn long_updates_per_drag_for_progress(&self, progress: f32) -> f32 {
        lerp_f32(
            self.long_updates_per_drag_early,
            self.long_updates_per_drag_late,
            progress.clamp(0.0, 1.0),
        )
    }

    fn speed_for_progress(&self, progress: f32) -> f32 {
        lerp_f32(
            self.speed_px_per_sec_early,
            self.speed_px_per_sec_late,
            progress.clamp(0.0, 1.0),
        )
    }

    fn short_drag_rate_for_progress(&self, progress: f32) -> f32 {
        lerp_f32(
            self.short_drag_rate_early,
            self.short_drag_rate_late,
            progress.clamp(0.0, 1.0),
        )
    }

    fn target_snap_like_for_progress(&self, progress: f32) -> f32 {
        let retry = lerp_f32(
            self.same_piece_retry_early,
            self.same_piece_retry_late,
            progress.clamp(0.0, 1.0),
        );
        (1.0 - retry).clamp(0.05, 0.95)
    }
}

fn default_imitation_profile() -> ImitationProfile {
    ImitationProfile {
        think_min_ms: 384,
        think_max_ms: 1901,
        tick_min_ms: 5,
        tick_max_ms: 21,
        transform_rate: 0.0,
        conflict_rate: 0.005,
        long_updates_per_drag_early: 48.0,
        long_updates_per_drag_late: 73.0,
        speed_px_per_sec_early: 620.0,
        speed_px_per_sec_late: 520.0,
        short_drag_rate_early: 0.50,
        short_drag_rate_late: 0.57,
        same_piece_retry_early: 0.60,
        same_piece_retry_late: 0.74,
    }
}

#[derive(Clone, Debug)]
struct PartialDrag {
    start_ts: i64,
    piece_id: u32,
    first_update_ts: Option<i64>,
    last_update_ts: Option<i64>,
    update_count: u32,
    first_pos: Option<(f32, f32)>,
    last_pos: Option<(f32, f32)>,
    same_piece_retry: bool,
}
async fn run_bot(
    room: RoomArgs,
    config: BotRunConfig,
    net: BotNetConfig,
    seed: Option<u64>,
    imitate_from: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    validate_bot_config(config)?;
    let imitation_profile = if let Some(path) = imitate_from.as_ref() {
        let profile = build_imitation_profile(path)?;
        eprintln!(
            "imitation_profile(file): think={}..{}ms tick={}..{}ms transform_rate={:.3} conflict_rate={:.3} long_updates/drag early={:.2} late={:.2} speed early={:.1}px/s late={:.1}px/s short_drag early={:.2} late={:.2} snap_like early={:.2} late={:.2}",
            profile.think_min_ms,
            profile.think_max_ms,
            profile.tick_min_ms,
            profile.tick_max_ms,
            profile.transform_rate,
            profile.conflict_rate,
            profile.long_updates_per_drag_early,
            profile.long_updates_per_drag_late,
            profile.speed_px_per_sec_early,
            profile.speed_px_per_sec_late,
            profile.short_drag_rate_early,
            profile.short_drag_rate_late,
            1.0 - profile.same_piece_retry_early,
            1.0 - profile.same_piece_retry_late
        );
        profile
    } else {
        let profile = default_imitation_profile();
        eprintln!(
            "imitation_profile(default): think={}..{}ms tick={}..{}ms transform_rate={:.3} conflict_rate={:.3} long_updates/drag early={:.2} late={:.2} speed early={:.1}px/s late={:.1}px/s short_drag early={:.2} late={:.2} snap_like early={:.2} late={:.2}",
            profile.think_min_ms,
            profile.think_max_ms,
            profile.tick_min_ms,
            profile.tick_max_ms,
            profile.transform_rate,
            profile.conflict_rate,
            profile.long_updates_per_drag_early,
            profile.long_updates_per_drag_late,
            profile.speed_px_per_sec_early,
            profile.speed_px_per_sec_late,
            profile.short_drag_rate_early,
            profile.short_drag_rate_late,
            1.0 - profile.same_piece_retry_early,
            1.0 - profile.same_piece_retry_late
        );
        profile
    };

    let mut rng = {
        let base_seed = seed.unwrap_or_else(|| rand::rng().random());
        StdRng::seed_from_u64(base_seed)
    };

    let (mut write, mut read) = connect_ws(&room.base_url, &room.room_id, None).await?;
    let mut state = BotState::new();
    let mut initialized = false;
    let init_wait_deadline = Instant::now() + StdDuration::from_secs(15);

    while Instant::now() < init_wait_deadline {
        let remaining = init_wait_deadline.saturating_duration_since(Instant::now());
        let msg = recv_server_msg_timeout_with_delay(
            &mut read,
            Duration::from_millis(remaining.as_millis() as u64),
            net,
            &mut rng,
        )
        .await?;
        let Some(msg) = msg else {
            continue;
        };
        if let ServerMsg::NeedInit = msg {
            return Err(err_msg(
                "room is not initialized; initialize once with a browser client or admin init flow",
            ));
        }
        if let ServerMsg::Welcome {
            initialized: ok, ..
        } = msg
        {
            if !ok {
                return Err(err_msg(
                    "room is not initialized; bot runner requires an initialized room",
                ));
            }
        }
        state.apply_server_msg(&msg)?;
        if state.client_id.is_some() && state.snapshot.is_some() {
            initialized = true;
            break;
        }
    }

    if !initialized {
        return Err(err_msg("failed to receive welcome+state before timeout"));
    }

    let end_at = Instant::now() + StdDuration::from_secs(config.duration_secs);
    let mut next_client_seq: u64 = 0;
    let mut snap_like_ewma = 0.5f32;
    let mut last_anchor_attempt: Option<u32> = None;
    let mut last_drag_snapped = false;
    let mut anchor_action_counts = HashMap::<u32, u32>::new();
    let mut touched_anchor_ids = HashSet::<u32>::new();

    while Instant::now() < end_at {
        pump_messages(
            &mut read,
            &mut state,
            Duration::from_millis(120),
            net,
            &mut rng,
        )
        .await?;
        let Some(snapshot) = state.snapshot.as_ref() else {
            continue;
        };
        if snapshot_is_complete(snapshot) {
            eprintln!("bot run finished early: puzzle is complete");
            break;
        }
        let think_bounds = (config.think_min_ms, config.think_max_ms);
        let think_ms = sample_low_biased_u64(&mut rng, think_bounds.0, think_bounds.1);
        sleep(Duration::from_millis(think_ms)).await;
        pump_messages(
            &mut read,
            &mut state,
            Duration::from_millis(20),
            net,
            &mut rng,
        )
        .await?;

        let Some(snapshot) = state.snapshot.as_ref() else {
            continue;
        };
        if snapshot_is_complete(snapshot) {
            eprintln!("bot run finished early: puzzle is complete");
            break;
        }
        let completion = completion_ratio(snapshot);
        let elapsed_ratio = if config.duration_secs == 0 {
            0.0
        } else {
            let remaining = end_at
                .saturating_duration_since(Instant::now())
                .as_secs_f32();
            (1.0 - remaining / config.duration_secs as f32).clamp(0.0, 1.0)
        };
        let target_snap_like = imitation_profile.target_snap_like_for_progress(completion);
        let snap_error = (target_snap_like - snap_like_ewma).clamp(-0.6, 0.6);
        let effective_conflict_rate = config.conflict_rate.clamp(0.0, 1.0);
        let retry_like = lerp_f32(
            imitation_profile.same_piece_retry_early,
            imitation_profile.same_piece_retry_late,
            completion.clamp(0.0, 1.0),
        )
        .clamp(0.05, 0.98);
        let anchors = candidate_anchors(snapshot);
        if anchors.is_empty() {
            continue;
        }

        let planned_action = choose_solver_action(
            snapshot,
            &state.ownership_by_anchor,
            state.client_id,
            effective_conflict_rate,
            completion,
            snap_error,
            last_anchor_attempt,
            &anchor_action_counts,
            &imitation_profile,
            &mut rng,
        );
        let coverage_anchor = choose_coverage_anchor(
            snapshot,
            &anchors,
            &touched_anchor_ids,
            &state.ownership_by_anchor,
            state.client_id,
            effective_conflict_rate,
            completion,
            elapsed_ratio,
            &mut rng,
        );

        if let Some(BotActionPlan::Flip { piece_id }) = planned_action {
            send_client_msg_with_delay(&mut write, ClientMsg::Select { piece_id }, net, &mut rng)
                .await?;
            send_client_msg_with_delay(
                &mut write,
                ClientMsg::Flip {
                    piece_id,
                    flipped: false,
                },
                net,
                &mut rng,
            )
            .await?;
            pump_messages(
                &mut read,
                &mut state,
                Duration::from_millis(30),
                net,
                &mut rng,
            )
            .await?;
            continue;
        }

        let mut planned_target: Option<((f32, f32), f32)> = None;
        let mut solver_directed = false;
        let mut anchor_id = if let Some(anchor) = coverage_anchor {
            anchor
        } else if let Some(BotActionPlan::Drag(plan)) = planned_action {
            planned_target = Some((plan.target_pos, plan.target_rot));
            solver_directed = plan.solver_directed;
            plan.anchor_id
        } else {
            choose_anchor(
                &anchors,
                &state.ownership_by_anchor,
                state.client_id,
                effective_conflict_rate,
                &mut rng,
            )
        };

        let force_retry = last_anchor_attempt
            .map(|prev| {
                let idx = prev as usize;
                if idx >= snapshot.state.positions.len() {
                    return false;
                }
                if let Some(owner) = state.ownership_by_anchor.get(&prev) {
                    if Some(*owner) != state.client_id
                        && rng.random::<f32>() >= effective_conflict_rate
                    {
                        return false;
                    }
                }
                let retry_target = if last_drag_snapped {
                    (retry_like * 0.38 + 0.04).clamp(0.08, 0.48)
                } else {
                    (retry_like * 0.48 + 0.10).clamp(0.14, 0.62)
                };
                rng.random::<f32>() < retry_target
            })
            .unwrap_or(false);
        if force_retry {
            if let Some(prev) = last_anchor_attempt {
                anchor_id = prev;
                planned_target = None;
                solver_directed = false;
            }
        }

        let anchor_idx = anchor_id as usize;
        if anchor_idx >= snapshot.state.positions.len()
            || anchor_idx >= snapshot.state.rotations.len()
        {
            continue;
        }

        let start_pos = snapshot.state.positions[anchor_idx];
        let start_rot = snapshot.state.rotations[anchor_idx];

        send_client_msg_with_delay(
            &mut write,
            ClientMsg::Select {
                piece_id: anchor_id,
            },
            net,
            &mut rng,
        )
        .await?;

        let (tick_lo, tick_hi) = (config.tick_min_ms, config.tick_max_ms);
        let tick_ms = sample_low_biased_u64(&mut rng, tick_lo, tick_hi).max(1);

        let short_drag_rate = (imitation_profile.short_drag_rate_for_progress(completion)
            - 0.35 * snap_error)
            .clamp(0.05, 0.95);
        let short_drag = rng.random::<f32>() < short_drag_rate;
        let mut desired_updates = if short_drag {
            rng.random_range(1.0f32..=2.5f32)
        } else {
            let mut value = imitation_profile.long_updates_per_drag_for_progress(completion);
            let jitter = value * 0.2;
            value += rng.random_range(-jitter..=jitter);
            value.max(3.0)
        };
        let solver_cap = if solver_directed { 72.0 } else { 56.0 };
        desired_updates = desired_updates.clamp(1.0, solver_cap);
        let base_steps = desired_updates.round().max(1.0) as u64;
        let drag_min_ms = if short_drag {
            (tick_ms.saturating_mul(14)).max(70)
        } else {
            config.drag_min_ms
        };
        let mut drag_ms = (base_steps * tick_ms).clamp(drag_min_ms, config.drag_max_ms);

        let mut speed = imitation_profile.speed_for_progress(completion);
        speed *= if snap_error > 0.0 {
            1.0 - 0.40 * snap_error
        } else {
            1.0 - 0.10 * snap_error
        };
        if short_drag {
            speed *= 0.85;
        }
        speed = speed.clamp(40.0, 1600.0);
        let min_distance = config.distance_min_px.max(8.0);
        let max_distance = config.distance_max_px.max(min_distance + 1.0);

        let mut target_pos;
        let mut target_rot;
        if let Some((planned_pos, planned_rot)) = planned_target {
            target_pos = planned_pos;
            target_rot = planned_rot;
            let distance = distance_2d(start_pos, target_pos);
            if distance > 1.0e-4 {
                let desired_distance = if solver_directed {
                    let solver_max_distance =
                        (max_distance * 2.5).clamp(max_distance + 1.0, 1600.0);
                    distance.clamp(min_distance, solver_max_distance)
                } else {
                    distance.clamp(min_distance, max_distance)
                };
                let scale = desired_distance / distance;
                target_pos = (
                    start_pos.0 + (target_pos.0 - start_pos.0) * scale,
                    start_pos.1 + (target_pos.1 - start_pos.1) * scale,
                );
            } else {
                let angle = rng.random_range(0.0f32..std::f32::consts::TAU);
                target_pos = (
                    start_pos.0 + min_distance * angle.cos(),
                    start_pos.1 + min_distance * angle.sin(),
                );
            }
        } else {
            let mut distance = speed * (drag_ms as f32 / 1000.0);
            if short_drag {
                distance *= rng.random_range(0.35f32..=0.95f32);
            } else {
                distance *= rng.random_range(0.75f32..=1.25f32);
            }
            let distance = distance.clamp(min_distance, max_distance);
            let angle = rng.random_range(0.0f32..std::f32::consts::TAU);
            target_pos = (
                start_pos.0 + distance * angle.cos(),
                start_pos.1 + distance * angle.sin(),
            );
            target_rot = start_rot;
        }
        drag_ms = drag_ms.clamp(drag_min_ms, config.drag_max_ms);
        let steps = (drag_ms / tick_ms).max(1);

        let geometry = bot_geometry_for_snapshot(snapshot);
        let rotation_enabled = geometry
            .as_ref()
            .map(|g| g.rotation_enabled)
            .unwrap_or(false);
        let mut effective_transform_rate = config.transform_rate.clamp(0.0, 1.0);
        if rotation_enabled && effective_transform_rate <= f32::EPSILON {
            effective_transform_rate = 0.40;
        }
        let rotation_tolerance = geometry
            .as_ref()
            .map(|g| g.rotation_snap_tolerance.max(0.5))
            .unwrap_or(5.0);
        let mut rot_delta = angle_delta(target_rot, start_rot);
        let rotate_drag = if rotation_enabled {
            if rot_delta.abs() <= rotation_tolerance * 0.35 {
                false
            } else {
                let required_rate = if solver_directed {
                    effective_transform_rate.max(0.60)
                } else {
                    effective_transform_rate
                };
                rng.random::<f32>() < required_rate
            }
        } else {
            false
        };
        if !rotate_drag {
            rot_delta = 0.0;
            if !rotation_enabled {
                target_rot = start_rot;
            }
        } else {
            target_rot = normalize_angle(start_rot + rot_delta);
        }

        let jitter_base = config.jitter_px
            * (1.0 - 0.50 * completion)
            * (1.0 - 0.35 * snap_error.max(0.0)).clamp(0.15, 1.2);
        let jitter_base = if solver_directed {
            (jitter_base * 0.35).max(0.0)
        } else {
            jitter_base.max(0.0)
        };

        let mut final_pos = start_pos;
        let mut final_rot = start_rot;
        let before_groups = group_count(snapshot);
        for step in 1..=steps {
            let t = step as f32 / steps as f32;
            let eased = t * t * (3.0 - 2.0 * t);
            let jitter_scale = (1.0 - t) * jitter_base;
            let jitter_x = if jitter_base <= 0.0 {
                0.0
            } else {
                rng.random_range(-jitter_scale..=jitter_scale)
            };
            let jitter_y = if jitter_base <= 0.0 {
                0.0
            } else {
                rng.random_range(-jitter_scale..=jitter_scale)
            };
            let next_pos = (
                start_pos.0 + (target_pos.0 - start_pos.0) * eased + jitter_x,
                start_pos.1 + (target_pos.1 - start_pos.1) * eased + jitter_y,
            );
            let next_rot = normalize_angle(start_rot + rot_delta * eased);

            next_client_seq = next_client_seq.saturating_add(1);
            if rotate_drag {
                send_client_msg_with_delay(
                    &mut write,
                    ClientMsg::Transform {
                        anchor_id,
                        pos: next_pos,
                        rot_deg: next_rot,
                        client_seq: next_client_seq,
                    },
                    net,
                    &mut rng,
                )
                .await?;
            } else {
                send_client_msg_with_delay(
                    &mut write,
                    ClientMsg::Move {
                        anchor_id,
                        pos: next_pos,
                        client_seq: next_client_seq,
                    },
                    net,
                    &mut rng,
                )
                .await?;
            }

            final_pos = next_pos;
            final_rot = if rotate_drag { next_rot } else { start_rot };

            sleep(Duration::from_millis(tick_ms)).await;
            pump_messages(
                &mut read,
                &mut state,
                Duration::from_millis(1),
                net,
                &mut rng,
            )
            .await?;
        }

        if rotate_drag {
            final_rot = target_rot;
        }

        send_client_msg_with_delay(
            &mut write,
            ClientMsg::Place {
                anchor_id,
                pos: final_pos,
                rot_deg: final_rot,
            },
            net,
            &mut rng,
        )
        .await?;
        last_anchor_attempt = Some(anchor_id);
        touched_anchor_ids.insert(anchor_id);
        let entry = anchor_action_counts.entry(anchor_id).or_insert(0);
        *entry = entry.saturating_add(1);

        pump_messages(
            &mut read,
            &mut state,
            Duration::from_millis(20),
            net,
            &mut rng,
        )
        .await?;
        if let Some(after_snapshot) = state.snapshot.as_ref() {
            let after_groups = group_count(after_snapshot);
            let snap_like = if after_groups < before_groups {
                1.0
            } else {
                0.0
            };
            snap_like_ewma = 0.88 * snap_like_ewma + 0.12 * snap_like;
            last_drag_snapped = snap_like > 0.5;
            if snapshot_is_complete(after_snapshot) {
                eprintln!("bot run finished early: puzzle is complete");
                break;
            }
        }
    }

    let _ = write.send(Message::Close(None)).await;
    Ok(())
}

fn validate_bot_config(config: BotRunConfig) -> Result<(), Box<dyn std::error::Error>> {
    if config.think_min_ms == 0 || config.think_max_ms < config.think_min_ms {
        return Err(err_msg("invalid think-time range"));
    }
    if config.drag_min_ms == 0 || config.drag_max_ms < config.drag_min_ms {
        return Err(err_msg("invalid drag-duration range"));
    }
    if config.tick_min_ms == 0 || config.tick_max_ms < config.tick_min_ms {
        return Err(err_msg("invalid move-tick range"));
    }
    if !(0.0..=1.0).contains(&config.transform_rate) {
        return Err(err_msg("transform-rate must be between 0 and 1"));
    }
    if !(0.0..=1.0).contains(&config.conflict_rate) {
        return Err(err_msg("conflict-rate must be between 0 and 1"));
    }
    if config.distance_min_px <= 0.0 || config.distance_max_px < config.distance_min_px {
        return Err(err_msg("invalid distance range"));
    }
    Ok(())
}

fn candidate_anchors(snapshot: &GameSnapshot) -> Vec<u32> {
    let total = (snapshot.puzzle.cols as usize) * (snapshot.puzzle.rows as usize);
    if total == 0 {
        return Vec::new();
    }
    let mut anchors = snapshot
        .state
        .group_order
        .iter()
        .copied()
        .filter(|id| (*id as usize) < total)
        .collect::<Vec<_>>();
    if anchors.is_empty() {
        anchors = (0..total as u32).collect::<Vec<_>>();
    }
    anchors
}

fn choose_coverage_anchor(
    snapshot: &GameSnapshot,
    anchors: &[u32],
    touched_anchor_ids: &HashSet<u32>,
    ownership_by_anchor: &HashMap<u32, ClientId>,
    self_id: Option<ClientId>,
    conflict_rate: f32,
    completion: f32,
    elapsed_ratio: f32,
    rng: &mut StdRng,
) -> Option<u32> {
    let cols = snapshot.puzzle.cols as usize;
    let rows = snapshot.puzzle.rows as usize;
    if cols == 0 || rows == 0 || anchors.is_empty() {
        return None;
    }
    let mut border_candidates = Vec::new();
    let mut untouched = Vec::new();
    for anchor in anchors.iter().copied() {
        let owned_by_other = ownership_by_anchor
            .get(&anchor)
            .map(|owner| Some(*owner) != self_id)
            .unwrap_or(false);
        if owned_by_other && rng.random::<f32>() >= conflict_rate {
            continue;
        }
        if touched_anchor_ids.contains(&anchor) {
            continue;
        }
        untouched.push(anchor);
        if is_border_piece(anchor as usize, cols, rows) {
            border_candidates.push(anchor);
        }
    }
    if !border_candidates.is_empty() {
        let total_border = border_piece_ids(cols, rows).len().max(1) as f32;
        let touched_border = touched_anchor_ids
            .iter()
            .copied()
            .filter(|anchor| is_border_piece(*anchor as usize, cols, rows))
            .count() as f32;
        let observed_border_ratio = (touched_border / total_border).clamp(0.0, 1.0);
        let target_border_ratio = (elapsed_ratio / 0.55).clamp(0.0, 1.0);
        let border_deficit = (target_border_ratio - observed_border_ratio).max(0.0);
        let mut pressure =
            (0.16 + border_deficit * 0.70 + (1.0 - completion.clamp(0.0, 1.0)) * 0.08)
                .clamp(0.08, 0.95);
        if elapsed_ratio >= 0.60 && observed_border_ratio < 1.0 {
            pressure = pressure.max(0.85);
        }
        if rng.random::<f32>() < pressure {
            let idx = rng.random_range(0..border_candidates.len());
            return Some(border_candidates[idx]);
        }
    }
    if !untouched.is_empty() {
        let pressure = lerp_f32(0.12, 0.02, completion.clamp(0.0, 1.0));
        if rng.random::<f32>() < pressure {
            let idx = rng.random_range(0..untouched.len());
            return Some(untouched[idx]);
        }
    }

    None
}

fn choose_anchor(
    anchors: &[u32],
    ownership_by_anchor: &HashMap<u32, ClientId>,
    self_id: Option<ClientId>,
    conflict_rate: f32,
    rng: &mut StdRng,
) -> u32 {
    let mut owned_by_others = Vec::new();
    let mut available = Vec::new();

    for anchor in anchors {
        match ownership_by_anchor.get(anchor) {
            Some(owner) if Some(*owner) != self_id => owned_by_others.push(*anchor),
            _ => available.push(*anchor),
        }
    }

    if !owned_by_others.is_empty() && rng.random::<f32>() < conflict_rate {
        let idx = rng.random_range(0..owned_by_others.len());
        return owned_by_others[idx];
    }
    if !available.is_empty() {
        let idx = rng.random_range(0..available.len());
        return available[idx];
    }
    let idx = rng.random_range(0..anchors.len());
    anchors[idx]
}

fn group_count(snapshot: &GameSnapshot) -> usize {
    let cols = snapshot.puzzle.cols as usize;
    let rows = snapshot.puzzle.rows as usize;
    if cols == 0 || rows == 0 {
        return 0;
    }
    groups_from_connections(&snapshot.state.connections, cols, rows).len()
}

fn completion_ratio(snapshot: &GameSnapshot) -> f32 {
    let total = (snapshot.puzzle.cols as usize) * (snapshot.puzzle.rows as usize);
    if total <= 1 {
        return 1.0;
    }
    let groups = group_count(snapshot).max(1);
    let numerator = (groups.saturating_sub(1)) as f32;
    let denom = (total.saturating_sub(1)) as f32;
    (1.0 - (numerator / denom)).clamp(0.0, 1.0)
}

fn snapshot_is_complete(snapshot: &GameSnapshot) -> bool {
    let cols = snapshot.puzzle.cols as usize;
    let rows = snapshot.puzzle.rows as usize;
    if cols == 0 || rows == 0 {
        return false;
    }
    is_fully_connected(&snapshot.state.connections, cols, rows)
}

fn bot_geometry_for_snapshot(snapshot: &GameSnapshot) -> Option<BotGeometry> {
    let cols = snapshot.puzzle.cols as usize;
    let rows = snapshot.puzzle.rows as usize;
    if cols == 0 || rows == 0 {
        return None;
    }
    let image_width = snapshot.puzzle.image_width as f32;
    let image_height = snapshot.puzzle.image_height as f32;
    if image_width <= 0.0 || image_height <= 0.0 {
        return None;
    }
    let piece_width = image_width / cols as f32;
    let piece_height = image_height / rows as f32;
    let layout = compute_workspace_layout(
        image_width,
        image_height,
        snapshot.rules.workspace_padding_ratio,
    );
    let puzzle_scale = layout.puzzle_scale.max(1.0e-4);
    let puzzle_view_min_x = layout.view_min_x / puzzle_scale;
    let puzzle_view_min_y = layout.view_min_y / puzzle_scale;
    let puzzle_view_width = layout.view_width / puzzle_scale;
    let puzzle_view_height = layout.view_height / puzzle_scale;
    let center_min_x = puzzle_view_min_x + piece_width * 0.5;
    let center_min_y = puzzle_view_min_y + piece_height * 0.5;
    let mut center_max_x = puzzle_view_min_x + puzzle_view_width - piece_width * 0.5;
    let mut center_max_y = puzzle_view_min_y + puzzle_view_height - piece_height * 0.5;
    if center_max_x < center_min_x {
        center_max_x = center_min_x;
    }
    if center_max_y < center_min_y {
        center_max_y = center_min_y;
    }
    let snap_distance = piece_width.min(piece_height) * snapshot.rules.snap_distance_ratio;
    Some(BotGeometry {
        cols,
        rows,
        piece_width,
        piece_height,
        center_min_x,
        center_max_x,
        center_min_y,
        center_max_y,
        snap_distance,
        rotation_snap_tolerance: snapshot.rules.rotation_snap_tolerance_deg,
        rotation_enabled: snapshot.rules.rotation_enabled,
    })
}

fn dir_base_vector(dir: usize, piece_width: f32, piece_height: f32) -> (f32, f32) {
    match dir {
        DIR_LEFT => (-piece_width, 0.0),
        DIR_RIGHT => (piece_width, 0.0),
        DIR_UP => (0.0, -piece_height),
        DIR_DOWN => (0.0, piece_height),
        _ => (0.0, 0.0),
    }
}

fn distance_2d(a: (f32, f32), b: (f32, f32)) -> f32 {
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    (dx * dx + dy * dy).sqrt()
}

fn is_border_piece(piece_id: usize, cols: usize, rows: usize) -> bool {
    if cols == 0 || rows == 0 {
        return false;
    }
    let row = piece_id / cols;
    let col = piece_id % cols;
    row == 0 || col == 0 || row + 1 == rows || col + 1 == cols
}

fn group_target_in_bounds(
    members: &[usize],
    anchor: usize,
    target_anchor_pos: (f32, f32),
    target_rot: f32,
    geometry: &BotGeometry,
) -> bool {
    for member in members {
        let (dx, dy) = piece_local_offset(
            *member,
            anchor,
            geometry.cols,
            geometry.piece_width,
            geometry.piece_height,
        );
        let (rx, ry) = rotate_vec(dx, dy, target_rot);
        let pos = (target_anchor_pos.0 + rx, target_anchor_pos.1 + ry);
        let center_x = pos.0 + geometry.piece_width * 0.5;
        let center_y = pos.1 + geometry.piece_height * 0.5;
        if center_x < geometry.center_min_x
            || center_x > geometry.center_max_x
            || center_y < geometry.center_min_y
            || center_y > geometry.center_max_y
        {
            return false;
        }
    }
    true
}

fn choose_solver_action(
    snapshot: &GameSnapshot,
    ownership_by_anchor: &HashMap<u32, ClientId>,
    self_id: Option<ClientId>,
    conflict_rate: f32,
    completion: f32,
    snap_error: f32,
    last_anchor: Option<u32>,
    anchor_action_counts: &HashMap<u32, u32>,
    imitation_profile: &ImitationProfile,
    rng: &mut StdRng,
) -> Option<BotActionPlan> {
    let geometry = bot_geometry_for_snapshot(snapshot)?;
    let total = geometry.cols.saturating_mul(geometry.rows);
    if total == 0 {
        return None;
    }

    let mut anchor_of = (0..total).collect::<Vec<_>>();
    let mut group_members_by_anchor: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut group_size_by_anchor: HashMap<usize, usize> = HashMap::new();
    for mut group in
        groups_from_connections(&snapshot.state.connections, geometry.cols, geometry.rows)
    {
        if group.is_empty() {
            continue;
        }
        group.sort_unstable();
        let anchor = group[0];
        for id in &group {
            if *id < total {
                anchor_of[*id] = anchor;
            }
        }
        group_size_by_anchor.insert(anchor, group.len());
        group_members_by_anchor.insert(anchor, group);
    }

    let border_ids = border_piece_ids(geometry.cols, geometry.rows);
    let border_total = border_ids.len().max(1);
    let mut border_anchor_counts = HashMap::<usize, usize>::new();
    for border_id in border_ids {
        if border_id >= total {
            continue;
        }
        let anchor = *anchor_of.get(border_id).unwrap_or(&border_id);
        let entry = border_anchor_counts.entry(anchor).or_insert(0);
        *entry = entry.saturating_add(1);
    }
    let border_connected_ratio =
        border_anchor_counts.values().copied().max().unwrap_or(0) as f32 / border_total as f32;
    let border_backlog = (1.0 - border_connected_ratio).clamp(0.0, 1.0);

    let mut flipped_singletons = Vec::new();
    for piece_id in 0..total {
        if !snapshot.state.flips.get(piece_id).copied().unwrap_or(false) {
            continue;
        }
        if anchor_of[piece_id] != piece_id {
            continue;
        }
        let anchor_u32 = piece_id as u32;
        let owned_by_other = ownership_by_anchor
            .get(&anchor_u32)
            .map(|owner| Some(*owner) != self_id)
            .unwrap_or(false);
        if owned_by_other && rng.random::<f32>() >= conflict_rate {
            continue;
        }
        flipped_singletons.push(anchor_u32);
    }
    if !flipped_singletons.is_empty() {
        let flipped_ratio = flipped_singletons.len() as f32 / total as f32;
        let flip_pressure =
            (0.02 + flipped_ratio * 0.20 + completion * 0.10 + snap_error.max(0.0) * 0.08)
                .clamp(0.01, 0.25);
        if rng.random::<f32>() < flip_pressure {
            let idx = rng.random_range(0..flipped_singletons.len());
            return Some(BotActionPlan::Flip {
                piece_id: flipped_singletons[idx],
            });
        }
    }

    let mut candidates = Vec::<SolveCandidate>::new();
    for (anchor, members) in &group_members_by_anchor {
        let anchor_u32 = *anchor as u32;
        let owned_by_other = ownership_by_anchor
            .get(&anchor_u32)
            .map(|owner| Some(*owner) != self_id)
            .unwrap_or(false);
        if owned_by_other && rng.random::<f32>() >= conflict_rate {
            continue;
        }
        if members
            .iter()
            .any(|id| snapshot.state.flips.get(*id).copied().unwrap_or(false))
        {
            continue;
        }
        let member_set = members.iter().copied().collect::<HashSet<_>>();
        let start_pos = snapshot
            .state
            .positions
            .get(*anchor)
            .copied()
            .unwrap_or((0.0, 0.0));
        let start_rot = snapshot
            .state
            .rotations
            .get(*anchor)
            .copied()
            .unwrap_or(0.0);

        for member in members {
            for dir in [DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT] {
                let Some(neighbor) = neighbor_id(*member, geometry.cols, geometry.rows, dir) else {
                    continue;
                };
                if member_set.contains(&neighbor) {
                    continue;
                }
                if snapshot.state.flips.get(neighbor).copied().unwrap_or(false) {
                    continue;
                }
                let neighbor_anchor = *anchor_of.get(neighbor).unwrap_or(&neighbor);
                if neighbor_anchor == *anchor {
                    continue;
                }
                let neighbor_pos = snapshot
                    .state
                    .positions
                    .get(neighbor)
                    .copied()
                    .unwrap_or((0.0, 0.0));
                let neighbor_center = (
                    neighbor_pos.0 + geometry.piece_width * 0.5,
                    neighbor_pos.1 + geometry.piece_height * 0.5,
                );
                let target_rot = if geometry.rotation_enabled {
                    normalize_angle(
                        snapshot
                            .state
                            .rotations
                            .get(neighbor)
                            .copied()
                            .unwrap_or(0.0),
                    )
                } else {
                    0.0
                };
                let base = dir_base_vector(dir, geometry.piece_width, geometry.piece_height);
                let (vx, vy) = rotate_vec(base.0, base.1, target_rot);
                let member_target_center = (neighbor_center.0 - vx, neighbor_center.1 - vy);
                let member_target_pos = (
                    member_target_center.0 - geometry.piece_width * 0.5,
                    member_target_center.1 - geometry.piece_height * 0.5,
                );
                let (member_dx, member_dy) = piece_local_offset(
                    *member,
                    *anchor,
                    geometry.cols,
                    geometry.piece_width,
                    geometry.piece_height,
                );
                let (member_rx, member_ry) = rotate_vec(member_dx, member_dy, target_rot);
                let target_anchor_pos = (
                    member_target_pos.0 - member_rx,
                    member_target_pos.1 - member_ry,
                );
                if !group_target_in_bounds(
                    members,
                    *anchor,
                    target_anchor_pos,
                    target_rot,
                    &geometry,
                ) {
                    continue;
                }
                let neighbor_size = *group_size_by_anchor.get(&neighbor_anchor).unwrap_or(&1);
                let gain = (members.len() + neighbor_size) as f32;
                let distance = distance_2d(start_pos, target_anchor_pos);
                let rot_need = angle_delta(target_rot, start_rot).abs();
                let member_border = is_border_piece(*member, geometry.cols, geometry.rows);
                let neighbor_border = is_border_piece(neighbor, geometry.cols, geometry.rows);
                let border_bonus = if member_border && neighbor_border {
                    lerp_f32(180.0, 60.0, completion) * (1.0 + 1.2 * border_backlog)
                } else if member_border || neighbor_border {
                    lerp_f32(120.0, 40.0, completion) * (1.0 + 0.6 * border_backlog)
                } else {
                    0.0
                };
                let touch_count =
                    anchor_action_counts.get(&anchor_u32).copied().unwrap_or(0) as f32;
                let novelty_bonus = lerp_f32(30.0, 5.0, completion) / (1.0 + touch_count * 0.45);
                let repeat_penalty = if Some(anchor_u32) == last_anchor {
                    lerp_f32(130.0, 40.0, completion)
                } else {
                    0.0
                };
                let late_border_penalty = if is_border_piece(*anchor, geometry.cols, geometry.rows)
                {
                    lerp_f32(0.0, 20.0, completion)
                } else {
                    0.0
                };
                let interior_penalty = if !member_border && !neighbor_border {
                    lerp_f32(140.0, 25.0, completion) * border_backlog
                } else {
                    0.0
                };
                let score =
                    gain * 170.0 - distance * 0.45 - rot_need * 0.85 + border_bonus + novelty_bonus
                        - repeat_penalty
                        - late_border_penalty
                        - interior_penalty;
                candidates.push(SolveCandidate {
                    anchor_id: anchor_u32,
                    members: members.clone(),
                    target_pos: target_anchor_pos,
                    target_rot,
                    border_priority: member_border || neighbor_border,
                    score,
                });
            }
        }
    }

    if candidates.is_empty() {
        if !flipped_singletons.is_empty() {
            let idx = rng.random_range(0..flipped_singletons.len());
            return Some(BotActionPlan::Flip {
                piece_id: flipped_singletons[idx],
            });
        }
        return None;
    }

    candidates.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    let focus_probability =
        (0.40 + 0.28 * completion + 0.12 * snap_error.max(0.0)).clamp(0.12, 0.92);
    let top_n = candidates.len().min(8);
    let chosen_idx = if border_backlog > 0.08 && completion < 0.82 {
        let border_focus = (0.60 + 0.30 * border_backlog).clamp(0.50, 0.95);
        if rng.random::<f32>() < border_focus {
            let border_indices = (0..top_n)
                .filter(|idx| candidates[*idx].border_priority)
                .collect::<Vec<_>>();
            if !border_indices.is_empty() {
                let idx = rng.random_range(0..border_indices.len());
                border_indices[idx]
            } else if rng.random::<f32>() < focus_probability {
                0
            } else {
                rng.random_range(0..top_n)
            }
        } else if rng.random::<f32>() < focus_probability {
            0
        } else {
            rng.random_range(0..top_n)
        }
    } else if rng.random::<f32>() < focus_probability {
        0
    } else {
        rng.random_range(0..top_n)
    };
    let chosen = candidates.remove(chosen_idx);

    let retry_like = lerp_f32(
        imitation_profile.same_piece_retry_early,
        imitation_profile.same_piece_retry_late,
        completion.clamp(0.0, 1.0),
    )
    .clamp(0.0, 1.0);
    let base_accuracy = (0.90 - retry_like * 0.34 + completion * 0.20 + snap_error.max(0.0) * 0.18)
        .clamp(0.38, 0.93);
    let miss = rng.random::<f32>() > base_accuracy;
    let snap_distance = geometry.snap_distance.max(1.0);

    let mut target_pos = chosen.target_pos;
    let mut target_rot = chosen.target_rot;

    if miss {
        let miss_min = snap_distance * lerp_f32(1.6, 0.9, completion);
        let miss_max = (snap_distance * lerp_f32(3.4, 1.8, completion)).max(miss_min + 0.1);
        let radius = rng.random_range(miss_min..=miss_max);
        let theta = rng.random_range(0.0f32..std::f32::consts::TAU);
        target_pos.0 += radius * theta.cos();
        target_pos.1 += radius * theta.sin();
        if geometry.rotation_enabled && rng.random::<f32>() < 0.7 {
            let tol = geometry.rotation_snap_tolerance.max(0.5);
            let rot_miss_min = tol * lerp_f32(1.2, 1.05, completion);
            let rot_miss_max = (45.0 * (1.0 - completion) + tol * 2.5).max(rot_miss_min + 0.5);
            let offset = rng.random_range(rot_miss_min..=rot_miss_max);
            let sign = if rng.random::<bool>() { 1.0 } else { -1.0 };
            target_rot = normalize_angle(target_rot + sign * offset);
        }
    } else {
        let settle_radius = snap_distance * lerp_f32(0.38, 0.18, completion);
        if settle_radius > 0.0 {
            let radius = rng.random_range(0.0f32..=settle_radius);
            let theta = rng.random_range(0.0f32..std::f32::consts::TAU);
            target_pos.0 += radius * theta.cos();
            target_pos.1 += radius * theta.sin();
        }
        if geometry.rotation_enabled {
            let tol = geometry.rotation_snap_tolerance.max(0.5);
            let rot_noise = rng.random_range(-tol * 0.35..=tol * 0.35);
            target_rot = normalize_angle(target_rot + rot_noise);
        }
    }

    if !group_target_in_bounds(
        &chosen.members,
        chosen.anchor_id as usize,
        target_pos,
        target_rot,
        &geometry,
    ) {
        target_pos = chosen.target_pos;
        target_rot = chosen.target_rot;
    }

    Some(BotActionPlan::Drag(DragPlan {
        anchor_id: chosen.anchor_id,
        target_pos,
        target_rot,
        solver_directed: true,
    }))
}

fn lerp_f32(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t.clamp(0.0, 1.0)
}

fn build_imitation_profile(path: &PathBuf) -> Result<ImitationProfile, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut rows = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let row: RecordLine = serde_json::from_str(&line)
            .map_err(|err| err_msg(format!("invalid NDJSON row in imitation profile: {err}")))?;
        rows.push(row);
    }

    if rows.is_empty() {
        return Err(err_msg("imitation profile input is empty"));
    }

    rows.sort_by(|a, b| a.ts_ms.cmp(&b.ts_ms).then(a.id.cmp(&b.id)));
    let first_ts = rows.first().map(|row| row.ts_ms).unwrap_or(0);
    let last_ts = rows.last().map(|row| row.ts_ms).unwrap_or(first_ts);
    let span_ms = (last_ts - first_ts).max(1) as f32;

    let mut think_intervals = Vec::<f32>::new();
    let mut move_intervals = Vec::<f32>::new();
    let mut transform_count = 0u64;
    let mut update_total = 0u64;
    let mut select_count = 0u64;
    let mut select_conflicts = 0u64;
    let mut update_conflicts = 0u64;

    let mut active_by_client: HashMap<u64, PartialDrag> = HashMap::new();
    let mut last_select_ts_by_client: HashMap<u64, i64> = HashMap::new();
    let mut last_update_ts_by_client: HashMap<u64, i64> = HashMap::new();
    let mut last_completed_by_client: HashMap<u64, (usize, u32, i64)> = HashMap::new();
    let mut drags = Vec::<DragSample>::new();

    for row in &rows {
        match row.kind.as_str() {
            "Select" => {
                select_count = select_count.saturating_add(1);
                if row.outcome == "Ignored" || row.outcome == "Rejected" {
                    select_conflicts = select_conflicts.saturating_add(1);
                }
                if let Some(prev_ts) = last_select_ts_by_client.insert(row.client_id, row.ts_ms) {
                    if row.ts_ms > prev_ts {
                        think_intervals.push((row.ts_ms - prev_ts) as f32);
                    }
                }
                if let Some((idx, prev_piece, prev_end_ts)) =
                    last_completed_by_client.get(&row.client_id).copied()
                {
                    if prev_piece == row.piece_id.unwrap_or(u32::MAX)
                        && row.ts_ms.saturating_sub(prev_end_ts) <= 2_500
                    {
                        if let Some(drag) = drags.get_mut(idx) {
                            drag.same_piece_retry = true;
                        }
                    }
                }
                if let Some(piece_id) = row.piece_id {
                    active_by_client.insert(
                        row.client_id,
                        PartialDrag {
                            start_ts: row.ts_ms,
                            piece_id,
                            first_update_ts: None,
                            last_update_ts: None,
                            update_count: 0,
                            first_pos: None,
                            last_pos: None,
                            same_piece_retry: false,
                        },
                    );
                }
            }
            "Move" | "Transform" => {
                update_total = update_total.saturating_add(1);
                if row.kind == "Transform" {
                    transform_count = transform_count.saturating_add(1);
                }
                if row.outcome == "Ignored" || row.outcome == "Rejected" {
                    update_conflicts = update_conflicts.saturating_add(1);
                }
                if let Some(prev_ts) = last_update_ts_by_client.insert(row.client_id, row.ts_ms) {
                    if row.ts_ms > prev_ts {
                        move_intervals.push((row.ts_ms - prev_ts) as f32);
                    }
                }
                if let Some(active) = active_by_client.get_mut(&row.client_id) {
                    active.update_count = active.update_count.saturating_add(1);
                    if active.first_update_ts.is_none() {
                        active.first_update_ts = Some(row.ts_ms);
                    }
                    active.last_update_ts = Some(row.ts_ms);
                    if let Some(pos) = row.pos {
                        if active.first_pos.is_none() {
                            active.first_pos = Some(pos);
                        }
                        active.last_pos = Some(pos);
                    }
                }
            }
            "Place" | "Release" | "Flip" => {
                let Some(active) = active_by_client.remove(&row.client_id) else {
                    continue;
                };
                let avg_update_interval_ms = match (
                    active.first_update_ts,
                    active.last_update_ts,
                    active.update_count,
                ) {
                    (Some(first), Some(last), updates) if updates > 1 && last > first => {
                        Some((last - first) as f32 / (updates - 1) as f32)
                    }
                    _ => None,
                };
                let drag_duration_ms = match (active.first_update_ts, active.last_update_ts) {
                    (Some(first), Some(last)) if last > first => Some((last - first) as f32),
                    _ => None,
                };
                let distance_px = match (active.first_pos, active.last_pos.or(row.pos)) {
                    (Some(start), Some(end)) => {
                        Some(((end.0 - start.0).powi(2) + (end.1 - start.1).powi(2)).sqrt())
                    }
                    _ => None,
                };
                let sample = DragSample {
                    start_ts: active.start_ts,
                    updates: active.update_count,
                    avg_update_interval_ms,
                    drag_duration_ms,
                    distance_px,
                    path_distance_px: distance_px,
                    same_piece_retry: active.same_piece_retry,
                };
                drags.push(sample);
                let idx = drags.len().saturating_sub(1);
                last_completed_by_client.insert(row.client_id, (idx, active.piece_id, row.ts_ms));
            }
            _ => {}
        }
    }

    let think_min_ms = percentile_u64_from_f32(&think_intervals, 0.20)
        .unwrap_or(250)
        .max(20);
    let think_max_ms = percentile_u64_from_f32(&think_intervals, 0.85)
        .unwrap_or(1400)
        .max(think_min_ms);

    let mut update_intervals_for_tick = move_intervals.clone();
    if update_intervals_for_tick.is_empty() {
        update_intervals_for_tick = drags
            .iter()
            .filter_map(|drag| drag.avg_update_interval_ms)
            .collect::<Vec<_>>();
    }
    let tick_min_ms = percentile_u64_from_f32(&update_intervals_for_tick, 0.20)
        .unwrap_or(35)
        .clamp(5, 500);
    let tick_max_ms = percentile_u64_from_f32(&update_intervals_for_tick, 0.85)
        .unwrap_or(90)
        .max(tick_min_ms);

    let transform_rate = if update_total == 0 {
        0.0
    } else {
        transform_count as f32 / update_total as f32
    };
    let conflict_rate = if select_count == 0 {
        0.0
    } else {
        (select_conflicts as f32 / select_count as f32)
            .max((update_conflicts as f32 / update_total.max(1) as f32) * 0.25)
            .clamp(0.0, 0.95)
    };

    let mut early_drags = Vec::new();
    let mut late_drags = Vec::new();
    let mut all_drags = Vec::new();
    for drag in &drags {
        let progress = (drag.start_ts - first_ts) as f32 / span_ms;
        all_drags.push(drag.clone());
        if progress <= 0.30 {
            early_drags.push(drag.clone());
        } else if progress >= 0.70 {
            late_drags.push(drag.clone());
        }
    }
    if early_drags.is_empty() {
        early_drags = all_drags.clone();
    }
    if late_drags.is_empty() {
        late_drags = all_drags.clone();
    }

    let early_long_drags = early_drags
        .iter()
        .filter(|drag| drag.updates > 2)
        .cloned()
        .collect::<Vec<_>>();
    let late_long_drags = late_drags
        .iter()
        .filter(|drag| drag.updates > 2)
        .cloned()
        .collect::<Vec<_>>();
    let fallback_long_drags = all_drags
        .iter()
        .filter(|drag| drag.updates > 2)
        .cloned()
        .collect::<Vec<_>>();

    let early_long_for_stats = if early_long_drags.is_empty() {
        &fallback_long_drags
    } else {
        &early_long_drags
    };
    let late_long_for_stats = if late_long_drags.is_empty() {
        &fallback_long_drags
    } else {
        &late_long_drags
    };

    let long_updates_per_drag_early =
        mean_u32(early_long_for_stats.iter().map(|drag| drag.updates))
            .or_else(|| mean_u32(early_drags.iter().map(|drag| drag.updates)))
            .unwrap_or(48.0);
    let long_updates_per_drag_late = mean_u32(late_long_for_stats.iter().map(|drag| drag.updates))
        .or_else(|| mean_u32(late_drags.iter().map(|drag| drag.updates)))
        .unwrap_or(long_updates_per_drag_early);
    let speed_px_per_sec_early = mean_f32(
        early_long_for_stats
            .iter()
            .filter_map(|drag| drag_speed_px_per_sec(drag)),
    )
    .or_else(|| {
        mean_f32(
            early_drags
                .iter()
                .filter_map(|drag| drag_speed_px_per_sec(drag)),
        )
    })
    .unwrap_or(220.0);
    let speed_px_per_sec_late = mean_f32(
        late_long_for_stats
            .iter()
            .filter_map(|drag| drag_speed_px_per_sec(drag)),
    )
    .or_else(|| {
        mean_f32(
            late_drags
                .iter()
                .filter_map(|drag| drag_speed_px_per_sec(drag)),
        )
    })
    .unwrap_or((speed_px_per_sec_early * 0.85).max(80.0));
    let short_drag_rate_early = short_drag_rate(&early_drags).unwrap_or(0.50);
    let short_drag_rate_late = short_drag_rate(&late_drags).unwrap_or(short_drag_rate_early);
    let same_piece_retry_early =
        mean_bool(early_drags.iter().map(|drag| drag.same_piece_retry)).unwrap_or(0.35);
    let same_piece_retry_late = mean_bool(late_drags.iter().map(|drag| drag.same_piece_retry))
        .unwrap_or((same_piece_retry_early * 1.15).clamp(0.02, 0.95));

    Ok(ImitationProfile {
        think_min_ms,
        think_max_ms,
        tick_min_ms,
        tick_max_ms,
        transform_rate: transform_rate.clamp(0.0, 1.0),
        conflict_rate,
        long_updates_per_drag_early: long_updates_per_drag_early.max(3.0),
        long_updates_per_drag_late: long_updates_per_drag_late.max(3.0),
        speed_px_per_sec_early: speed_px_per_sec_early.clamp(30.0, 2500.0),
        speed_px_per_sec_late: speed_px_per_sec_late.clamp(20.0, 2500.0),
        short_drag_rate_early: short_drag_rate_early.clamp(0.02, 0.95),
        short_drag_rate_late: short_drag_rate_late.clamp(0.02, 0.95),
        same_piece_retry_early: same_piece_retry_early.clamp(0.0, 1.0),
        same_piece_retry_late: same_piece_retry_late.clamp(0.0, 1.0),
    })
}

fn percentile_u64_from_f32(values: &[f32], p: f32) -> Option<u64> {
    let mut values = values
        .iter()
        .copied()
        .filter(|value| value.is_finite() && *value >= 0.0)
        .collect::<Vec<_>>();
    if values.is_empty() {
        return None;
    }
    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let idx = ((values.len().saturating_sub(1)) as f32 * p.clamp(0.0, 1.0)).round() as usize;
    Some(values[idx].round().max(0.0) as u64)
}

fn mean_f32(values: impl Iterator<Item = f32>) -> Option<f32> {
    let mut total = 0.0f32;
    let mut count = 0u64;
    for value in values {
        total += value;
        count = count.saturating_add(1);
    }
    if count == 0 {
        None
    } else {
        Some(total / count as f32)
    }
}

fn mean_u32(values: impl Iterator<Item = u32>) -> Option<f32> {
    mean_f32(values.map(|value| value as f32))
}

fn mean_bool(values: impl Iterator<Item = bool>) -> Option<f32> {
    let mut total = 0u64;
    let mut count = 0u64;
    for value in values {
        total = total.saturating_add(if value { 1 } else { 0 });
        count = count.saturating_add(1);
    }
    if count == 0 {
        None
    } else {
        Some(total as f32 / count as f32)
    }
}

fn short_drag_rate(drags: &[DragSample]) -> Option<f32> {
    if drags.is_empty() {
        return None;
    }
    let short = drags.iter().filter(|drag| drag.updates <= 2).count();
    Some(short as f32 / drags.len() as f32)
}

fn drag_speed_px_per_sec(drag: &DragSample) -> Option<f32> {
    match (drag.distance_px, drag.drag_duration_ms) {
        (Some(distance), Some(duration_ms)) if duration_ms > 0.0 => {
            Some(distance * 1000.0 / duration_ms)
        }
        _ => None,
    }
}

fn sample_low_biased_u64(rng: &mut StdRng, min: u64, max: u64) -> u64 {
    if max <= min {
        return min;
    }
    let span = (max - min) as f32;
    let u = rng.random::<f32>();
    let shaped = u.powf(1.9);
    min.saturating_add((span * shaped).round() as u64)
}

fn sample_network_delay_ms(rng: &mut StdRng, base_ms: u64, jitter_ms: u64) -> u64 {
    if jitter_ms == 0 {
        return base_ms;
    }
    base_ms.saturating_add(rng.random_range(0..=jitter_ms))
}

async fn send_client_msg_with_delay(
    write: &mut WsWrite,
    msg: ClientMsg,
    net: BotNetConfig,
    rng: &mut StdRng,
) -> Result<(), Box<dyn std::error::Error>> {
    let delay_ms = sample_network_delay_ms(rng, net.outbound_ms, net.jitter_ms);
    if delay_ms > 0 {
        sleep(Duration::from_millis(delay_ms)).await;
    }
    send_client_msg(write, msg).await
}

async fn recv_server_msg_timeout_with_delay(
    read: &mut WsRead,
    dur: Duration,
    net: BotNetConfig,
    rng: &mut StdRng,
) -> Result<Option<ServerMsg>, Box<dyn std::error::Error>> {
    let msg = recv_server_msg_timeout(read, dur).await?;
    if msg.is_some() {
        let delay_ms = sample_network_delay_ms(rng, net.inbound_ms, net.jitter_ms);
        if delay_ms > 0 {
            sleep(Duration::from_millis(delay_ms)).await;
        }
    }
    Ok(msg)
}

async fn pump_messages(
    read: &mut WsRead,
    state: &mut BotState,
    window: Duration,
    net: BotNetConfig,
    rng: &mut StdRng,
) -> Result<(), Box<dyn std::error::Error>> {
    let deadline = Instant::now() + StdDuration::from_millis(window.as_millis() as u64);
    loop {
        if Instant::now() >= deadline {
            break;
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        let maybe = recv_server_msg_timeout_with_delay(
            read,
            Duration::from_millis(remaining.as_millis().min(60) as u64),
            net,
            rng,
        )
        .await?;
        let Some(msg) = maybe else {
            break;
        };
        state.apply_server_msg(&msg)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn choose_anchor_prefers_conflict_when_requested() {
        let anchors = vec![1, 2, 3];
        let mut ownership = HashMap::new();
        ownership.insert(2, ClientId::from(999));
        let mut rng = StdRng::seed_from_u64(42);
        let picked = choose_anchor(&anchors, &ownership, Some(ClientId::from(7)), 1.0, &mut rng);
        assert_eq!(picked, 2);
    }

    #[test]
    fn imitation_profile_extracts_basic_timing() {
        let path =
            std::env::temp_dir().join(format!("heddo-imitation-test-{}", std::process::id()));
        let mut file = File::create(&path).expect("create temp ndjson");
        let rows = [
            serde_json::json!({"id":1,"ts_ms":1000,"client_id":1,"kind":"Select","outcome":"Applied","piece_id":10,"anchor_id":null,"pos":null,"reason":null,"room_seq":1,"rot_deg":null,"client_seq":null}),
            serde_json::json!({"id":2,"ts_ms":1040,"client_id":1,"kind":"Move","outcome":"Applied","piece_id":null,"anchor_id":10,"pos":[0.0,0.0],"reason":null,"room_seq":2,"rot_deg":null,"client_seq":1}),
            serde_json::json!({"id":3,"ts_ms":1080,"client_id":1,"kind":"Move","outcome":"Applied","piece_id":null,"anchor_id":10,"pos":[40.0,0.0],"reason":null,"room_seq":3,"rot_deg":null,"client_seq":2}),
            serde_json::json!({"id":4,"ts_ms":1120,"client_id":1,"kind":"Place","outcome":"Applied","piece_id":null,"anchor_id":10,"pos":[45.0,0.0],"reason":null,"room_seq":4,"rot_deg":0.0,"client_seq":null}),
            serde_json::json!({"id":5,"ts_ms":1400,"client_id":1,"kind":"Select","outcome":"Applied","piece_id":10,"anchor_id":null,"pos":null,"reason":null,"room_seq":5,"rot_deg":null,"client_seq":null}),
            serde_json::json!({"id":6,"ts_ms":1435,"client_id":1,"kind":"Move","outcome":"Applied","piece_id":null,"anchor_id":10,"pos":[45.0,0.0],"reason":null,"room_seq":6,"rot_deg":null,"client_seq":3}),
            serde_json::json!({"id":7,"ts_ms":1470,"client_id":1,"kind":"Place","outcome":"Applied","piece_id":null,"anchor_id":10,"pos":[46.0,0.0],"reason":null,"room_seq":7,"rot_deg":0.0,"client_seq":null}),
        ];
        for row in rows {
            writeln!(file, "{}", row).expect("write ndjson row");
        }
        drop(file);

        let profile = build_imitation_profile(&path).expect("build imitation profile");
        assert!(profile.think_min_ms >= 1);
        assert!(profile.tick_min_ms >= 1);
        assert!(profile.long_updates_per_drag_early >= 1.0);
        assert!(profile.speed_px_per_sec_early >= profile.speed_px_per_sec_late);
        assert!(profile.short_drag_rate_early >= 0.0);
        assert!(profile.same_piece_retry_early >= 0.0);

        let _ = std::fs::remove_file(path);
    }
}
