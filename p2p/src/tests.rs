use super::*;
use heddobureika_core::{
    GameRules, LogicalState, PlayRules, PlayableState, PuzzleInfo, TopologySpec,
};

fn snapshot() -> PlayableGameSnapshot {
    let rules = GameRules::default();
    let entry = &heddobureika_core::PUZZLE_CATALOG[0];
    let (width, height) =
        heddobureika_core::logical_image_size(entry.width, entry.height, rules.image_max_dimension);
    let spec = TopologySpec::grid(2, 1);
    let topology = heddobureika_core::build_topology_from_spec(&spec).unwrap();
    let playable = PlayableState::new(LogicalState::new(topology), PlayRules::default());
    PlayableGameSnapshot::from_playable(
        PuzzleInfo {
            label: entry.label.into(),
            image_ref: PuzzleImageRef::BuiltIn {
                slug: entry.slug.into(),
            },
            topology: spec.into(),
            shape_seed: 0,
            image_width: width,
            image_height: height,
        },
        rules,
        0,
        &playable,
        None,
    )
}

#[test]
fn command_ids_and_claims_prevent_replay_and_old_drag_commands() {
    let mut seq = Sequence::default();
    let select = ClientMsg::Select { piece_id: 0 };
    assert!(seq.accept(1, 1, &select));
    seq.granted = 1;
    let movement = ClientMsg::Move {
        piece_id: 0,
        drop_pos: heddobureika_core::PlayablePositionSnapshot {
            x_mm: 1.0,
            y_mm: 0.5,
        },
        client_seq: 1,
        base_revision: 0,
    };
    assert!(seq.accept(2, 1, &movement));
    assert!(!seq.accept(2, 1, &movement));
    assert!(seq.accept(3, 3, &select));
    seq.granted = 3;
    assert!(!seq.accept(4, 1, &movement));
    assert!(seq.accept(5, 3, &movement));
    assert!(!Sequence::default().accept(6, 3, &movement));
}

#[test]
fn saved_progress_must_have_compatible_versions_and_matching_topology() {
    let valid = snapshot();
    assert!(validate_snapshot(&valid).is_ok());
    let mut invalid = valid.clone();
    invalid.version += 1;
    assert!(validate_snapshot(&invalid).is_err());
    invalid = valid.clone();
    invalid.puzzle.topology.payload.clear();
    assert!(validate_snapshot(&invalid).is_err());
    invalid = valid.clone();
    invalid.puzzle.topology = TopologySpec::grid(1, 2).into();
    assert!(validate_snapshot(&invalid).is_err());
    invalid = valid;
    invalid.puzzle.topology.tag = "unknown-topology".into();
    invalid.state.topology = invalid.puzzle.topology.clone();
    assert!(validate_snapshot(&invalid).is_err());
}

async fn reset(events: &Receiver<Event>) -> Vec<ServerMsg> {
    time::timeout(Duration::from_secs(30), async {
        loop {
            match events.recv().await.unwrap() {
                Event::Reset(messages) => return messages,
                Event::Rejected(message) => panic!("{message}"),
                _ => {}
            }
        }
    })
    .await
    .expect("snapshot deadline")
}
async fn update(events: &Receiver<Event>) -> Vec<ServerMsg> {
    time::timeout(Duration::from_secs(10), async {
        loop {
            if let Event::Messages(messages) = events.recv().await.unwrap() {
                return messages;
            }
        }
    })
    .await
    .expect("update deadline")
}
#[cfg(not(target_arch = "wasm32"))]
async fn direct_endpoint(key: Option<SecretKey>) -> Endpoint {
    let mut builder = Endpoint::builder(presets::Minimal)
        .relay_mode(iroh::RelayMode::Disabled)
        .alpns(vec![ALPN.to_vec()]);
    if let Some(key) = key {
        builder = builder.secret_key(key);
    }
    builder.bind().await.unwrap()
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn resumable_room_keeps_invite_progress_and_guest_admission() {
    use std::{cell::RefCell, rc::Rc};
    tokio::task::LocalSet::new()
        .run_until(async {
            let host_key = Identity::generate();
            let guest_key = Identity::generate();
            let (mut host, host_handle, events) = Host::new(
                direct_endpoint(Some(host_key.clone())).await,
                "P2Presume1".into(),
                snapshot(),
            )
            .unwrap();
            let original_invite = host.invite();
            let saved = Rc::new(RefCell::new(host.identity()));
            let saved_for_events = saved.clone();
            // Simulated disk adapter: admission is acknowledged before Welcome.
            let storage = tokio::task::spawn_local(async move {
                while let Ok(event) = events.recv().await {
                    if let Event::PersistRoom {
                        identity,
                        committed,
                        ..
                    } = event
                    {
                        *saved_for_events.borrow_mut() = identity;
                        committed.send(Ok(())).await.unwrap();
                    }
                }
            });
            host.require_persistence();
            let host_task = tokio::task::spawn_local(host.run());
            let (guest, guest_handle, guest_events) = Guest::new(
                direct_endpoint(Some(guest_key.clone())).await,
                Invite::parse(&original_invite).unwrap(),
            );
            let guest_task = tokio::task::spawn_local(guest.run());
            reset(&guest_events).await;
            guest_handle
                .send(ClientMsg::Select { piece_id: 0 })
                .unwrap();
            guest_handle
                .send(ClientMsg::Move {
                    piece_id: 0,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 1.0,
                        y_mm: 0.75,
                    },
                    client_seq: 1,
                    base_revision: 0,
                })
                .unwrap();
            update(&guest_events).await;
            update(&guest_events).await;
            let checkpoint = host_handle.checkpoint().await.unwrap();
            let identity = saved.borrow().clone();
            assert_eq!(
                identity.guest,
                Some(guest_key.public().as_bytes().to_owned())
            );
            assert!(!format!("{identity:?}").contains(&identity.invite));
            guest_handle.close();
            host_handle.close();
            guest_task.await.unwrap();
            host_task.await.unwrap();
            storage.await.unwrap();
            let endpoint = direct_endpoint(Some(host_key)).await;
            let current_address = endpoint.addr();
            let (restored, restored_handle, events) =
                Host::resume(endpoint, identity, checkpoint.clone()).unwrap();
            assert_eq!(restored.invite(), original_invite);
            assert_eq!(restored.authority.ownerships().count(), 0);
            let stored = tokio::task::spawn_local(async move {
                while let Ok(event) = events.recv().await {
                    if let Event::PersistRoom { .. } = event {
                        panic!("existing guest must already be admitted");
                    }
                }
            });
            let restored_task = tokio::task::spawn_local(restored.run());
            // Minimal native test endpoints do not use address lookup. Supply its
            // new routing hint while keeping the original host ID and capability.
            let mut invite = Invite::parse(&original_invite).unwrap();
            invite.address = current_address;
            let (stranger, stranger_handle, stranger_events) =
                Guest::new(direct_endpoint(None).await, invite.clone());
            let stranger_task = tokio::task::spawn_local(stranger.run());
            time::timeout(Duration::from_secs(10), async {
                loop {
                    if let Event::Rejected(message) = stranger_events.recv().await.unwrap() {
                        assert!(message.contains("two players"));
                        break;
                    }
                }
            })
            .await
            .unwrap();
            stranger_handle.close();
            stranger_task.await.unwrap();
            let (guest, guest_handle, guest_events) =
                Guest::new(direct_endpoint(Some(guest_key)).await, invite);
            let guest_task = tokio::task::spawn_local(guest.run());
            let state = reset(&guest_events).await;
            let restored_snapshot = state
                .iter()
                .find_map(|msg| {
                    if let ServerMsg::State { snapshot, .. } = msg {
                        Some(snapshot)
                    } else {
                        None
                    }
                })
                .unwrap();
            assert_eq!(encode(restored_snapshot), encode(&checkpoint));
            guest_handle
                .send(ClientMsg::Select { piece_id: 0 })
                .unwrap();
            guest_handle
                .send(ClientMsg::Move {
                    piece_id: 0,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 2.0,
                        y_mm: 0.5,
                    },
                    client_seq: 1,
                    base_revision: checkpoint.seq,
                })
                .unwrap();
            update(&guest_events).await;
            update(&guest_events).await;
            let final_state = restored_handle.checkpoint().await.unwrap();
            let pose = final_state
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(heddobureika_core::PieceId(0))
                .unwrap();
            assert_eq!((pose.x_mm(), pose.y_mm()), (2.0, 0.5));
            let frozen = restored_handle.prepare_reload().await.unwrap();
            restored_handle
                .send(ClientMsg::Select { piece_id: 1 })
                .unwrap();
            restored_handle
                .send(ClientMsg::Move {
                    piece_id: 1,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 99.0,
                        y_mm: 99.0,
                    },
                    client_seq: 1,
                    base_revision: frozen.seq,
                })
                .unwrap();
            assert_eq!(
                encode(&restored_handle.checkpoint().await.unwrap()),
                encode(&frozen),
                "the reload barrier must stop later mutations until close"
            );
            guest_handle.close();
            restored_handle.close();
            guest_task.await.unwrap();
            restored_task.await.unwrap();
            stored.await.unwrap();
        })
        .await;
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn checkpoints_restore_confirmed_progress_in_a_fresh_session() {
    tokio::task::LocalSet::new()
        .run_until(async {
            let (mut host, handle, events) =
                Host::new(direct_endpoint(None).await, "P2Psave001".into(), snapshot()).unwrap();
            let old_invite = host.invite();
            let mut guest_seq = Sequence::default();
            assert!(matches!(
                host.apply(
                    GUEST,
                    1,
                    1,
                    ClientMsg::Select { piece_id: 0 },
                    &mut guest_seq,
                    0
                ),
                ServerFrame::Events(_)
            ));
            assert!(matches!(
                host.apply(
                    GUEST,
                    2,
                    1,
                    ClientMsg::Move {
                        piece_id: 0,
                        drop_pos: heddobureika_core::PlayablePositionSnapshot {
                            x_mm: 1.0,
                            y_mm: 0.75
                        },
                        client_seq: 1,
                        base_revision: 0,
                    },
                    &mut guest_seq,
                    1
                ),
                ServerFrame::Events(_)
            ));
            // The host cannot overwrite a guest-held group with an unclaimed move.
            assert!(matches!(
                host.apply(
                    HOST,
                    1,
                    0,
                    ClientMsg::Move {
                        piece_id: 0,
                        drop_pos: heddobureika_core::PlayablePositionSnapshot {
                            x_mm: 99.0,
                            y_mm: 99.0
                        },
                        client_seq: 1,
                        base_revision: 0,
                    },
                    &mut Sequence::default(),
                    2
                ),
                ServerFrame::Reset(_)
            ));
            let mut saved_seq = None;
            host.checkpoint(&mut saved_seq).await;
            let Event::Checkpoint(saved) = events.recv().await.unwrap() else {
                panic!("checkpoint")
            };
            let pose = saved
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(heddobureika_core::PieceId(0))
                .unwrap();
            assert_eq!((pose.x_mm(), pose.y_mm()), (1.0, 0.75));
            host.checkpoint(&mut saved_seq).await;
            assert!(
                events.try_recv().is_err(),
                "unchanged progress is not checkpointed twice"
            );
            let (restored, new_handle, _) = Host::new(
                direct_endpoint(None).await,
                "P2Psave002".into(),
                saved.clone(),
            )
            .unwrap();
            assert_ne!(old_invite, restored.invite());
            assert_eq!(
                encode(restored.authority.snapshot().unwrap()),
                encode(&saved)
            );
            assert_eq!(restored.authority.ownerships().count(), 0);
            new_handle.close();
            handle.close();
        })
        .await;
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn shutdown_emits_the_final_checkpoint_without_waiting_for_a_tick() {
    tokio::task::LocalSet::new()
        .run_until(async {
            let (host, handle, events) =
                Host::new(direct_endpoint(None).await, "P2Psave003".into(), snapshot()).unwrap();
            let task = tokio::task::spawn_local(host.run());
            reset(&events).await;
            handle.send(ClientMsg::Select { piece_id: 0 }).unwrap();
            handle
                .send(ClientMsg::Move {
                    piece_id: 0,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 2.0,
                        y_mm: 0.75,
                    },
                    client_seq: 1,
                    base_revision: 0,
                })
                .unwrap();
            handle.close();
            let saved = time::timeout(Duration::from_secs(10), async {
                let mut last = None;
                while let Ok(event) = events.recv().await {
                    if let Event::Checkpoint(snapshot) = event {
                        last = Some(snapshot);
                    }
                }
                last.unwrap()
            })
            .await
            .unwrap();
            task.await.unwrap();
            let pose = saved
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(heddobureika_core::PieceId(0))
                .unwrap();
            assert_eq!((pose.x_mm(), pose.y_mm()), (2.0, 0.75));
        })
        .await;
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn native_roundtrip_admission_and_reconnect() {
    tokio::task::LocalSet::new()
        .run_until(async {
            let endpoint = direct_endpoint(None).await;
            let (host, host_handle, host_events) =
                Host::new(endpoint, "P2Ptest001".into(), snapshot()).unwrap();
            let invite = Invite::parse(&host.invite()).unwrap();
            assert!(Invite::parse(&format!("{}x", invite.encode())).is_err());
            let host_task = tokio::task::spawn_local(host.run());
            reset(&host_events).await;
            let key = SecretKey::generate();
            let (guest, handle, events) =
                Guest::new(direct_endpoint(Some(key.clone())).await, invite.clone());
            let task = tokio::task::spawn_local(guest.run());
            let initial = reset(&events).await;
            assert!(matches!(
                &initial[0],
                ServerMsg::Welcome {
                    client_id: Some(GUEST),
                    ..
                }
            ));
            handle.send(ClientMsg::Select { piece_id: 0 }).unwrap();
            let selected = update(&events).await;
            assert_eq!(selected.len(), 2);
            let selected_host = update(&host_events).await;
            assert_eq!(encode(&selected), encode(&selected_host));
            handle
                .send(ClientMsg::Move {
                    piece_id: 0,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 1.0,
                        y_mm: 0.75,
                    },
                    client_seq: 1,
                    base_revision: 0,
                })
                .unwrap();
            let guest_move = update(&events).await;
            assert_eq!(encode(&guest_move), encode(&update(&host_events).await));
            assert!(matches!(
                &guest_move[0],
                ServerMsg::PlayableUpdate {
                    source: Some(GUEST),
                    client_seq: Some(1),
                    ..
                }
            ));
            // A third endpoint cannot replace the admitted guest.
            let (third, third_handle, third_events) =
                Guest::new(direct_endpoint(None).await, invite.clone());
            let third_task = tokio::task::spawn_local(third.run());
            time::timeout(Duration::from_secs(10), async {
                loop {
                    if let Event::Rejected(message) = third_events.recv().await.unwrap() {
                        assert!(message.contains("two players"));
                        break;
                    }
                }
            })
            .await
            .unwrap();
            third_handle.close();
            third_task.await.unwrap();
            // Reconnect uses a fresh command sequence and receives the host's latest
            // snapshot, including any accepted command whose echo was lost.
            handle.close();
            task.await.unwrap();
            let (guest, handle2, events2) =
                Guest::new(direct_endpoint(Some(key)).await, invite.clone());
            let task2 = tokio::task::spawn_local(guest.run());
            let resumed = reset(&events2).await;
            let saved = resumed
                .iter()
                .find_map(|m| {
                    if let ServerMsg::State { snapshot, .. } = m {
                        Some(snapshot)
                    } else {
                        None
                    }
                })
                .unwrap();
            let pose = saved
                .restore_playable_from_spec()
                .unwrap()
                .piece_world_pose(heddobureika_core::PieceId(0))
                .unwrap();
            assert!((pose.x_mm() - 1.0).abs() < 0.001);
            assert!((pose.y_mm() - 0.75).abs() < 0.001);
            assert!(!resumed.iter().any(|m| matches!(
                m,
                ServerMsg::ControlUpdate {
                    update: RoomControlUpdate::Ownership {
                        owner: Some(GUEST),
                        ..
                    },
                    ..
                }
            )));
            handle2.send(ClientMsg::Select { piece_id: 1 }).unwrap();
            update(&events2).await;
            // Host commands use the same authority; another player's claim wins.
            host_handle.send(ClientMsg::Select { piece_id: 1 }).unwrap();
            let host_reset = reset(&host_events).await;
            assert!(host_reset.iter().any(|m| matches!(
                m,
                ServerMsg::ControlUpdate {
                    update: RoomControlUpdate::Ownership {
                        owner: Some(GUEST),
                        ..
                    },
                    ..
                }
            )));
            handle2.close();
            task2.await.unwrap();
            host_handle.close();
            host_task.await.unwrap();
        })
        .await;
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn replaced_guest_stops_retrying_and_replacement_can_reload() {
    tokio::task::LocalSet::new()
        .run_until(async {
            let (host, host_handle, host_events) =
                Host::new(direct_endpoint(None).await, "P2Preplace".into(), snapshot()).unwrap();
            let invite = Invite::parse(&host.invite()).unwrap();
            let host_task = tokio::task::spawn_local(host.run());
            // Drain host checkpoints/statuses throughout the test.
            let host_reader =
                tokio::task::spawn_local(async move { while host_events.recv().await.is_ok() {} });
            let key = Identity::generate();
            let (first, first_handle, first_events) =
                Guest::new(direct_endpoint(Some(key.clone())).await, invite.clone());
            let first_task = tokio::task::spawn_local(first.run());
            reset(&first_events).await;
            first_handle
                .send(ClientMsg::Select { piece_id: 0 })
                .unwrap();
            update(&first_events).await;

            // A copied browser session can retain the very same endpoint key.
            let (second, second_handle, second_events) =
                Guest::new(direct_endpoint(Some(key.clone())).await, invite.clone());
            let second_task = tokio::task::spawn_local(second.run());
            let joined = reset(&second_events).await;
            assert!(!joined.iter().any(|m| matches!(
                m,
                ServerMsg::ControlUpdate {
                    update: RoomControlUpdate::Ownership {
                        owner: Some(GUEST),
                        ..
                    },
                    ..
                }
            )));
            time::timeout(Duration::from_secs(10), first_task)
                .await
                .expect("replaced guest must stop rather than reconnect")
                .unwrap();
            let mut rejected = false;
            while let Ok(event) = first_events.recv().await {
                match event {
                    Event::Rejected(message) => {
                        assert!(message.contains("another tab or app"));
                        rejected = true;
                    }
                    Event::Reset(_) => panic!("replaced guest reconnected"),
                    Event::Status {
                        connected: false, ..
                    } => {
                        panic!("replaced guest started retrying")
                    }
                    _ => {}
                }
            }
            assert!(rejected);
            assert!(
                first_handle
                    .send(ClientMsg::Select { piece_id: 0 })
                    .is_err()
            );
            second_handle
                .send(ClientMsg::Select { piece_id: 0 })
                .unwrap();
            update(&second_events).await;
            second_handle.close();
            second_task.await.unwrap();

            // Normal reloads still rejoin with the admitted identity and a
            // fresh claim sequence after the previous guest has gone away.
            let (reloaded, reloaded_handle, reloaded_events) =
                Guest::new(direct_endpoint(Some(key)).await, invite);
            let reloaded_task = tokio::task::spawn_local(reloaded.run());
            reset(&reloaded_events).await;
            reloaded_handle
                .send(ClientMsg::Select { piece_id: 0 })
                .unwrap();
            update(&reloaded_events).await;
            reloaded_handle
                .send(ClientMsg::Move {
                    piece_id: 0,
                    drop_pos: heddobureika_core::PlayablePositionSnapshot {
                        x_mm: 1.0,
                        y_mm: 0.75,
                    },
                    client_seq: 1,
                    base_revision: 0,
                })
                .unwrap();
            assert!(update(&reloaded_events).await.iter().any(|m| matches!(
                m,
                ServerMsg::PlayableUpdate {
                    source: Some(GUEST),
                    ..
                }
            )));
            reloaded_handle.close();
            reloaded_task.await.unwrap();
            host_handle.close();
            host_task.await.unwrap();
            host_reader.await.unwrap();
        })
        .await;
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::test]
async fn wrong_invitation_does_not_consume_guest_slot() {
    tokio::task::LocalSet::new()
        .run_until(async {
            let endpoint = direct_endpoint(None).await;
            let (host, host_handle, host_events) =
                Host::new(endpoint, "P2Ptest002".into(), snapshot()).unwrap();
            let mut invite = Invite::parse(&host.invite()).unwrap();
            let good = invite.clone();
            invite.secret[0] ^= 1;
            let host_task = tokio::task::spawn_local(host.run());
            reset(&host_events).await;
            let (guest, handle, events) = Guest::new(direct_endpoint(None).await, invite);
            let task = tokio::task::spawn_local(guest.run());
            time::timeout(Duration::from_secs(10), async {
                loop {
                    if let Event::Rejected(message) = events.recv().await.unwrap() {
                        assert_eq!(message, "Invalid P2P invite");
                        break;
                    }
                }
            })
            .await
            .unwrap();
            handle.close();
            task.await.unwrap();
            let (guest, handle, events) = Guest::new(direct_endpoint(None).await, good);
            let task = tokio::task::spawn_local(guest.run());
            reset(&events).await;
            handle.close();
            task.await.unwrap();
            host_handle.close();
            host_task.await.unwrap();
        })
        .await;
}

#[cfg(target_arch = "wasm32")]
wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen_test::wasm_bindgen_test]
async fn browser_relay_roundtrip() {
    let endpoint = bind(None).await.unwrap();
    time::timeout(Duration::from_secs(30), endpoint.online())
        .await
        .expect("host relay online");
    let (host, host_handle, host_events) =
        Host::new(endpoint, "P2Pweb0001".into(), snapshot()).unwrap();
    let invite = Invite::parse(&host.invite()).unwrap();
    n0_future::task::spawn(host.run());
    reset(&host_events).await;
    let endpoint = bind(None).await.unwrap();
    let (guest, handle, events) = Guest::new(endpoint, invite);
    n0_future::task::spawn(guest.run());
    reset(&events).await;
    handle.send(ClientMsg::Select { piece_id: 0 }).unwrap();
    update(&events).await;
    update(&host_events).await;
    handle
        .send(ClientMsg::Move {
            piece_id: 0,
            drop_pos: heddobureika_core::PlayablePositionSnapshot {
                x_mm: 1.0,
                y_mm: 0.75,
            },
            client_seq: 1,
            base_revision: 0,
        })
        .unwrap();
    let guest_update = update(&events).await;
    assert_eq!(encode(&guest_update), encode(&update(&host_events).await));
    assert!(matches!(
        &guest_update[0],
        ServerMsg::PlayableUpdate {
            source: Some(GUEST),
            ..
        }
    ));
    handle.close();
    host_handle.close();
}
