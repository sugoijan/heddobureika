use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::PathBuf;
use std::time::{Duration as StdDuration, Instant, SystemTime, UNIX_EPOCH};

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use clap::{Args, Parser, Subcommand};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::codec::{decode_result, encode};
use heddobureika_core::groups_from_connections;
use heddobureika_core::room::apply_room_update_to_snapshot;
use heddobureika_core::room_id::{RoomId, ROOM_ID_ALPHABET, ROOM_ID_LEN};
use heddobureika_core::{
    angle_delta, compute_workspace_layout, is_fully_connected, neighbor_id, normalize_angle,
    piece_local_offset, puzzle_by_slug, rotate_vec, AdminMsg, ClientId, ClientMsg, GameSnapshot,
    PuzzleImageRef, PuzzleSpec, RecordedCommand, RecordedCommandKind, RecordedCommandOutcome,
    RoomUpdate, ServerMsg, ASSET_CHUNK_BYTES, DEFAULT_PUZZLE_SLUG, DIR_DOWN, DIR_LEFT, DIR_RIGHT,
    DIR_UP, PRIVATE_UPLOAD_MAX_BYTES, PUZZLE_CATALOG,
};
use mime_guess::MimeGuess;
use p256::ecdsa::{signature::Signer, Signature, SigningKey};
use p256::elliptic_curve::rand_core::{OsRng, RngCore};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use serde::Deserialize;
use serde::Serialize;
use sha2::{Digest, Sha256};
use tokio::time::{sleep, timeout, Duration};
use tokio_tungstenite::tungstenite::client::IntoClientRequest;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async, MaybeTlsStream, WebSocketStream};
use url::Url;

mod bot;

#[derive(Parser)]
#[command(
    name = "heddobureika-cli",
    version,
    about = "Admin and simulation tools for heddobureika rooms"
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Rooms {
        #[command(subcommand)]
        command: RoomCommand,
    },
}

#[derive(Subcommand)]
enum RoomCommand {
    Create {
        #[arg(
            long,
            env = "ROOM_WS_BASE_URL",
            default_value = "ws://localhost:8787/ws"
        )]
        base_url: String,
        #[arg(long, env = "ROOM_ADMIN_TOKEN")]
        admin_token: String,
        #[arg(long, default_value = DEFAULT_PUZZLE_SLUG)]
        puzzle: String,
        #[arg(long)]
        puzzle_file: Option<PathBuf>,
        #[arg(long)]
        pieces: Option<u32>,
        #[arg(long)]
        seed: Option<String>,
        #[arg(long)]
        room_id: Option<String>,
        #[arg(long)]
        best_effort: bool,
        #[arg(long)]
        no_connect: bool,
    },
    Bot {
        #[command(subcommand)]
        command: bot::BotCommand,
    },
    Recording {
        #[command(subcommand)]
        command: RecordingCommand,
    },
}

#[derive(Subcommand)]
enum RecordingCommand {
    Enable {
        #[command(flatten)]
        room: AdminRoomArgs,
        #[arg(long)]
        max_events: Option<u32>,
    },
    Disable {
        #[command(flatten)]
        room: AdminRoomArgs,
    },
    Status {
        #[command(flatten)]
        room: AdminRoomArgs,
    },
    Export {
        #[command(flatten)]
        room: AdminRoomArgs,
        #[arg(long)]
        output: Option<PathBuf>,
        #[arg(long)]
        after_id: Option<u64>,
        #[arg(long, default_value_t = 500)]
        limit: u32,
    },
    Clear {
        #[command(flatten)]
        room: AdminRoomArgs,
    },
    Summarize {
        #[arg(long)]
        input: PathBuf,
    },
    Compare {
        #[arg(long)]
        baseline: PathBuf,
        #[arg(long)]
        candidate: PathBuf,
    },
}

#[derive(Args, Clone)]
struct RoomArgs {
    #[arg(
        long,
        env = "ROOM_WS_BASE_URL",
        default_value = "ws://localhost:8787/ws"
    )]
    base_url: String,
    #[arg(long)]
    room_id: String,
}

#[derive(Args, Clone)]
struct AdminRoomArgs {
    #[command(flatten)]
    room: RoomArgs,
    #[arg(long, env = "ROOM_ADMIN_TOKEN")]
    admin_token: String,
}

struct UploadPlan {
    path: PathBuf,
    pieces: Option<u32>,
    seed: Option<u32>,
}

type WsStream = WebSocketStream<MaybeTlsStream<tokio::net::TcpStream>>;
type WsWrite = futures_util::stream::SplitSink<WsStream, Message>;
type WsRead = futures_util::stream::SplitStream<WsStream>;

#[derive(Default)]
struct StatsAgg {
    total_events: u64,
    first_ts: Option<i64>,
    last_ts: Option<i64>,
    unique_clients: HashSet<u64>,
    by_kind: HashMap<String, u64>,
    by_outcome: HashMap<String, u64>,
    select_count: u64,
    move_count: u64,
    transform_count: u64,
    place_count: u64,
    release_count: u64,
    ignored_or_rejected_selects: u64,
    applied_count: u64,
    active_clients_multi_events: u64,
}

#[derive(Debug, Clone)]
struct StatsSummary {
    total_events: u64,
    duration_secs: f64,
    events_per_sec: f64,
    selects_per_sec: f64,
    updates_per_sec: f64,
    places_per_sec: f64,
    unique_clients: usize,
    select_count: u64,
    move_count: u64,
    transform_count: u64,
    place_count: u64,
    release_count: u64,
    avg_updates_per_drag: f64,
    transform_share: f64,
    select_conflict_rate: f64,
    applied_rate: f64,
    overlap_event_rate: f64,
    move_interval_p50_ms: f64,
    move_interval_p80_ms: f64,
    think_interval_p50_ms: f64,
    think_interval_p80_ms: f64,
    drag_duration_p50_ms: f64,
    drag_duration_p80_ms: f64,
    drag_speed_p50_px_per_sec: f64,
    drag_speed_p80_px_per_sec: f64,
    movement_speed_avg_px_per_sec: f64,
    short_drag_rate: f64,
    same_piece_retry_rate: f64,
    piece_variety_window_actions: usize,
    piece_variety_avg_unique: f64,
    piece_variety_p50_unique: f64,
    piece_variety_p80_unique: f64,
    inferred_piece_count: usize,
    inferred_grid_cols: usize,
    inferred_grid_rows: usize,
    progress_samples: u64,
    join_events: u64,
    groups_start: Option<u32>,
    groups_end: Option<u32>,
    largest_group_peak: Option<u32>,
    connected_edges_start: Option<u32>,
    connected_edges_end: Option<u32>,
    connected_edges_total: Option<u32>,
    border_metric_source: &'static str,
    completion_metric_source: &'static str,
    border_done_100_secs: Option<f64>,
    completion_time_secs: Option<f64>,
    early_updates_per_drag: f64,
    late_updates_per_drag: f64,
    early_short_drag_rate: f64,
    late_short_drag_rate: f64,
    early_same_piece_retry_rate: f64,
    late_same_piece_retry_rate: f64,
    by_kind: HashMap<String, u64>,
    by_outcome: HashMap<String, u64>,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct RecordLine {
    id: u64,
    ts_ms: i64,
    client_id: u64,
    kind: String,
    piece_id: Option<u32>,
    anchor_id: Option<u32>,
    pos: Option<(f32, f32)>,
    rot_deg: Option<f32>,
    client_seq: Option<u64>,
    room_seq: Option<u64>,
    outcome: String,
    reason: Option<String>,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ReasonProgressPayload {
    #[serde(default)]
    reason: Option<String>,
    #[serde(default)]
    groups_before: Option<u32>,
    #[serde(default)]
    groups_after: Option<u32>,
    #[serde(default)]
    largest_group_before: Option<u32>,
    #[serde(default)]
    largest_group_after: Option<u32>,
    #[serde(default)]
    connected_edges_before: Option<u32>,
    #[serde(default)]
    connected_edges_after: Option<u32>,
    #[serde(default)]
    total_edges: Option<u32>,
    #[serde(default)]
    border_done_before: Option<bool>,
    #[serde(default)]
    border_done_after: Option<bool>,
    #[serde(default)]
    solved_before: Option<bool>,
    #[serde(default)]
    solved_after: Option<bool>,
}

#[derive(Clone, Copy, Debug)]
struct ReasonProgress {
    groups_before: u32,
    groups_after: u32,
    largest_group_before: u32,
    largest_group_after: u32,
    connected_edges_before: u32,
    connected_edges_after: u32,
    total_edges: u32,
    border_done_before: bool,
    border_done_after: bool,
    solved_before: bool,
    solved_after: bool,
}

#[derive(Clone, Debug)]
struct DragSample {
    start_ts: i64,
    updates: u32,
    avg_update_interval_ms: Option<f32>,
    drag_duration_ms: Option<f32>,
    distance_px: Option<f32>,
    path_distance_px: Option<f32>,
    same_piece_retry: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    install_crypto_provider()?;
    let cli = Cli::parse();

    match cli.command {
        Commands::Rooms { command } => match command {
            RoomCommand::Create {
                base_url,
                admin_token,
                puzzle,
                puzzle_file,
                pieces,
                seed,
                room_id,
                best_effort,
                no_connect,
            } => {
                create_room(
                    base_url,
                    admin_token,
                    puzzle,
                    puzzle_file,
                    pieces,
                    seed,
                    room_id,
                    best_effort,
                    no_connect,
                )
                .await?;
            }
            RoomCommand::Bot { command } => {
                bot::run(command).await?;
            }
            RoomCommand::Recording { command } => match command {
                RecordingCommand::Enable { room, max_events } => {
                    let status = recording_set(room, true, max_events).await?;
                    print_recording_status(&status);
                }
                RecordingCommand::Disable { room } => {
                    let status = recording_set(room, false, None).await?;
                    print_recording_status(&status);
                }
                RecordingCommand::Status { room } => {
                    let status = recording_status(room).await?;
                    print_recording_status(&status);
                }
                RecordingCommand::Export {
                    room,
                    output,
                    after_id,
                    limit,
                } => {
                    let count = recording_export(room, output, after_id, limit).await?;
                    eprintln!("exported_rows: {count}");
                }
                RecordingCommand::Clear { room } => {
                    recording_clear(room).await?;
                    println!("recording_cleared: true");
                }
                RecordingCommand::Summarize { input } => {
                    let summary = summarize_file(&input)?;
                    print_summary(&summary);
                }
                RecordingCommand::Compare {
                    baseline,
                    candidate,
                } => {
                    let base = summarize_file(&baseline)?;
                    let cand = summarize_file(&candidate)?;
                    print_compare(&base, &cand);
                }
            },
        },
    }

    Ok(())
}

async fn create_room(
    base_url: String,
    admin_token: String,
    puzzle: String,
    puzzle_file: Option<PathBuf>,
    pieces: Option<u32>,
    seed: Option<String>,
    room_id: Option<String>,
    best_effort: bool,
    no_connect: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let room_id = match room_id {
        Some(id) => RoomId::parse(&id)?.to_string(),
        None => generate_room_id(),
    };
    let persistence = if best_effort {
        heddobureika_core::RoomPersistence::BestEffort
    } else {
        heddobureika_core::RoomPersistence::Durable
    };
    let seed = match seed.as_deref() {
        Some(raw) => Some(parse_seed_arg(raw)?),
        None => None,
    };

    let (create_spec, upload_plan) = if let Some(path) = puzzle_file {
        if puzzle_by_slug(&puzzle).is_none() {
            eprintln!("unknown puzzle: {puzzle}");
            eprintln!("available puzzles:");
            for entry in PUZZLE_CATALOG {
                eprintln!("  {} ({})", entry.slug, entry.label);
            }
            return Err(err_msg(format!("unknown puzzle: {puzzle}")));
        }
        (
            PuzzleSpec {
                image_ref: PuzzleImageRef::BuiltIn {
                    slug: puzzle.clone(),
                },
                pieces: None,
                seed: None,
            },
            Some(UploadPlan { path, pieces, seed }),
        )
    } else {
        if puzzle_by_slug(&puzzle).is_none() {
            eprintln!("unknown puzzle: {puzzle}");
            eprintln!("available puzzles:");
            for entry in PUZZLE_CATALOG {
                eprintln!("  {} ({})", entry.slug, entry.label);
            }
            return Err(err_msg(format!("unknown puzzle: {puzzle}")));
        }
        (
            PuzzleSpec {
                image_ref: PuzzleImageRef::BuiltIn {
                    slug: puzzle.clone(),
                },
                pieces,
                seed,
            },
            None,
        )
    };

    let join_url = build_join_url(&base_url, &room_id)?;
    let admin_url = build_admin_url(&base_url, &room_id, &admin_token)?;

    if no_connect {
        println!("room_id: {room_id}");
        println!("join_url: {join_url}");
        println!("admin_url: {admin_url}");
        return Ok(());
    }

    let (mut write, mut read) = connect_ws(&base_url, &room_id, Some(&admin_token)).await?;

    let create_msg = AdminMsg::Create {
        persistence,
        puzzle: create_spec,
    };
    send_admin_msg(&mut write, create_msg).await?;

    wait_for_admin_ack(&mut read, Duration::from_secs(30)).await?;

    if let Some(plan) = upload_plan {
        upload_private_file(&mut write, &mut read, plan).await?;
    }

    println!("room_id: {room_id}");
    println!("join_url: {join_url}");
    Ok(())
}

async fn wait_for_admin_ack(
    read: &mut WsRead,
    dur: Duration,
) -> Result<(), Box<dyn std::error::Error>> {
    let result = timeout(dur, async {
        loop {
            let Some(msg) = recv_server_msg(read).await? else {
                return Err(err_msg("server closed before admin ack"));
            };
            match msg {
                ServerMsg::AdminAck { .. } => return Ok(()),
                ServerMsg::Error { code, message } => {
                    return Err(err_msg(format!("server error {code}: {message}")));
                }
                _ => {}
            }
        }
    })
    .await;
    match result {
        Ok(inner) => inner,
        Err(_) => Err(err_msg("timed out waiting for admin ack")),
    }
}

async fn upload_private_file(
    write: &mut WsWrite,
    read: &mut WsRead,
    plan: UploadPlan,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytes = std::fs::read(&plan.path)?;
    if bytes.is_empty() {
        return Err(err_msg("puzzle file is empty"));
    }
    if bytes.len() > PRIVATE_UPLOAD_MAX_BYTES as usize {
        return Err(err_msg(format!(
            "upload exceeds limit (max {} bytes)",
            PRIVATE_UPLOAD_MAX_BYTES
        )));
    }

    let mime = MimeGuess::from_path(&plan.path)
        .first_or_octet_stream()
        .to_string();

    send_admin_msg(
        write,
        AdminMsg::UploadPrivateBegin {
            mime,
            size: bytes.len() as u32,
        },
    )
    .await?;

    for chunk in bytes.chunks(ASSET_CHUNK_BYTES) {
        send_admin_msg(
            write,
            AdminMsg::UploadPrivateChunk {
                bytes: chunk.to_vec(),
            },
        )
        .await?;
    }

    send_admin_msg(
        write,
        AdminMsg::UploadPrivateEnd {
            pieces: plan.pieces,
            seed: plan.seed,
        },
    )
    .await?;

    let result = timeout(Duration::from_secs(180), async {
        let mut got_ack = false;
        let mut got_state = false;
        loop {
            let Some(msg) = recv_server_msg(read).await? else {
                return Err(err_msg("server closed before upload ack"));
            };
            match msg {
                ServerMsg::UploadAck { .. } => {
                    got_ack = true;
                    if got_state {
                        return Ok(());
                    }
                }
                ServerMsg::State { .. } => {
                    got_state = true;
                    if got_ack {
                        return Ok(());
                    }
                }
                ServerMsg::Error { code, message } => {
                    return Err(err_msg(format!("server error {code}: {message}")));
                }
                _ => {}
            }
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err(err_msg("timed out waiting for upload ack")),
    }
}

async fn recording_set(
    room: AdminRoomArgs,
    enabled: bool,
    max_events: Option<u32>,
) -> Result<RecordingStatusLine, Box<dyn std::error::Error>> {
    let (mut write, mut read) = connect_ws(
        &room.room.base_url,
        &room.room.room_id,
        Some(room.admin_token.as_str()),
    )
    .await?;

    send_admin_msg(
        &mut write,
        AdminMsg::RecordingSet {
            enabled,
            max_events,
        },
    )
    .await?;

    wait_for_recording_status(&mut read, Duration::from_secs(10)).await
}

async fn recording_status(
    room: AdminRoomArgs,
) -> Result<RecordingStatusLine, Box<dyn std::error::Error>> {
    let (mut write, mut read) = connect_ws(
        &room.room.base_url,
        &room.room.room_id,
        Some(room.admin_token.as_str()),
    )
    .await?;

    send_admin_msg(&mut write, AdminMsg::RecordingStatus).await?;
    wait_for_recording_status(&mut read, Duration::from_secs(10)).await
}

async fn recording_clear(room: AdminRoomArgs) -> Result<(), Box<dyn std::error::Error>> {
    let (mut write, mut read) = connect_ws(
        &room.room.base_url,
        &room.room.room_id,
        Some(room.admin_token.as_str()),
    )
    .await?;

    send_admin_msg(&mut write, AdminMsg::RecordingClear).await?;

    let result = timeout(Duration::from_secs(10), async {
        loop {
            let Some(msg) = recv_server_msg(&mut read).await? else {
                return Err(err_msg("server closed before recording clear ack"));
            };
            match msg {
                ServerMsg::RecordingCleared => return Ok(()),
                ServerMsg::Error { code, message } => {
                    return Err(err_msg(format!("server error {code}: {message}")));
                }
                _ => {}
            }
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err(err_msg("timed out waiting for recording clear ack")),
    }
}

async fn recording_export(
    room: AdminRoomArgs,
    output: Option<PathBuf>,
    after_id: Option<u64>,
    limit: u32,
) -> Result<u64, Box<dyn std::error::Error>> {
    let (mut write, mut read) = connect_ws(
        &room.room.base_url,
        &room.room.room_id,
        Some(room.admin_token.as_str()),
    )
    .await?;

    let mut writer: Box<dyn Write> = match output {
        Some(path) => Box::new(BufWriter::new(File::create(path)?)),
        None => Box::new(BufWriter::new(std::io::stdout().lock())),
    };

    let mut cursor = after_id;
    let mut total = 0u64;
    let safe_limit = limit.clamp(1, 5000);

    loop {
        send_admin_msg(
            &mut write,
            AdminMsg::RecordingExport {
                after_id: cursor,
                limit: safe_limit,
            },
        )
        .await?;

        let rows_msg = wait_for_recording_rows(&mut read, Duration::from_secs(10)).await?;
        if rows_msg.rows.is_empty() {
            break;
        }

        for row in &rows_msg.rows {
            let value = serde_json::json!({
                "id": row.id,
                "ts_ms": row.ts_ms,
                "client_id": row.client_id.as_u64(),
                "kind": recorded_kind_name(row.kind),
                "piece_id": row.piece_id,
                "anchor_id": row.anchor_id,
                "pos": row.pos,
                "rot_deg": row.rot_deg,
                "client_seq": row.client_seq,
                "room_seq": row.room_seq,
                "outcome": recorded_outcome_name(row.outcome),
                "reason": row.reason,
            });
            serde_json::to_writer(&mut writer, &value)?;
            writer.write_all(b"\n")?;
            total = total.saturating_add(1);
        }

        writer.flush()?;

        cursor = rows_msg.next_after_id;
        if cursor.is_none() {
            break;
        }
    }

    Ok(total)
}

async fn wait_for_recording_status(
    read: &mut WsRead,
    dur: Duration,
) -> Result<RecordingStatusLine, Box<dyn std::error::Error>> {
    let result = timeout(dur, async {
        loop {
            let Some(msg) = recv_server_msg(read).await? else {
                return Err(err_msg("server closed before recording status"));
            };
            match msg {
                ServerMsg::RecordingStatus {
                    enabled,
                    capped,
                    max_events,
                    event_count,
                    dropped_events,
                } => {
                    return Ok(RecordingStatusLine {
                        enabled,
                        capped,
                        max_events,
                        event_count,
                        dropped_events,
                    });
                }
                ServerMsg::Error { code, message } => {
                    return Err(err_msg(format!("server error {code}: {message}")));
                }
                _ => {}
            }
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err(err_msg("timed out waiting for recording status")),
    }
}

async fn wait_for_recording_rows(
    read: &mut WsRead,
    dur: Duration,
) -> Result<RecordingRowsLine, Box<dyn std::error::Error>> {
    let result = timeout(dur, async {
        loop {
            let Some(msg) = recv_server_msg(read).await? else {
                return Err(err_msg("server closed before recording rows"));
            };
            match msg {
                ServerMsg::RecordingRows {
                    rows,
                    next_after_id,
                } => {
                    return Ok(RecordingRowsLine {
                        rows,
                        next_after_id,
                    });
                }
                ServerMsg::Error { code, message } => {
                    return Err(err_msg(format!("server error {code}: {message}")));
                }
                _ => {}
            }
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err(err_msg("timed out waiting for recording rows")),
    }
}

#[derive(Debug, Clone)]
struct RecordingStatusLine {
    enabled: bool,
    capped: bool,
    max_events: u32,
    event_count: u64,
    dropped_events: u64,
}

#[derive(Debug, Clone)]
struct RecordingRowsLine {
    rows: Vec<RecordedCommand>,
    next_after_id: Option<u64>,
}

fn print_recording_status(status: &RecordingStatusLine) {
    println!("enabled: {}", status.enabled);
    println!("capped: {}", status.capped);
    println!("max_events: {}", status.max_events);
    println!("event_count: {}", status.event_count);
    println!("dropped_events: {}", status.dropped_events);
}

fn summarize_file(path: &PathBuf) -> Result<StatsSummary, Box<dyn std::error::Error>> {
    const PIECE_VARIETY_WINDOW_ACTIONS: usize = 100;

    #[derive(Clone)]
    struct ActiveDrag {
        start_ts: i64,
        piece_id: u32,
        first_update_ts: Option<i64>,
        last_update_ts: Option<i64>,
        update_count: u32,
        first_pos: Option<(f32, f32)>,
        last_pos: Option<(f32, f32)>,
        path_distance_px: f32,
        same_piece_retry: bool,
    }

    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut agg = StatsAgg::default();
    let mut active_clients: HashSet<u64> = HashSet::new();
    let mut think_intervals_ms = Vec::<f64>::new();
    let mut move_intervals_ms = Vec::<f64>::new();
    let mut drags = Vec::<DragSample>::new();
    let mut last_select_ts_by_client = HashMap::<u64, i64>::new();
    let mut last_update_ts_by_client = HashMap::<u64, i64>::new();
    let mut active_by_client = HashMap::<u64, ActiveDrag>::new();
    let mut last_completed_by_client = HashMap::<u64, (usize, u32, i64)>::new();
    let mut piece_action_ids = Vec::<u32>::new();
    let mut max_piece_seen: Option<u32> = None;
    let mut last_applied_ts_by_piece = HashMap::<u32, i64>::new();
    let mut progress_samples = 0u64;
    let mut join_events = 0u64;
    let mut groups_start: Option<u32> = None;
    let mut groups_end: Option<u32> = None;
    let mut largest_group_peak: Option<u32> = None;
    let mut connected_edges_start: Option<u32> = None;
    let mut connected_edges_end: Option<u32> = None;
    let mut connected_edges_total: Option<u32> = None;
    let mut border_done_progress_ts: Option<i64> = None;
    let mut solved_progress_ts: Option<i64> = None;

    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let row: RecordLine = serde_json::from_str(&line)
            .map_err(|err| err_msg(format!("invalid NDJSON row: {err}")))?;
        let reason_progress = parse_reason_progress(row.reason.as_deref());

        agg.total_events = agg.total_events.saturating_add(1);
        agg.first_ts = Some(
            agg.first_ts
                .map(|value| value.min(row.ts_ms))
                .unwrap_or(row.ts_ms),
        );
        agg.last_ts = Some(
            agg.last_ts
                .map(|value| value.max(row.ts_ms))
                .unwrap_or(row.ts_ms),
        );
        agg.unique_clients.insert(row.client_id);
        *agg.by_kind.entry(row.kind.clone()).or_insert(0) += 1;
        *agg.by_outcome.entry(row.outcome.clone()).or_insert(0) += 1;

        if row.outcome == "Applied" {
            agg.applied_count = agg.applied_count.saturating_add(1);
            if let Some(progress) = reason_progress {
                progress_samples = progress_samples.saturating_add(1);
                if groups_start.is_none() {
                    groups_start = Some(progress.groups_before);
                }
                groups_end = Some(progress.groups_after);
                largest_group_peak = Some(
                    largest_group_peak
                        .unwrap_or(0)
                        .max(progress.largest_group_before)
                        .max(progress.largest_group_after),
                );
                if connected_edges_start.is_none() {
                    connected_edges_start = Some(progress.connected_edges_before);
                }
                connected_edges_end = Some(progress.connected_edges_after);
                connected_edges_total = Some(progress.total_edges);
                if progress.groups_after < progress.groups_before {
                    join_events = join_events
                        .saturating_add((progress.groups_before - progress.groups_after) as u64);
                }
                if border_done_progress_ts.is_none()
                    && !progress.border_done_before
                    && progress.border_done_after
                {
                    border_done_progress_ts = Some(row.ts_ms);
                }
                if solved_progress_ts.is_none() && !progress.solved_before && progress.solved_after
                {
                    solved_progress_ts = Some(row.ts_ms);
                }
            }
        }
        if let Some(piece_id) = action_piece_id(&row) {
            piece_action_ids.push(piece_id);
            max_piece_seen = Some(
                max_piece_seen
                    .map(|value| value.max(piece_id))
                    .unwrap_or(piece_id),
            );
            if row.outcome == "Applied" {
                last_applied_ts_by_piece.insert(piece_id, row.ts_ms);
            }
        }

        match row.kind.as_str() {
            "Select" => {
                agg.select_count = agg.select_count.saturating_add(1);
                if row.outcome == "Ignored" || row.outcome == "Rejected" {
                    agg.ignored_or_rejected_selects =
                        agg.ignored_or_rejected_selects.saturating_add(1);
                }
                if row.outcome == "Applied" {
                    active_clients.insert(row.client_id);
                }

                if let Some(prev_ts) = last_select_ts_by_client.insert(row.client_id, row.ts_ms) {
                    if row.ts_ms > prev_ts {
                        think_intervals_ms.push((row.ts_ms - prev_ts) as f64);
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
                        ActiveDrag {
                            start_ts: row.ts_ms,
                            piece_id,
                            first_update_ts: None,
                            last_update_ts: None,
                            update_count: 0,
                            first_pos: None,
                            last_pos: None,
                            path_distance_px: 0.0,
                            same_piece_retry: false,
                        },
                    );
                }
            }
            "Move" | "Transform" => {
                if row.kind == "Move" {
                    agg.move_count = agg.move_count.saturating_add(1);
                } else {
                    agg.transform_count = agg.transform_count.saturating_add(1);
                }
                if let Some(prev_ts) = last_update_ts_by_client.insert(row.client_id, row.ts_ms) {
                    if row.ts_ms > prev_ts {
                        move_intervals_ms.push((row.ts_ms - prev_ts) as f64);
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
                        if let Some(prev_pos) = active.last_pos {
                            active.path_distance_px += ((pos.0 - prev_pos.0).powi(2)
                                + (pos.1 - prev_pos.1).powi(2))
                            .sqrt();
                        }
                        active.last_pos = Some(pos);
                    }
                }
            }
            "Place" => {
                agg.place_count = agg.place_count.saturating_add(1);
                if row.outcome == "Applied" {
                    active_clients.remove(&row.client_id);
                }
            }
            "Release" => {
                agg.release_count = agg.release_count.saturating_add(1);
                if row.outcome == "Applied" {
                    active_clients.remove(&row.client_id);
                }
            }
            "Flip" => {
                if row.outcome == "Applied" {
                    active_clients.remove(&row.client_id);
                }
            }
            _ => {}
        }

        if matches!(row.kind.as_str(), "Place" | "Release" | "Flip") {
            if let Some(active) = active_by_client.remove(&row.client_id) {
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
                    path_distance_px: if active.update_count > 1 {
                        Some(active.path_distance_px)
                    } else {
                        None
                    },
                    same_piece_retry: active.same_piece_retry,
                };
                drags.push(sample);
                let idx = drags.len().saturating_sub(1);
                last_completed_by_client.insert(row.client_id, (idx, active.piece_id, row.ts_ms));
            }
        }

        if active_clients.len() > 1 {
            agg.active_clients_multi_events = agg.active_clients_multi_events.saturating_add(1);
        }
    }

    let duration_secs = match (agg.first_ts, agg.last_ts) {
        (Some(start), Some(end)) if end > start => (end - start) as f64 / 1000.0,
        _ => 0.0,
    };

    let total_updates = agg.move_count.saturating_add(agg.transform_count);
    let avg_updates_per_drag =
        mean_f64(drags.iter().map(|drag| drag.updates as f64)).unwrap_or(0.0);
    let transform_share = if total_updates == 0 {
        0.0
    } else {
        agg.transform_count as f64 / total_updates as f64
    };
    let select_conflict_rate = if agg.select_count == 0 {
        0.0
    } else {
        agg.ignored_or_rejected_selects as f64 / agg.select_count as f64
    };
    let applied_rate = if agg.total_events == 0 {
        0.0
    } else {
        agg.applied_count as f64 / agg.total_events as f64
    };
    let overlap_event_rate = if agg.total_events == 0 {
        0.0
    } else {
        agg.active_clients_multi_events as f64 / agg.total_events as f64
    };
    let selects_per_sec = if duration_secs <= 0.0 {
        0.0
    } else {
        agg.select_count as f64 / duration_secs
    };
    let updates_per_sec = if duration_secs <= 0.0 {
        0.0
    } else {
        total_updates as f64 / duration_secs
    };
    let places_per_sec = if duration_secs <= 0.0 {
        0.0
    } else {
        agg.place_count as f64 / duration_secs
    };

    let move_interval_p50_ms = percentile_f64(&move_intervals_ms, 0.50).unwrap_or(0.0);
    let move_interval_p80_ms = percentile_f64(&move_intervals_ms, 0.80).unwrap_or(0.0);
    let think_interval_p50_ms = percentile_f64(&think_intervals_ms, 0.50).unwrap_or(0.0);
    let think_interval_p80_ms = percentile_f64(&think_intervals_ms, 0.80).unwrap_or(0.0);

    let drag_durations_ms = drags
        .iter()
        .filter_map(|drag| drag.drag_duration_ms.map(|value| value as f64))
        .collect::<Vec<_>>();
    let drag_speeds_px_per_sec = drags
        .iter()
        .filter_map(|drag| {
            match (
                drag.path_distance_px.or(drag.distance_px),
                drag.drag_duration_ms,
            ) {
                (Some(distance), Some(duration_ms)) if duration_ms > 0.0 => {
                    Some((distance as f64) * 1000.0 / duration_ms as f64)
                }
                _ => None,
            }
        })
        .collect::<Vec<_>>();
    let drag_duration_p50_ms = percentile_f64(&drag_durations_ms, 0.50).unwrap_or(0.0);
    let drag_duration_p80_ms = percentile_f64(&drag_durations_ms, 0.80).unwrap_or(0.0);
    let drag_speed_p50_px_per_sec = percentile_f64(&drag_speeds_px_per_sec, 0.50).unwrap_or(0.0);
    let drag_speed_p80_px_per_sec = percentile_f64(&drag_speeds_px_per_sec, 0.80).unwrap_or(0.0);
    let movement_speed_avg_px_per_sec =
        mean_f64(drag_speeds_px_per_sec.iter().copied()).unwrap_or(0.0);

    let (piece_variety_avg_unique, piece_variety_p50_unique, piece_variety_p80_unique) =
        piece_variety_stats(&piece_action_ids, PIECE_VARIETY_WINDOW_ACTIONS);

    let inferred_piece_count = max_piece_seen.map(|value| value as usize + 1).unwrap_or(0);
    let (inferred_grid_cols, inferred_grid_rows) = infer_grid_dims(inferred_piece_count);
    let border_piece_ids = border_piece_ids(inferred_grid_cols, inferred_grid_rows);
    let legacy_border_done_100_secs = if border_piece_ids.is_empty() || agg.first_ts.is_none() {
        None
    } else {
        let mut done_at = i64::MIN;
        let mut complete = true;
        for piece_id in border_piece_ids {
            if let Some(ts) = last_applied_ts_by_piece.get(&(piece_id as u32)).copied() {
                done_at = done_at.max(ts);
            } else {
                complete = false;
                break;
            }
        }
        if complete {
            Some((done_at - agg.first_ts.unwrap_or(done_at)) as f64 / 1000.0)
        } else {
            None
        }
    };
    let legacy_completion_time_secs = if inferred_piece_count > 0 && agg.first_ts.is_some() {
        let mut done_at = i64::MIN;
        let mut complete = true;
        for piece_id in 0..inferred_piece_count {
            if let Some(ts) = last_applied_ts_by_piece.get(&(piece_id as u32)).copied() {
                done_at = done_at.max(ts);
            } else {
                complete = false;
                break;
            }
        }
        if complete {
            Some((done_at - agg.first_ts.unwrap_or(done_at)) as f64 / 1000.0)
        } else {
            None
        }
    } else {
        None
    };

    let (border_done_100_secs, border_metric_source) = if progress_samples > 0 {
        (
            border_done_progress_ts.map(|ts| (ts - agg.first_ts.unwrap_or(ts)) as f64 / 1000.0),
            "progress",
        )
    } else {
        (legacy_border_done_100_secs, "heuristic_touch")
    };
    let (completion_time_secs, completion_metric_source) = if progress_samples > 0 {
        (
            solved_progress_ts.map(|ts| (ts - agg.first_ts.unwrap_or(ts)) as f64 / 1000.0),
            "progress",
        )
    } else {
        (legacy_completion_time_secs, "heuristic_touch")
    };

    let short_drag_rate = if drags.is_empty() {
        0.0
    } else {
        drags.iter().filter(|drag| drag.updates <= 2).count() as f64 / drags.len() as f64
    };
    let same_piece_retry_rate = if drags.is_empty() {
        0.0
    } else {
        drags.iter().filter(|drag| drag.same_piece_retry).count() as f64 / drags.len() as f64
    };

    let first_ts = agg.first_ts.unwrap_or(0);
    let last_ts = agg.last_ts.unwrap_or(first_ts);
    let span_ms = (last_ts - first_ts).max(1) as f64;
    let mut early_drags = Vec::new();
    let mut late_drags = Vec::new();
    for drag in &drags {
        let progress = ((drag.start_ts - first_ts) as f64 / span_ms).clamp(0.0, 1.0);
        if progress <= 0.30 {
            early_drags.push(drag.clone());
        } else if progress >= 0.70 {
            late_drags.push(drag.clone());
        }
    }
    if early_drags.is_empty() {
        early_drags = drags.clone();
    }
    if late_drags.is_empty() {
        late_drags = drags.clone();
    }
    let early_updates_per_drag =
        mean_f64(early_drags.iter().map(|drag| drag.updates as f64)).unwrap_or(0.0);
    let late_updates_per_drag =
        mean_f64(late_drags.iter().map(|drag| drag.updates as f64)).unwrap_or(0.0);
    let early_short_drag_rate = if early_drags.is_empty() {
        0.0
    } else {
        early_drags.iter().filter(|drag| drag.updates <= 2).count() as f64
            / early_drags.len() as f64
    };
    let late_short_drag_rate = if late_drags.is_empty() {
        0.0
    } else {
        late_drags.iter().filter(|drag| drag.updates <= 2).count() as f64 / late_drags.len() as f64
    };
    let early_same_piece_retry_rate = if early_drags.is_empty() {
        0.0
    } else {
        early_drags
            .iter()
            .filter(|drag| drag.same_piece_retry)
            .count() as f64
            / early_drags.len() as f64
    };
    let late_same_piece_retry_rate = if late_drags.is_empty() {
        0.0
    } else {
        late_drags
            .iter()
            .filter(|drag| drag.same_piece_retry)
            .count() as f64
            / late_drags.len() as f64
    };

    Ok(StatsSummary {
        total_events: agg.total_events,
        duration_secs,
        events_per_sec: if duration_secs <= 0.0 {
            0.0
        } else {
            agg.total_events as f64 / duration_secs
        },
        selects_per_sec,
        updates_per_sec,
        places_per_sec,
        unique_clients: agg.unique_clients.len(),
        select_count: agg.select_count,
        move_count: agg.move_count,
        transform_count: agg.transform_count,
        place_count: agg.place_count,
        release_count: agg.release_count,
        avg_updates_per_drag,
        transform_share,
        select_conflict_rate,
        applied_rate,
        overlap_event_rate,
        move_interval_p50_ms,
        move_interval_p80_ms,
        think_interval_p50_ms,
        think_interval_p80_ms,
        drag_duration_p50_ms,
        drag_duration_p80_ms,
        drag_speed_p50_px_per_sec,
        drag_speed_p80_px_per_sec,
        movement_speed_avg_px_per_sec,
        short_drag_rate,
        same_piece_retry_rate,
        piece_variety_window_actions: PIECE_VARIETY_WINDOW_ACTIONS,
        piece_variety_avg_unique,
        piece_variety_p50_unique,
        piece_variety_p80_unique,
        inferred_piece_count,
        inferred_grid_cols,
        inferred_grid_rows,
        progress_samples,
        join_events,
        groups_start,
        groups_end,
        largest_group_peak,
        connected_edges_start,
        connected_edges_end,
        connected_edges_total,
        border_metric_source,
        completion_metric_source,
        border_done_100_secs,
        completion_time_secs,
        early_updates_per_drag,
        late_updates_per_drag,
        early_short_drag_rate,
        late_short_drag_rate,
        early_same_piece_retry_rate,
        late_same_piece_retry_rate,
        by_kind: agg.by_kind,
        by_outcome: agg.by_outcome,
    })
}

fn mean_f64(values: impl Iterator<Item = f64>) -> Option<f64> {
    let mut total = 0.0f64;
    let mut count = 0u64;
    for value in values {
        if !value.is_finite() {
            continue;
        }
        total += value;
        count = count.saturating_add(1);
    }
    if count == 0 {
        None
    } else {
        Some(total / count as f64)
    }
}

fn percentile_f64(values: &[f64], p: f64) -> Option<f64> {
    let mut values = values
        .iter()
        .copied()
        .filter(|value| value.is_finite() && *value >= 0.0)
        .collect::<Vec<_>>();
    if values.is_empty() {
        return None;
    }
    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let idx = ((values.len().saturating_sub(1)) as f64 * p.clamp(0.0, 1.0)).round() as usize;
    Some(values[idx])
}

fn action_piece_id(row: &RecordLine) -> Option<u32> {
    row.piece_id.or(row.anchor_id)
}

fn parse_reason_progress(reason: Option<&str>) -> Option<ReasonProgress> {
    let raw = reason?;
    let parsed: ReasonProgressPayload = serde_json::from_str(raw).ok()?;
    Some(ReasonProgress {
        groups_before: parsed.groups_before?,
        groups_after: parsed.groups_after?,
        largest_group_before: parsed.largest_group_before?,
        largest_group_after: parsed.largest_group_after?,
        connected_edges_before: parsed.connected_edges_before?,
        connected_edges_after: parsed.connected_edges_after?,
        total_edges: parsed.total_edges?,
        border_done_before: parsed.border_done_before?,
        border_done_after: parsed.border_done_after?,
        solved_before: parsed.solved_before?,
        solved_after: parsed.solved_after?,
    })
}

fn piece_variety_stats(piece_ids: &[u32], window: usize) -> (f64, f64, f64) {
    if piece_ids.is_empty() || window == 0 {
        return (0.0, 0.0, 0.0);
    }
    if piece_ids.len() <= window {
        let unique = piece_ids.iter().copied().collect::<HashSet<_>>().len() as f64;
        return (unique, unique, unique);
    }

    let mut unique_counts = Vec::<f64>::new();
    let mut counts = HashMap::<u32, u32>::new();
    for idx in 0..piece_ids.len() {
        let entry = counts.entry(piece_ids[idx]).or_insert(0);
        *entry = entry.saturating_add(1);
        if idx + 1 >= window {
            unique_counts.push(counts.len() as f64);
            let out_idx = idx + 1 - window;
            let out_piece = piece_ids[out_idx];
            if let Some(slot) = counts.get_mut(&out_piece) {
                *slot = slot.saturating_sub(1);
                if *slot == 0 {
                    counts.remove(&out_piece);
                }
            }
        }
    }

    let avg = mean_f64(unique_counts.iter().copied()).unwrap_or(0.0);
    let p50 = percentile_f64(&unique_counts, 0.50).unwrap_or(0.0);
    let p80 = percentile_f64(&unique_counts, 0.80).unwrap_or(0.0);
    (avg, p50, p80)
}

fn infer_grid_dims(total: usize) -> (usize, usize) {
    if total == 0 {
        return (0, 0);
    }
    let mut best_rows = 1usize;
    let mut best_cols = total;
    let mut best_diff = best_cols.saturating_sub(best_rows);
    let limit = (total as f64).sqrt().floor() as usize;
    for rows in 1..=limit.max(1) {
        if total % rows != 0 {
            continue;
        }
        let cols = total / rows;
        let diff = cols.abs_diff(rows);
        if diff < best_diff {
            best_diff = diff;
            best_rows = rows;
            best_cols = cols;
        }
    }
    if best_cols < best_rows {
        (best_rows, best_cols)
    } else {
        (best_cols, best_rows)
    }
}

fn border_piece_ids(cols: usize, rows: usize) -> Vec<usize> {
    if cols == 0 || rows == 0 {
        return Vec::new();
    }
    let mut border = Vec::new();
    for row in 0..rows {
        for col in 0..cols {
            if row == 0 || row + 1 == rows || col == 0 || col + 1 == cols {
                border.push(row * cols + col);
            }
        }
    }
    border
}

fn format_optional_secs(value: Option<f64>) -> String {
    match value {
        Some(value) => format!("{value:.3}"),
        None => "n/a".to_string(),
    }
}

fn format_optional_u32(value: Option<u32>) -> String {
    match value {
        Some(value) => value.to_string(),
        None => "n/a".to_string(),
    }
}

fn print_optional_compare_line(label: &str, baseline: Option<f64>, candidate: Option<f64>) {
    match (baseline, candidate) {
        (Some(base), Some(cand)) => {
            println!(
                "{label}: baseline={base:.3} candidate={cand:.3} delta={:+.3}",
                cand - base
            );
        }
        _ => {
            println!(
                "{label}: baseline={} candidate={}",
                format_optional_secs(baseline),
                format_optional_secs(candidate)
            );
        }
    }
}

fn print_optional_compare_line_u32(label: &str, baseline: Option<u32>, candidate: Option<u32>) {
    match (baseline, candidate) {
        (Some(base), Some(cand)) => {
            let delta = cand as i64 - base as i64;
            println!(
                "{label}: baseline={base} candidate={cand} delta={:+}",
                delta
            );
        }
        _ => {
            println!(
                "{label}: baseline={} candidate={}",
                format_optional_u32(baseline),
                format_optional_u32(candidate)
            );
        }
    }
}

fn print_summary(summary: &StatsSummary) {
    println!("total_events: {}", summary.total_events);
    println!("duration_secs: {:.3}", summary.duration_secs);
    println!("events_per_sec: {:.3}", summary.events_per_sec);
    println!("selects_per_sec: {:.3}", summary.selects_per_sec);
    println!("updates_per_sec: {:.3}", summary.updates_per_sec);
    println!("places_per_sec: {:.3}", summary.places_per_sec);
    println!("unique_clients: {}", summary.unique_clients);
    println!("select_count: {}", summary.select_count);
    println!("move_count: {}", summary.move_count);
    println!("transform_count: {}", summary.transform_count);
    println!("place_count: {}", summary.place_count);
    println!("release_count: {}", summary.release_count);
    println!("avg_updates_per_drag: {:.3}", summary.avg_updates_per_drag);
    println!("transform_share: {:.3}", summary.transform_share);
    println!("select_conflict_rate: {:.3}", summary.select_conflict_rate);
    println!("applied_rate: {:.3}", summary.applied_rate);
    println!("overlap_event_rate: {:.3}", summary.overlap_event_rate);
    println!("move_interval_p50_ms: {:.2}", summary.move_interval_p50_ms);
    println!("move_interval_p80_ms: {:.2}", summary.move_interval_p80_ms);
    println!(
        "think_interval_p50_ms: {:.2}",
        summary.think_interval_p50_ms
    );
    println!(
        "think_interval_p80_ms: {:.2}",
        summary.think_interval_p80_ms
    );
    println!("drag_duration_p50_ms: {:.2}", summary.drag_duration_p50_ms);
    println!("drag_duration_p80_ms: {:.2}", summary.drag_duration_p80_ms);
    println!(
        "drag_speed_p50_px_per_sec: {:.2}",
        summary.drag_speed_p50_px_per_sec
    );
    println!(
        "drag_speed_p80_px_per_sec: {:.2}",
        summary.drag_speed_p80_px_per_sec
    );
    println!(
        "movement_speed_avg_px_per_sec: {:.2}",
        summary.movement_speed_avg_px_per_sec
    );
    println!("short_drag_rate: {:.3}", summary.short_drag_rate);
    println!(
        "same_piece_retry_rate: {:.3}",
        summary.same_piece_retry_rate
    );
    println!(
        "piece_variety_window_actions: {}",
        summary.piece_variety_window_actions
    );
    println!(
        "piece_variety_avg_unique: {:.3}",
        summary.piece_variety_avg_unique
    );
    println!(
        "piece_variety_p50_unique: {:.3}",
        summary.piece_variety_p50_unique
    );
    println!(
        "piece_variety_p80_unique: {:.3}",
        summary.piece_variety_p80_unique
    );
    println!("inferred_piece_count: {}", summary.inferred_piece_count);
    println!(
        "inferred_grid: {}x{}",
        summary.inferred_grid_cols, summary.inferred_grid_rows
    );
    println!("progress_samples: {}", summary.progress_samples);
    println!("join_events: {}", summary.join_events);
    println!(
        "groups_start: {}",
        format_optional_u32(summary.groups_start)
    );
    println!("groups_end: {}", format_optional_u32(summary.groups_end));
    println!(
        "largest_group_peak: {}",
        format_optional_u32(summary.largest_group_peak)
    );
    println!(
        "connected_edges_start: {}",
        format_optional_u32(summary.connected_edges_start)
    );
    println!(
        "connected_edges_end: {}",
        format_optional_u32(summary.connected_edges_end)
    );
    println!(
        "connected_edges_total: {}",
        format_optional_u32(summary.connected_edges_total)
    );
    println!("border_metric_source: {}", summary.border_metric_source);
    println!(
        "completion_metric_source: {}",
        summary.completion_metric_source
    );
    println!(
        "border_done_100_secs: {}",
        format_optional_secs(summary.border_done_100_secs)
    );
    println!(
        "completion_time_secs: {}",
        format_optional_secs(summary.completion_time_secs)
    );
    println!(
        "early_updates_per_drag: {:.3}",
        summary.early_updates_per_drag
    );
    println!(
        "late_updates_per_drag: {:.3}",
        summary.late_updates_per_drag
    );
    println!(
        "early_short_drag_rate: {:.3}",
        summary.early_short_drag_rate
    );
    println!("late_short_drag_rate: {:.3}", summary.late_short_drag_rate);
    println!(
        "early_same_piece_retry_rate: {:.3}",
        summary.early_same_piece_retry_rate
    );
    println!(
        "late_same_piece_retry_rate: {:.3}",
        summary.late_same_piece_retry_rate
    );

    println!("by_kind:");
    for (kind, count) in sorted_counts(&summary.by_kind) {
        println!("  {kind}: {count}");
    }

    println!("by_outcome:");
    for (kind, count) in sorted_counts(&summary.by_outcome) {
        println!("  {kind}: {count}");
    }
}

fn print_compare(base: &StatsSummary, cand: &StatsSummary) {
    let similarity = similarity_score(base, cand);

    println!("similarity_score: {:.2}", similarity * 100.0);
    println!(
        "updates_per_sec: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.updates_per_sec,
        cand.updates_per_sec,
        cand.updates_per_sec - base.updates_per_sec
    );
    println!(
        "places_per_sec: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.places_per_sec,
        cand.places_per_sec,
        cand.places_per_sec - base.places_per_sec
    );
    println!(
        "events_per_sec: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.events_per_sec,
        cand.events_per_sec,
        cand.events_per_sec - base.events_per_sec
    );
    println!(
        "avg_updates_per_drag: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.avg_updates_per_drag,
        cand.avg_updates_per_drag,
        cand.avg_updates_per_drag - base.avg_updates_per_drag
    );
    println!(
        "move_interval_p50_ms: baseline={:.2} candidate={:.2} delta={:+.2}",
        base.move_interval_p50_ms,
        cand.move_interval_p50_ms,
        cand.move_interval_p50_ms - base.move_interval_p50_ms
    );
    println!(
        "drag_duration_p50_ms: baseline={:.2} candidate={:.2} delta={:+.2}",
        base.drag_duration_p50_ms,
        cand.drag_duration_p50_ms,
        cand.drag_duration_p50_ms - base.drag_duration_p50_ms
    );
    println!(
        "movement_speed_avg_px_per_sec: baseline={:.2} candidate={:.2} delta={:+.2}",
        base.movement_speed_avg_px_per_sec,
        cand.movement_speed_avg_px_per_sec,
        cand.movement_speed_avg_px_per_sec - base.movement_speed_avg_px_per_sec
    );
    println!(
        "piece_variety_avg_unique(window={}): baseline={:.3} candidate={:.3} delta={:+.3}",
        base.piece_variety_window_actions,
        base.piece_variety_avg_unique,
        cand.piece_variety_avg_unique,
        cand.piece_variety_avg_unique - base.piece_variety_avg_unique
    );
    println!(
        "piece_variety_p50_unique(window={}): baseline={:.3} candidate={:.3} delta={:+.3}",
        base.piece_variety_window_actions,
        base.piece_variety_p50_unique,
        cand.piece_variety_p50_unique,
        cand.piece_variety_p50_unique - base.piece_variety_p50_unique
    );
    println!(
        "piece_variety_p80_unique(window={}): baseline={:.3} candidate={:.3} delta={:+.3}",
        base.piece_variety_window_actions,
        base.piece_variety_p80_unique,
        cand.piece_variety_p80_unique,
        cand.piece_variety_p80_unique - base.piece_variety_p80_unique
    );
    println!(
        "progress_samples: baseline={} candidate={} delta={:+}",
        base.progress_samples,
        cand.progress_samples,
        cand.progress_samples as i64 - base.progress_samples as i64
    );
    println!(
        "join_events: baseline={} candidate={} delta={:+}",
        base.join_events,
        cand.join_events,
        cand.join_events as i64 - base.join_events as i64
    );
    print_optional_compare_line_u32("groups_start", base.groups_start, cand.groups_start);
    print_optional_compare_line_u32("groups_end", base.groups_end, cand.groups_end);
    print_optional_compare_line_u32(
        "largest_group_peak",
        base.largest_group_peak,
        cand.largest_group_peak,
    );
    print_optional_compare_line_u32(
        "connected_edges_start",
        base.connected_edges_start,
        cand.connected_edges_start,
    );
    print_optional_compare_line_u32(
        "connected_edges_end",
        base.connected_edges_end,
        cand.connected_edges_end,
    );
    print_optional_compare_line_u32(
        "connected_edges_total",
        base.connected_edges_total,
        cand.connected_edges_total,
    );
    println!(
        "border_metric_source: baseline={} candidate={}",
        base.border_metric_source, cand.border_metric_source
    );
    println!(
        "completion_metric_source: baseline={} candidate={}",
        base.completion_metric_source, cand.completion_metric_source
    );
    print_optional_compare_line(
        "border_done_100_secs",
        base.border_done_100_secs,
        cand.border_done_100_secs,
    );
    print_optional_compare_line(
        "completion_time_secs",
        base.completion_time_secs,
        cand.completion_time_secs,
    );
    println!(
        "short_drag_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.short_drag_rate,
        cand.short_drag_rate,
        cand.short_drag_rate - base.short_drag_rate
    );
    println!(
        "same_piece_retry_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.same_piece_retry_rate,
        cand.same_piece_retry_rate,
        cand.same_piece_retry_rate - base.same_piece_retry_rate
    );
    println!(
        "early_updates_per_drag: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.early_updates_per_drag,
        cand.early_updates_per_drag,
        cand.early_updates_per_drag - base.early_updates_per_drag
    );
    println!(
        "late_updates_per_drag: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.late_updates_per_drag,
        cand.late_updates_per_drag,
        cand.late_updates_per_drag - base.late_updates_per_drag
    );
    println!(
        "early_same_piece_retry_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.early_same_piece_retry_rate,
        cand.early_same_piece_retry_rate,
        cand.early_same_piece_retry_rate - base.early_same_piece_retry_rate
    );
    println!(
        "late_same_piece_retry_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.late_same_piece_retry_rate,
        cand.late_same_piece_retry_rate,
        cand.late_same_piece_retry_rate - base.late_same_piece_retry_rate
    );
    println!(
        "transform_share: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.transform_share,
        cand.transform_share,
        cand.transform_share - base.transform_share
    );
    println!(
        "select_conflict_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.select_conflict_rate,
        cand.select_conflict_rate,
        cand.select_conflict_rate - base.select_conflict_rate
    );
    println!(
        "applied_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.applied_rate,
        cand.applied_rate,
        cand.applied_rate - base.applied_rate
    );
    println!(
        "overlap_event_rate: baseline={:.3} candidate={:.3} delta={:+.3}",
        base.overlap_event_rate,
        cand.overlap_event_rate,
        cand.overlap_event_rate - base.overlap_event_rate
    );
}

fn similarity_score(base: &StatsSummary, cand: &StatsSummary) -> f64 {
    let mut components = vec![
        (base.events_per_sec, cand.events_per_sec, 0.10),
        (base.updates_per_sec, cand.updates_per_sec, 0.13),
        (base.places_per_sec, cand.places_per_sec, 0.07),
        (base.avg_updates_per_drag, cand.avg_updates_per_drag, 0.10),
        (base.move_interval_p50_ms, cand.move_interval_p50_ms, 0.10),
        (
            base.movement_speed_avg_px_per_sec,
            cand.movement_speed_avg_px_per_sec,
            0.08,
        ),
        (
            base.piece_variety_avg_unique,
            cand.piece_variety_avg_unique,
            0.06,
        ),
        (base.short_drag_rate, cand.short_drag_rate, 0.07),
        (base.same_piece_retry_rate, cand.same_piece_retry_rate, 0.07),
        (
            base.early_updates_per_drag,
            cand.early_updates_per_drag,
            0.05,
        ),
        (base.late_updates_per_drag, cand.late_updates_per_drag, 0.05),
        (
            base.early_same_piece_retry_rate,
            cand.early_same_piece_retry_rate,
            0.05,
        ),
        (
            base.late_same_piece_retry_rate,
            cand.late_same_piece_retry_rate,
            0.05,
        ),
    ];

    if let (Some(base), Some(cand)) = (base.border_done_100_secs, cand.border_done_100_secs) {
        components.push((base, cand, 0.06));
    }
    if let (Some(base), Some(cand)) = (base.completion_time_secs, cand.completion_time_secs) {
        components.push((base, cand, 0.06));
    }
    if base.progress_samples > 0 && cand.progress_samples > 0 {
        components.push((base.join_events as f64, cand.join_events as f64, 0.05));
        if let (Some(base_groups), Some(cand_groups)) = (base.groups_end, cand.groups_end) {
            components.push((base_groups as f64, cand_groups as f64, 0.06));
        }
        if let (Some(base_edges), Some(cand_edges)) =
            (base.connected_edges_end, cand.connected_edges_end)
        {
            components.push((base_edges as f64, cand_edges as f64, 0.06));
        }
    }

    let mut distance = 0.0;
    let mut total_weight = 0.0;
    for (a, b, w) in components {
        let denom = a.abs().max(b.abs()).max(1e-6);
        distance += ((a - b).abs() / denom).min(1.0) * w;
        total_weight += w;
    }
    if total_weight <= 0.0 {
        return 1.0;
    }
    (1.0 - (distance / total_weight)).clamp(0.0, 1.0)
}

fn sorted_counts(map: &HashMap<String, u64>) -> Vec<(String, u64)> {
    let mut pairs = map
        .iter()
        .map(|(k, v)| (k.clone(), *v))
        .collect::<Vec<(String, u64)>>();
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    pairs
}

async fn connect_ws(
    base_url: &str,
    room_id: &str,
    admin_token: Option<&str>,
) -> Result<(WsWrite, WsRead), Box<dyn std::error::Error>> {
    let room_url = build_join_url(base_url, room_id)?;
    let auth_protocol = build_auth_protocol(room_id, admin_token)?;

    let mut request = room_url
        .as_str()
        .into_client_request()
        .map_err(|err| err_msg(format!("failed to build websocket request: {err}")))?;
    request
        .headers_mut()
        .append("Sec-WebSocket-Protocol", auth_protocol.parse()?);

    let (ws, _response) = connect_async(request)
        .await
        .map_err(|err| err_msg(format!("failed to connect to {room_url}: {err}")))?;

    Ok(ws.split())
}

async fn send_admin_msg(
    write: &mut WsWrite,
    msg: AdminMsg,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(payload) = encode(&msg) else {
        return Err(err_msg("failed to encode admin message"));
    };
    write.send(Message::Binary(payload.into())).await?;
    Ok(())
}

async fn send_client_msg(
    write: &mut WsWrite,
    msg: ClientMsg,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(payload) = encode(&msg) else {
        return Err(err_msg("failed to encode client message"));
    };
    write.send(Message::Binary(payload.into())).await?;
    Ok(())
}

async fn recv_server_msg(
    read: &mut WsRead,
) -> Result<Option<ServerMsg>, Box<dyn std::error::Error>> {
    while let Some(next) = read.next().await {
        let message = next?;
        match message {
            Message::Binary(bytes) => {
                let msg = decode_result::<ServerMsg>(&bytes).map_err(|err| {
                    err_msg(format!(
                        "received undecodable binary message ({} bytes): {err}",
                        bytes.len()
                    ))
                })?;
                return Ok(Some(msg));
            }
            Message::Close(_) => return Ok(None),
            Message::Text(text) => {
                return Err(err_msg(format!("server sent text message: {text}")));
            }
            _ => {}
        }
    }
    Ok(None)
}

async fn recv_server_msg_timeout(
    read: &mut WsRead,
    dur: Duration,
) -> Result<Option<ServerMsg>, Box<dyn std::error::Error>> {
    match timeout(dur, recv_server_msg(read)).await {
        Ok(value) => value,
        Err(_) => Ok(None),
    }
}

fn generate_room_id() -> String {
    let mut rng = rand::rng();
    let alphabet = ROOM_ID_ALPHABET.as_bytes();
    let mut id = String::with_capacity(ROOM_ID_LEN);
    for _ in 0..ROOM_ID_LEN {
        let idx = rng.random_range(0..alphabet.len());
        id.push(alphabet[idx] as char);
    }
    id
}

fn build_admin_url(base_url: &str, room_id: &str, token: &str) -> Result<Url, url::ParseError> {
    let _ = token;
    build_join_url(base_url, room_id)
}

fn build_join_url(base_url: &str, room_id: &str) -> Result<Url, url::ParseError> {
    let mut url = Url::parse(base_url)?;
    let base_path = url.path().trim_end_matches('/');
    let path = format!("{}/{}", base_path, room_id);
    url.set_path(&path);
    url.set_query(None);
    Ok(url)
}

fn parse_seed_arg(raw: &str) -> Result<u32, Box<dyn std::error::Error>> {
    let trimmed = raw.trim();
    let value = if let Some(hex) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        u32::from_str_radix(hex, 16)?
    } else {
        trimmed.parse::<u32>()?
    };
    Ok(value)
}

fn err_msg(msg: impl Into<String>) -> Box<dyn std::error::Error> {
    std::io::Error::new(std::io::ErrorKind::Other, msg.into()).into()
}

fn install_crypto_provider() -> Result<(), Box<dyn std::error::Error>> {
    if rustls::crypto::CryptoProvider::get_default().is_some() {
        return Ok(());
    }
    if rustls::crypto::ring::default_provider()
        .install_default()
        .is_err()
    {
        // Another thread already installed a provider; continue.
    }
    Ok(())
}

#[derive(Serialize)]
struct AuthPayload {
    v: u8,
    client_id: String,
    ts: i64,
    nonce: String,
    pubkey: String,
    sig: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    admin_token: Option<String>,
}

fn build_auth_protocol(
    room_id: &str,
    admin_token: Option<&str>,
) -> Result<String, Box<dyn std::error::Error>> {
    const AUTH_PROTOCOL_PREFIX: &str = "heddo-auth-v1.";
    const AUTH_CONTEXT: &str = "heddobureika-auth-v1";

    let signing_key = SigningKey::random(&mut OsRng);
    let verifying_key = signing_key.verifying_key();
    let public_key_sec1 = verifying_key.to_encoded_point(false).as_bytes().to_vec();
    let client_id = derive_client_id(&public_key_sec1);
    let ts = now_ms();
    let nonce = random_nonce();
    let message = format!("{room_id}\n{ts}\n{nonce}\n{AUTH_CONTEXT}");
    let signature: Signature = signing_key.sign(message.as_bytes());
    let payload = AuthPayload {
        v: 1,
        client_id: client_id.to_string(),
        ts,
        nonce,
        pubkey: URL_SAFE_NO_PAD.encode(&public_key_sec1),
        sig: URL_SAFE_NO_PAD.encode(signature.to_bytes()),
        admin_token: admin_token.map(|token| token.trim().to_string()),
    };
    let payload_bytes = serde_json::to_vec(&payload)?;
    Ok(format!(
        "{AUTH_PROTOCOL_PREFIX}{}",
        URL_SAFE_NO_PAD.encode(payload_bytes)
    ))
}

fn derive_client_id(pubkey_sec1: &[u8]) -> u64 {
    let digest = Sha256::digest(pubkey_sec1);
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&digest[..8]);
    u64::from_be_bytes(bytes)
}

fn random_nonce() -> String {
    let mut bytes = [0u8; 12];
    OsRng.fill_bytes(&mut bytes);
    URL_SAFE_NO_PAD.encode(bytes)
}

fn now_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

fn recorded_kind_name(kind: RecordedCommandKind) -> &'static str {
    match kind {
        RecordedCommandKind::Init => "Init",
        RecordedCommandKind::AssetRequest => "AssetRequest",
        RecordedCommandKind::Select => "Select",
        RecordedCommandKind::Move => "Move",
        RecordedCommandKind::Transform => "Transform",
        RecordedCommandKind::Rotate => "Rotate",
        RecordedCommandKind::Place => "Place",
        RecordedCommandKind::Flip => "Flip",
        RecordedCommandKind::Release => "Release",
        RecordedCommandKind::Ping => "Ping",
    }
}

fn recorded_outcome_name(outcome: RecordedCommandOutcome) -> &'static str {
    match outcome {
        RecordedCommandOutcome::Applied => "Applied",
        RecordedCommandOutcome::AcceptedNoStateChange => "AcceptedNoStateChange",
        RecordedCommandOutcome::Ignored => "Ignored",
        RecordedCommandOutcome::Rejected => "Rejected",
        RecordedCommandOutcome::HandlerError => "HandlerError",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recorded_kind_names_are_stable() {
        assert_eq!(recorded_kind_name(RecordedCommandKind::Select), "Select");
        assert_eq!(
            recorded_kind_name(RecordedCommandKind::Transform),
            "Transform"
        );
    }

    #[test]
    fn similarity_score_identical_is_one() {
        let summary = StatsSummary {
            total_events: 100,
            duration_secs: 10.0,
            events_per_sec: 10.0,
            selects_per_sec: 2.0,
            updates_per_sec: 5.0,
            places_per_sec: 2.0,
            unique_clients: 2,
            select_count: 20,
            move_count: 40,
            transform_count: 10,
            place_count: 20,
            release_count: 5,
            avg_updates_per_drag: 2.5,
            transform_share: 0.2,
            select_conflict_rate: 0.1,
            applied_rate: 0.9,
            overlap_event_rate: 0.2,
            move_interval_p50_ms: 12.0,
            move_interval_p80_ms: 20.0,
            think_interval_p50_ms: 550.0,
            think_interval_p80_ms: 1200.0,
            drag_duration_p50_ms: 620.0,
            drag_duration_p80_ms: 940.0,
            drag_speed_p50_px_per_sec: 180.0,
            drag_speed_p80_px_per_sec: 290.0,
            movement_speed_avg_px_per_sec: 210.0,
            short_drag_rate: 0.40,
            same_piece_retry_rate: 0.35,
            piece_variety_window_actions: 100,
            piece_variety_avg_unique: 12.0,
            piece_variety_p50_unique: 11.0,
            piece_variety_p80_unique: 16.0,
            inferred_piece_count: 150,
            inferred_grid_cols: 15,
            inferred_grid_rows: 10,
            progress_samples: 100,
            join_events: 12,
            groups_start: Some(150),
            groups_end: Some(1),
            largest_group_peak: Some(150),
            connected_edges_start: Some(0),
            connected_edges_end: Some(280),
            connected_edges_total: Some(280),
            border_metric_source: "progress",
            completion_metric_source: "progress",
            border_done_100_secs: Some(400.0),
            completion_time_secs: Some(800.0),
            early_updates_per_drag: 20.0,
            late_updates_per_drag: 34.0,
            early_short_drag_rate: 0.52,
            late_short_drag_rate: 0.46,
            early_same_piece_retry_rate: 0.30,
            late_same_piece_retry_rate: 0.41,
            by_kind: HashMap::new(),
            by_outcome: HashMap::new(),
        };
        assert!((similarity_score(&summary, &summary) - 1.0).abs() < 1e-9);
    }
}
