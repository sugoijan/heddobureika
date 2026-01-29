use clap::{Parser, Subcommand};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::codec::{decode_result, encode};
use heddobureika_core::catalog::{puzzle_by_slug, DEFAULT_PUZZLE_SLUG, PUZZLE_CATALOG};
use heddobureika_core::protocol::{AdminMsg, RoomPersistence, ServerMsg};
use heddobureika_core::room_id::{ROOM_ID_ALPHABET, ROOM_ID_LEN, RoomId};
use rand::Rng;
use tokio::time::{timeout, Duration};
use tokio_tungstenite::tungstenite::Message;
use url::Url;

#[derive(Parser)]
#[command(name = "heddobureika-cli", version, about = "Admin tools for heddobureika rooms")]
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
        #[arg(long, env = "ROOM_WS_BASE_URL", default_value = "ws://localhost:8787/ws")]
        base_url: String,
        #[arg(long, env = "ROOM_ADMIN_TOKEN")]
        admin_token: String,
        #[arg(long, default_value = DEFAULT_PUZZLE_SLUG)]
        puzzle: String,
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
                pieces,
                seed,
                room_id,
                best_effort,
                no_connect,
            } => {
                let room_id = match room_id {
                    Some(id) => RoomId::parse(&id)?.to_string(),
                    None => generate_room_id(),
                };
                let persistence = if best_effort {
                    RoomPersistence::BestEffort
                } else {
                    RoomPersistence::Durable
                };
                if puzzle_by_slug(&puzzle).is_none() {
                    eprintln!("unknown puzzle: {puzzle}");
                    eprintln!("available puzzles:");
                    for entry in PUZZLE_CATALOG {
                        eprintln!("  {} ({})", entry.slug, entry.label);
                    }
                    return Err(err_msg(format!("unknown puzzle: {puzzle}")));
                }
                let seed = match seed.as_deref() {
                    Some(raw) => Some(parse_seed_arg(raw)?),
                    None => None,
                };

                let admin_url = build_admin_url(&base_url, &room_id, &admin_token)?;
                let join_url = build_join_url(&base_url, &room_id)?;

                if no_connect {
                    println!("room_id: {room_id}");
                    println!("join_url: {join_url}");
                    println!("admin_url: {admin_url}");
                    return Ok(());
                }

                let (ws, _response) = tokio_tungstenite::connect_async(admin_url.as_str())
                    .await
                    .map_err(|err| format!("failed to connect to {admin_url}: {err}"))?;
                let (mut write, mut read) = ws.split();

                let msg = AdminMsg::Create {
                    persistence,
                    puzzle,
                    pieces,
                    seed,
                };
                let Some(payload) = encode(&msg) else {
                    return Err(err_msg("failed to encode admin create message"));
                };
                write.send(Message::Binary(payload.into())).await?;

                let ack_result = timeout(Duration::from_secs(30), async {
                    while let Some(message) = read.next().await {
                        let message = match message {
                            Ok(message) => message,
                            Err(err) => {
                                return Err(format!("websocket error: {err}"));
                            }
                        };
                        match message {
                            Message::Binary(bytes) => {
                                println!("message has {} bytes", bytes.len());
                                let msg = match decode_result::<ServerMsg>(&bytes) {
                                    Ok(msg) => msg,
                                    Err(err) => {
                                        eprintln!(
                                            "warning: received undecodable binary message ({} bytes): {err}; waiting for admin ack...",
                                            bytes.len()
                                        );
                                        continue;
                                    }
                                };
                                match msg {
                                    ServerMsg::AdminAck { .. } => return Ok(()),
                                    ServerMsg::Error { code, message } => {
                                        return Err(format!("server error {code}: {message}"));
                                    }
                                    _ => {}
                                }
                            }
                            Message::Text(text) => {
                                return Err(format!("server text message: {text}"));
                            }
                            Message::Close(frame) => {
                                return Err(format!("server closed connection: {frame:?}"));
                            }
                            _ => {}
                        }
                    }
                    Err("server closed before acknowledging admin create".to_string())
                })
                .await;

                match ack_result {
                    Ok(Ok(())) => {}
                    Ok(Err(err)) => return Err(err_msg(err)),
                    Err(_) => return Err(err_msg("timed out waiting for admin create ack")),
                }

                println!("room_id: {room_id}");
                println!("join_url: {join_url}");
            }
        },
    }

    Ok(())
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
    let mut url = Url::parse(base_url)?;
    let base_path = url.path().trim_end_matches('/');
    let path = format!("{}/{}", base_path, room_id);
    url.set_path(&path);
    url.query_pairs_mut().append_pair("admin_token", token);
    Ok(url)
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
    let value = if let Some(hex) = trimmed.strip_prefix("0x").or_else(|| trimmed.strip_prefix("0X"))
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
