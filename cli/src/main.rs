use clap::{Parser, Subcommand};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::codec::{decode, encode};
use heddobureika_core::catalog::{puzzle_by_slug, DEFAULT_PUZZLE_SLUG, PUZZLE_CATALOG};
use heddobureika_core::protocol::{AdminMsg, RoomPersistence, ServerMsg};
use heddobureika_core::room_id::{ROOM_ID_ALPHABET, ROOM_ID_LEN, RoomId};
use rand::Rng;
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
                    return Ok(());
                }
                let seed = match seed.as_deref() {
                    Some(raw) => Some(parse_seed_arg(raw)?),
                    None => None,
                };

                let admin_url = build_admin_url(&base_url, &room_id, &admin_token)?;
                let join_url = build_join_url(&base_url, &room_id)?;

                println!("room_id: {room_id}");
                println!("join_url: {join_url}");

                if no_connect {
                    println!("admin_url: {admin_url}");
                    return Ok(());
                }

                let (ws, _response) = tokio_tungstenite::connect_async(admin_url.as_str()).await?;
                let (mut write, mut read) = ws.split();

                let msg = AdminMsg::Create {
                    persistence,
                    puzzle,
                    pieces,
                    seed,
                };
                if let Some(payload) = encode(&msg) {
                    write.send(Message::Binary(payload.into())).await?;
                }

                if let Some(message) = read.next().await {
                    match message? {
                        Message::Text(text) => println!("server: {text}"),
                        Message::Binary(bytes) => {
                            if let Some(msg) = decode::<ServerMsg>(&bytes) {
                                println!("server: {:?}", msg);
                            }
                        }
                        Message::Close(frame) => println!("server closed: {frame:?}"),
                        _ => {}
                    }
                }
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
