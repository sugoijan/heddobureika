use clap::{Parser, Subcommand};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::codec::{decode, encode};
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

                let msg = AdminMsg::Create { persistence };
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
