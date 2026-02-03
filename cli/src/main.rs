use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use clap::{Parser, Subcommand};
use futures_util::{SinkExt, StreamExt};
use heddobureika_core::codec::{decode_result, encode};
use heddobureika_core::catalog::{puzzle_by_slug, DEFAULT_PUZZLE_SLUG, PUZZLE_CATALOG};
use heddobureika_core::protocol::{AdminMsg, PuzzleSpec, RoomPersistence, ServerMsg};
use heddobureika_core::{PuzzleImageRef, ASSET_CHUNK_BYTES, PRIVATE_UPLOAD_MAX_BYTES};
use mime_guess::MimeGuess;
use heddobureika_core::room_id::{ROOM_ID_ALPHABET, ROOM_ID_LEN, RoomId};
use p256::ecdsa::{signature::Signer, Signature, SigningKey};
use rand::Rng;
use rand_core::{OsRng, RngCore};
use serde::Serialize;
use sha2::{Digest, Sha256};
use tokio::time::{timeout, Duration};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::tungstenite::client::IntoClientRequest;
use url::Url;
use std::path::PathBuf;

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
}

struct UploadPlan {
    path: PathBuf,
    pieces: Option<u32>,
    seed: Option<u32>,
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
                let room_id = match room_id {
                    Some(id) => RoomId::parse(&id)?.to_string(),
                    None => generate_room_id(),
                };
                let persistence = if best_effort {
                    RoomPersistence::BestEffort
                } else {
                    RoomPersistence::Durable
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
                            image_ref: PuzzleImageRef::BuiltIn { slug: puzzle.clone() },
                            pieces: None,
                            seed: None,
                        },
                        Some(UploadPlan {
                            path,
                            pieces,
                            seed,
                        }),
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
                            image_ref: PuzzleImageRef::BuiltIn { slug: puzzle.clone() },
                            pieces,
                            seed,
                        },
                        None,
                    )
                };

                let admin_url = build_admin_url(&base_url, &room_id, &admin_token)?;
                let join_url = build_join_url(&base_url, &room_id)?;

                if no_connect {
                    println!("room_id: {room_id}");
                    println!("join_url: {join_url}");
                    println!("admin_url: {admin_url}");
                    return Ok(());
                }

                let auth_protocol = build_auth_protocol(&room_id, &admin_token)?;
                let mut request = admin_url
                    .as_str()
                    .into_client_request()
                    .map_err(|err| format!("failed to build websocket request: {err}"))?;
                request
                    .headers_mut()
                    .append("Sec-WebSocket-Protocol", auth_protocol.parse()?);

                let (ws, _response) = tokio_tungstenite::connect_async(request)
                    .await
                    .map_err(|err| format!("failed to connect to {admin_url}: {err}"))?;
                let (mut write, mut read) = ws.split();

                let msg = AdminMsg::Create {
                    persistence,
                    puzzle: create_spec,
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

                if let Some(plan) = upload_plan {
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
                    let begin = AdminMsg::UploadPrivateBegin {
                        mime,
                        size: bytes.len() as u32,
                    };
                    let Some(begin_payload) = encode(&begin) else {
                        return Err(err_msg("failed to encode upload begin message"));
                    };
                    write.send(Message::Binary(begin_payload.into())).await?;
                    for chunk in bytes.chunks(ASSET_CHUNK_BYTES) {
                        let msg = AdminMsg::UploadPrivateChunk {
                            bytes: chunk.to_vec(),
                        };
                        let Some(payload) = encode(&msg) else {
                            return Err(err_msg("failed to encode upload chunk"));
                        };
                        write.send(Message::Binary(payload.into())).await?;
                    }
                    let end = AdminMsg::UploadPrivateEnd {
                        pieces: plan.pieces,
                        seed: plan.seed,
                    };
                    let Some(end_payload) = encode(&end) else {
                        return Err(err_msg("failed to encode upload end message"));
                    };
                    write.send(Message::Binary(end_payload.into())).await?;

                    let upload_result = timeout(Duration::from_secs(60), async {
                        let mut got_ack = false;
                        let mut got_state = false;
                        while let Some(message) = read.next().await {
                            let message = match message {
                                Ok(message) => message,
                                Err(err) => return Err(format!("websocket error: {err}")),
                            };
                            match message {
                                Message::Binary(bytes) => {
                                    let msg = match decode_result::<ServerMsg>(&bytes) {
                                        Ok(msg) => msg,
                                        Err(err) => {
                                            eprintln!(
                                                "warning: received undecodable binary message ({} bytes): {err}; waiting for upload ack...",
                                                bytes.len()
                                            );
                                            continue;
                                        }
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
                        Err("server closed before acknowledging upload".to_string())
                    })
                    .await;

                    match upload_result {
                        Ok(Ok(())) => {}
                        Ok(Err(err)) => return Err(err_msg(err)),
                        Err(_) => return Err(err_msg("timed out waiting for upload ack")),
                    }
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

#[derive(Serialize)]
struct AuthPayload {
    v: u8,
    client_id: String,
    ts: i64,
    nonce: String,
    pubkey: String,
    sig: String,
    admin_token: String,
}

fn build_auth_protocol(room_id: &str, admin_token: &str) -> Result<String, Box<dyn std::error::Error>> {
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
        admin_token: admin_token.trim().to_string(),
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
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}
