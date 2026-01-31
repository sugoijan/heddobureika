use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gloo::timers::future::TimeoutFuture;
use js_sys::{Array, Math, Uint8Array};
use wasm_bindgen::JsValue;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{BinaryType, CloseEvent, ErrorEvent, Event, MessageEvent, WebSocket};

use heddobureika_core::{decode, encode, AdminMsg, ClientMsg, ServerMsg};

#[allow(dead_code)]
pub(crate) struct WsHandlers {
    onopen: Closure<dyn FnMut(Event)>,
    onmessage: Closure<dyn FnMut(MessageEvent)>,
    onerror: Closure<dyn FnMut(ErrorEvent)>,
    onclose: Closure<dyn FnMut(Event)>,
}

#[derive(Clone)]
pub(crate) struct MultiplayerSyncAdapter {
    ws: Rc<RefCell<Option<WebSocket>>>,
    handlers: Rc<RefCell<Option<WsHandlers>>>,
    closing: Rc<Cell<bool>>,
}

const WS_DELAY_IN_KEY: &str = "heddobureika.debug.ws_in_ms";
const WS_DELAY_OUT_KEY: &str = "heddobureika.debug.ws_out_ms";
const WS_DELAY_JITTER_KEY: &str = "heddobureika.debug.ws_jitter_ms";

#[derive(Clone, Copy)]
struct WsDelayConfig {
    inbound_ms: u32,
    outbound_ms: u32,
    jitter_ms: u32,
}

fn read_storage_u32(key: &str) -> Option<u32> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(key).ok()??;
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    trimmed.parse::<u32>().ok()
}

fn load_ws_delay_config() -> WsDelayConfig {
    WsDelayConfig {
        inbound_ms: read_storage_u32(WS_DELAY_IN_KEY).unwrap_or(0),
        outbound_ms: read_storage_u32(WS_DELAY_OUT_KEY).unwrap_or(0),
        jitter_ms: read_storage_u32(WS_DELAY_JITTER_KEY).unwrap_or(0),
    }
}

fn compute_delay_ms(base: u32, jitter: u32) -> u32 {
    if base == 0 && jitter == 0 {
        return 0;
    }
    let extra = if jitter == 0 {
        0
    } else {
        (Math::random() * jitter as f64).round() as u32
    };
    base.saturating_add(extra)
}

fn inbound_delay_ms() -> u32 {
    let config = load_ws_delay_config();
    compute_delay_ms(config.inbound_ms, config.jitter_ms)
}

fn outbound_delay_ms() -> u32 {
    let config = load_ws_delay_config();
    compute_delay_ms(config.outbound_ms, config.jitter_ms)
}

fn send_bytes_with_delay(ws: &WebSocket, bytes: Vec<u8>) {
    let delay = outbound_delay_ms();
    if delay == 0 {
        let _ = ws.send_with_u8_array(&bytes);
        return;
    }
    let ws = ws.clone();
    spawn_local(async move {
        TimeoutFuture::new(delay).await;
        if ws.ready_state() == WebSocket::OPEN {
            let _ = ws.send_with_u8_array(&bytes);
        }
    });
}

impl MultiplayerSyncAdapter {
    pub(crate) fn new() -> Self {
        Self {
            ws: Rc::new(RefCell::new(None)),
            handlers: Rc::new(RefCell::new(None)),
            closing: Rc::new(Cell::new(false)),
        }
    }

    pub(crate) fn connect(
        &mut self,
        url: &str,
        on_server_msg: Rc<dyn Fn(ServerMsg)>,
        on_fail: Rc<dyn Fn()>,
    ) {
        self.connect_with_open(url, on_server_msg, on_fail, None, None);
    }

    pub(crate) fn connect_with_open(
        &mut self,
        url: &str,
        on_server_msg: Rc<dyn Fn(ServerMsg)>,
        on_fail: Rc<dyn Fn()>,
        on_open: Option<Rc<dyn Fn()>>,
        protocols: Option<Vec<String>>,
    ) {
        self.disconnect();
        let closing = Rc::new(Cell::new(false));
        self.closing = closing.clone();

        let url = url.trim();
        if url.is_empty() {
            return;
        }

        let ws = match protocols {
            Some(protocols) if !protocols.is_empty() => {
                let list = Array::new();
                for protocol in protocols {
                    list.push(&JsValue::from_str(&protocol));
                }
                WebSocket::new_with_str_sequence(url, &list.into())
            }
            _ => WebSocket::new(url),
        };
        let ws = match ws {
            Ok(ws) => ws,
            Err(_) => {
                gloo::console::warn!("failed to open websocket", url);
                on_fail();
                return;
            }
        };
        ws.set_binary_type(BinaryType::Arraybuffer);
        *self.ws.borrow_mut() = Some(ws.clone());

        let opened = Rc::new(Cell::new(false));
        let onopen = {
            let opened = opened.clone();
            let url = url.to_string();
            let on_open = on_open.clone();
            Closure::wrap(Box::new(move |_event: Event| {
                opened.set(true);
                gloo::console::log!("websocket connected", url.clone());
                if let Some(on_open) = on_open.as_ref() {
                    on_open();
                }
            }) as Box<dyn FnMut(Event)>)
        };
        let onmessage = {
            let on_server_msg = on_server_msg.clone();
            Closure::wrap(Box::new(move |event: MessageEvent| {
                let data = event.data();
                let Ok(buffer) = data.dyn_into::<js_sys::ArrayBuffer>() else {
                    return;
                };
                let bytes = Uint8Array::new(&buffer).to_vec();
                let on_server_msg = on_server_msg.clone();
                let delay = inbound_delay_ms();
                if delay == 0 {
                    if let Some(msg) = decode::<ServerMsg>(&bytes) {
                        on_server_msg(msg);
                    }
                    return;
                }
                spawn_local(async move {
                    TimeoutFuture::new(delay).await;
                    if let Some(msg) = decode::<ServerMsg>(&bytes) {
                        on_server_msg(msg);
                    }
                });
            }) as Box<dyn FnMut(MessageEvent)>)
        };
        let onerror = {
            let url = url.to_string();
            Closure::wrap(Box::new(move |_event: ErrorEvent| {
                gloo::console::warn!("websocket error", url.clone());
            }) as Box<dyn FnMut(ErrorEvent)>)
        };
        let onclose = {
            let ws_ref = self.ws.clone();
            let handlers_ref = self.handlers.clone();
            let opened = opened.clone();
            let url = url.to_string();
            let on_fail = on_fail.clone();
            let closing = closing.clone();
            Closure::wrap(Box::new(move |event: Event| {
                ws_ref.borrow_mut().take();
                handlers_ref.borrow_mut().take();
                if closing.get() {
                    return;
                }
                if !opened.get() {
                    gloo::console::warn!(
                        "websocket failed to connect (room may be invalid)",
                        url.clone()
                    );
                    on_fail();
                    return;
                }
                if let Some(close) = event.dyn_ref::<CloseEvent>() {
                    let reason = close.reason();
                    if reason.is_empty() {
                        gloo::console::log!("websocket closed", url.clone(), close.code());
                    } else {
                        gloo::console::log!(
                            "websocket closed",
                            url.clone(),
                            close.code(),
                            reason
                        );
                    }
                } else {
                    gloo::console::log!("websocket closed", url.clone());
                }
                on_fail();
            }) as Box<dyn FnMut(Event)>)
        };

        ws.set_onopen(Some(onopen.as_ref().unchecked_ref()));
        ws.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
        ws.set_onerror(Some(onerror.as_ref().unchecked_ref()));
        ws.set_onclose(Some(onclose.as_ref().unchecked_ref()));

        *self.handlers.borrow_mut() = Some(WsHandlers {
            onopen,
            onmessage,
            onerror,
            onclose,
        });
    }

    pub(crate) fn send(&self, msg: ClientMsg) {
        let ws = {
            let ws_guard = self.ws.borrow();
            let Some(ws) = ws_guard.as_ref() else {
                return;
            };
            ws.clone()
        };
        if ws.ready_state() != WebSocket::OPEN {
            return;
        }
        if let Some(bytes) = encode(&msg) {
            send_bytes_with_delay(&ws, bytes);
        }
    }

    pub(crate) fn send_admin(&self, msg: AdminMsg) {
        let ws = {
            let ws_guard = self.ws.borrow();
            let Some(ws) = ws_guard.as_ref() else {
                return;
            };
            ws.clone()
        };
        if ws.ready_state() != WebSocket::OPEN {
            return;
        }
        if let Some(bytes) = encode(&msg) {
            send_bytes_with_delay(&ws, bytes);
        }
    }

    pub(crate) fn disconnect(&mut self) {
        self.closing.set(true);
        self.handlers.borrow_mut().take();
        if let Some(ws) = self.ws.borrow_mut().take() {
            let _ = ws.close();
        }
    }
}

impl Default for MultiplayerSyncAdapter {
    fn default() -> Self {
        Self::new()
    }
}
