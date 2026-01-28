use std::cell::{Cell, RefCell};
use std::rc::Rc;

use js_sys::Uint8Array;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::{BinaryType, CloseEvent, ErrorEvent, Event, MessageEvent, WebSocket};

use heddobureika_core::{decode, encode, ClientMsg, ServerMsg};

#[allow(dead_code)]
pub(crate) struct WsHandlers {
    onopen: Closure<dyn FnMut(Event)>,
    onmessage: Closure<dyn FnMut(MessageEvent)>,
    onerror: Closure<dyn FnMut(ErrorEvent)>,
    onclose: Closure<dyn FnMut(Event)>,
}

pub(crate) struct MultiplayerSyncAdapter {
    ws: Rc<RefCell<Option<WebSocket>>>,
    handlers: Rc<RefCell<Option<WsHandlers>>>,
    closing: Rc<Cell<bool>>,
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
        self.disconnect();
        let closing = Rc::new(Cell::new(false));
        self.closing = closing.clone();

        let url = url.trim();
        if url.is_empty() {
            return;
        }

        let ws = match WebSocket::new(url) {
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
            Closure::wrap(Box::new(move |_event: Event| {
                opened.set(true);
                gloo::console::log!("websocket connected", url.clone());
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
                if let Some(msg) = decode::<ServerMsg>(&bytes) {
                    on_server_msg(msg);
                }
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
            let _ = ws.send_with_u8_array(&bytes);
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
