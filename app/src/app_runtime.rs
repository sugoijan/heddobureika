use std::cell::RefCell;

use crate::app_router::{self, InitConfig};

thread_local! {
    static INIT_CONFIG: RefCell<Option<InitConfig>> = RefCell::new(None);
}

pub(crate) fn set_init_config(config: InitConfig) {
    INIT_CONFIG.with(|slot| {
        *slot.borrow_mut() = Some(config);
    });
}

pub(crate) fn init_config() -> InitConfig {
    INIT_CONFIG.with(|slot| slot.borrow().clone())
        .unwrap_or_else(app_router::load_init_config)
}
