use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::app_core::{AppCore, AppSnapshot};
use crate::app_router;
use crate::sync_runtime;
use crate::core::RendererKind;
use crate::runtime::{GameSyncView, GameView, ViewHooks};

thread_local! {
    static SVG_VIEW_ADAPTER: RefCell<Option<SvgViewAdapter>> = RefCell::new(None);
    static SVG_VIEW_STARTED: Cell<bool> = Cell::new(false);
    static SVG_VIEW_SUBSCRIPTION: RefCell<Option<crate::app_core::AppSubscription>> = RefCell::new(None);
    static SVG_VIEW_RENDER_HOOK: RefCell<Option<Rc<dyn Fn(AppSnapshot)>>> = RefCell::new(None);
}

pub(crate) fn run() {
    if app_router::load_renderer_preference() != RendererKind::Svg {
        return;
    }
    let core = AppCore::shared();
    let mut adapter = SvgViewAdapter::new();
    let core_for_hooks = core.clone();
    adapter.init(ViewHooks {
        on_action: Rc::new(move |action| {
            sync_runtime::dispatch_view_action(&core_for_hooks, action, true);
        }),
    });
    let core_for_render = core.clone();
    let subscription = core.subscribe(Rc::new(move || {
        let snapshot = core_for_render.snapshot();
        let sync_view = sync_runtime::sync_view();
        SVG_VIEW_ADAPTER.with(|slot| {
            if let Some(adapter) = slot.borrow_mut().as_mut() {
                adapter.render(&snapshot, &sync_view);
            }
        });
    }));
    SVG_VIEW_ADAPTER.with(|slot| {
        *slot.borrow_mut() = Some(adapter);
    });
    SVG_VIEW_SUBSCRIPTION.with(|slot| {
        *slot.borrow_mut() = Some(subscription);
    });
}

pub(crate) fn set_render_hook(hook: Rc<dyn Fn(AppSnapshot)>) {
    SVG_VIEW_RENDER_HOOK.with(|slot| {
        *slot.borrow_mut() = Some(hook);
    });
}

pub(crate) fn clear_render_hook() {
    SVG_VIEW_RENDER_HOOK.with(|slot| {
        *slot.borrow_mut() = None;
    });
}

fn ensure_started() {
    let should_start = SVG_VIEW_STARTED.with(|started| {
        if started.get() {
            false
        } else {
            started.set(true);
            true
        }
    });
    if should_start {
        crate::yew_app::run_svg();
    }
}

pub(crate) struct SvgViewAdapter {
    hooks: Option<ViewHooks>,
}

impl SvgViewAdapter {
    fn new() -> Self {
        Self { hooks: None }
    }
}

impl GameView for SvgViewAdapter {
    fn init(&mut self, hooks: ViewHooks) {
        self.hooks = Some(hooks.clone());
        ensure_started();
    }

    fn render(&mut self, snapshot: &AppSnapshot, _sync_view: &dyn GameSyncView) {
        let snapshot = snapshot.clone();
        SVG_VIEW_RENDER_HOOK.with(|slot| {
            if let Some(hook) = slot.borrow().as_ref() {
                hook(snapshot);
            }
        });
    }

    fn shutdown(&mut self) {
        self.hooks = None;
    }
}
