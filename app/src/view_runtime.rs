use std::cell::RefCell;
use std::rc::Rc;

use crate::core::{RenderSettings, SvgRenderSettings, WgpuRenderSettings};

/// A renderer's settings hook plus the last settings applied, so a renderer
/// that registers after the settings changed still receives them.
struct SettingsSlot<T> {
    hook: RefCell<Option<Rc<dyn Fn(T)>>>,
    last: RefCell<Option<T>>,
}

impl<T: Clone> SettingsSlot<T> {
    const fn new() -> Self {
        Self {
            hook: RefCell::new(None),
            last: RefCell::new(None),
        }
    }

    fn set_hook(&self, hook: Option<Rc<dyn Fn(T)>>) {
        *self.hook.borrow_mut() = hook.clone();
        if let (Some(hook), Some(settings)) = (hook, self.last.borrow().clone()) {
            hook(settings);
        }
    }

    fn apply(&self, settings: T) {
        *self.last.borrow_mut() = Some(settings.clone());
        if let Some(hook) = self.hook.borrow().as_ref() {
            hook(settings);
        }
    }
}

/// The renderer currently driving the page, as seen by modules that must not
/// know which one it is (sync layer, crash handling).
pub(crate) trait ActiveView {
    /// Schedules a redraw with the latest snapshot.
    fn request_render(&self);
    /// A downloaded private image is now in the local store.
    fn private_image_stored(&self, hash: &str);
}

thread_local! {
    static SVG: SettingsSlot<SvgRenderSettings> = const { SettingsSlot::new() };
    static WGPU: SettingsSlot<WgpuRenderSettings> = const { SettingsSlot::new() };
    static ACTIVE_VIEW: RefCell<Option<Rc<dyn ActiveView>>> = const { RefCell::new(None) };
}

pub(crate) fn set_active_view(view: Option<Rc<dyn ActiveView>>) {
    ACTIVE_VIEW.with(|slot| *slot.borrow_mut() = view);
}

pub(crate) fn request_render() {
    let view = ACTIVE_VIEW.with(|slot| slot.borrow().clone());
    if let Some(view) = view {
        view.request_render();
    }
}

pub(crate) fn private_image_stored(hash: &str) {
    let view = ACTIVE_VIEW.with(|slot| slot.borrow().clone());
    if let Some(view) = view {
        view.private_image_stored(hash);
    }
}

pub(crate) fn set_svg_settings_hook(hook: Option<Rc<dyn Fn(SvgRenderSettings)>>) {
    SVG.with(|slot| slot.set_hook(hook));
}

pub(crate) fn set_wgpu_settings_hook(hook: Option<Rc<dyn Fn(WgpuRenderSettings)>>) {
    WGPU.with(|slot| slot.set_hook(hook));
}

pub(crate) fn apply_svg_settings(settings: SvgRenderSettings) {
    SVG.with(|slot| slot.apply(settings));
}

pub(crate) fn apply_wgpu_settings(settings: WgpuRenderSettings) {
    WGPU.with(|slot| slot.apply(settings));
}

pub(crate) fn apply_render_settings(settings: &RenderSettings) {
    apply_svg_settings(settings.svg.clone());
    apply_wgpu_settings(settings.wgpu.clone());
}
