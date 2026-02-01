use std::cell::RefCell;
use std::rc::Rc;

use crate::core::{RenderSettings, SvgRenderSettings, WgpuRenderSettings};

thread_local! {
    static SVG_HOOK: RefCell<Option<Rc<dyn Fn(SvgRenderSettings)>>> = RefCell::new(None);
    static WGPU_HOOK: RefCell<Option<Rc<dyn Fn(WgpuRenderSettings)>>> = RefCell::new(None);
    static SVG_LAST: RefCell<Option<SvgRenderSettings>> = RefCell::new(None);
    static WGPU_LAST: RefCell<Option<WgpuRenderSettings>> = RefCell::new(None);
}

pub(crate) fn set_svg_settings_hook(hook: Option<Rc<dyn Fn(SvgRenderSettings)>>) {
    SVG_HOOK.with(|slot| {
        *slot.borrow_mut() = hook.clone();
    });
    if let Some(hook) = hook {
        if let Some(settings) = SVG_LAST.with(|slot| slot.borrow().clone()) {
            hook(settings);
        }
    }
}

pub(crate) fn set_wgpu_settings_hook(hook: Option<Rc<dyn Fn(WgpuRenderSettings)>>) {
    WGPU_HOOK.with(|slot| {
        *slot.borrow_mut() = hook.clone();
    });
    if let Some(hook) = hook {
        if let Some(settings) = WGPU_LAST.with(|slot| slot.borrow().clone()) {
            hook(settings);
        }
    }
}

pub(crate) fn apply_svg_settings(settings: SvgRenderSettings) {
    SVG_LAST.with(|slot| {
        *slot.borrow_mut() = Some(settings.clone());
    });
    SVG_HOOK.with(|slot| {
        if let Some(hook) = slot.borrow().as_ref() {
            hook(settings);
        }
    });
}

pub(crate) fn apply_wgpu_settings(settings: WgpuRenderSettings) {
    WGPU_LAST.with(|slot| {
        *slot.borrow_mut() = Some(settings.clone());
    });
    WGPU_HOOK.with(|slot| {
        if let Some(hook) = slot.borrow().as_ref() {
            hook(settings);
        }
    });
}

pub(crate) fn apply_render_settings(settings: &RenderSettings) {
    apply_svg_settings(settings.svg.clone());
    apply_wgpu_settings(settings.wgpu.clone());
}
