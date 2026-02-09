use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum BootState {
    ColdStart,
    LoadingRoute,
    LoadingStorage,
    InitSync,
    Ready,
}

thread_local! {
    static BOOT_STATE: RefCell<BootState> = RefCell::new(BootState::ColdStart);
    static BOOT_HOOKS: RefCell<Vec<(u64, Rc<dyn Fn()>)>> = RefCell::new(Vec::new());
    static NEXT_HOOK_ID: RefCell<u64> = RefCell::new(1);
}

pub(crate) fn boot_state() -> BootState {
    BOOT_STATE.with(|state| state.borrow().clone())
}

pub(crate) fn set_boot_state(next: BootState) {
    let hooks = BOOT_STATE.with(|state| {
        let mut state = state.borrow_mut();
        if *state == next {
            return Vec::new();
        }
        *state = next;
        BOOT_HOOKS
            .with(|hooks| hooks.borrow().iter().map(|(_, hook)| hook.clone()).collect())
    });
    for hook in hooks {
        hook();
    }
}

pub(crate) fn add_boot_state_hook(hook: Rc<dyn Fn()>) -> u64 {
    BOOT_HOOKS.with(|hooks| {
        let mut hooks = hooks.borrow_mut();
        let id = NEXT_HOOK_ID.with(|next| {
            let mut next = next.borrow_mut();
            let id = *next;
            *next = next.saturating_add(1);
            id
        });
        hooks.push((id, hook));
        id
    })
}

pub(crate) fn remove_boot_state_hook(id: u64) {
    BOOT_HOOKS.with(|hooks| {
        hooks.borrow_mut().retain(|(hook_id, _)| *hook_id != id);
    });
}
