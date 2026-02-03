#[derive(Clone, Debug)]
pub enum SyncAction {
    Move { anchor_id: usize, pos: (f32, f32) },
    Transform {
        anchor_id: usize,
        pos: (f32, f32),
        rot_deg: f32,
    },
    Place {
        anchor_id: usize,
        pos: (f32, f32),
        rot_deg: f32,
    },
    Flip { piece_id: usize, flipped: bool },
    Release { anchor_id: usize },
}

#[derive(Clone, Debug)]
pub enum CoreAction {
    BeginDrag {
        piece_id: usize,
        x: f32,
        y: f32,
        shift_key: bool,
        rotate_mode: bool,
        right_click: bool,
        click_slop: f32,
        pointer_id: Option<i32>,
    },
    DragMove {
        x: f32,
        y: f32,
    },
    DragEnd {
        pointer_id: Option<i32>,
    },
    SetHovered {
        hovered: Option<usize>,
    },
    Sync(SyncAction),
}
