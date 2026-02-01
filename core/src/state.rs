use crate::{compute_workspace_layout, GameRules, GridChoice, PuzzleInfo, WorkspaceLayout, FALLBACK_GRID};

#[derive(Clone, Debug)]
pub struct CoreState {
    pub puzzle_info: Option<PuzzleInfo>,
    pub grid: GridChoice,
    pub piece_width: f32,
    pub piece_height: f32,
    pub positions: Vec<(f32, f32)>,
    pub rotations: Vec<f32>,
    pub flips: Vec<bool>,
    pub connections: Vec<[bool; 4]>,
    pub solved: bool,
    pub layout: WorkspaceLayout,
    pub scramble_nonce: u32,
    pub rules: GameRules,
}

impl CoreState {
    pub fn new() -> Self {
        let rules = GameRules::default();
        let layout = compute_workspace_layout(1.0, 1.0, rules.workspace_padding_ratio);
        Self {
            puzzle_info: None,
            grid: FALLBACK_GRID,
            piece_width: 0.0,
            piece_height: 0.0,
            positions: Vec::new(),
            rotations: Vec::new(),
            flips: Vec::new(),
            connections: Vec::new(),
            solved: false,
            layout,
            scramble_nonce: 0,
            rules,
        }
    }
}

impl Default for CoreState {
    fn default() -> Self {
        Self::new()
    }
}
