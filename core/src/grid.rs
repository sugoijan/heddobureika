#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GridChoice {
    pub target_count: u32,
    pub cols: u32,
    pub rows: u32,
    pub actual_count: u32,
}

pub const TARGET_PIECE_COUNTS: [u32; 11] = [
    50, 100, 150, 300, 500, 750, 1000, 1500, 2000, 3000, 5000,
];
pub const DEFAULT_TARGET_COUNT: u32 = 100;
pub const GRID_REL_COUNT_TOL: f32 = 0.05;
pub const GRID_PIECE_RATIO_MAX: f32 = 1.42;
pub const GRID_ROW_MIN: u32 = 2;
pub const GRID_ROW_WIDEN: f32 = 1.5;
pub const GRID_NEIGHBOR_COLS: i32 = 3;
pub const GRID_SCORE_COUNT: f32 = 1.0;
pub const GRID_SCORE_GRID: f32 = 1.0;
pub const GRID_SCORE_PIECE: f32 = 0.5;
pub const SOLVE_TIME_FACTOR: f32 = 4.1;
pub const SOLVE_TIME_EXPONENT: f32 = 1.3;

pub const FALLBACK_GRID: GridChoice = GridChoice {
    target_count: 80,
    cols: 10,
    rows: 8,
    actual_count: 80,
};

pub fn grid_choice_index(choices: &[GridChoice], cols: u32, rows: u32) -> Option<usize> {
    choices
        .iter()
        .position(|choice| choice.cols == cols && choice.rows == rows)
}

pub fn grid_choice_label(choice: &GridChoice) -> String {
    if choice.actual_count == choice.target_count {
        format!(
            "{} pieces ({}x{})",
            choice.target_count, choice.cols, choice.rows
        )
    } else {
        format!(
            "{} pieces ({}x{}, actual {})",
            choice.target_count, choice.cols, choice.rows, choice.actual_count
        )
    }
}

pub fn best_grid_for_count(width: u32, height: u32, target: u32) -> Option<GridChoice> {
    if target == 0 || width == 0 || height == 0 {
        return None;
    }
    let aspect = width as f32 / height as f32;
    let piece_ratio_max = GRID_PIECE_RATIO_MAX.max(1.0);
    let piece_ratio_min = 1.0 / piece_ratio_max;
    let base = (target as f32).sqrt().ceil() as u32;
    let r_hi = ((base as f32) * GRID_ROW_WIDEN).ceil() as u32;
    let r_hi = r_hi.max(GRID_ROW_MIN);
    let mut best: Option<(GridChoice, f32)> = None;
    for r in GRID_ROW_MIN..=r_hi {
        let c_star = target as f32 / r as f32;
        let c0 = c_star.round() as i32;
        for dc in -GRID_NEIGHBOR_COLS..=GRID_NEIGHBOR_COLS {
            let c = c0 + dc;
            if c < 2 {
                continue;
            }
            let actual = r as i32 * c;
            let actual_u = actual as u32;
            let rel_err = ((actual_u as f32) - (target as f32)).abs() / target as f32;
            if rel_err > GRID_REL_COUNT_TOL {
                continue;
            }
            let grid_ratio = c as f32 / r as f32;
            let piece_ratio = aspect / grid_ratio;
            if piece_ratio < piece_ratio_min || piece_ratio > piece_ratio_max {
                continue;
            }
            let eps = 1e-12;
            let count_term = rel_err.powi(2);
            let grid_term = ((grid_ratio + eps) / (aspect + eps)).ln().powi(2);
            let piece_term = (piece_ratio + eps).ln().powi(2);
            let score = GRID_SCORE_COUNT * count_term
                + GRID_SCORE_GRID * grid_term
                + GRID_SCORE_PIECE * piece_term;
            let choice = GridChoice {
                target_count: target,
                cols: c as u32,
                rows: r,
                actual_count: actual_u,
            };
            match &best {
                Some((_best_choice, best_score)) if score >= *best_score => {}
                _ => {
                    best = Some((choice, score));
                }
            }
        }
    }
    best.map(|(choice, _)| choice)
}

pub fn build_grid_choices(width: u32, height: u32) -> Vec<GridChoice> {
    TARGET_PIECE_COUNTS
        .iter()
        .filter_map(|target| best_grid_for_count(width, height, *target))
        .collect()
}
