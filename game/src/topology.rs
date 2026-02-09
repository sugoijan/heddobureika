//! Topology contracts for geometry-agnostic puzzle behavior.

use std::num::NonZeroU32;
use std::sync::OnceLock;

use crate::ids::{EdgeId, PieceId};
pub use crate::traits::topology::PuzzleTopology;
use crate::units::{AngleDeg, LengthMm};

/// Relative transform expectation for topology-defined neighbor relationships.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RelativePose {
    pub dx: LengthMm,
    pub dy: LengthMm,
    pub drot: AngleDeg,
}

/// Rectangular grid puzzle topology.
///
/// Piece ids are row-major:
/// `piece = row * cols + col`.
///
/// Edge ids are packed in two contiguous ranges:
/// 1) Horizontal adjacencies (left-right), row-major.
/// 2) Vertical adjacencies (up-down), row-major.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GridTopology {
    cols: NonZeroU32,
    rows: NonZeroU32,
}

impl GridTopology {
    pub fn new(cols: NonZeroU32, rows: NonZeroU32) -> Self {
        Self { cols, rows }
    }

    pub fn try_new(cols: u32, rows: u32) -> Option<Self> {
        Some(Self {
            cols: NonZeroU32::new(cols)?,
            rows: NonZeroU32::new(rows)?,
        })
    }

    pub fn cols(&self) -> NonZeroU32 {
        self.cols
    }

    pub fn rows(&self) -> NonZeroU32 {
        self.rows
    }

    pub fn piece_id(&self, row: u32, col: u32) -> Option<PieceId> {
        if row >= self.rows.get() || col >= self.cols.get() {
            return None;
        }
        Some(PieceId(row * self.cols.get() + col))
    }

    pub fn piece_row_col(&self, piece: PieceId) -> Option<(u32, u32)> {
        let id = piece.as_u32();
        let total = self.piece_count();
        if id >= total {
            return None;
        }
        Some((id / self.cols.get(), id % self.cols.get()))
    }

    /// Canonical piece position in millimeters for testing/projection baselines.
    ///
    /// Current scale is 1 mm per grid cell.
    pub fn canonical_position_mm(&self, piece: PieceId) -> Option<(LengthMm, LengthMm)> {
        let (row, col) = self.piece_row_col(piece)?;
        let x = LengthMm::try_new(col as f32)?;
        let y = LengthMm::try_new(row as f32)?;
        Some((x, y))
    }

    fn horizontal_edge_count(&self) -> u32 {
        self.rows.get() * self.cols.get().saturating_sub(1)
    }

    pub fn debug_dot_graph(&self) -> String {
        let mut lines = Vec::new();
        lines.push("graph grid_topology {".to_string());
        lines.push("  graph [layout=neato, overlap=false, splines=true];".to_string());
        lines.push(
            "  node [shape=box, style=filled, fillcolor=\"#e8f3d6\", fontname=\"Iosevka\"];"
                .to_string(),
        );

        for id in 0..self.piece_count() {
            let piece = PieceId(id);
            let (row, col) = self.piece_row_col(piece).unwrap_or((0, 0));
            let (x, y) = self
                .canonical_position_mm(piece)
                .unwrap_or((LengthMm::zero(), LengthMm::zero()));
            lines.push(format!(
                "  p{id} [label=\"{id}\\nr{row}c{col}\", pos=\"{:.3},{:.3}!\"];",
                x.as_mm_f32(),
                -y.as_mm_f32()
            ));
        }

        for edge in 0..self.edge_count() {
            let (a, b) = self.edge_endpoints(EdgeId(edge));
            lines.push(format!("  p{} -- p{};", a.as_u32(), b.as_u32()));
        }

        lines.push("}".to_string());
        lines.join("\n")
    }
}

impl PuzzleTopology for GridTopology {
    fn piece_count(&self) -> u32 {
        self.cols.get() * self.rows.get()
    }

    fn edge_count(&self) -> u32 {
        self.horizontal_edge_count() + self.cols.get() * self.rows.get().saturating_sub(1)
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        let idx = edge.as_u32();
        let horizontal = self.horizontal_edge_count();

        if idx < horizontal {
            // Horizontal edge index:
            // row = idx / (cols - 1), col = idx % (cols - 1), between (row,col) and (row,col+1)
            let stride = self.cols.get().saturating_sub(1).max(1);
            let row = idx / stride;
            let col = idx % stride;
            let a = row * self.cols.get() + col;
            let b = a + 1;
            return (PieceId(a), PieceId(b));
        }

        // Vertical edge index:
        // local = idx - horizontal
        // row = local / cols, col = local % cols, between (row,col) and (row+1,col)
        let local = idx.saturating_sub(horizontal);
        let row = local / self.cols.get().max(1);
        let col = local % self.cols.get().max(1);
        let a = row * self.cols.get() + col;
        let b = a + self.cols.get();
        (PieceId(a), PieceId(b))
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        let Some((ra, ca)) = self.piece_row_col(a) else {
            return RelativePose::default();
        };
        let Some((rb, cb)) = self.piece_row_col(b) else {
            return RelativePose::default();
        };
        let dx = LengthMm::try_new(cb as f32 - ca as f32).unwrap_or_default();
        let dy = LengthMm::try_new(rb as f32 - ra as f32).unwrap_or_default();
        RelativePose {
            dx,
            dy,
            drot: AngleDeg::zero(),
        }
    }

    fn symmetry_angles(&self, _piece: PieceId) -> &[AngleDeg] {
        grid_symmetry_angles()
    }
}

/// Piece shape class for triangular tessellation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrianglePieceKind {
    Regular,
    HalfRegular,
}

/// Triangular tessellation over a rectangular region with:
/// - top row of half triangles,
/// - middle rows of regular triangles,
/// - bottom row of half triangles.
///
/// For `cols=3, rows=2`, this yields:
/// - piece rows: `2 * rows + 1 = 5`
/// - regular triangles: `cols * (2 * rows - 1) = 9`
/// - half triangles on top+bottom edges: `2 * cols = 6`
/// - total pieces: `15`
#[derive(Clone, Debug, PartialEq)]
pub struct TriangularTessellationTopology {
    cols: NonZeroU32,
    rows: NonZeroU32,
    piece_kinds: Box<[TrianglePieceKind]>,
    edges: Box<[(PieceId, PieceId)]>,
    canonical_positions_mm: Box<[(LengthMm, LengthMm)]>,
}

impl TriangularTessellationTopology {
    pub fn new(cols: NonZeroU32, rows: NonZeroU32) -> Self {
        let piece_rows = rows.get() * 2 + 1;
        let total_piece_count = cols.get() * piece_rows;

        let mut piece_kinds = Vec::with_capacity(total_piece_count as usize);
        for piece_row in 0..piece_rows {
            let kind = if piece_row == 0 || piece_row + 1 == piece_rows {
                TrianglePieceKind::HalfRegular
            } else {
                TrianglePieceKind::Regular
            };
            for _ in 0..cols.get() {
                piece_kinds.push(kind);
            }
        }

        let mut canonical_positions_mm =
            vec![(LengthMm::zero(), LengthMm::zero()); total_piece_count as usize];

        for piece_row in 0..piece_rows {
            for col in 0..cols.get() {
                let piece = Self::piece_id_at_unchecked(cols, piece_row, col).as_usize();
                let x = if piece_row % 2 == 0 {
                    col as f32
                } else {
                    col as f32 + 0.5
                };
                let y = piece_row as f32;
                canonical_positions_mm[piece] = (
                    LengthMm::try_new(x).unwrap_or_default(),
                    LengthMm::try_new(y).unwrap_or_default(),
                );
            }
        }

        let mut edges = Vec::new();
        // Vertical edges between consecutive piece rows (same column).
        for piece_row in 0..(piece_rows - 1) {
            for col in 0..cols.get() {
                edges.push((
                    Self::piece_id_at_unchecked(cols, piece_row, col),
                    Self::piece_id_at_unchecked(cols, piece_row + 1, col),
                ));
            }
        }

        // Horizontal zig-zag edges by row parity:
        // even rows connect (0-1), (2-3), ...
        // odd rows connect (1-2), (3-4), ...
        for piece_row in 0..piece_rows {
            let start = if piece_row % 2 == 0 { 0 } else { 1 };
            let mut col = start;
            while col + 1 < cols.get() {
                edges.push((
                    Self::piece_id_at_unchecked(cols, piece_row, col),
                    Self::piece_id_at_unchecked(cols, piece_row, col + 1),
                ));
                col += 2;
            }
        }

        Self {
            cols,
            rows,
            piece_kinds: piece_kinds.into_boxed_slice(),
            edges: edges.into_boxed_slice(),
            canonical_positions_mm: canonical_positions_mm.into_boxed_slice(),
        }
    }

    pub fn try_new(cols: u32, rows: u32) -> Option<Self> {
        Some(Self::new(NonZeroU32::new(cols)?, NonZeroU32::new(rows)?))
    }

    pub fn example_3x2() -> Self {
        Self::new(
            NonZeroU32::new(3).expect("3 must be non-zero"),
            NonZeroU32::new(2).expect("2 must be non-zero"),
        )
    }

    pub fn cols(&self) -> NonZeroU32 {
        self.cols
    }

    pub fn rows(&self) -> NonZeroU32 {
        self.rows
    }

    pub fn regular_piece_count(&self) -> u32 {
        self.cols.get() * (self.piece_row_count().saturating_sub(2))
    }

    pub fn half_piece_count(&self) -> u32 {
        self.cols.get() * 2
    }

    pub fn piece_row_count(&self) -> u32 {
        self.rows.get() * 2 + 1
    }

    pub fn piece_id_at(&self, piece_row: u32, col: u32) -> Option<PieceId> {
        if piece_row >= self.piece_row_count() || col >= self.cols.get() {
            return None;
        }
        Some(Self::piece_id_at_unchecked(self.cols, piece_row, col))
    }

    pub fn piece_row_col(&self, piece: PieceId) -> Option<(u32, u32)> {
        let id = piece.as_u32();
        let total = self.piece_count();
        if id >= total {
            return None;
        }
        Some((id / self.cols.get(), id % self.cols.get()))
    }

    pub fn piece_kind(&self, piece: PieceId) -> Option<TrianglePieceKind> {
        self.piece_kinds.get(piece.as_usize()).copied()
    }

    pub fn top_half_piece_id(&self, col: u32) -> Option<PieceId> {
        self.piece_id_at(0, col)
    }

    pub fn bottom_half_piece_id(&self, col: u32) -> Option<PieceId> {
        self.piece_id_at(self.piece_row_count().saturating_sub(1), col)
    }

    pub fn regular_piece_id(&self, piece_row: u32, col: u32) -> Option<PieceId> {
        if piece_row == 0 || piece_row + 1 >= self.piece_row_count() {
            return None;
        }
        if col >= self.cols.get() {
            return None;
        }
        self.piece_id_at(piece_row, col)
    }

    pub fn canonical_position_mm(&self, piece: PieceId) -> Option<(LengthMm, LengthMm)> {
        self.canonical_positions_mm.get(piece.as_usize()).copied()
    }

    pub fn debug_dot_graph(&self) -> String {
        let mut lines = Vec::new();
        lines.push("graph triangular_tessellation {".to_string());
        lines.push("  graph [layout=neato, overlap=false, splines=true];".to_string());
        lines.push("  node [style=filled, fontname=\"Iosevka\"];".to_string());

        for id in 0..self.piece_count() {
            let piece = PieceId(id);
            let (x, y) = self
                .canonical_position_mm(piece)
                .unwrap_or((LengthMm::zero(), LengthMm::zero()));
            let (piece_row, col) = self.piece_row_col(piece).unwrap_or((0, 0));
            let (shape, color, kind_label) = match self.piece_kind(piece) {
                Some(TrianglePieceKind::HalfRegular) => ("box", "#d7ecff", "half"),
                Some(TrianglePieceKind::Regular) => ("triangle", "#f7e7c3", "regular"),
                None => ("ellipse", "#eeeeee", "unknown"),
            };
            lines.push(format!(
                "  p{id} [label=\"{id}\\n{kind_label}\\nr{piece_row}c{col}\", shape={shape}, fillcolor=\"{color}\", pos=\"{:.3},{:.3}!\"];",
                x.as_mm_f32(),
                -y.as_mm_f32()
            ));
        }

        for edge in 0..self.edge_count() {
            let (a, b) = self.edge_endpoints(EdgeId(edge));
            lines.push(format!("  p{} -- p{};", a.as_u32(), b.as_u32()));
        }

        lines.push("}".to_string());
        lines.join("\n")
    }

    fn piece_id_at_unchecked(cols: NonZeroU32, piece_row: u32, col: u32) -> PieceId {
        PieceId(piece_row * cols.get() + col)
    }
}

impl PuzzleTopology for TriangularTessellationTopology {
    fn piece_count(&self) -> u32 {
        self.piece_kinds.len() as u32
    }

    fn edge_count(&self) -> u32 {
        self.edges.len() as u32
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        self.edges
            .get(edge.as_usize())
            .copied()
            .unwrap_or((PieceId(0), PieceId(0)))
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        let Some((ax, ay)) = self.canonical_position_mm(a) else {
            return RelativePose::default();
        };
        let Some((bx, by)) = self.canonical_position_mm(b) else {
            return RelativePose::default();
        };
        RelativePose {
            dx: LengthMm::try_new(bx.as_mm_f32() - ax.as_mm_f32()).unwrap_or_default(),
            dy: LengthMm::try_new(by.as_mm_f32() - ay.as_mm_f32()).unwrap_or_default(),
            drot: AngleDeg::zero(),
        }
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        match self.piece_kind(piece) {
            Some(TrianglePieceKind::HalfRegular) => triangular_half_symmetry_angles(),
            _ => triangular_regular_symmetry_angles(),
        }
    }
}

fn grid_symmetry_angles() -> &'static [AngleDeg] {
    static ANGLES: OnceLock<Box<[AngleDeg]>> = OnceLock::new();
    ANGLES
        .get_or_init(|| {
            vec![
                AngleDeg::try_new(90.0).expect("finite"),
                AngleDeg::try_new(180.0).expect("finite"),
                AngleDeg::try_new(270.0).expect("finite"),
            ]
            .into_boxed_slice()
        })
        .as_ref()
}

fn triangular_regular_symmetry_angles() -> &'static [AngleDeg] {
    static ANGLES: OnceLock<Box<[AngleDeg]>> = OnceLock::new();
    ANGLES
        .get_or_init(|| {
            vec![
                AngleDeg::try_new(60.0).expect("finite"),
                AngleDeg::try_new(120.0).expect("finite"),
                AngleDeg::try_new(180.0).expect("finite"),
                AngleDeg::try_new(240.0).expect("finite"),
                AngleDeg::try_new(300.0).expect("finite"),
            ]
            .into_boxed_slice()
        })
        .as_ref()
}

fn triangular_half_symmetry_angles() -> &'static [AngleDeg] {
    grid_symmetry_angles()
}
