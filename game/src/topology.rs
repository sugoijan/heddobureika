//! Topology contracts for geometry-agnostic puzzle behavior.

use std::num::NonZeroU32;
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

use crate::ids::{EdgeId, PieceId};
use crate::playable::{PlayableState, Pose2};
use crate::rotation_step::SymmetryStrength;
pub use crate::traits::topology::{FrameBounds, PieceOuterFeature, PuzzleTopology};
use crate::units::{AngleDeg, LengthMm};

/// Relative transform expectation for topology-defined neighbor relationships.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RelativePose {
    pub dx: LengthMm,
    pub dy: LengthMm,
    pub drot: AngleDeg,
}

pub type GenericTopology = Arc<dyn PuzzleTopology>;
pub type GenericPlayableState = PlayableState<GenericTopology>;

/// Topology-neutral, transport-friendly puzzle identity. Carries a `tag`
/// (which topology family this is) and an opaque `payload` (the bytes
/// that family uses to reconstruct itself). Consumers should treat the
/// `payload` as opaque — only the topology that emitted it knows how
/// to read it.
///
/// The only code that should look at `tag` is `build_topology_from_spec`
/// below. Everywhere else, two `TopologySpec`s are interchangeable iff
/// they are byte-equal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TopologySpec {
    pub tag: String,
    pub payload: Vec<u8>,
}

impl TopologySpec {
    /// Constructs a placeholder spec used in tests / fallback paths. Not
    /// resolvable back to a `PuzzleTopology`.
    pub fn unknown() -> Self {
        Self {
            tag: "unknown".to_string(),
            payload: Vec::new(),
        }
    }

    /// Convenience constructor for a `GridTopology` spec.
    pub fn grid(cols: u32, rows: u32) -> Self {
        GridTopology::new_spec(cols, rows)
    }

    /// Convenience constructor for a `TriangularTessellationTopology` spec.
    pub fn triangular_tessellation(cols: u32, rows: u32) -> Self {
        TriangularTessellationTopology::new_spec(cols, rows)
    }

    /// Convenience constructor for a `HexagonalTopology` spec. `cols`
    /// must be odd (see `HexagonalTopology::new`). `aspect_ratio` is
    /// the puzzle image's `W/H`; it controls how much the outer
    /// columns stretch to keep inner hexes regular.
    pub fn hexagonal(cols: u32, rows: u32, aspect_ratio: f32) -> Self {
        crate::hexagonal_topology::HexagonalTopology::new_spec(cols, rows, aspect_ratio)
    }

    /// Convenience constructor for a `VoronoiTopology` spec.
    ///
    /// `aspect_ratio` is the image's width / height. Voronoi identity
    /// depends on aspect because the cell geometry would distort if the
    /// tessellation were stretched to a different aspect after the fact.
    pub fn voronoi(piece_count: u32, seed: u32, aspect_ratio: f32) -> Self {
        crate::voronoi_topology::VoronoiTopology::new_spec(piece_count, seed, aspect_ratio)
    }
}

/// Trait implemented by every concrete `PuzzleTopology` that can be
/// serialized to / deserialized from a `TopologySpec`. Each topology owns
/// its own tag string and the encoding of its parameters; nobody outside
/// the topology should interpret the payload.
pub trait SerializableTopology: PuzzleTopology + Sized {
    const TAG: &'static str;

    /// Encode `self`'s parameters into a byte payload. The reverse of
    /// `read_payload`.
    fn write_payload(&self) -> Vec<u8>;

    /// Decode a `(cols, rows)`-style payload into a topology instance.
    /// Returns `None` for malformed input.
    fn read_payload(bytes: &[u8]) -> Option<Self>;

    /// Builds a `TopologySpec` for this concrete topology.
    fn to_spec(&self) -> TopologySpec {
        TopologySpec {
            tag: Self::TAG.to_string(),
            payload: self.write_payload(),
        }
    }
}

/// The single dispatch site that interprets `TopologySpec::tag`. Every
/// other consumer of `TopologySpec` should treat the spec as opaque and
/// route through this function (or `PuzzleTopology::to_spec` for the
/// inverse direction).
///
/// Adding a new topology means: implement `SerializableTopology` on the
/// new topology and add one match arm here.
pub fn build_topology_from_spec(spec: &TopologySpec) -> Option<GenericTopology> {
    match spec.tag.as_str() {
        GridTopology::TAG => GridTopology::read_payload(&spec.payload)
            .map(|t| Arc::new(t) as Arc<dyn PuzzleTopology>),
        TriangularTessellationTopology::TAG => {
            TriangularTessellationTopology::read_payload(&spec.payload)
                .map(|t| Arc::new(t) as Arc<dyn PuzzleTopology>)
        }
        <crate::hexagonal_topology::HexagonalTopology as SerializableTopology>::TAG => {
            crate::hexagonal_topology::HexagonalTopology::read_payload(&spec.payload)
                .map(|t| Arc::new(t) as Arc<dyn PuzzleTopology>)
        }
        <crate::voronoi_topology::VoronoiTopology as SerializableTopology>::TAG => {
            crate::voronoi_topology::VoronoiTopology::read_payload(&spec.payload)
                .map(|t| Arc::new(t) as Arc<dyn PuzzleTopology>)
        }
        tag if tag == crate::voronoi_topology::VoronoiTopology::legacy_tag() => {
            crate::voronoi_topology::VoronoiTopology::read_legacy_payload(&spec.payload)
                .map(|t| Arc::new(t) as Arc<dyn PuzzleTopology>)
        }
        _ => None,
    }
}

/// Helper: pack two `u32`s into 8 little-endian bytes. Used by grid and
/// triangular topologies for their `(cols, rows)` payloads.
fn write_two_u32_payload(a: u32, b: u32) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(8);
    bytes.extend_from_slice(&a.to_le_bytes());
    bytes.extend_from_slice(&b.to_le_bytes());
    bytes
}

fn read_two_u32_payload(bytes: &[u8]) -> Option<(u32, u32)> {
    if bytes.len() != 8 {
        return None;
    }
    let a = u32::from_le_bytes(bytes[0..4].try_into().ok()?);
    let b = u32::from_le_bytes(bytes[4..8].try_into().ok()?);
    Some((a, b))
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
    /// Returns the piece-CENTER pose in piece-count units: piece `(col, row)`
    /// sits at `(col + 0.5, row + 0.5)`. This places piece 0's top-left
    /// pixel at the workspace top-left under the standard
    /// `piece_positions_px` conversion (`pose.x * piece_width - piece_width / 2`).
    pub fn canonical_position_mm(&self, piece: PieceId) -> Option<(LengthMm, LengthMm)> {
        let (row, col) = self.piece_row_col(piece)?;
        let x = LengthMm::try_new(col as f32 + 0.5)?;
        let y = LengthMm::try_new(row as f32 + 0.5)?;
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

impl GridTopology {
    /// Convenience constructor for a `TopologySpec` describing a grid of
    /// the given dimensions. Equivalent to `TopologySpec::grid(cols, rows)`.
    pub fn new_spec(cols: u32, rows: u32) -> TopologySpec {
        TopologySpec {
            tag: <Self as SerializableTopology>::TAG.to_string(),
            payload: write_two_u32_payload(cols, rows),
        }
    }
}

impl SerializableTopology for GridTopology {
    const TAG: &'static str = "grid";

    fn write_payload(&self) -> Vec<u8> {
        write_two_u32_payload(self.cols.get(), self.rows.get())
    }

    fn read_payload(bytes: &[u8]) -> Option<Self> {
        let (cols, rows) = read_two_u32_payload(bytes)?;
        Self::try_new(cols, rows)
    }
}

impl PuzzleTopology for GridTopology {
    fn to_spec(&self) -> TopologySpec {
        <Self as SerializableTopology>::to_spec(self)
    }

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

    fn frame_bounds(&self) -> Option<FrameBounds> {
        Some(FrameBounds {
            min_x: 0.5,
            min_y: 0.5,
            max_x: self.cols.get().saturating_sub(1) as f32 + 0.5,
            max_y: self.rows.get().saturating_sub(1) as f32 + 0.5,
        })
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        let Some((row, col)) = self.piece_row_col(piece) else {
            return false;
        };
        row == 0 || row + 1 == self.rows.get() || col == 0 || col + 1 == self.cols.get()
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        let Some((row, col)) = self.piece_row_col(piece) else {
            return;
        };
        let cols = self.cols.get();
        let rows = self.rows.get();
        // Grid pieces are unit squares in pose units centred on the
        // anchor; outer edges sit at ±0.5 in piece-local coords.
        // Corner pieces emit *two* BorderEdges; the universal solver
        // turns those into a two-axis snap, so no CornerAttachment is
        // needed.
        if row == 0 {
            visitor(PieceOuterFeature::BorderEdge {
                p1: (-0.5, -0.5),
                p2: (0.5, -0.5),
            });
        }
        if col + 1 == cols {
            visitor(PieceOuterFeature::BorderEdge {
                p1: (0.5, -0.5),
                p2: (0.5, 0.5),
            });
        }
        if row + 1 == rows {
            visitor(PieceOuterFeature::BorderEdge {
                p1: (-0.5, 0.5),
                p2: (0.5, 0.5),
            });
        }
        if col == 0 {
            visitor(PieceOuterFeature::BorderEdge {
                p1: (-0.5, -0.5),
                p2: (-0.5, 0.5),
            });
        }
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        Some((
            PieceId(0),
            Pose2::try_from_mm_degrees(0.5, 0.5, 0.0).unwrap_or_default(),
        ))
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        Some((self.cols.get(), self.rows.get()))
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        (self.cols.get() as f32, self.rows.get() as f32)
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        let (row, col) = self.piece_row_col(piece)?;
        Some((col as f32 + 0.5, row as f32 + 0.5))
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        let default_settings = crate::grid_shape::GridShapeSettings::default();
        let settings = settings
            .downcast_ref::<crate::grid_shape::GridShapeSettings>()
            .unwrap_or(&default_settings);
        let piece_width = LengthMm::try_new(image_width as f32 / self.cols.get() as f32)?;
        let piece_height = LengthMm::try_new(image_height as f32 / self.rows.get() as f32)?;
        use crate::traits::shaping::TopologyShaper;
        let cache = crate::grid_shape::GridJigsawShaper
            .build_cache(self, piece_width, piece_height, shape_seed, settings)
            .ok()?;
        let pose_unit_x = piece_width.as_mm_f32();
        let pose_unit_y = piece_height.as_mm_f32();
        let cols = self.cols.get();
        let mask_pad_px = cache.mask_pad.as_mm_f32().ceil();
        // Grid pieces are uniform; the typical piece bbox is the pose
        // unit on each axis, so frame radius == grid's historical
        // `min(piece_w, piece_h) * 0.05`.
        let frame_shape = crate::render_geometry::PuzzleFrameShape::from_image_and_pieces(
            image_width,
            image_height,
            [pose_unit_x, pose_unit_y],
        );
        crate::render_geometry::build_render_geometry_from_atlas(
            self,
            &cache.atlas,
            image_width,
            image_height,
            [pose_unit_x, pose_unit_y],
            [0.0, 0.0],
            mask_pad_px,
            frame_shape,
            |piece| {
                let (x, y) = self.canonical_position_in_pose_units(piece)?;
                Some((x * pose_unit_x, y * pose_unit_y))
            },
            |piece| {
                let id = piece.as_u32();
                let col = id % cols;
                let row = id / cols;
                (col as f32 * pose_unit_x, row as f32 * pose_unit_y)
            },
        )
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

impl TriangularTessellationTopology {
    /// Convenience constructor for a `TopologySpec` describing a
    /// triangular-tessellation puzzle of the given dimensions.
    pub fn new_spec(cols: u32, rows: u32) -> TopologySpec {
        TopologySpec {
            tag: <Self as SerializableTopology>::TAG.to_string(),
            payload: write_two_u32_payload(cols, rows),
        }
    }
}

impl SerializableTopology for TriangularTessellationTopology {
    const TAG: &'static str = "triangular_tessellation";

    fn write_payload(&self) -> Vec<u8> {
        write_two_u32_payload(self.cols.get(), self.rows.get())
    }

    fn read_payload(bytes: &[u8]) -> Option<Self> {
        let (cols, rows) = read_two_u32_payload(bytes)?;
        Self::try_new(cols, rows)
    }
}

impl PuzzleTopology for TriangularTessellationTopology {
    fn to_spec(&self) -> TopologySpec {
        <Self as SerializableTopology>::to_spec(self)
    }

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

    fn frame_bounds(&self) -> Option<FrameBounds> {
        let mut iter = self.canonical_positions_mm.iter();
        let first = iter.next()?;
        let mut min_x = first.0.as_mm_f32();
        let mut min_y = first.1.as_mm_f32();
        let mut max_x = min_x;
        let mut max_y = min_y;
        for (x, y) in iter {
            min_x = min_x.min(x.as_mm_f32());
            min_y = min_y.min(y.as_mm_f32());
            max_x = max_x.max(x.as_mm_f32());
            max_y = max_y.max(y.as_mm_f32());
        }
        Some(FrameBounds {
            min_x,
            min_y,
            max_x,
            max_y,
        })
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        let Some((piece_row, col)) = self.piece_row_col(piece) else {
            return false;
        };
        // Top/bottom half-rows always touch a horizontal frame edge.
        // Regular-row pieces in the first or last column touch one of
        // the vertical frame edges via the mesh boundary.
        piece_row == 0
            || piece_row + 1 == self.piece_row_count()
            || col == 0
            || col + 1 == self.cols.get()
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        let Some((piece_row, col)) = self.piece_row_col(piece) else {
            return;
        };
        let piece_rows = self.piece_row_count();
        let cols = self.cols.get();
        if !self.is_frame_border_piece(piece) {
            return;
        }

        let on_top = piece_row == 0;
        let on_bottom = piece_row + 1 == piece_rows;
        let on_left = col == 0;
        let on_right = col + 1 == cols;
        let is_corner = (on_top || on_bottom) && (on_left || on_right);

        // The triangular layout has piece anchors at canonical
        // `(col, piece_row)` for even rows and `(col+0.5, piece_row)`
        // for odd rows. The visual frame runs from `(0, 0)` to
        // `(cols, piece_rows)`. Half-row anchors are NOT symmetric
        // about the puzzle interior: top half-row anchors sit ON the
        // top frame line, while bottom half-row anchors sit one pose
        // unit ABOVE the bottom frame line (the piece body extends
        // downward to the frame). The piece-local offsets below model
        // that asymmetry so each outer feature's world position lands
        // on the actual visual frame line.
        let anchor_x = if piece_row % 2 == 0 {
            col as f32
        } else {
            col as f32 + 0.5
        };
        let anchor_y = piece_row as f32;
        let frame_x_left = 0.0_f32;
        let frame_x_right = cols as f32;
        let frame_y_top = 0.0_f32;
        let frame_y_bottom = piece_rows as f32;

        if on_top {
            let local_y = frame_y_top - anchor_y;
            visitor(PieceOuterFeature::BorderEdge {
                p1: (-0.5, local_y),
                p2: (0.5, local_y),
            });
        }
        if on_bottom {
            let local_y = frame_y_bottom - anchor_y;
            visitor(PieceOuterFeature::BorderEdge {
                p1: (-0.5, local_y),
                p2: (0.5, local_y),
            });
        }
        // Regular-row left/right pieces touch the side frame at a
        // single mesh vertex — we model that as a unit-long vertical
        // "edge" whose midpoint sits on the frame line in world coords.
        if !is_corner && (on_left || on_right) {
            let local_x = if on_left {
                frame_x_left - anchor_x
            } else {
                frame_x_right - anchor_x
            };
            visitor(PieceOuterFeature::BorderEdge {
                p1: (local_x, -0.5),
                p2: (local_x, 0.5),
            });
        }
        // Half-row corner pieces: the outer corner sits at the puzzle
        // frame corner, NOT at the anchor. Compute the piece-local
        // offset accordingly so the CornerAttachment lands on the
        // correct frame corner under all four rotations.
        if is_corner {
            let local_x = if on_left {
                frame_x_left - anchor_x
            } else {
                frame_x_right - anchor_x
            };
            let local_y = if on_top {
                frame_y_top - anchor_y
            } else {
                frame_y_bottom - anchor_y
            };
            visitor(PieceOuterFeature::CornerAttachment {
                point: (local_x, local_y),
            });
        }
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        Some((PieceId(0), Pose2::try_from_mm_degrees(0.0, 0.0, 0.0)?))
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        Some((self.cols.get(), self.rows.get()))
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        // Triangular pieces stack `2 * rows + 1` piece-rows vertically (a
        // half-row above and below the regular triangles) — that's the
        // actual y-axis span in pose units, not `rows`.
        (self.cols.get() as f32, self.piece_row_count() as f32)
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        // The visual puzzle frame matches `image_extent_in_pose_units`:
        // pieces and the rendered rounded-rectangle border both run
        // from `0` to `(cols, piece_row_count)` in pose units. The
        // half-row pieces' geometry sits asymmetrically — top half-row
        // anchors lie on the top frame line, bottom half-row anchors
        // sit one pose unit above the bottom frame line — but the
        // FRAME ITSELF is symmetric. `visit_outer_features` accounts
        // for the asymmetric anchor placement in the per-piece offsets.
        self.image_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        let (x, y) = self.canonical_position_mm(piece)?;
        Some((x.as_mm_f32(), y.as_mm_f32()))
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        _settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        use crate::traits::shaping::TopologyShaper;
        let piece_rows = self.piece_row_count().max(1);
        let pose_unit_x = image_width as f32 / self.cols.get().max(1) as f32;
        let pose_unit_y = image_height as f32 / piece_rows as f32;
        // Estimate the regular triangle bbox in pixels: in canonical
        // units a unit-side equilateral triangle has a bbox of
        // `(1, sqrt(3)/2)`; the canonical mesh occupies
        // `(1.5*cols, (sqrt(3)/2)*piece_row_count)` and is stretched to
        // `(image_w, image_h)`, so the per-axis stretch factors give
        // typical bbox = `(pose_unit_x / 1.5, pose_unit_y)`.
        let typical_x = pose_unit_x / 1.5;
        let typical_y = pose_unit_y;
        let frame_shape = crate::render_geometry::PuzzleFrameShape::from_image_and_pieces(
            image_width,
            image_height,
            [typical_x, typical_y],
        );
        let shaper = crate::triangular_shape::TriangularTessellationShaper;
        let settings = crate::triangular_shape::TriangularTessellationShapeSettings {
            corner_radius_px: frame_shape.corner_radius_px,
        };
        let cache = shaper
            .build_cache(
                self,
                LengthMm::try_new(image_width as f32)?,
                LengthMm::try_new(image_height as f32)?,
                shape_seed,
                &settings,
            )
            .ok()?;
        let mask_pad_px = cache.mask_pad.as_mm_f32().ceil();
        crate::render_geometry::build_render_geometry_from_atlas(
            self,
            &cache.atlas,
            image_width,
            image_height,
            [pose_unit_x, pose_unit_y],
            [0.0, 0.0],
            mask_pad_px,
            frame_shape,
            |piece| {
                let (x, y) = self.canonical_position_in_pose_units(piece)?;
                Some((x * pose_unit_x, y * pose_unit_y))
            },
            |_piece| (0.0, 0.0),
        )
    }
}

impl<T: PuzzleTopology + ?Sized> PuzzleTopology for &T {
    fn to_spec(&self) -> TopologySpec {
        (*self).to_spec()
    }

    fn piece_count(&self) -> u32 {
        (*self).piece_count()
    }

    fn edge_count(&self) -> u32 {
        (*self).edge_count()
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        (*self).edge_endpoints(edge)
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        (*self).expected_relative_pose(a, b)
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        (*self).symmetry_angles(piece)
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        (*self).symmetry_strength(piece)
    }

    fn step_rotation_cw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        (*self).step_rotation_cw(piece, current, rotation_snap_tolerance)
    }

    fn step_rotation_ccw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        (*self).step_rotation_ccw(piece, current, rotation_snap_tolerance)
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        (*self).frame_bounds()
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        (*self).is_frame_border_piece(piece)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        (*self).visit_outer_features(piece, visitor)
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        (*self).identity_frame_anchor()
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        (*self).dims_hint()
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        (*self).image_extent_in_pose_units()
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        (*self).snap_frame_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        (*self).canonical_position_in_pose_units(piece)
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        (*self).build_render_geometry(image_width, image_height, shape_seed, settings)
    }
}

impl<T: PuzzleTopology + ?Sized> PuzzleTopology for Box<T> {
    fn to_spec(&self) -> TopologySpec {
        self.as_ref().to_spec()
    }

    fn piece_count(&self) -> u32 {
        self.as_ref().piece_count()
    }

    fn edge_count(&self) -> u32 {
        self.as_ref().edge_count()
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        self.as_ref().edge_endpoints(edge)
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        self.as_ref().expected_relative_pose(a, b)
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        self.as_ref().symmetry_angles(piece)
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        self.as_ref().symmetry_strength(piece)
    }

    fn step_rotation_cw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_cw(piece, current, rotation_snap_tolerance)
    }

    fn step_rotation_ccw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_ccw(piece, current, rotation_snap_tolerance)
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        self.as_ref().frame_bounds()
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        self.as_ref().is_frame_border_piece(piece)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        self.as_ref().visit_outer_features(piece, visitor)
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        self.as_ref().identity_frame_anchor()
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        self.as_ref().dims_hint()
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().image_extent_in_pose_units()
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().snap_frame_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        self.as_ref().canonical_position_in_pose_units(piece)
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        self.as_ref()
            .build_render_geometry(image_width, image_height, shape_seed, settings)
    }
}

impl<T: PuzzleTopology + ?Sized> PuzzleTopology for Rc<T> {
    fn to_spec(&self) -> TopologySpec {
        self.as_ref().to_spec()
    }

    fn piece_count(&self) -> u32 {
        self.as_ref().piece_count()
    }

    fn edge_count(&self) -> u32 {
        self.as_ref().edge_count()
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        self.as_ref().edge_endpoints(edge)
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        self.as_ref().expected_relative_pose(a, b)
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        self.as_ref().symmetry_angles(piece)
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        self.as_ref().symmetry_strength(piece)
    }

    fn step_rotation_cw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_cw(piece, current, rotation_snap_tolerance)
    }

    fn step_rotation_ccw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_ccw(piece, current, rotation_snap_tolerance)
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        self.as_ref().frame_bounds()
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        self.as_ref().is_frame_border_piece(piece)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        self.as_ref().visit_outer_features(piece, visitor)
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        self.as_ref().identity_frame_anchor()
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        self.as_ref().dims_hint()
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().image_extent_in_pose_units()
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().snap_frame_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        self.as_ref().canonical_position_in_pose_units(piece)
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        self.as_ref()
            .build_render_geometry(image_width, image_height, shape_seed, settings)
    }
}

impl<T: PuzzleTopology + ?Sized> PuzzleTopology for Arc<T> {
    fn to_spec(&self) -> TopologySpec {
        self.as_ref().to_spec()
    }

    fn piece_count(&self) -> u32 {
        self.as_ref().piece_count()
    }

    fn edge_count(&self) -> u32 {
        self.as_ref().edge_count()
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        self.as_ref().edge_endpoints(edge)
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        self.as_ref().expected_relative_pose(a, b)
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        self.as_ref().symmetry_angles(piece)
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        self.as_ref().symmetry_strength(piece)
    }

    fn step_rotation_cw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_cw(piece, current, rotation_snap_tolerance)
    }

    fn step_rotation_ccw(
        &self,
        piece: PieceId,
        current: AngleDeg,
        rotation_snap_tolerance: AngleDeg,
    ) -> AngleDeg {
        self.as_ref()
            .step_rotation_ccw(piece, current, rotation_snap_tolerance)
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        self.as_ref().frame_bounds()
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        self.as_ref().is_frame_border_piece(piece)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        self.as_ref().visit_outer_features(piece, visitor)
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        self.as_ref().identity_frame_anchor()
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        self.as_ref().dims_hint()
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().image_extent_in_pose_units()
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.as_ref().snap_frame_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        self.as_ref().canonical_position_in_pose_units(piece)
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        self.as_ref()
            .build_render_geometry(image_width, image_height, shape_seed, settings)
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
