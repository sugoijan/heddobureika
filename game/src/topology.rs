//! Topology contracts for geometry-agnostic puzzle behavior.

use std::collections::HashMap;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

pub use crate::triangular_lattice::{TriDirection, TriLattice};

use crate::ids::{EdgeId, PieceId};
use crate::playable::{PlayableState, Pose2};
use crate::rotation_step::SymmetryStrength;
pub use crate::traits::topology::{FrameBounds, ImagePlacement, PieceOuterFeature, PuzzleTopology};
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

    /// Convenience constructor for a `TriangularTessellationTopology` spec
    /// with the default (horizontal) guide-line direction. `lines` is the
    /// number of strips across the spanned axis; `points` the number of
    /// points along each line.
    pub fn triangular_tessellation(lines: u32, points: u32) -> Self {
        TriangularTessellationTopology::new_spec(lines, points)
    }

    /// Convenience constructor for a `TriangularTessellationTopology` spec
    /// with an explicit guide-line direction.
    pub fn triangular_tessellation_directed(
        direction: TriDirection,
        lines: u32,
        points: u32,
    ) -> Self {
        TriangularTessellationTopology::new_spec_directed(direction, lines, points)
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

/// Helper: pack three `u32`s into 12 little-endian bytes. Used by the
/// triangular topology for its `(direction, lines, points)` payload.
fn write_three_u32_payload(a: u32, b: u32, c: u32) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(12);
    bytes.extend_from_slice(&a.to_le_bytes());
    bytes.extend_from_slice(&b.to_le_bytes());
    bytes.extend_from_slice(&c.to_le_bytes());
    bytes
}

fn read_three_u32_payload(bytes: &[u8]) -> Option<(u32, u32, u32)> {
    if bytes.len() != 12 {
        return None;
    }
    let a = u32::from_le_bytes(bytes[0..4].try_into().ok()?);
    let b = u32::from_le_bytes(bytes[4..8].try_into().ok()?);
    let c = u32::from_le_bytes(bytes[8..12].try_into().ok()?);
    Some((a, b, c))
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

/// Whether a triangular piece is a regular interior triangle or an
/// irregular border filler.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrianglePieceKind {
    /// Interior triangle — all three sides shared with neighbours, so it is
    /// exactly equilateral.
    Regular,
    /// Border filler closing the jagged lattice edge to the frame. Has at
    /// least one frame edge and is generally not equilateral.
    Border,
}

/// Equilateral-triangle tiling of a rectangle (see [`crate::triangular_lattice`]).
///
/// A guide-line `direction` is chosen; evenly spaced lines span one axis
/// fully while points step along them at the equilateral spacing, so every
/// interior (3-shared-side) triangle is exactly regular. The jagged
/// line-ends are closed with non-regular border fillers, yielding a clean
/// rectangular puzzle. `lines` is the number of strips across the spanned
/// axis; `points` the number of points along each line.
#[derive(Clone, Debug, PartialEq)]
pub struct TriangularTessellationTopology {
    direction: TriDirection,
    lines: NonZeroU32,
    points: NonZeroU32,
    /// Per-piece lattice vertex indices (into `vertices`).
    faces: Box<[[u32; 3]]>,
    /// Lattice vertices in pose units.
    vertices: Box<[(f32, f32)]>,
    /// Per-piece: regular interior triangle (`true`) vs border filler.
    inner: Box<[bool]>,
    /// Per-piece canonical centroid in pose units (the solved anchor).
    centroids: Box<[(f32, f32)]>,
    edges: Box<[(PieceId, PieceId)]>,
    /// Pose-unit extent `(width, height)` of the tiled rectangle.
    extent: (f32, f32),
}

impl TriangularTessellationTopology {
    /// Horizontal-direction constructor (the default). `lines` strips span
    /// the vertical axis; `points` step along each horizontal line.
    pub fn new(lines: NonZeroU32, points: NonZeroU32) -> Self {
        Self::new_directed(TriDirection::Horizontal, lines, points)
    }

    pub fn new_directed(direction: TriDirection, lines: NonZeroU32, points: NonZeroU32) -> Self {
        let lattice = TriLattice::build(direction, lines.get(), points.get().max(2))
            .expect("valid triangular lattice parameters");
        let faces: Box<[[u32; 3]]> = lattice
            .faces
            .iter()
            .map(|f| [f[0] as u32, f[1] as u32, f[2] as u32])
            .collect();
        let centroids: Box<[(f32, f32)]> = (0..lattice.faces.len())
            .map(|i| lattice.face_centroid(i))
            .collect();
        let edges = derive_triangular_edges(&lattice.faces);
        Self {
            direction,
            lines,
            points,
            faces,
            vertices: lattice.vertices.into_boxed_slice(),
            inner: lattice.inner.into_boxed_slice(),
            centroids,
            edges,
            extent: lattice.extent,
        }
    }

    pub fn try_new(lines: u32, points: u32) -> Option<Self> {
        Self::try_new_directed(TriDirection::Horizontal, lines, points)
    }

    pub fn try_new_directed(direction: TriDirection, lines: u32, points: u32) -> Option<Self> {
        Some(Self::new_directed(
            direction,
            NonZeroU32::new(lines)?,
            NonZeroU32::new(points.max(2))?,
        ))
    }

    pub fn example_3x2() -> Self {
        Self::try_new(3, 5).expect("valid example")
    }

    pub fn direction(&self) -> TriDirection {
        self.direction
    }

    pub fn lines(&self) -> NonZeroU32 {
        self.lines
    }

    pub fn points(&self) -> NonZeroU32 {
        self.points
    }

    /// Pose-unit extent `(width, height)` of the tiled rectangle.
    pub fn pose_extent(&self) -> (f32, f32) {
        self.extent
    }

    /// Lattice vertices (pose units) and per-piece vertex-index triples —
    /// consumed by the shaper to build the scaled mesh.
    pub fn lattice_geometry(&self) -> (&[(f32, f32)], &[[u32; 3]]) {
        (&self.vertices, &self.faces)
    }

    pub fn piece_kind(&self, piece: PieceId) -> Option<TrianglePieceKind> {
        self.inner.get(piece.as_usize()).map(|&inner| {
            if inner {
                TrianglePieceKind::Regular
            } else {
                TrianglePieceKind::Border
            }
        })
    }

    /// Canonical (solved) centroid of a piece in pose units.
    pub fn canonical_position_mm(&self, piece: PieceId) -> Option<(LengthMm, LengthMm)> {
        let (x, y) = self.centroids.get(piece.as_usize()).copied()?;
        Some((
            LengthMm::try_new(x).unwrap_or_default(),
            LengthMm::try_new(y).unwrap_or_default(),
        ))
    }

    /// Frame (border) edges of a piece, as endpoint pairs in pose units
    /// relative to the piece centroid (BEFORE rotation) — used by
    /// `visit_outer_features`. Empty for interior pieces.
    fn border_edges_local(&self, piece: PieceId) -> Vec<((f32, f32), (f32, f32))> {
        let idx = piece.as_usize();
        let Some(face) = self.faces.get(idx) else {
            return Vec::new();
        };
        if self.inner.get(idx).copied().unwrap_or(false) {
            return Vec::new();
        }
        let (cx, cy) = self.centroids[idx];
        let mut out = Vec::new();
        for k in 0..3 {
            let a = face[k] as usize;
            let b = face[(k + 1) % 3] as usize;
            if self.edge_is_border(a, b) {
                let (ax, ay) = self.vertices[a];
                let (bx, by) = self.vertices[b];
                out.push(((ax - cx, ay - cy), (bx - cx, by - cy)));
            }
        }
        out
    }

    /// Whether the undirected vertex edge `(a, b)` is on the frame (owned by
    /// exactly one face).
    fn edge_is_border(&self, a: usize, b: usize) -> bool {
        let mut count = 0;
        for face in self.faces.iter() {
            let f = [face[0] as usize, face[1] as usize, face[2] as usize];
            for k in 0..3 {
                let (u, v) = (f[k], f[(k + 1) % 3]);
                if (u == a && v == b) || (u == b && v == a) {
                    count += 1;
                }
            }
        }
        count == 1
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
            let (shape, color, kind_label) = match self.piece_kind(piece) {
                Some(TrianglePieceKind::Border) => ("box", "#d7ecff", "border"),
                Some(TrianglePieceKind::Regular) => ("triangle", "#f7e7c3", "regular"),
                None => ("ellipse", "#eeeeee", "unknown"),
            };
            lines.push(format!(
                "  p{id} [label=\"{id}\\n{kind_label}\", shape={shape}, fillcolor=\"{color}\", pos=\"{:.3},{:.3}!\"];",
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

impl TriangularTessellationTopology {
    /// Convenience constructor for a `TopologySpec` (horizontal direction).
    pub fn new_spec(lines: u32, points: u32) -> TopologySpec {
        Self::new_spec_directed(TriDirection::Horizontal, lines, points)
    }

    /// Convenience constructor for a `TopologySpec` with explicit direction.
    pub fn new_spec_directed(direction: TriDirection, lines: u32, points: u32) -> TopologySpec {
        TopologySpec {
            tag: <Self as SerializableTopology>::TAG.to_string(),
            payload: write_three_u32_payload(direction.as_u32(), lines, points),
        }
    }
}

impl SerializableTopology for TriangularTessellationTopology {
    const TAG: &'static str = "triangular_tessellation";

    fn write_payload(&self) -> Vec<u8> {
        write_three_u32_payload(self.direction.as_u32(), self.lines.get(), self.points.get())
    }

    fn read_payload(bytes: &[u8]) -> Option<Self> {
        let (dir, lines, points) = read_three_u32_payload(bytes)?;
        Self::try_new_directed(TriDirection::from_u32(dir), lines, points)
    }
}

/// Interior (shared) edges of a triangular lattice, as `(faceA, faceB)`
/// piece pairs with `faceA < faceB`, in a deterministic order.
fn derive_triangular_edges(faces: &[[usize; 3]]) -> Box<[(PieceId, PieceId)]> {
    let key = |a: usize, b: usize| if a < b { (a, b) } else { (b, a) };
    let mut owners: HashMap<(usize, usize), Vec<usize>> = HashMap::new();
    for (fi, f) in faces.iter().enumerate() {
        for k in 0..3 {
            owners
                .entry(key(f[k], f[(k + 1) % 3]))
                .or_default()
                .push(fi);
        }
    }
    let mut pairs: Vec<(u32, u32)> = owners
        .values()
        .filter(|o| o.len() == 2)
        .map(|o| (o[0].min(o[1]) as u32, o[0].max(o[1]) as u32))
        .collect();
    pairs.sort_unstable();
    pairs.dedup();
    pairs
        .into_iter()
        .map(|(a, b)| (PieceId(a), PieceId(b)))
        .collect()
}

impl PuzzleTopology for TriangularTessellationTopology {
    fn to_spec(&self) -> TopologySpec {
        <Self as SerializableTopology>::to_spec(self)
    }

    fn piece_count(&self) -> u32 {
        self.faces.len() as u32
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
            Some(TrianglePieceKind::Regular) => triangular_regular_symmetry_angles(),
            // Border fillers aren't equilateral; treat their rotation hints
            // like the grid's 90° steps (weak).
            _ => triangular_half_symmetry_angles(),
        }
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        match self.piece_kind(piece) {
            Some(TrianglePieceKind::Regular) => SymmetryStrength::Strong,
            _ => SymmetryStrength::Weak,
        }
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        let mut iter = self.centroids.iter();
        let first = iter.next()?;
        let mut min_x = first.0;
        let mut min_y = first.1;
        let mut max_x = min_x;
        let mut max_y = min_y;
        for (x, y) in iter {
            min_x = min_x.min(*x);
            min_y = min_y.min(*y);
            max_x = max_x.max(*x);
            max_y = max_y.max(*y);
        }
        Some(FrameBounds {
            min_x,
            min_y,
            max_x,
            max_y,
        })
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        // Border (filler) pieces are exactly the non-interior faces.
        !self.inner.get(piece.as_usize()).copied().unwrap_or(false)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        // Each frame edge of a border piece is emitted directly from the
        // lattice, in piece-local pose units (relative to the centroid,
        // before rotation). The frame-snap solver matches each to the
        // nearest/most-parallel frame side; a corner filler has two such
        // edges, giving a 2-axis constraint without a special corner case.
        for (p1, p2) in self.border_edges_local(piece) {
            visitor(PieceOuterFeature::BorderEdge { p1, p2 });
        }
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        // Solved group pose = the anchor piece's (piece 0's) canonical
        // centroid, so the assembled puzzle lands at its frame position.
        let (x, y) = self.centroids.first().copied()?;
        Some((PieceId(0), Pose2::try_from_mm_degrees(x, y, 0.0)?))
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        Some((self.lines.get(), self.points.get()))
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        self.extent
    }

    /// Triangular keeps pieces exactly equilateral, so the lattice is scaled
    /// UNIFORMLY (same factor on both axes) and centred — the image area
    /// outside the frame is the letterbox crop. This is the single source of
    /// truth the renderer and the worker both consume.
    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        let (ex, ey) = self.extent;
        let w = image_width as f32;
        let h = image_height as f32;
        if ex <= 0.0 || ey <= 0.0 {
            return ImagePlacement {
                pose_unit_px: [1.0, 1.0],
                origin_px: [0.0, 0.0],
                frame_px: [w, h],
            };
        }
        let scale = (w / ex).min(h / ey);
        let frame_w = ex * scale;
        let frame_h = ey * scale;
        ImagePlacement {
            pose_unit_px: [scale, scale],
            origin_px: [(w - frame_w) * 0.5, (h - frame_h) * 0.5],
            frame_px: [frame_w, frame_h],
        }
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.extent
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        self.centroids.get(piece.as_usize()).copied()
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        _settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        use crate::traits::shaping::TopologyShaper;
        if self.extent.0 <= 0.0 || self.extent.1 <= 0.0 {
            return None;
        }
        // Uniform scale + centred frame — the shared placement the worker also
        // uses, so client and server never disagree on where pieces sit.
        let placement = self.image_placement(image_width, image_height);
        let scale = placement.pose_unit_px[0];
        let [frame_w, frame_h] = placement.frame_px;
        let [origin_x, origin_y] = placement.origin_px;

        let corner_radius_px = crate::render_geometry::PuzzleFrameShape::from_image_and_pieces(
            frame_w.max(1.0) as u32,
            frame_h.max(1.0) as u32,
            [scale, scale * crate::triangular_lattice::TRI_ROW_HEIGHT],
        )
        .corner_radius_px;
        let frame_shape = crate::render_geometry::PuzzleFrameShape {
            bounds: crate::render_geometry::RectPx {
                x: origin_x,
                y: origin_y,
                width: frame_w,
                height: frame_h,
            },
            corner_radius_px,
        };
        let shaper = crate::triangular_shape::TriangularTessellationShaper;
        let settings =
            crate::triangular_shape::TriangularTessellationShapeSettings { corner_radius_px };
        let cache = shaper
            .build_cache(
                self,
                LengthMm::try_new(frame_w)?,
                LengthMm::try_new(frame_h)?,
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
            [scale, scale],
            [origin_x, origin_y],
            mask_pad_px,
            frame_shape,
            |piece| {
                let (x, y) = self.canonical_position_in_pose_units(piece)?;
                Some((origin_x + x * scale, origin_y + y * scale))
            },
            move |_piece| (origin_x, origin_y),
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

    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        (*self).image_placement(image_width, image_height)
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

    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        self.as_ref().image_placement(image_width, image_height)
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

    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        self.as_ref().image_placement(image_width, image_height)
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

    fn image_placement(&self, image_width: u32, image_height: u32) -> ImagePlacement {
        self.as_ref().image_placement(image_width, image_height)
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
