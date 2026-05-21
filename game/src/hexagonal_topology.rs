//! Hexagonal-tiling puzzle topology (flat-top hexagons, offset columns).
//!
//! Layout invariants:
//! - `cols` must be odd. This makes columns `0` and `cols-1` both **even**,
//!   so the left and right edges of the puzzle are anchored by clipped
//!   "virtual" corner hexagons at all four rectangle corners.
//! - Even columns hold `rows` hexagons each (centres at `y = r * √3` in
//!   pose units, where `r ∈ [0, rows)`); their top and bottom hexagons
//!   sit ON the frame line and are cut in half by it.
//! - Odd columns hold `rows - 1` hexagons each (centres at `y = (r +
//!   0.5) * √3` for `r ∈ [0, rows - 1)`); their top hex's flat top side
//!   sits ON the top frame line (tangent), and their bottom hex's flat
//!   bottom side sits on the bottom frame line.
//!
//! Pose-unit convention: 1 pose unit = 1 inner hexagon side length.
//! The puzzle's snap-frame extent is `(total_x_pose, (rows-1)*√3)`,
//! where `total_x_pose = aspect * (rows-1) * √3`. Inner columns sit
//! at uniform `1.5` spacing in pose units; the outer columns (col 0
//! and col C-1) can sit further out — `outer_gap_pose >= 1.5` — so
//! the outer-column hexes absorb the horizontal "extra" left over
//! when `aspect * (R-1) * √3 > (C-1) * 1.5`. When the aspect would
//! force `outer_gap_pose < 1.5` (= inner hexes would need to compress,
//! making outer pieces THINNER than regular), we clamp to `1.5` and
//! fall back to uniform layout.

use std::num::NonZeroU32;
use std::sync::OnceLock;

use crate::ids::{EdgeId, PieceId};
use crate::playable::Pose2;
use crate::topology::{RelativePose, SerializableTopology, TopologySpec};
use crate::traits::topology::{FrameBounds, PieceOuterFeature, PuzzleTopology};
use crate::units::{AngleDeg, LengthMm};

const SQRT_3: f32 = 1.732_050_8;
const SQRT_3_OVER_2: f32 = 0.866_025_4;

/// Geometric classification of a hex piece in the rectangular frame.
///
/// Used by the shaper to pick the right clipping polygon and by the
/// snap solver to decide which outer features to emit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HexPieceKind {
    /// Interior hex, fully inside the puzzle frame.
    Regular,
    /// Half-hex: the centre sits ON a frame line and the outer half is
    /// clipped away. The result is a symmetric trapezoid (top/bottom
    /// frame) or a pentagon (left/right frame).
    EdgeCut,
    /// Full hex with one flat side resting on a frame line — only
    /// appears on the top and bottom frame lines (odd columns).
    EdgeTangent,
    /// Corner hex: centre sits at a rectangle corner, the result of
    /// clipping is a right trapezoid.
    Corner,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HexagonalTopology {
    cols: NonZeroU32,
    rows: NonZeroU32,
    /// Image aspect ratio (W/H). When `aspect` produces an
    /// `outer_gap_pose > 1.5`, the outermost columns absorb the
    /// horizontal "extra" so inner hexes stay regular. When the
    /// natural layout would force inner hexes to compress (i.e. would
    /// require thinner edges), `outer_gap_pose` is clamped to `1.5`
    /// and we fall back to uniform layout.
    aspect_ratio: f32,
    /// Cached `outer_gap_pose` (in pose units = inner hex sides). For
    /// uniform layout this is `1.5`. For wider-edges layout it's
    /// `(aspect*(R-1)*√3 - (C-3)*1.5) / 2`, clamped to `>= 1.5`.
    outer_gap_pose: f32,
    piece_kinds: Box<[HexPieceKind]>,
    edges: Box<[(PieceId, PieceId)]>,
    canonical_positions_mm: Box<[(LengthMm, LengthMm)]>,
}

impl HexagonalTopology {
    /// Constructs a new hex topology. `cols` must be odd; otherwise
    /// returns `None`. `rows` must be at least 1. `aspect_ratio` is
    /// the puzzle's `W/H` (only impacts where the outer columns sit;
    /// inner hex shape stays regular when the aspect leaves
    /// horizontal extra, otherwise we fall back to uniform layout).
    pub fn new(cols: NonZeroU32, rows: NonZeroU32, aspect_ratio: f32) -> Option<Self> {
        if cols.get() % 2 == 0 {
            return None;
        }
        if !aspect_ratio.is_finite() || aspect_ratio <= 0.0 {
            return None;
        }
        let c = cols.get();
        let r = rows.get();
        let total = piece_count_for(c, r);

        let outer_gap_pose = compute_outer_gap_pose(c, r, aspect_ratio);

        // Piece kinds, canonical positions: one pass walking each
        // (col, row_in_col).
        let mut piece_kinds = vec![HexPieceKind::Regular; total as usize];
        let mut canonical_positions_mm = vec![(LengthMm::zero(), LengthMm::zero()); total as usize];

        for col in 0..c {
            let col_is_odd = col % 2 == 1;
            let rows_in_col = if col_is_odd { r - 1 } else { r };
            for row_in_col in 0..rows_in_col {
                let id = piece_id_unchecked(c, r, col, row_in_col).as_usize();
                let x = col_pose_x(col, c, outer_gap_pose);
                let y = if col_is_odd {
                    (row_in_col as f32 + 0.5) * SQRT_3
                } else {
                    row_in_col as f32 * SQRT_3
                };
                canonical_positions_mm[id] = (
                    LengthMm::try_new(x).unwrap_or_default(),
                    LengthMm::try_new(y).unwrap_or_default(),
                );
                piece_kinds[id] = classify(c, r, col, row_in_col);
            }
        }

        // Edges: walk every piece, ask the 6-direction neighbour table,
        // emit `(min, max)` pairs into a HashSet so duplicates collapse.
        let mut edge_set = std::collections::BTreeSet::new();
        for col in 0..c {
            let col_is_odd = col % 2 == 1;
            let rows_in_col = if col_is_odd { r - 1 } else { r };
            for row_in_col in 0..rows_in_col {
                let here = piece_id_unchecked(c, r, col, row_in_col);
                for (n_col, n_row) in neighbour_coords(col, row_in_col) {
                    if !is_valid_coord(c, r, n_col, n_row) {
                        continue;
                    }
                    let other = piece_id_unchecked(c, r, n_col as u32, n_row as u32);
                    let (a, b) = if here.as_u32() < other.as_u32() {
                        (here, other)
                    } else {
                        (other, here)
                    };
                    edge_set.insert((a.as_u32(), b.as_u32()));
                }
            }
        }
        let edges: Vec<(PieceId, PieceId)> = edge_set
            .into_iter()
            .map(|(a, b)| (PieceId(a), PieceId(b)))
            .collect();

        Some(Self {
            cols,
            rows,
            aspect_ratio,
            outer_gap_pose,
            piece_kinds: piece_kinds.into_boxed_slice(),
            edges: edges.into_boxed_slice(),
            canonical_positions_mm: canonical_positions_mm.into_boxed_slice(),
        })
    }

    pub fn try_new(cols: u32, rows: u32, aspect_ratio: f32) -> Option<Self> {
        Self::new(NonZeroU32::new(cols)?, NonZeroU32::new(rows)?, aspect_ratio)
    }

    /// Convenience constructor that picks the aspect for which the
    /// layout would be *exactly uniform* (= no edge widening, no
    /// inner squish). Used by tests and snapshots that don't care
    /// about image aspect.
    pub fn try_new_uniform(cols: u32, rows: u32) -> Option<Self> {
        let aspect = Self::uniform_aspect_for(cols, rows)?;
        Self::try_new(cols, rows, aspect)
    }

    /// The image aspect (W/H) for which a `cols × rows` hex grid is
    /// exactly uniform (`outer_gap_pose = 1.5`, inner hexes regular
    /// under uniform stretch).
    pub fn uniform_aspect_for(cols: u32, rows: u32) -> Option<f32> {
        if cols < 3 || rows < 2 {
            return None;
        }
        Some((cols as f32 - 1.0) * 1.5 / ((rows as f32 - 1.0) * SQRT_3))
    }

    pub fn cols(&self) -> NonZeroU32 {
        self.cols
    }

    pub fn rows(&self) -> NonZeroU32 {
        self.rows
    }

    pub fn aspect_ratio(&self) -> f32 {
        self.aspect_ratio
    }

    pub fn outer_gap_pose(&self) -> f32 {
        self.outer_gap_pose
    }

    pub fn piece_id_at(&self, col: u32, row_in_col: u32) -> Option<PieceId> {
        let c = self.cols.get();
        let r = self.rows.get();
        if !is_valid_coord(c, r, col as i32, row_in_col as i32) {
            return None;
        }
        Some(piece_id_unchecked(c, r, col, row_in_col))
    }

    pub fn piece_col_row(&self, piece: PieceId) -> Option<(u32, u32)> {
        let id = piece.as_u32();
        let total = self.piece_count();
        if id >= total {
            return None;
        }
        let r = self.rows.get();
        let pair = id / (2 * r - 1);
        let in_pair = id % (2 * r - 1);
        if in_pair < r {
            Some((2 * pair, in_pair))
        } else {
            Some((2 * pair + 1, in_pair - r))
        }
    }

    pub fn piece_kind(&self, piece: PieceId) -> Option<HexPieceKind> {
        self.piece_kinds.get(piece.as_usize()).copied()
    }

    pub fn canonical_position_mm(&self, piece: PieceId) -> Option<(LengthMm, LengthMm)> {
        self.canonical_positions_mm.get(piece.as_usize()).copied()
    }

    /// Convenience constructor for a `TopologySpec` describing a
    /// hexagonal puzzle of the given dimensions and aspect ratio.
    pub fn new_spec(cols: u32, rows: u32, aspect_ratio: f32) -> TopologySpec {
        TopologySpec {
            tag: <Self as SerializableTopology>::TAG.to_string(),
            payload: write_hex_payload(cols, rows, aspect_ratio),
        }
    }
}

fn write_hex_payload(cols: u32, rows: u32, aspect: f32) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(12);
    bytes.extend_from_slice(&cols.to_le_bytes());
    bytes.extend_from_slice(&rows.to_le_bytes());
    bytes.extend_from_slice(&aspect.to_bits().to_le_bytes());
    bytes
}

/// Computes the pose-unit "outer gap" between col 0 and col 1 (and
/// col C-2 and col C-1). For uniform layout this is 1.5 (= one inner
/// column spacing). When the puzzle aspect leaves horizontal extra,
/// the outer gap stretches beyond 1.5 so outer-column hexes absorb
/// it; inner hexes stay regular. When the aspect would force
/// outer_gap < 1.5 (= edges would have to be THINNER), we clamp to
/// 1.5 and fall back to uniform layout (inner hexes squeezed instead).
pub(crate) fn compute_outer_gap_pose(cols: u32, rows: u32, aspect: f32) -> f32 {
    if cols < 3 || rows < 2 {
        return 1.5;
    }
    let total_x_pose = aspect * (rows as f32 - 1.0) * SQRT_3;
    let inner_span = (cols as f32 - 3.0) * 1.5;
    let gap = (total_x_pose - inner_span) * 0.5;
    gap.max(1.5)
}

/// Returns the pose-x coordinate of column `col` in a topology with
/// `cols` total columns and the given `outer_gap_pose`. Cols 0 and
/// `cols-1` are the "outer" columns; everything between them sits at
/// a regular 1.5-step spacing.
pub(crate) fn col_pose_x(col: u32, cols: u32, outer_gap_pose: f32) -> f32 {
    if col == 0 {
        0.0
    } else if col + 1 == cols {
        outer_gap_pose + (cols as f32 - 3.0) * 1.5 + outer_gap_pose
    } else {
        outer_gap_pose + (col as f32 - 1.0) * 1.5
    }
}

/// Number of pieces in a `C × R` hex puzzle. `(2*R*C - C + 1) / 2` —
/// equivalent to `(C+1)/2 * R + (C-1)/2 * (R-1)`.
fn piece_count_for(cols: u32, rows: u32) -> u32 {
    (cols * (2 * rows - 1) + 1) / 2
}

/// Even columns count from the front: col `2k` corresponds to pair `k`.
/// Even cols hold `R` pieces, odd cols hold `R-1` pieces, packed into
/// `2R-1`-sized "pairs". With `C` odd the last pair has only the even
/// half.
fn piece_id_unchecked(_cols: u32, rows: u32, col: u32, row_in_col: u32) -> PieceId {
    let r = rows;
    let pair = col / 2;
    if col % 2 == 0 {
        PieceId(pair * (2 * r - 1) + row_in_col)
    } else {
        PieceId(pair * (2 * r - 1) + r + row_in_col)
    }
}

fn is_valid_coord(cols: u32, rows: u32, col: i32, row_in_col: i32) -> bool {
    if col < 0 || (col as u32) >= cols || row_in_col < 0 {
        return false;
    }
    let limit = if (col as u32) % 2 == 0 {
        rows
    } else {
        rows.saturating_sub(1)
    };
    (row_in_col as u32) < limit
}

/// Offset-coordinate neighbour table for flat-top, offset-column hex
/// grids. Returns the six candidate `(col, row_in_col)` neighbours;
/// the caller filters out invalid coords (off-grid).
fn neighbour_coords(col: u32, row_in_col: u32) -> [(i32, i32); 6] {
    let c = col as i32;
    let r = row_in_col as i32;
    if col % 2 == 0 {
        [
            (c, r - 1),     // N
            (c, r + 1),     // S
            (c + 1, r - 1), // NE
            (c + 1, r),     // SE
            (c - 1, r - 1), // NW
            (c - 1, r),     // SW
        ]
    } else {
        [
            (c, r - 1),     // N
            (c, r + 1),     // S
            (c + 1, r),     // NE
            (c + 1, r + 1), // SE
            (c - 1, r),     // NW
            (c - 1, r + 1), // SW
        ]
    }
}

fn classify(cols: u32, rows: u32, col: u32, row_in_col: u32) -> HexPieceKind {
    let col_is_odd = col % 2 == 1;
    let on_left = col == 0;
    let on_right = col + 1 == cols;
    if col_is_odd {
        // Odd columns only touch the top or bottom edge (tangent), and
        // never the left/right edges (their hexagons are inset).
        let on_top = row_in_col == 0;
        let on_bottom = row_in_col + 1 == rows.saturating_sub(1);
        if on_top || on_bottom {
            HexPieceKind::EdgeTangent
        } else {
            HexPieceKind::Regular
        }
    } else {
        let on_top = row_in_col == 0;
        let on_bottom = row_in_col + 1 == rows;
        let is_corner = (on_top || on_bottom) && (on_left || on_right);
        if is_corner {
            HexPieceKind::Corner
        } else if on_top || on_bottom || on_left || on_right {
            HexPieceKind::EdgeCut
        } else {
            HexPieceKind::Regular
        }
    }
}

impl SerializableTopology for HexagonalTopology {
    const TAG: &'static str = "hexagonal";

    fn write_payload(&self) -> Vec<u8> {
        write_hex_payload(self.cols.get(), self.rows.get(), self.aspect_ratio)
    }

    fn read_payload(bytes: &[u8]) -> Option<Self> {
        if bytes.len() != 12 {
            return None;
        }
        let cols = u32::from_le_bytes(bytes[0..4].try_into().ok()?);
        let rows = u32::from_le_bytes(bytes[4..8].try_into().ok()?);
        let aspect_bits = u32::from_le_bytes(bytes[8..12].try_into().ok()?);
        let aspect = f32::from_bits(aspect_bits);
        Self::try_new(cols, rows, aspect)
    }
}

impl PuzzleTopology for HexagonalTopology {
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
        // Interior (Regular) hexes have 60° rotational symmetry — the
        // user can step-rotate by 60°. Boundary pieces (corner / edge-
        // cut / edge-tangent) only fit the puzzle frame at 90°
        // multiples, matching the rectangular frame's symmetry, so
        // they use 90° step quanta.
        match self.piece_kind(piece) {
            Some(HexPieceKind::Regular) => hex_regular_symmetry_angles(),
            _ => hex_boundary_symmetry_angles(),
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
        !matches!(self.piece_kind(piece), Some(HexPieceKind::Regular) | None)
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        Some((PieceId(0), Pose2::try_from_mm_degrees(0.0, 0.0, 0.0)?))
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        Some((self.cols.get(), self.rows.get()))
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        let c = self.cols.get();
        let r = self.rows.get();
        let total_x = col_pose_x(c.saturating_sub(1), c, self.outer_gap_pose);
        (total_x, (r as f32 - 1.0) * SQRT_3)
    }

    fn snap_frame_extent_in_pose_units(&self) -> (f32, f32) {
        self.image_extent_in_pose_units()
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        let (x, y) = self.canonical_position_mm(piece)?;
        Some((x.as_mm_f32(), y.as_mm_f32()))
    }

    // Phase 3 — frame snap features.
    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        let Some((col, row_in_col)) = self.piece_col_row(piece) else {
            return;
        };
        let Some(kind) = self.piece_kind(piece) else {
            return;
        };
        let c = self.cols.get();
        let r = self.rows.get();

        let on_top_even = col % 2 == 0 && row_in_col == 0;
        let on_bottom_even = col % 2 == 0 && row_in_col + 1 == r;
        let on_left = col == 0;
        let on_right = col + 1 == c;
        let on_top_tangent = col % 2 == 1 && row_in_col == 0;
        let on_bottom_tangent = col % 2 == 1 && row_in_col + 1 == r - 1;

        match kind {
            HexPieceKind::Regular => return,
            HexPieceKind::Corner => {
                // The corner hex's anchor sits AT the rectangle corner
                // (in pose units the anchor IS `(0|extent_x, 0|extent_y)`).
                // So all three features live at piece-local origin:
                // - horizontal `BorderEdge` along the top/bottom frame
                // - vertical `BorderEdge` along the left/right frame
                // - `CornerAttachment` at the (rounded-clip-removed)
                //   geometric corner, so the universal solver still
                //   pulls the piece onto the corner.
                visitor(PieceOuterFeature::BorderEdge {
                    p1: (-1.0, 0.0),
                    p2: (1.0, 0.0),
                });
                visitor(PieceOuterFeature::BorderEdge {
                    p1: (0.0, -SQRT_3_OVER_2),
                    p2: (0.0, SQRT_3_OVER_2),
                });
                visitor(PieceOuterFeature::CornerAttachment { point: (0.0, 0.0) });
            }
            HexPieceKind::EdgeCut => {
                if on_top_even || on_bottom_even {
                    // Horizontal cut through the centre.
                    visitor(PieceOuterFeature::BorderEdge {
                        p1: (-1.0, 0.0),
                        p2: (1.0, 0.0),
                    });
                } else if on_left || on_right {
                    // Vertical cut through the centre.
                    visitor(PieceOuterFeature::BorderEdge {
                        p1: (0.0, -SQRT_3_OVER_2),
                        p2: (0.0, SQRT_3_OVER_2),
                    });
                }
            }
            HexPieceKind::EdgeTangent => {
                if on_top_tangent {
                    visitor(PieceOuterFeature::BorderEdge {
                        p1: (-0.5, -SQRT_3_OVER_2),
                        p2: (0.5, -SQRT_3_OVER_2),
                    });
                }
                if on_bottom_tangent {
                    visitor(PieceOuterFeature::BorderEdge {
                        p1: (-0.5, SQRT_3_OVER_2),
                        p2: (0.5, SQRT_3_OVER_2),
                    });
                }
            }
        }
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        _settings: &dyn std::any::Any,
    ) -> Option<crate::render_geometry::PuzzleRenderGeometry> {
        use crate::traits::shaping::TopologyShaper;
        // Per-axis pose-unit derived from the topology's image extent.
        // In wider-edges mode (`outer_gap_pose > 1.5`) these are
        // equal — inner hexes regular. In uniform fallback they
        // differ — hexes stretched anisotropically.
        let (extent_x_pose, extent_y_pose) = self.image_extent_in_pose_units();
        let pose_unit_x = image_width as f32 / extent_x_pose.max(1.0e-3);
        let pose_unit_y = image_height as f32 / extent_y_pose.max(1.0e-3);
        // Typical-piece bbox in px: a hex spans `(2*s_x, √3*s_y)`.
        let typical_x = pose_unit_x * 2.0;
        let typical_y = pose_unit_y * SQRT_3;
        let frame_shape = crate::render_geometry::PuzzleFrameShape::from_image_and_pieces(
            image_width,
            image_height,
            [typical_x, typical_y],
        );
        let shaper = crate::hexagonal_shape::HexagonalShaper;
        let settings = crate::hexagonal_shape::HexagonalShapeSettings {
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

fn hex_regular_symmetry_angles() -> &'static [AngleDeg] {
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

fn hex_boundary_symmetry_angles() -> &'static [AngleDeg] {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_even_cols() {
        assert!(HexagonalTopology::try_new_uniform(2, 2).is_none());
        assert!(HexagonalTopology::try_new_uniform(4, 3).is_none());
    }

    #[test]
    fn piece_count_matches_formula() {
        for &(c, r, expected) in &[
            (3, 2, 5),
            (3, 3, 8),
            (5, 2, 8),
            (5, 3, 13),
            (5, 4, 18),
            (7, 3, 18),
            (7, 4, 25),
            (9, 5, 41),
        ] {
            let t = HexagonalTopology::try_new_uniform(c, r).expect("valid");
            assert_eq!(t.piece_count(), expected, "{}x{}", c, r);
        }
    }

    #[test]
    fn piece_id_round_trip() {
        let t = HexagonalTopology::try_new_uniform(5, 3).expect("valid");
        for id in 0..t.piece_count() {
            let piece = PieceId(id);
            let (col, row_in_col) = t.piece_col_row(piece).expect("decode");
            let recovered = t.piece_id_at(col, row_in_col).expect("encode");
            assert_eq!(recovered, piece, "id {id}");
        }
    }

    #[test]
    fn canonical_positions_match_layout() {
        let t = HexagonalTopology::try_new_uniform(3, 2).expect("valid");
        // C=3, R=2. Even col 0 has 2 hexes at (0, 0), (0, √3).
        // Odd col 1 has 1 hex at (1.5, √3/2). Even col 2 has 2 hexes
        // at (3, 0), (3, √3).
        let positions: Vec<_> = (0..t.piece_count())
            .map(|id| {
                let (x, y) = t.canonical_position_mm(PieceId(id)).unwrap();
                (x.as_mm_f32(), y.as_mm_f32())
            })
            .collect();
        assert_eq!(positions.len(), 5);
        let approx = |a: f32, b: f32| (a - b).abs() < 1.0e-4;
        // piece 0: col=0, row=0 → (0, 0)
        assert!(approx(positions[0].0, 0.0));
        assert!(approx(positions[0].1, 0.0));
        // piece 1: col=0, row=1 → (0, √3)
        assert!(approx(positions[1].0, 0.0));
        assert!(approx(positions[1].1, SQRT_3));
        // piece 2: col=1, row=0 → (1.5, √3/2)
        assert!(approx(positions[2].0, 1.5));
        assert!(approx(positions[2].1, SQRT_3 / 2.0));
        // piece 3: col=2, row=0 → (3, 0)
        assert!(approx(positions[3].0, 3.0));
        assert!(approx(positions[3].1, 0.0));
        // piece 4: col=2, row=1 → (3, √3)
        assert!(approx(positions[4].0, 3.0));
        assert!(approx(positions[4].1, SQRT_3));
    }

    #[test]
    fn piece_kinds_match_layout() {
        let t = HexagonalTopology::try_new_uniform(5, 3).expect("valid");
        // Corners in even cols 0 and 4 at row 0 and row 2.
        assert_eq!(
            t.piece_kind(t.piece_id_at(0, 0).unwrap()),
            Some(HexPieceKind::Corner)
        );
        assert_eq!(
            t.piece_kind(t.piece_id_at(0, 2).unwrap()),
            Some(HexPieceKind::Corner)
        );
        assert_eq!(
            t.piece_kind(t.piece_id_at(4, 0).unwrap()),
            Some(HexPieceKind::Corner)
        );
        assert_eq!(
            t.piece_kind(t.piece_id_at(4, 2).unwrap()),
            Some(HexPieceKind::Corner)
        );
        // Left edge (col 0, interior row): EdgeCut.
        assert_eq!(
            t.piece_kind(t.piece_id_at(0, 1).unwrap()),
            Some(HexPieceKind::EdgeCut)
        );
        // Top edge cut (col 2, row 0): EdgeCut.
        assert_eq!(
            t.piece_kind(t.piece_id_at(2, 0).unwrap()),
            Some(HexPieceKind::EdgeCut)
        );
        // Tangent (odd col, top row): EdgeTangent.
        assert_eq!(
            t.piece_kind(t.piece_id_at(1, 0).unwrap()),
            Some(HexPieceKind::EdgeTangent)
        );
        // Tangent (odd col, bottom row): EdgeTangent. C=5, R=3, odd col has R-1=2 hexes.
        assert_eq!(
            t.piece_kind(t.piece_id_at(1, 1).unwrap()),
            Some(HexPieceKind::EdgeTangent)
        );
        // Interior hex (col 2, row 1): Regular.
        assert_eq!(
            t.piece_kind(t.piece_id_at(2, 1).unwrap()),
            Some(HexPieceKind::Regular)
        );
    }

    #[test]
    fn is_frame_border_piece_matches_kind() {
        let t = HexagonalTopology::try_new_uniform(5, 3).expect("valid");
        for id in 0..t.piece_count() {
            let piece = PieceId(id);
            let kind = t.piece_kind(piece).unwrap();
            assert_eq!(
                t.is_frame_border_piece(piece),
                !matches!(kind, HexPieceKind::Regular),
                "piece {id}"
            );
        }
    }

    #[test]
    fn edges_are_unique_and_endpoints_valid() {
        for (c, r) in [(3, 2), (5, 3), (7, 4), (9, 5)] {
            let t = HexagonalTopology::try_new_uniform(c, r).expect("valid");
            let mut seen = std::collections::HashSet::new();
            for idx in 0..t.edge_count() {
                let (a, b) = t.edge_endpoints(EdgeId(idx));
                assert!(a.as_u32() < t.piece_count());
                assert!(b.as_u32() < t.piece_count());
                assert_ne!(a, b);
                assert!(a.as_u32() < b.as_u32(), "edges must be (min, max)");
                assert!(seen.insert((a, b)), "duplicate edge");
            }
        }
    }

    #[test]
    fn neighbour_count_is_six_for_interior() {
        // For a sufficiently large grid, an interior piece has 6 edges.
        let t = HexagonalTopology::try_new_uniform(9, 5).expect("valid");
        // Pick a clearly interior piece: col 4, row_in_col 2.
        let p = t.piece_id_at(4, 2).expect("interior piece");
        let mut count = 0;
        for idx in 0..t.edge_count() {
            let (a, b) = t.edge_endpoints(EdgeId(idx));
            if a == p || b == p {
                count += 1;
            }
        }
        assert_eq!(count, 6, "interior hex must have 6 neighbours");
    }

    #[test]
    fn serializes_and_deserializes() {
        for (c, r) in [(3, 2), (5, 3), (7, 4)] {
            let original = HexagonalTopology::try_new_uniform(c, r).expect("valid");
            let spec = <HexagonalTopology as SerializableTopology>::to_spec(&original);
            assert_eq!(spec.tag, "hexagonal");
            assert_eq!(spec.payload.len(), 12);
            let restored = HexagonalTopology::read_payload(&spec.payload).expect("decode");
            assert_eq!(restored, original);
        }
    }

    #[test]
    fn read_payload_rejects_bad_input() {
        assert!(HexagonalTopology::read_payload(&[]).is_none());
        assert!(HexagonalTopology::read_payload(&[0u8; 7]).is_none());
        // Even cols rejected.
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&4u32.to_le_bytes());
        bytes.extend_from_slice(&2u32.to_le_bytes());
        assert!(HexagonalTopology::read_payload(&bytes).is_none());
        // Zero rows rejected.
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&3u32.to_le_bytes());
        bytes.extend_from_slice(&0u32.to_le_bytes());
        assert!(HexagonalTopology::read_payload(&bytes).is_none());
    }

    #[test]
    fn build_render_geometry_produces_one_piece_per_cell() {
        for (c, r, w, h) in [(3, 2, 450, 260), (5, 3, 600, 350), (7, 4, 900, 520)] {
            let t = HexagonalTopology::try_new_uniform(c, r).expect("valid");
            let geom = t
                .build_render_geometry(
                    w,
                    h,
                    0,
                    &crate::hexagonal_shape::HexagonalShapeSettings::default(),
                )
                .expect("render geometry");
            assert_eq!(geom.pieces.len(), t.piece_count() as usize, "{c}x{r}");
            for piece in &geom.pieces {
                assert!(piece.bounds_px.width > 0.0);
                assert!(piece.bounds_px.height > 0.0);
            }
        }
    }

    #[test]
    fn snap_frame_extent_matches_image_extent() {
        for (c, r) in [(3, 2), (5, 3), (7, 4)] {
            let t = HexagonalTopology::try_new_uniform(c, r).expect("valid");
            assert_eq!(
                t.snap_frame_extent_in_pose_units(),
                t.image_extent_in_pose_units()
            );
        }
    }
}
