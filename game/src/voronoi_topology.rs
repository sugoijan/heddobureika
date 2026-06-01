//! Bounded Voronoi puzzle topology.
//!
//! The topology is deterministic from `(piece_count, seed, image_width,
//! image_height)`.  The image dimensions are stored only as an aspect-ratio
//! hint; canonical coordinates are scaled so the total rectangle area is the
//! requested piece count.

use std::collections::HashMap;
use std::f32::consts::PI;
use std::num::NonZeroU32;

use crate::edge_compose::ConnectorSeg;
use crate::edge_profile::{build_edge_profile_segments, ConnectorShape, EdgeProfileInput};
use crate::grid_shape::GridShapeSettings;
use crate::ids::{EdgeId, PieceId};
use crate::playable::Pose2;
use crate::render_geometry::{build_render_geometry_from_atlas, PuzzleRenderGeometry};
use crate::rotation_step::SymmetryStrength;
use crate::shape::{EdgeSideGeometryMm, PathMm, PathSegMm, PointMm, ShapeAtlasMm};
use crate::shape_atlas_builder::{PieceEdgeBuilderSpec, ShapeAtlasBuilder};
use crate::topology::{RelativePose, SerializableTopology, TopologySpec};
use crate::traits::topology::{FrameBounds, PieceOuterFeature, PuzzleTopology};
use crate::units::{AngleDeg, LengthMm};

const LEGACY_TAG: &str = "voronoi_canary";
const LLOYD_ITERS: usize = 4;
const CANDIDATE_LAYOUTS: u32 = 6;
const EPS: f32 = 1.0e-5;
const BORDER_EPS: f32 = 1.0e-4;
const ROTATION_EDGE_MIN_RATIO: f32 = 0.22;
const VORONOI_TAB_MIN_EDGE_RATIO: f32 = 0.30;
const VORONOI_TAB_SIZE_MIN: f32 = 0.035;
const VORONOI_TAB_SIZE_MAX: f32 = 0.18;
/// Polygon edges shorter than `COLLAPSE_TINY_RATIO * median_edge_len` are
/// fused away: their endpoints merge into a single vertex shared by every
/// cell that touched them. Matches `VORONOI_TAB_MIN_EDGE_RATIO` so we
/// collapse exactly the edges that would have rendered as a flat seam
/// (no tab/blank) — those edges aren't physical connectors, they're
/// numerical artefacts of perturbed 4-site Voronoi junctions.
const COLLAPSE_TINY_RATIO: f32 = VORONOI_TAB_MIN_EDGE_RATIO;
const COLLAPSE_MAX_PASSES: usize = 4;
/// Distance under which two polygon vertices coming from different cells
/// are treated as the same Voronoi junction. The Lloyd / clipping pass
/// produces shared junctions up to single-precision rounding error, so a
/// small absolute eps works well.
const VERTEX_MERGE_EPS: f32 = 1.0e-3;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct Point {
    x: f32,
    y: f32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum VoronoiCellEdgeKind {
    Interior(EdgeId),
    Border,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct VoronoiCellEdge {
    start: Point,
    end: Point,
    kind: VoronoiCellEdgeKind,
}

#[derive(Clone, Debug, PartialEq)]
struct VoronoiCell {
    site: Point,
    polygon: Box<[Point]>,
    edges: Box<[VoronoiCellEdge]>,
    touches_frame: bool,
}

#[derive(Clone, Debug, PartialEq)]
struct VoronoiLayout {
    extent_x: f32,
    extent_y: f32,
    cells: Box<[VoronoiCell]>,
    topology_edges: Box<[(PieceId, PieceId)]>,
}

/// Bounded Voronoi topology with one cell per piece.
///
/// The topology is identified by `(piece_count, seed, aspect_ratio)`.
/// `aspect_ratio = image_width / image_height` is part of identity because
/// changing it changes every cell's shape — there is no way to "stretch"
/// a Voronoi tessellation built for one aspect to render correctly at
/// another. The UI rebuilds the spec when the image changes; see
/// `topology_kinds::VORONOI` for that path.
#[derive(Clone, Debug, PartialEq)]
pub struct VoronoiTopology {
    piece_count: NonZeroU32,
    seed: u32,
    /// Image aspect ratio (width / height). Always finite and > 0.
    aspect_ratio: f32,
    layout: VoronoiLayout,
    rotation_candidates: Box<[Box<[AngleDeg]>]>,
}

impl VoronoiTopology {
    pub fn new(piece_count: NonZeroU32, seed: u32, aspect_ratio: f32) -> Option<Self> {
        let aspect_ratio = sanitize_aspect_ratio(aspect_ratio)?;
        let layout = build_layout(piece_count.get(), seed, aspect_ratio);
        let rotation_candidates = build_rotation_candidates(&layout);
        Some(Self {
            piece_count,
            seed,
            aspect_ratio,
            layout,
            rotation_candidates,
        })
    }

    pub fn try_new(piece_count: u32, seed: u32, aspect_ratio: f32) -> Option<Self> {
        Self::new(NonZeroU32::new(piece_count)?, seed, aspect_ratio)
    }

    pub fn new_spec(piece_count: u32, seed: u32, aspect_ratio: f32) -> TopologySpec {
        TopologySpec {
            tag: Self::TAG.to_string(),
            payload: write_payload(piece_count, seed, aspect_ratio),
        }
    }

    /// Pre-renaming tag for the canary Voronoi topology. Snapshots saved
    /// while the topology was named `voronoi_canary` route through
    /// `read_legacy_payload` instead of `read_payload`. New code should
    /// not use this; the topology kind registry maps it back to the
    /// current `voronoi` kind for UI purposes.
    pub fn legacy_tag() -> &'static str {
        LEGACY_TAG
    }

    /// Decodes the pre-rename canary payload format (8 bytes:
    /// `(piece_count, seed)`). Snapshots from before the aspect-aware
    /// rename still resolve through this path.
    pub fn read_legacy_payload(bytes: &[u8]) -> Option<Self> {
        if bytes.len() != 8 {
            return None;
        }
        let piece_count = u32::from_le_bytes(bytes[0..4].try_into().ok()?);
        let seed = u32::from_le_bytes(bytes[4..8].try_into().ok()?);
        Self::try_new(piece_count, seed, 1.0)
    }

    pub fn seed(&self) -> u32 {
        self.seed
    }

    pub fn aspect_ratio(&self) -> f32 {
        self.aspect_ratio
    }

    pub fn canonical_site(&self, piece: PieceId) -> Option<(f32, f32)> {
        self.layout
            .cells
            .get(piece.as_usize())
            .map(|cell| (cell.site.x, cell.site.y))
    }

    fn cell(&self, piece: PieceId) -> Option<&VoronoiCell> {
        self.layout.cells.get(piece.as_usize())
    }

    fn piece_bbox(&self, piece: PieceId) -> Option<(f32, f32)> {
        let cell = self.cell(piece)?;
        let mut min_x = f32::INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut max_y = f32::NEG_INFINITY;
        for p in cell.polygon.iter() {
            min_x = min_x.min(p.x);
            min_y = min_y.min(p.y);
            max_x = max_x.max(p.x);
            max_y = max_y.max(p.y);
        }
        Some(((max_x - min_x).max(EPS), (max_y - min_y).max(EPS)))
    }
}

impl SerializableTopology for VoronoiTopology {
    const TAG: &'static str = "voronoi";

    fn write_payload(&self) -> Vec<u8> {
        write_payload(self.piece_count.get(), self.seed, self.aspect_ratio)
    }

    fn read_payload(bytes: &[u8]) -> Option<Self> {
        if bytes.len() != 12 {
            return None;
        }
        let piece_count = u32::from_le_bytes(bytes[0..4].try_into().ok()?);
        let seed = u32::from_le_bytes(bytes[4..8].try_into().ok()?);
        let aspect_bits = u32::from_le_bytes(bytes[8..12].try_into().ok()?);
        let aspect_ratio = f32::from_bits(aspect_bits);
        Self::try_new(piece_count, seed, aspect_ratio)
    }
}

fn write_payload(piece_count: u32, seed: u32, aspect_ratio: f32) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(12);
    bytes.extend_from_slice(&piece_count.to_le_bytes());
    bytes.extend_from_slice(&seed.to_le_bytes());
    bytes.extend_from_slice(&aspect_ratio.to_bits().to_le_bytes());
    bytes
}

fn sanitize_aspect_ratio(value: f32) -> Option<f32> {
    if value.is_finite() && value > 0.0 {
        Some(value)
    } else {
        None
    }
}

impl PuzzleTopology for VoronoiTopology {
    fn to_spec(&self) -> TopologySpec {
        <Self as SerializableTopology>::to_spec(self)
    }

    fn piece_count(&self) -> u32 {
        self.piece_count.get()
    }

    fn edge_count(&self) -> u32 {
        self.layout.topology_edges.len() as u32
    }

    fn edge_endpoints(&self, edge: EdgeId) -> (PieceId, PieceId) {
        self.layout
            .topology_edges
            .get(edge.as_usize())
            .copied()
            .unwrap_or((PieceId(0), PieceId(0)))
    }

    fn expected_relative_pose(&self, a: PieceId, b: PieceId) -> RelativePose {
        let Some(a) = self.cell(a) else {
            return RelativePose::default();
        };
        let Some(b) = self.cell(b) else {
            return RelativePose::default();
        };
        RelativePose {
            dx: LengthMm::try_new(b.site.x - a.site.x).unwrap_or_default(),
            dy: LengthMm::try_new(b.site.y - a.site.y).unwrap_or_default(),
            drot: AngleDeg::zero(),
        }
    }

    fn symmetry_angles(&self, piece: PieceId) -> &[AngleDeg] {
        self.rotation_candidates
            .get(piece.as_usize())
            .map(|angles| angles.as_ref())
            .unwrap_or(&[])
    }

    fn symmetry_strength(&self, piece: PieceId) -> SymmetryStrength {
        // Frame border cells use the universal `[90°, 180°, 270°]` set —
        // their angles are real rotation steps shared by every frame
        // piece. Interior cells expose heuristic edge-angle-derived sets
        // that are unique per piece; treating those as authoritative for
        // group rotation would leak the puzzle answer.
        match self.cell(piece) {
            Some(cell) if cell.touches_frame => SymmetryStrength::Strong,
            Some(_) => SymmetryStrength::Weak,
            None => SymmetryStrength::Strong,
        }
    }

    fn frame_bounds(&self) -> Option<FrameBounds> {
        Some(FrameBounds {
            min_x: 0.0,
            min_y: 0.0,
            max_x: self.layout.extent_x,
            max_y: self.layout.extent_y,
        })
    }

    fn is_frame_border_piece(&self, piece: PieceId) -> bool {
        self.cell(piece)
            .map(|cell| cell.touches_frame)
            .unwrap_or(false)
    }

    fn visit_outer_features(&self, piece: PieceId, visitor: &mut dyn FnMut(PieceOuterFeature)) {
        let Some(cell) = self.cell(piece) else {
            return;
        };
        if !cell.touches_frame {
            return;
        }

        let site = cell.site;
        let extent_x = self.layout.extent_x;
        let extent_y = self.layout.extent_y;
        let polygon = &cell.polygon;
        let n = polygon.len();
        if n < 2 {
            return;
        }

        // Polygon edge ⇒ BorderEdge when both endpoints sit on the
        // same frame side. Two non-collinear cell edges on two
        // perpendicular sides become two independent 1-axis constraints,
        // which the universal solver folds into a 2-axis snap — same
        // affordance as a multi-piece edge chain hitting both sides.
        for i in 0..n {
            let v1 = polygon[i];
            let v2 = polygon[(i + 1) % n];
            let same_side = (near(v1.x, 0.0, BORDER_EPS) && near(v2.x, 0.0, BORDER_EPS))
                || (near(v1.x, extent_x, BORDER_EPS) && near(v2.x, extent_x, BORDER_EPS))
                || (near(v1.y, 0.0, BORDER_EPS) && near(v2.y, 0.0, BORDER_EPS))
                || (near(v1.y, extent_y, BORDER_EPS) && near(v2.y, extent_y, BORDER_EPS));
            if same_side {
                visitor(PieceOuterFeature::BorderEdge {
                    p1: (v1.x - site.x, v1.y - site.y),
                    p2: (v2.x - site.x, v2.y - site.y),
                });
            }
        }

        // A polygon vertex sitting on a frame corner ⇒ CornerAttachment.
        // The "false corner" cell whose two boundary segments hit two
        // perpendicular sides via different vertices intentionally does
        // NOT match here — it picks up two BorderEdges instead.
        for v in polygon.iter() {
            let on_left = near(v.x, 0.0, BORDER_EPS);
            let on_right = near(v.x, extent_x, BORDER_EPS);
            let on_top = near(v.y, 0.0, BORDER_EPS);
            let on_bottom = near(v.y, extent_y, BORDER_EPS);
            if (on_left || on_right) && (on_top || on_bottom) {
                visitor(PieceOuterFeature::CornerAttachment {
                    point: (v.x - site.x, v.y - site.y),
                });
            }
        }
    }

    fn identity_frame_anchor(&self) -> Option<(PieceId, Pose2)> {
        let cell = self.cell(PieceId(0))?;
        Some((
            PieceId(0),
            Pose2::try_from_mm_degrees(cell.site.x, cell.site.y, 0.0)?,
        ))
    }

    fn dims_hint(&self) -> Option<(u32, u32)> {
        None
    }

    fn image_extent_in_pose_units(&self) -> (f32, f32) {
        (self.layout.extent_x, self.layout.extent_y)
    }

    fn canonical_position_in_pose_units(&self, piece: PieceId) -> Option<(f32, f32)> {
        let cell = self.cell(piece)?;
        Some((cell.site.x, cell.site.y))
    }

    fn piece_extent_mm(&self, piece: PieceId) -> (LengthMm, LengthMm) {
        let Some((w, h)) = self.piece_bbox(piece) else {
            return (LengthMm::zero(), LengthMm::zero());
        };
        (
            LengthMm::try_new(w).unwrap_or_default(),
            LengthMm::try_new(h).unwrap_or_default(),
        )
    }

    fn build_render_geometry(
        &self,
        image_width: u32,
        image_height: u32,
        shape_seed: u32,
        settings: &dyn std::any::Any,
    ) -> Option<PuzzleRenderGeometry> {
        let default_settings = GridShapeSettings::default();
        let settings = settings
            .downcast_ref::<GridShapeSettings>()
            .unwrap_or(&default_settings);
        let pose_unit_x = image_width as f32 / self.layout.extent_x.max(EPS);
        let pose_unit_y = image_height as f32 / self.layout.extent_y.max(EPS);
        // Voronoi layout calibrates `extent_{x,y}` so each pose unit
        // covers ~one piece's area; using pose units as the typical
        // extent matches what `min(typical bbox)` would give and aligns
        // the frame radius with grid's reference value.
        let frame_shape = crate::render_geometry::PuzzleFrameShape::from_image_and_pieces(
            image_width,
            image_height,
            [pose_unit_x, pose_unit_y],
        );
        let atlas = build_voronoi_atlas(
            self,
            [pose_unit_x, pose_unit_y],
            shape_seed,
            settings,
            frame_shape.corner_radius_px,
        )?;
        let mask_pad_px = voronoi_mask_pad_px(image_width, image_height, settings);
        build_render_geometry_from_atlas(
            self,
            &atlas,
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

fn build_layout(piece_count: u32, seed: u32, aspect_ratio: f32) -> VoronoiLayout {
    let aspect = aspect_ratio.max(EPS);
    let extent_x = (piece_count as f32 * aspect).sqrt();
    let extent_y = (piece_count as f32 / aspect).sqrt();

    let mut best_sites = Vec::new();
    let mut best_polygons = Vec::new();
    let mut best_score = f32::INFINITY;
    for salt in 0..CANDIDATE_LAYOUTS {
        let mut sites = initial_sites(
            piece_count,
            seed ^ salt.wrapping_mul(0x9E37_79B9),
            extent_x,
            extent_y,
        );
        let mut polygons = bounded_cells(&sites, extent_x, extent_y);
        for _ in 0..LLOYD_ITERS {
            let mut next_sites = Vec::with_capacity(sites.len());
            for (idx, polygon) in polygons.iter().enumerate() {
                let centroid = polygon_centroid(polygon).unwrap_or(sites[idx]);
                next_sites.push(Point {
                    x: centroid.x.clamp(EPS, extent_x - EPS),
                    y: centroid.y.clamp(EPS, extent_y - EPS),
                });
            }
            sites = next_sites;
            polygons = bounded_cells(&sites, extent_x, extent_y);
        }
        let score = layout_score(&polygons, piece_count as f32, extent_x, extent_y);
        if score < best_score {
            best_score = score;
            best_sites = sites;
            best_polygons = polygons;
        }
    }

    collapse_tiny_edges(&mut best_polygons, extent_x, extent_y);

    let (cells, topology_edges) = derive_cell_edges(best_sites, best_polygons, extent_x, extent_y);
    VoronoiLayout {
        extent_x,
        extent_y,
        cells: cells.into_boxed_slice(),
        topology_edges: topology_edges.into_boxed_slice(),
    }
}

/// Fuse short polygon edges into their endpoints.
///
/// A bounded Voronoi diagram in general position meets at 3-site vertices,
/// but the Lloyd relaxation perturbs near-degenerate 4-site junctions into
/// pairs of close 3-site vertices joined by a sub-pixel edge. Those edges
/// never carry a tab/blank (they're too short for the connector profile)
/// yet still register as `topology_edges`, so two pieces "stick" with no
/// physical interlock. Collapsing the segment to its midpoint across every
/// cell that touched it restores the natural higher-order junction and
/// removes the spurious connection.
fn collapse_tiny_edges(polygons: &mut Vec<Vec<Point>>, extent_x: f32, extent_y: f32) {
    for _ in 0..COLLAPSE_MAX_PASSES {
        let median = median_polygon_edge_len(polygons, extent_x, extent_y);
        if median <= EPS {
            break;
        }
        let threshold = median * COLLAPSE_TINY_RATIO;
        if !collapse_one_pass(polygons, extent_x, extent_y, threshold) {
            break;
        }
    }
}

fn median_polygon_edge_len(polygons: &[Vec<Point>], extent_x: f32, extent_y: f32) -> f32 {
    let mut lengths = Vec::new();
    for polygon in polygons {
        let n = polygon.len();
        if n < 2 {
            continue;
        }
        for i in 0..n {
            let a = polygon[i];
            let b = polygon[(i + 1) % n];
            if segment_on_border(a, b, extent_x, extent_y) {
                continue;
            }
            let len = distance_sq(a, b).sqrt();
            if len > EPS {
                lengths.push(len);
            }
        }
    }
    if lengths.is_empty() {
        return 0.0;
    }
    lengths.sort_by(|a, b| a.total_cmp(b));
    lengths[lengths.len() / 2]
}

fn collapse_one_pass(
    polygons: &mut Vec<Vec<Point>>,
    extent_x: f32,
    extent_y: f32,
    threshold: f32,
) -> bool {
    let mut flat = Vec::new();
    let mut indices = Vec::with_capacity(polygons.len());
    for polygon in polygons.iter() {
        let start = flat.len();
        for point in polygon {
            flat.push(*point);
        }
        indices.push(start..flat.len());
    }
    if flat.is_empty() {
        return false;
    }

    let mut dsu = Dsu::new(flat.len());

    // Phase 1: merge vertices that already represent the same Voronoi
    // junction (computed independently by adjacent cells, identical up to
    // float rounding).
    spatial_union(&flat, VERTEX_MERGE_EPS, &mut dsu);
    let mut centroids = cluster_centroids(&flat, &mut dsu, extent_x, extent_y);

    // Phase 2: scan every polygon edge in cluster space; below-threshold
    // edges merge their endpoint clusters.
    let mut merged_any = false;
    for range in indices.iter() {
        let n = range.end - range.start;
        if n < 2 {
            continue;
        }
        for i in 0..n {
            let a_flat = range.start + i;
            let b_flat = range.start + (i + 1) % n;
            let a_root = dsu.find(a_flat);
            let b_root = dsu.find(b_flat);
            if a_root == b_root {
                continue;
            }
            let len = distance_sq(centroids[a_root], centroids[b_root]).sqrt();
            if len < threshold {
                dsu.union(a_root, b_root);
                merged_any = true;
            }
        }
    }

    if !merged_any {
        // No tiny edges this pass, but Phase 1 may still have tightened
        // duplicate vertices — apply the cluster-centroid rewrite so the
        // next pass (and `derive_cell_edges`) sees identical endpoints
        // across cells.
        let changed = apply_clusters(polygons, &indices, &mut dsu, &centroids);
        return changed;
    }

    centroids = cluster_centroids(&flat, &mut dsu, extent_x, extent_y);
    apply_clusters(polygons, &indices, &mut dsu, &centroids);
    true
}

fn apply_clusters(
    polygons: &mut [Vec<Point>],
    indices: &[std::ops::Range<usize>],
    dsu: &mut Dsu,
    centroids: &[Point],
) -> bool {
    let mut changed = false;
    for (poly_idx, range) in indices.iter().enumerate() {
        let mut new_points = Vec::with_capacity(range.end - range.start);
        for flat_idx in range.clone() {
            new_points.push(centroids[dsu.find(flat_idx)]);
        }
        let cleaned = clean_polygon(new_points);
        if cleaned != polygons[poly_idx] {
            changed = true;
        }
        polygons[poly_idx] = cleaned;
    }
    changed
}

/// Hashed-grid union of `points` within `eps`.
fn spatial_union(points: &[Point], eps: f32, dsu: &mut Dsu) {
    if points.is_empty() {
        return;
    }
    let cell_size = (eps * 2.0).max(EPS * 4.0);
    let mut grid: HashMap<(i32, i32), Vec<usize>> = HashMap::new();
    let key = |p: Point| {
        (
            (p.x / cell_size).floor() as i32,
            (p.y / cell_size).floor() as i32,
        )
    };
    for (idx, p) in points.iter().enumerate() {
        grid.entry(key(*p)).or_default().push(idx);
    }
    let eps_sq = eps * eps;
    for (idx, p) in points.iter().enumerate() {
        let (kx, ky) = key(*p);
        for dx in -1..=1 {
            for dy in -1..=1 {
                let Some(bucket) = grid.get(&(kx + dx, ky + dy)) else {
                    continue;
                };
                for &other in bucket {
                    if other <= idx {
                        continue;
                    }
                    if distance_sq(*p, points[other]) <= eps_sq {
                        dsu.union(idx, other);
                    }
                }
            }
        }
    }
}

/// Per-cluster centroid (mean of member positions), with the rectangle
/// boundary preserved exactly: if any cluster member sits on a frame
/// side, the centroid is snapped to that side too.
fn cluster_centroids(points: &[Point], dsu: &mut Dsu, extent_x: f32, extent_y: f32) -> Vec<Point> {
    let n = points.len();
    let mut sum_x = vec![0.0_f32; n];
    let mut sum_y = vec![0.0_f32; n];
    let mut count = vec![0_u32; n];
    let mut snap_x = vec![None::<f32>; n];
    let mut snap_y = vec![None::<f32>; n];
    for (idx, p) in points.iter().enumerate() {
        let root = dsu.find(idx);
        sum_x[root] += p.x;
        sum_y[root] += p.y;
        count[root] += 1;
        if near(p.x, 0.0, BORDER_EPS) {
            snap_x[root] = Some(0.0);
        } else if near(p.x, extent_x, BORDER_EPS) {
            snap_x[root] = Some(extent_x);
        }
        if near(p.y, 0.0, BORDER_EPS) {
            snap_y[root] = Some(0.0);
        } else if near(p.y, extent_y, BORDER_EPS) {
            snap_y[root] = Some(extent_y);
        }
    }
    let mut centroids = vec![Point::default(); n];
    for root in 0..n {
        if count[root] == 0 {
            continue;
        }
        let c = count[root] as f32;
        centroids[root] = Point {
            x: snap_x[root].unwrap_or(sum_x[root] / c),
            y: snap_y[root].unwrap_or(sum_y[root] / c),
        };
    }
    centroids
}

#[derive(Debug)]
struct Dsu {
    parent: Vec<u32>,
    rank: Vec<u8>,
}

impl Dsu {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n as u32).collect(),
            rank: vec![0; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        let mut cur = x;
        while self.parent[cur] as usize != cur {
            let next = self.parent[cur] as usize;
            self.parent[cur] = self.parent[next];
            cur = next;
        }
        cur
    }

    fn union(&mut self, a: usize, b: usize) -> bool {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return false;
        }
        let (small, large) = if self.rank[ra] < self.rank[rb] {
            (ra, rb)
        } else if self.rank[ra] > self.rank[rb] {
            (rb, ra)
        } else {
            self.rank[ra] += 1;
            (rb, ra)
        };
        self.parent[small] = large as u32;
        true
    }
}

fn initial_sites(piece_count: u32, seed: u32, extent_x: f32, extent_y: f32) -> Vec<Point> {
    let aspect = (extent_x / extent_y).max(EPS);
    let mut rows = ((piece_count as f32 / aspect).sqrt().ceil() as u32).max(1);
    let mut cols = ((piece_count + rows - 1) / rows).max(1);
    while rows.saturating_mul(cols) < piece_count {
        cols = cols.saturating_add(1);
    }
    while rows > 1 && cols.saturating_mul(rows - 1) >= piece_count {
        rows -= 1;
    }
    let cell_w = extent_x / cols as f32;
    let cell_h = extent_y / rows as f32;
    let mut sites = Vec::with_capacity(piece_count as usize);
    for row in 0..rows {
        for col in 0..cols {
            if sites.len() == piece_count as usize {
                return sites;
            }
            let id = sites.len() as u32;
            let jx = rand_range(seed, id ^ 0xA51C_E11A, -0.32, 0.32);
            let jy = rand_range(seed, id ^ 0xC0FF_EE11, -0.32, 0.32);
            sites.push(Point {
                x: ((col as f32 + 0.5 + jx) * cell_w).clamp(EPS, extent_x - EPS),
                y: ((row as f32 + 0.5 + jy) * cell_h).clamp(EPS, extent_y - EPS),
            });
        }
    }
    sites
}

fn bounded_cells(sites: &[Point], extent_x: f32, extent_y: f32) -> Vec<Vec<Point>> {
    let index = SpatialIndex::new(sites, extent_x, extent_y);
    bounded_cells_with_index(sites, extent_x, extent_y, &index)
}

fn bounded_cells_with_index(
    sites: &[Point],
    extent_x: f32,
    extent_y: f32,
    index: &SpatialIndex,
) -> Vec<Vec<Point>> {
    let rect = vec![
        Point { x: 0.0, y: 0.0 },
        Point {
            x: extent_x,
            y: 0.0,
        },
        Point {
            x: extent_x,
            y: extent_y,
        },
        Point {
            x: 0.0,
            y: extent_y,
        },
    ];
    let mut cells = Vec::with_capacity(sites.len());
    for (idx, site) in sites.iter().copied().enumerate() {
        let mut polygon = rect.clone();
        for other_idx in index.candidate_indices(site, idx) {
            if idx == other_idx {
                continue;
            }
            let other = sites[other_idx];
            polygon = clip_to_bisector(&polygon, site, other);
            if polygon.is_empty() {
                break;
            }
        }
        cells.push(clean_polygon(polygon));
    }
    cells
}

#[derive(Clone, Debug)]
struct SpatialIndex {
    cols: usize,
    rows: usize,
    bucket_size: f32,
    buckets: Vec<Vec<usize>>,
}

impl SpatialIndex {
    fn new(sites: &[Point], extent_x: f32, extent_y: f32) -> Self {
        let target_bucket_size = (extent_x * extent_y / sites.len().max(1) as f32)
            .sqrt()
            .max(EPS);
        let cols = ((extent_x / target_bucket_size).ceil() as usize).max(1);
        let rows = ((extent_y / target_bucket_size).ceil() as usize).max(1);
        let bucket_size = (extent_x / cols as f32)
            .max(extent_y / rows as f32)
            .max(EPS);
        let mut buckets = vec![Vec::new(); cols * rows];
        for (idx, site) in sites.iter().copied().enumerate() {
            let (col, row) = Self::bucket_for(site, cols, rows, bucket_size);
            buckets[row * cols + col].push(idx);
        }
        Self {
            cols,
            rows,
            bucket_size,
            buckets,
        }
    }

    fn bucket_for(point: Point, cols: usize, rows: usize, bucket_size: f32) -> (usize, usize) {
        let col = (point.x / bucket_size).floor() as isize;
        let row = (point.y / bucket_size).floor() as isize;
        (
            col.clamp(0, cols.saturating_sub(1) as isize) as usize,
            row.clamp(0, rows.saturating_sub(1) as isize) as usize,
        )
    }

    fn candidate_indices(&self, site: Point, site_idx: usize) -> Vec<usize> {
        let (col, row) = Self::bucket_for(site, self.cols, self.rows, self.bucket_size);
        let mut out = Vec::with_capacity(64);
        let max_ring = self.cols.max(self.rows);
        for ring in 0..=max_ring {
            self.collect_ring(col, row, ring, &mut out);
            if out.len() >= 32 && ring >= 3 {
                break;
            }
            if ring >= 8 && out.len() >= 12 {
                break;
            }
        }
        out.retain(|idx| *idx != site_idx);
        out.sort_unstable();
        out.dedup();
        out
    }

    fn nearby_indices(&self, point: Point, min_count: usize) -> Vec<usize> {
        let (col, row) = Self::bucket_for(point, self.cols, self.rows, self.bucket_size);
        let mut out = Vec::with_capacity(min_count.max(16));
        let max_ring = self.cols.max(self.rows);
        for ring in 0..=max_ring {
            self.collect_ring(col, row, ring, &mut out);
            if out.len() >= min_count && ring >= 2 {
                break;
            }
        }
        out.sort_unstable();
        out.dedup();
        out
    }

    fn collect_ring(&self, col: usize, row: usize, ring: usize, out: &mut Vec<usize>) {
        let min_col = col.saturating_sub(ring);
        let max_col = (col + ring).min(self.cols.saturating_sub(1));
        let min_row = row.saturating_sub(ring);
        let max_row = (row + ring).min(self.rows.saturating_sub(1));
        for r in min_row..=max_row {
            for c in min_col..=max_col {
                if ring > 0 && c > min_col && c < max_col && r > min_row && r < max_row {
                    continue;
                }
                out.extend(self.buckets[r * self.cols + c].iter().copied());
            }
        }
    }
}

fn clip_to_bisector(polygon: &[Point], site: Point, other: Point) -> Vec<Point> {
    if polygon.is_empty() {
        return Vec::new();
    }
    let nx = other.x - site.x;
    let ny = other.y - site.y;
    let limit = (other.x * other.x + other.y * other.y - site.x * site.x - site.y * site.y) * 0.5;
    let inside = |p: Point| p.x * nx + p.y * ny <= limit + EPS;
    let intersect = |a: Point, b: Point| {
        let da = a.x * nx + a.y * ny - limit;
        let db = b.x * nx + b.y * ny - limit;
        let denom = da - db;
        if denom.abs() <= EPS {
            return a;
        }
        let t = (da / denom).clamp(0.0, 1.0);
        Point {
            x: a.x + (b.x - a.x) * t,
            y: a.y + (b.y - a.y) * t,
        }
    };

    let mut out = Vec::new();
    let mut prev = *polygon.last().expect("non-empty polygon");
    let mut prev_inside = inside(prev);
    for &current in polygon {
        let current_inside = inside(current);
        if current_inside {
            if !prev_inside {
                out.push(intersect(prev, current));
            }
            out.push(current);
        } else if prev_inside {
            out.push(intersect(prev, current));
        }
        prev = current;
        prev_inside = current_inside;
    }
    clean_polygon(out)
}

fn clean_polygon(points: Vec<Point>) -> Vec<Point> {
    let mut out = Vec::with_capacity(points.len());
    for point in points {
        if out
            .last()
            .map(|prev: &Point| distance_sq(*prev, point) > EPS * EPS)
            .unwrap_or(true)
        {
            out.push(point);
        }
    }
    if out.len() > 1 && distance_sq(out[0], *out.last().unwrap()) <= EPS * EPS {
        out.pop();
    }
    out
}

fn derive_cell_edges(
    sites: Vec<Point>,
    polygons: Vec<Vec<Point>>,
    extent_x: f32,
    extent_y: f32,
) -> (Vec<VoronoiCell>, Vec<(PieceId, PieceId)>) {
    #[derive(Clone, Copy)]
    struct Candidate {
        piece: PieceId,
        edge_index: usize,
        sort_key: f32,
    }

    let index = SpatialIndex::new(&sites, extent_x, extent_y);
    let mut edge_lists: Vec<Vec<VoronoiCellEdge>> = Vec::with_capacity(polygons.len());
    let mut shared: HashMap<PairKey, Vec<Candidate>> = HashMap::new();
    for (piece_idx, polygon) in polygons.iter().enumerate() {
        let piece = PieceId(piece_idx as u32);
        let mut edges = Vec::with_capacity(polygon.len());
        for idx in 0..polygon.len() {
            let start = polygon[idx];
            let end = polygon[(idx + 1) % polygon.len()];
            if distance_sq(start, end) <= EPS * EPS {
                continue;
            }
            let is_border = segment_on_border(start, end, extent_x, extent_y);
            let edge_index = edges.len();
            edges.push(VoronoiCellEdge {
                start,
                end,
                kind: VoronoiCellEdgeKind::Border,
            });
            if !is_border {
                if let Some(neighbor) =
                    nearest_neighbor_for_edge(&sites, &index, piece_idx, start, end)
                {
                    shared
                        .entry(PairKey::new(piece, neighbor))
                        .or_default()
                        .push(Candidate {
                            piece,
                            edge_index,
                            sort_key: edge_sort_key(start, end),
                        });
                }
            }
        }
        edge_lists.push(edges);
    }

    let mut topology_edges = Vec::new();
    // HashMap iteration is non-deterministic; the order this loop runs in
    // becomes the EdgeId assignment, which gets persisted in saved
    // snapshots' `edge_active` arrays. A reloaded puzzle has to produce
    // the same EdgeId ↔ piece-pair mapping or every active edge in the
    // snapshot points at the wrong neighbors and `LogicalState::validate`
    // rejects the restore.
    let mut shared_entries: Vec<_> = shared.into_iter().collect();
    shared_entries.sort_by_key(|(pair, _)| (pair.a, pair.b));
    for (pair, candidates) in shared_entries {
        let mut a_candidates = candidates
            .iter()
            .copied()
            .filter(|candidate| candidate.piece == pair.a)
            .collect::<Vec<_>>();
        let mut b_candidates = candidates
            .iter()
            .copied()
            .filter(|candidate| candidate.piece == pair.b)
            .collect::<Vec<_>>();
        a_candidates.sort_by(|a, b| a.sort_key.total_cmp(&b.sort_key));
        b_candidates.sort_by(|a, b| a.sort_key.total_cmp(&b.sort_key));
        let pair_count = a_candidates.len().min(b_candidates.len());
        if pair_count == 0 {
            continue;
        }
        for idx in 0..pair_count {
            let a = a_candidates[idx];
            let b = b_candidates[idx];
            let edge = EdgeId(topology_edges.len() as u32);
            topology_edges.push((a.piece, b.piece));
            if let Some(cell_edges) = edge_lists.get_mut(a.piece.as_usize()) {
                if let Some(cell_edge) = cell_edges.get_mut(a.edge_index) {
                    cell_edge.kind = VoronoiCellEdgeKind::Interior(edge);
                }
            }
            if let Some(cell_edges) = edge_lists.get_mut(b.piece.as_usize()) {
                if let Some(cell_edge) = cell_edges.get_mut(b.edge_index) {
                    cell_edge.kind = VoronoiCellEdgeKind::Interior(edge);
                }
            }
        }
    }

    pair_unmatched_internal_edges(&mut edge_lists, &mut topology_edges, extent_x, extent_y);

    let cells = sites
        .into_iter()
        .zip(polygons)
        .zip(edge_lists)
        .map(|((site, polygon), edges)| {
            let touches_frame = polygon
                .iter()
                .any(|p| point_on_border(*p, extent_x, extent_y));
            VoronoiCell {
                site,
                polygon: polygon.into_boxed_slice(),
                edges: edges.into_boxed_slice(),
                touches_frame,
            }
        })
        .collect();
    (cells, topology_edges)
}

fn pair_unmatched_internal_edges(
    edge_lists: &mut [Vec<VoronoiCellEdge>],
    topology_edges: &mut Vec<(PieceId, PieceId)>,
    extent_x: f32,
    extent_y: f32,
) {
    let mut unmatched = Vec::new();
    for (piece_idx, edges) in edge_lists.iter().enumerate() {
        for (edge_index, edge) in edges.iter().enumerate() {
            if matches!(edge.kind, VoronoiCellEdgeKind::Border)
                && !segment_on_border(edge.start, edge.end, extent_x, extent_y)
            {
                unmatched.push(UnmatchedEdge {
                    piece: PieceId(piece_idx as u32),
                    edge_index,
                    start: edge.start,
                    end: edge.end,
                });
            }
        }
    }

    let mut used = vec![false; unmatched.len()];
    for idx in 0..unmatched.len() {
        if used[idx] {
            continue;
        }
        let current = unmatched[idx];
        let mut best = None;
        let mut best_score = f32::INFINITY;
        for other_idx in (idx + 1)..unmatched.len() {
            if used[other_idx] || unmatched[other_idx].piece == current.piece {
                continue;
            }
            let score = segment_match_score(current, unmatched[other_idx]);
            if score < best_score {
                best_score = score;
                best = Some(other_idx);
            }
        }
        let Some(other_idx) = best else {
            continue;
        };
        let tolerance = current_edge_len(current)
            .max(current_edge_len(unmatched[other_idx]))
            .max(1.0)
            * 0.08;
        if best_score.sqrt() > tolerance {
            continue;
        }
        let other = unmatched[other_idx];
        let edge = EdgeId(topology_edges.len() as u32);
        topology_edges.push((current.piece, other.piece));
        if let Some(piece_edges) = edge_lists.get_mut(current.piece.as_usize()) {
            if let Some(piece_edge) = piece_edges.get_mut(current.edge_index) {
                piece_edge.kind = VoronoiCellEdgeKind::Interior(edge);
            }
        }
        if let Some(piece_edges) = edge_lists.get_mut(other.piece.as_usize()) {
            if let Some(piece_edge) = piece_edges.get_mut(other.edge_index) {
                piece_edge.kind = VoronoiCellEdgeKind::Interior(edge);
            }
        }
        used[idx] = true;
        used[other_idx] = true;
    }
}

#[derive(Clone, Copy)]
struct UnmatchedEdge {
    piece: PieceId,
    edge_index: usize,
    start: Point,
    end: Point,
}

fn segment_match_score(a: UnmatchedEdge, b: UnmatchedEdge) -> f32 {
    let forward = distance_sq(a.start, b.start) + distance_sq(a.end, b.end);
    let reversed = distance_sq(a.start, b.end) + distance_sq(a.end, b.start);
    forward.min(reversed)
}

fn current_edge_len(edge: UnmatchedEdge) -> f32 {
    distance_sq(edge.start, edge.end).sqrt()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct PairKey {
    a: PieceId,
    b: PieceId,
}

impl PairKey {
    fn new(a: PieceId, b: PieceId) -> Self {
        if a <= b {
            Self { a, b }
        } else {
            Self { a: b, b: a }
        }
    }
}

fn nearest_neighbor_for_edge(
    sites: &[Point],
    index: &SpatialIndex,
    owner_idx: usize,
    start: Point,
    end: Point,
) -> Option<PieceId> {
    let midpoint = Point {
        x: f32::midpoint(start.x, end.x),
        y: f32::midpoint(start.y, end.y),
    };
    let owner = *sites.get(owner_idx)?;
    let owner_start_d2 = distance_sq(start, owner);
    let owner_mid_d2 = distance_sq(midpoint, owner);
    let owner_end_d2 = distance_sq(end, owner);
    let mut best = None;
    let mut best_score = f32::INFINITY;
    for candidate_idx in index.nearby_indices(midpoint, 64) {
        if candidate_idx == owner_idx {
            continue;
        }
        let Some(candidate) = sites.get(candidate_idx).copied() else {
            continue;
        };
        let start_d2 = distance_sq(start, candidate);
        let mid_d2 = distance_sq(midpoint, candidate);
        let end_d2 = distance_sq(end, candidate);
        let score = (start_d2 - owner_start_d2).abs()
            + (mid_d2 - owner_mid_d2).abs() * 2.0
            + (end_d2 - owner_end_d2).abs()
            + mid_d2 * 1.0e-5;
        if score < best_score {
            best_score = score;
            best = Some(PieceId(candidate_idx as u32));
        }
    }
    best
}

fn edge_sort_key(start: Point, end: Point) -> f32 {
    let dx = (end.x - start.x).abs();
    let dy = (end.y - start.y).abs();
    if dx >= dy {
        (start.x + end.x) * 0.5
    } else {
        (start.y + end.y) * 0.5
    }
}

fn build_rotation_candidates(layout: &VoronoiLayout) -> Box<[Box<[AngleDeg]>]> {
    layout
        .cells
        .iter()
        .map(|cell| {
            if cell.touches_frame {
                return boxed_angles(&[90.0, 180.0, 270.0]);
            }

            let median_len = median_edge_len(cell);
            let min_len = (median_len * ROTATION_EDGE_MIN_RATIO).max(EPS);
            let mut edge_angles = Vec::new();
            for edge in cell.edges.iter() {
                let len = distance_sq(edge.start, edge.end).sqrt();
                if len < min_len {
                    continue;
                }
                edge_angles.push((len, line_angle_degrees(edge.start, edge.end)));
            }
            edge_angles.sort_by(|a, b| b.0.total_cmp(&a.0));

            let mut values = Vec::new();
            if let Some((_, reference)) = edge_angles.first().copied() {
                for (_, other) in edge_angles.iter().copied().skip(1) {
                    push_angle_value(&mut values, reference - other);
                }
            }

            if values.is_empty() {
                boxed_angles(&[180.0])
            } else {
                values.sort_by(|a, b| a.total_cmp(b));
                values
                    .into_iter()
                    .filter_map(AngleDeg::try_new)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            }
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn boxed_angles(values: &[f32]) -> Box<[AngleDeg]> {
    values
        .iter()
        .copied()
        .filter_map(AngleDeg::try_new)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn median_edge_len(cell: &VoronoiCell) -> f32 {
    let mut lengths = cell
        .edges
        .iter()
        .map(|edge| distance_sq(edge.start, edge.end).sqrt())
        .filter(|len| *len > EPS)
        .collect::<Vec<_>>();
    if lengths.is_empty() {
        return 1.0;
    }
    lengths.sort_by(|a, b| a.total_cmp(b));
    lengths[lengths.len() / 2]
}

fn line_angle_degrees(start: Point, end: Point) -> f32 {
    normalize_half_turn((end.y - start.y).atan2(end.x - start.x).to_degrees())
}

fn normalize_half_turn(mut angle: f32) -> f32 {
    angle %= 180.0;
    if angle < 0.0 {
        angle += 180.0;
    }
    if angle <= 1.0e-3 || (180.0 - angle) <= 1.0e-3 {
        0.0
    } else {
        angle
    }
}

fn normalize_full_turn(mut angle: f32) -> f32 {
    angle %= 360.0;
    if angle < 0.0 {
        angle += 360.0;
    }
    if angle <= 1.0e-3 || (360.0 - angle) <= 1.0e-3 {
        0.0
    } else {
        angle
    }
}

fn push_angle_value(values: &mut Vec<f32>, value: f32) {
    let value = normalize_full_turn(value);
    if value <= 1.0e-3 {
        return;
    }
    if values
        .iter()
        .any(|existing| shortest_angle_distance(*existing, value).abs() <= 1.0)
    {
        return;
    }
    values.push(value);
}

fn shortest_angle_distance(a: f32, b: f32) -> f32 {
    let mut diff = normalize_full_turn(b - a);
    if diff > 180.0 {
        diff -= 360.0;
    }
    diff
}

fn build_voronoi_atlas(
    topology: &VoronoiTopology,
    pose_unit_px: [f32; 2],
    shape_seed: u32,
    settings: &GridShapeSettings,
    corner_radius_px: f32,
) -> Option<ShapeAtlasMm> {
    let edge_sizing = VoronoiEdgeSizing {
        typical_len: typical_interior_edge_len_px(topology, pose_unit_px).max(EPS),
    };

    // Cache the canonical (side A) shaped path per interior edge so both
    // pieces that share the edge see the same connector — side B uses
    // the reverse-traversal of the same path.
    let mut interior_paths = HashMap::<EdgeId, PathMm>::new();
    let mut builder = ShapeAtlasBuilder::new(topology);

    for (piece_idx, cell) in topology.layout.cells.iter().enumerate() {
        let piece = PieceId(piece_idx as u32);
        let mut edges = Vec::with_capacity(cell.edges.len());
        for (edge_idx, edge) in cell.edges.iter().enumerate() {
            match edge.kind {
                VoronoiCellEdgeKind::Interior(edge_id) => {
                    let endpoints = topology
                        .layout
                        .topology_edges
                        .get(edge_id.as_usize())
                        .copied()?;
                    let on_side_a = endpoints.0 == piece;
                    let path = interior_paths
                        .entry(edge_id)
                        .or_insert_with(|| {
                            // Always shape from endpoint A's perspective so
                            // side B can reuse the same path by reversal.
                            let (start, end) = if on_side_a {
                                (edge.start, edge.end)
                            } else {
                                (edge.end, edge.start)
                            };
                            shaped_edge_path(
                                scale_point(start, pose_unit_px),
                                scale_point(end, pose_unit_px),
                                shape_seed ^ edge_id.as_u32().wrapping_mul(0x85EB_CA6B),
                                settings,
                                edge_sizing,
                            )
                        })
                        .clone();
                    let oriented = if on_side_a { path } else { reverse_path(&path) };
                    let side_geometry =
                        side_geometry_from_path(oriented, edge.start, edge.end, pose_unit_px);
                    edges.push(PieceEdgeBuilderSpec::Interior {
                        edge: edge_id,
                        side_geometry,
                    });
                }
                VoronoiCellEdgeKind::Border => {
                    let border_path = border_edge_path(
                        cell,
                        edge_idx,
                        topology.layout.extent_x,
                        topology.layout.extent_y,
                        pose_unit_px,
                        corner_radius_px,
                    );
                    let start = border_path.start;
                    let end = path_end_point(&border_path);
                    let side_geometry = EdgeSideGeometryMm {
                        path: border_path,
                        connection_point: point_mm(
                            (start.x_mm() + end.x_mm()) * 0.5,
                            (start.y_mm() + end.y_mm()) * 0.5,
                        ),
                    };
                    // Pack `side_label` into the high digits of the sort
                    // key so border edges remain grouped by frame side
                    // (top / right / bottom / left) and ordered by
                    // position within each side.
                    let side_label = border_side(
                        edge.start,
                        edge.end,
                        topology.layout.extent_x,
                        topology.layout.extent_y,
                    )
                    .unwrap_or(4);
                    let position_key = if side_label < 4 {
                        border_sort_key(edge.start, edge.end, side_label)
                    } else {
                        piece.as_u32() as f32 + edge_idx as f32 * 1.0e-3
                    };
                    // `(side_label * LARGE_OFFSET) + position_key` keeps
                    // every side-label's keys in a disjoint band; the
                    // builder's stable sort then orders within the band
                    // by position. Total extent is bounded by
                    // `topology.layout.extent_{x,y}`, so any constant
                    // larger than that suffices for the offset.
                    let band_offset = (topology.layout.extent_x
                        + topology.layout.extent_y
                        + topology.piece_count() as f32
                        + 1.0)
                        * 16.0;
                    let sort_key = side_label as f32 * band_offset + position_key;
                    edges.push(PieceEdgeBuilderSpec::Border {
                        side_geometry,
                        frame_sort_key: sort_key,
                    });
                }
            }
        }
        builder.push_piece(piece, edges).ok()?;
    }
    builder.build().ok()
}

fn border_edge_path(
    cell: &VoronoiCell,
    edge_idx: usize,
    extent_x: f32,
    extent_y: f32,
    pose_unit_px: [f32; 2],
    radius: f32,
) -> PathMm {
    let edge = cell.edges[edge_idx];
    let prev = cell
        .edges
        .get((edge_idx + cell.edges.len().saturating_sub(1)) % cell.edges.len().max(1))
        .copied();
    let next = cell
        .edges
        .get((edge_idx + 1) % cell.edges.len().max(1))
        .copied();
    let mut start = scale_point(edge.start, pose_unit_px);
    let mut end = scale_point(edge.end, pose_unit_px);

    let start_corner = border_corner(edge.start, extent_x, extent_y);
    let end_corner = border_corner(edge.end, extent_x, extent_y);
    if radius > 0.0 {
        if start_corner.is_some()
            && prev
                .map(|prev| {
                    matches!(prev.kind, VoronoiCellEdgeKind::Border)
                        && distance_sq(prev.end, edge.start) <= BORDER_EPS * BORDER_EPS
                })
                .unwrap_or(false)
        {
            start = trim_point(
                scale_point(edge.start, pose_unit_px),
                scale_point(edge.end, pose_unit_px),
                radius,
            );
        }
        if end_corner.is_some()
            && next
                .map(|next| {
                    matches!(next.kind, VoronoiCellEdgeKind::Border)
                        && distance_sq(next.start, edge.end) <= BORDER_EPS * BORDER_EPS
                })
                .unwrap_or(false)
        {
            end = trim_point(
                scale_point(edge.end, pose_unit_px),
                scale_point(edge.start, pose_unit_px),
                radius,
            );
        }
    }

    let mut segs = vec![PathSegMm::LineTo {
        to: point_mm(end.x, end.y),
    }];
    if let (Some(corner), Some(next_edge)) = (end_corner, next) {
        if matches!(next_edge.kind, VoronoiCellEdgeKind::Border)
            && distance_sq(next_edge.start, edge.end) <= BORDER_EPS * BORDER_EPS
        {
            let next_start = trim_point(
                scale_point(next_edge.start, pose_unit_px),
                scale_point(next_edge.end, pose_unit_px),
                radius,
            );
            for point in rounded_corner_points(
                corner,
                end,
                next_start,
                extent_x * pose_unit_px[0],
                extent_y * pose_unit_px[1],
                radius,
            ) {
                segs.push(PathSegMm::LineTo {
                    to: point_mm(point.x, point.y),
                });
            }
        }
    }

    PathMm::new(point_mm(start.x, start.y), segs.into_boxed_slice(), false)
}

fn trim_point(from: Point, toward: Point, radius: f32) -> Point {
    let len = distance_sq(from, toward).sqrt();
    if len <= EPS {
        return from;
    }
    let trim = radius.min(len * 0.4);
    let t = trim / len;
    Point {
        x: from.x + (toward.x - from.x) * t,
        y: from.y + (toward.y - from.y) * t,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RectCorner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

fn border_corner(point: Point, extent_x: f32, extent_y: f32) -> Option<RectCorner> {
    if near(point.x, 0.0, BORDER_EPS) && near(point.y, 0.0, BORDER_EPS) {
        Some(RectCorner::TopLeft)
    } else if near(point.x, extent_x, BORDER_EPS) && near(point.y, 0.0, BORDER_EPS) {
        Some(RectCorner::TopRight)
    } else if near(point.x, extent_x, BORDER_EPS) && near(point.y, extent_y, BORDER_EPS) {
        Some(RectCorner::BottomRight)
    } else if near(point.x, 0.0, BORDER_EPS) && near(point.y, extent_y, BORDER_EPS) {
        Some(RectCorner::BottomLeft)
    } else {
        None
    }
}

fn rounded_corner_points(
    corner: RectCorner,
    from: Point,
    to: Point,
    width: f32,
    height: f32,
    radius: f32,
) -> Vec<Point> {
    if radius <= EPS {
        return vec![to];
    }
    let center = match corner {
        RectCorner::TopLeft => Point {
            x: radius,
            y: radius,
        },
        RectCorner::TopRight => Point {
            x: width - radius,
            y: radius,
        },
        RectCorner::BottomRight => Point {
            x: width - radius,
            y: height - radius,
        },
        RectCorner::BottomLeft => Point {
            x: radius,
            y: height - radius,
        },
    };
    let a0 = (from.y - center.y).atan2(from.x - center.x);
    let mut delta = (to.y - center.y).atan2(to.x - center.x) - a0;
    while delta > PI {
        delta -= 2.0 * PI;
    }
    while delta < -PI {
        delta += 2.0 * PI;
    }
    let steps = 5;
    let mut points = Vec::with_capacity(steps);
    for step in 1..=steps {
        let angle = a0 + delta * (step as f32 / steps as f32);
        points.push(Point {
            x: center.x + radius * angle.cos(),
            y: center.y + radius * angle.sin(),
        });
    }
    if let Some(last) = points.last_mut() {
        *last = to;
    }
    points
}

#[derive(Clone, Copy, Debug)]
struct VoronoiEdgeSizing {
    typical_len: f32,
}

fn typical_interior_edge_len_px(topology: &VoronoiTopology, pose_unit_px: [f32; 2]) -> f32 {
    let mut lengths = Vec::new();
    for cell in topology.layout.cells.iter() {
        for edge in cell.edges.iter() {
            if matches!(edge.kind, VoronoiCellEdgeKind::Interior(_)) {
                let start = scale_point(edge.start, pose_unit_px);
                let end = scale_point(edge.end, pose_unit_px);
                lengths.push(distance_sq(start, end).sqrt());
            }
        }
    }
    if lengths.is_empty() {
        let area = (topology.layout.extent_x * pose_unit_px[0])
            * (topology.layout.extent_y * pose_unit_px[1]);
        return (area / topology.piece_count().max(1) as f32).sqrt();
    }
    lengths.sort_by(|a, b| a.total_cmp(b));
    lengths[lengths.len() / 2]
}

fn shaped_edge_path(
    start: Point,
    end: Point,
    seed: u32,
    settings: &GridShapeSettings,
    sizing: VoronoiEdgeSizing,
) -> PathMm {
    let len = distance_sq(start, end).sqrt().max(EPS);
    let sign = if rand_unit(seed, 8) < 0.5 { 1 } else { -1 };
    let Some(connector) = connector_shape(seed, settings, len, sizing) else {
        return map_connector_path(start, end, &[ConnectorSeg::LineTo { to: (len, 0.0) }]);
    };
    let depth_limit = len * settings.tab_depth_cap.clamp(0.05, 0.35);
    let input = EdgeProfileInput {
        len_mm: len,
        depth_base_mm: len,
        depth_limit_mm: depth_limit,
        sign,
        connector: Some(connector),
    };
    let segments = build_edge_profile_segments(settings.edge_style, &input);
    map_connector_path(start, end, &segments)
}

fn connector_shape(
    seed: u32,
    settings: &GridShapeSettings,
    edge_len: f32,
    sizing: VoronoiEdgeSizing,
) -> Option<ConnectorShape> {
    let variation = settings.variation.clamp(0.0, 1.0);
    let jitter = |salt: u32, base: f32, range: f32, min: f32, max: f32| {
        let value = base + (rand_unit(seed, salt) * 2.0 - 1.0) * range * variation;
        value.clamp(min, max)
    };
    let tab_size_raw = jitter(0, settings.tab_width, 0.16, 0.2, 0.72);
    let tab_depth = jitter(2, settings.tab_depth, 0.35, 0.2, 1.1);
    let tab_size_scale = settings.tab_size_scale.clamp(0.1, 0.5);
    let tab_size_min = settings.tab_size_min.clamp(0.02, 0.24);
    let tab_size_max = settings.tab_size_max.clamp(tab_size_min, 0.24);
    let typical_tab_size = (tab_size_raw * tab_size_scale).clamp(tab_size_min, tab_size_max);
    let target_tab_len = typical_tab_size * sizing.typical_len;
    if edge_len < sizing.typical_len * VORONOI_TAB_MIN_EDGE_RATIO {
        return None;
    }
    let tab_size = (target_tab_len / edge_len).clamp(VORONOI_TAB_SIZE_MIN, VORONOI_TAB_SIZE_MAX);
    let jitter_strength = settings.jitter_strength.clamp(0.0, 0.3);
    let jitter_base = (variation * jitter_strength).clamp(0.0, jitter_strength);
    let jitter_depth = jitter_base * tab_depth;
    let jitter_len = jitter_base * settings.jitter_len_bias.clamp(0.0, 1.0);
    Some(ConnectorShape {
        tab_size,
        tab_depth,
        a: rand_range(seed, 3, -jitter_depth, jitter_depth),
        b: rand_range(seed, 4, -jitter_len, jitter_len),
        c: rand_range(seed, 5, -jitter_depth, jitter_depth),
        d: rand_range(seed, 6, -jitter_len, jitter_len),
        e: rand_range(seed, 7, -jitter_depth, jitter_depth),
    })
}

fn map_connector_path(start: Point, end: Point, segments: &[ConnectorSeg]) -> PathMm {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    let len = (dx * dx + dy * dy).sqrt().max(EPS);
    let tx = dx / len;
    let ty = dy / len;
    let nx = -ty;
    let ny = tx;
    let map = |x: f32, y: f32| Point {
        x: start.x + tx * x + nx * y,
        y: start.y + ty * x + ny * y,
    };
    let mut path_segs = Vec::with_capacity(segments.len());
    for segment in segments {
        match *segment {
            ConnectorSeg::LineTo { to } => {
                let p = map(to.0, to.1);
                path_segs.push(PathSegMm::LineTo {
                    to: point_mm(p.x, p.y),
                });
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let c1 = map(c1.0, c1.1);
                let c2 = map(c2.0, c2.1);
                let to = map(to.0, to.1);
                path_segs.push(PathSegMm::CubicTo {
                    c1: point_mm(c1.x, c1.y),
                    c2: point_mm(c2.x, c2.y),
                    to: point_mm(to.x, to.y),
                });
            }
        }
    }
    PathMm::new(
        point_mm(start.x, start.y),
        path_segs.into_boxed_slice(),
        false,
    )
}

fn side_geometry_from_path(
    mut path: PathMm,
    canonical_start: Point,
    canonical_end: Point,
    pose_unit_px: [f32; 2],
) -> EdgeSideGeometryMm {
    let start = scale_point(canonical_start, pose_unit_px);
    let end = scale_point(canonical_end, pose_unit_px);
    path.start = point_mm(start.x, start.y);
    set_path_end_point(&mut path, point_mm(end.x, end.y));
    EdgeSideGeometryMm {
        connection_point: path_midpoint(&path),
        path,
    }
}

fn reverse_path(path: &PathMm) -> PathMm {
    let mut states = Vec::with_capacity(path.segs.len());
    let mut current = path.start;
    for seg in path.segs.iter() {
        let end = match seg {
            PathSegMm::LineTo { to } => *to,
            PathSegMm::CubicTo { to, .. } => *to,
        };
        states.push((current, seg.clone(), end));
        current = end;
    }
    let start = current;
    let mut segs = Vec::with_capacity(states.len());
    for (segment_start, segment, _) in states.into_iter().rev() {
        match segment {
            PathSegMm::LineTo { .. } => segs.push(PathSegMm::LineTo { to: segment_start }),
            PathSegMm::CubicTo { c1, c2, .. } => segs.push(PathSegMm::CubicTo {
                c1: c2,
                c2: c1,
                to: segment_start,
            }),
        }
    }
    PathMm::new(start, segs.into_boxed_slice(), false)
}

fn set_path_end_point(path: &mut PathMm, target: PointMm) {
    if let Some(last) = path.segs.last_mut() {
        match last {
            PathSegMm::LineTo { to } => *to = target,
            PathSegMm::CubicTo { to, .. } => *to = target,
        }
    }
}

fn path_midpoint(path: &PathMm) -> PointMm {
    let end = path_end_point(path);
    point_mm(
        f32::midpoint(path.start.x_mm(), end.x_mm()),
        f32::midpoint(path.start.y_mm(), end.y_mm()),
    )
}

fn path_end_point(path: &PathMm) -> PointMm {
    let mut current = path.start;
    for seg in path.segs.iter() {
        current = match seg {
            PathSegMm::LineTo { to } => *to,
            PathSegMm::CubicTo { to, .. } => *to,
        };
    }
    current
}

fn scale_point(point: Point, pose_unit_px: [f32; 2]) -> Point {
    Point {
        x: point.x * pose_unit_px[0],
        y: point.y * pose_unit_px[1],
    }
}

fn point_mm(x: f32, y: f32) -> PointMm {
    PointMm {
        x: LengthMm::try_new(x).unwrap_or_default(),
        y: LengthMm::try_new(y).unwrap_or_default(),
    }
}

fn voronoi_mask_pad_px(image_width: u32, image_height: u32, settings: &GridShapeSettings) -> f32 {
    let cap = settings.tab_depth_cap.clamp(0.05, 0.35);
    (image_width.max(image_height) as f32 * cap * 0.04)
        .ceil()
        .max(2.0)
}

fn layout_score(polygons: &[Vec<Point>], piece_count: f32, extent_x: f32, extent_y: f32) -> f32 {
    let target_area = extent_x * extent_y / piece_count.max(1.0);
    let mut score = 0.0;
    for polygon in polygons {
        let area = polygon_area(polygon).abs();
        if area <= EPS {
            score += 1_000_000.0;
            continue;
        }
        let ratio = area / target_area;
        score += (ratio.ln()).abs() * 20.0;
        let perimeter = polygon_perimeter(polygon).max(EPS);
        let compactness = 4.0 * std::f32::consts::PI * area / (perimeter * perimeter);
        score += (1.0 - compactness.clamp(0.0, 1.0)) * 6.0;
        let (w, h) = polygon_bbox(polygon);
        let aspect = (w / h.max(EPS)).max(h / w.max(EPS));
        score += (aspect - 1.0).max(0.0) * 0.4;
    }
    score
}

fn polygon_area(points: &[Point]) -> f32 {
    if points.len() < 3 {
        return 0.0;
    }
    let mut sum = 0.0;
    for idx in 0..points.len() {
        let a = points[idx];
        let b = points[(idx + 1) % points.len()];
        sum += a.x * b.y - b.x * a.y;
    }
    sum * 0.5
}

fn polygon_centroid(points: &[Point]) -> Option<Point> {
    if points.len() < 3 {
        return None;
    }
    let mut cross_sum = 0.0;
    let mut cx = 0.0;
    let mut cy = 0.0;
    for idx in 0..points.len() {
        let a = points[idx];
        let b = points[(idx + 1) % points.len()];
        let cross = a.x * b.y - b.x * a.y;
        cross_sum += cross;
        cx += (a.x + b.x) * cross;
        cy += (a.y + b.y) * cross;
    }
    if cross_sum.abs() <= EPS {
        return None;
    }
    Some(Point {
        x: cx / (3.0 * cross_sum),
        y: cy / (3.0 * cross_sum),
    })
}

fn polygon_perimeter(points: &[Point]) -> f32 {
    if points.len() < 2 {
        return 0.0;
    }
    let mut sum = 0.0;
    for idx in 0..points.len() {
        sum += distance_sq(points[idx], points[(idx + 1) % points.len()]).sqrt();
    }
    sum
}

fn polygon_bbox(points: &[Point]) -> (f32, f32) {
    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;
    for p in points {
        min_x = min_x.min(p.x);
        min_y = min_y.min(p.y);
        max_x = max_x.max(p.x);
        max_y = max_y.max(p.y);
    }
    ((max_x - min_x).max(EPS), (max_y - min_y).max(EPS))
}

fn distance_sq(a: Point, b: Point) -> f32 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    dx * dx + dy * dy
}

fn segment_on_border(a: Point, b: Point, extent_x: f32, extent_y: f32) -> bool {
    (near(a.x, 0.0, BORDER_EPS) && near(b.x, 0.0, BORDER_EPS))
        || (near(a.x, extent_x, BORDER_EPS) && near(b.x, extent_x, BORDER_EPS))
        || (near(a.y, 0.0, BORDER_EPS) && near(b.y, 0.0, BORDER_EPS))
        || (near(a.y, extent_y, BORDER_EPS) && near(b.y, extent_y, BORDER_EPS))
}

fn point_on_border(p: Point, extent_x: f32, extent_y: f32) -> bool {
    near(p.x, 0.0, BORDER_EPS)
        || near(p.x, extent_x, BORDER_EPS)
        || near(p.y, 0.0, BORDER_EPS)
        || near(p.y, extent_y, BORDER_EPS)
}

fn border_side(a: Point, b: Point, extent_x: f32, extent_y: f32) -> Option<usize> {
    if near(a.y, 0.0, BORDER_EPS) && near(b.y, 0.0, BORDER_EPS) {
        Some(0)
    } else if near(a.x, extent_x, BORDER_EPS) && near(b.x, extent_x, BORDER_EPS) {
        Some(1)
    } else if near(a.y, extent_y, BORDER_EPS) && near(b.y, extent_y, BORDER_EPS) {
        Some(2)
    } else if near(a.x, 0.0, BORDER_EPS) && near(b.x, 0.0, BORDER_EPS) {
        Some(3)
    } else {
        None
    }
}

fn border_sort_key(a: Point, b: Point, side: usize) -> f32 {
    match side {
        0 => (a.x + b.x) * 0.5,
        1 => (a.y + b.y) * 0.5,
        2 => -((a.x + b.x) * 0.5),
        3 => -((a.y + b.y) * 0.5),
        _ => 0.0,
    }
}

fn near(a: f32, b: f32, eps: f32) -> bool {
    (a - b).abs() <= eps
}

fn splitmix32(mut value: u32) -> u32 {
    value = value.wrapping_add(0x9E37_79B9);
    let mut z = value;
    z = (z ^ (z >> 16)).wrapping_mul(0x85EB_CA6B);
    z = (z ^ (z >> 13)).wrapping_mul(0xC2B2_AE35);
    z ^ (z >> 16)
}

fn rand_unit(seed: u32, salt: u32) -> f32 {
    let mixed = splitmix32(seed ^ salt);
    (mixed >> 8) as f32 / ((1u32 << 24) as f32)
}

fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::topology::build_topology_from_spec;

    #[test]
    fn voronoi_round_trips_through_spec() {
        let topology = VoronoiTopology::try_new(7, 42, 16.0 / 9.0).expect("topology");
        let spec = <VoronoiTopology as SerializableTopology>::to_spec(&topology);
        assert_eq!(spec.tag, VoronoiTopology::TAG);
        assert_eq!(spec.payload.len(), 12);
        let rebuilt = VoronoiTopology::read_payload(&spec.payload).expect("rebuilt");
        assert_eq!(rebuilt.piece_count.get(), 7);
        assert_eq!(rebuilt.seed, 42);
        assert!((rebuilt.aspect_ratio() - 16.0 / 9.0).abs() < 1.0e-6);
    }

    #[test]
    fn voronoi_read_payload_rejects_wrong_size() {
        // 16-byte intermediate format and 8-byte canary format must NOT
        // be silently accepted by `read_payload` for the canonical tag —
        // the legacy path is a separate sentinel.
        assert!(VoronoiTopology::read_payload(&[0u8; 16]).is_none());
        assert!(VoronoiTopology::read_payload(&[0u8; 8]).is_none());
    }

    #[test]
    fn voronoi_read_payload_rejects_non_finite_aspect() {
        let mut bytes = Vec::with_capacity(12);
        bytes.extend_from_slice(&5u32.to_le_bytes());
        bytes.extend_from_slice(&0u32.to_le_bytes());
        bytes.extend_from_slice(&f32::NAN.to_bits().to_le_bytes());
        assert!(VoronoiTopology::read_payload(&bytes).is_none());
    }

    #[test]
    fn legacy_voronoi_canary_payload_still_decodes() {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&5u32.to_le_bytes());
        bytes.extend_from_slice(&11u32.to_le_bytes());
        let rebuilt = VoronoiTopology::read_legacy_payload(&bytes).expect("legacy");
        assert_eq!(rebuilt.piece_count(), 5);
        assert_eq!(rebuilt.seed(), 11);
        assert!((rebuilt.aspect_ratio() - 1.0).abs() < 1.0e-6);
    }

    #[test]
    fn build_topology_from_spec_accepts_voronoi() {
        let topology =
            build_topology_from_spec(&TopologySpec::voronoi(9, 3, 4.0 / 3.0)).expect("topology");
        assert_eq!(topology.piece_count(), 9);
        assert!(topology.edge_count() > 0);
    }

    #[test]
    fn build_topology_from_spec_accepts_legacy_voronoi_canary() {
        let mut payload = Vec::new();
        payload.extend_from_slice(&6u32.to_le_bytes());
        payload.extend_from_slice(&4u32.to_le_bytes());
        let topology = build_topology_from_spec(&TopologySpec {
            tag: LEGACY_TAG.to_string(),
            payload,
        })
        .expect("legacy topology");
        assert_eq!(topology.to_spec().tag, VoronoiTopology::TAG);
        assert_eq!(topology.piece_count(), 6);
    }

    #[test]
    fn voronoi_topology_edges_are_valid_and_relative_pose_uses_sites() {
        let topology = VoronoiTopology::try_new(16, 5, 4.0 / 3.0).expect("topology");
        assert_eq!(topology.piece_count(), 16);
        assert!(topology.edge_count() > 0);
        for idx in 0..topology.edge_count() {
            let (a, b) = topology.edge_endpoints(EdgeId(idx));
            assert!(a.as_u32() < topology.piece_count());
            assert!(b.as_u32() < topology.piece_count());
            assert_ne!(a, b);
            let rel = topology.expected_relative_pose(a, b);
            let (ax, ay) = topology.canonical_site(a).unwrap();
            let (bx, by) = topology.canonical_site(b).unwrap();
            assert!((rel.dx.as_mm_f32() - (bx - ax)).abs() < 1.0e-4);
            assert!((rel.dy.as_mm_f32() - (by - ay)).abs() < 1.0e-4);
        }
    }

    #[test]
    fn voronoi_rotation_candidates_use_frame_and_edge_angles() {
        let topology = VoronoiTopology::try_new(36, 19, 1.6).expect("topology");
        let mut saw_frame_piece = false;
        let mut saw_interior_piece_with_irregular_angles = false;

        for idx in 0..topology.piece_count() {
            let piece = PieceId(idx);
            let angles = topology.symmetry_angles(piece);
            if topology.is_frame_border_piece(piece) {
                saw_frame_piece = true;
                assert_eq!(angle_values(angles), vec![90.0, 180.0, 270.0]);
            } else if !angles.is_empty() {
                let values = angle_values(angles);
                let non_tiny_edges = non_tiny_edge_count(topology.cell(piece).unwrap());
                assert!(values.len() <= non_tiny_edges.max(1));
                if values.iter().any(|value| {
                    (*value - 90.0).abs() > 1.0
                        && (*value - 180.0).abs() > 1.0
                        && (*value - 270.0).abs() > 1.0
                }) {
                    saw_interior_piece_with_irregular_angles = true;
                }
            }
        }

        assert!(saw_frame_piece);
        assert!(saw_interior_piece_with_irregular_angles);
    }

    #[test]
    fn voronoi_render_geometry_has_one_piece_per_cell() {
        let topology = VoronoiTopology::try_new(9, 1, 1.0).expect("topology");
        let geom = topology
            .build_render_geometry(300, 300, 0, &GridShapeSettings::default())
            .expect("render geometry");
        assert_eq!(geom.pieces.len(), 9);
        let mut edge_refs = vec![0u32; topology.edge_count() as usize];
        for piece in &geom.pieces {
            assert!(piece.bounds_px.width > 0.0);
            assert!(piece.bounds_px.height > 0.0);
            assert_eq!(piece.edge_svgs.len(), piece.topology_edges.len());
            for edge in piece.topology_edges.iter().flatten() {
                edge_refs[edge.as_usize()] += 1;
            }
            let (site_x, site_y) = topology.canonical_site(piece.id).unwrap();
            let pose = Pose2::try_from_mm_degrees(site_x, site_y, 0.0).unwrap();
            let top_left = geom.pose_to_piece_top_left(piece.id, pose).unwrap();
            assert!((top_left.0 - piece.image_origin_px[0]).abs() < 1.0e-3);
            assert!((top_left.1 - piece.image_origin_px[1]).abs() < 1.0e-3);
        }
        assert!(edge_refs.iter().all(|count| *count == 2));
    }

    #[test]
    fn voronoi_builds_render_geometry_for_supported_count_samples() {
        for count in [50, 51, 97, 333, 997, 5000] {
            let topology = VoronoiTopology::try_new(count, 23, 1280.0 / 720.0).expect("topology");
            assert_eq!(topology.piece_count(), count);
            let geom = topology
                .build_render_geometry(1280, 720, 0, &GridShapeSettings::default())
                .unwrap_or_else(|| panic!("render geometry for count {count}"));
            assert_eq!(geom.pieces.len(), count as usize);
            assert!(topology.edge_count() >= count.saturating_sub(1));
            for piece in &geom.pieces {
                assert_eq!(piece.edge_svgs.len(), piece.topology_edges.len());
            }
        }
    }

    #[test]
    fn voronoi_quality_regression_for_common_aspects() {
        for ratio in [1.0_f32, 3.0, 1.0 / 3.0] {
            let topology = VoronoiTopology::try_new(24, 17, ratio).expect("topology");
            let target = topology.layout.extent_x * topology.layout.extent_y / 24.0;
            for cell in topology.layout.cells.iter() {
                let area = polygon_area(&cell.polygon).abs();
                assert!(
                    area > target * 0.22,
                    "tiny cell area {area} target {target}"
                );
                assert!(area < target * 2.8, "huge cell area {area} target {target}");
                let (bw, bh) = polygon_bbox(&cell.polygon);
                let aspect = (bw / bh.max(EPS)).max(bh / bw.max(EPS));
                assert!(aspect < 8.0, "oblong cell aspect {aspect}");
            }
        }
    }

    fn angle_values(angles: &[AngleDeg]) -> Vec<f32> {
        let mut values = angles
            .iter()
            .map(|angle| (angle.as_degrees_f32() * 10.0).round() / 10.0)
            .collect::<Vec<_>>();
        values.sort_by(|a, b| a.total_cmp(b));
        values
    }

    fn non_tiny_edge_count(cell: &VoronoiCell) -> usize {
        let median_len = median_edge_len(cell);
        let min_len = (median_len * ROTATION_EDGE_MIN_RATIO).max(EPS);
        cell.edges
            .iter()
            .filter(|edge| distance_sq(edge.start, edge.end).sqrt() >= min_len)
            .count()
    }

    #[test]
    fn voronoi_topology_edges_are_deterministic_across_builds() {
        // `derive_cell_edges` used to iterate a HashMap when assigning
        // EdgeIds, which made the EdgeId ↔ piece-pair mapping vary across
        // wasm reloads (HashMap RandomState reseeds per process). Saved
        // snapshots persist `edge_active` indexed by EdgeId, so a flaky
        // mapping silently invalidates every restored Voronoi game. The
        // sort-by-pair in `derive_cell_edges` pins this down; this test
        // is the regression guard.
        for (count, seed, aspect) in [(40, 7, 1.0_f32), (60, 13, 4.0 / 3.0), (120, 21, 0.75)] {
            let a = VoronoiTopology::try_new(count, seed, aspect).expect("a");
            let b = VoronoiTopology::try_new(count, seed, aspect).expect("b");
            assert_eq!(a.edge_count(), b.edge_count(), "count {count} seed {seed}");
            for idx in 0..a.edge_count() {
                let edge = EdgeId(idx);
                assert_eq!(
                    a.edge_endpoints(edge),
                    b.edge_endpoints(edge),
                    "mismatch at count={count} seed={seed} EdgeId({idx})",
                );
            }
        }
    }

    #[test]
    fn voronoi_has_no_sub_threshold_interior_edges() {
        // Sweep a handful of seeds / aspects / piece counts: every
        // interior topology edge should be at least
        // `COLLAPSE_TINY_RATIO * global_median_edge_len` long. That's
        // the same threshold the collapse pass uses internally, so any
        // sub-threshold survivor would mean the pass terminated early.
        for (count, seed, aspect) in [
            (40, 7, 1.0_f32),
            (80, 13, 16.0 / 9.0),
            (120, 21, 3.0 / 4.0),
            (240, 99, 2.0),
        ] {
            let topology = VoronoiTopology::try_new(count, seed, aspect).expect("topology");
            let layout = &topology.layout;
            let polygons: Vec<Vec<Point>> = layout
                .cells
                .iter()
                .map(|cell| cell.polygon.iter().copied().collect())
                .collect();
            let median = median_polygon_edge_len(&polygons, layout.extent_x, layout.extent_y);
            let threshold = (median * COLLAPSE_TINY_RATIO).max(EPS);
            for cell in layout.cells.iter() {
                for edge in cell.edges.iter() {
                    if !matches!(edge.kind, VoronoiCellEdgeKind::Interior(_)) {
                        continue;
                    }
                    let len = distance_sq(edge.start, edge.end).sqrt();
                    assert!(
                        len + 1.0e-4 >= threshold,
                        "tiny interior edge len {len} below threshold {threshold} \
                         (count={count} seed={seed} aspect={aspect})",
                    );
                }
            }
        }
    }
}
