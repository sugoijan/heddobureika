//! Shared edge-profile machinery: tab/blank generation, frame-fit
//! probing, and straight-line / reverse helpers for piece edges.
//!
//! Both `triangular_shape` and `hexagonal_shape` need the SAME
//! tab/blank profile generator — it operates purely on a straight
//! segment `(start, end)` and a deterministic `(seed, edge)` identity,
//! independent of the topology. This module hosts that machinery so
//! topology-specific shapers stay focused on per-piece polygon
//! geometry.
//!
//! All public functions return `Option<PathMm>` (or plain `PathMm` for
//! infallible ones); callers wrap into their own error types.

use crate::edge_compose::ConnectorSeg;
use crate::edge_profile::{
    build_edge_profile_segments, ConnectorShape, EdgeProfileInput, EdgeShapeStyle,
};
use crate::ids::EdgeId;
use crate::shape::{PathMm, PathSegMm, PointMm};

const TAB_DEPTH_LIMIT: f32 = 0.24;
const TAB_WIDTH_AVG_MIN: f32 = 0.075;
const TAB_WIDTH_AVG_MAX: f32 = 0.115;
const TAB_DEPTH_AVG_MIN: f32 = 0.055;
const TAB_DEPTH_AVG_MAX: f32 = 0.075;

/// 2D point in pixel coordinates, shared by every shaper.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point2 {
    pub x: f32,
    pub y: f32,
}

/// Generates a tab/blank profile path between two pixel-space
/// endpoints. The `(seed, edge)` pair determines the deterministic
/// tab geometry; `average_edge_len` is the typical edge length used
/// to scale tab width/depth relative to the piece size; `frame_width`
/// / `frame_height` constrain the tab to stay inside the puzzle image
/// (degrading to a straight line if it can't).
pub fn profiled_path(
    start: Point2,
    end: Point2,
    seed: u32,
    edge: EdgeId,
    average_edge_len: f32,
    frame_width: f32,
    frame_height: f32,
) -> Option<PathMm> {
    profiled_path_with_sign(
        start,
        end,
        seed,
        edge,
        average_edge_len,
        frame_width,
        frame_height,
        None,
    )
}

/// As [`profiled_path`] but with an explicit `forced_sign`: `Some(+1)`
/// always bulges the tab in the +ny direction (the perpendicular
/// pointing OUT of a CW-traversed piece's boundary — so the piece
/// computing the canonical path "wins" the tab), and `Some(-1)` bulges
/// the other way. `None` picks the seed-derived sign (default
/// behavior).
///
/// Unlike [`profiled_path`], this function NEVER falls back to a
/// straight line. The `frame_width` / `frame_height` arguments are
/// used as a TIE-BREAKER between the two candidate signs (preferring
/// the sign whose tab stays inside the puzzle frame), but if neither
/// sign fits, the forced (or preferred) sign's tab is emitted anyway.
/// This matches the hexagonal-tiling expectation that every interior
/// edge carries a real tab/blank — straight edges are reserved for
/// the puzzle outline.
pub fn profiled_path_with_sign(
    start: Point2,
    end: Point2,
    seed: u32,
    edge: EdgeId,
    average_edge_len: f32,
    frame_width: f32,
    frame_height: f32,
    forced_sign: Option<i8>,
) -> Option<PathMm> {
    let len = distance(start, end);
    if len <= 1.0e-5 {
        return straight_path(start, end);
    }
    let params = connector_for_edge(seed, edge, len, average_edge_len);
    let signs: [i8; 2] = match forced_sign {
        Some(s) if s >= 0 => [1, -1],
        Some(_) => [-1, 1],
        None => {
            let preferred: i8 = if splitmix32(seed ^ edge.as_u32() ^ 0xD1CE_BA5E) & 1 == 0 {
                1
            } else {
                -1
            };
            [preferred, -preferred]
        }
    };
    // Walk both signs. Prefer the first one whose tab fits inside the
    // puzzle frame; if neither fits, emit the FIRST sign's tab anyway
    // (= the forced or seed-preferred direction). We never fall back
    // to a straight line — the caller's polygon is guaranteed to be
    // bounded by the puzzle frame, so the tab might extend a few px
    // past the rounded outline at worst, which the renderer clips.
    let mut first_attempt: Option<PathMm> = None;
    for sign in signs {
        let input = EdgeProfileInput {
            len_mm: len,
            depth_base_mm: average_edge_len,
            depth_limit_mm: TAB_DEPTH_LIMIT,
            sign,
            connector: Some(params),
        };
        let segments = build_edge_profile_segments(EdgeShapeStyle::Classic, &input);
        let fits = connector_segments_fit_frame(start, end, &segments, frame_width, frame_height);
        let path = path_from_connector_segments(start, end, &segments)?;
        if fits {
            return Some(path);
        }
        if first_attempt.is_none() {
            first_attempt = Some(path);
        }
    }
    first_attempt
}

pub fn straight_path(start: Point2, end: Point2) -> Option<PathMm> {
    Some(PathMm::new(
        point(start)?,
        vec![PathSegMm::LineTo { to: point(end)? }].into_boxed_slice(),
        false,
    ))
}

/// Returns the input path walked from end to start (used by interior
/// edges to give one piece the mirror of the other piece's path).
pub fn reverse_path(path: &PathMm) -> PathMm {
    let mut states = Vec::<(PointMm, PathSegMm, PointMm)>::with_capacity(path.segs.len());
    let mut current = path.start;
    for segment in path.segs.iter() {
        let segment_start = current;
        let segment = segment.clone();
        match segment {
            PathSegMm::LineTo { to } => {
                current = to;
            }
            PathSegMm::CubicTo { to, .. } => {
                current = to;
            }
        }
        states.push((segment_start, segment, current));
    }
    let mut segs = Vec::<PathSegMm>::with_capacity(states.len());
    for (start, segment, _) in states.into_iter().rev() {
        match segment {
            PathSegMm::LineTo { .. } => segs.push(PathSegMm::LineTo { to: start }),
            PathSegMm::CubicTo { c1, c2, .. } => segs.push(PathSegMm::CubicTo {
                c1: c2,
                c2: c1,
                to: start,
            }),
        }
    }
    PathMm::new(current, segs.into_boxed_slice(), false)
}

fn connector_segments_fit_frame(
    start: Point2,
    end: Point2,
    segments: &[ConnectorSeg],
    frame_width: f32,
    frame_height: f32,
) -> bool {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len <= 1.0e-5 {
        return true;
    }
    let tx = dx / len;
    let ty = dy / len;
    let nx = -ty;
    let ny = tx;
    for segment in segments {
        let points: &[(f32, f32)] = match segment {
            ConnectorSeg::LineTo { to } => std::slice::from_ref(to),
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let candidates = [*c1, *c2, *to];
                for local in candidates {
                    let p = map_local(start, tx, ty, nx, ny, local);
                    if !point_in_frame(p, frame_width, frame_height) {
                        return false;
                    }
                }
                continue;
            }
        };
        for local in points {
            let p = map_local(start, tx, ty, nx, ny, *local);
            if !point_in_frame(p, frame_width, frame_height) {
                return false;
            }
        }
    }
    true
}

fn path_from_connector_segments(
    start: Point2,
    end: Point2,
    segments: &[ConnectorSeg],
) -> Option<PathMm> {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len <= 1.0e-5 {
        return straight_path(start, end);
    }
    let tx = dx / len;
    let ty = dy / len;
    let nx = -ty;
    let ny = tx;
    let mut path_segments = Vec::<PathSegMm>::with_capacity(segments.len());
    for segment in segments {
        match *segment {
            ConnectorSeg::LineTo { to } => {
                path_segments.push(PathSegMm::LineTo {
                    to: point(map_local(start, tx, ty, nx, ny, to))?,
                });
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                path_segments.push(PathSegMm::CubicTo {
                    c1: point(map_local(start, tx, ty, nx, ny, c1))?,
                    c2: point(map_local(start, tx, ty, nx, ny, c2))?,
                    to: point(map_local(start, tx, ty, nx, ny, to))?,
                });
            }
        }
    }
    let mut path = PathMm::new(point(start)?, path_segments.into_boxed_slice(), false);
    set_path_end_point(&mut path, point(end)?);
    Some(path)
}

fn connector_for_edge(seed: u32, edge: EdgeId, len: f32, average_edge_len: f32) -> ConnectorShape {
    let base = seed ^ edge.as_u32().wrapping_mul(0x9E37_79B9);
    let average = average_edge_len.max(1.0e-5);
    let tab_width = average * rand_range(base, 0, TAB_WIDTH_AVG_MIN, TAB_WIDTH_AVG_MAX);
    let tab_depth = average * rand_range(base, 1, TAB_DEPTH_AVG_MIN, TAB_DEPTH_AVG_MAX);
    let tab_size = (tab_width / len.max(1.0e-5)).clamp(0.04, 0.24);
    let tab_depth = (tab_depth / (average * tab_size).max(1.0e-5)).clamp(0.2, 1.4);
    let jitter_depth = 0.015;
    let jitter_len = (average * 0.025 / len.max(1.0e-5)).clamp(0.0, 0.05);
    ConnectorShape {
        tab_size,
        tab_depth,
        a: rand_range(base, 2, -jitter_depth, jitter_depth),
        b: rand_range(base, 3, -jitter_len, jitter_len),
        c: rand_range(base, 4, -jitter_depth, jitter_depth),
        d: rand_range(base, 5, -jitter_len, jitter_len),
        e: rand_range(base, 6, -jitter_depth, jitter_depth),
    }
}

fn map_local(start: Point2, tx: f32, ty: f32, nx: f32, ny: f32, local: (f32, f32)) -> Point2 {
    Point2 {
        x: start.x + tx * local.0 + nx * local.1,
        y: start.y + ty * local.0 + ny * local.1,
    }
}

fn point_in_frame(p: Point2, frame_width: f32, frame_height: f32) -> bool {
    // Tolerance is in pixels. The check sees cubic-Bézier *control*
    // points, which routinely sit 1–2 px outside the actual curve for
    // a tab on a short edge. The pre-existing 0.01-px tolerance was
    // strict enough that short partial-clipped border-piece edges
    // would fall back to straight lines — even though the visible
    // bulge stays well inside the frame. 2 px lets those through
    // while still rejecting tabs that genuinely escape the puzzle.
    let epsilon = 2.0;
    p.x >= -epsilon
        && p.y >= -epsilon
        && p.x <= frame_width + epsilon
        && p.y <= frame_height + epsilon
}

fn point(value: Point2) -> Option<PointMm> {
    let x = if value.x > -0.01 && value.x < 0.0 {
        0.0
    } else {
        value.x
    };
    let y = if value.y > -0.01 && value.y < 0.0 {
        0.0
    } else {
        value.y
    };
    PointMm::try_from_mm(x, y)
}

fn set_path_end_point(path: &mut PathMm, target: PointMm) {
    if let Some(last) = path.segs.last_mut() {
        match last {
            PathSegMm::LineTo { to } => *to = target,
            PathSegMm::CubicTo { to, .. } => *to = target,
        }
    } else {
        path.start = target;
    }
}

fn distance(a: Point2, b: Point2) -> f32 {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    (dx * dx + dy * dy).sqrt()
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
    let top = mixed >> 8;
    top as f32 / ((1u32 << 24) as f32)
}

fn rand_range(seed: u32, salt: u32, min: f32, max: f32) -> f32 {
    min + (max - min) * rand_unit(seed, salt)
}

// ---------------------------------------------------------------------
// Rounded puzzle-frame boundary helpers.
//
// The puzzle frame is a rectangle with `corner_radius` rounded corners.
// Border-piece edges that sit on this rounded outline need their path
// to follow the boundary — straight chord on flat sections, cubic arc
// on rounded corners, composite if the edge straddles a transition.
// `BoundaryArcClassifier` knows where the 4 corner arcs sit and
// `path_along_boundary(a, b)` builds the right `PathMm` between two
// boundary points.
// ---------------------------------------------------------------------

/// Cubic-Bézier approximation of the short arc from `start` to `end`
/// around `centre`. Returns `None` if the sweep is degenerate (start
/// and end coincide); caller should fall back to a straight chord.
pub fn arc_cubic_path(start: Point2, end: Point2, centre: Point2) -> Option<PathMm> {
    let mut segs: Vec<PathSegMm> = Vec::with_capacity(1);
    if !push_arc_cubic_seg(&mut segs, start, end, centre)? {
        return straight_path(start, end);
    }
    Some(PathMm::new(point(start)?, segs.into_boxed_slice(), false))
}

/// Appends one cubic-bézier segment approximating the short arc from
/// `start` to `end` around `centre`. Returns `Some(true)` if the
/// segment was appended, `Some(false)` if the sweep is effectively
/// zero (caller should emit a straight line), or `None` if any
/// generated `PointMm` couldn't be constructed.
fn push_arc_cubic_seg(
    out: &mut Vec<PathSegMm>,
    start: Point2,
    end: Point2,
    centre: Point2,
) -> Option<bool> {
    let theta1 = (start.y - centre.y).atan2(start.x - centre.x);
    let theta2 = (end.y - centre.y).atan2(end.x - centre.x);
    let mut sweep = theta2 - theta1;
    if sweep > std::f32::consts::PI {
        sweep -= 2.0 * std::f32::consts::PI;
    } else if sweep <= -std::f32::consts::PI {
        sweep += 2.0 * std::f32::consts::PI;
    }
    if sweep.abs() <= 1.0e-6 {
        return Some(false);
    }
    let alpha = (4.0 / 3.0) * (sweep / 4.0).tan();
    let tan_start = Point2 {
        x: -(start.y - centre.y),
        y: start.x - centre.x,
    };
    let tan_end = Point2 {
        x: -(end.y - centre.y),
        y: end.x - centre.x,
    };
    let c1 = Point2 {
        x: start.x + alpha * tan_start.x,
        y: start.y + alpha * tan_start.y,
    };
    let c2 = Point2 {
        x: end.x - alpha * tan_end.x,
        y: end.y - alpha * tan_end.y,
    };
    out.push(PathSegMm::CubicTo {
        c1: point(c1)?,
        c2: point(c2)?,
        to: point(end)?,
    });
    Some(true)
}

#[derive(Clone, Copy, Debug)]
struct ArcInfo {
    centre: Point2,
    transitions: [Point2; 2],
    corner: FrameCorner,
}

#[derive(Clone, Copy, Debug)]
enum FrameCorner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

#[derive(Clone, Copy, Debug)]
pub struct BoundaryArcClassifier {
    arcs: [ArcInfo; 4],
    width: f32,
    height: f32,
    radius: f32,
    tolerance: f32,
}

impl BoundaryArcClassifier {
    pub fn new(width: f32, height: f32, radius: f32) -> Self {
        Self {
            arcs: [
                ArcInfo {
                    centre: Point2 {
                        x: radius,
                        y: radius,
                    },
                    transitions: [Point2 { x: 0.0, y: radius }, Point2 { x: radius, y: 0.0 }],
                    corner: FrameCorner::TopLeft,
                },
                ArcInfo {
                    centre: Point2 {
                        x: width - radius,
                        y: radius,
                    },
                    transitions: [
                        Point2 {
                            x: width - radius,
                            y: 0.0,
                        },
                        Point2 {
                            x: width,
                            y: radius,
                        },
                    ],
                    corner: FrameCorner::TopRight,
                },
                ArcInfo {
                    centre: Point2 {
                        x: width - radius,
                        y: height - radius,
                    },
                    transitions: [
                        Point2 {
                            x: width,
                            y: height - radius,
                        },
                        Point2 {
                            x: width - radius,
                            y: height,
                        },
                    ],
                    corner: FrameCorner::BottomRight,
                },
                ArcInfo {
                    centre: Point2 {
                        x: radius,
                        y: height - radius,
                    },
                    transitions: [
                        Point2 {
                            x: radius,
                            y: height,
                        },
                        Point2 {
                            x: 0.0,
                            y: height - radius,
                        },
                    ],
                    corner: FrameCorner::BottomLeft,
                },
            ],
            width,
            height,
            radius,
            tolerance: (radius * 0.08).max(1.0e-3),
        }
    }

    pub fn radius(&self) -> f32 {
        self.radius
    }

    /// Returns the two transition points (where the arc meets the
    /// straight sections) for the corner closest to `(cx, cy)`. Used
    /// by callers that need to inset a sharp-corner polygon vertex
    /// onto the rounded outline.
    pub fn corner_transitions(&self, cx: f32, cy: f32) -> [Point2; 2] {
        let idx = self
            .nearest_corner_idx(Point2 { x: cx, y: cy })
            .unwrap_or(0);
        self.arcs[idx].transitions
    }

    fn nearest_corner_idx(&self, p: Point2) -> Option<usize> {
        let mut best: Option<(usize, f32)> = None;
        for (i, arc) in self.arcs.iter().enumerate() {
            let cx = match arc.corner {
                FrameCorner::TopLeft | FrameCorner::BottomLeft => 0.0,
                FrameCorner::TopRight | FrameCorner::BottomRight => self.width,
            };
            let cy = match arc.corner {
                FrameCorner::TopLeft | FrameCorner::TopRight => 0.0,
                FrameCorner::BottomLeft | FrameCorner::BottomRight => self.height,
            };
            let d = (p.x - cx).hypot(p.y - cy);
            if best.map_or(true, |(_, b)| d < b) {
                best = Some((i, d));
            }
        }
        best.map(|(i, _)| i)
    }

    fn in_arc_quadrant(&self, p: Point2, arc: &ArcInfo) -> bool {
        let tol = self.tolerance;
        match arc.corner {
            FrameCorner::TopLeft => p.x <= self.radius + tol && p.y <= self.radius + tol,
            FrameCorner::TopRight => {
                p.x >= self.width - self.radius - tol && p.y <= self.radius + tol
            }
            FrameCorner::BottomRight => {
                p.x >= self.width - self.radius - tol && p.y >= self.height - self.radius - tol
            }
            FrameCorner::BottomLeft => {
                p.x <= self.radius + tol && p.y >= self.height - self.radius - tol
            }
        }
    }

    fn arc_index(&self, p: Point2) -> Option<usize> {
        for (idx, arc) in self.arcs.iter().enumerate() {
            if !self.in_arc_quadrant(p, arc) {
                continue;
            }
            let dx = p.x - arc.centre.x;
            let dy = p.y - arc.centre.y;
            let dist = (dx * dx + dy * dy).sqrt();
            if (dist - self.radius).abs() <= self.tolerance {
                return Some(idx);
            }
        }
        None
    }

    /// Builds the path along the rounded boundary from `a` to `b`.
    /// Three composite forms are produced: pure arc (both endpoints on
    /// the same corner arc), pure chord (both on straight sections),
    /// or arc + line / line + arc when the segment straddles the
    /// transition between an arc and a straight side.
    pub fn path_along_boundary(&self, a: Point2, b: Point2) -> Option<PathMm> {
        let arc_a = self.arc_index(a);
        let arc_b = self.arc_index(b);
        match (arc_a, arc_b) {
            (Some(ia), Some(ib)) if ia == ib => arc_cubic_path(a, b, self.arcs[ia].centre),
            (Some(ia), None) => {
                let transition = nearest_transition(&self.arcs[ia], b);
                let mut segs: Vec<PathSegMm> = Vec::with_capacity(2);
                push_arc_cubic_seg(&mut segs, a, transition, self.arcs[ia].centre)?;
                segs.push(PathSegMm::LineTo { to: point(b)? });
                Some(PathMm::new(point(a)?, segs.into_boxed_slice(), false))
            }
            (None, Some(ib)) => {
                let transition = nearest_transition(&self.arcs[ib], a);
                let mut segs: Vec<PathSegMm> = Vec::with_capacity(2);
                segs.push(PathSegMm::LineTo {
                    to: point(transition)?,
                });
                push_arc_cubic_seg(&mut segs, transition, b, self.arcs[ib].centre)?;
                Some(PathMm::new(point(a)?, segs.into_boxed_slice(), false))
            }
            (Some(_), Some(_)) => straight_path(a, b),
            (None, None) => straight_path(a, b),
        }
    }
}

fn nearest_transition(arc: &ArcInfo, reference: Point2) -> Point2 {
    let d0 = distance(arc.transitions[0], reference);
    let d1 = distance(arc.transitions[1], reference);
    if d0 <= d1 {
        arc.transitions[0]
    } else {
        arc.transitions[1]
    }
}
