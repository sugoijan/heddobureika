//! Composition utilities that apply connector profiles onto host-edge frames.

use crate::edge_host::{map_local_point, map_point, HostEdgeFrame, WarpField};
use crate::shape::{PathMm, PathSegMm, PointMm};
use crate::units::LengthMm;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConnectorSeg {
    LineTo {
        to: (f32, f32),
    },
    CubicTo {
        c1: (f32, f32),
        c2: (f32, f32),
        to: (f32, f32),
    },
}

pub fn reverse_connector_segments(segments: &[ConnectorSeg]) -> Vec<ConnectorSeg> {
    let mut states = Vec::with_capacity(segments.len());
    let mut current = (0.0, 0.0);

    for segment in segments {
        let end = match *segment {
            ConnectorSeg::LineTo { to } => to,
            ConnectorSeg::CubicTo { to, .. } => to,
        };
        states.push((current, *segment, end));
        current = end;
    }

    let mut reversed = Vec::with_capacity(segments.len());
    for (start, segment, _end) in states.into_iter().rev() {
        match segment {
            ConnectorSeg::LineTo { .. } => reversed.push(ConnectorSeg::LineTo { to: start }),
            ConnectorSeg::CubicTo { c1, c2, .. } => reversed.push(ConnectorSeg::CubicTo {
                c1: c2,
                c2: c1,
                to: start,
            }),
        }
    }

    reversed
}

pub fn map_segments_to_path(
    start: (f32, f32),
    segments: &[ConnectorSeg],
    frame: HostEdgeFrame,
    warp: &WarpField<'_>,
) -> PathMm {
    let (sx, sy) = map_point(frame, start.0, start.1, warp);
    let mut segs = Vec::with_capacity(segments.len());

    for segment in segments {
        match *segment {
            ConnectorSeg::LineTo { to } => {
                let (tx, ty) = map_point(frame, to.0, to.1, warp);
                segs.push(PathSegMm::LineTo {
                    to: point_mm(tx, ty),
                });
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let (c1x, c1y) = map_point(frame, c1.0, c1.1, warp);
                let (c2x, c2y) = map_point(frame, c2.0, c2.1, warp);
                let (tx, ty) = map_point(frame, to.0, to.1, warp);
                segs.push(PathSegMm::CubicTo {
                    c1: point_mm(c1x, c1y),
                    c2: point_mm(c2x, c2y),
                    to: point_mm(tx, ty),
                });
            }
        }
    }

    PathMm::new(point_mm(sx, sy), segs.into_boxed_slice(), false)
}

pub fn append_mapped_segments(
    out: &mut Vec<PathSegMm>,
    segments: &[ConnectorSeg],
    frame: HostEdgeFrame,
    warp: &WarpField<'_>,
) {
    for segment in segments {
        match *segment {
            ConnectorSeg::LineTo { to } => {
                let (tx, ty) = map_point(frame, to.0, to.1, warp);
                out.push(PathSegMm::LineTo {
                    to: point_mm(tx, ty),
                });
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let (c1x, c1y) = map_point(frame, c1.0, c1.1, warp);
                let (c2x, c2y) = map_point(frame, c2.0, c2.1, warp);
                let (tx, ty) = map_point(frame, to.0, to.1, warp);
                out.push(PathSegMm::CubicTo {
                    c1: point_mm(c1x, c1y),
                    c2: point_mm(c2x, c2y),
                    to: point_mm(tx, ty),
                });
            }
        }
    }
}

pub fn append_mapped_local_line_points(
    out: &mut Vec<PathSegMm>,
    offset: (f32, f32),
    warp: &WarpField<'_>,
    points: &[(f32, f32)],
) {
    for &(x, y) in points {
        let (wx, wy) = map_local_point(offset, warp, x, y);
        out.push(PathSegMm::LineTo {
            to: point_mm(wx, wy),
        });
    }
}

pub fn connection_point_for_side(
    start: (f32, f32),
    segments: &[ConnectorSeg],
    tangent_scale: f32,
    normal_scale: f32,
    frame: HostEdgeFrame,
    warp: &WarpField<'_>,
) -> (f32, f32) {
    let (local_x, local_y) =
        side_connection_point_local(start, segments, tangent_scale, normal_scale);
    map_point(frame, local_x, local_y, warp)
}

fn side_connection_point_local(
    start: (f32, f32),
    segments: &[ConnectorSeg],
    tangent_scale: f32,
    normal_scale: f32,
) -> (f32, f32) {
    let mut sampled = Vec::with_capacity(1 + segments.len() * 24);
    let mut current = start;
    sampled.push(current);

    for seg in segments {
        match *seg {
            ConnectorSeg::LineTo { to } => {
                sampled.push(to);
                current = to;
            }
            ConnectorSeg::CubicTo { c1, c2, to } => {
                let p0 = current;
                let steps = 24usize;
                for i in 1..=steps {
                    let t = i as f32 / steps as f32;
                    sampled.push(cubic_point(p0, c1, c2, to, t));
                }
                current = to;
            }
        }
    }

    if sampled.is_empty() {
        return start;
    }

    let tan = tangent_scale.abs().max(1.0e-6);
    let nor = normal_scale.abs().max(1.0e-6);

    let mut normalized = Vec::with_capacity(sampled.len());
    for point in &sampled {
        normalized.push((point.0 / tan, point.1 / nor));
    }

    let mut max_abs_y = 0.0_f32;
    let mut peak_idx = 0usize;
    for (idx, point) in normalized.iter().enumerate() {
        let abs_y = point.1.abs();
        if abs_y > max_abs_y {
            max_abs_y = abs_y;
            peak_idx = idx;
        }
    }

    if max_abs_y <= 1.0e-5 {
        let first = sampled.first().copied().unwrap_or(start);
        let last = sampled.last().copied().unwrap_or(start);
        return ((first.0 + last.0) * 0.5, (first.1 + last.1) * 0.5);
    }

    let sign = normalized
        .get(peak_idx)
        .map(|point| point.1.signum())
        .unwrap_or(1.0)
        .max(-1.0)
        .min(1.0);
    let threshold = max_abs_y * 0.5;

    let peak = normalized
        .get(peak_idx)
        .copied()
        .unwrap_or((start.0 / tan, start.1 / nor));
    let left = threshold_crossing_left(&normalized, peak_idx, threshold).unwrap_or_else(|| {
        normalized
            .first()
            .copied()
            .unwrap_or((peak.0 - 0.25, sign * threshold))
    });
    let right = threshold_crossing_right(&normalized, peak_idx, threshold).unwrap_or_else(|| {
        normalized
            .last()
            .copied()
            .unwrap_or((peak.0 + 0.25, sign * threshold))
    });

    if let Some((cx, cy)) = circumcenter(left, peak, right) {
        if cx.is_finite()
            && cy.is_finite()
            && cy.signum() == sign.signum()
            && cy.abs() >= threshold * 0.35
        {
            return (cx * tan, cy * nor);
        }
    }

    (((left.0 + right.0) * 0.5) * tan, (sign * threshold) * nor)
}

fn threshold_crossing_left(
    points: &[(f32, f32)],
    peak_idx: usize,
    threshold: f32,
) -> Option<(f32, f32)> {
    if points.len() < 2 || peak_idx == 0 {
        return None;
    }

    for i in (1..=peak_idx).rev() {
        let a = points[i - 1];
        let b = points[i];
        let aa = a.1.abs();
        let bb = b.1.abs();
        if aa <= threshold && bb >= threshold && (bb - aa).abs() > 1.0e-6 {
            let t = (threshold - aa) / (bb - aa);
            return Some(lerp_point(a, b, t.clamp(0.0, 1.0)));
        }
    }
    None
}

fn threshold_crossing_right(
    points: &[(f32, f32)],
    peak_idx: usize,
    threshold: f32,
) -> Option<(f32, f32)> {
    if points.len() < 2 || peak_idx + 1 >= points.len() {
        return None;
    }

    for i in peak_idx..(points.len() - 1) {
        let a = points[i];
        let b = points[i + 1];
        let aa = a.1.abs();
        let bb = b.1.abs();
        if aa >= threshold && bb <= threshold && (aa - bb).abs() > 1.0e-6 {
            let t = (aa - threshold) / (aa - bb);
            return Some(lerp_point(a, b, t.clamp(0.0, 1.0)));
        }
    }
    None
}

fn lerp_point(a: (f32, f32), b: (f32, f32), t: f32) -> (f32, f32) {
    (a.0 + (b.0 - a.0) * t, a.1 + (b.1 - a.1) * t)
}

fn circumcenter(a: (f32, f32), b: (f32, f32), c: (f32, f32)) -> Option<(f32, f32)> {
    let d = 2.0 * (a.0 * (b.1 - c.1) + b.0 * (c.1 - a.1) + c.0 * (a.1 - b.1));
    if d.abs() <= 1.0e-6 {
        return None;
    }

    let a2 = a.0 * a.0 + a.1 * a.1;
    let b2 = b.0 * b.0 + b.1 * b.1;
    let c2 = c.0 * c.0 + c.1 * c.1;

    let ux = (a2 * (b.1 - c.1) + b2 * (c.1 - a.1) + c2 * (a.1 - b.1)) / d;
    let uy = (a2 * (c.0 - b.0) + b2 * (a.0 - c.0) + c2 * (b.0 - a.0)) / d;
    Some((ux, uy))
}

fn cubic_point(
    p0: (f32, f32),
    p1: (f32, f32),
    p2: (f32, f32),
    p3: (f32, f32),
    t: f32,
) -> (f32, f32) {
    let u = 1.0 - t;
    let tt = t * t;
    let uu = u * u;
    let uuu = uu * u;
    let ttt = tt * t;
    (
        uuu * p0.0 + 3.0 * uu * t * p1.0 + 3.0 * u * tt * p2.0 + ttt * p3.0,
        uuu * p0.1 + 3.0 * uu * t * p1.1 + 3.0 * u * tt * p2.1 + ttt * p3.1,
    )
}

fn point_mm(x: f32, y: f32) -> PointMm {
    PointMm {
        x: LengthMm::try_new(x).unwrap_or_default(),
        y: LengthMm::try_new(y).unwrap_or_default(),
    }
}
