//! Edge profile shaping strategies (connector only).

use crate::edge_compose::ConnectorSeg;
pub use crate::traits::edge_profile::EdgeProfileStrategy;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum EdgeShapeStyle {
    #[default]
    Classic,
    Trapezoid,
    OffsetCircle,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConnectorShape {
    pub tab_size: f32,
    pub tab_depth: f32,
    pub a: f32,
    pub b: f32,
    pub c: f32,
    pub d: f32,
    pub e: f32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EdgeProfileInput {
    pub len_mm: f32,
    pub depth_base_mm: f32,
    pub depth_limit_mm: f32,
    pub sign: i8,
    pub connector: Option<ConnectorShape>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TabBlankEdgeProfile;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TrapezoidEdgeProfile;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct OffsetCircleEdgeProfile;

pub fn build_edge_profile_segments(
    style: EdgeShapeStyle,
    input: &EdgeProfileInput,
) -> Vec<ConnectorSeg> {
    match style {
        EdgeShapeStyle::Classic => TabBlankEdgeProfile.build_segments(input),
        EdgeShapeStyle::Trapezoid => TrapezoidEdgeProfile.build_segments(input),
        EdgeShapeStyle::OffsetCircle => OffsetCircleEdgeProfile.build_segments(input),
    }
}

impl EdgeProfileStrategy for TabBlankEdgeProfile {
    fn build_segments(&self, input: &EdgeProfileInput) -> Vec<ConnectorSeg> {
        let Some(connector) = input.connector else {
            return straight_edge(input.len_mm);
        };
        if input.sign == 0 {
            return straight_edge(input.len_mm);
        }

        let sign = input.sign as f32;
        let l = |v: f32| input.len_mm * v;
        let w = |v: f32| input.depth_base_mm * v * sign;

        let mut t_depth = connector.tab_size * connector.tab_depth;
        let max_t_depth = input.depth_limit_mm / 3.0;
        if t_depth > max_t_depth {
            t_depth = max_t_depth;
        }

        let max_jitter_depth = (input.depth_limit_mm - 3.0 * t_depth).max(0.0);
        let mut a = connector
            .a
            .clamp(-input.depth_limit_mm, input.depth_limit_mm);
        let b = connector.b;
        let mut c = connector.c.clamp(-max_jitter_depth, max_jitter_depth);
        let d = connector.d;
        let mut e = connector
            .e
            .clamp(-input.depth_limit_mm, input.depth_limit_mm);

        if max_jitter_depth == 0.0 {
            a = 0.0;
            c = 0.0;
            e = 0.0;
        }

        let t_len = connector.tab_size;

        let p1 = (l(0.2), w(a));
        let p2 = (l(0.5 + b + d), w(-t_depth + c));
        let p3 = (l(0.5 - t_len + b), w(t_depth + c));
        let p4 = (l(0.5 - 2.0 * t_len + b - d), w(3.0 * t_depth + c));
        let p5 = (l(0.5 + 2.0 * t_len + b - d), w(3.0 * t_depth + c));
        let p6 = (l(0.5 + t_len + b), w(t_depth + c));
        let p7 = (l(0.5 + b + d), w(-t_depth + c));
        let p8 = (l(0.8), w(e));
        let p9 = (l(1.0), w(0.0));

        vec![
            ConnectorSeg::CubicTo {
                c1: p1,
                c2: p2,
                to: p3,
            },
            ConnectorSeg::CubicTo {
                c1: p4,
                c2: p5,
                to: p6,
            },
            ConnectorSeg::CubicTo {
                c1: p7,
                c2: p8,
                to: p9,
            },
        ]
    }
}

impl EdgeProfileStrategy for TrapezoidEdgeProfile {
    fn build_segments(&self, input: &EdgeProfileInput) -> Vec<ConnectorSeg> {
        let Some(connector) = input.connector else {
            return straight_edge(input.len_mm);
        };
        if input.sign == 0 {
            return straight_edge(input.len_mm);
        }

        let sign = input.sign as f32;
        let max_depth = input.depth_limit_mm.max(0.05);
        let depth_norm = (connector.tab_size * connector.tab_depth).clamp(0.03, max_depth * 0.85);
        let center = input.len_mm * (0.5 + connector.b.clamp(-0.22, 0.22) * 0.55).clamp(0.14, 0.86);

        let neck_half = (input.len_mm * connector.tab_size.clamp(0.04, 0.28) * 0.45)
            .clamp(input.len_mm * 0.04, input.len_mm * 0.18);
        let flare = (neck_half * 0.45).max(input.len_mm * 0.02);
        let outer_half = (neck_half + flare).min(input.len_mm * 0.32);

        let center = center.clamp(
            outer_half + input.len_mm * 0.05,
            input.len_mm - outer_half - input.len_mm * 0.05,
        );

        let base_left = center - neck_half;
        let base_right = center + neck_half;
        let shoulder_left = center - outer_half;
        let shoulder_right = center + outer_half;

        let y_top = input.depth_base_mm * (depth_norm + connector.c * 0.12) * sign;
        let y_base_left = input.depth_base_mm * connector.a * 0.08 * sign;
        let y_base_right = input.depth_base_mm * connector.e * 0.08 * sign;

        vec![
            ConnectorSeg::LineTo {
                to: (base_left, y_base_left),
            },
            ConnectorSeg::LineTo {
                to: (shoulder_left, y_top),
            },
            ConnectorSeg::LineTo {
                to: (shoulder_right, y_top),
            },
            ConnectorSeg::LineTo {
                to: (base_right, y_base_right),
            },
            ConnectorSeg::LineTo {
                to: (input.len_mm, 0.0),
            },
        ]
    }
}

impl EdgeProfileStrategy for OffsetCircleEdgeProfile {
    fn build_segments(&self, input: &EdgeProfileInput) -> Vec<ConnectorSeg> {
        let Some(connector) = input.connector else {
            return straight_edge(input.len_mm);
        };
        if input.sign == 0 {
            return straight_edge(input.len_mm);
        }

        let sign = input.sign as f32;
        let depth_norm = (connector.tab_size * connector.tab_depth)
            .clamp(0.04, input.depth_limit_mm.max(0.06) * 0.95);
        let center_off = input.depth_base_mm * depth_norm.max(0.04);

        let neck_half = (input.len_mm * (connector.tab_size.clamp(0.05, 0.30) * 0.50 + 0.03))
            .clamp(input.len_mm * 0.08, input.len_mm * 0.26);
        let cx = input.len_mm * (0.5 + connector.b.clamp(-0.25, 0.25) * 0.4).clamp(0.18, 0.82);
        let cy = -sign * center_off;
        let radius = (neck_half * neck_half + center_off * center_off).sqrt();

        let start = (cx - neck_half, 0.0);
        let end = (cx + neck_half, 0.0);
        let a0 = (start.1 - cy).atan2(start.0 - cx);
        let a2 = (end.1 - cy).atan2(end.0 - cx);
        let am = 0.5 * (a0 + a2);

        let (c1a, c2a, to_a) = arc_cubic((cx, cy), radius, a0, am);
        let (c1b, c2b, to_b) = arc_cubic((cx, cy), radius, am, a2);

        vec![
            ConnectorSeg::LineTo { to: start },
            ConnectorSeg::CubicTo {
                c1: c1a,
                c2: c2a,
                to: to_a,
            },
            ConnectorSeg::CubicTo {
                c1: c1b,
                c2: c2b,
                to: to_b,
            },
            ConnectorSeg::LineTo {
                to: (input.len_mm, 0.0),
            },
        ]
    }
}

fn straight_edge(len_mm: f32) -> Vec<ConnectorSeg> {
    vec![ConnectorSeg::LineTo { to: (len_mm, 0.0) }]
}

fn arc_cubic(
    center: (f32, f32),
    radius: f32,
    a0: f32,
    a1: f32,
) -> ((f32, f32), (f32, f32), (f32, f32)) {
    let (cx, cy) = center;
    let p0 = (cx + radius * a0.cos(), cy + radius * a0.sin());
    let p3 = (cx + radius * a1.cos(), cy + radius * a1.sin());
    let k = (4.0 / 3.0) * ((a1 - a0) * 0.25).tan();

    let t0 = (-a0.sin(), a0.cos());
    let t1 = (-a1.sin(), a1.cos());

    let p1 = (p0.0 + k * radius * t0.0, p0.1 + k * radius * t0.1);
    let p2 = (p3.0 - k * radius * t1.0, p3.1 - k * radius * t1.1);
    (p1, p2, p3)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trapezoid_profile_has_wider_outer_span_than_neck() {
        let profile = TrapezoidEdgeProfile;
        let segs = profile.build_segments(&sample_input(1));
        assert_eq!(segs.len(), 5);

        let points = line_to_points(&segs);
        assert_eq!(points.len(), 5);

        let base_left = points[0].0;
        let shoulder_left = points[1].0;
        let shoulder_right = points[2].0;
        let base_right = points[3].0;

        assert!(shoulder_left < base_left);
        assert!(shoulder_right > base_right);
    }

    #[test]
    fn offset_circle_profile_is_sign_mirrored() {
        let profile = OffsetCircleEdgeProfile;
        let pos = profile.build_segments(&sample_input(1));
        let neg = profile.build_segments(&sample_input(-1));

        let pos_points = all_points(&pos);
        let neg_points = all_points(&neg);
        assert_eq!(pos_points.len(), neg_points.len());

        for (a, b) in pos_points.iter().zip(neg_points.iter()) {
            assert!(
                (a.0 - b.0).abs() <= 1.0e-4,
                "x mismatch: {} vs {}",
                a.0,
                b.0
            );
            assert!(
                (a.1 + b.1).abs() <= 1.0e-4,
                "y mismatch: {} vs {}",
                a.1,
                b.1
            );
        }
    }

    fn sample_input(sign: i8) -> EdgeProfileInput {
        EdgeProfileInput {
            len_mm: 40.0,
            depth_base_mm: 30.0,
            depth_limit_mm: 0.32,
            sign,
            connector: Some(ConnectorShape {
                tab_size: 0.18,
                tab_depth: 0.95,
                a: 0.02,
                b: -0.01,
                c: 0.03,
                d: 0.0,
                e: -0.02,
            }),
        }
    }

    fn line_to_points(segs: &[ConnectorSeg]) -> Vec<(f32, f32)> {
        segs.iter()
            .filter_map(|seg| match seg {
                ConnectorSeg::LineTo { to } => Some(*to),
                ConnectorSeg::CubicTo { .. } => None,
            })
            .collect()
    }

    fn all_points(segs: &[ConnectorSeg]) -> Vec<(f32, f32)> {
        let mut out = Vec::new();
        for seg in segs {
            match seg {
                ConnectorSeg::LineTo { to } => out.push(*to),
                ConnectorSeg::CubicTo { c1, c2, to } => {
                    out.push(*c1);
                    out.push(*c2);
                    out.push(*to);
                }
            }
        }
        out
    }
}
