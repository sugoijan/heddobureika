//! Host-edge geometry utilities (layout/warp only).

use std::f32::consts::PI;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeOrientation {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BorderFrameShape {
    Rectangle,
    Circle,
    RegularPolygon { sides: u8 },
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct LineWave {
    pub amplitude: f32,
    pub skew: f32,
}

pub struct WarpField<'a> {
    pub width: f32,
    pub height: f32,
    pub horizontal: &'a [LineWave],
    pub vertical: &'a [LineWave],
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HostEdgeFrame {
    pub orientation: EdgeOrientation,
    pub origin: (f32, f32),
    pub offset: (f32, f32),
}

pub fn sample_line_wave(lines: &[LineWave], t: f32) -> LineWave {
    let last = lines.len().saturating_sub(1) as f32;
    if last <= 0.0 {
        return LineWave {
            amplitude: 0.0,
            skew: 0.0,
        };
    }

    let clamped = t.clamp(0.0, 1.0) * last;
    let idx = clamped.floor() as usize;
    let next = (idx + 1).min(lines.len().saturating_sub(1));
    let frac = clamped - idx as f32;

    let current = lines[idx];
    let following = lines[next];

    LineWave {
        amplitude: current.amplitude + (following.amplitude - current.amplitude) * frac,
        skew: current.skew + (following.skew - current.skew) * frac,
    }
}

pub fn warp_point(x: f32, y: f32, warp: &WarpField<'_>) -> (f32, f32) {
    let u = (x / warp.width).clamp(0.0, 1.0);
    let v = (y / warp.height).clamp(0.0, 1.0);

    let h_wave = sample_line_wave(warp.horizontal, v);
    let v_wave = sample_line_wave(warp.vertical, u);

    let u_skew = (u + h_wave.skew * u * (1.0 - u)).clamp(0.0, 1.0);
    let v_skew = (v + v_wave.skew * v * (1.0 - v)).clamp(0.0, 1.0);

    let dy = h_wave.amplitude * (PI * u_skew).sin();
    let dx = v_wave.amplitude * (PI * v_skew).sin();

    (x + dx, y + dy)
}

pub fn map_point(frame: HostEdgeFrame, x: f32, y: f32, warp: &WarpField<'_>) -> (f32, f32) {
    let (ox, oy) = frame.origin;
    let (dx, dy) = frame.offset;

    let (gx, gy) = match frame.orientation {
        EdgeOrientation::Top => (ox + x, oy - y),
        EdgeOrientation::Right => (ox + y, oy + x),
        EdgeOrientation::Bottom => (ox + x, oy + y),
        EdgeOrientation::Left => (ox - y, oy + x),
    };

    let (wx, wy) = warp_point(gx + dx, gy + dy, warp);
    (wx - dx, wy - dy)
}

pub fn map_local_point(offset: (f32, f32), warp: &WarpField<'_>, x: f32, y: f32) -> (f32, f32) {
    let (wx, wy) = warp_point(offset.0 + x, offset.1 + y, warp);
    (wx - offset.0, wy - offset.1)
}

pub fn corner_arc_points(
    cx: f32,
    cy: f32,
    radius: f32,
    start_angle: f32,
    end_angle: f32,
    steps: usize,
) -> Vec<(f32, f32)> {
    let steps = steps.max(1);
    let mut end = end_angle;
    if end < start_angle {
        end += 2.0 * PI;
    }

    let span = end - start_angle;
    let mut points = Vec::with_capacity(steps + 1);
    for step in 0..=steps {
        let t = step as f32 / steps as f32;
        let angle = start_angle + span * t;
        points.push((cx + radius * angle.cos(), cy + radius * angle.sin()));
    }

    points
}

pub fn frame_outline_points(
    width: f32,
    height: f32,
    shape: BorderFrameShape,
    rotation_deg: f32,
    inset_mm: f32,
    detail: usize,
) -> Vec<(f32, f32)> {
    let width = width.max(1.0e-6);
    let height = height.max(1.0e-6);
    let max_inset = 0.49 * width.min(height);
    let inset = inset_mm.clamp(0.0, max_inset);

    let cx = width * 0.5;
    let cy = height * 0.5;
    let rx = (width * 0.5 - inset).max(1.0e-6);
    let ry = (height * 0.5 - inset).max(1.0e-6);
    let rot = rotation_deg.to_radians();

    match shape {
        BorderFrameShape::Rectangle => vec![
            (inset, inset),
            (width - inset, inset),
            (width - inset, height - inset),
            (inset, height - inset),
        ],
        BorderFrameShape::Circle => {
            let steps = detail.max(24);
            let mut points = Vec::with_capacity(steps);
            for i in 0..steps {
                let t = i as f32 / steps as f32;
                let ang = rot + t * 2.0 * PI - PI * 0.5;
                points.push((cx + rx * ang.cos(), cy + ry * ang.sin()));
            }
            points
        }
        BorderFrameShape::RegularPolygon { sides } => {
            let steps = (sides.max(3)) as usize;
            let mut points = Vec::with_capacity(steps);
            for i in 0..steps {
                let t = i as f32 / steps as f32;
                let ang = rot + t * 2.0 * PI - PI * 0.5;
                points.push((cx + rx * ang.cos(), cy + ry * ang.sin()));
            }
            points
        }
    }
}

pub fn warp_points(points: &[(f32, f32)], warp: &WarpField<'_>) -> Vec<(f32, f32)> {
    let mut out = Vec::with_capacity(points.len());
    for &(x, y) in points {
        out.push(warp_point(x, y, warp));
    }
    out
}
