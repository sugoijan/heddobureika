//! Fresh equilateral-triangle tiling of a rectangle.
//!
//! Construction (see design discussion): pick a guide-line direction, lay
//! evenly spaced lines across one axis spanning it fully, and step points
//! along each line at the equilateral spacing. The lattice cells are then
//! exact equilateral triangles. The line ends are jagged (the rows alternate
//! by half a step), so we close the rectangle with non-regular *filler*
//! triangles that reach the straight border. The result tiles a clean
//! rectangle where every interior (3-shared-side) triangle is regular and
//! only the border fillers are irregular.
//!
//! Everything here is in pose units with triangle side `s = 1` and row
//! height `h = sqrt(3)/2`; the shaper scales uniformly to pixels so the
//! triangles stay equilateral, and the render layer crops the image to the
//! lattice's `extent`.

use std::collections::HashMap;

/// Row height of a unit-side equilateral triangle (`sqrt(3)/2`).
pub const TRI_ROW_HEIGHT: f32 = 0.866_025_4;

/// Which way the guide lines run.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TriDirection {
    /// Horizontal guide lines stacked top→bottom; jagged left/right edges.
    Horizontal,
    /// Vertical guide lines stacked left→right; jagged top/bottom edges.
    Vertical,
}

impl TriDirection {
    pub fn from_u32(v: u32) -> Self {
        if v == 1 {
            TriDirection::Vertical
        } else {
            TriDirection::Horizontal
        }
    }

    pub fn as_u32(self) -> u32 {
        match self {
            TriDirection::Horizontal => 0,
            TriDirection::Vertical => 1,
        }
    }
}

/// A generated equilateral lattice over a rectangle, in pose units.
#[derive(Clone, Debug)]
pub struct TriLattice {
    pub direction: TriDirection,
    /// Number of strips between guide lines across the spanned axis.
    pub lines: u32,
    /// Number of points along each guide line (on the long, even rows).
    pub points: u32,
    /// Vertex positions in pose units.
    pub vertices: Vec<(f32, f32)>,
    /// Triangles (CCW), in stable piece order.
    pub faces: Vec<[usize; 3]>,
    /// `true` for each face that has no border (frame) edge — i.e. a
    /// regular interior triangle. Indexed by face/piece id.
    pub inner: Vec<bool>,
    /// Pose-unit extent `(width, height)` of the tiled rectangle.
    pub extent: (f32, f32),
}

impl TriLattice {
    pub fn build(direction: TriDirection, lines: u32, points: u32) -> Option<Self> {
        if lines == 0 || points < 2 {
            return None;
        }
        // Generate the horizontal layout, then transpose for vertical.
        let mut lat = build_horizontal(lines, points)?;
        if direction == TriDirection::Vertical {
            for v in lat.vertices.iter_mut() {
                *v = (v.1, v.0);
            }
            lat.extent = (lat.extent.1, lat.extent.0);
            // Transposing mirrors the plane, flipping triangle winding.
            for face in lat.faces.iter_mut() {
                face.swap(1, 2);
            }
        }
        lat.direction = direction;
        lat.inner = compute_inner_flags(&lat.faces);
        Some(lat)
    }

    /// Centroid of a face in pose units.
    pub fn face_centroid(&self, face: usize) -> (f32, f32) {
        let [a, b, c] = self.faces[face];
        let (ax, ay) = self.vertices[a];
        let (bx, by) = self.vertices[b];
        let (cx, cy) = self.vertices[c];
        ((ax + bx + cx) / 3.0, (ay + by + cy) / 3.0)
    }
}

/// Half-integer column key used to dedupe shared vertices within a row.
fn build_horizontal(lines: u32, points: u32) -> Option<TriLattice> {
    let n = lines as usize;
    let m = points as usize;
    let h = TRI_ROW_HEIGHT;

    let mut vertices: Vec<(f32, f32)> = Vec::new();
    let mut index: HashMap<(usize, i32), usize> = HashMap::new();
    let mut vid = |row: usize, col_half: i32| -> usize {
        *index.entry((row, col_half)).or_insert_with(|| {
            let id = vertices.len();
            vertices.push((col_half as f32 * 0.5, row as f32 * h));
            id
        })
    };

    let last = 2 * (m as i32 - 1); // col-half of the right border / last even point
    let mut faces: Vec<[usize; 3]> = Vec::new();

    for i in 0..n {
        let top_even = i % 2 == 0;
        if top_even {
            // top row `i`: even (x = 0..m-1); bottom row `i+1`: odd.
            for j in 0..m - 1 {
                let c = 2 * j as i32;
                faces.push([vid(i, c), vid(i, c + 2), vid(i + 1, c + 1)]); // down
            }
            for j in 0..m.saturating_sub(2) {
                let c = 2 * j as i32;
                faces.push([vid(i + 1, c + 1), vid(i + 1, c + 3), vid(i, c + 2)]);
                // up
            }
            faces.push([vid(i, 0), vid(i + 1, 1), vid(i + 1, 0)]); // left filler
            faces.push([vid(i, last), vid(i + 1, last), vid(i + 1, last - 1)]); // right filler
        } else {
            // top row `i`: odd; bottom row `i+1`: even.
            for j in 0..m - 1 {
                let c = 2 * j as i32;
                faces.push([vid(i + 1, c), vid(i + 1, c + 2), vid(i, c + 1)]); // up
            }
            for j in 0..m.saturating_sub(2) {
                let c = 2 * j as i32;
                faces.push([vid(i, c + 1), vid(i, c + 3), vid(i + 1, c + 2)]); // down
            }
            faces.push([vid(i + 1, 0), vid(i, 0), vid(i, 1)]); // left filler
            faces.push([vid(i + 1, last), vid(i, last - 1), vid(i, last)]); // right filler
        }
    }

    // Normalize winding to CCW (positive signed area).
    for face in faces.iter_mut() {
        if signed_area(&vertices, face) < 0.0 {
            face.swap(1, 2);
        }
    }

    Some(TriLattice {
        direction: TriDirection::Horizontal,
        lines,
        points,
        extent: ((m - 1) as f32, n as f32 * h),
        inner: Vec::new(), // filled by `build`
        vertices,
        faces,
    })
}

fn signed_area(verts: &[(f32, f32)], face: &[usize; 3]) -> f32 {
    let (ax, ay) = verts[face[0]];
    let (bx, by) = verts[face[1]];
    let (cx, cy) = verts[face[2]];
    0.5 * ((bx - ax) * (cy - ay) - (cx - ax) * (by - ay))
}

/// A face is "inner" (regular) when none of its edges is a border edge —
/// i.e. every edge is shared with exactly one other face.
fn compute_inner_flags(faces: &[[usize; 3]]) -> Vec<bool> {
    let mut edge_use: HashMap<(usize, usize), u32> = HashMap::new();
    let edge_key = |a: usize, b: usize| if a < b { (a, b) } else { (b, a) };
    for f in faces {
        for k in 0..3 {
            *edge_use.entry(edge_key(f[k], f[(k + 1) % 3])).or_insert(0) += 1;
        }
    }
    faces
        .iter()
        .map(|f| (0..3).all(|k| edge_use.get(&edge_key(f[k], f[(k + 1) % 3])).copied() == Some(2)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edge_len(v: &[(f32, f32)], a: usize, b: usize) -> f32 {
        let dx = v[a].0 - v[b].0;
        let dy = v[a].1 - v[b].1;
        (dx * dx + dy * dy).sqrt()
    }

    fn assert_inner_faces_equilateral(direction: TriDirection, lines: u32, points: u32) {
        let lat = TriLattice::build(direction, lines, points).expect("lattice");
        let mut inner_count = 0;
        for (fi, face) in lat.faces.iter().enumerate() {
            if !lat.inner[fi] {
                continue;
            }
            inner_count += 1;
            let s0 = edge_len(&lat.vertices, face[0], face[1]);
            let s1 = edge_len(&lat.vertices, face[1], face[2]);
            let s2 = edge_len(&lat.vertices, face[2], face[0]);
            let max = s0.max(s1).max(s2);
            let min = s0.min(s1).min(s2);
            assert!(
                (max - min).abs() < 1.0e-4,
                "{direction:?} {lines}x{points} face {fi} not equilateral: {s0},{s1},{s2}"
            );
            // Each side should be ~1 pose unit.
            assert!((max - 1.0).abs() < 1.0e-4, "face {fi} side != 1: {max}");
        }
        assert!(
            inner_count > 0,
            "{direction:?} {lines}x{points}: no inner faces"
        );
    }

    #[test]
    fn inner_faces_are_unit_equilateral_both_directions() {
        for (lines, points) in [(2u32, 3u32), (3, 5), (6, 8), (7, 7), (4, 10)] {
            assert_inner_faces_equilateral(TriDirection::Horizontal, lines, points);
            assert_inner_faces_equilateral(TriDirection::Vertical, lines, points);
        }
    }

    #[test]
    fn lattice_tiles_the_rectangle_without_gaps_or_overlap() {
        // Sample a grid of points strictly inside the extent; each must be
        // covered by exactly one face (clean rectangular tiling).
        let lat = TriLattice::build(TriDirection::Horizontal, 4, 6).expect("lattice");
        let (w, h) = lat.extent;
        let steps = 31;
        for ix in 0..steps {
            for iy in 0..steps {
                // Off-grid phase so samples don't land exactly on shared edges.
                let px = w * (ix as f32 + 0.37) / steps as f32;
                let py = h * (iy as f32 + 0.31) / steps as f32;
                let covered = lat
                    .faces
                    .iter()
                    .filter(|f| point_in_triangle(&lat.vertices, f, px, py))
                    .count();
                assert_eq!(
                    covered, 1,
                    "point ({px:.3},{py:.3}) covered by {covered} faces"
                );
            }
        }
    }

    fn point_in_triangle(v: &[(f32, f32)], f: &[usize; 3], px: f32, py: f32) -> bool {
        let (ax, ay) = v[f[0]];
        let (bx, by) = v[f[1]];
        let (cx, cy) = v[f[2]];
        let d1 = (px - bx) * (ay - by) - (ax - bx) * (py - by);
        let d2 = (px - cx) * (by - cy) - (bx - cx) * (py - cy);
        let d3 = (px - ax) * (cy - ay) - (cx - ax) * (py - ay);
        let has_neg = d1 < 0.0 || d2 < 0.0 || d3 < 0.0;
        let has_pos = d1 > 0.0 || d2 > 0.0 || d3 > 0.0;
        !(has_neg && has_pos)
    }
}
