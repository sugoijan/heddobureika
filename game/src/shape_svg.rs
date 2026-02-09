//! SVG conversion helpers for canonical shape geometry.

use std::fmt::Write;

use crate::ids::PieceId;
use crate::shape::{FrameGeometryMm, PathMm, PathSegMm, PieceGeometryProvider};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SvgPiecePaths {
    pub outline: String,
    pub edges: Box<[String]>,
}

pub fn path_to_svg_d(path: &PathMm) -> String {
    let mut d = String::with_capacity(128 + path.segs.len() * 40);
    let _ = write!(
        d,
        "M {} {}",
        fmt_f32(path.start.x_mm()),
        fmt_f32(path.start.y_mm())
    );

    for seg in path.segs.iter() {
        match seg {
            PathSegMm::LineTo { to } => {
                let _ = write!(d, " L {} {}", fmt_f32(to.x_mm()), fmt_f32(to.y_mm()));
            }
            PathSegMm::CubicTo { c1, c2, to } => {
                let _ = write!(
                    d,
                    " C {} {} {} {} {} {}",
                    fmt_f32(c1.x_mm()),
                    fmt_f32(c1.y_mm()),
                    fmt_f32(c2.x_mm()),
                    fmt_f32(c2.y_mm()),
                    fmt_f32(to.x_mm()),
                    fmt_f32(to.y_mm())
                );
            }
        }
    }

    if path.closed {
        d.push_str(" Z");
    }

    d
}

pub fn piece_to_svg_paths(cache: &impl PieceGeometryProvider, piece: PieceId) -> SvgPiecePaths {
    let piece_geom = cache.piece_geometry(piece);
    let first_edge = cache.piece_edge_geometry(piece, 0);

    let mut outline_segs = Vec::new();
    let mut edges = Vec::with_capacity(piece_geom.edges.len());
    for idx in 0..piece_geom.edges.len() {
        let edge = cache.piece_edge_geometry(piece, idx);
        edges.push(path_to_svg_d(&edge.path));
        outline_segs.extend(edge.path.segs.iter().cloned());
    }

    let outline = path_to_svg_d(&PathMm::new(
        first_edge.path.start,
        outline_segs.into_boxed_slice(),
        true,
    ));

    SvgPiecePaths {
        outline,
        edges: edges.into_boxed_slice(),
    }
}

pub fn cache_to_svg_paths(cache: &impl PieceGeometryProvider) -> Box<[SvgPiecePaths]> {
    let mut out = Vec::with_capacity(cache.piece_count() as usize);
    for piece in 0..cache.piece_count() {
        out.push(piece_to_svg_paths(cache, PieceId(piece)));
    }
    out.into_boxed_slice()
}

pub fn frame_to_svg_paths(frame: &FrameGeometryMm) -> Box<[String]> {
    frame
        .edges
        .iter()
        .map(path_to_svg_d)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn fmt_f32(value: f32) -> String {
    format!("{:.3}", value)
}
