use std::num::NonZeroU32;

use heddobureika_game::{
    cache_to_svg_paths, frame_to_svg_paths, path_to_svg_d, BorderEdgeGeometryMm, BorderEdgeId,
    EdgeId, EdgeShapeStyle, EdgeSide, EdgeSideGeometryMm, FrameEdgeId, FrameGeometryMm,
    GridJigsawShaper, GridPuzzleDefinition, GridShapeSettings, GridTopology,
    InteriorEdgeGeometryMm, PathMm, PathSegMm, PieceEdgeRef, PieceGeometryMm,
    PieceGeometryProvider, PieceId, PointMm, PuzzleTopology, ShapeAtlasMm, TopologyShaper,
};

#[test]
fn grid_shape_counts_match_topology() {
    let topology = GridTopology::new(
        NonZeroU32::new(3).expect("non-zero"),
        NonZeroU32::new(2).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(40.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(30.0).expect("finite"),
            0x5EED_2520,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    assert_eq!(cache.piece_count(), topology.piece_count());
    for piece in 0..cache.piece_count() {
        let geom = cache.piece_geometry(PieceId(piece));
        assert_eq!(geom.edges.len(), 4);
    }
}

#[test]
fn shape_atlas_validate_passes_for_grid_cache() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(42.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(33.0).expect("finite"),
            0xA5A5_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    cache
        .atlas
        .validate(&topology)
        .expect("generated atlas should be valid");
}

#[test]
fn piece_outline_is_derived_from_ring() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(42.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(33.0).expect("finite"),
            0xBEEF_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    for piece in 0..cache.piece_count() {
        let piece_id = PieceId(piece);
        let geom = cache.piece_geometry(piece_id);
        let first = cache.piece_edge_geometry(piece_id, 0);
        let mut segs = Vec::new();
        for idx in 0..geom.edges.len() {
            segs.extend(
                cache
                    .piece_edge_geometry(piece_id, idx)
                    .path
                    .segs
                    .iter()
                    .cloned(),
            );
        }
        let manual = PathMm::new(first.path.start, segs.into_boxed_slice(), true);
        let derived = cache.atlas.piece_outline(piece_id);
        assert_eq!(manual, derived);
    }
}

#[test]
fn interior_edge_has_exactly_two_piece_refs() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(42.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(33.0).expect("finite"),
            0xA5A5_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    let mut edge_usage = vec![0u32; topology.edge_count() as usize];
    let mut side_a = vec![0u32; topology.edge_count() as usize];
    let mut side_b = vec![0u32; topology.edge_count() as usize];

    for piece in 0..cache.piece_count() {
        let geom = cache.piece_geometry(PieceId(piece));
        for edge_ref in geom.edges.iter() {
            if let PieceEdgeRef::Interior { edge, side } = edge_ref {
                let idx = edge.as_usize();
                edge_usage[idx] += 1;
                match side {
                    EdgeSide::A => side_a[idx] += 1,
                    EdgeSide::B => side_b[idx] += 1,
                }
            }
        }
    }

    for idx in 0..(topology.edge_count() as usize) {
        assert_eq!(edge_usage[idx], 2);
        assert_eq!(side_a[idx], 1);
        assert_eq!(side_b[idx], 1);
    }
}

#[test]
fn interior_edge_endpoints_match_topology() {
    let topology = GridTopology::new(
        NonZeroU32::new(5).expect("non-zero"),
        NonZeroU32::new(4).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x1234_5678,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    assert_eq!(
        cache.atlas.interior_edges.len(),
        topology.edge_count() as usize
    );
    for edge in 0..topology.edge_count() {
        let edge_id = EdgeId(edge);
        let atlas_edge = cache.interior_edge_geometry(edge_id);
        assert_eq!(atlas_edge.endpoints, topology.edge_endpoints(edge_id));
    }
}

#[test]
fn grid_shape_edge_mapping_is_complete_and_consistent() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(42.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(33.0).expect("finite"),
            0xA5A5_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    let cols = topology.cols().get();
    let rows = topology.rows().get();
    for piece in 0..cache.piece_count() {
        let geom = cache.piece_geometry(PieceId(piece));
        assert_eq!(geom.edges.len(), 4);
        let (row, col) = topology
            .piece_row_col(PieceId(piece))
            .expect("row/col should exist");

        for side in 0..4 {
            let expected = expected_grid_edge_id(rows, cols, row, col, side).map(EdgeId::from);
            match (geom.edges[side], expected) {
                (PieceEdgeRef::Interior { edge, .. }, Some(expected_id)) => {
                    assert_eq!(edge, expected_id)
                }
                (PieceEdgeRef::Border { .. }, None) => {}
                _ => panic!("piece edge mapping mismatch at piece {piece} side {side}"),
            }
        }
    }
}

#[test]
fn grid_shape_connection_points_match_between_joined_piece_edges() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let piece_w = heddobureika_game::LengthMm::try_new(42.0).expect("finite");
    let piece_h = heddobureika_game::LengthMm::try_new(33.0).expect("finite");
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            piece_w,
            piece_h,
            0xA5A5_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    for edge in 0..topology.edge_count() {
        let edge_id = EdgeId(edge);
        let atlas_edge = cache.interior_edge_geometry(edge_id);
        let (piece_a, piece_b) = atlas_edge.endpoints;

        let (row_a, col_a) = topology
            .piece_row_col(piece_a)
            .expect("row/col for piece_a");
        let (row_b, col_b) = topology
            .piece_row_col(piece_b)
            .expect("row/col for piece_b");

        let point_a = atlas_edge.side_a.connection_point;
        let point_b = atlas_edge.side_b.connection_point;

        let world_a = (
            point_a.x_mm() + col_a as f32 * piece_w.as_mm_f32(),
            point_a.y_mm() + row_a as f32 * piece_h.as_mm_f32(),
        );
        let world_b = (
            point_b.x_mm() + col_b as f32 * piece_w.as_mm_f32(),
            point_b.y_mm() + row_b as f32 * piece_h.as_mm_f32(),
        );

        assert_approx(world_a.0, world_b.0);
        assert_approx(world_a.1, world_b.1);
    }
}

#[test]
fn every_border_edge_references_one_frame_edge() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(42.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(33.0).expect("finite"),
            0xA5A5_0101,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    let mut frame_use = vec![0u32; cache.frame_geometry().edges.len()];
    for border in cache.atlas.border_edges.iter() {
        let frame_idx = border.frame_edge.as_usize();
        assert!(frame_idx < frame_use.len());
        frame_use[frame_idx] += 1;
    }

    assert_eq!(cache.atlas.border_edges.len(), frame_use.len());
    for usage in frame_use {
        assert_eq!(usage, 1);
    }
}

#[test]
fn every_frame_edge_is_referenced_once_by_border_edge() {
    let topology = GridTopology::new(
        NonZeroU32::new(6).expect("non-zero"),
        NonZeroU32::new(4).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(50.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(30.0).expect("finite"),
            0x1111_2222,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    let mut usage = vec![0u32; cache.frame_geometry().edges.len()];
    for border_idx in 0..cache.atlas.border_edges.len() {
        let border = cache.border_edge_geometry(BorderEdgeId(border_idx as u32));
        usage[border.frame_edge.as_usize()] += 1;
    }

    for count in usage {
        assert_eq!(count, 1);
    }
}

#[test]
fn piece_ring_is_closed_and_contiguous_for_all_pieces() {
    let topology = GridTopology::new(
        NonZeroU32::new(5).expect("non-zero"),
        NonZeroU32::new(4).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x3333_4444,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    for piece in 0..cache.piece_count() {
        let piece_id = PieceId(piece);
        let geom = cache.piece_geometry(piece_id);
        assert!(!geom.edges.is_empty());

        let first_start = cache.piece_edge_geometry(piece_id, 0).path.start;
        let mut prev_end = first_start;

        for idx in 0..geom.edges.len() {
            let edge = cache.piece_edge_geometry(piece_id, idx);
            assert_eq!(edge.path.start, prev_end);
            prev_end = path_end_point(&edge.path);
        }

        assert_eq!(prev_end, first_start);
    }
}

#[test]
fn grid_shape_is_seed_deterministic() {
    let topology = GridTopology::new(
        NonZeroU32::new(5).expect("non-zero"),
        NonZeroU32::new(4).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let settings = GridShapeSettings::default();

    let cache_a = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x1234_5678,
            &settings,
        )
        .expect("grid cache should build");
    let cache_b = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x1234_5678,
            &settings,
        )
        .expect("grid cache should build");

    assert_eq!(cache_a, cache_b);
}

#[test]
fn grid_shape_varies_with_seed() {
    let topology = GridTopology::new(
        NonZeroU32::new(5).expect("non-zero"),
        NonZeroU32::new(4).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let settings = GridShapeSettings::default();

    let cache_a = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x1111_1111,
            &settings,
        )
        .expect("grid cache should build");
    let cache_b = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(24.0).expect("finite"),
            0x2222_2222,
            &settings,
        )
        .expect("grid cache should build");

    let mut changed_interior_edge = false;
    for edge in 0..topology.edge_count() {
        let edge_id = EdgeId(edge);
        let path_a = path_to_svg_d(&cache_a.interior_edge_geometry(edge_id).side_a.path);
        let path_b = path_to_svg_d(&cache_b.interior_edge_geometry(edge_id).side_a.path);
        if path_a != path_b {
            changed_interior_edge = true;
            break;
        }
    }

    assert!(
        changed_interior_edge,
        "expected at least one interior edge path to vary by seed"
    );
}

#[test]
fn grid_shape_styles_produce_distinct_interior_paths() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let base_settings = GridShapeSettings::default();

    let mut classic_settings = base_settings;
    classic_settings.edge_style = EdgeShapeStyle::Classic;
    let mut trapezoid_settings = base_settings;
    trapezoid_settings.edge_style = EdgeShapeStyle::Trapezoid;
    let mut circle_settings = base_settings;
    circle_settings.edge_style = EdgeShapeStyle::OffsetCircle;

    let classic = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(45.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            0x8BAD_F00D,
            &classic_settings,
        )
        .expect("classic cache should build");
    let trapezoid = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(45.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            0x8BAD_F00D,
            &trapezoid_settings,
        )
        .expect("trapezoid cache should build");
    let circle = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(45.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(35.0).expect("finite"),
            0x8BAD_F00D,
            &circle_settings,
        )
        .expect("offset-circle cache should build");

    let edge = EdgeId(0);
    let d_classic = path_to_svg_d(&classic.interior_edge_geometry(edge).side_a.path);
    let d_trapezoid = path_to_svg_d(&trapezoid.interior_edge_geometry(edge).side_a.path);
    let d_circle = path_to_svg_d(&circle.interior_edge_geometry(edge).side_a.path);

    assert_ne!(d_classic, d_trapezoid);
    assert_ne!(d_classic, d_circle);
    assert_ne!(d_trapezoid, d_circle);
}

#[test]
fn grid_shape_styles_keep_join_connection_points_aligned() {
    let topology = GridTopology::new(
        NonZeroU32::new(4).expect("non-zero"),
        NonZeroU32::new(3).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let piece_w = heddobureika_game::LengthMm::try_new(42.0).expect("finite");
    let piece_h = heddobureika_game::LengthMm::try_new(33.0).expect("finite");

    for style in [
        EdgeShapeStyle::Classic,
        EdgeShapeStyle::Trapezoid,
        EdgeShapeStyle::OffsetCircle,
    ] {
        let mut settings = GridShapeSettings::default();
        settings.edge_style = style;
        let cache = shaper
            .build_cache(&topology, piece_w, piece_h, 0xA5A5_0101, &settings)
            .expect("grid cache should build");

        for edge in 0..topology.edge_count() {
            let edge_id = EdgeId(edge);
            let atlas_edge = cache.interior_edge_geometry(edge_id);
            let (piece_a, piece_b) = atlas_edge.endpoints;

            let (row_a, col_a) = topology
                .piece_row_col(piece_a)
                .expect("row/col for piece_a");
            let (row_b, col_b) = topology
                .piece_row_col(piece_b)
                .expect("row/col for piece_b");

            let point_a = atlas_edge.side_a.connection_point;
            let point_b = atlas_edge.side_b.connection_point;
            let world_a = (
                point_a.x_mm() + col_a as f32 * piece_w.as_mm_f32(),
                point_a.y_mm() + row_a as f32 * piece_h.as_mm_f32(),
            );
            let world_b = (
                point_b.x_mm() + col_b as f32 * piece_w.as_mm_f32(),
                point_b.y_mm() + row_b as f32 * piece_h.as_mm_f32(),
            );

            assert_approx_with_tol(world_a.0, world_b.0, 5.0e-2);
            assert_approx_with_tol(world_a.1, world_b.1, 5.0e-2);
        }
    }
}

#[test]
fn shape_svg_handles_variable_edge_counts() {
    let edge0 = EdgeSideGeometryMm {
        path: PathMm::new(
            point(0.0, 0.0),
            vec![PathSegMm::CubicTo {
                c1: point(3.0, 1.0),
                c2: point(7.0, 1.0),
                to: point(10.0, 0.0),
            }]
            .into_boxed_slice(),
            false,
        ),
        connection_point: point(5.0, 0.0),
    };
    let edge1 = EdgeSideGeometryMm {
        path: PathMm::new(
            point(10.0, 0.0),
            vec![PathSegMm::LineTo {
                to: point(11.0, 3.0),
            }]
            .into_boxed_slice(),
            false,
        ),
        connection_point: point(10.5, 1.5),
    };
    let edge2 = EdgeSideGeometryMm {
        path: PathMm::new(
            point(11.0, 3.0),
            vec![PathSegMm::LineTo {
                to: point(10.0, 8.0),
            }]
            .into_boxed_slice(),
            false,
        ),
        connection_point: point(10.5, 5.5),
    };
    let edge3 = EdgeSideGeometryMm {
        path: PathMm::new(
            point(10.0, 8.0),
            vec![PathSegMm::LineTo {
                to: point(0.0, 8.0),
            }]
            .into_boxed_slice(),
            false,
        ),
        connection_point: point(5.0, 8.0),
    };
    let edge4 = EdgeSideGeometryMm {
        path: PathMm::new(
            point(0.0, 8.0),
            vec![PathSegMm::LineTo {
                to: point(0.0, 0.0),
            }]
            .into_boxed_slice(),
            false,
        ),
        connection_point: point(0.0, 4.0),
    };

    let border_edges = vec![
        BorderEdgeGeometryMm {
            piece: PieceId(0),
            side: edge0.clone(),
            frame_edge: FrameEdgeId(0),
        },
        BorderEdgeGeometryMm {
            piece: PieceId(0),
            side: edge1.clone(),
            frame_edge: FrameEdgeId(1),
        },
        BorderEdgeGeometryMm {
            piece: PieceId(0),
            side: edge2.clone(),
            frame_edge: FrameEdgeId(2),
        },
        BorderEdgeGeometryMm {
            piece: PieceId(0),
            side: edge3.clone(),
            frame_edge: FrameEdgeId(3),
        },
        BorderEdgeGeometryMm {
            piece: PieceId(0),
            side: edge4.clone(),
            frame_edge: FrameEdgeId(4),
        },
    ]
    .into_boxed_slice();

    let atlas = ShapeAtlasMm {
        pieces: vec![PieceGeometryMm {
            edges: vec![
                PieceEdgeRef::Border {
                    edge: BorderEdgeId(0),
                },
                PieceEdgeRef::Border {
                    edge: BorderEdgeId(1),
                },
                PieceEdgeRef::Border {
                    edge: BorderEdgeId(2),
                },
                PieceEdgeRef::Border {
                    edge: BorderEdgeId(3),
                },
                PieceEdgeRef::Border {
                    edge: BorderEdgeId(4),
                },
            ]
            .into_boxed_slice(),
        }]
        .into_boxed_slice(),
        interior_edges: Box::default(),
        border_edges,
        frame: FrameGeometryMm {
            edges: vec![
                edge0.path.clone(),
                edge1.path.clone(),
                edge2.path.clone(),
                edge3.path.clone(),
                edge4.path.clone(),
            ]
            .into_boxed_slice(),
        },
    };

    struct DummyCache {
        atlas: ShapeAtlasMm,
    }

    impl PieceGeometryProvider for DummyCache {
        fn piece_count(&self) -> u32 {
            self.atlas.pieces.len() as u32
        }

        fn piece_geometry(&self, piece: PieceId) -> &PieceGeometryMm {
            self.atlas
                .pieces
                .get(piece.as_usize())
                .expect("piece id should be valid for dummy cache")
        }

        fn interior_edge_geometry(&self, edge: EdgeId) -> &InteriorEdgeGeometryMm {
            self.atlas
                .interior_edges
                .get(edge.as_usize())
                .expect("interior edge id should be valid for dummy cache")
        }

        fn border_edge_geometry(&self, edge: BorderEdgeId) -> &BorderEdgeGeometryMm {
            self.atlas
                .border_edges
                .get(edge.as_usize())
                .expect("border edge id should be valid for dummy cache")
        }

        fn frame_geometry(&self) -> &FrameGeometryMm {
            &self.atlas.frame
        }

        fn piece_edge_geometry(&self, piece: PieceId, edge_index: usize) -> &EdgeSideGeometryMm {
            self.atlas.piece_edge_geometry(piece, edge_index)
        }
    }

    let cache = DummyCache { atlas };

    let svg = heddobureika_game::piece_to_svg_paths(&cache, PieceId(0));
    assert!(svg.outline.starts_with("M "));
    assert_eq!(svg.edges.len(), 5);
    assert!(svg.edges[0].contains(" C "));

    let all = cache_to_svg_paths(&cache);
    assert_eq!(all.len(), 1);
    assert_eq!(all[0].edges.len(), 5);

    let frame_svg = frame_to_svg_paths(cache.frame_geometry());
    assert_eq!(frame_svg.len(), 5);
}

#[test]
fn grid_puzzle_definition_builds_and_rebuilds() {
    let mut grid = GridPuzzleDefinition::new(
        NonZeroU32::new(3).expect("non-zero"),
        NonZeroU32::new(2).expect("non-zero"),
        heddobureika_game::LengthMm::try_new(45.0).expect("finite"),
        heddobureika_game::LengthMm::try_new(36.0).expect("finite"),
        0xABCD_EF01,
        GridShapeSettings::default(),
    )
    .expect("grid puzzle should build");

    assert_eq!(grid.shape_cache.piece_count(), grid.topology.piece_count());

    let before = cache_to_svg_paths(&grid.shape_cache);
    grid.rebuild_shapes().expect("rebuild should succeed");
    let rebuilt_same = cache_to_svg_paths(&grid.shape_cache);
    assert_eq!(before, rebuilt_same);

    grid.shape_seed ^= 0x00FF_00FF;
    grid.rebuild_shapes().expect("rebuild should succeed");
    let rebuilt_changed = cache_to_svg_paths(&grid.shape_cache);
    assert_ne!(rebuilt_same, rebuilt_changed);
}

#[test]
#[should_panic]
fn piece_geometry_panics_for_out_of_range_piece_id() {
    let topology = GridTopology::new(
        NonZeroU32::new(2).expect("non-zero"),
        NonZeroU32::new(2).expect("non-zero"),
    );
    let shaper = GridJigsawShaper;
    let cache = shaper
        .build_cache(
            &topology,
            heddobureika_game::LengthMm::try_new(30.0).expect("finite"),
            heddobureika_game::LengthMm::try_new(30.0).expect("finite"),
            0x1234_0000,
            &GridShapeSettings::default(),
        )
        .expect("grid cache should build");

    let _ = cache.piece_geometry(PieceId(cache.piece_count()));
}

fn expected_grid_edge_id(rows: u32, cols: u32, row: u32, col: u32, side: usize) -> Option<u32> {
    let horizontal_count = rows * cols.saturating_sub(1);
    match side {
        0 if row > 0 => Some(horizontal_count + (row - 1) * cols + col),
        1 if col + 1 < cols => Some(row * cols.saturating_sub(1) + col),
        2 if row + 1 < rows => Some(horizontal_count + row * cols + col),
        3 if col > 0 => Some(row * cols.saturating_sub(1) + (col - 1)),
        _ => None,
    }
}

fn point(x_mm: f32, y_mm: f32) -> PointMm {
    PointMm::try_from_mm(x_mm, y_mm).expect("point should be finite")
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

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-3,
        "expected {expected}, got {actual}"
    );
}

fn assert_approx_with_tol(actual: f32, expected: f32, tol: f32) {
    assert!(
        (actual - expected).abs() <= tol,
        "expected {expected}, got {actual}, tol={tol}"
    );
}
