use heddobureika_game::{
    canonicalize_symmetry_angles, intersect_symmetry_angles, next_step_canonical, next_step_target,
    AngleDeg, EdgeId, PieceId, PuzzleTopology, RelativePose, StepDirection,
};

#[test]
fn canonicalization_empty_injects_zero() {
    let out = canonicalize_symmetry_angles(&[]);
    assert_eq!(out.len(), 1);
    assert_approx(out[0].as_degrees_f32(), 0.0);
}

#[test]
fn canonicalization_normalizes_dedups_and_sorts() {
    let raw = angles(&[360.0, -90.0, 90.0, 450.0, 180.0, 180.0, 270.0]);
    let out = canonicalize_symmetry_angles(&raw);
    let as_deg = out.iter().map(|a| a.as_degrees_f32()).collect::<Vec<_>>();
    assert_eq!(as_deg, vec![0.0, 90.0, 180.0, 270.0]);
}

#[test]
fn intersection_square_and_triangle_keeps_common_symmetries() {
    let square = angles(&[90.0, 180.0, 270.0]);
    let triangle = angles(&[60.0, 120.0, 180.0, 240.0, 300.0]);
    let out = intersect_symmetry_angles(&square, &triangle);
    let as_deg = out.iter().map(|a| a.as_degrees_f32()).collect::<Vec<_>>();
    assert_eq!(as_deg, vec![0.0, 180.0]);
}

#[test]
fn intersection_without_non_zero_common_falls_back_to_zero() {
    let a = angles(&[90.0]);
    let b = angles(&[120.0]);
    let out = intersect_symmetry_angles(&a, &b);
    let as_deg = out.iter().map(|a| a.as_degrees_f32()).collect::<Vec<_>>();
    assert_eq!(as_deg, vec![0.0]);
}

#[test]
fn step_selection_respects_lattice_tolerance_and_wrap() {
    let grid = angles(&[90.0, 180.0, 270.0]);
    let tolerance = AngleDeg::try_new(5.0).expect("finite");

    let cw_near = next_step_canonical(
        &grid,
        AngleDeg::try_new(88.0).expect("finite"),
        tolerance,
        StepDirection::Cw,
    );
    assert_approx(cw_near.as_degrees_f32(), 180.0);

    let cw_not_near = next_step_canonical(
        &grid,
        AngleDeg::try_new(86.0).expect("finite"),
        tolerance,
        StepDirection::Cw,
    );
    assert_approx(cw_not_near.as_degrees_f32(), 90.0);

    let ccw_near = next_step_canonical(
        &grid,
        AngleDeg::try_new(2.0).expect("finite"),
        tolerance,
        StepDirection::Ccw,
    );
    assert_approx(ccw_near.as_degrees_f32(), 270.0);

    let wrap = next_step_canonical(
        &grid,
        AngleDeg::try_new(350.0).expect("finite"),
        tolerance,
        StepDirection::Cw,
    );
    assert_approx(wrap.as_degrees_f32(), 0.0);
}

#[test]
fn directional_target_exposes_congruent_turn_for_zero_only_symmetry() {
    let only_zero = angles(&[]);
    let tolerance = AngleDeg::try_new(5.0).expect("finite");

    let cw = next_step_target(&only_zero, AngleDeg::zero(), tolerance, StepDirection::Cw);
    assert_approx(cw.canonical.as_degrees_f32(), 0.0);
    assert_approx(cw.directional.as_degrees_f32(), 360.0);

    let ccw = next_step_target(&only_zero, AngleDeg::zero(), tolerance, StepDirection::Ccw);
    assert_approx(ccw.canonical.as_degrees_f32(), 0.0);
    assert_approx(ccw.directional.as_degrees_f32(), -360.0);
}

#[test]
fn puzzle_topology_default_step_rotation_uses_symmetry_contract() {
    let topo = DummySymmetryTopology;
    let tolerance = AngleDeg::try_new(5.0).expect("finite");
    let next = topo.step_rotation_cw(
        PieceId(0),
        AngleDeg::try_new(88.0).expect("finite"),
        tolerance,
    );
    assert_approx(next.as_degrees_f32(), 180.0);
}

#[derive(Clone, Debug)]
struct DummySymmetryTopology;

impl PuzzleTopology for DummySymmetryTopology {
    fn piece_count(&self) -> u32 {
        1
    }

    fn edge_count(&self) -> u32 {
        0
    }

    fn edge_endpoints(&self, _edge: EdgeId) -> (PieceId, PieceId) {
        (PieceId(0), PieceId(0))
    }

    fn expected_relative_pose(&self, _a: PieceId, _b: PieceId) -> RelativePose {
        RelativePose::default()
    }

    fn symmetry_angles(&self, _piece: PieceId) -> &[AngleDeg] {
        static ANGLES: std::sync::OnceLock<Box<[AngleDeg]>> = std::sync::OnceLock::new();
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
}

fn angles(values: &[f32]) -> Vec<AngleDeg> {
    values
        .iter()
        .map(|v| AngleDeg::try_new(*v).expect("finite"))
        .collect()
}

fn assert_approx(actual: f32, expected: f32) {
    assert!(
        (actual - expected).abs() <= 1.0e-3,
        "expected {expected}, got {actual}"
    );
}
