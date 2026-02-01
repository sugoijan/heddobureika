#![cfg(not(feature = "dev-panel-yew"))]

use std::collections::{BTreeMap, BTreeSet};

use naga::back::glsl;
use naga::front::wgsl;
use naga::valid::{Capabilities, ValidationFlags, Validator};
use naga::{AddressSpace, Binding, Dim, ShaderStage, TypeInner};

const PIECE_SRC: &str = include_str!("../src/wgpu_piece.wgsl");
const FRAME_SRC: &str = include_str!("../src/wgpu_frame.wgsl");
const UI_SRC: &str = include_str!("../src/wgpu_ui.wgsl");

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ResourceKind {
    Uniform,
    Texture2d,
    Sampler,
}

#[test]
fn piece_shader_validates_and_targets_webgl2() {
    assert_shader(
        "piece",
        PIECE_SRC,
        &[0, 1, 2, 3, 4, 5, 6, 7, 8],
        &[
            ((0, 0), ResourceKind::Uniform),
            ((0, 1), ResourceKind::Texture2d),
            ((0, 2), ResourceKind::Texture2d),
            ((0, 3), ResourceKind::Sampler),
        ],
    );
}

#[test]
fn frame_shader_validates_and_targets_webgl2() {
    assert_shader(
        "frame",
        FRAME_SRC,
        &[0, 1, 2, 3],
        &[((0, 0), ResourceKind::Uniform)],
    );
}

#[test]
fn ui_shader_validates_and_targets_webgl2() {
    assert_shader(
        "ui",
        UI_SRC,
        &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        &[
            ((0, 0), ResourceKind::Uniform),
            ((1, 0), ResourceKind::Texture2d),
            ((1, 1), ResourceKind::Sampler),
        ],
    );
}

fn assert_shader(
    label: &str,
    source: &str,
    expected_vertex_locations: &[u32],
    expected_resources: &[((u32, u32), ResourceKind)],
) {
    let (module, info) = parse_and_validate(source, label);
    let vertex = entry_point(&module, ShaderStage::Vertex, "vs_main", label);
    let fragment = entry_point(&module, ShaderStage::Fragment, "fs_main", label);

    let actual_locations = entry_point_input_locations(vertex, &module);
    let expected_locations: BTreeSet<u32> = expected_vertex_locations.iter().copied().collect();
    assert_eq!(
        actual_locations, expected_locations,
        "{label} vertex input locations mismatch"
    );

    let output_locations = entry_point_output_locations(fragment, &module);
    let expected_output: BTreeSet<u32> = [0].into_iter().collect();
    assert_eq!(
        output_locations, expected_output,
        "{label} fragment output location mismatch"
    );

    let actual_resources = resource_bindings(&module);
    let expected_resources: BTreeMap<(u32, u32), ResourceKind> =
        expected_resources.iter().copied().collect();
    assert_eq!(
        actual_resources, expected_resources,
        "{label} resource bindings mismatch"
    );

    assert_webgl2_glsl(&module, &info, ShaderStage::Vertex, "vs_main", label);
    assert_webgl2_glsl(&module, &info, ShaderStage::Fragment, "fs_main", label);
}

fn parse_and_validate(source: &str, label: &str) -> (naga::Module, naga::valid::ModuleInfo) {
    let module = wgsl::parse_str(source).unwrap_or_else(|err| {
        panic!("{label} WGSL parse failed: {err}");
    });
    let info = Validator::new(ValidationFlags::all(), Capabilities::all())
        .subgroup_stages(naga::valid::ShaderStages::all())
        .subgroup_operations(naga::valid::SubgroupOperationSet::all())
        .validate(&module)
        .unwrap_or_else(|err| {
            panic!("{label} WGSL validation failed: {err}");
        });
    (module, info)
}

fn entry_point<'a>(
    module: &'a naga::Module,
    stage: ShaderStage,
    name: &str,
    label: &str,
) -> &'a naga::EntryPoint {
    module
        .entry_points
        .iter()
        .find(|entry| entry.stage == stage && entry.name == name)
        .unwrap_or_else(|| panic!("{label} missing entry point {stage:?}:{name}"))
}

fn entry_point_input_locations(
    entry: &naga::EntryPoint,
    module: &naga::Module,
) -> BTreeSet<u32> {
    let mut locations = BTreeSet::new();
    for argument in &entry.function.arguments {
        collect_binding_locations(module, argument.ty, argument.binding.as_ref(), &mut locations);
    }
    locations
}

fn entry_point_output_locations(
    entry: &naga::EntryPoint,
    module: &naga::Module,
) -> BTreeSet<u32> {
    let mut locations = BTreeSet::new();
    if let Some(result) = &entry.function.result {
        collect_binding_locations(module, result.ty, result.binding.as_ref(), &mut locations);
    }
    locations
}

fn collect_binding_locations(
    module: &naga::Module,
    ty: naga::Handle<naga::Type>,
    binding: Option<&Binding>,
    locations: &mut BTreeSet<u32>,
) {
    match binding {
        Some(Binding::Location { location, .. }) => {
            locations.insert(*location);
            return;
        }
        Some(Binding::BuiltIn(_)) => {
            return;
        }
        _ => {}
    }

    let TypeInner::Struct { members, .. } = &module.types[ty].inner else {
        return;
    };
    for member in members {
        collect_binding_locations(module, member.ty, member.binding.as_ref(), locations);
    }
}

fn resource_bindings(module: &naga::Module) -> BTreeMap<(u32, u32), ResourceKind> {
    let mut bindings = BTreeMap::new();
    for (_, global) in module.global_variables.iter() {
        let Some(binding) = global.binding else {
            continue;
        };
        let kind = match global.space {
            AddressSpace::Uniform => ResourceKind::Uniform,
            AddressSpace::Handle => match &module.types[global.ty].inner {
                TypeInner::Image { dim: Dim::D2, .. } => ResourceKind::Texture2d,
                TypeInner::Sampler { .. } => ResourceKind::Sampler,
                other => panic!("unexpected handle type for binding {binding:?}: {other:?}"),
            },
            other => panic!("unexpected address space for binding {binding:?}: {other:?}"),
        };
        if bindings.insert((binding.group, binding.binding), kind).is_some() {
            panic!("duplicate binding for group {} binding {}", binding.group, binding.binding);
        }
    }
    bindings
}

fn assert_webgl2_glsl(
    module: &naga::Module,
    info: &naga::valid::ModuleInfo,
    stage: ShaderStage,
    entry: &str,
    label: &str,
) {
    let mut output = String::new();
    let mut options = glsl::Options::default();
    options.version = glsl::Version::Embedded {
        version: 300,
        is_webgl: true,
    };
    let pipeline = glsl::PipelineOptions {
        entry_point: entry.into(),
        shader_stage: stage,
        multiview: None,
    };
    let mut writer = glsl::Writer::new(
        &mut output,
        module,
        info,
        &options,
        &pipeline,
        naga::proc::BoundsCheckPolicies::default(),
    )
    .unwrap_or_else(|err| {
        panic!("{label} GLSL writer init failed for {stage:?}:{entry}: {err}");
    });
    writer.write().unwrap_or_else(|err| {
        panic!("{label} GLSL writer failed for {stage:?}:{entry}: {err}");
    });
    assert!(
        output.starts_with("#version 300 es"),
        "{label} GLSL output is not WebGL2-compatible"
    );
}
