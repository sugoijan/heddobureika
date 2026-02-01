#![cfg(all(target_arch = "wasm32", not(feature = "dev-panel-yew")))]

use naga::back::glsl;
use naga::front::wgsl;
use naga::valid::{Capabilities, ValidationFlags, Validator};
use naga::ShaderStage;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::*;
use web_sys::{window, HtmlCanvasElement, WebGl2RenderingContext, WebGlProgram, WebGlShader};

const PIECE_SRC: &str = include_str!("../src/wgpu_piece.wgsl");
const FRAME_SRC: &str = include_str!("../src/wgpu_frame.wgsl");
const UI_SRC: &str = include_str!("../src/wgpu_ui.wgsl");

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn piece_shader_compiles_in_webgl2() {
    assert_webgl2_compiles("piece", PIECE_SRC);
}

#[wasm_bindgen_test]
fn frame_shader_compiles_in_webgl2() {
    assert_webgl2_compiles("frame", FRAME_SRC);
}

#[wasm_bindgen_test]
fn ui_shader_compiles_in_webgl2() {
    assert_webgl2_compiles("ui", UI_SRC);
}

#[wasm_bindgen_test(async)]
async fn shaders_compile_in_webgpu() {
    let instance = wgpu::Instance::default();
    let Some(adapter) = instance.request_adapter(&wgpu::RequestAdapterOptions::default()).await else {
        return;
    };
    let (device, _queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("WebGPU device request failed");

    assert_webgpu_shader(&device, "piece", PIECE_SRC).await;
    assert_webgpu_shader(&device, "frame", FRAME_SRC).await;
    assert_webgpu_shader(&device, "ui", UI_SRC).await;
}

fn assert_webgl2_compiles(label: &str, source: &str) {
    let (module, info) = parse_and_validate(source, label);
    let vs_glsl = glsl_webgl2(&module, &info, ShaderStage::Vertex, "vs_main", label);
    let fs_glsl = glsl_webgl2(&module, &info, ShaderStage::Fragment, "fs_main", label);

    let gl = webgl2_context();
    let vertex = compile_shader(&gl, WebGl2RenderingContext::VERTEX_SHADER, &vs_glsl)
        .unwrap_or_else(|err| panic!("{label} WebGL2 vertex compile failed: {err}"));
    let fragment = compile_shader(&gl, WebGl2RenderingContext::FRAGMENT_SHADER, &fs_glsl)
        .unwrap_or_else(|err| panic!("{label} WebGL2 fragment compile failed: {err}"));
    let _program = link_program(&gl, &vertex, &fragment)
        .unwrap_or_else(|err| panic!("{label} WebGL2 program link failed: {err}"));
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

fn glsl_webgl2(
    module: &naga::Module,
    info: &naga::valid::ModuleInfo,
    stage: ShaderStage,
    entry: &str,
    label: &str,
) -> String {
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
    output
}

fn webgl2_context() -> WebGl2RenderingContext {
    let window = window().expect("window missing");
    let document = window.document().expect("document missing");
    let canvas = document
        .create_element("canvas")
        .expect("create canvas")
        .dyn_into::<HtmlCanvasElement>()
        .expect("canvas element");
    let context = canvas
        .get_context("webgl2")
        .expect("get WebGL2 context")
        .expect("WebGL2 unsupported");
    context
        .dyn_into::<WebGl2RenderingContext>()
        .expect("WebGL2 context cast failed")
}

fn compile_shader(
    gl: &WebGl2RenderingContext,
    shader_type: u32,
    source: &str,
) -> Result<WebGlShader, String> {
    let shader = gl
        .create_shader(shader_type)
        .ok_or_else(|| "unable to create shader".to_string())?;
    gl.shader_source(&shader, source);
    gl.compile_shader(&shader);
    let compiled = gl
        .get_shader_parameter(&shader, WebGl2RenderingContext::COMPILE_STATUS)
        .as_bool()
        .unwrap_or(false);
    if compiled {
        Ok(shader)
    } else {
        Err(gl.get_shader_info_log(&shader).unwrap_or_default())
    }
}

fn link_program(
    gl: &WebGl2RenderingContext,
    vertex: &WebGlShader,
    fragment: &WebGlShader,
) -> Result<WebGlProgram, String> {
    let program = gl
        .create_program()
        .ok_or_else(|| "unable to create program".to_string())?;
    gl.attach_shader(&program, vertex);
    gl.attach_shader(&program, fragment);
    gl.link_program(&program);
    let linked = gl
        .get_program_parameter(&program, WebGl2RenderingContext::LINK_STATUS)
        .as_bool()
        .unwrap_or(false);
    if linked {
        Ok(program)
    } else {
        Err(gl.get_program_info_log(&program).unwrap_or_default())
    }
}

async fn assert_webgpu_shader(device: &wgpu::Device, label: &str, source: &str) {
    device.push_error_scope(wgpu::ErrorFilter::Validation);
    device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some(label),
        source: wgpu::ShaderSource::Wgsl(source.into()),
    });
    if let Some(err) = device.pop_error_scope().await {
        panic!("WebGPU shader {label} failed validation: {err:?}");
    }
}
