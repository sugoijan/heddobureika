struct UiGlobals {
    view_min: vec2<f32>,
    view_size: vec2<f32>,
    _pad: vec4<f32>,
};

@group(0) @binding(0)
var<uniform> globals: UiGlobals;

@group(1) @binding(0)
var ui_tex: texture_2d<f32>;

@group(1) @binding(1)
var ui_sampler: sampler;

struct VertexIn {
    @location(0) pos: vec2<f32>,
    @location(1) inst_pos: vec2<f32>,
    @location(2) inst_size: vec2<f32>,
    @location(3) inst_pivot: vec2<f32>,
    @location(4) inst_rot: f32,
    @location(5) inst_opacity: f32,
};

struct VertexOut {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) opacity: f32,
};

fn rotate_point(p: vec2<f32>, angle: f32) -> vec2<f32> {
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(p.x * c - p.y * s, p.x * s + p.y * c);
}

@vertex
fn vs_main(input: VertexIn) -> VertexOut {
    var out: VertexOut;
    let local = input.pos * input.inst_size;
    let angle = input.inst_rot * 0.017453292;
    let rotated = rotate_point(local - input.inst_pivot, angle) + input.inst_pivot;
    let world = input.inst_pos + rotated;
    let ndc_x = (world.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    out.uv = input.pos + vec2<f32>(0.5, 0.5);
    out.opacity = input.inst_opacity;
    return out;
}

@fragment
fn fs_main(input: VertexOut) -> @location(0) vec4<f32> {
    let tex = textureSample(ui_tex, ui_sampler, input.uv);
    return vec4<f32>(tex.rgb, tex.a * input.opacity);
}
