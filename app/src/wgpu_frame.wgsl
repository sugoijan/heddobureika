struct FrameGlobals {
    view_min: vec2<f32>,
    view_size: vec2<f32>,
    color: vec4<f32>,
    output_gamma: f32,
    puzzle_scale: f32,
    _pad: vec2<f32>,
};

@group(0) @binding(0)
var<uniform> globals: FrameGlobals;

struct VertexIn {
    @location(0) pos: vec2<f32>,
    @location(1) inst_pos: vec2<f32>,
    @location(2) inst_size: vec2<f32>,
    @location(3) inst_rot: f32,
};

struct VertexOut {
    @builtin(position) position: vec4<f32>,
    @location(0) color: vec4<f32>,
};

fn rotate_point(p: vec2<f32>, angle: f32) -> vec2<f32> {
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(p.x * c - p.y * s, p.x * s + p.y * c);
}

fn srgb_to_linear(color: vec3<f32>) -> vec3<f32> {
    return pow(color, vec3<f32>(2.2));
}

fn apply_output_gamma(color: vec3<f32>) -> vec3<f32> {
    return pow(color, vec3<f32>(globals.output_gamma));
}

@vertex
fn vs_main(input: VertexIn) -> VertexOut {
    var out: VertexOut;
    let local = input.pos * input.inst_size;
    let rotated = rotate_point(local, input.inst_rot);
    let world = input.inst_pos + rotated;
    let world_scaled = world * globals.puzzle_scale;
    let ndc_x = (world_scaled.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world_scaled.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    out.color = globals.color;
    return out;
}

@fragment
fn fs_main(input: VertexOut) -> @location(0) vec4<f32> {
    let linear = srgb_to_linear(input.color.rgb);
    return vec4<f32>(apply_output_gamma(linear), input.color.a);
}
