struct UiGlobals {
    view_min: vec2<f32>,
    view_size: vec2<f32>,
    viewport_px: vec2<f32>,
    output_gamma: f32,
    _pad: f32,
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
    @location(6) inst_uv_min: vec2<f32>,
    @location(7) inst_uv_max: vec2<f32>,
    @location(8) inst_color: vec4<f32>,
    @location(9) inst_radius_blur: vec4<f32>,
};

struct VertexOut {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) opacity: f32,
    @location(2) color: vec4<f32>,
    @location(3) local_pos: vec2<f32>,
    @location(4) size: vec2<f32>,
    @location(5) radius_blur: vec4<f32>,
};

fn rotate_point(p: vec2<f32>, angle: f32) -> vec2<f32> {
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(p.x * c - p.y * s, p.x * s + p.y * c);
}

fn srgb_to_linear(color: vec3<f32>) -> vec3<f32> {
    return pow(color, vec3<f32>(2.2));
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
    out.uv = mix(input.inst_uv_min, input.inst_uv_max, input.pos + vec2<f32>(0.5, 0.5));
    out.opacity = input.inst_opacity;
    out.color = input.inst_color;
    out.local_pos = (input.pos + vec2<f32>(0.5, 0.5)) * input.inst_size;
    out.size = input.inst_size;
    out.radius_blur = input.inst_radius_blur;
    return out;
}

@fragment
fn fs_main(input: VertexOut) -> @location(0) vec4<f32> {
    let radius = input.radius_blur.x;
    let blur_uv = input.radius_blur.yz;
    let desaturate = input.radius_blur.w;
    let tex = textureSample(ui_tex, ui_sampler, input.uv);
    let tint = srgb_to_linear(input.color.rgb);
    var color = tex * vec4<f32>(tint, input.color.a);
    if (desaturate > 0.0) {
        let luma = dot(color.rgb, vec3<f32>(0.299, 0.587, 0.114));
        let gray = vec3<f32>(luma);
        color = vec4<f32>(mix(color.rgb, gray, desaturate), color.a);
    }
    var alpha = color.a * input.opacity;
    if (radius > 0.0) {
        let half_size = input.size * 0.5;
        let p = input.local_pos - half_size;
        let r = min(radius, min(half_size.x, half_size.y));
        let q = abs(p) - (half_size - vec2<f32>(r));
        let dist = length(max(q, vec2<f32>(0.0))) + min(max(q.x, q.y), 0.0) - r;
        let pixel = max(globals.view_size.x / globals.viewport_px.x, globals.view_size.y / globals.viewport_px.y);
        let mask = smoothstep(0.0, pixel * 1.5, -dist);
        alpha *= mask;
    }
    let out_rgb = pow(color.rgb, vec3<f32>(globals.output_gamma));
    return vec4<f32>(out_rgb, alpha);
}
