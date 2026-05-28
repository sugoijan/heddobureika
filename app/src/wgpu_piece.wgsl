struct Globals {
    view_min: vec2<f32>,
    view_size: vec2<f32>,
    image_size: vec2<f32>,
    atlas_size: vec2<f32>,
    piece_size: vec2<f32>,
    mask_pad: vec2<f32>,
    render_mode: f32,
    output_gamma: f32,
    emboss_strength: f32,
    emboss_rim: f32,
    outline_width_px: f32,
    edge_aa: f32,
    puzzle_scale: f32,
    flip_thickness_px: f32,
    outline_color: vec4<f32>,
};

@group(0) @binding(0)
var<uniform> globals: Globals;

@group(0) @binding(1)
var art_tex: texture_2d<f32>;

@group(0) @binding(2)
var mask_tex: texture_2d<f32>;

@group(0) @binding(3)
var tex_sampler: sampler;

struct VertexIn {
    @location(0) pos: vec2<f32>,
    @location(1) inst_pos: vec2<f32>,
    @location(2) inst_size: vec2<f32>,
    @location(3) inst_rot: f32,
    @location(4) inst_flip: f32,
    @location(5) inst_hover: f32,
    @location(6) inst_drag: f32,
    @location(7) inst_piece_origin: vec2<f32>,
    @location(8) inst_mask_origin: vec2<f32>,
    @location(9) inst_pose_anchor: vec2<f32>,
};

struct VertexOut {
    @builtin(position) position: vec4<f32>,
    @location(0) art_uv: vec2<f32>,
    @location(1) mask_uv: vec2<f32>,
    @location(2) local_pos: vec2<f32>,
    @location(3) flip: f32,
    @location(4) hover: f32,
    @location(5) rot: f32,
};

fn rotate_point(p: vec2<f32>, angle: f32) -> vec2<f32> {
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(p.x * c - p.y * s, p.x * s + p.y * c);
}

@vertex
fn vs_main(input: VertexIn) -> VertexOut {
    var out: VertexOut;
    let full_size = input.inst_size + globals.mask_pad * 2.0;
    let local = (input.pos + vec2<f32>(0.5, 0.5)) * full_size;
    let anchor_padded = globals.mask_pad + input.inst_pose_anchor;
    let is_back = input.inst_flip > 0.5;
    let center = anchor_padded;
    let drag = input.inst_drag;
    let drag_active = abs(drag) > 1e-4;
    let drag_scale = select(1.0, 1.02, drag_active);
    let drag_rot = drag * 0.017453292;
    let angle = input.inst_rot * 0.017453292 + drag_rot;
    let rotated = rotate_point(local - center, angle) + center;
    let scaled = (rotated - center) * drag_scale + center;
    let world = (input.inst_pos - globals.mask_pad) + scaled;
    // Flip: rotate the thin slab about a WORKSPACE-vertical axis through the
    // piece anchor (applied here in world space, after the piece's own
    // rotation). Orthographically that foreshortens X about the axis by
    // cos(beta); the front/back faces sit +/- half the thickness in depth,
    // which projects to a +/- (thickness/2)*sin(beta) horizontal shift, so the
    // two faces stay a true ~2mm apart. beta = progress*180deg. cos(180)=-1
    // reproduces the mirrored back pose exactly (== the static flip), so no
    // rotation negate is needed.
    let axis_x = (input.inst_pos.x - globals.mask_pad.x) + anchor_padded.x;
    let flip_beta = input.inst_flip * 3.14159265;
    let z_sign = select(1.0, -1.0, is_back);
    let flipped_x = axis_x
        + (world.x - axis_x) * cos(flip_beta)
        + z_sign * globals.flip_thickness_px * 0.5 * sin(flip_beta);
    let world_scaled = vec2<f32>(flipped_x, world.y) * globals.puzzle_scale;
    let ndc_x = (world_scaled.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world_scaled.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    let piece_local = local - globals.mask_pad;
    out.art_uv = (input.inst_piece_origin + piece_local) / globals.image_size;
    out.mask_uv = (input.inst_mask_origin + piece_local) / globals.atlas_size;
    out.local_pos = piece_local;
    out.flip = input.inst_flip;
    out.hover = input.inst_hover;
    out.rot = angle;
    return out;
}

fn srgb_to_linear(color: vec3<f32>) -> vec3<f32> {
    return pow(color, vec3<f32>(2.2));
}

fn apply_output_gamma(color: vec3<f32>) -> vec3<f32> {
    return pow(color, vec3<f32>(globals.output_gamma));
}

fn outline_rgb_for(hover: f32) -> vec3<f32> {
    let owned = hover > 1.5 && hover < 2.5;
    let solved = globals.outline_color.g > 0.5;
    let use_owned = owned && !solved;
    let owned_rgb = vec3<f32>(0.1176, 0.4706, 0.8235);
    return select(globals.outline_color.rgb, owned_rgb, use_owned);
}

fn back_pattern(local_pos: vec2<f32>) -> vec3<f32> {
    let tile = vec2<f32>(28.0, 28.0);
    let p = local_pos - floor(local_pos / tile) * tile;
    let bg = srgb_to_linear(vec3<f32>(0.56, 0.36, 0.20));
    let fg1 = srgb_to_linear(vec3<f32>(0.45, 0.27, 0.14));
    let fg2 = srgb_to_linear(vec3<f32>(0.23, 0.14, 0.09));
    var color = bg;
    if (distance(p, vec2<f32>(7.0, 7.0)) < 2.8) {
        color = fg1;
    }
    if (distance(p, vec2<f32>(21.0, 21.0)) < 2.8) {
        color = fg1;
    }
    if (distance(p, vec2<f32>(21.0, 7.0)) < 1.8) {
        color = fg2;
    }
    if (distance(p, vec2<f32>(7.0, 21.0)) < 1.8) {
        color = fg2;
    }
    return color;
}

@fragment
fn fs_main(input: VertexOut) -> @location(0) vec4<f32> {
    let mask = textureSample(mask_tex, tex_sampler, input.mask_uv).r;
    let outline_threshold = 0.05;
    let mask_fwidth = abs(dpdx(mask)) + abs(dpdy(mask));
    let edge_aa = max(mask_fwidth * globals.edge_aa, 1e-4);
    let outline_stroke_width_px = max(globals.outline_width_px, 0.5);
    let outline_texel = vec2<f32>(outline_stroke_width_px, outline_stroke_width_px) / globals.atlas_size;
    let outline_left = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(-outline_texel.x, 0.0)).r;
    let outline_right = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(outline_texel.x, 0.0)).r;
    let outline_up = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(0.0, -outline_texel.y)).r;
    let outline_down = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(0.0, outline_texel.y)).r;
    let outline_up_left = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(-outline_texel.x, -outline_texel.y)).r;
    let outline_up_right = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(outline_texel.x, -outline_texel.y)).r;
    let outline_down_left = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(-outline_texel.x, outline_texel.y)).r;
    let outline_down_right = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(outline_texel.x, outline_texel.y)).r;
    let outline_max_cardinal = max(max(outline_left, outline_right), max(outline_up, outline_down));
    let outline_max_diagonal = max(max(outline_up_left, outline_up_right), max(outline_down_left, outline_down_right));
    let outline_max_neighbor = max(outline_max_cardinal, outline_max_diagonal);
    let outline_min_cardinal = min(min(outline_left, outline_right), min(outline_up, outline_down));
    let outline_min_diagonal = min(min(outline_up_left, outline_up_right), min(outline_down_left, outline_down_right));
    let outline_min_neighbor = min(outline_min_cardinal, outline_min_diagonal);
    let outline_neighbor_fwidth = abs(dpdx(outline_max_neighbor)) + abs(dpdy(outline_max_neighbor));
    let outline_aa = max(max(mask_fwidth, outline_neighbor_fwidth) * globals.edge_aa, 1e-4);
    let outline_outside = 1.0 - smoothstep(outline_threshold, outline_threshold + outline_aa, mask);
    let outline_inside = smoothstep(outline_threshold, outline_threshold + outline_aa, outline_max_neighbor);
    let outline_edge = outline_outside * outline_inside;
    if (globals.render_mode > 0.5) {
        let show = input.hover >= 0.5 && input.flip <= 0.5;
        let outline = srgb_to_linear(outline_rgb_for(input.hover));
        let outline_alpha = globals.outline_color.a * outline_edge;
        let out_color = vec4<f32>(apply_output_gamma(outline), outline_alpha);
        return select(vec4<f32>(0.0), out_color, show);
    }
    let art = textureSample(art_tex, tex_sampler, input.art_uv);
    let edge_alpha = smoothstep(outline_threshold, outline_threshold + edge_aa, mask);

    let flipped = input.flip > 0.5;
    var rgb = select(art.rgb, back_pattern(input.local_pos), flipped);
    var alpha = select(art.a * edge_alpha, edge_alpha, flipped);
    let emboss_texel = vec2<f32>(1.0, 1.0) / globals.atlas_size;
    let emboss_left = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(-emboss_texel.x, 0.0)).r;
    let emboss_right = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(emboss_texel.x, 0.0)).r;
    let emboss_up = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(0.0, -emboss_texel.y)).r;
    let emboss_down = textureSample(mask_tex, tex_sampler, input.mask_uv + vec2<f32>(0.0, emboss_texel.y)).r;
    let grad_local = vec2<f32>(emboss_right - emboss_left, emboss_down - emboss_up);
    let grad_len = length(grad_local);
    let inv_grad_len = select(0.0, 1.0 / grad_len, grad_len > 1e-4);
    let grad_dir = grad_local * inv_grad_len;
    let outer_mask = textureSample(
        mask_tex,
        tex_sampler,
        input.mask_uv - grad_dir * emboss_texel * globals.emboss_rim,
    ).r;
    let rim = clamp(mask - outer_mask, 0.0, 1.0);
    var normal = -grad_dir;
    normal = rotate_point(normal, input.rot);
    let light_dir = normalize(vec2<f32>(-1.0, -1.0));
    let light = dot(normalize(normal), light_dir);
    let highlight = max(light, 0.0);
    let shadow = max(-light, 0.0);
    let emboss_factor = select(1.0, 0.0, flipped);
    let emboss = rim * globals.emboss_strength * emboss_factor;
    // XXX: add a little bit of bias to both to be closer the SVG filter look
    rgb = mix(rgb, vec3<f32>(1.0), emboss * highlight * 1.1);
    rgb = mix(rgb, vec3<f32>(0.0), emboss * shadow * 1.5);
    if (input.hover > 2.5) {
        let center = globals.piece_size * 0.5;
        let dot = select(0.0, 1.0, distance(input.local_pos, center) < 4.0);
        let dot_color = srgb_to_linear(vec3<f32>(0.0, 1.0, 0.0));
        rgb = rgb * (1.0 - dot) + dot_color * dot;
    }
    let flip_hover = flipped && input.hover > 0.5;
    let flipped_neighbor_fwidth = abs(dpdx(outline_min_neighbor)) + abs(dpdy(outline_min_neighbor));
    let flipped_outline_aa = max(max(mask_fwidth, flipped_neighbor_fwidth) * globals.edge_aa, 1e-4);
    let flipped_inside = smoothstep(outline_threshold, outline_threshold + flipped_outline_aa, mask);
    let flipped_outside = 1.0 - smoothstep(outline_threshold, outline_threshold + flipped_outline_aa, outline_min_neighbor);
    let flipped_edge = flipped_inside * flipped_outside;
    let flipped_outline = srgb_to_linear(outline_rgb_for(input.hover));
    rgb = mix(rgb, flipped_outline, flipped_edge * select(0.0, 1.0, flip_hover));
    if (mask < outline_threshold) {
        discard;
    }
    return vec4<f32>(apply_output_gamma(rgb), alpha);
}

// Pseudo-3D flip "extrusion" side: a solid cardboard band on the workspace-
// vertical flip axis. Its width is the projected thickness (thickness*sin(beta))
// so it is zero at the endpoints and fully visible at the half-way (edge-on)
// point; its height matches the piece's workspace vertical extent. Drawn behind
// the face so the (foreshortening) face covers it until edge-on, then the rim
// shows as the solid 2mm edge between the front and back faces. Degenerates
// off-screen when the piece is not mid-flip, so it costs nothing when settled.
@vertex
fn vs_flip_side(input: VertexIn) -> VertexOut {
    var out: VertexOut;
    out.art_uv = vec2<f32>(0.0, 0.0);
    out.mask_uv = vec2<f32>(0.0, 0.0);
    out.local_pos = vec2<f32>(0.0, 0.0);
    out.flip = input.inst_flip;
    out.hover = 0.0;
    out.rot = 0.0;
    let flip_beta = input.inst_flip * 3.14159265;
    let band_w = globals.flip_thickness_px * abs(sin(flip_beta));
    if (band_w < 0.001) {
        out.position = vec4<f32>(2.0, 2.0, 2.0, 1.0); // off-screen / clipped
        return out;
    }
    let anchor_padded = globals.mask_pad + input.inst_pose_anchor;
    let drag = input.inst_drag;
    let drag_rot = drag * 0.017453292;
    let angle = input.inst_rot * 0.017453292 + drag_rot;
    // Workspace vertical extent of the (rotated) piece bounding box.
    let height_ws = abs(input.inst_size.x * sin(angle)) + abs(input.inst_size.y * cos(angle));
    let axis_x = (input.inst_pos.x - globals.mask_pad.x) + anchor_padded.x;
    let axis_y = (input.inst_pos.y - globals.mask_pad.y) + anchor_padded.y;
    let wx = axis_x + input.pos.x * band_w;
    let wy = axis_y + input.pos.y * height_ws;
    let world_scaled = vec2<f32>(wx, wy) * globals.puzzle_scale;
    let ndc_x = (world_scaled.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world_scaled.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    // Invert the (un-flipped) piece transform to find which piece texel this
    // band point covers, so the rim is trimmed to the piece silhouette rather
    // than its bounding box. Clamp to this piece's atlas cell so out-of-piece
    // points sample transparent padding, never a neighbour.
    let world_pt = vec2<f32>(wx, wy);
    let rotated = world_pt - (input.inst_pos - globals.mask_pad);
    let local = rotate_point(rotated - anchor_padded, -angle) + anchor_padded;
    let piece_local = local - globals.mask_pad;
    let cell_min = (input.inst_mask_origin - globals.mask_pad) / globals.atlas_size;
    let cell_max = (input.inst_mask_origin + input.inst_size + globals.mask_pad) / globals.atlas_size;
    out.mask_uv = clamp(
        (input.inst_mask_origin + piece_local) / globals.atlas_size,
        cell_min,
        cell_max,
    );
    return out;
}

@fragment
fn fs_flip_side(input: VertexOut) -> @location(0) vec4<f32> {
    // Trim the rim to the piece silhouette (so it doesn't overrun the bounding
    // box) via the piece mask, then fill with the solid cardboard edge color.
    let mask = textureSample(mask_tex, tex_sampler, input.mask_uv).r;
    let outline_threshold = 0.05;
    let mask_fwidth = abs(dpdx(mask)) + abs(dpdy(mask));
    let edge_aa = max(mask_fwidth * globals.edge_aa, 1e-4);
    let edge_alpha = smoothstep(outline_threshold, outline_threshold + edge_aa, mask);
    if (mask < outline_threshold) {
        discard;
    }
    let edge_rgb = srgb_to_linear(vec3<f32>(0.56, 0.36, 0.20));
    return vec4<f32>(apply_output_gamma(edge_rgb), edge_alpha);
}
