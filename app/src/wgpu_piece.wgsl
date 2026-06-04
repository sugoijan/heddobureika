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
    // Drop-shadow params (read only by the shadow pass). `shadow_offset` is the
    // bottom-right offset in puzzle px (negative of the emboss `light_dir`);
    // `shadow_radius` is the blur radius in atlas texels; `shadow_darkness` is
    // the opacity.
    shadow_offset: vec2<f32>,
    shadow_darkness: f32,
    shadow_radius: f32,
    // Uniform scale-up for the held/dragged piece or group, gated per-instance
    // by a non-zero `inst_drag`. Matches the CPU-side anchor spread so group
    // members stay aligned.
    drag_scale: f32,
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
    @location(10) inst_held: f32,
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
    // Scale-up is gated by the held flag (persists for the whole hold), while the
    // rotation comes from `inst_drag` (animated, may pass through zero). Keeping
    // these independent avoids the group-misalignment glitch that arises if the
    // per-piece scale and the CPU-side anchor spread disagree.
    let drag_scale = select(1.0, globals.drag_scale, input.inst_held > 0.5);
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

// Pseudo-3D flip thickness rim: the piece outline extruded in depth from -t/2
// to +t/2 and rotated about the workspace-vertical flip axis, exactly like the
// face (`vs_main`) but with a per-vertex depth sign rather than one per face.
// Drawn behind the face so the foreshortened/shifted face covers the advancing
// wall, leaving the receding wall visible as the true ~2mm cardboard edge. The
// geometry IS the silhouette, so the rim follows tabs/blanks and never spills
// outside the piece. Fed only for the one piece that is mid-flip; degenerates
// off-screen at the settled endpoints (sin(beta)==0).
struct EdgeIn {
    @location(0) pos: vec2<f32>,
    @location(11) z_sign: f32,
    @location(1) inst_pos: vec2<f32>,
    @location(2) inst_size: vec2<f32>,
    @location(3) inst_rot: f32,
    @location(4) inst_flip: f32,
    @location(5) inst_hover: f32,
    @location(6) inst_drag: f32,
    @location(7) inst_piece_origin: vec2<f32>,
    @location(8) inst_mask_origin: vec2<f32>,
    @location(9) inst_pose_anchor: vec2<f32>,
    @location(10) inst_held: f32,
};

@vertex
fn vs_flip_edge(input: EdgeIn) -> VertexOut {
    var out: VertexOut;
    out.art_uv = vec2<f32>(0.0, 0.0);
    out.mask_uv = vec2<f32>(0.0, 0.0);
    out.local_pos = vec2<f32>(0.0, 0.0);
    out.flip = input.inst_flip;
    out.hover = 0.0;
    out.rot = 0.0;
    let flip_beta = input.inst_flip * 3.14159265;
    if (abs(sin(flip_beta)) < 0.0005) {
        out.position = vec4<f32>(2.0, 2.0, 2.0, 1.0); // off-screen when settled
        return out;
    }
    // `input.pos` is the outline vertex in piece-local px (relative to the
    // bounds top-left), the same frame as `inst_pose_anchor`. Lift it into the
    // padded-quad frame so the rest matches `vs_main` exactly.
    let local = input.pos + globals.mask_pad;
    let anchor_padded = globals.mask_pad + input.inst_pose_anchor;
    let center = anchor_padded;
    let drag = input.inst_drag;
    let drag_scale = select(1.0, globals.drag_scale, input.inst_held > 0.5);
    let drag_rot = drag * 0.017453292;
    let angle = input.inst_rot * 0.017453292 + drag_rot;
    let rotated = rotate_point(local - center, angle) + center;
    let scaled = (rotated - center) * drag_scale + center;
    let world = (input.inst_pos - globals.mask_pad) + scaled;
    let axis_x = (input.inst_pos.x - globals.mask_pad.x) + anchor_padded.x;
    let flipped_x = axis_x
        + (world.x - axis_x) * cos(flip_beta)
        + input.z_sign * globals.flip_thickness_px * 0.5 * sin(flip_beta);
    let world_scaled = vec2<f32>(flipped_x, world.y) * globals.puzzle_scale;
    let ndc_x = (world_scaled.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world_scaled.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    return out;
}

@fragment
fn fs_flip_edge(input: VertexOut) -> @location(0) vec4<f32> {
    // Solid cardboard edge color. Opaque so overlapping wall quads (and the
    // closed-loop seam) don't double-blend.
    let edge_rgb = srgb_to_linear(vec3<f32>(0.56, 0.36, 0.20));
    return vec4<f32>(apply_output_gamma(edge_rgb), 1.0);
}

// Drop shadow. Places the piece exactly like `vs_main` (rotation about the
// anchor, position) but shifts the whole quad by `globals.shadow_offset`
// (bottom-right, opposite the emboss light) and ignores the flip foreshorten
// and drag scale. Drawn directly into the scene right before each group's
// pieces (in z-order), so a group's shadow falls on lower pieces and the
// group's own pieces (drawn after) hide the shadow under them — no intra-group
// self-shadow. Members of one group have disjoint masks, so their shadows don't
// overlap (no double-darkening). `art_uv`/`local_pos` carry the piece's atlas
// cell bounds so the blur taps stay inside the cell (no neighbour bleed).
@vertex
fn vs_shadow(input: VertexIn) -> VertexOut {
    var out: VertexOut;
    let full_size = input.inst_size + globals.mask_pad * 2.0;
    let local = (input.pos + vec2<f32>(0.5, 0.5)) * full_size;
    let anchor_padded = globals.mask_pad + input.inst_pose_anchor;
    let center = anchor_padded;
    let angle = input.inst_rot * 0.017453292;
    let rotated = rotate_point(local - center, angle) + center;
    let world = (input.inst_pos - globals.mask_pad) + rotated;
    // Apply the same flip transform as `vs_main` so a flipped/mid-flip piece's
    // shadow matches its mirrored/foreshortened silhouette (otherwise the shadow
    // looks mirrored). Then shift by the shadow offset.
    let axis_x = (input.inst_pos.x - globals.mask_pad.x) + anchor_padded.x;
    let flip_beta = input.inst_flip * 3.14159265;
    let z_sign = select(1.0, -1.0, input.inst_flip > 0.5);
    let flipped_x = axis_x
        + (world.x - axis_x) * cos(flip_beta)
        + z_sign * globals.flip_thickness_px * 0.5 * sin(flip_beta);
    let world_scaled = (vec2<f32>(flipped_x, world.y) + globals.shadow_offset) * globals.puzzle_scale;
    let ndc_x = (world_scaled.x - globals.view_min.x) / globals.view_size.x * 2.0 - 1.0;
    let ndc_y = 1.0 - (world_scaled.y - globals.view_min.y) / globals.view_size.y * 2.0;
    out.position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    let piece_local = local - globals.mask_pad;
    out.mask_uv = (input.inst_mask_origin + piece_local) / globals.atlas_size;
    // Atlas cell bounds (including padding) for clamping blur taps.
    out.art_uv = (input.inst_mask_origin - globals.mask_pad) / globals.atlas_size;
    out.local_pos = (input.inst_mask_origin + input.inst_size + globals.mask_pad)
        / globals.atlas_size;
    out.flip = 0.0;
    out.hover = 0.0;
    out.rot = angle;
    return out;
}

@fragment
fn fs_shadow(input: VertexOut) -> @location(0) vec4<f32> {
    // 5x5 Gaussian blur of the piece mask coverage; tap spacing scales with the
    // radius. Taps are clamped to the piece's atlas cell so they fade into the
    // transparent padding rather than bleeding into a neighbouring piece.
    let cell_min = input.art_uv;
    let cell_max = input.local_pos;
    var weights = array<f32, 5>(1.0, 4.0, 6.0, 4.0, 1.0);
    let texel = (globals.shadow_radius * 0.5) / globals.atlas_size;
    var coverage = 0.0;
    var weight_sum = 0.0;
    for (var j = 0; j < 5; j = j + 1) {
        for (var i = 0; i < 5; i = i + 1) {
            let w = weights[i] * weights[j];
            let offset = vec2<f32>(f32(i) - 2.0, f32(j) - 2.0) * texel;
            let uv = clamp(input.mask_uv + offset, cell_min, cell_max);
            coverage = coverage + w * textureSampleLevel(mask_tex, tex_sampler, uv, 0.0).r;
            weight_sum = weight_sum + w;
        }
    }
    coverage = coverage / weight_sum;
    if (coverage <= 0.0) {
        discard;
    }
    return vec4<f32>(0.0, 0.0, 0.0, coverage * globals.shadow_darkness);
}
