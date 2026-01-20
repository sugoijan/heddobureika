use gloo::events::{EventListener, EventListenerOptions, EventListenerPhase};
use gloo::render::{request_animation_frame, AnimationFrame};
use gloo::timers::callback::Interval;
use js_sys::{Date, Function, Reflect};
use std::collections::VecDeque;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    Element, Event, HtmlCanvasElement, HtmlImageElement, HtmlInputElement, HtmlSelectElement,
    InputEvent, KeyboardEvent, Touch, TouchEvent,
};
use yew::prelude::*;

mod core;
use crate::core::*;
mod renderer;
use crate::renderer::{build_mask_atlas, Instance, MaskAtlasData, WgpuRenderer};

const IMAGE_SRC: &str = "puzzles/zoe-potter.jpg";

#[derive(Default)]
struct DragHandlers {
    on_move: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_release: Option<Rc<dyn Fn(&MouseEvent)>>,
    on_touch_move: Option<Rc<dyn Fn(&TouchEvent)>>,
    on_touch_release: Option<Rc<dyn Fn(&TouchEvent)>>,
}

fn load_saved_board() -> Option<SavedBoard> {
    let window = web_sys::window()?;
    let storage = window.local_storage().ok()??;
    let raw = storage.get_item(STORAGE_KEY).ok()??;
    let state: SavedBoard = serde_json::from_str(&raw).ok()?;
    if state.version != STORAGE_VERSION {
        return None;
    }
    Some(state)
}

fn save_board_state(state: &SavedBoard) {
    let Ok(raw) = serde_json::to_string(state) else {
        return;
    };
    let Some(storage) = web_sys::window().and_then(|window| window.local_storage().ok().flatten())
    else {
        return;
    };
    let _ = storage.set_item(STORAGE_KEY, &raw);
}

fn event_to_svg_coords(
    event: &MouseEvent,
    svg_ref: &NodeRef,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
) -> Option<(f32, f32)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let rect_left = rect.left() as f32;
    let rect_top = rect.top() as f32;
    let x = view_min_x + (event.client_x() as f32 - rect_left) * view_width / rect_width;
    let y = view_min_y + (event.client_y() as f32 - rect_top) * view_height / rect_height;
    Some((x, y))
}

fn touch_from_event(event: &TouchEvent, touch_id: Option<i32>, use_changed: bool) -> Option<Touch> {
    let list = if use_changed {
        event.changed_touches()
    } else {
        event.touches()
    };
    if let Some(id) = touch_id {
        for idx in 0..list.length() {
            if let Some(touch) = list.item(idx) {
                if touch.identifier() == id {
                    return Some(touch);
                }
            }
        }
        None
    } else {
        list.item(0)
    }
}

fn touch_event_to_svg_coords(
    event: &TouchEvent,
    svg_ref: &NodeRef,
    view_min_x: f32,
    view_min_y: f32,
    view_width: f32,
    view_height: f32,
    touch_id: Option<i32>,
    use_changed: bool,
) -> Option<(f32, f32)> {
    let svg = svg_ref.cast::<Element>()?;
    let rect = svg.get_bounding_client_rect();
    let rect_width = rect.width() as f32;
    let rect_height = rect.height() as f32;
    if rect_width <= 0.0 || rect_height <= 0.0 {
        return None;
    }
    let touch = touch_from_event(event, touch_id, use_changed)?;
    let rect_left = rect.left() as f32;
    let rect_top = rect.top() as f32;
    let x = view_min_x + (touch.client_x() as f32 - rect_left) * view_width / rect_width;
    let y = view_min_y + (touch.client_y() as f32 - rect_top) * view_height / rect_height;
    Some((x, y))
}

fn pick_piece_at(
    x: f32,
    y: f32,
    positions: &[(f32, f32)],
    rotations: &[f32],
    flips: &[bool],
    z_order: &[usize],
    mask_atlas: &MaskAtlasData,
    cols: usize,
    piece_width: f32,
    piece_height: f32,
    mask_pad: f32,
) -> Option<usize> {
    if cols == 0 || piece_width <= 0.0 || piece_height <= 0.0 {
        return None;
    }
    let center_x = piece_width * 0.5;
    let center_y = piece_height * 0.5;
    let min_x = -mask_pad;
    let min_y = -mask_pad;
    let max_x = piece_width + mask_pad;
    let max_y = piece_height + mask_pad;
    let atlas_width = mask_atlas.width as i32;
    let atlas_height = mask_atlas.height as i32;
    for &piece_id in z_order.iter().rev() {
        let col = piece_id % cols;
        let row = piece_id / cols;
        let base_x = col as f32 * piece_width;
        let base_y = row as f32 * piece_height;
        let pos = positions.get(piece_id).copied().unwrap_or((base_x, base_y));
        let rotation = rotations.get(piece_id).copied().unwrap_or(0.0);
        let flipped = flips.get(piece_id).copied().unwrap_or(false);
        let mut local_x = x - pos.0;
        let mut local_y = y - pos.1;
        if rotation.abs() > f32::EPSILON {
            let (rx, ry) = rotate_point(local_x, local_y, center_x, center_y, -rotation);
            local_x = rx;
            local_y = ry;
        }
        if flipped {
            local_x = piece_width - local_x;
        }
        if local_x < min_x
            || local_y < min_y
            || local_x > max_x
            || local_y > max_y
        {
            continue;
        }
        let mask_origin = match mask_atlas.origins.get(piece_id) {
            Some(origin) => *origin,
            None => continue,
        };
        let mask_x = (mask_origin[0] + local_x).floor() as i32;
        let mask_y = (mask_origin[1] + local_y).floor() as i32;
        if mask_x < 0 || mask_y < 0 || mask_x >= atlas_width || mask_y >= atlas_height {
            continue;
        }
        let idx = (mask_y as u32 * mask_atlas.width + mask_x as u32) as usize * 4;
        let alpha = mask_atlas.pixels.get(idx).copied().unwrap_or(0);
        if alpha > 16 {
            return Some(piece_id);
        }
    }
    None
}

fn on_setting_change<F>(settings: UseStateHandle<ShapeSettings>, updater: F) -> Callback<InputEvent>
where
    F: Fn(&mut ShapeSettings, f32) + 'static,
{
    Callback::from(move |event: InputEvent| {
        let input: HtmlInputElement = event.target_unchecked_into();
        if let Ok(value) = input.value().parse::<f32>() {
            let mut next = (*settings).clone();
            updater(&mut next, value);
            settings.set(next);
        }
    })
}

fn time_nonce(previous: u32) -> u32 {
    let now = Date::now() as u32;
    splitmix32(now ^ previous.wrapping_add(0x9E37_79B9))
}

fn prefers_dark_mode() -> bool {
    let Some(window) = web_sys::window() else {
        return false;
    };
    let Ok(match_media) = Reflect::get(&window, &"matchMedia".into()) else {
        return false;
    };
    let Ok(match_media) = match_media.dyn_into::<Function>() else {
        return false;
    };
    let Ok(query) = match_media.call1(&window, &"(prefers-color-scheme: dark)".into()) else {
        return false;
    };
    Reflect::get(&query, &"matches".into())
        .ok()
        .and_then(|value| value.as_bool())
        .unwrap_or(false)
}
#[function_component(App)]
fn app() -> Html {
    let image_size = use_state(|| None::<(u32, u32)>);
    let image_size_value = *image_size;
    let image_element = use_state(|| None::<HtmlImageElement>);
    let settings = use_state(ShapeSettings::default);
    let settings_value = (*settings).clone();
    let depth_cap = settings_value
        .tab_depth_cap
        .clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    let curve_detail = settings_value
        .curve_detail
        .clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    let mut grid_choices = if let Some((width, height)) = image_size_value {
        build_grid_choices(width, height)
    } else {
        Vec::new()
    };
    if image_size_value.is_some() && grid_choices.is_empty() {
        grid_choices.push(FALLBACK_GRID);
    }
    let grid_default_index = grid_choices
        .iter()
        .position(|choice| choice.target_count == DEFAULT_TARGET_COUNT)
        .unwrap_or(0);
    let grid_index = use_state(|| grid_default_index);
    let grid_index_value = *grid_index;
    let grid = grid_choices
        .get(grid_index_value)
        .copied()
        .or_else(|| grid_choices.first().copied())
        .unwrap_or(FALLBACK_GRID);
    let total = (grid.cols * grid.rows) as usize;
    let grid_label = if image_size_value.is_some() && !grid_choices.is_empty() {
        grid_choice_label(&grid)
    } else {
        "--".to_string()
    };
    let solve_time_label = if image_size_value.is_some() {
        let pieces = grid.actual_count as f32;
        format_duration(SOLVE_TIME_FACTOR * pieces.powf(SOLVE_TIME_EXPONENT))
    } else {
        "--".to_string()
    };
    let grid_options: Html = grid_choices
        .iter()
        .enumerate()
        .map(|(index, choice)| {
            let label = grid_choice_label(choice);
            html! {
                <option value={index.to_string()} selected={index == grid_index_value}>
                    {label}
                </option>
            }
        })
        .collect();

    let tab_width_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_width = value.clamp(TAB_WIDTH_MIN, TAB_WIDTH_MAX);
    });
    let tab_depth_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_depth = value.clamp(TAB_DEPTH_MIN, TAB_DEPTH_MAX);
    });
    let tab_size_scale_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_size_scale = value.clamp(TAB_SIZE_SCALE_MIN, TAB_SIZE_SCALE_MAX);
    });
    let tab_size_min_input = on_setting_change(settings.clone(), |settings, value| {
        let max_allowed = settings
            .tab_size_max
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_min = value.clamp(TAB_SIZE_MIN_LIMIT, max_allowed);
    });
    let tab_size_max_input = on_setting_change(settings.clone(), |settings, value| {
        let min_allowed = settings
            .tab_size_min
            .clamp(TAB_SIZE_MIN_LIMIT, TAB_SIZE_MAX_LIMIT);
        settings.tab_size_max = value.clamp(min_allowed, TAB_SIZE_MAX_LIMIT);
    });
    let skew_input = on_setting_change(settings.clone(), |settings, value| {
        settings.skew_range = value.clamp(0.0, SKEW_RANGE_MAX);
    });
    let jitter_strength_input = on_setting_change(settings.clone(), |settings, value| {
        settings.jitter_strength = value.clamp(JITTER_STRENGTH_MIN, JITTER_STRENGTH_MAX);
    });
    let jitter_len_bias_input = on_setting_change(settings.clone(), |settings, value| {
        settings.jitter_len_bias = value.clamp(JITTER_LEN_BIAS_MIN, JITTER_LEN_BIAS_MAX);
    });
    let tab_depth_cap_input = on_setting_change(settings.clone(), |settings, value| {
        settings.tab_depth_cap = value.clamp(TAB_DEPTH_CAP_MIN, TAB_DEPTH_CAP_MAX);
    });
    let curve_detail_input = on_setting_change(settings.clone(), |settings, value| {
        settings.curve_detail = value.clamp(CURVE_DETAIL_MIN, CURVE_DETAIL_MAX);
    });
    let variation_input = on_setting_change(settings.clone(), |settings, value| {
        settings.variation = value.clamp(VARIATION_MIN, VARIATION_MAX);
    });
    let line_bend_input = on_setting_change(settings.clone(), |settings, value| {
        settings.line_bend_ratio = value.clamp(LINE_BEND_MIN, MAX_LINE_BEND_RATIO);
    });
    let positions = use_state(Vec::<(f32, f32)>::new);
    let active_id = use_state(|| None::<usize>);
    let active_id_value = *active_id;
    let dragging_members = use_state(Vec::<usize>::new);
    let animating_members = use_state(Vec::<usize>::new);
    let drag_state = use_mut_ref(|| None::<DragState>);
    let drag_frame = use_mut_ref(|| None::<AnimationFrame>);
    let drag_pending = use_mut_ref(|| None::<(f32, f32)>);
    let rotation_anim = use_mut_ref(|| None::<RotationAnimation>);
    let rotation_anim_handle = use_mut_ref(|| None::<Interval>);
    let rotation_queue = use_mut_ref(|| VecDeque::<QueuedRotation>::new());
    let preview_corner = use_state(|| PreviewCorner::BottomLeft);
    let preview_revealed = use_state(|| false);
    let theme_mode = use_state(|| ThemeMode::System);
    let theme_mode_value = *theme_mode;
    let theme_toggle_ref = use_node_ref();
    let drag_handlers = use_mut_ref(DragHandlers::default);
    let restore_state = use_mut_ref(|| None::<SavedBoard>);
    let restore_attempted = use_mut_ref(|| false);
    let grid_initialized = use_mut_ref(|| false);
    let svg_ref = use_node_ref();
    let canvas_ref = use_node_ref();
    let workspace_scale = use_state(|| WORKSPACE_SCALE_DEFAULT);
    let workspace_scale_value = *workspace_scale;
    let z_order = use_state(Vec::<usize>::new);
    let connections = use_state(Vec::<[bool; 4]>::new);
    let rotations = use_state(Vec::<f32>::new);
    let flips = use_state(Vec::<bool>::new);
    let rotation_enabled = use_state(|| true);
    let rotation_enabled_value = *rotation_enabled;
    let animations_enabled = use_state(|| false);
    let animations_enabled_value = *animations_enabled;
    let emboss_enabled = use_state(|| false);
    let emboss_enabled_value = *emboss_enabled;
    let fast_render = use_state(|| true);
    let fast_render_value = *fast_render;
    let fast_filter = use_state(|| true);
    let fast_filter_value = *fast_filter;
    let wgpu_enabled = use_state(|| false);
    let wgpu_enabled_value = *wgpu_enabled;
    let wgpu_renderer = use_mut_ref(|| None::<WgpuRenderer>);
    let mask_atlas = use_mut_ref(|| None::<Rc<MaskAtlasData>>);
    let wgpu_revision = use_state(|| 0u32);
    let wgpu_revision_value = *wgpu_revision;
    let hovered_id = use_state(|| None::<usize>);
    let rotation_noise = use_state(|| ROTATION_NOISE_DEFAULT);
    let rotation_noise_value = *rotation_noise;
    let rotation_snap_tolerance = use_state(|| ROTATION_SNAP_TOLERANCE_DEFAULT_DEG);
    let rotation_snap_tolerance_value = *rotation_snap_tolerance;
    let rotation_lock_threshold = use_state(|| ROTATION_LOCK_THRESHOLD_DEFAULT);
    let rotation_lock_threshold_value = *rotation_lock_threshold;
    let snap_distance_ratio = use_state(|| SNAP_DISTANCE_RATIO_DEFAULT);
    let snap_distance_ratio_value = *snap_distance_ratio;
    let scramble_nonce = use_state(|| 0u32);
    let scramble_nonce_value = *scramble_nonce;
    let save_revision = use_state(|| 0u32);
    let save_revision_value = *save_revision;
    let frame_snap_ratio = use_state(|| FRAME_SNAP_DEFAULT);
    let frame_snap_ratio_value = *frame_snap_ratio;
    let solved = use_state(|| false);
    let solved_value = *solved;
    let show_controls = use_state(|| false);
    let show_controls_value = *show_controls;
    let show_debug = use_state(|| false);
    let show_debug_value = *show_debug;
    let dragging_members_value = (*dragging_members).clone();
    let animating_members_value = (*animating_members).clone();
    let hovered_id_value = *hovered_id;
    let preview_revealed_value = *preview_revealed;
    let status_label = if solved_value { "Solved" } else { "In progress" };
    let status_class = if solved_value {
        "status status-solved"
    } else {
        "status"
    };
    let solved_banner = if solved_value {
        html! { <div class="solved-banner">{ "Solved!" }</div> }
    } else {
        html! {}
    };
    let seed_label = if image_size_value.is_some() {
        let cols = grid.cols as usize;
        let rows = grid.rows as usize;
        format!(
            "{:#x}",
            scramble_seed(PUZZLE_SEED, scramble_nonce_value, cols, rows)
        )
    } else {
        "--".to_string()
    };
    let (connections_label, border_connections_label) = if image_size_value.is_some() {
        let connections_value = &*connections;
        if connections_value.len() == total {
            let (connected, border_connected, total_expected, border_expected) = count_connections(
                connections_value,
                grid.cols as usize,
                grid.rows as usize,
            );
            (
                format_progress(connected, total_expected),
                format_progress(border_connected, border_expected),
            )
        } else {
            ("--".to_string(), "--".to_string())
        }
    } else {
        ("--".to_string(), "--".to_string())
    };
    {
        let positions = positions.clone();
        let active_id = active_id.clone();
        let drag_state = drag_state.clone();
        let dragging_members = dragging_members.clone();
        let animating_members = animating_members.clone();
        let rotation_anim = rotation_anim.clone();
        let rotation_anim_handle = rotation_anim_handle.clone();
        let rotation_queue = rotation_queue.clone();
        let z_order = z_order.clone();
        let connections = connections.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let solved = solved.clone();
        let scramble_nonce = scramble_nonce.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let grid_default_index = grid_default_index;
        let restore_state = restore_state.clone();
        let restore_attempted = restore_attempted.clone();
        let grid_initialized = grid_initialized.clone();
        use_effect_with(
            (grid_index_value, image_size_value),
            move |(grid_index_value, image_size_value)| {
                if let Some((width, height)) = *image_size_value {
                    let mut allow_scramble = true;
                    let mut skip_scramble = false;
                    if grid_choices.is_empty() {
                        allow_scramble = false;
                    } else if *grid_index_value >= grid_choices.len() {
                        grid_index.set(grid_default_index.min(grid_choices.len() - 1));
                        *grid_initialized.borrow_mut() = true;
                        allow_scramble = false;
                    }
                    if allow_scramble {
                        let saved_state = {
                            let mut attempted = restore_attempted.borrow_mut();
                            if !*attempted {
                                *attempted = true;
                                *restore_state.borrow_mut() = load_saved_board();
                            }
                            restore_state.borrow_mut().take()
                        };
                        if let Some(state) = saved_state {
                            if state.image_width == width && state.image_height == height {
                                if let Some(saved_index) =
                                    grid_choice_index(&grid_choices, state.cols, state.rows)
                                {
                                    if saved_index != *grid_index_value {
                                        *restore_state.borrow_mut() = Some(state);
                                        grid_index.set(saved_index);
                                        *grid_initialized.borrow_mut() = true;
                                        skip_scramble = true;
                                    } else {
                                        let cols = state.cols as usize;
                                        let rows = state.rows as usize;
                                        let total = cols * rows;
                                        if validate_saved_board(&state, total) {
                                            let piece_width = width as f32 / state.cols as f32;
                                            let piece_height = height as f32 / state.rows as f32;
                                            positions.set(state.positions.clone());
                                            rotations.set(state.rotations.clone());
                                            flips.set(state.flips.clone());
                                            connections.set(state.connections.clone());
                                            z_order.set(state.z_order.clone());
                                            scramble_nonce.set(state.scramble_nonce);
                                            active_id.set(None);
                                            dragging_members.set(Vec::new());
                                            animating_members.set(Vec::new());
                                            *rotation_anim.borrow_mut() = None;
                                            rotation_anim_handle.borrow_mut().take();
                                            rotation_queue.borrow_mut().clear();
                                            *drag_state.borrow_mut() = None;
                                            let solved_now = is_solved(
                                                &state.positions,
                                                &state.rotations,
                                                &state.flips,
                                                &state.connections,
                                                cols,
                                                rows,
                                                piece_width,
                                                piece_height,
                                                rotation_enabled_value,
                                            );
                                            solved.set(solved_now);
                                            *grid_initialized.borrow_mut() = true;
                                            skip_scramble = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if allow_scramble && !skip_scramble {
                        let mut initialized = grid_initialized.borrow_mut();
                        if !*initialized {
                            if *grid_index_value != grid_default_index {
                                grid_index.set(grid_default_index);
                                *initialized = true;
                                allow_scramble = false;
                            } else {
                                *initialized = true;
                            }
                        }
                    }
                    if allow_scramble && !skip_scramble {
                        let grid = grid_choices
                            .get(*grid_index_value)
                            .copied()
                            .unwrap_or(FALLBACK_GRID);
                        let cols = grid.cols as usize;
                        let rows = grid.rows as usize;
                        let piece_width = width as f32 / grid.cols as f32;
                        let piece_height = height as f32 / grid.rows as f32;
                        let view_width = width as f32 * workspace_scale_value;
                        let view_height = height as f32 * workspace_scale_value;
                        let view_min_x = (width as f32 - view_width) * 0.5;
                        let view_min_y = (height as f32 - view_height) * 0.5;
                        let margin =
                            piece_width.max(piece_height) * (depth_cap + MAX_LINE_BEND_RATIO);
                        let nonce = time_nonce(*scramble_nonce);
                        let seed = scramble_seed(PUZZLE_SEED, nonce, cols, rows);
                        let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
                        let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
                        let (next_positions, order) = scramble_layout(
                            seed,
                            cols,
                            rows,
                            piece_width,
                            piece_height,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            margin,
                        );
                        positions.set(next_positions);
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        animating_members.set(Vec::new());
                        *rotation_anim.borrow_mut() = None;
                        rotation_anim_handle.borrow_mut().take();
                        rotation_queue.borrow_mut().clear();
                        *drag_state.borrow_mut() = None;
                        z_order.set(order);
                        connections.set(vec![[false; 4]; cols * rows]);
                        rotations.set(scramble_rotations(
                            rotation_seed,
                            cols * rows,
                            rotation_enabled_value,
                        ));
                        flips.set(scramble_flips(flip_seed, cols * rows, FLIP_CHANCE));
                        solved.set(false);
                        scramble_nonce.set(nonce);
                    }
                }
                || ()
            },
        );
    }

    let on_grid_change = {
        let grid_index = grid_index.clone();
        let grid_choices_len = grid_choices.len();
        Callback::from(move |event: Event| {
            let select: HtmlSelectElement = event.target_unchecked_into();
            if let Ok(value) = select.value().parse::<usize>() {
                if value < grid_choices_len {
                    grid_index.set(value);
                }
            }
        })
    };
    let on_workspace_scale = {
        let workspace_scale = workspace_scale.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                workspace_scale.set(value.clamp(WORKSPACE_SCALE_MIN, WORKSPACE_SCALE_MAX));
            }
        })
    };
    let on_frame_snap = {
        let frame_snap_ratio = frame_snap_ratio.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                frame_snap_ratio.set(value.clamp(FRAME_SNAP_MIN, FRAME_SNAP_MAX));
            }
        })
    };
    let on_snap_distance = {
        let snap_distance_ratio = snap_distance_ratio.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                snap_distance_ratio.set(value.clamp(
                    SNAP_DISTANCE_RATIO_MIN,
                    SNAP_DISTANCE_RATIO_MAX,
                ));
            }
        })
    };
    let on_rotation_toggle = {
        let rotation_enabled = rotation_enabled.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let positions = positions.clone();
        let connections = connections.clone();
        let image_size = image_size.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let solved = solved.clone();
        let save_revision = save_revision.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            rotation_enabled.set(enabled);
            let total = positions.len();
            let rotations_snapshot = if enabled {
                (*rotations).clone()
            } else {
                let zeroed = vec![0.0; total];
                rotations.set(zeroed.clone());
                zeroed
            };
            if let Some((width, height)) = *image_size {
                let grid = grid_choices
                    .get(*grid_index)
                    .copied()
                    .unwrap_or(FALLBACK_GRID);
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let piece_width = width as f32 / grid.cols as f32;
                let piece_height = height as f32 / grid.rows as f32;
                let positions_snapshot = (*positions).clone();
                let flips_snapshot = (*flips).clone();
                let connections_snapshot = (*connections).clone();
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    enabled,
                );
                solved.set(solved_now);
            }
            save_revision.set(save_revision.wrapping_add(1));
        })
    };
    let on_rotation_noise = {
        let rotation_noise = rotation_noise.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                rotation_noise.set(value.clamp(ROTATION_NOISE_MIN, ROTATION_NOISE_MAX));
            }
        })
    };
    let on_rotation_snap_tolerance = {
        let rotation_snap_tolerance = rotation_snap_tolerance.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                rotation_snap_tolerance.set(value.clamp(
                    ROTATION_SNAP_TOLERANCE_MIN_DEG,
                    ROTATION_SNAP_TOLERANCE_MAX_DEG,
                ));
            }
        })
    };
    let on_rotation_lock_threshold = {
        let rotation_lock_threshold = rotation_lock_threshold.clone();
        let positions = positions.clone();
        Callback::from(move |event: InputEvent| {
            let input: HtmlInputElement = event.target_unchecked_into();
            if let Ok(value) = input.value().parse::<f32>() {
                let max_value = positions.len().max(ROTATION_LOCK_THRESHOLD_MIN);
                let rounded = value.round() as usize;
                let clamped = rounded
                    .max(ROTATION_LOCK_THRESHOLD_MIN)
                    .min(max_value);
                rotation_lock_threshold.set(clamped);
            }
        })
    };
    let on_animations_toggle = {
        let animations_enabled = animations_enabled.clone();
        let animating_members = animating_members.clone();
        let rotation_anim = rotation_anim.clone();
        let rotation_anim_handle = rotation_anim_handle.clone();
        let rotation_queue = rotation_queue.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            let enabled = input.checked();
            animations_enabled.set(enabled);
            if !enabled {
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
            }
        })
    };
    let on_emboss_toggle = {
        let emboss_enabled = emboss_enabled.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            emboss_enabled.set(input.checked());
        })
    };
    let on_theme_toggle = {
        let theme_mode = theme_mode.clone();
        Callback::from(move |event: MouseEvent| {
            event.prevent_default();
            let next = match *theme_mode {
                ThemeMode::System => ThemeMode::Light,
                ThemeMode::Light => ThemeMode::Dark,
                ThemeMode::Dark => ThemeMode::System,
            };
            theme_mode.set(next);
        })
    };
    let on_fast_render_toggle = {
        let fast_render = fast_render.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            fast_render.set(input.checked());
        })
    };
    let on_fast_filter_toggle = {
        let fast_filter = fast_filter.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            fast_filter.set(input.checked());
        })
    };
    let on_wgpu_toggle = {
        let wgpu_enabled = wgpu_enabled.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            wgpu_enabled.set(input.checked());
        })
    };
    let on_debug_toggle = {
        let show_debug = show_debug.clone();
        Callback::from(move |event: Event| {
            let input: HtmlInputElement = event.target_unchecked_into();
            show_debug.set(input.checked());
        })
    };

    {
        let theme_toggle_ref = theme_toggle_ref.clone();
        use_effect_with(theme_mode_value, move |mode| {
            if let Some(input) = theme_toggle_ref.cast::<HtmlInputElement>() {
                let (checked, indeterminate) = match *mode {
                    ThemeMode::System => (false, true),
                    ThemeMode::Light => (false, false),
                    ThemeMode::Dark => (true, false),
                };
                input.set_checked(checked);
                input.set_indeterminate(indeterminate);
            }
            || ()
        });
    }

    {
        use_effect_with(theme_mode_value, move |mode| {
            if let Some(window) = web_sys::window() {
                if let Some(document) = window.document() {
                    if let Some(body) = document.body() {
                        let theme_value = match *mode {
                            ThemeMode::System => "system",
                            ThemeMode::Light => "light",
                            ThemeMode::Dark => "dark",
                        };
                        let _ = body.set_attribute("data-theme", theme_value);
                    }
                }
            }
            || ()
        });
    }

    {
        let canvas_ref = canvas_ref.clone();
        let image_element = image_element.clone();
        let wgpu_renderer = wgpu_renderer.clone();
        let mask_atlas = mask_atlas.clone();
        let wgpu_revision = wgpu_revision.clone();
        let hovered_id = hovered_id.clone();
        use_effect_with(
            (
                wgpu_enabled_value,
                image_size_value,
                grid,
                settings_value.clone(),
                workspace_scale_value,
                depth_cap,
                curve_detail,
                theme_mode_value,
            ),
            move |(
                wgpu_enabled_value,
                image_size_value,
                grid,
                settings_value,
                workspace_scale_value,
                depth_cap,
                curve_detail,
                theme_mode_value,
            )| {
                let cleanup: fn() = || ();
                if *wgpu_enabled_value {
                    hovered_id.set(None);
                }
                let build_inputs = if *wgpu_enabled_value {
                    if let (Some((width, height)), Some(image), Some(canvas)) = (
                        *image_size_value,
                        (*image_element).clone(),
                        canvas_ref.cast::<HtmlCanvasElement>(),
                    ) {
                        Some((width, height, image, canvas))
                    } else {
                        None
                    }
                } else {
                    wgpu_renderer.borrow_mut().take();
                    mask_atlas.borrow_mut().take();
                    None
                };

                if let Some((width, height, image, canvas)) = build_inputs {
                    let grid = *grid;
                    let settings_value = settings_value.clone();
                    let width_f = width as f32;
                    let height_f = height as f32;
                    let view_width = width_f * *workspace_scale_value;
                    let view_height = height_f * *workspace_scale_value;
                    let view_min_x = (width_f - view_width) * 0.5;
                    let view_min_y = (height_f - view_height) * 0.5;
                    let piece_width = width_f / grid.cols as f32;
                    let piece_height = height_f / grid.rows as f32;

                    let pieces = build_pieces(grid.rows, grid.cols);
                    let (horizontal, vertical) =
                        build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &settings_value);
                    let (horizontal_waves, vertical_waves) = build_line_waves(
                        grid.rows,
                        grid.cols,
                        PUZZLE_SEED,
                        piece_width,
                        piece_height,
                        settings_value.line_bend_ratio,
                    );
                    let warp_field = WarpField {
                        width: width_f,
                        height: height_f,
                        horizontal: &horizontal_waves,
                        vertical: &vertical_waves,
                    };
                    let mut paths = Vec::with_capacity(pieces.len());
                    for piece in &pieces {
                        paths.push(build_piece_path(
                            piece,
                            piece_width,
                            piece_height,
                            &horizontal,
                            &vertical,
                            &warp_field,
                            *depth_cap,
                            *curve_detail,
                        ));
                    }

                    let max_depth = piece_width.max(piece_height) * *depth_cap;
                    let max_bend = horizontal_waves
                        .iter()
                        .chain(vertical_waves.iter())
                        .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
                    let mask_pad = (max_depth + max_bend).ceil();
                    let mask_atlas_data = match build_mask_atlas(
                        &pieces,
                        &paths,
                        piece_width,
                        piece_height,
                        grid,
                        mask_pad,
                    ) {
                        Ok(atlas) => Rc::new(atlas),
                        Err(err) => {
                            web_sys::console::error_1(&err);
                            return cleanup;
                        }
                    };
                    *mask_atlas.borrow_mut() = Some(mask_atlas_data.clone());
                    wgpu_renderer.borrow_mut().take();
                    let prefers_dark = prefers_dark_mode();
                    let is_dark_theme = match theme_mode_value {
                        ThemeMode::Dark => true,
                        ThemeMode::Light => false,
                        ThemeMode::System => prefers_dark,
                    };
                    spawn_local(async move {
                        match WgpuRenderer::new(
                            canvas,
                            image,
                            pieces,
                            paths,
                            grid,
                            piece_width,
                            piece_height,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            mask_atlas_data,
                            mask_pad,
                            is_dark_theme,
                        )
                        .await
                        {
                            Ok(renderer) => {
                                *wgpu_renderer.borrow_mut() = Some(renderer);
                                wgpu_revision.set(wgpu_revision.wrapping_add(1));
                            }
                            Err(err) => {
                                web_sys::console::error_1(&err);
                            }
                        }
                    });
                }
                cleanup
            },
        );
    }

    {
        let wgpu_renderer = wgpu_renderer.clone();
        let mask_atlas = mask_atlas.clone();
        use_effect_with(
            (
                wgpu_enabled_value,
                wgpu_revision_value,
                image_size_value,
                grid,
                (*positions).clone(),
                (*rotations).clone(),
                (*flips).clone(),
                (*z_order).clone(),
                (*connections).clone(),
                hovered_id_value,
                show_debug_value,
            ),
            move |(
                wgpu_enabled_value,
                _wgpu_revision_value,
                image_size_value,
                grid,
                positions_value,
                rotations_value,
                flips_value,
                z_order_value,
                connections_value,
                hovered_id_value,
                show_debug_value,
            )| {
                let cleanup: fn() = || ();
                if !*wgpu_enabled_value {
                    return cleanup;
                }
                let Some((width, height)) = *image_size_value else {
                    return cleanup;
                };
                let Some(mask_atlas) = mask_atlas.borrow().as_ref().cloned() else {
                    return cleanup;
                };
                let mut renderer_ref = wgpu_renderer.borrow_mut();
                let Some(renderer) = renderer_ref.as_mut() else {
                    return cleanup;
                };

                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return cleanup;
                }
                let piece_width = width as f32 / grid.cols as f32;
                let piece_height = height as f32 / grid.rows as f32;
                let order: Vec<usize> = if z_order_value.len() == total {
                    z_order_value.to_vec()
                } else {
                    (0..total).collect()
                };
                let mut hovered_mask = vec![false; total];
                if *show_debug_value {
                    hovered_mask.fill(true);
                } else if let Some(id) = *hovered_id_value {
                    if id < total && id < connections_value.len() {
                        for member in collect_group(&connections_value, id, cols, rows) {
                            if member < hovered_mask.len() {
                                hovered_mask[member] = true;
                            }
                        }
                    }
                }
                let mut instances = Vec::with_capacity(order.len());
                for id in order {
                    let col = id % cols;
                    let row = id / cols;
                    let base_x = col as f32 * piece_width;
                    let base_y = row as f32 * piece_height;
                    let pos = positions_value.get(id).copied().unwrap_or((base_x, base_y));
                    let rotation = rotations_value.get(id).copied().unwrap_or(0.0);
                    let flipped = flips_value.get(id).copied().unwrap_or(false);
                    let hovered = hovered_mask.get(id).copied().unwrap_or(false);
                    let mask_origin = mask_atlas.origins.get(id).copied().unwrap_or([0.0, 0.0]);
                    instances.push(Instance {
                        pos: [pos.0, pos.1],
                        size: [piece_width, piece_height],
                        rotation,
                        flip: if flipped { 1.0 } else { 0.0 },
                        hover: if *show_debug_value {
                            2.0
                        } else if hovered {
                            1.0
                        } else {
                            0.0
                        },
                        _pad: 0.0,
                        piece_origin: [base_x, base_y],
                        mask_origin,
                    });
                }
                renderer.update_instances(&instances);
                renderer.render();
                cleanup
            },
        );
    }

    {
        let positions = positions.clone();
        let rotations = rotations.clone();
        let flips = flips.clone();
        let connections = connections.clone();
        let z_order = z_order.clone();
        let image_size = image_size.clone();
        let grid_index = grid_index.clone();
        let grid_choices = grid_choices.clone();
        let scramble_nonce = scramble_nonce.clone();
        let active_id = active_id.clone();
        let animating_members = animating_members.clone();
        use_effect_with(save_revision_value, move |save_revision_value| {
            let should_save =
                *save_revision_value > 0 && active_id.is_none() && animating_members.is_empty();
            if should_save {
                if let Some((width, height)) = *image_size {
                    let grid = grid_choices
                        .get(*grid_index)
                        .copied()
                        .unwrap_or(FALLBACK_GRID);
                    let total = (grid.cols * grid.rows) as usize;
                    let positions_value = (*positions).clone();
                    let rotations_value = (*rotations).clone();
                    let flips_value = (*flips).clone();
                    let connections_value = (*connections).clone();
                    let z_order_value = (*z_order).clone();
                    if positions_value.len() == total
                        && rotations_value.len() == total
                        && flips_value.len() == total
                        && connections_value.len() == total
                        && z_order_value.len() == total
                        && z_order_value.iter().all(|id| *id < total)
                    {
                        let state = SavedBoard {
                            version: STORAGE_VERSION,
                            cols: grid.cols,
                            rows: grid.rows,
                            image_width: width,
                            image_height: height,
                            positions: positions_value,
                            rotations: rotations_value,
                            flips: flips_value,
                            connections: connections_value,
                            z_order: z_order_value,
                            scramble_nonce: *scramble_nonce,
                        };
                        save_board_state(&state);
                    }
                }
            }
            || ()
        });
    }

    {
        let show_controls = show_controls.clone();
        use_effect_with(
            show_controls_value,
            move |show_controls_value| {
                let current = *show_controls_value;
                let window = web_sys::window().expect("window available");
                let options = EventListenerOptions {
                    phase: EventListenerPhase::Capture,
                    passive: false,
                };
                let listener = EventListener::new_with_options(
                    &window,
                    "keydown",
                    options,
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<KeyboardEvent>() {
                            if event.repeat() {
                                return;
                            }
                            let key = event.key();
                            let code = event.code();
                            let toggle = matches!(key.as_str(), "?" | "d" | "D")
                                || matches!(code.as_str(), "KeyD" | "Slash");
                            if toggle {
                                let next = !current;
                                gloo::console::log!(
                                    "controls",
                                    format!("{} -> {}", current, next),
                                    key,
                                    code
                                );
                                show_controls.set(next);
                                event.prevent_default();
                            }
                        }
                    },
                );
                || drop(listener)
            },
        );
    }

    {
        let drag_handlers = drag_handlers.clone();
        use_effect_with(
            (),
            move |_| {
                let window = web_sys::window().expect("window available");
                let move_handlers = drag_handlers.clone();
                let move_listener = EventListener::new_with_options(
                    &window,
                    "mousemove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = move_handlers.borrow().on_move.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_move_handlers = drag_handlers.clone();
                let touch_move_listener = EventListener::new_with_options(
                    &window,
                    "touchmove",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) = touch_move_handlers.borrow().on_touch_move.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let up_handlers = drag_handlers.clone();
                let up_listener = EventListener::new_with_options(
                    &window,
                    "mouseup",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<MouseEvent>() {
                            if let Some(handler) = up_handlers.borrow().on_release.as_ref() {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_end_handlers = drag_handlers.clone();
                let touch_end_listener = EventListener::new_with_options(
                    &window,
                    "touchend",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_end_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                let touch_cancel_handlers = drag_handlers.clone();
                let touch_cancel_listener = EventListener::new_with_options(
                    &window,
                    "touchcancel",
                    EventListenerOptions {
                        phase: EventListenerPhase::Capture,
                        passive: false,
                    },
                    move |event: &Event| {
                        if let Some(event) = event.dyn_ref::<TouchEvent>() {
                            if let Some(handler) =
                                touch_cancel_handlers.borrow().on_touch_release.as_ref()
                            {
                                handler(event);
                            }
                        }
                    },
                );
                || {
                    drop(move_listener);
                    drop(touch_move_listener);
                    drop(up_listener);
                    drop(touch_end_listener);
                    drop(touch_cancel_listener);
                }
            },
        );
    }

    {
        let image_size = image_size.clone();
        let image_element = image_element.clone();
        use_effect_with(
            (),
            move |_| {
                let img = HtmlImageElement::new().expect("create image element");
                let img_clone = img.clone();
                let onload = Closure::<dyn FnMut()>::wrap(Box::new(move || {
                    let width = img_clone.natural_width();
                    let height = img_clone.natural_height();
                    image_size.set(Some((width, height)));
                    image_element.set(Some(img_clone.clone()));
                }));
                img.set_onload(Some(onload.as_ref().unchecked_ref()));
                img.set_src(IMAGE_SRC);
                onload.forget();
                || ()
            },
        );
    }

    let (content, on_scramble, on_solve, on_solve_rotation, on_unflip, scramble_disabled) =
        if let Some((width, height)) = *image_size {
        let width_f = width as f32;
        let height_f = height as f32;
        let view_width = width_f * workspace_scale_value;
        let view_height = height_f * workspace_scale_value;
        let view_min_x = (width_f - view_width) * 0.5;
        let view_min_y = (height_f - view_height) * 0.5;
        let view_box = format!(
            "{} {} {} {}",
            fmt_f32(view_min_x),
            fmt_f32(view_min_y),
            fmt_f32(view_width),
            fmt_f32(view_height)
        );
        let piece_width = width_f / grid.cols as f32;
        let piece_height = height_f / grid.rows as f32;
        let max_depth = piece_width.max(piece_height) * depth_cap;
        let pieces = build_pieces(grid.rows, grid.cols);

        let (horizontal, vertical) =
            build_edge_maps(grid.rows, grid.cols, PUZZLE_SEED, &settings_value);
        let (horizontal_waves, vertical_waves) = build_line_waves(
            grid.rows,
            grid.cols,
            PUZZLE_SEED,
            piece_width,
            piece_height,
            settings_value.line_bend_ratio,
        );
        let warp_field = WarpField {
            width: width_f,
            height: height_f,
            horizontal: &horizontal_waves,
            vertical: &vertical_waves,
        };
        let max_bend = horizontal_waves
            .iter()
            .chain(vertical_waves.iter())
            .fold(0.0_f32, |acc, wave| acc.max(wave.amplitude.abs()));
        let mask_pad = (max_depth + max_bend).ceil();
        let base_w = piece_width + mask_pad * 2.0;
        let base_h = piece_height + mask_pad * 2.0;
        let base_diag = (base_w * base_w + base_h * base_h).sqrt();
        let rotation_pad = (base_diag - base_w.max(base_h)) * 0.5;
        let emboss_pad = EMBOSS_OFFSET.abs() + EMBOSS_RIM + rotation_pad;
        let mut frame_corner_radius_value = piece_width.min(piece_height) * CORNER_RADIUS_RATIO;
        let max_corner_radius = piece_width.min(piece_height) * 0.45;
        if frame_corner_radius_value > max_corner_radius {
            frame_corner_radius_value = max_corner_radius;
        }
        let frame_corner_radius = fmt_f32(frame_corner_radius_value);
        let mask_x = fmt_f32(-mask_pad);
        let mask_y = fmt_f32(-mask_pad);
        let mask_width = fmt_f32(piece_width + mask_pad * 2.0);
        let mask_height = fmt_f32(piece_height + mask_pad * 2.0);
        let emboss_x = fmt_f32(-mask_pad - emboss_pad);
        let emboss_y = fmt_f32(-mask_pad - emboss_pad);
        let emboss_width = fmt_f32(piece_width + (mask_pad + emboss_pad) * 2.0);
        let emboss_height = fmt_f32(piece_height + (mask_pad + emboss_pad) * 2.0);
        let emboss_offset_neg = fmt_f32(-EMBOSS_OFFSET);
        let emboss_rim_radius = fmt_f32(EMBOSS_RIM);
        let center_min_x = view_min_x + piece_width * 0.5;
        let center_min_y = view_min_y + piece_height * 0.5;
        let mut center_max_x = view_min_x + view_width - piece_width * 0.5;
        let mut center_max_y = view_min_y + view_height - piece_height * 0.5;
        if center_max_x < center_min_x {
            center_max_x = center_min_x;
        }
        if center_max_y < center_min_y {
            center_max_y = center_min_y;
        }
        let piece_shapes: Vec<(Piece, PiecePaths)> = pieces
            .iter()
            .map(|piece| {
                let paths = build_piece_path(
                    piece,
                    piece_width,
                    piece_height,
                    &horizontal,
                    &vertical,
                    &warp_field,
                    depth_cap,
                    curve_detail,
                );
                (*piece, paths)
            })
            .collect();

        let positions_value = (*positions).clone();
        let rotations_value = (*rotations).clone();
        let flips_value = (*flips).clone();
        let connections_value = (*connections).clone();
        let hovered_group = if let Some(id) = hovered_id_value {
            if id < connections_value.len() {
                collect_group(&connections_value, id, grid.cols as usize, grid.rows as usize)
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        let mut hovered_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &hovered_group {
            if *id < hovered_mask.len() {
                hovered_mask[*id] = true;
            }
        }
        let mut dragging_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &dragging_members_value {
            if *id < dragging_mask.len() {
                dragging_mask[*id] = true;
            }
        }
        let mut animating_mask = vec![false; (grid.cols * grid.rows) as usize];
        for id in &animating_members_value {
            if *id < animating_mask.len() {
                animating_mask[*id] = true;
            }
        }
        let z_order_value = (*z_order).clone();
        let drag_move_common: Rc<dyn Fn(f32, f32) -> bool> = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let drag_state = drag_state.clone();
            Rc::new(move |x: f32, y: f32| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    if drag.rotate_mode && rotation_enabled_value {
                        let current_angle = (y - drag.pivot_y).atan2(x - drag.pivot_x);
                        let delta_deg = (current_angle - drag.start_angle).to_degrees();
                        let mut next = (*positions).clone();
                        let mut next_rotations = (*rotations).clone();
                        let flips_snapshot = &*flips;
                        for (index, member) in drag.members.iter().enumerate() {
                            if let Some(start) = drag.start_positions.get(index) {
                                let center_x = start.0 + piece_width * 0.5;
                                let center_y = start.1 + piece_height * 0.5;
                                let (rx, ry) = rotate_point(
                                    center_x,
                                    center_y,
                                    drag.pivot_x,
                                    drag.pivot_y,
                                    delta_deg,
                                );
                                if let Some(pos) = next.get_mut(*member) {
                                    *pos = (
                                        rx - piece_width * 0.5,
                                        ry - piece_height * 0.5,
                                    );
                                }
                                if let Some(rot) = next_rotations.get_mut(*member) {
                                    let start_rot =
                                        drag.start_rotations.get(index).copied().unwrap_or(0.0);
                                    let flipped = flips_snapshot.get(*member).copied().unwrap_or(false);
                                    let signed_delta = if flipped { -delta_deg } else { delta_deg };
                                    *rot = normalize_angle(start_rot + signed_delta);
                                }
                            }
                        }
                        positions.set(next);
                        rotations.set(next_rotations);
                        true
                    } else {
                        let mut dx = x - drag.start_x;
                        let mut dy = y - drag.start_y;
                        if !drag.start_positions.is_empty() {
                            let mut min_start_x = f32::INFINITY;
                            let mut max_start_x = f32::NEG_INFINITY;
                            let mut min_start_y = f32::INFINITY;
                            let mut max_start_y = f32::NEG_INFINITY;
                            for start in &drag.start_positions {
                                let center_x = start.0 + piece_width * 0.5;
                                let center_y = start.1 + piece_height * 0.5;
                                min_start_x = min_start_x.min(center_x);
                                max_start_x = max_start_x.max(center_x);
                                min_start_y = min_start_y.min(center_y);
                                max_start_y = max_start_y.max(center_y);
                            }
                            let min_dx = center_min_x - min_start_x;
                            let max_dx = center_max_x - max_start_x;
                            let min_dy = center_min_y - min_start_y;
                            let max_dy = center_max_y - max_start_y;
                            let rubber_limit = piece_width.min(piece_height) * RUBBER_BAND_RATIO;
                            if min_dx <= max_dx {
                                dx = rubber_band_clamp(dx, min_dx, max_dx, rubber_limit);
                            }
                            if min_dy <= max_dy {
                                dy = rubber_band_clamp(dy, min_dy, max_dy, rubber_limit);
                            }
                        }
                        let mut next = (*positions).clone();
                        for (index, member) in drag.members.iter().enumerate() {
                            if let Some(pos) = next.get_mut(*member) {
                                let start = drag.start_positions[index];
                                *pos = (start.0 + dx, start.1 + dy);
                            }
                        }
                        positions.set(next);
                        true
                    }
                } else {
                    false
                }
            })
        };
        let schedule_drag_move: Rc<dyn Fn(f32, f32) -> bool> = {
            let drag_move_common = drag_move_common.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            Rc::new(move |x, y| {
                *drag_pending.borrow_mut() = Some((x, y));
                if drag_frame.borrow().is_some() {
                    return true;
                }
                let drag_pending = drag_pending.clone();
                let drag_frame_for_tick = drag_frame.clone();
                let drag_move_common = drag_move_common.clone();
                let handle = request_animation_frame(move |_| {
                    let coords = drag_pending.borrow_mut().take();
                    drag_frame_for_tick.borrow_mut().take();
                    if let Some((x, y)) = coords {
                        drag_move_common(x, y);
                    }
                });
                *drag_frame.borrow_mut() = Some(handle);
                true
            })
        };
        let drag_move = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let wgpu_enabled_value = wgpu_enabled_value;
            let schedule_drag_move = schedule_drag_move.clone();
            move |event: &MouseEvent| {
                let target_ref = if wgpu_enabled_value {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                if let Some((x, y)) = event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) {
                    if schedule_drag_move(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_move_touch = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let wgpu_enabled_value = wgpu_enabled_value;
            let drag_state = drag_state.clone();
            let active_id = active_id.clone();
            let dragging_members = dragging_members.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            let schedule_drag_move = schedule_drag_move.clone();
            move |event: &TouchEvent| {
                if event.touches().length() > 1 {
                    if drag_state.borrow().is_some() {
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                        drag_pending.borrow_mut().take();
                        drag_frame.borrow_mut().take();
                    }
                    return;
                }
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                let target_ref = if wgpu_enabled_value {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                if let Some((x, y)) = touch_event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    false,
                ) {
                    if schedule_drag_move(x, y) {
                        event.prevent_default();
                    }
                }
            }
        };
        let drag_release_common: Rc<dyn Fn(Option<(f32, f32)>) -> bool> = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let drag_pending = drag_pending.clone();
            let drag_frame = drag_frame.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Rc::new(move |coords: Option<(f32, f32)>| {
                let drag = drag_state.borrow().clone();
                if let Some(drag) = drag {
                    let ctrl_flip = drag.rotate_mode;
                    let mut next = (*positions).clone();
                    let mut next_rotations = (*rotations).clone();
                    let mut next_flips = (*flips).clone();
                    let mut next_connections = (*connections).clone();
                    let start_positions_all = next.clone();
                    let start_rotations_all = next_rotations.clone();
                    let cols = grid.cols as usize;
                    let rows = grid.rows as usize;
                    let snap_distance = piece_width.min(piece_height) * snap_distance_ratio_value;
                    let click_tolerance = piece_width.min(piece_height) * CLICK_MOVE_RATIO;
                    let start_group_animation = {
                        let positions = positions.clone();
                        let rotations = rotations.clone();
                        let flips = flips.clone();
                        let connections = connections.clone();
                        let solved = solved.clone();
                        let save_revision = save_revision.clone();
                        let rotation_anim = rotation_anim.clone();
                        let rotation_anim_handle = rotation_anim_handle.clone();
                        let animating_members = animating_members.clone();
                        let rotation_queue = rotation_queue.clone();
                        let rotation_enabled_value = rotation_enabled_value;
                        let rotation_noise_value = rotation_noise_value;
                        let animations_enabled_value = animations_enabled_value;
                        let rotation_snap_tolerance_value = rotation_snap_tolerance_value;
                        let cols = cols;
                        let rows = rows;
                        let piece_width = piece_width;
                        let piece_height = piece_height;
                        let frame_snap_ratio_value = frame_snap_ratio_value;
                        let snap_distance_ratio_value = snap_distance_ratio_value;
                        let center_min_x = center_min_x;
                        let center_max_x = center_max_x;
                        let center_min_y = center_min_y;
                        let center_max_y = center_max_y;
                        let view_min_x = view_min_x;
                        let view_min_y = view_min_y;
                        let view_width = view_width;
                        let view_height = view_height;
                        move |anim: RotationAnimation,
                              connections_override: Option<Vec<[bool; 4]>>| {
                            let connections_override = connections_override.map(Rc::new);
                            if !animations_enabled_value {
                                rotation_queue.borrow_mut().clear();
                                let mut next_positions = (*positions).clone();
                                let mut next_rotations = (*rotations).clone();
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) =
                                                anim.start_positions.get(index)
                                            {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    *delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + *delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        target_center,
                                        target_rot,
                                        ..
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let rot = normalize_angle(*target_rot);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                *target_center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                let should_snap = matches!(&anim.kind, AnimationKind::Pivot { .. });
                                let mut connections_snapshot = connections_override
                                    .as_ref()
                                    .map(|snapshot| (**snapshot).clone())
                                    .unwrap_or_else(|| (*connections).clone());
                                let flips_snapshot = (*flips).clone();
                                if should_snap {
                                    let snap_distance = piece_width.min(piece_height)
                                        * snap_distance_ratio_value;
                                    apply_snaps_for_group(
                                        &anim.members,
                                        &mut next_positions,
                                        &mut next_rotations,
                                        &flips_snapshot,
                                        &mut connections_snapshot,
                                        cols,
                                        rows,
                                        piece_width,
                                        piece_height,
                                        snap_distance,
                                        frame_snap_ratio_value,
                                        center_min_x,
                                        center_max_x,
                                        center_min_y,
                                        center_max_y,
                                        view_min_x,
                                        view_min_y,
                                        view_width,
                                        view_height,
                                        rotation_snap_tolerance_value,
                                        rotation_enabled_value,
                                    );
                                }
                                positions.set(next_positions.clone());
                                rotations.set(next_rotations.clone());
                                if should_snap || connections_override.is_some() {
                                    connections.set(connections_snapshot.clone());
                                }
                                animating_members.set(Vec::new());
                                *rotation_anim.borrow_mut() = None;
                                rotation_anim_handle.borrow_mut().take();
                                let solved_now = is_solved(
                                    &next_positions,
                                    &next_rotations,
                                    &flips_snapshot,
                                    &connections_snapshot,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                return;
                            }
                            let members = anim.members.clone();
                            *rotation_anim.borrow_mut() = Some(anim);
                            animating_members.set(members);
                            rotation_anim_handle.borrow_mut().take();
                            let positions = positions.clone();
                            let rotations = rotations.clone();
                            let flips = flips.clone();
                            let connections = connections.clone();
                            let solved = solved.clone();
                            let rotation_anim = rotation_anim.clone();
                            let rotation_anim_handle = rotation_anim_handle.clone();
                            let animating_members = animating_members.clone();
                            let rotation_anim_handle_for_tick = rotation_anim_handle.clone();
                            let rotation_queue = rotation_queue.clone();
                            let connections_override = connections_override.clone();
                            let interval = Interval::new(16, move || {
                                let now = Date::now() as f32;
                                let anim = match rotation_anim.borrow().clone() {
                                    Some(value) => value,
                                    None => {
                                        rotation_anim_handle_for_tick.borrow_mut().take();
                                        animating_members.set(Vec::new());
                                        return;
                                    }
                                };
                                let mut t = (now - anim.start_time) / anim.duration;
                                if t < 0.0 {
                                    t = 0.0;
                                } else if t > 1.0 {
                                    t = 1.0;
                                }
                                let eased = t * t * (3.0 - 2.0 * t);
                                let mut next_positions = (*positions).clone();
                                let mut next_rotations = (*rotations).clone();
                                match &anim.kind {
                                    AnimationKind::Pivot {
                                        pivot_x,
                                        pivot_y,
                                        delta,
                                    } => {
                                        let current_delta = *delta * eased;
                                        for (index, member) in anim.members.iter().enumerate() {
                                            if let Some(start) = anim.start_positions.get(index) {
                                                let center_x = start.0 + piece_width * 0.5;
                                                let center_y = start.1 + piece_height * 0.5;
                                                let (rx, ry) = rotate_point(
                                                    center_x,
                                                    center_y,
                                                    *pivot_x,
                                                    *pivot_y,
                                                    current_delta,
                                                );
                                                if let Some(pos) =
                                                    next_positions.get_mut(*member)
                                                {
                                                    *pos = (
                                                        rx - piece_width * 0.5,
                                                        ry - piece_height * 0.5,
                                                    );
                                                }
                                                if let Some(rot) =
                                                    next_rotations.get_mut(*member)
                                                {
                                                    let base = anim
                                                        .start_rotations
                                                        .get(index)
                                                        .copied()
                                                        .unwrap_or(0.0);
                                                    *rot = normalize_angle(base + current_delta);
                                                }
                                            }
                                        }
                                    }
                                    AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    } => {
                                        let anchor_row = (*anchor_id / cols) as i32;
                                        let anchor_col = (*anchor_id % cols) as i32;
                                        let center = (
                                            start_center.0
                                                + (target_center.0 - start_center.0) * eased,
                                            start_center.1
                                                + (target_center.1 - start_center.1) * eased,
                                        );
                                        let delta = angle_delta(*target_rot, *start_rot);
                                        let rot = normalize_angle(*start_rot + delta * eased);
                                        for member in &anim.members {
                                            let aligned = aligned_center_from_anchor(
                                                anchor_row,
                                                anchor_col,
                                                center,
                                                *member,
                                                cols,
                                                piece_width,
                                                piece_height,
                                                rot,
                                            );
                                            if let Some(pos) = next_positions.get_mut(*member) {
                                                *pos = (
                                                    aligned.0 - piece_width * 0.5,
                                                    aligned.1 - piece_height * 0.5,
                                                );
                                            }
                                            if let Some(rot_ref) =
                                                next_rotations.get_mut(*member)
                                            {
                                                *rot_ref = rot;
                                            }
                                        }
                                    }
                                }
                                positions.set(next_positions.clone());
                                rotations.set(next_rotations.clone());
                                if t >= 1.0 {
                                    let queued = rotation_queue.borrow_mut().pop_front();
                                    if let Some(next_step) = queued {
                                        let members = next_step.members.clone();
                                        let mut start_positions =
                                            Vec::with_capacity(members.len());
                                        let mut start_rotations =
                                            Vec::with_capacity(members.len());
                                        for member in &members {
                                            if let Some(pos) =
                                                next_positions.get(*member)
                                            {
                                                start_positions.push(*pos);
                                            } else {
                                                start_positions.push((0.0, 0.0));
                                            }
                                            let rot = next_rotations
                                                .get(*member)
                                                .copied()
                                                .unwrap_or(0.0);
                                            start_rotations.push(rot);
                                        }
                                        let current_angle = members
                                            .first()
                                            .and_then(|id| next_rotations.get(*id))
                                            .copied()
                                            .unwrap_or(0.0);
                                        let delta = click_rotation_delta(
                                            current_angle,
                                            next_step.noise,
                                            rotation_noise_value,
                                            rotation_snap_tolerance_value,
                                        );
                                        *rotation_anim.borrow_mut() = Some(RotationAnimation {
                                            start_time: Date::now() as f32,
                                            duration: SNAP_ANIMATION_MS,
                                            members: members.clone(),
                                            start_positions,
                                            start_rotations,
                                            kind: AnimationKind::Pivot {
                                                pivot_x: next_step.pivot_x,
                                                pivot_y: next_step.pivot_y,
                                                delta,
                                            },
                                        });
                                        animating_members.set(members);
                                        return;
                                    }
                                    *rotation_anim.borrow_mut() = None;
                                    animating_members.set(Vec::new());
                                    let should_snap =
                                        matches!(&anim.kind, AnimationKind::Pivot { .. });
                                    let mut snapped_positions = next_positions.clone();
                                    let mut snapped_rotations = next_rotations.clone();
                                    let mut connections_snapshot = connections_override
                                        .as_ref()
                                        .map(|snapshot| (**snapshot).clone())
                                        .unwrap_or_else(|| (*connections).clone());
                                    let flips_snapshot = (*flips).clone();
                                    if should_snap {
                                        let snap_distance = piece_width.min(piece_height)
                                            * snap_distance_ratio_value;
                                        apply_snaps_for_group(
                                            &anim.members,
                                            &mut snapped_positions,
                                            &mut snapped_rotations,
                                            &flips_snapshot,
                                            &mut connections_snapshot,
                                            cols,
                                            rows,
                                            piece_width,
                                            piece_height,
                                            snap_distance,
                                            frame_snap_ratio_value,
                                            center_min_x,
                                            center_max_x,
                                            center_min_y,
                                            center_max_y,
                                            view_min_x,
                                            view_min_y,
                                            view_width,
                                            view_height,
                                            rotation_snap_tolerance_value,
                                            rotation_enabled_value,
                                        );
                                    }
                                    positions.set(snapped_positions.clone());
                                    rotations.set(snapped_rotations.clone());
                                    if should_snap || connections_override.is_some() {
                                        connections.set(connections_snapshot.clone());
                                    }
                                    let solved_now = is_solved(
                                        &snapped_positions,
                                        &snapped_rotations,
                                        &flips_snapshot,
                                        &connections_snapshot,
                                        cols,
                                        rows,
                                        piece_width,
                                        piece_height,
                                        rotation_enabled_value,
                                    );
                                    solved.set(solved_now);
                                    save_revision.set(save_revision.wrapping_add(1));
                                    rotation_anim_handle_for_tick.borrow_mut().take();
                                }
                            });
                            *rotation_anim_handle.borrow_mut() = Some(interval);
                        }
                    };
                    if let Some((x, y)) = coords {
                        let dx = x - drag.start_x;
                        let dy = y - drag.start_y;
                        let dist = (dx * dx + dy * dy).sqrt();
                        let elapsed = Date::now() as f32 - drag.start_time;
                        if dist <= click_tolerance && elapsed <= CLICK_MAX_DURATION_MS {
                            let click_id = drag.primary_id;
                            let was_flipped = next_flips.get(click_id).copied().unwrap_or(false);
                            if ctrl_flip {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = !*flip;
                                }
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
                                let solved_now = is_solved(
                                    &next,
                                    &next_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                positions.set(next);
                                connections.set(next_connections);
                                rotations.set(next_rotations);
                                flips.set(next_flips);
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                            if was_flipped {
                                if let Some(flip) = next_flips.get_mut(click_id) {
                                    *flip = false;
                                }
                                clear_piece_connections(&mut next_connections, click_id, cols, rows);
                                let solved_now = is_solved(
                                    &next,
                                    &next_rotations,
                                    &next_flips,
                                    &next_connections,
                                    cols,
                                    rows,
                                    piece_width,
                                    piece_height,
                                    rotation_enabled_value,
                                );
                                positions.set(next);
                                connections.set(next_connections);
                                rotations.set(next_rotations);
                                flips.set(next_flips);
                                solved.set(solved_now);
                                save_revision.set(save_revision.wrapping_add(1));
                                active_id.set(None);
                                dragging_members.set(Vec::new());
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                            if rotation_enabled_value && !drag.members.is_empty() {
                                let pivot_x = drag.start_x;
                                let pivot_y = drag.start_y;
                                let members = drag.members.clone();
                                let mut noise = 0.0;
                                if rotation_noise_value > 0.0 {
                                    let noise_seed = splitmix32(
                                        (drag.start_time as u32)
                                            ^ (drag.primary_id as u32)
                                                .wrapping_mul(0x9E37_79B9),
                                    );
                                    noise = rand_range(
                                        noise_seed,
                                        members.len() as u32,
                                        -rotation_noise_value,
                                        rotation_noise_value,
                                    );
                                }
                                if animations_enabled_value {
                                    if let Some(active_anim) = rotation_anim.borrow().clone() {
                                        if active_anim.members.contains(&click_id) {
                                            rotation_queue
                                                .borrow_mut()
                                                .push_back(QueuedRotation {
                                                    members: active_anim.members,
                                                    pivot_x,
                                                    pivot_y,
                                                    noise,
                                                });
                                            dragging_members.set(Vec::new());
                                            active_id.set(None);
                                            *drag_state.borrow_mut() = None;
                                            drag_pending.borrow_mut().take();
                                            drag_frame.borrow_mut().take();
                                            return true;
                                        }
                                    }
                                }
                                let current_angle = members
                                    .first()
                                    .and_then(|id| next_rotations.get(*id))
                                    .copied()
                                    .unwrap_or(0.0);
                                let group_size = members.len();
                                let total = cols * rows;
                                let rotation_locked =
                                    group_size == total || group_size > rotation_lock_threshold_value;
                                if group_size > 1
                                    && rotation_locked
                                    && angle_matches(
                                        current_angle,
                                        0.0,
                                        rotation_snap_tolerance_value,
                                    )
                                {
                                    active_id.set(None);
                                    dragging_members.set(Vec::new());
                                    *drag_state.borrow_mut() = None;
                                    drag_pending.borrow_mut().take();
                                    drag_frame.borrow_mut().take();
                                    return true;
                                }
                                let delta = click_rotation_delta(
                                    current_angle,
                                    noise,
                                    rotation_noise_value,
                                    rotation_snap_tolerance_value,
                                );
                                let mut start_positions = Vec::with_capacity(members.len());
                                let mut start_rotations = Vec::with_capacity(members.len());
                                for member in &members {
                                    if let Some(pos) = next.get(*member) {
                                        start_positions.push(*pos);
                                    } else {
                                        start_positions.push((0.0, 0.0));
                                    }
                                    let rot = next_rotations.get(*member).copied().unwrap_or(0.0);
                                    start_rotations.push(rot);
                                }
                                rotation_queue.borrow_mut().clear();
                                start_group_animation(
                                    RotationAnimation {
                                        start_time: Date::now() as f32,
                                        duration: SNAP_ANIMATION_MS,
                                        members: members.clone(),
                                        start_positions,
                                        start_rotations,
                                        kind: AnimationKind::Pivot {
                                            pivot_x,
                                            pivot_y,
                                            delta,
                                        },
                                    },
                                    None,
                                );
                                dragging_members.set(Vec::new());
                                active_id.set(None);
                                *drag_state.borrow_mut() = None;
                                drag_pending.borrow_mut().take();
                                drag_frame.borrow_mut().take();
                                return true;
                            }
                        }
                    }
                    let group_after = apply_snaps_for_group(
                        &drag.members,
                        &mut next,
                        &mut next_rotations,
                        &next_flips,
                        &mut next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        snap_distance,
                        frame_snap_ratio_value,
                        center_min_x,
                        center_max_x,
                        center_min_y,
                        center_max_y,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                        rotation_snap_tolerance_value,
                        rotation_enabled_value,
                    );

                    let mut pending_animation = None;
                    if rotation_enabled_value && !group_after.is_empty() {
                        let anchor_id = if group_after.contains(&drag.primary_id) {
                            drag.primary_id
                        } else {
                            group_after[0]
                        };
                        if anchor_id < next.len() {
                            let start_rot = start_rotations_all
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(0.0);
                            let target_rot = next_rotations
                                .get(anchor_id)
                                .copied()
                                .unwrap_or(start_rot);
                            if angle_delta(target_rot, start_rot).abs() > 0.01 {
                                let start_pos = start_positions_all
                                    .get(anchor_id)
                                    .copied()
                                    .unwrap_or(next[anchor_id]);
                                let target_pos = next[anchor_id];
                                let start_center = (
                                    start_pos.0 + piece_width * 0.5,
                                    start_pos.1 + piece_height * 0.5,
                                );
                                let target_center = (
                                    target_pos.0 + piece_width * 0.5,
                                    target_pos.1 + piece_height * 0.5,
                                );
                                let mut member_positions = Vec::with_capacity(group_after.len());
                                let mut member_rotations = Vec::with_capacity(group_after.len());
                                for member in &group_after {
                                    if let Some(pos) = start_positions_all.get(*member) {
                                        member_positions.push(*pos);
                                    } else {
                                        member_positions.push((0.0, 0.0));
                                    }
                                    let rot = start_rotations_all
                                        .get(*member)
                                        .copied()
                                        .unwrap_or(0.0);
                                    member_rotations.push(rot);
                                }
                                pending_animation = Some(RotationAnimation {
                                    start_time: Date::now() as f32,
                                    duration: SNAP_ANIMATION_MS,
                                    members: group_after.clone(),
                                    start_positions: member_positions,
                                    start_rotations: member_rotations,
                                    kind: AnimationKind::Anchor {
                                        anchor_id,
                                        start_center,
                                        target_center,
                                        start_rot,
                                        target_rot,
                                    },
                                });
                            }
                        }
                    }
                    if let Some(anim) = pending_animation {
                        let connections_snapshot = next_connections.clone();
                        connections.set(next_connections);
                        flips.set(next_flips);
                        start_group_animation(anim, Some(connections_snapshot));
                        active_id.set(None);
                        dragging_members.set(Vec::new());
                        *drag_state.borrow_mut() = None;
                        drag_pending.borrow_mut().take();
                        drag_frame.borrow_mut().take();
                        return true;
                    }

                    let solved_now = is_solved(
                        &next,
                        &next_rotations,
                        &next_flips,
                        &next_connections,
                        cols,
                        rows,
                        piece_width,
                        piece_height,
                        rotation_enabled_value,
                    );
                    positions.set(next);
                    connections.set(next_connections);
                    rotations.set(next_rotations);
                    flips.set(next_flips);
                    solved.set(solved_now);
                    save_revision.set(save_revision.wrapping_add(1));
                    active_id.set(None);
                    dragging_members.set(Vec::new());
                    *drag_state.borrow_mut() = None;
                    drag_pending.borrow_mut().take();
                    drag_frame.borrow_mut().take();
                    return true;
                }
                false
            })
        };
        let drag_release = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let wgpu_enabled_value = wgpu_enabled_value;
            let drag_release_common = drag_release_common.clone();
            move |event: &MouseEvent| {
                let target_ref = if wgpu_enabled_value {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                let coords = event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_release_touch = {
            let svg_ref = svg_ref.clone();
            let canvas_ref = canvas_ref.clone();
            let wgpu_enabled_value = wgpu_enabled_value;
            let drag_state = drag_state.clone();
            let drag_release_common = drag_release_common.clone();
            move |event: &TouchEvent| {
                let touch_id = drag_state
                    .borrow()
                    .as_ref()
                    .and_then(|drag| drag.touch_id);
                let target_ref = if wgpu_enabled_value {
                    &canvas_ref
                } else {
                    &svg_ref
                };
                let coords = touch_event_to_svg_coords(
                    event,
                    target_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    true,
                );
                if drag_release_common(coords) {
                    event.prevent_default();
                }
            }
        };
        let drag_move = Rc::new(drag_move);
        let drag_move_touch = Rc::new(drag_move_touch);
        let drag_release = Rc::new(drag_release);
        let drag_release_touch = Rc::new(drag_release_touch);
        {
            let mut handlers = drag_handlers.borrow_mut();
            handlers.on_move = Some(drag_move.clone());
            handlers.on_release = Some(drag_release.clone());
            handlers.on_touch_move = Some(drag_move_touch.clone());
            handlers.on_touch_release = Some(drag_release_touch.clone());
        }

        let on_scramble = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let scramble_nonce = scramble_nonce.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let next_nonce = time_nonce(*scramble_nonce);
                scramble_nonce.set(next_nonce);
                let seed = scramble_seed(PUZZLE_SEED, next_nonce, cols, rows);
                let rotation_seed = splitmix32(seed ^ 0xC0DE_F00D);
                let flip_seed = splitmix32(seed ^ 0xF11F_5EED);
                let (next_positions, order) = scramble_layout(
                    seed,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    mask_pad,
                );
                positions.set(next_positions);
                connections.set(vec![[false; 4]; total]);
                z_order.set(order);
                rotations.set(scramble_rotations(
                    rotation_seed,
                    total,
                    rotation_enabled_value,
                ));
                flips.set(scramble_flips(flip_seed, total, FLIP_CHANCE));
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(false);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_solve = {
            let positions = positions.clone();
            let connections = connections.clone();
            let z_order = z_order.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let active_id = active_id.clone();
            let drag_state = drag_state.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let mut next_positions = Vec::with_capacity(total);
                for row in 0..rows {
                    for col in 0..cols {
                        next_positions.push((
                            col as f32 * piece_width,
                            row as f32 * piece_height,
                        ));
                    }
                }
                let order: Vec<usize> = (0..total).collect();
                positions.set(next_positions);
                connections.set(build_full_connections(cols, rows));
                z_order.set(order);
                rotations.set(vec![0.0; total]);
                flips.set(vec![false; total]);
                active_id.set(None);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                *drag_state.borrow_mut() = None;
                solved.set(true);
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_solve_rotation = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let positions_snapshot = (*positions).clone();
                let flips_snapshot = (*flips).clone();
                let connections_snapshot = (*connections).clone();
                let zeroed = vec![0.0; total];
                rotations.set(zeroed.clone());
                let solved_now = is_solved(
                    &positions_snapshot,
                    &zeroed,
                    &flips_snapshot,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                solved.set(solved_now);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                save_revision.set(save_revision.wrapping_add(1));
            })
        };
        let on_unflip = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let connections = connections.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let solved = solved.clone();
            let save_revision = save_revision.clone();
            Callback::from(move |_: MouseEvent| {
                let cols = grid.cols as usize;
                let rows = grid.rows as usize;
                let total = cols * rows;
                if total == 0 {
                    return;
                }
                let positions_snapshot = (*positions).clone();
                let rotations_snapshot = (*rotations).clone();
                let connections_snapshot = (*connections).clone();
                let cleared = vec![false; total];
                flips.set(cleared.clone());
                let solved_now = is_solved(
                    &positions_snapshot,
                    &rotations_snapshot,
                    &cleared,
                    &connections_snapshot,
                    cols,
                    rows,
                    piece_width,
                    piece_height,
                    rotation_enabled_value,
                );
                solved.set(solved_now);
                dragging_members.set(Vec::new());
                animating_members.set(Vec::new());
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                save_revision.set(save_revision.wrapping_add(1));
            })
        };

        let color_pattern_bg = "#8f5b32";
        let color_pattern_fg1 = "#734423";
        let color_pattern_fg2 = "#3a2418";
        let back_pattern = html! {
            <pattern
                id="piece-back-pattern"
                patternUnits="userSpaceOnUse"
                width="28"
                height="28"
            >
                <rect width="28" height="28" fill={color_pattern_bg} />
                <circle cx="7" cy="7" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="21" r="2.8" fill={color_pattern_fg1} />
                <circle cx="21" cy="7" r="1.8" fill={color_pattern_fg2} />
                <circle cx="7" cy="21" r="1.8" fill={color_pattern_fg2} />
            </pattern>
        };

        let emboss_opacity = format!("{}", fmt_f32(EMBOSS_OPACITY));
        let emboss_filter_body = html! {
            <>
                <@{"feComponentTransfer"} in="SourceAlpha" result="a">
                    <@{"feFuncA"} type="linear" slope="1" />
                </@>
                <@{"feOffset"} in="a" dx={emboss_offset_neg.clone()} dy={emboss_offset_neg.clone()} result="aOff" />
                <@{"feFlood"} flood-color="#000" result="black" />
                <@{"feComposite"} in="black" in2="a" operator="in" result="blackShape" />
                <@{"feFlood"} flood-color="#fff" flood-opacity="0.6" result="white" />
                <@{"feComposite"} in="white" in2="aOff" operator="in" result="whiteShape" />
                <@{"feMorphology"} in="whiteShape" operator="erode" radius="0.6" result="whiteThin"/>
                <@{"feGaussianBlur"} in="whiteThin" stdDeviation="0.5" result="whiteShapeBlur"/>
                <@{"feComposite"} in="whiteShapeBlur" in2="blackShape" operator="over" result="overlayFull"/>
                <@{"feMorphology"} in="a" operator="erode" radius={emboss_rim_radius.clone()} result="aInner" />
                <@{"feComposite"} in="a" in2="aInner" operator="arithmetic" k1="0" k2="1" k3="-1" k4="0" result="rim" />
                <@{"feComposite"} in="overlayFull" in2="rim" operator="in" result="overlayRim" />
                <@{"feComponentTransfer"} in="overlayRim" result="overlayRimOpacity">
                  <@{"feFuncA"} type="linear" slope={emboss_opacity}/>
                </@>
                <@{"feMerge"}>
                    <@{"feMergeNode"} in="SourceGraphic" />
                    <@{"feMergeNode"} in="overlayRimOpacity" />
                </@>
            </>
        };
        let emboss_filter = if fast_filter_value {
            let filter_res = format!(
                "{} {}",
                fmt_f32(piece_width * 0.2),
                fmt_f32(piece_height * 0.2)
            );
            html! {
                <filter
                    id="emboss"
                    x={emboss_x.clone()}
                    y={emboss_y.clone()}
                    width={emboss_width.clone()}
                    height={emboss_height.clone()}
                    filterUnits="userSpaceOnUse"
                    primitiveUnits="userSpaceOnUse"
                    color-interpolation-filters="linearRGB"
                    filterRes={filter_res}
                >
                    {emboss_filter_body}
                </filter>
            }
        } else {
            html! {
                <filter
                    id="emboss"
                    x={emboss_x.clone()}
                    y={emboss_y.clone()}
                    width={emboss_width.clone()}
                    height={emboss_height.clone()}
                    filterUnits="userSpaceOnUse"
                    primitiveUnits="userSpaceOnUse"
                    color-interpolation-filters="linearRGB"
                >
                    {emboss_filter_body}
                </filter>
            }
        };

        let mask_defs: Html = piece_shapes
            .iter()
            .map(|(piece, paths)| {
                let mask_id = format!("piece-mask-{}", piece.id);
                html! {
                    <mask
                        id={mask_id}
                        maskUnits="userSpaceOnUse"
                        maskContentUnits="userSpaceOnUse"
                        x={mask_x.clone()}
                        y={mask_y.clone()}
                        width={mask_width.clone()}
                        height={mask_height.clone()}
                        mask-type="luminance"
                    >
                        <rect
                            x={mask_x.clone()}
                            y={mask_y.clone()}
                            width={mask_width.clone()}
                            height={mask_height.clone()}
                            fill="black"
                        />
                        <path d={paths.outline.clone()} fill="white" />
                    </mask>
                }
            })
            .collect();

        let begin_drag: Rc<dyn Fn(usize, f32, f32, bool, bool, Option<i32>)> = {
            let positions = positions.clone();
            let rotations = rotations.clone();
            let drag_state = drag_state.clone();
            let active_id = active_id.clone();
            let dragging_members = dragging_members.clone();
            let animating_members = animating_members.clone();
            let rotation_anim = rotation_anim.clone();
            let rotation_anim_handle = rotation_anim_handle.clone();
            let rotation_queue = rotation_queue.clone();
            let z_order = z_order.clone();
            let connections = connections.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            Rc::new(move |piece_id, x, y, shift_key, rotate_mode, touch_id| {
                let positions_snapshot = (*positions).clone();
                let mut connections_snapshot = (*connections).clone();
                let mut members = if shift_key {
                    clear_piece_connections(&mut connections_snapshot, piece_id, cols, rows);
                    connections.set(connections_snapshot);
                    vec![piece_id]
                } else {
                    collect_group(&connections_snapshot, piece_id, cols, rows)
                };
                if members.is_empty() {
                    members.push(piece_id);
                }
                *rotation_anim.borrow_mut() = None;
                rotation_anim_handle.borrow_mut().take();
                rotation_queue.borrow_mut().clear();
                animating_members.set(Vec::new());
                dragging_members.set(members.clone());
                let base_col = piece_id % cols;
                let base_row = piece_id / cols;
                let base_pos = (
                    base_col as f32 * piece_width,
                    base_row as f32 * piece_height,
                );
                let pos = positions_snapshot.get(piece_id).copied().unwrap_or(base_pos);
                let pivot_x = pos.0 + piece_width * 0.5;
                let pivot_y = pos.1 + piece_height * 0.5;
                let start_angle = (y - pivot_y).atan2(x - pivot_x);
                let mut order = (*z_order).clone();
                let mut in_group = vec![false; cols * rows];
                for id in &members {
                    if *id < in_group.len() {
                        in_group[*id] = true;
                    }
                }
                let mut group_order = Vec::new();
                for id in &order {
                    if *id < in_group.len() && in_group[*id] {
                        group_order.push(*id);
                    }
                }
                order.retain(|id| !in_group.get(*id).copied().unwrap_or(false));
                order.extend(group_order);
                z_order.set(order);
                let mut start_positions = Vec::with_capacity(members.len());
                for id in &members {
                    if let Some(start) = positions_snapshot.get(*id) {
                        start_positions.push(*start);
                    } else {
                        start_positions.push(pos);
                    }
                }
                let rotations_snapshot = (*rotations).clone();
                let mut start_rotations = Vec::with_capacity(members.len());
                for id in &members {
                    let rot = rotations_snapshot.get(*id).copied().unwrap_or(0.0);
                    start_rotations.push(rot);
                }
                *drag_state.borrow_mut() = Some(DragState {
                    start_x: x,
                    start_y: y,
                    start_time: Date::now() as f32,
                    primary_id: piece_id,
                    touch_id,
                    rotate_mode,
                    pivot_x,
                    pivot_y,
                    start_angle,
                    members,
                    start_positions,
                    start_rotations,
                });
                active_id.set(Some(piece_id));
            })
        };

        let mut nodes = Vec::with_capacity(piece_shapes.len());
        for (piece, paths) in piece_shapes.iter() {
            let piece_id = piece.id;
            let piece_x = piece.col as f32 * piece_width;
            let piece_y = piece.row as f32 * piece_height;
            let current =
                positions_value.get(piece.id).copied().unwrap_or((piece_x, piece_y));
            let rotation = rotations_value.get(piece.id).copied().unwrap_or(0.0);
            let flipped = flips_value.get(piece.id).copied().unwrap_or(false);
            let center_x = piece_width * 0.5;
            let center_y = piece_height * 0.5;
            let flip_transform = if flipped {
                format!(
                    " translate({} {}) scale(-1 1) translate(-{} -{})",
                    fmt_f32(center_x),
                    fmt_f32(center_y),
                    fmt_f32(center_x),
                    fmt_f32(center_y)
                )
            } else {
                String::new()
            };
            let outer_transform = format!(
                "translate({} {})",
                fmt_f32(current.0),
                fmt_f32(current.1)
            );
            let inner_transform = format!(
                "{} rotate({} {} {})",
                flip_transform,
                fmt_f32(rotation),
                fmt_f32(center_x),
                fmt_f32(center_y)
            );
            let mask_ref = format!("url(#piece-mask-{})", piece.id);
            let img_x = fmt_f32(-piece_x);
            let img_y = fmt_f32(-piece_y);
            let is_dragging = dragging_mask.get(piece.id).copied().unwrap_or(false);
            let is_animating = animating_mask.get(piece.id).copied().unwrap_or(false);
            let is_hovered = hovered_mask.get(piece.id).copied().unwrap_or(false);
            let mut class = if is_dragging {
                if flipped {
                    "piece dragging flipped".to_string()
                } else {
                    "piece dragging".to_string()
                }
            } else if is_animating {
                if flipped {
                    "piece animating flipped".to_string()
                } else {
                    "piece animating".to_string()
                }
            } else if flipped {
                "piece flipped".to_string()
            } else {
                "piece".to_string()
            };
            if is_hovered {
                class.push_str(" hovered");
            }
            let connection = connections_value
                .get(piece.id)
                .copied()
                .unwrap_or([false; 4]);
            let mut external_path = String::new();
            for (dir, edge_path) in [
                (DIR_UP, &paths.edges[DIR_UP]),
                (DIR_RIGHT, &paths.edges[DIR_RIGHT]),
                (DIR_DOWN, &paths.edges[DIR_DOWN]),
                (DIR_LEFT, &paths.edges[DIR_LEFT]),
            ] {
                let connected = connection.get(dir).copied().unwrap_or(false);
                if !connected {
                    if !external_path.is_empty() {
                        external_path.push(' ');
                    }
                    external_path.push_str(edge_path);
                }
            }
            let external_outline = if external_path.is_empty() {
                html! {}
            } else {
                html! {
                    <g class="piece-outline-group edge-external">
                        <path class="piece-outline edge-external" d={external_path} />
                    </g>
                }
            };
            let simple_outline = if emboss_enabled_value {
                html! {}
            } else {
                html! {
                    <path class="piece-outline piece-outline-simple" d={paths.outline.clone()} />
                }
            };
            let on_piece_down = {
                let svg_ref = svg_ref.clone();
                let begin_drag = begin_drag.clone();
                Callback::from(move |event: MouseEvent| {
                    if let Some((x, y)) = event_to_svg_coords(
                        &event,
                        &svg_ref,
                        view_min_x,
                        view_min_y,
                        view_width,
                        view_height,
                    ) {
                        begin_drag(piece_id, x, y, event.shift_key(), event.ctrl_key(), None);
                    }
                    event.prevent_default();
                })
            };
            let on_piece_touch = {
                let svg_ref = svg_ref.clone();
                let begin_drag = begin_drag.clone();
                Callback::from(move |event: TouchEvent| {
                    if event.touches().length() > 1 {
                        return;
                    }
                    if let Some(touch) = touch_from_event(&event, None, true) {
                        let touch_id = Some(touch.identifier());
                        if let Some((x, y)) = touch_event_to_svg_coords(
                            &event,
                            &svg_ref,
                            view_min_x,
                            view_min_y,
                            view_width,
                            view_height,
                            touch_id,
                            true,
                        ) {
                            begin_drag(piece_id, x, y, false, false, touch_id);
                        }
                    }
                })
            };
            let on_piece_enter = {
                let hovered_id = hovered_id.clone();
                let piece_id = piece_id;
                Callback::from(move |_| {
                    hovered_id.set(Some(piece_id));
                })
            };
            let on_piece_leave = {
                let hovered_id = hovered_id.clone();
                Callback::from(move |_| {
                    hovered_id.set(None);
                })
            };
            let debug_overlay = if show_debug_value {
                let label = format!(
                    "#{}\nx:{}\ny:{}\nr:{}",
                    piece.id,
                    fmt_f32(current.0),
                    fmt_f32(current.1),
                    fmt_f32(rotation)
                );
                html! {
                    <>
                        <circle
                            class="piece-debug-center"
                            cx={fmt_f32(center_x)}
                            cy={fmt_f32(center_y)}
                            r="3"
                        />
                        <text
                            class="piece-debug-label"
                            x={fmt_f32(center_x)}
                            y={fmt_f32(center_y - 12.0)}
                        >
                            {label
                                .lines()
                                .enumerate()
                                .map(|(idx, line)| {
                                    html! {
                                        <tspan
                                            x={fmt_f32(center_x)}
                                            dy={if idx == 0 { "-20" } else { "20" }}
                                        >
                                            {line}
                                        </tspan>
                                    }
                                })
                                .collect::<Html>()}
                        </text>
                    </>
                }
            } else {
                html! {}
            };
            let emboss_target = if emboss_enabled_value {
                "url(#emboss)"
            } else {
                "none"
            };
            let node = html! {
                <g
                    key={piece.id.to_string()}
                    class={class}
                    transform={outer_transform}
                    onmousedown={on_piece_down}
                    ontouchstart={on_piece_touch}
                    onmouseenter={on_piece_enter}
                    onmouseleave={on_piece_leave}
                >
                    <g transform={inner_transform.clone()}>
                        {external_outline}
                        <path class="piece-hitbox" d={paths.outline.clone()} />
                    </g>
                    <g class="piece-surface" filter={emboss_target}>
                        <g transform={inner_transform.clone()}>
                            <rect
                                class="piece-back"
                                x={img_x.clone()}
                                y={img_y.clone()}
                                width={width.to_string()}
                                height={height.to_string()}
                                fill="url(#piece-back-pattern)"
                                mask={mask_ref.clone()}
                            />
                            <image
                                class="piece-image"
                                href={IMAGE_SRC}
                                x={img_x}
                                y={img_y}
                                width={width.to_string()}
                                height={height.to_string()}
                                preserveAspectRatio="xMidYMid meet"
                                mask={mask_ref}
                            />
                        </g>
                    </g>
                    <g transform={inner_transform}>
                        {simple_outline}
                        {debug_overlay}
                    </g>
                </g>
            };
            nodes.push(node);
        }
        let piece_nodes: Html = if z_order_value.len() == nodes.len() {
            z_order_value
                .iter()
                .filter_map(|id| nodes.get(*id))
                .cloned()
                .collect()
        } else {
            nodes.into_iter().collect()
        };
        let on_canvas_down = {
            let canvas_ref = canvas_ref.clone();
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let begin_drag = begin_drag.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            Callback::from(move |event: MouseEvent| {
                let Some((x, y)) = event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) else {
                    return;
                };
                let mask_atlas_ref = mask_atlas.borrow();
                let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                    return;
                };
                let positions_snapshot = (*positions).clone();
                let rotations_snapshot = (*rotations).clone();
                let flips_snapshot = (*flips).clone();
                let mut order = (*z_order).clone();
                let total = cols * rows;
                if order.len() != total {
                    order = (0..total).collect();
                }
                if let Some(piece_id) = pick_piece_at(
                    x,
                    y,
                    &positions_snapshot,
                    &rotations_snapshot,
                    &flips_snapshot,
                    &order,
                    mask_atlas,
                    cols,
                    piece_width,
                    piece_height,
                    mask_pad,
                ) {
                    begin_drag(piece_id, x, y, event.shift_key(), event.ctrl_key(), None);
                }
                event.prevent_default();
            })
        };
        let on_canvas_touch = {
            let canvas_ref = canvas_ref.clone();
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let begin_drag = begin_drag.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            Callback::from(move |event: TouchEvent| {
                if event.touches().length() > 1 {
                    return;
                }
                let Some(touch) = touch_from_event(&event, None, true) else {
                    return;
                };
                let touch_id = Some(touch.identifier());
                let Some((x, y)) = touch_event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                    touch_id,
                    true,
                ) else {
                    return;
                };
                let mask_atlas_ref = mask_atlas.borrow();
                let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                    return;
                };
                let positions_snapshot = (*positions).clone();
                let rotations_snapshot = (*rotations).clone();
                let flips_snapshot = (*flips).clone();
                let mut order = (*z_order).clone();
                let total = cols * rows;
                if order.len() != total {
                    order = (0..total).collect();
                }
                if let Some(piece_id) = pick_piece_at(
                    x,
                    y,
                    &positions_snapshot,
                    &rotations_snapshot,
                    &flips_snapshot,
                    &order,
                    mask_atlas,
                    cols,
                    piece_width,
                    piece_height,
                    mask_pad,
                ) {
                    begin_drag(piece_id, x, y, false, false, touch_id);
                }
                event.prevent_default();
            })
        };
        let on_canvas_move = {
            let canvas_ref = canvas_ref.clone();
            let positions = positions.clone();
            let rotations = rotations.clone();
            let flips = flips.clone();
            let z_order = z_order.clone();
            let mask_atlas = mask_atlas.clone();
            let hovered_id = hovered_id.clone();
            let cols = grid.cols as usize;
            let rows = grid.rows as usize;
            Callback::from(move |event: MouseEvent| {
                let Some((x, y)) = event_to_svg_coords(
                    &event,
                    &canvas_ref,
                    view_min_x,
                    view_min_y,
                    view_width,
                    view_height,
                ) else {
                    hovered_id.set(None);
                    return;
                };
                let mask_atlas_ref = mask_atlas.borrow();
                let Some(mask_atlas) = mask_atlas_ref.as_ref() else {
                    hovered_id.set(None);
                    return;
                };
                let positions_snapshot = &*positions;
                let rotations_snapshot = &*rotations;
                let flips_snapshot = &*flips;
                let order_snapshot = &*z_order;
                let total = cols * rows;
                let mut fallback_order = Vec::new();
                let order = if order_snapshot.len() == total {
                    order_snapshot.as_slice()
                } else {
                    fallback_order = (0..total).collect();
                    fallback_order.as_slice()
                };
                let hit = pick_piece_at(
                    x,
                    y,
                    positions_snapshot,
                    rotations_snapshot,
                    flips_snapshot,
                    order,
                    mask_atlas,
                    cols,
                    piece_width,
                    piece_height,
                    mask_pad,
                );
                hovered_id.set(hit);
            })
        };
        let on_canvas_leave = {
            let hovered_id = hovered_id.clone();
            Callback::from(move |_: MouseEvent| {
                hovered_id.set(None);
            })
        };
        let on_context_menu = Callback::from(|event: MouseEvent| {
            event.prevent_default();
        });
        let bounds_inset = 1.0;
        let bounds = html! {
            <>
                <rect
                    class="workspace-bounds"
                    x={fmt_f32(view_min_x)}
                    y={fmt_f32(view_min_y)}
                    width={fmt_f32(view_width)}
                    height={fmt_f32(view_height)}
                />
                <rect
                    class="puzzle-bounds"
                    x={fmt_f32(bounds_inset)}
                    y={fmt_f32(bounds_inset)}
                    width={fmt_f32(width_f - 2.0 * bounds_inset)}
                    height={fmt_f32(height_f - 2.0 * bounds_inset)}
                    rx={frame_corner_radius.clone()}
                    ry={frame_corner_radius.clone()}
                />
            </>
        };

        let mut svg_class = if show_debug_value {
            "puzzle-image debug".to_string()
        } else {
            "puzzle-image".to_string()
        };
        if !animations_enabled_value {
            svg_class.push_str(" no-anim");
        }
        if fast_render_value {
            svg_class.push_str(" fast-render");
        }
        let mut canvas_class = "puzzle-canvas".to_string();
        if active_id_value.is_some() {
            canvas_class.push_str(" dragging");
        } else if hovered_id_value.is_some() {
            canvas_class.push_str(" hover");
        }
        let canvas_node = if wgpu_enabled_value {
            html! {
                <canvas
                    class={canvas_class}
                    ref={canvas_ref}
                    width={view_width.round().to_string()}
                    height={view_height.round().to_string()}
                    onmousedown={on_canvas_down}
                    onmousemove={on_canvas_move}
                    onmouseleave={on_canvas_leave}
                    ontouchstart={on_canvas_touch}
                    oncontextmenu={on_context_menu.clone()}
                />
            }
        } else {
            html! {}
        };
        let svg_node = if wgpu_enabled_value {
            html! {}
        } else {
            html! {
                <svg
                    xmlns="http://www.w3.org/2000/svg"
                    class={svg_class}
                    viewBox={view_box}
                    width={fmt_f32(view_width)}
                    height={fmt_f32(view_height)}
                    preserveAspectRatio="xMidYMid meet"
                    ref={svg_ref}
                    oncontextmenu={on_context_menu}
                >
                    <defs>
                        {back_pattern}
                        {emboss_filter}
                        {mask_defs}
                    </defs>
                    {bounds}
                    {piece_nodes}
                </svg>
            }
        };
        (
            html! {
                <>
                    {canvas_node}
                    {svg_node}
                </>
            },
            on_scramble,
            on_solve,
            on_solve_rotation,
            on_unflip,
            false,
        )
    } else {
        (
            html! { <p>{ "Loading puzzle image..." }</p> },
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            Callback::from(|_: MouseEvent| {}),
            true,
        )
    };

    let controls_hint = html! {
        <a
            class="controls-hint"
            href="https://github.com/sugoijan/heddobureika"
            target="_blank"
            rel="noopener noreferrer"
        >
            { "source" }
        </a>
    };
    let preview_corner_class = match *preview_corner {
        PreviewCorner::BottomLeft => "corner-bl",
        PreviewCorner::BottomRight => "corner-br",
        PreviewCorner::TopLeft => "corner-tl",
        PreviewCorner::TopRight => "corner-tr",
    };
    let preview_state_class = if preview_revealed_value {
        "preview-revealed"
    } else {
        "preview-hidden"
    };
    let preview_class = format!(
        "preview-box {} {}",
        preview_corner_class, preview_state_class
    );
    let on_preview_toggle = {
        let preview_revealed = preview_revealed.clone();
        Callback::from(move |_| {
            preview_revealed.set(!*preview_revealed);
        })
    };
    let preview_toggle_label = if preview_revealed_value {
        "Hide preview"
    } else {
        "Show preview"
    };
    let on_preview_hide = {
        let preview_revealed = preview_revealed.clone();
        Callback::from(move |_| {
            if *preview_revealed {
                preview_revealed.set(false);
            }
        })
    };
    let on_preview_horizontal = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::BottomRight,
                PreviewCorner::BottomRight => PreviewCorner::BottomLeft,
                PreviewCorner::TopLeft => PreviewCorner::TopRight,
                PreviewCorner::TopRight => PreviewCorner::TopLeft,
            };
            preview_corner.set(next);
        })
    };
    let on_preview_vertical = {
        let preview_corner = preview_corner.clone();
        Callback::from(move |_| {
            let next = match *preview_corner {
                PreviewCorner::BottomLeft => PreviewCorner::TopLeft,
                PreviewCorner::BottomRight => PreviewCorner::TopRight,
                PreviewCorner::TopLeft => PreviewCorner::BottomLeft,
                PreviewCorner::TopRight => PreviewCorner::BottomRight,
            };
            preview_corner.set(next);
        })
    };
    let preview_arrow_horizontal = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::TopLeft => "preview-arrow preview-arrow-right",
        PreviewCorner::BottomRight | PreviewCorner::TopRight => "preview-arrow preview-arrow-left",
    };
    let preview_arrow_vertical = match *preview_corner {
        PreviewCorner::BottomLeft | PreviewCorner::BottomRight => "preview-arrow preview-arrow-up",
        PreviewCorner::TopLeft | PreviewCorner::TopRight => "preview-arrow preview-arrow-down",
    };
    let preview_toggle_slash = if preview_revealed_value {
        html! { <line class="preview-toggle-slash" x1="5" y1="19" x2="19" y2="5" /> }
    } else {
        html! {}
    };
    let preview_box = html! {
        <aside class={preview_class}>
            <button
                class="preview-toggle"
                type="button"
                aria-label={preview_toggle_label}
                aria-pressed={if preview_revealed_value { "true" } else { "false" }}
                onclick={on_preview_toggle}
            >
                <svg class="preview-toggle-icon" viewBox="0 0 24 24" aria-hidden="true">
                    <path
                        class="preview-toggle-eye"
                        d="M2 12c2.4-4.2 5.8-6.4 10-6.4s7.6 2.2 10 6.4c-2.4 4.2-5.8 6.4-10 6.4S4.4 16.2 2 12z"
                    />
                    <circle class="preview-toggle-pupil" cx="12" cy="12" r="3.2" />
                    {preview_toggle_slash}
                </svg>
            </button>
            <button
                class={preview_arrow_horizontal}
                type="button"
                aria-label="Move preview horizontally"
                onclick={on_preview_horizontal}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <button
                class={preview_arrow_vertical}
                type="button"
                aria-label="Move preview vertically"
                onclick={on_preview_vertical}
            >
                <svg class="preview-arrow-icon" viewBox="0 0 12 12" aria-hidden="true">
                    <polyline points="4,2 8,6 4,10" />
                </svg>
            </button>
            <img src={IMAGE_SRC} alt="preview" onclick={on_preview_hide} />
        </aside>
    };
    let controls_panel = if show_controls_value {
        html! {
            <aside class="controls">
                <h2>{ "Dev Panel" }</h2>
                <p class={status_class}>{ status_label }</p>
                <div class="control">
                    <label>
                        { "Seed" }
                        <span class="control-value">{ seed_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label>
                        { "Expected solve time" }
                        <span class="control-value">{ solve_time_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label>
                        { "Connections" }
                        <span class="control-value">{ connections_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label>
                        { "Border connections" }
                        <span class="control-value">{ border_connections_label }</span>
                    </label>
                </div>
                <div class="control">
                    <label for="grid-select">
                        { "Grid" }
                        <span class="control-value">{ grid_label }</span>
                    </label>
                    <select
                        id="grid-select"
                        onchange={on_grid_change}
                    >
                        {grid_options}
                    </select>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_scramble}
                        disabled={scramble_disabled}
                    >
                        { "Scramble" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_solve}
                        disabled={scramble_disabled}
                    >
                        { "Solve" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_solve_rotation}
                        disabled={scramble_disabled}
                    >
                        { "Solve rotation" }
                    </button>
                </div>
                <div class="control">
                    <button
                        class="control-button"
                        type="button"
                        onclick={on_unflip}
                        disabled={scramble_disabled}
                    >
                        { "Unflip all" }
                    </button>
                </div>
                <div class="control">
                    <label for="workspace-scale">
                        { "Workspace scale" }
                        <span class="control-value">{ fmt_f32(workspace_scale_value) }</span>
                    </label>
                    <input
                        id="workspace-scale"
                        type="range"
                        min={WORKSPACE_SCALE_MIN.to_string()}
                        max={WORKSPACE_SCALE_MAX.to_string()}
                        step="0.05"
                        value={workspace_scale_value.to_string()}
                        oninput={on_workspace_scale}
                    />
                </div>
                <div class="control">
                    <label for="animations-enabled">
                        { "Animations: " } { if animations_enabled_value { "On" } else { "Off" } }
                        <input
                            id="animations-enabled"
                            type="checkbox"
                            checked={animations_enabled_value}
                            onchange={on_animations_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="emboss-enabled">
                        { "Emboss: " } { if emboss_enabled_value { "On" } else { "Off" } }
                        <input
                            id="emboss-enabled"
                            type="checkbox"
                            checked={emboss_enabled_value}
                            onchange={on_emboss_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="theme-mode">
                        { "Theme: " }
                        { match theme_mode_value {
                            ThemeMode::System => "System",
                            ThemeMode::Light => "Light",
                            ThemeMode::Dark => "Dark",
                        } }
                        <input
                            id="theme-mode"
                            type="checkbox"
                            ref={theme_toggle_ref}
                            onclick={on_theme_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="fast-render">
                        { "Fast render: " } { if fast_render_value { "On" } else { "Off" } }
                        <input
                            id="fast-render"
                            type="checkbox"
                            checked={fast_render_value}
                            onchange={on_fast_render_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="fast-filter">
                        { "Fast filter: " } { if fast_filter_value { "On" } else { "Off" } }
                        <input
                            id="fast-filter"
                            type="checkbox"
                            checked={fast_filter_value}
                            onchange={on_fast_filter_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="wgpu-enabled">
                        { "WGPU preview: " } { if wgpu_enabled_value { "On" } else { "Off" } }
                        <input
                            id="wgpu-enabled"
                            type="checkbox"
                            checked={wgpu_enabled_value}
                            onchange={on_wgpu_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="frame-snap">
                        { "Frame snap" }
                        <span class="control-value">{ fmt_f32(frame_snap_ratio_value) }</span>
                    </label>
                    <input
                        id="frame-snap"
                        type="range"
                        min={FRAME_SNAP_MIN.to_string()}
                        max={FRAME_SNAP_MAX.to_string()}
                        step="0.05"
                        value={frame_snap_ratio_value.to_string()}
                        oninput={on_frame_snap}
                    />
                </div>
                <div class="control">
                    <label for="snap-distance">
                        { "Snap distance tol" }
                        <span class="control-value">{ fmt_f32(snap_distance_ratio_value) }</span>
                    </label>
                    <input
                        id="snap-distance"
                        type="range"
                        min={SNAP_DISTANCE_RATIO_MIN.to_string()}
                        max={SNAP_DISTANCE_RATIO_MAX.to_string()}
                        step="0.01"
                        value={snap_distance_ratio_value.to_string()}
                        oninput={on_snap_distance}
                    />
                </div>
                <div class="control">
                    <label for="rotation-snap-tolerance">
                        { "Snap angle tol (deg)" }
                        <span class="control-value">{ fmt_f32(rotation_snap_tolerance_value) }</span>
                    </label>
                    <input
                        id="rotation-snap-tolerance"
                        type="range"
                        min={ROTATION_SNAP_TOLERANCE_MIN_DEG.to_string()}
                        max={ROTATION_SNAP_TOLERANCE_MAX_DEG.to_string()}
                        step="0.5"
                        value={rotation_snap_tolerance_value.to_string()}
                        oninput={on_rotation_snap_tolerance}
                    />
                </div>
                <div class="control">
                    <label for="rotation-lock-threshold">
                        { "Aligned rotate <= " }
                        <span class="control-value">{ rotation_lock_threshold_value }</span>
                    </label>
                    <input
                        id="rotation-lock-threshold"
                        type="range"
                        min={ROTATION_LOCK_THRESHOLD_MIN.to_string()}
                        max={total.max(ROTATION_LOCK_THRESHOLD_MIN).to_string()}
                        step="1"
                        value={rotation_lock_threshold_value.to_string()}
                        oninput={on_rotation_lock_threshold}
                    />
                </div>
                <div class="control">
                    <label for="rotation-enabled">
                        { "Rotation: " } { if rotation_enabled_value { "On" } else { "Off" } }
                        <input
                            id="rotation-enabled"
                            type="checkbox"
                            checked={rotation_enabled_value}
                            onchange={on_rotation_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="rotation-noise">
                        { "Rotation noise" }
                        <span class="control-value">{ fmt_f32(rotation_noise_value) }</span>
                    </label>
                    <input
                        id="rotation-noise"
                        type="range"
                        min={ROTATION_NOISE_MIN.to_string()}
                        max={ROTATION_NOISE_MAX.to_string()}
                        step="0.1"
                        value={rotation_noise_value.to_string()}
                        oninput={on_rotation_noise}
                    />
                </div>
                <div class="control">
                    <label for="debug-enabled">
                        { "Debug overlay: " } { if show_debug_value { "On" } else { "Off" } }
                        <input
                            id="debug-enabled"
                            type="checkbox"
                            checked={show_debug_value}
                            onchange={on_debug_toggle}
                        />
                    </label>
                </div>
                <div class="control">
                    <label for="tab-width">
                        { "Tab size" }
                        <span class="control-value">{ fmt_f32(settings_value.tab_width) }</span>
                    </label>
                    <input
                        id="tab-width"
                        type="range"
                        min={TAB_WIDTH_MIN.to_string()}
                        max={TAB_WIDTH_MAX.to_string()}
                        step="0.005"
                        value={settings_value.tab_width.to_string()}
                        oninput={tab_width_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-depth">
                        { "Tab depth" }
                        <span class="control-value">{ fmt_f32(settings_value.tab_depth) }</span>
                    </label>
                    <input
                        id="tab-depth"
                        type="range"
                        min={TAB_DEPTH_MIN.to_string()}
                        max={TAB_DEPTH_MAX.to_string()}
                        step="0.01"
                        value={settings_value.tab_depth.to_string()}
                        oninput={tab_depth_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-scale">
                        { "Tab size scale" }
                        <span class="control-value">
                            { fmt_f32(settings_value.tab_size_scale) }
                        </span>
                    </label>
                    <input
                        id="tab-size-scale"
                        type="range"
                        min={TAB_SIZE_SCALE_MIN.to_string()}
                        max={TAB_SIZE_SCALE_MAX.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_scale.to_string()}
                        oninput={tab_size_scale_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-min">
                        { "Tab size min" }
                        <span class="control-value">{ fmt_f32(settings_value.tab_size_min) }</span>
                    </label>
                    <input
                        id="tab-size-min"
                        type="range"
                        min={TAB_SIZE_MIN_LIMIT.to_string()}
                        max={settings_value.tab_size_max.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_min.to_string()}
                        oninput={tab_size_min_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-size-max">
                        { "Tab size max" }
                        <span class="control-value">{ fmt_f32(settings_value.tab_size_max) }</span>
                    </label>
                    <input
                        id="tab-size-max"
                        type="range"
                        min={settings_value.tab_size_min.to_string()}
                        max={TAB_SIZE_MAX_LIMIT.to_string()}
                        step="0.005"
                        value={settings_value.tab_size_max.to_string()}
                        oninput={tab_size_max_input}
                    />
                </div>
                <div class="control">
                    <label for="skew-range">
                        { "Center skew" }
                        <span class="control-value">{ fmt_f32(settings_value.skew_range) }</span>
                    </label>
                    <input
                        id="skew-range"
                        type="range"
                        min="0.0"
                        max={SKEW_RANGE_MAX.to_string()}
                        step="0.005"
                        value={settings_value.skew_range.to_string()}
                        oninput={skew_input}
                    />
                </div>
                <div class="control">
                    <label for="variation">
                        { "Variation" }
                        <span class="control-value">{ fmt_f32(settings_value.variation) }</span>
                    </label>
                    <input
                        id="variation"
                        type="range"
                        min={VARIATION_MIN.to_string()}
                        max={VARIATION_MAX.to_string()}
                        step="0.01"
                        value={settings_value.variation.to_string()}
                        oninput={variation_input}
                    />
                </div>
                <div class="control">
                    <label for="jitter-strength">
                        { "Jitter strength" }
                        <span class="control-value">
                            { fmt_f32(settings_value.jitter_strength) }
                        </span>
                    </label>
                    <input
                        id="jitter-strength"
                        type="range"
                        min={JITTER_STRENGTH_MIN.to_string()}
                        max={JITTER_STRENGTH_MAX.to_string()}
                        step="0.005"
                        value={settings_value.jitter_strength.to_string()}
                        oninput={jitter_strength_input}
                    />
                </div>
                <div class="control">
                    <label for="jitter-len-bias">
                        { "Length jitter bias" }
                        <span class="control-value">
                            { fmt_f32(settings_value.jitter_len_bias) }
                        </span>
                    </label>
                    <input
                        id="jitter-len-bias"
                        type="range"
                        min={JITTER_LEN_BIAS_MIN.to_string()}
                        max={JITTER_LEN_BIAS_MAX.to_string()}
                        step="0.01"
                        value={settings_value.jitter_len_bias.to_string()}
                        oninput={jitter_len_bias_input}
                    />
                </div>
                <div class="control">
                    <label for="line-bend">
                        { "Grid bend" }
                        <span class="control-value">{ fmt_f32(settings_value.line_bend_ratio) }</span>
                    </label>
                    <input
                        id="line-bend"
                        type="range"
                        min={LINE_BEND_MIN.to_string()}
                        max={MAX_LINE_BEND_RATIO.to_string()}
                        step="0.01"
                        value={settings_value.line_bend_ratio.to_string()}
                        oninput={line_bend_input}
                    />
                </div>
                <div class="control">
                    <label for="tab-depth-cap">
                        { "Tab depth cap" }
                        <span class="control-value">
                            { fmt_f32(settings_value.tab_depth_cap) }
                        </span>
                    </label>
                    <input
                        id="tab-depth-cap"
                        type="range"
                        min={TAB_DEPTH_CAP_MIN.to_string()}
                        max={TAB_DEPTH_CAP_MAX.to_string()}
                        step="0.01"
                        value={settings_value.tab_depth_cap.to_string()}
                        oninput={tab_depth_cap_input}
                    />
                </div>
                <div class="control">
                    <label for="curve-detail">
                        { "Curve detail" }
                        <span class="control-value">
                            { fmt_f32(settings_value.curve_detail) }
                        </span>
                    </label>
                    <input
                        id="curve-detail"
                        type="range"
                        min={CURVE_DETAIL_MIN.to_string()}
                        max={CURVE_DETAIL_MAX.to_string()}
                        step="0.05"
                        value={settings_value.curve_detail.to_string()}
                        oninput={curve_detail_input}
                    />
                </div>
            </aside>
        }
    } else {
        html! {}
    };
    html! {
        <main class="app">
            {content}
            {solved_banner}
            {controls_hint}
            {preview_box}
            {controls_panel}
        </main>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    fn assert_close(actual: f32, expected: f32) {
        let delta = (actual - expected).abs();
        assert!(
            delta <= 1e-6,
            "expected {:.6} got {:.6} (delta {:.6})",
            expected,
            actual,
            delta
        );
    }

    #[wasm_bindgen_test]
    fn align_group_sets_uniform_rotation() {
        let cols = 3usize;
        let piece_width = 100.0;
        let piece_height = 100.0;
        let mut positions = vec![(0.0, 0.0); 4];
        let mut rotations = vec![0.0; 4];
        rotations[0] = 11.027;
        rotations[1] = 359.504;
        positions[0] = (12.0, -44.0);
        positions[1] = (180.0, 36.0);
        let members = vec![0usize, 1usize];
        let anchor_id = 0usize;
        let anchor_center = (50.0, 50.0);
        let target_rot = 11.027;

        align_group_to_anchor(
            &mut positions,
            &mut rotations,
            &members,
            anchor_id,
            anchor_center,
            target_rot,
            cols,
            piece_width,
            piece_height,
        );

        for id in &members {
            assert_close(rotations[*id], normalize_angle(target_rot));
        }

        let base_rotation = rotations[members[0]];
        for id in &members[1..] {
            assert_close(rotations[*id], base_rotation);
        }

        for id in 0..rotations.len() {
            if !members.contains(&id) {
                assert_close(rotations[id], 0.0);
            }
        }

        let (dx, dy) = rotate_vec(piece_width, 0.0, normalize_angle(target_rot));
        let expected_center = (anchor_center.0 + dx, anchor_center.1 + dy);
        let pos = positions[1];
        let center = (pos.0 + piece_width * 0.5, pos.1 + piece_height * 0.5);
        assert_close(center.0, expected_center.0);
        assert_close(center.1, expected_center.1);
    }
}
