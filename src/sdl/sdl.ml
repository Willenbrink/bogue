open Tsdl
open Sdl

(* Initialization and Shutdown *)
module Init = Init
let quit = quit
let init_sub_system = init_sub_system
let quit_sub_system () = quit_sub_system Init.events
let was_init = was_init

let set_hint () =
  if set_hint Hint.render_scale_quality "1"
  then ()
  else failwith "set_hint failed"



(* Events *)
type event = Sdl.event
module Event =
  struct
    include Event

    let kind ev = enum (get ev typ)
  end

let event_struct = ref None

let wait_event () =
  begin
  match !event_struct with
  | None -> event_struct := Some (Event.create ())
  | Some _ -> ()
    end;
  match wait_event !event_struct with
  | Error (`Msg msg) -> failwith msg
  | Ok () -> match !event_struct with
    | None -> failwith "Event struct uninitialised"
    | Some ev -> ev

(* Mouse *)
module System_cursor = System_cursor
let create_system_cursor = create_system_cursor
let set_cursor = set_cursor

(* Keyboard *)
type keycode = int
module K = K
type keymod = int
module Kmod = Kmod

let get_key_name = get_key_name
let get_mod_state = get_mod_state
let is_text_input_active = is_text_input_active
let start_text_input = start_text_input
let stop_text_input = stop_text_input
let set_text_input_rect = set_text_input_rect
let pressed = pressed



(* Errors *)
let get_error = get_error

(* Windows *)
module Window = Window
type window = Sdl.window

let get_window_size = get_window_size
let set_window_size = set_window_size
let get_window_flags = get_window_flags
let show_window = show_window
let hide_window = hide_window

let create_window = create_window
let destroy_window = destroy_window
let get_window_id = get_window_id
let set_window_icon = set_window_icon
let get_window_pixel_format = get_window_pixel_format

(* OpenGL Contexts *)
type gl_context = Sdl.gl_context
module Gl = Gl
let gl_delete_context = gl_delete_context
let gl_set_attribute = gl_set_attribute

(* Renderers *)
type flip = Sdl.flip
module Flip = Flip

type texture = Sdl.texture

type renderer = Sdl.renderer
module Renderer = Renderer

type render_info = Sdl.renderer_info = {
  ri_name : string;
  ri_flags : Renderer.flags;
  ri_texture_formats : Pixel.format_enum list;
  ri_max_texture_width : int;
  ri_max_texture_height : int;
}

let create_renderer = create_renderer
let get_renderer = get_renderer
let get_window_position = get_window_position
let render_target_supported = render_target_supported
let render_draw_points = render_draw_points
let get_renderer_info = get_renderer_info
let get_render_draw_color = get_render_draw_color
let set_render_draw_color = set_render_draw_color
let destroy_renderer = destroy_renderer
let render_set_clip_rect = render_set_clip_rect
let render_copy_ex = render_copy_ex
let render_draw_point = render_draw_point
let render_draw_lines = render_draw_lines
let render_present = render_present
let render_fill_rect = render_fill_rect
let render_is_clip_enabled = render_is_clip_enabled
let render_get_clip_rect = render_get_clip_rect
let get_render_target = get_render_target
let set_render_target = set_render_target
let render_clear = render_clear
let get_renderer_output_size = get_renderer_output_size
let render_copy = render_copy
let set_render_draw_blend_mode = set_render_draw_blend_mode
let render_read_pixels = render_read_pixels

(* Textures *)
module Texture = Texture
let create_texture = create_texture
let create_texture_from_surface = create_texture_from_surface
let destroy_texture = destroy_texture
let query_texture = query_texture
let get_texture_alpha_mod = get_texture_alpha_mod
let set_texture_alpha_mod = set_texture_alpha_mod
let set_texture_blend_mode = set_texture_blend_mode
let update_texture = update_texture

(* Pixels Formats *)
module Pixel = Pixel
module Blend = Blend
let pixel_format_enum_to_masks = pixel_format_enum_to_masks
let alloc_format = alloc_format
let map_rgba = map_rgba
let free_format = free_format
let get_pixel_format_name = get_pixel_format_name

(* Surface *)
type surface = Sdl.surface
let create_rgb_surface = create_rgb_surface
let free_surface = free_surface
let blit_scaled = blit_scaled
let get_surface_format_enum = get_surface_format_enum
let get_surface_pitch = get_surface_pitch
let get_surface_size = get_surface_size
let fill_rect = fill_rect
let set_surface_blend_mode = set_surface_blend_mode
let blit_surface = blit_surface
let get_clip_rect = get_clip_rect
let set_surface_rle = set_surface_rle

(* Color *)
type color = Sdl.color
module Color = Color

(* Points *)
type point = Sdl.point
module Point = Point

(* Rectangles *)
type rect = Sdl.rect
module Rect = Rect
let intersect_rect = intersect_rect
