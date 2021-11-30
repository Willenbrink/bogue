(* a 'window' is in fine just a layout *)
(* Do not mistake it with a hardware (SDL) window *)

open Utils

type t =
  { layout : unit Layout.t;
    mutable is_fresh : bool;
    mutable bogue : bool;
    (* if bogue = false this means that bogue didn't create the corresponding
       SDL window, and we should neither clear it before rendering, nor
       RenderPresent it after rendering *)
  }

let create ?(is_fresh = false) ?(bogue = false) layout =
  let g = layout#current_geom in
  Layout.(layout#set_current_geom { g with x = not_specified; y = not_specified });
  { layout; is_fresh; bogue }

let get_layout w = w.layout

let is_fresh w =
  w.is_fresh

let set_fresh w =
  w.is_fresh <- true

let to_refresh w =
  w.is_fresh <- false

let window w =
  Layout.window w.layout

(* physical size *)
let size w =
  Sdl.get_window_size (window w)

let get_canvas w =
  Layout.get_canvas w.layout

(** get SDL windows id, in case the canvas was created *)
let id w =
  Sdl.get_window_id (Layout.window w.layout)

let equal w1 w2 =
  Widget.equal w1.layout w2.layout

let render w =
  Layout.render w.layout

let flip ?clear w =
  if not (is_fresh w)
  then begin
    render w;
    let clear = default clear w.bogue in
    printd debug_graphics "clear=%b" clear;
    let present = w.bogue in
    Layout.flip ~clear ~present w.layout;
    set_fresh w
  end
  else Draw.clear_layers (w.layout#layer)

let make_sdl_window w =
  printd debug_board "Make_window for layout %s" (Layout.sprint_id w.layout);
  Layout.make_window w.layout;
  w.bogue <- true
