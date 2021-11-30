(** a check button, optional label *)

(* TODO: label. For the moment this is done by combining widgets, see
   Widget.check_box_with_label *)
(* TODO: keyboard focus *)

open Utils
open Base

type style =
  | Square
  | Circle (* circle is used for radio buttons *)

let default_circle_size = (12,14);; (* TODO compute at run-time *)
let default_square_size = (17,18)

class t ?(init = false) ?(style = Square) () =
  let size = match style with
      Square -> default_square_size
    | Circle -> default_circle_size
  in

  object (self)
    inherit [bool] w size "Check" Cursor.Hand
    inherit [bool] stateful init

    method set_state x = state <- x

    method unload = ()

    method action = state <- (not state)

    val style = style
    method get_style = style

    (* TODO load the symbol at run-time so that we can change color *)

    method display canvas layer g =
      printd debug_graphics "Display button";
      let open Draw in
      let texture_on = match style with
        | Square -> canvas.textures.check_on
        | Circle -> canvas.textures.radio_on
      in
      let texture_off = match style with
        | Square -> canvas.textures.check_off
        | Circle -> canvas.textures.radio_off
      in
      let tex = if self#state
        then texture_on
        else texture_off in
      (* if self#size = None
       * then _size <- (let w,h = tex_size tex in
       *                 Some (unscale_size (w, h))); *)

      (* DEBUG *)
      (* let (w,h) = tex_size tex in *)
      (* Printf.printf "TEX SIZE=(%u,%u)\n" w h; *)

      [center_tex_to_layer ~clip:false ~horiz:false canvas layer tex g]
      (* we could center horizontally, but then first one should change textures so
         that check_off and check_on have same width. *)
  end

(************* display ***********)

