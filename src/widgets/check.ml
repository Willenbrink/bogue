(** a check button *)

(* TODO: keyboard focus *)

open Base

type style =
  | Square
  | Circle (* circle is used for radio buttons *)

let default_circle_size = (12,14) (* TODO compute at run-time *)
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

    method execute await yield =
      await#f [`Mouse_press] @@ function
      | Mouse_press _, _ ->
        self#set_state @@ not self#state;
        yield self#state;
        self#execute await yield

    method unload = ()

    method style = style

    method display geom =
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.gray in
      Raylib.image_draw_circle (Raylib.addr img) 10 10 5 Raylib.Color.black;
      let tex_ray = Raylib.load_texture_from_image img in

      [geom, tex_ray]
  end
