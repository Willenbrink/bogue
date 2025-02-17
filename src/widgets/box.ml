(* A Box.t is a passive widget that contains a rectangular texture, which can be
   specified by a color or a Style.background --- which means it can contain an
   Image.t. *)
(* The rectangle can have rounded corners, and a border. *)
(* A Box.t can be used directly as a background for layouts that support
   Layout.background *)

open Base

class ['a] t () =
  object (self)
    inherit ['a] w "Box" Cursor.Arrow

    method min_size = (0,0)

    method execute await _ =
      await#forever

    method render geom =
      (* TODO: make sure hoffset <= h *)
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.blank in
      let tex = Raylib.load_texture_from_image img in
      Raylib.unload_image img;
      [geom, tex]

  end
