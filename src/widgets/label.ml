open Base

class ['a] t ?(name = "Label") text =
  object (self)
    inherit ['a] w name Cursor.Arrow

    val mutable text = text
    method text = text
    method set_text x =
      text <- x

    method min_size =
      (15 * String.length text, 30)
      (* TODO FIXME transition to e.g. stb_truetype to avoid initialization <-> window circular dependency *)
      (* Specifically: Raylib requires init before loading fonts. Fonts are required for window size *)
      (* let font, size = Font.State.get () in *)
      (* let res = Raylib.measure_text_ex font text size 0. in *)
      (* Raylib.Vector2.(int_of_float @@ x res, int_of_float @@ y res) *)

    method execute await _ =
      await#forever

    method render geom =
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.blank in
      Font.draw_text (Raylib.addr img) text 0. 0.;

      let tex = Raylib.load_texture_from_image img in
      Raylib.unload_image img;
      [geom, tex]

  end
