open Base

class ['a] t ?(name = "Label") text =
  object (self)
    inherit ['a] w name Cursor.Arrow

    val mutable text = text
    method text = text
    method set_text x =
      text <- x

    method min_size = (10 * String.length text, 10)

    method execute await _ =
      await#forever

    method render geom =
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.blank in
      Raylib.image_draw_text (Raylib.addr img) text 0 0 14 Raylib.Color.black;

      let tex = Raylib.load_texture_from_image img in
      Raylib.unload_image img;
      [geom, tex]

  end
