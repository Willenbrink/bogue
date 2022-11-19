open Base

class t ?(init = false) () =
  object (self)
    inherit [bool] w "Check" Cursor.Hand
    inherit [bool] stateful init

    method set_state x = state <- x

    method min_size =
      (20, 20)

    method execute await yield =
      await#f [`Mouse_press] @@ function
      | Mouse_press _, _ ->
        self#set_state @@ not self#state;
        yield self#state;
        self#execute await yield

    method render geom =
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.gray in
      Raylib.image_draw_circle (Raylib.addr img) 10 10 5 Raylib.Color.black;
      let tex_ray = Raylib.load_texture_from_image img in

      [geom, tex_ray]
  end
