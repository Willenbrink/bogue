open Base

(* TODO consider this problem. How can we get single value precision without complicating
   everything significantly? *)
(* this function can be used for the ~t_to function to slow down the slider when
   the range of values is big (bigger than the number of pixels of the slider).
   When the user first move the slider, the slope will be 1 (1 pixel => 1 step),
   and then of course the slope becomes higher in order to catch up.  The price
   to pay is that the slider position has to be corrected when mouse_button is
   up. And the function has to be changed for each new starting value x0.  *)
(* x and x0 are between 0 and 1. Range of values is [0,M]. This is only needed
   when M>1 *)
(* k>=2 is the nonlinearity, it has to be even. k=2 should be enough *)
(* TODO modify the formula to allow k odd *)
(* let slow k m x0 x = *)
(*   if x >= x0 *)
(*   then if (float k) *. (1. -. x0) *. m >= float (k-1) (\* we have to slow down *\) *)
(*     then let t = (x -. x0) /. (1. -. x0) in *)
(*       (m -. 1. -. x0 *. m) *. (Float.pow t (float_of_int k)) +. t +. x0 *. m *)
(*     else x *. m (\* just linear *\) *)
(*   else *)
(*   if (float k) *. m *. x0 >= float (k-1) (\* we have to slow down *\) *)
(*   then let t = (x -. x0) /. x0 in *)
(*     (1. -. x0 *. m) *. (Float.pow t (float_of_int k)) +. t +. x0 *. m *)
(*   else x *. m *)

class t ?(init = 0) ?(length = 200) ?(live = false)
    ?(thickness = 20) ?w ?h ?(tick_size = 50) ?(max = 100) ?(step = Stdlib.max 1 (max / 100)) () =
  (* Slider can take values from 0 to max, both included. Must be non zero. *)
  let () = assert (max > 0) in

  object (self)
    inherit [int] w "Slider" Cursor.Hand
    inherit [int] stateful init

    method min_size =
      (200, 20)

    val mutable thickness = thickness(* in pixels *)
    val mutable tick_size = tick_size(* in pixel Size of the handle *)
    method tick_size = tick_size
    method set_tick_size x =
      tick_size <- Stdlib.max 25 x

    (* we store here the room position (unscaled) *)
    val mutable room_x = 0
    val mutable room_y = 0

    val mutable key_speed = 1.

    (* Compute the pre-value (in principle between 0 and max, but sometimes can be
       outside if the tick is large) from the mouse position *)
    method compute_value (x,y) offset =
      let w,h = self#min_size in
      if tick_size = w then 0 (* the value should be undefined here *)
      else offset + (max * (x - tick_size/2 - room_x)) / (w - tick_size)

    (* This should be called on mouse_button_down. *)
    (* If the click is over the tick, we do *not* change value: this is the standard
       behavious in most GUIs, and is a good idea imho. This requires storing an
       offset. *)
    method click pos =
      let mouse_v = self#compute_value pos 0 in
      let v =
        if abs (mouse_v - state) * (length - tick_size) <= max * tick_size/2
        then state
        else Stdlib.max 0 (min mouse_v max)
      in
      state <- v;
      state - mouse_v

    (* This should be called on key_down. *)
    method receive_key k =
      let increase () = state <- (min max (state + step)) in
      let decrease () = state <- (Stdlib.max 0 (state - step)) in
      let open GLFW in
      match k with
      | Left | Down -> decrease ()
      | Right | Up -> increase ()
      | _ -> ()

    method execute await yield =
      await#f [`Mouse_press] @@ function
      | Mouse_press (pos,Event.LMB), _ ->
        let offset = self#click pos in
        begin
          await#f [`Mouse_release; `Mouse_motion; `Key_press] @@ function
          | Mouse_release (_,_),_ -> ()
          | Mouse_motion pos,_ ->
            let v = self#compute_value pos offset in
            let v = (Stdlib.max 0 (min v max)) in
            state <- v;
            if live then yield state;
            raise Repeat
          | Key_press (k,[]),_ -> self#receive_key k
        end;
        yield state;
        self#execute await yield

    method private x_pos =
      room_x + state * (fst self#min_size - tick_size) / max

    method private y_pos =
      room_y + fst self#min_size - tick_size - state * (fst self#min_size - tick_size) / max

    method render geom =
      room_x <- geom.x;
      room_y <- geom.y;

      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.gray in
      Raylib.image_draw_rectangle (Raylib.addr img) self#x_pos geom.y tick_size thickness Raylib.Color.lightgray;
      let tex_ray = Raylib.load_texture_from_image img in
      Raylib.unload_image img;

      [geom, tex_ray]
  end
