open Base

class t ?(switch = false) ?border_r ?border_c
    ?label_on ?label_off ?(init = false) text =
  let label_on, label_off =
    let default = new Label.t text in
    Option.value ~default label_on,
    Option.value ~default label_off
  in

  let bm = 6 in (* logical size - TODO theme this var ? *)

  object (self)
    inherit [bool] w "Button" Cursor.Hand
    inherit [bool] stateful init

    val label_on = label_on
    val label_off = label_off
    val mutable mouse_over = false
    val box_on = new Box.t ()
    val box_off = new Box.t ()

    method release = if not switch then state <- false

    method min_size =
      let w_l, h_l =
        if state
        then label_on#min_size
        else label_off#min_size
      in
      let w_b, h_b =
        if state
        then box_on#min_size
        else box_off#min_size
      in
      (w_l + w_b, min h_l h_b)


    (* TODO button and switch should be two different classes.
       A button has essentially no state and yielding only true
       is meaningless and confusing. Instead it should have type unit (there is only
       one acceptable value after all!) *)
    method execute await' yield =
      let await ts h = await'#f (`Mouse_enter :: `Mouse_leave :: ts) @@ function
        | Mouse_enter, _ -> mouse_over <- true; raise Repeat
        | Mouse_leave, _ -> mouse_over <- false; raise Repeat
        | evg -> h evg
      in
      if switch
      then begin
        await [`Mouse_press] @@ function
        | Mouse_press (_, Event.LMB), _ ->
          state <- not state;
          yield state;
          self#execute await' yield
      end
      else begin
        (* TODO this may be a bit too much codegolf *)
        await (if state then [`Mouse_release] else [`Mouse_press]) @@ function
        | Mouse_release (_, Event.LMB), _
        | Mouse_press (_, Event.LMB), _ ->
          state <- not state;
          yield state;
          self#execute await' yield
      end

    method text =
      if self#state
      then label_on#text
      else label_off#text

    method set_text text =
      if self#state
      then label_on#set_text text
      else label_off#set_text text

    method mouse_enter = mouse_over <- true
    method mouse_leave = mouse_over <- false

    method render geom =
      let dx,dy = if self#state then (0, 1) else (0, 0) in
      let box = if self#state
        then box_on
        else box_off
      in
      (*let margin = if self#state b then 0 else button_margin in*)
      (*  box canvas.renderer ~bg (x+margin) (y+margin) (w-2*margin) (h-2*margin); *)
      let box_blit_ray = box#display
          { geom with
                   x = geom.x (* + margin *);
                   y = geom.y (* + margin *);
                   voffset = geom.voffset } in
      let label_blit_ray =
        (if self#state then label_on else label_off)#display
          { geom with
                   x = geom.x + bm + dx;
                   y = geom.y + bm + dy;
                   w = geom.w - 2*bm;
                   h = geom.h - 2*bm;
                   }
      in
      List.concat [box_blit_ray; label_blit_ray]
  end
