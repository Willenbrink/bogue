(** a clickable button *)
(* TODO click on an image *)

(* TODO in case of Switch, dim the label when not selected *)
(* ==> label_on, label_off ? *)

open Base
open Utils

let color_on = Draw.find_color Theme.button_color_on
let color_off = Draw.find_color Theme.button_color_off

let bg_on = Style.color_bg (Draw.opaque color_on)
let bg_off = Style.color_bg (Draw.opaque color_off)

class t ?(switch = false) ?(size = (0,0) (* TODO give sensible default *)) ?border_r ?border_c ?fg ?(bg_on = bg_on) ?(bg_off = bg_off) ?bg_over
    ?label ?label_on ?label_off ?(init = false) text =
  let label_on, label_off = match label, label_on, label_off with
    | None, None, None -> let l = new Label.t ?fg text in l,l
    | Some l, None, None -> l,l
    | None, _, _ ->
      default label_on (new Label.t ?fg text),
      default label_off (new Label.t ?fg text)
    | _ -> printd debug_warning
             "label argument was ignored because label_on and/or \
              label_off was provided";
      default label_on (new Label.t ?fg text),
      default label_off (new Label.t ?fg text) in

  let button_margin = 6 in (* logical size - TODO theme this var ? *)
  let bm = Theme.scale_int button_margin in

  object (self)
    inherit [bool] w size "Button" Cursor.Hand
    inherit [bool] stateful init

    val label_on = label_on
    val label_off = label_off
    val mutable mouse_over = false
    val box_on = new Box.t ()
    val box_off = new Box.t ()

    method release = if not switch then state <- false

    (* TODO button and switch should be two different classes.
       A button has essentially no state and yielding only true
       is meaningless and confusing. Instead it should have type unit (there is only
       one acceptable value after all!) *)
    method execute await' yield =
      let await ts h = await'#f (`Mouse_enter :: `Mouse_leave :: ts) @@ function
        | Mouse_enter _,_ -> mouse_over <- true; raise Repeat
        | Mouse_leave _,_ -> mouse_over <- false; raise Repeat
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

    method! size =
      let (w,h) = label_on#size in
      let (w',h') = label_off#size in
      let w = imax w w' and h = imax h h' in
      (w + 2*button_margin, h + 2*button_margin)

    method display g =
      let dx,dy = if self#state then (0, 1) else (0, 0) in
      let box = if self#state
        then box_on
        else box_off
      in
      (*let margin = if self#state b then 0 else button_margin in*)
      (*  Draw.box canvas.Draw.renderer ~bg (x+margin) (y+margin) (w-2*margin) (h-2*margin); *)
      let box_blit_ray = box#display
          Draw.( { x = g.x (* + margin *);
                   y = g.y (* + margin *);
                   w = g.w;
                   h = g.h;
                   voffset = g.voffset } ) in
      let label_blit_ray =
        (if self#state then label_on else label_off)#display
          Draw.( { x = g.x + bm + dx;
                   y = g.y + bm + dy;
                   w = g.w - 2*bm;
                   h = g.h - 2*bm;
                   voffset = g.voffset } )
      in
      List.concat [box_blit_ray; label_blit_ray]
  end
