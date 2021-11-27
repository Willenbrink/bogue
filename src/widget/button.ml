(** a clickable button *)
(* TODO click on an image *)

(* TODO in case of Switch, dim the label when not selected *)
(* ==> label_on, label_off ? *)

open Utils

open Base

let color_on = Draw.find_color Theme.button_color_on
let color_off = Draw.find_color Theme.button_color_off

let bg_on = Style.color_bg (Draw.opaque color_on)
let bg_off = Style.color_bg (Draw.opaque color_off)

class t ?(switch = false) ?(size = (0,0) (* TODO give sensible default *)) ?border_r ?border_c ?fg ?(bg_on = bg_on) ?(bg_off = bg_off) ?bg_over
    ?label ?label_on ?label_off ?(state = false) text =
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
  let border_on, border_off = match border_c, border_r with
    | None, None -> None, None
    | None, Some radius ->
      Some Style.(border ~radius (line ~color:(Style.get_color bg_on) () )),
      Some Style.(border ~radius (line ~color:(Style.get_color bg_off) () ))
    | _ ->
      let s = Style.(border ?radius:border_r (line ?color:border_c ())) in
      Some s, Some s
  in

  let button_margin = 6 in (* logical size - TODO theme this var ? *)
  let bm = Theme.scale_int button_margin in

  object (self)
    inherit w size "Button" Cursor.Hand

    val label_on = label_on
    val label_off = label_off
    val mutable mouse_over = false
    val box_on = new Box.t ~bg:bg_on ?border:border_on ()
    val box_off = new Box.t ~bg:bg_off ?border:border_off ()
    val box_over = map_option bg_over (fun bg -> new Box.t ~bg ())

    val mutable state = state
    method state = state
    method set_state x = state <- x

    method press = if switch then self#set_state (not self#state) else self#set_state true
    method release = if not switch then self#set_state false

    method! unload =
      label_on#unload;
      label_off#unload;
      box_on#unload;
      box_off#unload;
      do_option box_over (fun o -> o#unload)

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


    method display canvas layer g =
      (* For safety (?), if the size is too small, the check icon is not clipped (see
         [display] below). *)
      let resize (w,h) b =
        let size = w - 2*button_margin, h - 2*button_margin in
        List.iter (fun x -> x#resize size) [label_on; label_off];
        List.iter (fun x -> x#resize (w,h)) [box_on; box_off];
        do_option box_over (fun x -> x#resize (w,h))
      in

      let (dx,dy) = if self#state then (0, 1) else (0, 0) in
      let box = if self#state
        then box_on
        else if mouse_over
        then default box_over box_off
        else box_off
      in
      (*let margin = if self#state b then 0 else button_margin in*)
      (*  Draw.box canvas.Draw.renderer ~bg (x+margin) (y+margin) (w-2*margin) (h-2*margin); *)
      let box_blit = box#display canvas layer
          Draw.( { x = g.x (* + margin *);
                   y = g.y (* + margin *);
                   w = g.w;
                   h = g.h;
                   voffset = g.voffset } ) in
      let label_blit =
        (if self#state then label_on else label_off)#display canvas layer
          Draw.( { x = g.x + bm + dx;
                   y = g.y + bm + dy;
                   w = g.w - 2*bm;
                   h = g.h - 2*bm;
                   voffset = g.voffset } )
      in List.concat [box_blit; label_blit]

    initializer
      connect_main self ~target:self (fun _ -> self#press) Trigger.buttons_down;
      connect_main self ~target:self (fun _ -> self#release) Trigger.buttons_up;
      connect_main self ~target:self (fun _ -> self#mouse_enter) [Trigger.mouse_enter];
      connect_main self ~target:self (fun _ -> self#mouse_leave) [Trigger.mouse_leave];

  end
