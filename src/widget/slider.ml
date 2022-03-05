open Utils

open Base

type kind =
  | Horizontal (* Horizontal bar with a small slider; values increase from left
                  to right. No background *)
  | HBar (* Horizontal bar filled up to the value *)
  | Vertical (* Warning: values increase from bottom to top *)
  | Circular (* Origin is on the positive real axi *)

let check_max m =
  if m <= 0
  then (printd debug_error "Max value of slider must be positive, not %i." m; 1)
  else m

let make_box_blit ~dst ?(shadow=true) ~focus voffset canvas layer box =
  (* Let's see if it is nice to add a "shadow" to the tick *)
  let box_blit = Draw.make_blit ~voffset ~dst canvas layer box in
  if shadow && focus then
    let shadow_blits = Draw.box_shadow ~offset:(0,0) ~size:(Theme.scale_int 6)
        canvas layer dst in
    List.rev (box_blit :: shadow_blits)
  else [box_blit]

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
let slow k m x0 x =
  if x >= x0
  then if (float k) *. (1. -. x0) *. m >= float (k-1) (* we have to slow down *)
    then let t = (x -. x0) /. (1. -. x0) in
      (m -. 1. -. x0 *. m) *. (Float.pow t (float_of_int k)) +. t +. x0 *. m
    else x *. m (* just linear *)
  else
  if (float k) *. m *. x0 >= float (k-1) (* we have to slow down *)
  then let t = (x -. x0) /. x0 in
    (1. -. x0 *. m) *. (Float.pow t (float_of_int k)) +. t +. x0 *. m
  else x *. m

(* value is ignored if a var is provided *)
class t ?(priority = Main) ?step ?(kind = Horizontal) ?(init = 0) ?(length = 200)
    ?(thickness = 20) ?w ?h ?tick_size ?var ?(m = 100) () =
  let tick_size = default tick_size (match kind with
      | HBar -> 4
      | _ -> 50) in
  let size = match kind with
    | HBar
    | Horizontal
    | Circular -> default w length, default h thickness
    | Vertical -> default w thickness, default h length in
  let thickness = match kind with
    | HBar
    | Horizontal -> snd size
    | Vertical -> fst size
    | Circular -> thickness in
  let step = default step (max 1 (m/100)) in
  object (self)
    inherit [int] w size "Slider" Cursor.Hand as super
    inherit [int] stateful init

    val mutable pointer_motion = false

    (* This has to be done for each external call to this module *)
    (* It will update the position of the slider by looking at the var. Hence, if
       the var has a nontrivial transformation, it might be that the value differs
       from the value initially computed from the mouse position. See example 34. *)
    val mutable var = default var
        (Tvar.create (Avar.var init) ~t_from:(Avar.get) ~t_to:(Avar.var))
    method set_state x = state <- x; Tvar.set var x
    (* method! state = Tvar.get var *)

    method set_var x = var <- x

    (* Slider can take values from 0 to max, both included. Must be non zero. *)
    val mutable max = check_max m
    method max = max
    method set_max x = max <- check_max x

    val mutable clicked_value : int option = None
    method clicked_value = clicked_value

    (* If offset=0, the tick will place itself with the mouse pointer exactly in
       its middle point. Offset is used to not move the tick if one clicks at
       any other position of the tick. *)
    val mutable offset = 0
    val step = step
    val mutable thickness = thickness(* in pixels *)
    val mutable tick_size = tick_size(* in pixel Size of the handle *)
    method tick_size = tick_size
    method set_tick_size x =
      let min_tick_size =
        match kind with
        | HBar -> 4
        | Horizontal -> 25
        | Vertical -> 20
        | Circular -> 15
      in
      tick_size <- imax min_tick_size x

    val kind = kind
    (* we store here the room position (unscaled) *)
    val mutable room_x = 0
    val mutable room_y = 0

    (* we need to replicate here the keyboard_focus field of the layout, because
       we use it to render the widget differently if it has keyboard_focu It
       acts similarly as the .active field of Text_input. It is set by
       Widget.set_keyboard_focu *)
    val mutable keyboard_focus = false
    method keyboard_focus = keyboard_focus
    method focus = keyboard_focus <- true
    method unfocus = keyboard_focus <- false

    val mutable key_speed = 1.
    val mutable key_time = Time.now ()
    (* TODO render is only used for circular. Otherwise all textures are created and
       destroyed on the fly. Change this ? *)
    val mutable render : Draw.texture option = None

    method length =
      let w,h = self#size in
      match kind with
      | HBar
      | Horizontal -> w
      | Circular -> imin w h
      | Vertical -> h

    method unload =
      match render with
      | None -> ()
      | Some tex -> begin
          Draw.forget_texture tex;
          render <- None
        end

    (* events *)

    (* Compute the pre-value (in principle between 0 and max, but sometimes can be
       outside if the tick is large) from the mouse position *)
    method compute_value ev =
      let w,h = size in
      let x,y = Mouse.pointer_pos ev in
      let v = match kind with
        | Horizontal ->
          if tick_size = w then 0 (* the value should be undefined here *)
          else offset + (max * (x - tick_size/2 - (room_x))) / (w - tick_size)
        | HBar ->
          (max * (x - (room_x))) / w
        | Vertical ->
          if tick_size = h then 0 (* undefined *)
          else offset + max - (max * (y - tick_size/2 - (room_y))) / (h - tick_size)
        | Circular ->
          let x0 = room_x + w/2 in
          let y0 = room_y + h/2 in
          if x = x0 then if y>y0 then 3 * max / 4 else max / 4
          else
            let a = (float max) *. atan (float (y0-y) /. (float (x-x0))) /. pi /. 2. in
            let a' = if x > x0 then if y <= y0 then a else a +. (float max)
              else a +. (float  max) /. 2. in
            round a' in
      (* printd debug_custom "Mouse (%d,%d), value=%d" x y v; *)
      v

    (* This should be called on mouse_button_down. *)
    (* If the click is over the tick, we do *not* change value: this is the standard
       behavious in most GUIs, and is a good idea imho. This requires storing an
       offset. *)
    method click ev =
      if !debug then assert (offset = 0);
      (* in some fast mouse motion it can happen that button_up is lost, so this
         assertion fail *)
      let old = state in
      let mouse_v = self#compute_value ev in
      let v =
        if abs (mouse_v - old) * (length - tick_size) <= max * tick_size/2
        then begin (* test à revoir: mouse over tick *)
          (* printd debug_custom "OVER TICK"; *)
          offset <- (old - mouse_v);
          old
        end
        else (Stdlib.max 0 (min mouse_v max)) in
      printd debug_board "Slider value : %d" v;
      clicked_value <- (Some v);
      (* keyboard_focus <- true; *)
      state <- v
    (* we add an animation to the original Avar. For this we need some
       gymnastic to get the current and final value for it *)
    (* TODO this works only for scrolling, because has_anim is detected for
       scrolling.  Otherwise, has_anim does not detect this animation yet *)
    (* let avar = var.Tvar.var in *)
    (* let final = Avar.get (var.Tvar.t_to v) in *)
    (* avar <- (Avar.fromto (Avar.get (avar)) final) *)

    (* this should be called on mouse_button_up: *)
    (* not necessary anymore, since keyboard_focus is treated by the main loop *)
    (* let click_focus s ev = *)
    (*  if Sdl.Event.(get ev mouse_button_state) = Sdl.pressed *)
    (* (* = DIRTY trick, see bogue.ml *) *)
    (*  then keyboard_focus <- true *)

    (* This should be called on mouse_button_up: *)
    method release =
      clicked_value <- None;
      pointer_motion <- false;
      offset <- 0

    (* on mouse motion: *)
    method slide ev =
      let v = self#compute_value ev in
      let v = (Stdlib.max 0 (min v max)) in
      printd debug_board "Slider value : %d" v;
      pointer_motion <- true;
      state <- v

    (* Use this to increase the step when using keyboard. *)
    method change_speed =
      let t = Time.now () in
      if Time.(t - (key_time)) > 200
      (* delay too long, we return to initial speed. TODO: check that this is bigger
         than system delay between two key repeats, otherwise this will always
         apply *)
      then key_speed <- 1.
      else key_speed <- (key_speed *. 1.1);
      key_time <- t;
      let step = step * (round (key_speed)) in
      step

    method increase =
      let step = self#change_speed in
      state <- (min max (state + step))

    method decrease =
      let step = self#change_speed in
      state <- (Stdlib.max 0 (state - step))


    (* This should be called on key_down. *)
    method receive_key ev =
      (* TODO: we could use some animation to make it smoother *)
      if self#keyboard_focus then
        begin
          match Trigger.event_kind ev with
          | `Key_down ->
            (match Sdl.Event.(get ev keyboard_keycode) with
             | c when c = Sdl.K.left -> self#decrease
             | c when c = Sdl.K.down -> self#decrease
             | c when c = Sdl.K.right -> self#increase
             | c when c = Sdl.K.up -> self#increase
             | c -> (printd debug_event "==> Key down event discarded.";
                     printd debug_event "Key=[%s], mod=%u, Keycode:%u" (Sdl.get_key_name c) (Sdl.get_mod_state ()) c))
          | _ -> printd debug_event "Warning: Event should not happen here"
        end

    method execute =
      await Trigger.(buttons_down @ buttons_up @ pointer_motion @ [key_down]) @@ fun (ev,_) ->
      if Trigger.(match of_event ev with
          | x when List.mem x buttons_down -> self#click ev; false
          | x when List.mem x buttons_up -> self#release; true
          | x when List.mem x pointer_motion ->
            if mm_pressed ev
            || event_kind ev = `Finger_motion
            then begin
              self#slide ev;
              self#update;
              true
            end else false
          | x when List.mem x [key_down] -> self#receive_key ev; false
          | _ -> assert false
        )
      then self#state
      else self#execute

    method! focus_with_keyboard = self#focus

    method! remove_keyboard_focus = self#unfocus

    method! guess_unset_keyboard_focus = false

    method! resize (w,h) =
      super#resize (w,h);
      thickness <- (match kind with
          | HBar | Horizontal -> h
          | Vertical -> w
          | Circular -> thickness)

    method private x_pos =
      room_x + state * (self#length - tick_size) / max

    method private y_pos =
      room_y + self#length - tick_size - state
                                         * (self#length - tick_size) / max

    method display canvas layer g =
      (* We use y_pos before updating to display a gradient box at the real mouse
         position, in case of non-linear (vertical) slider (see example 34)...  TODO
         do the same for Horizontal slider *)
      let oldy = self#y_pos in
      let scale = Theme.scale_int in
      let tick_size = scale tick_size
      and thickness = scale thickness in
      let open Draw in
      let renderer = canvas.renderer in
      let gx = Theme.unscale_int g.x
      and gy = Theme.unscale_int g.y in
      if room_x <> gx then room_x <- gx;
      if room_y <> gy then room_y <- gy;
      let focus = self#keyboard_focus in
      let shadow = true (* for testing *) in
      let c = if shadow then opaque Button.color_on
        else set_alpha 200 Button.color_on in
      let color = if self#keyboard_focus && not shadow
        then Draw.(darker c)
        else c in
      let x0 = scale (self#x_pos) in
      (*   set_color renderer (opaque color); *)
      match kind with
      | Horizontal ->
        (* let rect = Sdl.Rect.create ~x:x0 ~y:g.y ~w:thickness ~h:width in *)
        (* go (Sdl.render_fill_rect renderer (Some rect)); *)
        let box = texture canvas.renderer ~color ~w:tick_size ~h:thickness in
        let dst = Sdl.Rect.create ~x:x0 ~y:g.y ~w:tick_size ~h:thickness in
        forget_texture box; (* or save ? but be careful color may change *)
        make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
      | HBar ->
        (* horizontal gradient for the slider *)
        let colors = [opaque Button.color_on; opaque Button.color_off] in
        let box = gradient_texture canvas.renderer ~w:(x0 - g.x + tick_size)
            ~h:thickness ~angle:90. colors in
        let dst = Sdl.Rect.create ~x:g.x ~y:g.y ~w:(x0 - g.x + tick_size)
            ~h:thickness in
        forget_texture box; (* or save ? *)
        make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
      (* [make_blit ~voffset:g.voffset ~dst canvas layer box] *)
      | Vertical ->
        let y0 = scale (self#y_pos) in
        let dy = scale oldy - y0 in
        let y = imin y0 (y0 + dy) in
        let h = imax tick_size (abs dy) in (* see example 34 .*)
        let box = if abs dy <= 3 || not pointer_motion
        (* the 3 is completely heuristic. See example 35. Ideally we want
           0. *)
          then texture canvas.renderer ~color ~h ~w:thickness
          else let colors = [opaque Button.color_on;
                             opaque Button.color_off] in
            (* let _ = print_endline (Printf.sprintf "dy = %i" dy) in *)
            let colors = if dy < 0 then colors else List.rev colors in
            gradient_texture canvas.renderer ~h ~w:thickness colors in
        let dst = Sdl.Rect.create ~x:g.x ~y ~h ~w:thickness in
        forget_texture box; (* or save ? *)
        make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
      | Circular ->
        let radius = (imin g.w g.h)/2 - 2 in
        let tex = match render with
          | Some t -> t
          | None ->
            let t' = ring_tex renderer ~color:(lighter (transp grey))
                ~radius ~width:thickness (g.w/2) (g.h/2) in
            (* j'ai essayé de mettre une taille double puis de réduire avec
               render_copy, mais apparemment ça ne fait pas d'antialiasing *)
            (* let t' = convolution ~emboss:false renderer *)
            (*            (gaussian_blur ~radius:3) 3 t in *)
            render <- (Some t'); t' in
        let w',h' = tex_size tex in
        let dst = Sdl.Rect.create ~x:(g.x) ~y:(g.y) ~w:w' ~h:h' in
        (* go (Sdl.render_copy ~dst renderer tex); *)
        let sbox = make_blit ~voffset:g.voffset ~dst canvas layer tex in
        (* ring renderer ~bg:(lighter (opaque grey)) ~radius:(w/2-2)
           ~width (x+w/2) (y+h/2); *)
        let tick = ray_to_layer canvas layer ~voffset:g.voffset ~bg:color
            ~thickness:tick_size
            ~angle:(360. *. (float (max - state)) /. (float max)) ~radius
            ~width:thickness (g.x + g.w/2) (g.y + g.h/2) in
        [sbox; tick]

    (* TODO *)
    (* create a slider with a simple Tvar that executes an action each time the
       local value of the slider is modified by the slider *)
    (* let create_with_action ?step ?kind ~value ?length ?thickness ?tick_size *)
    (*     ~action max = *)
    (*   let v = Var.create (Avar.var value) in *)
    (*   let t_from a = Avar.get a in *)
    (*   let t_to x = action x; Avar.var x in *)
    (*   let var = Tvar.create v ~t_from ~t_to in *)
    (*   create ?step ?kind ~var ?length ?thickness ?tick_size max *)

    (* use ~lock if the user is not authorized to slide *)
    initializer
      let w = self in
      connect_main w ~target:w w#click Trigger.buttons_down;
      let on_release _ = w#release in
      connect_main w ~target:w on_release Trigger.buttons_up;
      let slide ev =
        if Trigger.mm_pressed ev || Trigger.event_kind ev = `Finger_motion
        then (w#slide ev; w#update)
      in
      connect ~priority w slide Trigger.pointer_motion;
      connect ~priority w ~target:w w#receive_key [Sdl.Event.key_down];
  end


(* FIXME might clash with normal initializer *)
(* create a slider with a simple Tvar that executes an action each time the
   local value of the slider is modified by the slider *)
let slider_with_action ?priority ?step ?kind ~value ?length ?thickness ?tick_size
    ~action max =
  let v = Avar.var value in
  let t_from a = Avar.get a in
  let t_to x = action x; Avar.var x in
  let var = Tvar.create v ~t_from ~t_to in
  new t ?priority ?step ?kind ~var ?length ?thickness ?tick_size max
