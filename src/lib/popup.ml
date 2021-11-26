(** put a sublayout on top of the main layout *)
(* recall that "on top" means "insert_after" layer; change ? *)

(* TODO implement the resize function *)

open Utils

let new_layer_above base =
  printd debug_graphics "Create new layer";
  Chain.insert_after base (Draw.new_layer ())

(* search top_layer inside the layout. *)
(* use the global toplayer instead (Chain.last) or at least the top_layer of the
   whole connected component ? (top_layer (Layout.top_house layout)). In fact
   since the top_house is supposed to contain all the graphics of the window,
   and there is one layer chain per window, the two choices should give the same
   answer. Thus it's better to use Chain.last layer. OK see below ... *)
let rec top_layer layout =
  let open Layout in
  match layout#content with
  | Leaf _ -> layout#layer
  | List r -> match list_max Chain.compare (List.map top_layer r) with
    | None -> printd debug_error "Error: there should be a top layer"; layout#layer
    | Some l -> l

let global_top_layer layout : Draw.layer =
  Chain.last (layout#layer)

(* Register a resize function that will follow (size AND position) the model
   layout. For the position to work correctly, both layouts must be in the same
   house. *)
let resize_same_as model room =
  let resize _ =
    let open Layout in
    match model#house, room#house with
    | Some h1, Some h2 when Layout.equal h1 h2 ->
      let keep_resize = true in
      let size = get_size model in
      let x = xpos model in
      let y = ypos model in
      setx ~keep_resize room x;
      sety ~keep_resize room y;
      set_size ~keep_resize room size
    | _ -> printd debug_error
             "[resize_same_as] must apply to two rooms in the same house. Maybe \
              you should use [Layout.resize_follow_house]."
  in
  room#set_resize resize

(* create a box of the dimension of the layout *)
let filter_screen ?color ?layer ?keyboard_focus layout =
  let w,h = Layout.(width layout, height layout) in
  printd debug_graphics "Create filter screen (%d,%d)" w h;
  let b = match color with
    | None -> (new Empty.t (w,h))
    | Some color -> (new Box.t ~size:(w,h) ~bg:(Style.Solid color)() :> Widget.t) in
  let screen = (* Layout.(flat_of_w ~sep:0 layout#canvas [b]) in *)
    Layout.(resident ~name:"filter" ?canvas:layout#canvas b) in
  (* Layout.(screen.geometry <- {screen.geometry with w; h}); *)
  do_option layer (Layout.set_layer screen);
  screen#set_keyboard_focus keyboard_focus;
  screen

(** add a screen on top of the layout. This can be useful to make the whole
    layout clickable as a whole. To make sure this works as expected, it should
    be called dynamically and not statically before running the board, because if
    other layers are created afterwards, the screen might endup not being on top
    of everything. *)
let add_screen ?(color = Draw.(transp red) (* DEBUG *) ) layout =
  let base_layer = top_layer layout in
  let screen_layer = new_layer_above base_layer in
  let screen = filter_screen ~color ~layer:screen_layer layout in
  Layout.add_room ~dst:layout screen;
  Layout.resize_follow_house screen;
  screen

(* TODO add dx dy *)
let attach_on_top ?(dx=0) ?(dy=0) house layout =
  let open Layout in
  setx layout (getx layout + dx);
  sety layout (gety layout + dy);
  global_set_layer layout (global_top_layer house);
  add_room ~dst:house layout;
  resize_same_as house layout


(** add two layers on top of the house: one for the screen to hide the house,
    one for the layout on top of the screen. Return the screen. *)
(* TODO: use add_screen to reduce code *)
let attach ?bg ?(show=true) house layout =
  let base_layer = global_top_layer house in  (* eg. 10 *)
  let filter_layer = new_layer_above base_layer in (* eg. 20 *)
  let top_layer = new_layer_above filter_layer in  (* eg. 30 *)
  (* We change layer for layout and all its children: *)
  Layout.global_set_layer layout top_layer;
  let screen = filter_screen ?color:bg house in
  Layout.set_layer screen filter_layer;
  Layout.add_room ~dst:house screen;
  Layout.scale_resize screen;
  Layout.add_room ~halign:Draw.Center ~valign:Draw.Center ~dst:house layout;
  Layout.scale_resize layout;
  screen#set_show show;
  layout#set_show show;
  Trigger.push_keyboard_focus (screen#id);
  Trigger.push_mouse_focus (screen#id); (* redundant *)
  (* When inserting new elements on the fly, one needs to ask to mouse to
     refresh its focus, see b_main.ml. *)
  screen


(* some predefined popup designs *)

let slide_in ~dst content buttons =
  let border = Style.(border (line ~color:Draw.(opaque grey) ())) in
  let shadow = Style.shadow () in
  let background = Layout.Box
      (new Box.t ~shadow
        ~bg:(Style.Solid Draw.(opaque (pale grey)))
        ~border ()) in
  let popup = Layout.tower ~align:Draw.Center ~background [content; buttons] in
  let screen = attach ~bg:(Draw.(set_alpha 200 (pale grey))) dst popup in
  (* Layout.slide_in ~dst popup; *)
  popup, screen

let one_button ?size ~button ~dst content =
  let close_btn = new Button.t ~border_r:3 button in
  let w,h = match size with None -> None,None | Some (w,h) -> (Some w, Some h) in
  let popup, screen = slide_in ~dst content (Layout.resident ?w ?h ((close_btn :> Widget.t))) in
  let close _ =
    Layout.hide popup;
    Layout.hide screen;
    Layout.fade_out screen in
  Widget.on_release ~release:close (close_btn :> Widget.t)

(* a text and a close button. *)
(* TODO the ?w and ?h define the size of the text_display (not automatically
   detected). It should also include the size of the close button *)
let info ?size ?(button="Close") text dst =
  let td = ((new Text_display.t ?size (Text_display.paragraphs_of_string text)) :> Widget.t)
           |> Layout.resident in
  one_button ?size ~button ~dst td

(* ?w and ?h to specify a common size for both buttons *)
let two_buttons ?size ~label1 ~label2 ~action1 ~action2
    content dst =
  let w,h = match size with None -> None,None | Some (w,h) -> (Some w, Some h) in
  let btn1 = new Button.t ~border_r:3 label1 in
  let btn2 = new Button.t ~border_r:3 label2 in
  let buttons = Layout.(flat ~vmargin:0 ~sep:(2*Theme.room_margin)
                          [resident ?w ?h ((btn1 :> Widget.t)); resident ?w ?h ((btn2 :> Widget.t))]) in
  let popup, screen = slide_in ~dst content buttons in
  let close () =
    (*Layout.hide popup;*)
    Layout.fade_out ~hide:true popup;
    (*Layout.hide screen*)
    Layout.fade_out ~hide:true screen in
  let do1 _ =
    close ();
    action1 () in
  let do2 _ =
    close ();
    action2 () in
  Widget.on_release ~release:do1 (btn1 :> Widget.t);
  Widget.on_release ~release:do2 (btn2 :> Widget.t)

let yesno ?size ?(yes="Yes") ?(no="No") ~yes_action ~no_action text dst =
  let td = ((new Text_display.t ?size (Text_display.paragraphs_of_string text)) :> Widget.t)
           |> Layout.resident in
  two_buttons ?size ~label1:yes ~label2:no ~action1:yes_action ~action2:no_action
    td dst


(* tooltips *)

(* tooltips are small popups which are displayed on a specified layout, close to
   a specified target. The target should be a room inside the layout. Tooltips
   don't have 'screens' like usual popups because they don't prevent the user to
   do anything. Tooltips are displayed when the "mouse_at_rest" event is
   triggered to the specified widget, and removed when the "mouse_leave" event is
   triggered. *)

(* the content of the tooltips is usually a simple text, but it can be any
   layout *)

type position =
  | LeftOf
  | RightOf
  | Above
  | Below
  | Mouse

let tooltip ?background ?(position = Below) text ~target widget layout =
  let t = new Label.t ~font_size:Theme.small_font_size text in
  let border = Style.(border ~radius:5 (line ())) in
  let background = default background
      (Layout.Box (new Box.t ~bg:(Style.Solid Draw.(opaque (pale grey)))
                    ~border ())) in
  let tooltip = Layout.tower_of_w ~sep:3 ~background [(t :> Widget.t)] in
  attach_on_top layout tooltip;
  tooltip#set_show false;

  let show_tooltip _ =
    let open Layout in
    if not tooltip#show then begin
      let x,y = pos_from layout target in
      (* print_endline (Printf.sprintf "(%i,%i)" x y); *)
      let x',y' = match position with
        | Below -> x, y+(height target)+2
        | Above -> x, y-(height tooltip)-2
        | LeftOf -> x-(width tooltip)-2, y
        | RightOf -> x+(width target)+2, y
        | Mouse -> let x,y = Trigger.mouse_pos () in (x+8,y+8) in
      sety tooltip y';
      setx tooltip x';
      tooltip#set_show true;
      Layout.fade_in tooltip
    end in
  let to_show = ref true in
  let hide_tooltip b =
    to_show := false;
    ignore (Timeout.add 200 (fun () ->
        tooltip#set_show !to_show;
        Trigger.push_redraw b#id))
  in
  let enter _ =
    if tooltip#show
    then to_show := true in (* this amounts to cancelling the timeout, which is
                               what we want to do when we re-enter the target *)

  Widget.mouse_over ~enter ~leave:hide_tooltip widget;
  Widget.connect_main widget ~target:widget show_tooltip [Trigger.mouse_at_rest]
