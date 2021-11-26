(** an interactive window to select the Debug level *)
open Tsdl
open Utils
module W = Widget
module L = Layout

let is_set code =
  code land !debug_code <> 0;;

let toggle code =
  debug_code := !debug_code lxor code;;

let set code b =
  if b then debug_code := !debug_code lor code
  else debug_code := !debug_code land (lnot code);;

let create () =
  let save_layer = Draw.get_current_layer () in
  Draw.use_new_layer (); (* TODO this should be saved to the window, not global, otherwise layouts that are created after this but in an older window will be drawn on this layer, and thus not shown at all... *)
  let b = W.check_box ~state:!W.draw_boxes () in
  let l = new Label.t "Turn on debug rectangles" in
  let dbg_boxes = L.flat_of_w ~align:Draw.Center W.[(b :> W.t); (l :> W.t)] in

  let action w _ _ =
    W.draw_boxes := w#get_state in
  (* let action _ _ _ = () in
   * let c_boxes = W.connect b b action [Sdl.Event.mouse_button_down; Sdl.Event.finger_down] in *)

  let b = W.check_box ~state:!debug () in
  let l = new Label.t "Turn on debugging trace" in
  let dbg_button = L.flat_of_w ~align:Draw.Center [(b :> W.t); (l :> W.t)] in

  let title = new Label.t "Debug Variables" in
  let action code w ev =
    set code w#state
  in
  let rec loop vars rooms connections =
    match vars with
    | [] -> rooms, connections
    | (var,code)::rest ->
      let bb = W.check_box ~state:(is_set code) () in
      let ll = new Label.t var in
      let btn = L.flat_of_w ~sep:0 W.[(bb :> W.t); (ll :> W.t)] in
      let c = W.connect bb ~target:bb (action code bb) [Sdl.Event.mouse_button_down] in
      loop rest (btn :: rooms) (c :: connections) in

  let rooms, connections = loop debug_vars [] [] in

  let panel = L.tower ~sep:0 ((L.flat_of_w ~sep:10 [(title :> W.t)]) :: rooms) in
  let action w ev =
    let ok = w#state in
    debug := ok;
    if ok
    then (L.show panel; L.fade_in panel)
    else (L.hide panel; L.fade_out panel)  in
  W.connect b ~target:b (action b) [Sdl.Event.mouse_button_down];

  (* List.iter (fun c -> let src = c.source in W.(add_connection src c)) (c_boxes :: c :: connections); *)
  panel#set_show !debug;
  let layout = L.tower ~sep:0 [dbg_boxes; dbg_button; panel] in
  Draw.set_current_layer save_layer; (* DEBUG: not here ! *)
  layout;;

(* let _ = *)
(*   let layout = create () in *)
(*   let board = make [] [layout] in *)
(*   run board; *)
(*   Draw.quit;; *)
