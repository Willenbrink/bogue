(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(** Vu Ngoc San, December 2013 -- now *)

(* doc on threads:
   http://ocamlunix.forge.ocamlcore.org/threads.html
*)

open Interop
open Interop.Utils

module E = Sdl.Event

type 'a board = {
  mutable window : 'a Layout.t;
  mutable mouse_focus: 'a Layout.t option;
  (* : the room containing the mouse. It must contain a Widget. *)
  mutable keyboard_focus: 'a Layout.t option;
  (* : the room with keyboard focus. It must contain a Widget *)
  (* It is important that keyboard focus may be different from mouse focus, cf
     example 12: one wants to be able to continue typing even when the mouse
     goes out of the text entry widget. *)
  mutable button_down: 'a Layout.t option;
  (* : the room where the button_down has been registered. Used to trigger
     full_click event *)
  mutable mouse_alive: bool;
  (* True as soon as the mouse has moved. Because SDL will report position 0,0
     when the window opens, but we dont want to activate a widget if it is
     located at 0,0...*)
}

let close_window_layout layout =
  printd debug_board "Closing layout...";
  if Sdl.is_text_input_active () then Sdl.stop_text_input ();
  (* DEBUG: clipboard sometimes causes problems *)
  (* if Sdl.has_clipboard_text ()  *)
  (* then begin let text = go(Sdl.get_clipboard_text ()) in *)
  (*   printd debug_warning "Clipboard has [%s]" text *)
  (* end; *)
  Layout.delete_textures layout;
  (* now we destroy the canvas (renderer and window): *)
  Draw.destroy_canvas (Layout.get_canvas layout);
  Layout.remove_canvas layout

(* call this to close everything. Don't use the layouts after this ! *)
(* However in principle you can run board again, and then the layouts are
   usable(?) *)
let exit_board board =
  close_window_layout board.window;
  board.mouse_focus <- None;
  board.keyboard_focus <- None;
  board.button_down <- None;
  Draw.destroy_textures (); (* en principe inutile: déjà fait *)
  (* Layout.clear_wtable (); *)
  Draw.check_memory ();
  Trigger.flush_all ();
  flush_log ()

let quit = Draw.quit

(** redisplay the layouts to the layers *)
(* there is no clear screen here *)
(* it will display a layout only if window.refresh = true *)
(* it should NOT be used more than once per loop (because of transparency) *)
let display board =
  (* We flush the events asking for redrawing. TODO: make it window-specific. *)
  Trigger.(flush redraw);
  if not board.window#is_fresh then Layout.render board.window

(** Render all layers and flip window buffers, only for windows with the
    is_fresh=false field *)
let flip ?clear board =
  Layout.flip ?clear board.window;
  Draw.destroy_textures ()

(** update window that was resized by user *)
let resize window =
  if window#size <> Layout.get_physical_size window
  then begin
    printd debug_graphics "Resize window (Layout #%u)" window#id;
    Layout.render window;
    Layout.flip window;
    window#set_fresh false;
    (* Window.set_fresh window; ?*)
    (* we remove all the window_event_exposed event: *)
    (* TODO: move this to the reaction to the exposed event only ? *)
    ignore (Trigger.filter_events (fun e -> E.(get e typ <> window_event || get e window_event_id <> window_event_exposed)));
    Thread.delay 0.1;
  end

(*************)
let show board =
  let w = board.window in
  Sdl.show_window (Layout.window w);
  w#set_fresh false;
  Draw.update_background (Layout.get_canvas w)

(* return the widget with mouse focus *)
let mouse_focus_widget board =
  Option.map (fun l -> l#content) board.mouse_focus

(* return the widget with keyboard_focus *)
let keyboard_focus_widget board =
  Option.map (fun l -> l#content) board.keyboard_focus

let button_down_widget board =
  Option.map (fun l -> l#content) board.button_down

(* which layout (ie window) has mouse focus ? *)
let layout_focus board =
  match Sdl.get_mouse_focus () with
  | None -> None
  | Some _ -> Some board.window

(* detect layout under mouse, with top layer (= largest "depth") *)
let check_mouse_focus board =
  if board.mouse_alive
  then let (x,y) = Mouse.pos () in
    printd debug_board "Mouse pos:(%u,%u)" x y;
    Option.bind (layout_focus board) (Layout.top_focus x y)
  else None

(* [check_mouse_motion] deals with sending the mouse_enter/mouse_leave events *)
(* The optional [target] argument can be used to specify the layout that should
   be considered as the new layout "under mouse", instead of really checking
   mouse position. Used for keyboard interaction. *)
(* This also sets the cursor. Overriding cursor is always possible by reacting
   to the mouse_leave/enter events. We try to keep at most one mouse_leave event
   and at most one mouse_enter event in the queue (however see remark in
   trigger.ml line 476). The rule is that this function is called only when
   there is no pending event, which means that no mouse_enter/leave event will
   be sent until the previous ones are dealt with.  Therefore it is NOT
   guaranteed that all widgets receive their due mouse_enter/leave events, in
   case many of them are triggered at the same time. The program should not rely
   on this.  *)
let check_mouse_motion ?target board =
  let open Layout in
  let mf = match target with
    | Some _ -> target
    | None -> check_mouse_focus board in
  match board.mouse_focus, mf with
  (* on compare l'ancien et le nouveau. See remarks in trigger.ml *)
  | None, None -> ()
  | Some r, None ->
    unset_focus r;
    Trigger.push_mouse_leave (r#id);
    set_cursor None;
  | None, Some r ->
    (* set_focus r; *)
    Trigger.push_mouse_enter (r#id);
    (* set_cursor (Some r); *)
  | Some w1, Some w2 ->
    if not (Widget.equal w1#content w2)
    then (
      (* set_focus w2; *)
      unset_focus w1;
      (* we send mouse_leave to w1 *)
      Trigger.push_mouse_leave (w1#id);
      (* we send mouse_enter to w2 *)
      Trigger.push_mouse_enter (w2#id);
      (* TODO its is NOT good to send 2 events at the same time in case of
         animation... *)
      (* set_cursor (Some w2) *)
    )
(* board.mouse_focus <- mf *)
(* Rm: in case of triggered action, this is already done by the redraw/refresh
   event *)

(* Impose mouse focus, and trigger mouse_enter/leave events as a consequence
   (regardless of actual mouse position.) *)
let set_mouse_focus board target =
  check_mouse_motion ?target board

let set_keyboard_focus (board : 'a board) (ro : 'a Layout.t option) =
  board.keyboard_focus <- ro;
  do_option ro (fun r ->
      r#focus_with_keyboard;
      check_mouse_motion ~target:r#content board)
(* = selecting something via the keyboard should also set this as mouse focus
   (to get highlight, to trigger mouse_leave, etc. but without moving the
   mouse cursor...) *)


(** react to the TAB key *)
(* we activate the next keyboard focus *)
(* TODOO this should not permit to activate items that are hidden behind a
   popup... Maybe we could restrict TAB nagivation to a unique layer ? (or
   layers above the current one?)*)
let tab board =
  let current_room = match board.keyboard_focus with
    | Some r -> r
    | None -> match board.mouse_focus with
      | Some r -> r
      | None -> match layout_focus board with
        | Some l -> l
        | None -> board.window in
  printd debug_board "Current room #%u" current_room#id;
  Layout.keyboard_focus_before_tab := Some (current_room :> Widget.common)

let debug_shortcuts =
  let open Shortcut in
  [
    ([Ctrl], K.d, fun _ ->
        debug := not !debug;
        print_endline (Printf.sprintf "Debug set to %b" !debug));
    ([Ctrl; Shift], K.i, fun board ->
        print_endline "Mouse Focus Layout parents:";
        print_endline Print.(option layout_up board.mouse_focus));
    (* ([Ctrl], K.i, fun board -> *)
    (*     print_endline "Hover Layout children (don't trust this):"; *)
    (*     print_endline Print.(option layout_down (check_mouse_hover board))); *)
    (* ([Ctrl; Shift], K.s, fun board -> (\* snapshot *\) *)
    (*     Print.dump board.windows_house); *)
    ([Ctrl], K.m, fun _ ->
        print_endline "Garbage collecting...";
        Gc.compact ();
        Draw.memory_info ());
  ]

let refresh_custom_windows board =
  (fun w -> printd debug_board "BOGUE WINDOW=%b" w#bogue;
    if not w#bogue then w#set_fresh false)
    board.window

(* [one_step] is what is executed during the main loop *)
let one_step before_display ?clear (board : 'a board) =
  let ev = Interop.Event.poll_event () in

  before_display ();
  board.window#handle_widget ev;

  let t = Time.now () in
  flip ?clear board; (* This is where all rendering takes place. *)
  printd debug_graphics "==> Rendering took %u ms" (Time.now () - t);
  printd debug_graphics "---------- end of loop -----------"

(* make the board. Each layout in the list will be displayed in a different
   window. *)
let make window : 'a board =
  window#set_bogue true;
  { window;
    mouse_focus = None;
    keyboard_focus = None;
    button_down = None;
    mouse_alive = false}

(** The main function that loops indefinitely *)
(* one can insert code to be executed at two different places: "before_display"
   means after Sync was executed and before Layout.display (except for manual
   CTRL-L which would occur before it. "after_display" means just after all
   textures have been calculated and rendered. Of course these two will not be
   executed at all if there is no event to trigger display. *)
let run ?(before_display = fun () -> ()) ?(after_display = fun () -> ()) (board : 'a board) =
  Trigger.flush_all ();
  Layout.make_window board.window;
  show board;
  Thread.delay 0.01; (* we need some delay for the initial Mouse position to be detected *)
  Sdl.pump_events ();
  Sdl.stop_text_input ();
  (* List.iter (Widget.set_canvas canvas) board.widgets; *)
  (* Warning: layouts may have different canvas because of different layers *)

  (* We have to display the board in order to detect mouse focus
     (otherwise the 'show' field of layouts are not set). *)
  display board;
  (* board.mouse_focus <- check_mouse_focus board; *)
  do_option board.mouse_focus (fun l ->
      Layout.set_focus l;
      (* we send mouse_enter event to the widget where the mouse is
         positionned at startup *)
      (* Widget.wake_up_all Trigger.(create_event mouse_enter) (Layout.widget l);
       * display board *)
      Trigger.push_mouse_enter (l#id)
    );
  flip ~clear:true board;

  Trigger.renew_my_event ();
  try
    while true do
      one_step before_display ~clear:true board;
      after_display ();
    done
  with
  | Exit -> exit_board board
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    raise e
