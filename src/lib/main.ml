(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(** Vu Ngoc San, December 2013 -- now *)

(* doc on threads:
   http://ocamlunix.forge.ocamlcore.org/threads.html
*)

open Utils

module E = Sdl.Event

exception Exit

type 'a board = {
  mutable windows : 'a Layout.t list;
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
  mutable shortcuts : 'a board Shortcut.t;
  (* Global keyboard shortcuts. TODO some shortcuts should be executed only on
     Key_up to avoid auto-repeat. => create 2 maps, shortcuts_down et
     shortcuts_up . Or maybe all? *)
  mutable shortcut_pressed: bool;
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
  List.iter close_window_layout board.windows;
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
  List.iter (fun w ->
      if not w#is_fresh then Layout.render w)
    board.windows

(** Render all layers and flip window buffers, only for windows with the
    is_fresh=false field *)
let flip ?clear board =
  List.iter (Layout.flip ?clear) board.windows;
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

(** add a new window (given by the layout) to the board *)
let add_window board window =
  (* update board *)
  let windows = List.rev (window :: (List.rev board.windows)) in
  board.windows <- windows;

  (* show *)
  Sdl.show_window (Layout.window window);
  Draw.update_background (Layout.get_canvas window);

  (* run *)
  display board;
  do_option board.mouse_focus Layout.set_focus;
  flip board;
  Trigger.renew_my_event ();
  window

let same_window w1 w2 = Sdl.(get_window_id w1 = get_window_id w2)

(** get window (layout) by id. Not used... (layout_event can do it somehow) *)
let get_window_by_id board id =
  let rec loop = function
    | [] -> printd debug_error "There is no window with this id#%d" id;
      List.hd board.windows;
    | w::rest -> if id = Layout.id w then w
      else loop rest in
  loop board.windows

let remove_window board window =
  let windows = List.filter (fun w -> not (Widget.equal window w)) board.windows in
  board.windows <- windows;
  printd debug_board "** Remove window #%u (Layout #%u)"
    (Layout.id window) window#id;
  close_window_layout window;
  (* We reset all focus for safety. TODO: one could reset only those that
     belonged to the removed window. *)
  board.mouse_focus <- None;
  board.keyboard_focus <- None;
  board.button_down <- None

(*************)
let show board =
  List.iter (fun w ->
      Sdl.show_window (Layout.window w);
      w#set_fresh false;
      Draw.update_background (Layout.get_canvas w)) board.windows

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
  | None -> None (* the mouse is outside of the SDL windows *)
  | Some w -> list_check_ok
                (fun l -> same_window (Layout.window l)  w) board.windows

(** which window corresponds to the event ? *)
let window_of_event board ev =
  try
    let w_id = match Trigger.event_kind ev with
      | `Bogue_redraw ->
        let id = E.(get ev user_code) in
        board.windows
        |> List.find (fun l -> id = l#content#id)
        |> (fun r ->
            let id = Sdl.get_window_id (Layout.window r) in
            printd debug_event "Redraw event window_id=%d" id;
            id)
      | _ -> Trigger.window_id ev
    in
    list_check_ok (fun w -> w_id = Layout.id w) board.windows
  with Not_found ->
    printd debug_error "Search window for event %s caused an error" (Trigger.sprint_ev ev);
    None

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
  let open Trigger in
  let mf = match target with
    | Some _ -> target
    | None -> check_mouse_focus board in
  let () = match board.mouse_focus, mf with
    (* on compare l'ancien et le nouveau. See remarks in trigger.ml *)
    | None, None -> ()
    | Some r, None ->
      unset_focus r;
      push_mouse_leave (r#id);
      set_cursor None;
    | None, Some r ->
      (* set_focus r; *)
      push_mouse_enter (r#id);
      (* set_cursor (Some r); *)
    | Some w1, Some w2 ->
      if not (Widget.equal w1#content w2)
      then (
        (* set_focus w2; *)
        unset_focus w1;
        (* we send mouse_leave to w1 *)
        push_mouse_leave (w1#id);
        (* we send mouse_enter to w2 *)
        push_mouse_enter (w2#id);
        (* TODO its is NOT good to send 2 events at the same time in case of
           animation... *)
        (* set_cursor (Some w2) *)
      ) in
  ()
(* board.mouse_focus <- mf *)
(* Rm: in case of triggered action, this is already done by the redraw/refresh
   event *)

(* guess which widget the event should be targetted to (or None) *)
let hand_to_target_widget board ev =
  let wid = Trigger.window_id ev in
  let roomo = List.find_opt (fun x -> Layout.id x = wid) board.windows in
  (* let roomo = *)
  (*   if not (E.(get ev typ) = Trigger.mouse_enter || *)
  (*           E.(get ev typ) = Trigger.mouse_leave) *)
  (*   then match board.button_down with *)
  (*     | Some r (\*when !dragging*\) -> *)
  (*       printd debug_board "Target: select button_down"; *)
  (*       Some r *)
  (*     (\* when dragging, the board.button_down has priority over all *\) *)
  (*     (\* TODO: it happens also for push buttons, scroll bars, etc... *\) *)
  (*     (\* OR: give board.button_down priority for ALL but for menus (find *)
  (*        something else, like activate what was selected in the menu...) *\) *)
  (*     | None -> *)
  (*       if Trigger.text_event ev *)
  (*       || Option.map Layout.has_keyboard_focus board.button_down = Some true *)
  (*          (\* if the button remains down, the initial text event keeps *)
  (*             listening to events *\) *)
  (*          (\* TODO: idem for mouse_button_up ? *\) *)
  (*       then (printd debug_board "Target: select keyboard widget"; *)
  (*             (board.keyboard_focus)) *)
  (*       else (printd debug_board "Target: select mouse widget"; *)
  (*             (board.mouse_focus)) *)
  (*   else let id = E.get ev Trigger.room_id in *)
  (*     Layout.of_id_opt ~not_found:(fun () -> *)
  (*         printd debug_error "The room #%u has disappeared but was pointed by \ *)
             (*                             the mouse enter/leave event" id) id *)
  (* in *)
  roomo

let activate : 'a board -> 'a Layout.t option -> unit = fun board roomo ->
  board.button_down <- roomo;
  (match board.keyboard_focus, roomo with
   | Some kr, Some mr when not (kr == mr) ->
     kr#remove_keyboard_focus;
     kr#content#update
   | Some kr, None -> kr#remove_keyboard_focus;
     kr#content#update
   | _ -> ())

(* Impose mouse focus, and trigger mouse_enter/leave events as a consequence
   (regardless of actual mouse position.) *)
let set_mouse_focus board target =
  check_mouse_motion ?target board

let set_keyboard_focus (board : 'a board) (ro : 'a Layout.t option) =
  activate board ro;
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
        | None -> List.hd board.windows in
  printd debug_board "Current room #%u" current_room#id;
  Layout.keyboard_focus_before_tab := Some (current_room :> Widget.common)

(** open/close the debugging window *)
let toggle_debug_window =
  let window = ref None in fun (board : unit board) ->
    match !window with
    | None ->
      print_endline "OPENING DEBUG WINDOW"
    (* let debug_window = Debug_window.create () in *)
    (* Layout.make_window debug_window; *)
    (* let w = add_window board debug_window in *)
    (* window := Some w *)
    | Some w ->
      remove_window board w;
      window := None

let debug_shortcuts =
  let open Shortcut in
  [
    ([Ctrl], K.d, fun _ ->
        debug := not !debug;
        print_endline (Printf.sprintf "Debug set to %b" !debug));
    ([Ctrl; Shift], K.d, toggle_debug_window);
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
  List.iter (fun w -> printd debug_board "BOGUE WINDOW=%b" w#bogue;
              if not w#bogue then w#set_fresh false)
    board.windows

(* [one_step] is what is executed during the main loop *)
let one_step before_display ?clear (board : 'a board) =
  let e = !Trigger.my_event in
  let evo =
    (* (go (Sdl.wait_event (Some e)); Some e) *)
    (* DOC: As of SDL 2.0, this function does not put the
       application's process to sleep waiting for events; it polls for
       events in a loop internally. This may change in the future to
       improve power savings. *)
    (* ME: as a result, it seems that Sdl.wait_event prevents other
       threads from executing nicely *)
    Some (Trigger.wait_event e)
  in
  (* Flush any motion events to avoid lag when there are too many events.
     Note that we already stored the latest event in e *)
  Trigger.flush (E.finger_motion);
  Trigger.flush (E.mouse_motion);
  (* from this point there is no need to keep several events for redrawing
     the same window. TODO ?? BUT keeping one might still be necessary ? *)
  (* Note: we will need some small delay to grab new events after
     empty_events *)
  (* Note, do not flush var_changed, it is used by radiolist.ml,
     cf. example30 *)

  (* We put here the events that should be filtered or modified. This
     returns the evo_layout that the layout (& widget) is authorized to treat
     thereafter. *)
  let evo_layout =
    Option.bind evo (fun e ->
        printd debug_event "== > Filtering event type: %s" (Trigger.sprint_ev e);
        match Trigger.event_kind e with
        | `Bogue_keyboard_focus ->
          (* we filter and treat only the last event *)
          let e' = default (Trigger.get_last (Trigger.keyboard_focus)) e in
          (* set_keyboard_focus board *)
          (*   (Layout.of_id_opt (get e' user_code) *)
          (*      ~not_found:(fun () -> *)
          (*          printd debug_error "Room #%u pointed by event %s has disappeared" *)
          (*            (get e' user_code) (Trigger.sprint_ev e'))); *)
          Some e' (* ou None ? *)
        | `Bogue_mouse_focus ->
          printd debug_event "Require Mouse FOCUS";
          (* we filter and treat only the last event *)
          let e' = default (Trigger.get_last (Trigger.mouse_focus)) e in
          (* set_mouse_focus board (Layout.of_id_opt (get e' user_code) |> Option.map (fun x -> x#content)); *)
          Some e'
        | `Bogue_mouse_enter ->
          printd debug_event "Mouse ENTER";
          (* by design, only one mouse_enter event can exist in the queue. *)
          evo
        | `Bogue_mouse_leave ->
          printd debug_event "Mouse LEAVE";
          (* by design, only one mouse_leave event can exist in the queue. *)
          evo
        | _ -> evo
      )
  in

  (* Now we treat events that should be used before being sent to the layout
     & widget, but without filtering *)
  do_option evo_layout (fun e ->
      let open E in
      printd debug_event "== > Treating event type: %d" (get e typ);
      begin match Trigger.event_kind e with
        (* Here we treat key events that have priority over the widgets *)
        (* | `Key_up when get e keyboard_keycode = Sdl.K.escape -> raise Exit *) (* TODO close sub-dialogs *)
        | `Key_up -> board.shortcut_pressed <- false;
          (* I assume auto-repeat will never trigger Key_up, but it seems that
                 it's not always the case... (can happen when a new window opens) *)
        | `Key_down ->
          let pair = get e keyboard_keycode, get e keyboard_keymod in
          if not board.shortcut_pressed
          then
            (Printf.printf "Handling shortcut\n";
             match (Shortcut.find pair board.shortcuts) with
             | None -> print_endline "No shortcut found."
             | Some a -> board.shortcut_pressed <- true; a board)
        (* | `Key_down when get e keyboard_keycode = Sdl.K.tab -> tab board *)
        (* | `Key_down when get e keyboard_keycode = Sdl.K.i ->
         *   if Trigger.ctrl_shift_pressed ()
         *   then (print_endline "Mouse Focus Layout parents:";
         *         print_endline Print.(option layout_up board.mouse_focus))
         *   else if Trigger.ctrl_pressed ()
         *   then (print_endline "Hover Layout children (don't trust this):";
         *         print_endline Print.(option layout_down (check_mouse_hover board))) *)
        (* | `Key_down when get e keyboard_keycode = Sdl.K.s (\* snapshot *\)
         *               && Trigger.ctrl_shift_pressed () ->
         *   Print.dump board.windows_house *)
        (* | `Key_up when get e keyboard_keycode = Sdl.K.d
         *             && Trigger.ctrl_shift_pressed () -> toggle_debug_window board
         * | `Key_up when get e keyboard_keycode = Sdl.K.d
         *             && Trigger.ctrl_pressed () -> debug := not !debug *)
        (* | `Key_down when get e keyboard_keycode = Sdl.K.l
         *               && Trigger.ctrl_pressed () ->
         *   print_endline "User Redraw";
         *   display board;
         *   show board; *)
        (* | `Key_down when get e keyboard_keycode = Sdl.K.m
         *               && Trigger.ctrl_pressed () ->
         *   Draw.memory_info ();
         *   if !debug then (print_endline "Garbage collecting...";
         *                   Gc.compact ();
         *                   Draw.memory_info ()) *)
        | `Mouse_button_down
        | `Finger_down ->
          Trigger.button_down e; (* TODO for touch too... *)
          activate board board.mouse_focus
        | `Mouse_button_up
        | `Finger_up ->
          Trigger.button_up e; (* TODO for touch too... *)
          if not !Trigger.too_fast
          && (map2_option board.button_down board.mouse_focus Widget.equal
              = Some true
              || map_option board.button_down Layout.has_keyboard_focus
                 = Some true)
          then begin
            printd debug_event "full click";
            Trigger.(push_event (full_click_event ()));
            (* full click means that the press and released were done on the
               same widget. It does not mean that the click was "quick". For
               this, check Trigger.single_click. *)
            (* = this trigger does not work well because all user_event are
               captured to trigger redraw, it ends up with an infinite
               redraw loop, since all "connections" add a User0
               event... Maybe we could do this if we make sure that we add
               connections without "redraw" (update_target=false) *)
            set e mouse_button_state Sdl.pressed;
            (* = this is a DIRTY HACK ! we set button_state to "pressed" (it
               should be "released") to store the fact that we have a full
               click *) (* TODO: use the full_click event instead *)
            (* Now we set keyboard_focus on "admissible" widgets: *)
            do_option board.mouse_focus (fun x ->
                printd debug_board "Mouse focus: %d" x#id);
            do_option board.keyboard_focus (fun x ->
                printd debug_board "Keyboard focus: %d" x#id);
            do_option board.button_down (fun x ->
                printd debug_board "Set keyboard_focus to #%d" x#id;
                x#focus_with_keyboard);
            board.keyboard_focus <- board.button_down; (* OK ?? *)
          end
        | `Mouse_wheel -> ()
        (* TODO change. mouse_wheel should be captured by the widget itself. *)
        | `Window_event ->
          let wid = get e window_event_id in
          printd debug_event "Window event [%d]" wid;
          (* Warning: on my system, resizing window by dragging the corner
                does not trigger only 6 = resize, but triggers event 4=
                "window_event_moved"... and sometimes 3=exposed *)
          (* Some window events may come by pair; for instance if you
                 middle_click on the maximize button, it can trigger 10 (mouse
                 enter) and then 6 (resize). So the 6 should not be flushed
                 ! *)
          begin
            match window_event_enum wid with
            (* | `Resized *)
            (* https://wiki.libsdl.org/SDL_WindowEventID *)
            | `Size_changed ->
              printd debug_event "Size_changed => Resize to (%lu,%lu)"
                (get e window_data1) (get e window_data2);
              do_option (window_of_event board e) resize
            | `Exposed ->
              (* the exposed event is triggered by X11 when part of the
                 window is lost and should be re-rendered. Sometimes several
                 exposed events are triggered because they correspond to
                 several regions of the window. This seems to be unreachable
                 via SDL. *)
              printd debug_event "Exposed";
              (* (\* for some reason, when the size changes quickly, Exposed is *)
              (*    triggered but not Resized nor `Size_changed...*\) *)
              (* do_option (window_of_event board e) (fun w -> *)
              (*     if Window.size w <> Layout.get_physical_size w *)
              (*     then (Layout.resize_from_window ~flip:false l; *)
              (*           Thread.delay 0.002); (\* only to be nice *\) *)
              (*     (\* else Trigger.flush_but_n 8; *\) (\* DEBUG *\) *)
              (*     (\* the renderer changed, we need to recreate all *)
              (*        textures *\) *)
              (*     Window.to_refresh w) *)
            | `Close ->
              printd (debug_board+debug_event) "Asking window to close";
              do_option (window_of_event board e) (remove_window board);
            | _ as enum ->
              printd debug_event "%s" (Trigger.window_event_name enum);
              do_option (window_of_event board e) (fun w ->
                  w#set_fresh false;
                  check_mouse_motion board;
                  (* Otherwise we don't get mouse_leave when the mouse leaves the
                       window. OK here ? *)
                  (* Warning: the behaviour of SDL seems to be the following:
                       when the window has no focus and the user click on it,
                       there is NO Button_down NEITHER Button_up event, instead
                       there is a Window "Take_focus" event. We follow this
                       here. It means that the user has to click a second time to
                       activate a button.*) )
          end;
          (* TODO just display the corresponding window, not all of them *)
        | `Quit -> raise Exit
        | _ -> ()
      end);


  before_display ();

  let has_no_event = Trigger.has_no_event () in
  (* Now, the widget has the event *)
  (* note that the widget will usually emit a redraw event, this is why we save
     the state of Trigger.has_no_event () *)
  (match evo_layout with
   | None -> ()
   | Some ev ->
     match hand_to_target_widget board ev with
     | None -> ()
     | Some room -> room#handle_widget ev
  );

  (* the board can use the event that was filtered by the widget *)
  do_option evo_layout (fun ev ->
      match Trigger.event_kind ev with
      (*          | `Key_down when E.(get e keyboard_keycode) = Sdl.K.tab -> tab board e *)
      | _ -> ());

  (* Finally we do final updates before flip with the original, unfiltered
     event "e" *)
  (* TODO we should not use 'e' if there is an anim, because it will be an
     old event *)
  begin
    do_option evo (fun e ->
        match Trigger.event_kind e with
        | `Mouse_motion ->
          board.mouse_alive <- true;
          if has_no_event && not (Trigger.mm_pressed e) then check_mouse_motion board
        (* In most situations, when the button is pressed, we don't want to lose
           the initial focus, and we don't want to activate anything else. There
           is one (common) exception: when clicking a menu entry, we would like
           to navigate menus while mouse button is down. How to handle this
           particular case? If we remove the mm_pressed test it works nicely for
           menus, but it not usual for other things (like scroll bar). I don't
           see any other solution than adding a new flag
           'allow_focus_change_when_mm_pressed' somewhere... TODO? *)
        | `Mouse_button_up | `Finger_up ->
          board.button_down <- None;
          check_mouse_motion board
        | `Bogue_redraw ->
          (* Sometimes there are too many redraw events in the queue, this would
             cause a noticeable delay if only one can be treated by
             iteration. Cf example 28/bis.  Hence we leave at most one. Flushing
             all here is NOT recommended, it can prevent the correct detection
             of new animations (ex: adding sliding popups). *)
          do_option Trigger.(get_last redraw) (fun ev -> Trigger.push_event ev);
          printd debug_event "Redraw";
          do_option (window_of_event board e) (fun w -> w#set_fresh false)
        | _ -> ());
    Thread.delay 0.005;
    (* even when there is no anim, we need to to be nice to other treads, in
             particular when an event is triggered very rapidly (mouse_motion) and
             captured by a connection, without anim. Should we put also here a FPS
             ?? *)
  end;
  let t = Time.now () in
  flip ?clear board; (* This is where all rendering takes place. *)
  printd debug_graphics "==> Rendering took %u ms" (Time.now () - t);
  printd debug_graphics "---------- end of loop -----------"

(* make the board. Each layout in the list will be displayed in a different
   window. *)
let make ?(shortcuts = []) window : 'a board =
  window#set_bogue true;
  let shortcuts =
    let open Shortcut in
    empty
    |> add_list shortcuts
    (* FIXME disabled because of type problems *)
    (* |> add_list debug_shortcuts (\* FIXME Add this always or only when debugging? *\) *)
    (* |> add K.tab tab *)
    |> add ~kmod:[Ctrl] K.l (fun board ->
        print_endline "User Redraw";
        display board;
        show board)
  in
  { windows = [window];
    mouse_focus = None;
    keyboard_focus = None;
    button_down = None;
    shortcuts; shortcut_pressed = false;
    mouse_alive = false}

(** The main function that loops indefinitely *)
(* one can insert code to be executed at two different places: "before_display"
   means after Sync was executed and before Layout.display (except for manual
   CTRL-L which would occur before it. "after_display" means just after all
   textures have been calculated and rendered. Of course these two will not be
   executed at all if there is no event to trigger display. *)
let run ?(before_display = fun () -> ()) ?(after_display = fun () -> ()) (board : 'a board) =
  Trigger.flush_all ();
  List.iter Layout.make_window board.windows;
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
