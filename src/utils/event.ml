(* TODO turn this into a normal variant? Using polymorphic variants is a major hassle
   because it seems that any interesting feature requires a lot of type shenanigans.
   It seems we would need excessive GADTs, polymorphic annotations etc. Would enable us to:
   - Encode possible rich events in the await type. Major problem: type escaping scope
   - Encode bottom: Empty polymorphic variants are not typable
*)
type t = [
  (* TODO currently there is no way to write an empty polymorphic variant, see
     https://github.com/ocaml/ocaml/issues/10687
  *)
  (* | `Bottom *)
  | `Key_press
  | `Key_repeat
  | `Key_release
  | `Codepoint
  | `Mouse_motion
  | `Mouse_enter
  | `Mouse_leave
  | `Mouse_press
  | `Mouse_release
  | `Scroll
]
[@@deriving show]

(* TODO we need some form of global listening. Press slider -> Move mouse off -> Release *)
(* These events can normally only be listened to by one widget *)
let keyboard_events : t list =
  [`Key_press; `Key_repeat; `Key_release; `Codepoint]

type button =
  (* Left/Right/Middle Mouse Button *)
  | LMB
  | RMB
  | MMB
  | B4
  | B5
  | B6
  | B7
  | B8
[@@deriving enum, show]

(* TODO alt missing *)
type modifier =
  | Ctrl
  | Shift
[@@deriving show]

(* TODO currently there is no way to write an empty polymorphic variant, see
   https://github.com/ocaml/ocaml/issues/10687
*)
type t_rich =
  | Key_press : int * modifier list -> t_rich
  | Key_repeat : int * modifier list ->  t_rich
  | Key_release : int * modifier list ->  t_rich
  (* TODO use a UTF8 library of some sort instead of using string? *)
  | Codepoint : string -> t_rich
  | Mouse_motion : (int * int) -> t_rich
  | Mouse_enter : (int * int) -> t_rich
  | Mouse_leave : (int * int) -> t_rich
  | Mouse_press : (int * int) * button -> t_rich
  | Mouse_release : (int * int) * button -> t_rich
  | Scroll : t_rich
[@@deriving show]

let strip (ev : t_rich) : t = match ev with
  | Key_press _ -> `Key_press
  | Key_repeat _ -> `Key_repeat
  | Key_release _ -> `Key_release
  | Codepoint _ -> `Codepoint
  | Mouse_motion _ -> `Mouse_motion
  | Mouse_enter _ -> `Mouse_enter
  | Mouse_leave _ -> `Mouse_leave
  | Mouse_press _ -> `Mouse_press
  | Mouse_release _ -> `Mouse_release
  | Scroll -> `Scroll

type t_win = [
  | `Exit
  | `Resize of int * int
]

let queue : t list ref = ref []

module E = Sdl.Event

let rec poll_event () : (t_rich, t_win) Either.t =
  let ev = Trigger.wait_event () in
  Trigger.flush (E.finger_motion);
  Trigger.flush (E.mouse_motion);
  match Trigger.event_kind ev with
  | `Quit ->
    Either.Right `Exit
  | `Window_event ->
    begin
      let eid = Sdl.Event.(get ev window_event_id) in
      (* TODO right now only one window is supported *)
      (* let wid = Sdl.Event.(get ev window_window_id) in *)
      let data1 = Sdl.Event.(get ev window_data1) |> Int32.to_int in
      let data2 = Sdl.Event.(get ev window_data2) |> Int32.to_int in
      match eid with
      | id when id = Sdl.Event.window_event_size_changed ->
        Either.right @@ `Resize (data1,data2)
      | _ ->
        Printf.printf "Window event %i\n" eid;
        poll_event ()
    end
  | `Key_down ->
    let modifiers =
      (if Trigger.ctrl_pressed () then [Ctrl] else [])
      @ (if Trigger.shift_pressed () then [Shift] else [])
    in
    Either.left @@
    Key_press (Sdl.Event.(get ev keyboard_keycode), modifiers)
  | `Key_up ->
    let modifiers =
      (if Trigger.ctrl_pressed () then [Ctrl] else [])
      @ (if Trigger.shift_pressed () then [Shift] else [])
    in
    Either.left @@
    Key_release (Sdl.Event.(get ev keyboard_keycode), modifiers)
  | `Mouse_button_down -> Either.left @@ Mouse_press (Mouse.pointer_pos ev, LMB) (* TODO *)
  | `Mouse_button_up -> Either.left @@ Mouse_release (Mouse.pointer_pos ev, LMB) (* TODO *)
  | `Mouse_motion -> Either.left @@
    (* let left = if Trigger.mm_pressed ev then [Left] else [] in *)
    Mouse_motion (Mouse.pointer_pos ev)
  | `Text_input -> Either.left @@
    Codepoint Sdl.Event.(get ev text_input_text)
  | `Bogue_keyboard_focus | `Bogue_mouse_focus | `Bogue_startup | `Bogue_stop
  | `Bogue_stopped | `Bogue_full_click | `Bogue_mouse_enter | `Bogue_mouse_leave
  | `Bogue_var_changed | `Bogue_redraw ->
    print_endline "Bogue event";
    assert false
  (* Events to ignore for now *)
  | `App_did_enter_background | `App_did_enter_foreground
  | `App_low_memory | `App_terminating | `App_will_enter_background
  | `App_will_enter_foreground | `Clipboard_update
  | `Controller_axis_motion | `Controller_button_down
  | `Controller_button_up | `Controller_device_added
  | `Controller_device_remapped | `Controller_device_removed
  | `Dollar_gesture | `Dollar_record | `Drop_file | `Finger_down
  | `Finger_motion | `Finger_up | `Joy_axis_motion | `Joy_ball_motion
  | `Joy_button_down | `Joy_button_up | `Joy_device_added
  | `Joy_device_removed | `Joy_hat_motion ->
    Printf.printf "Unimportant event %s\n" (Trigger.sprint_ev ev);
    poll_event ()
  | `Mouse_wheel | `Multi_gesture | `Sys_wm_event
  | `Text_editing | `User_event
  | `Display_event | `Sensor_update ->
    Trigger.sprint_ev ev |> print_endline;
    poll_event ()
  | `Unknown 772 ->
    poll_event () (* TODO Where does this event come from? *)
  | `Unknown id ->
    Printf.printf "Received event with id %i\n" id;
    assert false
  | _ ->
    print_endline "Specific Event";
    Trigger.sprint_ev ev |> print_endline;
    failwith "Impossible"
