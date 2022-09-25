open Interop
open Interop.Utils

let exit_board (layout : 'a Layout.t) =
  if Sdl.is_text_input_active () then Sdl.stop_text_input ();
  Draw.destroy_canvas layout#canvas;
  flush_log ()

(*************)
let debug_shortcuts =
  let open Shortcut in
  [
    ([Ctrl], K.d, fun _ ->
        debug := not !debug;
        print_endline (Printf.sprintf "Debug set to %b" !debug));
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

(** The main function that loops indefinitely *)
let run ?title ?(before_display = fun () -> ()) ?(after_display = fun () -> ()) widget =
  let window = new Layout.t ?title widget in
  Sdl.show_window window#canvas.Draw.window;

  try
    while true do
      let ev = Interop.Event.poll_event () in

      before_display ();
      window#handle_widget ev;
      after_display ();
    done
  with
  | Exit -> exit_board window
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    raise e
