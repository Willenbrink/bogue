(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(** Vu Ngoc San, December 2013 -- now *)

(* doc on threads:
   http://ocamlunix.forge.ocamlcore.org/threads.html
*)

open Interop
open Interop.Utils

let exit_board (layout : 'a Layout.t) =
  printd debug_board "Closing layout...";
  if Sdl.is_text_input_active () then Sdl.stop_text_input ();
  Draw.destroy_canvas layout#canvas;
  Draw.check_memory ();
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
(* one can insert code to be executed at two different places: "before_display"
   means after Sync was executed and before Layout.display (except for manual
   CTRL-L which would occur before it. "after_display" means just after all
   textures have been calculated and rendered. Of course these two will not be
   executed at all if there is no event to trigger display. *)
let run ?title ?(before_display = fun () -> ()) ?(after_display = fun () -> ()) widget =
  let window = new Layout.t ?title widget in
  Sdl.show_window (Layout.window window);
  Thread.delay 0.01; (* we need some delay for the initial Mouse position to be detected *)
  Sdl.stop_text_input ();

  Layout.flip window;
  Draw.destroy_textures ();

  try
    while true do
      let ev = Interop.Event.poll_event () in

      before_display ();
      window#handle_widget ev;

      Layout.flip window;
      Draw.destroy_textures ();
      after_display ();
    done
  with
  | Exit -> exit_board window
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    raise e
