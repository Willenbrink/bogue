(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(** Vu Ngoc San, December 2013 -- now *)

(* doc on threads:
   http://ocamlunix.forge.ocamlcore.org/threads.html
*)

open Interop
open Interop.Utils

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
let exit_board window =
  close_window_layout window;
  Draw.destroy_textures (); (* en principe inutile: déjà fait *)
  (* Layout.clear_wtable (); *)
  Draw.check_memory ();
  Trigger.flush_all ();
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
let run ?(before_display = fun () -> ()) ?(after_display = fun () -> ()) window =
  Trigger.flush_all ();
  Layout.make_window window;
  Sdl.show_window (Layout.window window);
  window#set_fresh false;
  Draw.update_background (Layout.get_canvas window);
  Thread.delay 0.01; (* we need some delay for the initial Mouse position to be detected *)
  Sdl.stop_text_input ();
  (* List.iter (Widget.set_canvas canvas) board.widgets; *)
  (* Warning: layouts may have different canvas because of different layers *)

  (* We have to display the board in order to detect mouse focus
     (otherwise the 'show' field of layouts are not set). *)
  Trigger.(flush redraw);
  if not window#is_fresh then Layout.render window;
  (* board.mouse_focus <- check_mouse_focus board; *)
  Layout.flip window;
  Draw.destroy_textures ();

  Trigger.renew_my_event ();
  try
    while true do
      let ev = Interop.Event.poll_event () in

      before_display ();
      window#handle_widget ev;

      let t = Time.now () in
      Layout.flip window;
      Draw.destroy_textures ();
      printd debug_graphics "==> Rendering took %u ms" (Time.now () - t);
      after_display ();
    done
  with
  | Exit -> exit_board window
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    raise e
