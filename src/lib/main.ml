open Interop
open Interop.Utils

module A = Widget.Await (struct type t = Widget.bottom end)

let geometry = ref Draw.{x=0; y=0; w=0; h=0; voffset=0}

let display widget =
  Raylib.begin_drawing ();
  begin
    let blits = widget#display !geometry in
    Raylib.clear_background Raylib.Color.green;

    let draw (Draw.{x; y; _}, tex) =
      Raylib.draw_texture tex x y Raylib.Color.blue;
      (* Raylib.unload_texture tex; *)
    in

    List.iter draw blits;
    (* let img = Raylib.gen_image_color 10 10 Raylib.Color.gray in *)
    (* Raylib.image_draw_rectangle (Raylib.addr img) 2 2 5 5 Raylib.Color.lightgray; *)
    (* let tex_ray = Raylib.load_texture_from_image img in *)
    (* Raylib.unload_image img; *)

    (* Raylib.draw_texture tex_ray 50 50 Raylib.Color.pink; *)
    (* Raylib.draw_circle 10 10 5.0 Raylib.Color.blue; *)
  end;
  Raylib.end_drawing ()

(** The main function that loops indefinitely *)
let run ?title (widget : bottom Widget.t) =
  let w,h = widget#size in
  Draw.init ?title ~w ~h ();
  Event.init ();
  geometry :=  Draw.{x=0; y=0; w; h; voffset=0};

  let triggers = ref [] in
  let cont = ref (fun _ _ -> ()) in
  begin
    match widget#execute A.await A.yield with
    | _ -> . (* Root widget never terminates *)
    | [%effect? (A.Await triggers'), k] ->
      triggers := triggers';
      cont := fun ev geom -> EffectHandlers.Deep.continue k (ev,geom)
  end;
  let handle_widget (ev : (Event.t_rich, Event.t_win) Either.t) =
    match ev with
    | Either.Right `Init ->
      ()
    | Either.Right `Resize (w,h) ->
      Printf.printf "Resize to %i,%i\n%!" w h;
      geometry := {!geometry with w; h}
    (* Sdl.set_window_size (window self) ~w ~h; *)
    | Either.Right `Exit -> raise Exit
    | Either.Left ev ->

      (* let w,h = Sdl.get_window_size @@ (self :> 'a t)#canvas.Draw.window in *)
      (* Printf.printf "geom: %iw,%ih\n" w h; *)
      if List.mem (Event.strip ev) !triggers
      then
        begin
          Printf.printf "Event %s is handled\n" (Event.show_t_rich ev);
          (* We use deep effect handlers due to ppx_effects only supporting them. *)
          (* Here, this becomes a bit confusing. The previous effect handler is still in place, *)
          (* so executing the continuation will automatically store the new continuation in refcell cont. *)
          (* Afterwards it returns (). Thus, this line looks deceptively simple *)
          !cont ev !geometry
        end
  in

  try
    while true do
      Event.wait ()
      |> List.iter handle_widget;
      display widget;
      flush_all ();
    done
  with
  | Exit -> flush_log ()
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    raise e
