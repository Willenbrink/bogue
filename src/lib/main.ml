open Widget

module A = Widget.Await (struct type t = Widget.bottom end)

let geometry = ref {x=0; y=0; w=0; h=0; voffset=0}

let display widget =
  Raylib.begin_drawing ();
  begin
    let blits = widget#display !geometry in
    Raylib.clear_background Raylib.Color.white;

    let draw ({x; y; _}, tex) =
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
  (* TODO To initialize raylib we need the window size. I believe this is not a technical limitation. *)
  (* Perhaps picking apart raylib/replacing it completely with GLFW would solve this. *)
  (* Commented because we need initialized raylib to check size of labels (i.e. text) *)
  let w,h = widget#min_size in
  (* let w,h = 600,400 in *)
  Window.init ?title ~w ~h ();

  let open Font.State.Let in
  let* () = Font.default () in
  Font.set_default ();
  Event.init ();
  geometry :=  {x=0; y=0; w; h; voffset=0};

  let triggers = ref [] in
  let cont = ref (fun _ _ -> ()) in
  begin
    match widget#execute A.await A.yield with
    | _ -> . (* Root widget never terminates *)
    | [%effect? (A.Await triggers'), k] ->
      triggers := triggers';
      cont := fun ev geom -> Eff.continue k (ev,geom)
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

      (* let w,h = Sdl.get_window_size @@ (self :> 'a t)#canvas.window in *)
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

  let rec loop () =
    Event.wait ()
    |> List.iter handle_widget;
    display widget;
    flush_all ();
    loop ()
  in
  loop ()

let enter ?title (widget : bottom Widget.t) =
  try
    run ?title widget
  with
  | Exit ->
    flush_all ()
