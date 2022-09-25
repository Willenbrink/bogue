(* Layout is the main object type. *)

(* a layout is a 'box' which can contain 'sub-boxes'. We use the terminology of
   houses: a house contains several rooms. Each room can be viewed as a house
   which contains other rooms etc. Thus, this is a simple graph with variable
   degree. A leaf (a room which does not contain subrooms) is called a resident;
   it contains a Widget. In the whole (connected) tree, the summit is the main
   layout: the only one which does not belong to any house; it is called the
   top_house, and corresponds to a "physical" SDL window.  The size of the SDL
   window should always match the size of the top_house. *)

(* Warning: a widget should *not* appear twice (or more) inside a
   Layout. Otherwise, the results are not going to be satisfactory: a widget is
   associated to a geometry in a layout. Instead one should use two differents
   widgets with a connection between them to synchronize the data *)

open Interop
type geometry = Draw.geometry = {
  x : int;
  y : int;
  w : int;
  h : int;
  voffset : int;
}
module W = Widget

class ['a] t ?id ?title (widget : 'a W.t) =
  object (self)
    val mutable geometry =
      let w,h = widget#size in
      Draw.{x=0; y=0; w; h; voffset=0}
    method geometry = geometry
    method set_geometry x = geometry <- x

    method content = widget

    (* the canvas contains the "hardware" information to render the room *)
    (* the canvas is not really an intrinsic property of the layout, it is used
       only when rendering is required. It may change "without notice" when a
       layout is copied into another window *)
    val mutable canvas : Draw.canvas =
      let w,h = widget#size in
      Draw.init ?title ~w ~h ()
    method canvas = canvas
    method set_canvas x = canvas <- x

    val mutable cc = None

    (* return the resident widget, or Not_found *)
    method handle_widget (ev : (Event.t_rich,Event.t_win) Either.t) =
      let module A = W.Await (struct type t = W.bottom end) in
      match ev with
      | Either.Right `Resize (w,h) ->
        Printf.printf "Resize to %i,%i\n%!" w h;
        self#set_geometry {self#geometry with w; h};
        (* Sdl.set_window_size (window self) ~w ~h; *)
        self#display
      | Either.Right `Exit -> raise Exit
      | Either.Left ev ->
        let widget = self#content in

        (* let w,h = Sdl.get_window_size @@ (self :> 'a t)#canvas.Draw.window in *)
        (* Printf.printf "geom: %iw,%ih\n" w h; *)
        let f = match cc with
          | None -> fun () ->
            print_endline "Start execution of root widget";
            (* This should never terminate as the root widget#execute loops forever *)
            let _ = widget#execute A.await A.yield in
            failwith "Root widget terminated"
          | Some (triggers, cont) -> fun () ->
            if List.mem (Event.strip ev) triggers
            then begin
              Printf.printf "Event %s is handled\n" (Event.show_t_rich ev);
              (* This terminates as bottom only prevents yields, not awaits. *)
              (* Once the await is handled (i.e. the continuation stored) the handler terminates. *)
              (* Afterwards the continuation terminates. *)
              cont ev self#geometry;
            end
        in
        begin
          match f () with
          | () -> ()
          (* We attempted the continuation and it rejected the event.
              The continuation has not been used so we do nothing here.
              In case the continuation is used but does terminate it raises
              an exception. *)
          | [%effect? (A.Await triggers), k] ->
            (* print_endline "#EOH#\n"; *)
            cc <- Some (triggers, fun ev geom -> EffectHandlers.Deep.continue k (ev,geom))
        end;
        self#display

    method display =
        let blits = widget#display canvas self#geometry in
      Draw.clear_canvas canvas;
      List.iter Draw.render_blit blits;
      Draw.(sdl_flip canvas.renderer);
      Draw.destroy_textures ();
  end
