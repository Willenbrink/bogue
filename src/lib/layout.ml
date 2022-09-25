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
open Interop.Utils
type geometry = Draw.geometry = {
  x : int;
  y : int;
  w : int;
  h : int;
  voffset : int;
}
module W = Widget

type adjust =
  | Fit
  | Width
  | Height
  | Nothing

let sprint_id r =
  Printf.sprintf "#%u%s" r#id (match r#name with
      | "" -> ""
      | s -> Printf.sprintf " (%s)" s)

let window t = t#canvas.Draw.window

class ['a] t ?id ?title ?(adjust = Fit)
    (content' : 'a W.t) =
  object (self)
    (* should we adjust the size of this room to fit its content ? *)
    method adjust = adjust

    (* relative geometry wrt the house. All components are dynamic variables,
       that need to be recomputed at each iteration. Note: rooms inside a house
       must be physically inside the geometry of the house. If not, they will
       not be detected by the mouse, for instance. *)
    val mutable geometry =
      let w,h = content'#size in
      Draw.{x=0; y=0; w; h; voffset=0}
    method geometry = geometry
    method set_geometry x = geometry <- x

    val mutable color : Draw.color option = None
    method color = color
    method set_color bg = color <- bg

    val mutable content : 'a Widget.t = content'
    method content = content
    method set_content x = content <- x

    (* : the particular layer = chain element of this layout. If a room
       contains other Rooms, its layer should be at least as deep as the layers
       of the Rooms, otherwise the "background" might end-up not being at the
       background... *)
    (* in principle a chain of layers is attached to a window. When creating a
       new window, one has to select a new layer chain (use_new_layer) *)
    val mutable layer = Draw.current_layer
    method layer = layer
    method set_layer x = layer <- x

    (* the canvas contains the "hardware" information to render the room *)
    (* the canvas is not really an intrinsic property of the layout, it is used
       only when rendering is required. It may change "without notice" when a
       layout is copied into another window *)
    val mutable canvas : Draw.canvas =
      let w,h = content'#size in
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

        (* let w,h = Sdl.get_window_size @@ window (self :> 'a t) in *)
        (* Printf.printf "geom: %iw,%ih\n" w h; *)
        let f = match cc with
          | None -> fun () ->
            print_endline "Start execution of root widget";
            let _ = widget#execute A.await A.yield in
            failwith "Root widget terminated"
          | Some (triggers, cont) -> fun () ->
            if List.mem (Event.strip ev) triggers
            then begin
              Printf.printf "Event %s is handled\n" (Event.show_t_rich ev);
              cont ev self#geometry;
              (* TODO why does this terminate?
                 And why is it no problem if we don't overwrite the continuation? *)
              (* failwith "Root continuation terminated" *)
              (* cc <- None *)
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

    (* this function sends all the blits to be displayed to the layers *)
    (* it does not directly interact with the renderer *)
    (* pos0 is the position of the house containing the room *)
    method display =
        (* clip contains the rect that should contain the current room r. But of
           course, clip can be much bigger than r. *)
        let g = self#geometry in
        let x = g.x in
        let y = g.y + g.voffset in
        (*print_endline ("ALPHA=" ^ (string_of_float (Avar.old room#geometry.transform.alpha)));*)

        let bg = match color with
          | None -> []
          | Some c ->
            let box = new Box.t ~size:(g.w,g.h) ~bg:(Style.Solid c) () in
            box#display canvas self#layer
                Draw.(scale_geom {x; y; w = g.w; h = g.h; voffset = - g.voffset})
        in

        let blits = self#content#display canvas self#layer Draw.{g with x; y} in
        let blits = List.rev_append bg blits in

        (* TODO presumably blit has clip = None as default *)
        List.iter (fun blit -> Draw.blit_to_layer { blit with clip = None }) blits
  end

let flip w =
  (if Draw.window_is_shown (window w)
  then w#display);
  (* go (Sdl.set_render_target (renderer layout) None); *)
  Draw.clear_canvas w#canvas;
  printd debug_graphics "Render layers";
  Draw.render_all_layers w#layer;
  printd debug_graphics "Present";
  Draw.(sdl_flip w#canvas.renderer)
