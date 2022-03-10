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

(* FIXME copy of style? *)
type background = (* TODO instead we should keep track of how the box was created... in case we want to recreate (eg. use it for another window... ?) *)
  (* TODO use Style.background ? Cependant Style est antérieur (et utilisé par)
     à Box... *)
  | Solid of Draw.color
  | Box : unit Box.t -> background

let color_bg color =
  Solid color

let bg_color = color_bg Draw.(opaque bg_color)

type adjust =
  | Fit
  | Width
  | Height
  | Nothing

let sprint_id r =
  Printf.sprintf "#%u%s" r#id (match r#name with
      | "" -> ""
      | s -> Printf.sprintf " (%s)" s)

(* get the window of the layout *)
let window t = match t#canvas with
  | None -> failwith "Invalid"
  | Some c -> c.Draw.window

class ['a] t ?id ?name ?(adjust = Fit)
    ?(layer = Draw.get_current_layer ())
    ?mask ?background ?shadow ?keyboard_focus ?(mouse_focus=false)
    ?canvas
    ?(is_fresh = false)
    _geometry (content' : 'a W.t) =
  object (self)
    (* FIXME what is the size of a room? We have a resize but no base size *)
    inherit Base.common ?id ?name (0,0) ()

    (* should we adjust the size of this room to fit its content ? *)
    method adjust = adjust

    val mutable is_fresh = is_fresh
    method is_fresh = is_fresh
    method set_fresh b = is_fresh <- b

    (* relative geometry wrt the house. All components are dynamic variables,
       that need to be recomputed at each iteration. Note: rooms inside a house
       must be physically inside the geometry of the house. If not, they will
       not be detected by the mouse, for instance. *)
    val mutable geometry = _geometry
    method geometry = geometry
    method set_geometry x = geometry <- x

    val mutable background : background option = background
    (* Some (Solid Draw.(opaque blue))) DEBUG *)
    method background = background
    method set_background x = background <- x

    (* this can be used to force recreating the background, for instance after
       changing the size of the room *)
    method unload =
      do_option background (function
          | Box b -> b#unload
          | Solid _ -> ())

    val mutable shadow : Style.shadow option = shadow
    method shadow = shadow
    method set_shadow x = shadow <- x

    (* If there is a mask, a position (x,y) will be declared inside the layout
       if it corresponds to a mask pixel with alpha value <> 0. A mask will act
       as a clip if it is uniformly white, and the shape is given by nonzero
       alpha values. (TODO) *)
    method mask : Sdl.surface option = mask

    val mutable content : 'a Widget.t = content'
    method content = content
    method set_content x = content <- x

    (* : the particular layer = chain element of this layout. If a room
       contains other Rooms, its layer should be at least as deep as the layers
       of the Rooms, otherwise the "background" might end-up not being at the
       background... *)
    (* in principle a chain of layers is attached to a window. When creating a
       new window, one has to select a new layer chain (use_new_layer) *)
    val mutable layer = layer
    method layer = layer
    method set_layer x = layer <- x

    (* the canvas contains the "hardware" information to render the room *)
    (* the canvas is not really an intrinsic property of the layout, it is used
       only when rendering is required. It may change "without notice" when a
       layout is copied into another window *)
    val mutable canvas : Draw.canvas option = canvas
    method canvas = canvas
    method set_canvas x = canvas <- x

    (* set interactively when has mouse focus *)
    val mutable mouse_focus = mouse_focus
    method mouse_focus = mouse_focus
    method set_mouse_focus x = mouse_focus <- x

    (* None = cannot have focus; Some b = has focus or not *)
    (* TODO: should we move the keyboard_focus to the Widget ? A layout which
       contains a Rooms list cannot really have keyboard_focus...and in fact it
       will not be detected by 'next_keyboard' *)
    val mutable keyboard_focus : bool option = keyboard_focus
    method keyboard_focus = keyboard_focus
    method set_keyboard_focus x = keyboard_focus <- x

    (** set keyboard_focus if possible *)
    (* we don't lock because it will be modified only by the main loop *)
    method! focus_with_keyboard =
      do_option self#keyboard_focus (fun b ->
          if not b then begin
            printd debug_board "Setting layout keyboard_focus";
            self#set_keyboard_focus @@ Some true;
            self#content#focus_with_keyboard
          end
        )

    method! remove_keyboard_focus =
      do_option self#keyboard_focus (fun b -> if b then self#set_keyboard_focus @@ Some false);
      self#content#remove_keyboard_focus

    val mutable cc = None

    (* return the resident widget, or Not_found *)
    method handle_widget (ev : (Event.t_rich,Event.t_win) Either.t) =
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
            let _ = widget#execute in
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
          | [%effect? (W.Await triggers), k] ->
            (* print_endline "#EOH#\n"; *)
            cc <- Some (triggers, fun ev geom -> EffectHandlers.Deep.continue k (ev,geom))
        end;
        self#display

    (* this function sends all the blits to be displayed to the layers *)
    (* it does not directly interact with the renderer *)
    (* pos0 is the position of the house containing the room *)
    method display =
      is_fresh <- false;
      match canvas with
      | None -> failwith "No canvas"
      | Some canvas ->
        (* clip contains the rect that should contain the current room r. But of
           course, clip can be much bigger than r. *)
        let g = self#geometry in
        let x = g.x in
        let y = g.y + g.voffset in
        (*print_endline ("ALPHA=" ^ (string_of_float (Avar.old room#geometry.transform.alpha)));*)

        (* background (cf compute_background)*)
        let bg = match self#background with
          | None -> []
          | Some bg ->
            let box = match bg with
              | Solid c ->
                (* let c = Draw.random_color () in *)  (* DEBUG *)
                let b = new Box.t ~size:(g.w,g.h) ~bg:(Style.Solid c) ?shadow:self#shadow () in
                self#set_background (Some (Box b));
                b
              | Box b -> b
            in
            let blits =
              box#display canvas (self#layer)
                Draw.(scale_geom {x; y; w = g.w; h = g.h; voffset = - g.voffset})
            in
            blits
        in
        (* !!! in case of shadow, the blits contains several elements!! *)

        let blits = self#content#display canvas self#layer Draw.{g with x; y} in
        let blits = List.rev_append bg blits in

        (* TODO presumably blit has clip = None as default *)
        List.iter (fun blit -> Draw.blit_to_layer { blit with clip = None }) blits
  end

(* The whole connected component of a layout is a tree, whose vertices (nodes)
   are rooms and leaves are widgets (=Resident). The number of branches (=Rooms
   list) from a vertex is arbitrary. The house field gives the parent of a
   vertex.

   There are several interesting ways of going through a tree:
   - through every vertex
   - only through leaves
   - only leaves at a common level (=same generation number)
   - nearest neighbour (left, right, up, or down) in the planar embedding
*)

(* We use words "room", "layout", and "house" for the same type of object.

   - "layout" will in general refer to the main house, ie containing everything
     that is displayed on the window.
   - "house" in general refers to a parent of some room, ie an object contaning
     sub-rooms.
   - "room" is the generic term for sub objects contained in the general layout.
*)


exception Fatal_error of (Widget.common * string)

(*let rooms_table : (int, room) Hashtbl.t = Hashtbl.create 50;;*)
(* this is where we store the reverse lookup: room#id ==> room *)
(* of course the problem is to free this when rooms are not used anymore... to
   prevent a memory leak. *)
(* TODO use weak tables (or Ephemerons ???) *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Weak.html *)

(* let of_id id = *)
(*   try Hashtbl.find rooms_table id with *)
(*   | Not_found -> failwith (Printf.sprintf "Cannot find room with id=%d" id);; *)


(* pressing the TAB key in the main loop will switch the keyboard focus to
   another room. Here we save the room that had keyboard focus just before
   pressing TAB. This global variable should be thread safe because it is
   modified only by the main loop. Another option could be to store the room_id
   in an event. (?) *)
let keyboard_focus_before_tab : Widget.common option ref = ref None

(* get the renderer of the layout *)
let renderer t = match t#canvas with
  | Some c -> c.Draw.renderer
  | _ -> failwith "Cannot get renderer because no canvas was defined"

let get_canvas l =
  match l#canvas with
  | Some c -> c
  | None ->
    raise (Fatal_error
             ((l :> Widget.common), Printf.sprintf "The room #%d is not associated with any canvas"
                l#id))

(* test if layouts share the same layer (= same depth) *)
let same_layer l1 l2 =
  Chain.(l1#layer == l2#layer)

(* if !debug is true, we replace the background by solid red *)
let delete_background room =
  printd debug_memory "Delete background for room %s" (sprint_id room);
  do_option room#background
    (fun b ->
       let () = room#set_background
           (if !debug then Some (Solid Draw.(opaque red)) else None) in
       match b with
       | Solid _ -> ()
       | Box b -> b#unload;
    )

(* force compute background at current size. Canvas must be created *)
let compute_background room =
  do_option room#background (
    fun bg ->
      let g = room#geometry in
      Sdl.log "COMPUTE BG w=%u h=%u" g.w g.h;
      let box = match bg with
        | Solid c ->
          let b = new Box.t ~size:(g.w,g.h) ~bg:(Style.Solid c) () in
          room#set_background (Some (Box b));
          b
        | Box b -> b#unload; b in
      ignore (box#display (get_canvas room) room#layer
                (Draw.scale_geom g)))

(* change background *)
(* can be called by a thread *)
(* TODO it should not be allowed to use a background of type Box in case the box
   already belongs to another room... *)
let set_background l b =
  l#unload;
  l#set_background b

(** get size of layout *)
let get_size l =
  l#geometry.w, l#geometry.h

let get_physical_size l =
  get_size l |> Draw.scale_size

(** get width of layout *)
let width l =
  l#geometry.w

(** get height *)
let height l =
  l#geometry.h

let resize_content room = room#content#resize (get_size room)

(* l must be the top house *)
let adjust_window_size l =
  if l#canvas <> None
  then let w,h = get_physical_size l in
    let win = window l in
    if (w,h) <> Sdl.get_window_size win
    then begin
      Sdl.set_window_size win ~w ~h;
      Trigger.(push_event (create_event E.window_event_resized))
    end
    else printd debug_graphics "Window for layout %s already has the required size."
        (sprint_id l)

(* Change the size of the room. By default this will cancel the resize function
   of this room. If [set_size] or its derivatives [set_width] and [set_height]
   are used as part of a layout resize function of the same room, this default
   behaviour should be disabled to prevent the resize function to cancel itself:
   use [keep_resize:true].  *)
(* TODO faire un module Resize avec keep_resize=true par défaut. *)
let set_size ?(check_window = true) ?(update_bg = false) ?w ?h (l : 'a t) =
  let () = match w,h with
    | Some w, Some h ->
      l#set_geometry { l#geometry with w; h };
    | Some w, None ->
      l#set_geometry { l#geometry with w };
    | None, Some h ->
      l#set_geometry { l#geometry with h };
    | None, None -> () in

  if update_bg && l#canvas <> None then compute_background l;
  (* = ou plutot unload_background ?? *)
  if check_window then adjust_window_size l;
  resize_content l

let set_height ?check_window ?update_bg l h =
  set_size ?check_window ?update_bg ~h l

let set_width ?check_window ?update_bg l w =
  set_size ?check_window ?update_bg ~w l

(* The public version of [set_size] *)
let set_size ?check_window ?update_bg l (w,h) =
  set_size ?check_window ?update_bg ~w ~h l

(** get current absolute x position (relative to the top-left corner of the
    window). Not necessarily up-to-date. *)
let xpos l =
  l#geometry.x

(** get current absolute y position *)
let ypos l =
  l#geometry.y

(** change x of layout, without adjusting parent house. Warning, by default this
    disables the resize function. *)
(* this is the x coordinate wrt the containing house *)
(* this won't work if there is an animation running (see Avar.set) *)
let setx l x =
  l#set_geometry { l#geometry with x }

(** change y of layout, without adjusting parent house *)
(* see above *)
let sety l y =
  l#set_geometry { l#geometry with y }

(* see above *)
(* warning, it the animation is not finished, using Avar.set has almost no
   effect *)
let set_voffset (l : 'a t) vo =
  l#set_geometry  { l#geometry with voffset = vo }

(* use this to shift the voffset by a constant amount without stopping an
   animation *)
let shift_voffset (l : 'a t) dv =
  l#set_geometry  { l#geometry with voffset = l#geometry.voffset + dv }

(* see get_window_pos. It should be set *after* Bogue.make and *before*
   Bogue.run. Otherwise it has possibly no effect, or perhaps causes some
   glitches. TODO make a test to ensure this ?? *)
let set_window_pos layout (x,y)=
  let g = layout#geometry in
  layout#set_geometry { g with x; y }

(********************)


(* use this to reset all widget textures (room + all children) for reducing
   memory. The layout can still be used without any impact, the textures will be
   recreated on the fly. If you want to really remove all created textures, you
   have to use delete_backgrounds too; but then the backgrounds will *not* be
   recreated. *)
let unload_widget_textures room = room#unload

(* same, but for all rooms + widgets *)
let unloads room =
  room#unload;
  room#content#unload

let delete_textures room =
  unloads room;
  delete_background room

let remove_canvas room =
  delete_textures room;
  room#set_canvas None

(** create a room (=layout) with a unique resident (=widget), no margin possible *)
(* x and y should be 0 if the room is the main layout *)
(* warning, the widget is always centered *)
(* x,y specification will be overwritten if the room is then included in a flat
   or tower, which is essentially always the case... *)
let resident ?name ?(x = 0) ?(y = 0) ?w ?h ?background ?canvas ?layer
    ?keyboard_focus widget =
  let widget = (widget :> 'a Widget.t) in
  let (w',h') = widget#size in
  let w = default w w' in
  let h = default h h' in
  let keyboard_focus = match keyboard_focus with
    | Some true -> Some false
    | Some false -> None
    | None -> widget#guess_unset_keyboard_focus |> (fun b -> if b then None else Some false) in
  let geometry = Draw.geometry ~x ~y ~w ~h () in
  new t ?name ?background ?keyboard_focus ?layer ?canvas geometry widget

(* Flip buffers. Here the layout SHOULD be the main layout (house) of the window
*)
(* only one canvas/renderer is used, the one specified by the layout *)
let flip ?(clear=false) ?(present=true) layout =
  printd debug_graphics "flip layout %s" (sprint_id layout);
  (* go (Sdl.set_render_target (renderer layout) None); *)
  if clear then Draw.clear_canvas (get_canvas layout);
  printd debug_graphics "Render layers";
  (* : we assume that the layout layer is in the same component as the
     current_layer... TODO do better *)
  Draw.render_all_layers (layout#layer);
  if present then begin
    printd debug_graphics "Present";
    Draw.(sdl_flip (renderer layout))
  end

(* prerender the layout to the layers *)
let render (layout : 'a t) =
  (* let renderer = renderer layout in *)
  (* go (Sdl.render_set_clip_rect renderer None); *)
  (* Draw.(set_color renderer (opaque black)); *)
  (* go (Sdl.render_clear renderer); *)
  (* Draw.clear_canvas (get_canvas layout); *)
  (* We should not clear the canvas here, since all rendering is done at the end
     of the main loop, with flip *)
  if Draw.window_is_shown (window layout) then layout#display
  else printd debug_board "Window (layout #%u) is hidden" layout#id

let flip ?clear w =
  if not w#is_fresh
  then begin
    render w;
    let clear = default clear true in
    printd debug_graphics "clear=%b" clear;
    flip ~clear ~present:true w;
    w#set_fresh true
  end
  else Draw.clear_layers w#layer

(** initialize SDL if necessary and create a window of the size of the layout *)
let make_window ?window layout =
  printd debug_graphics "Make window";
  let w,h = get_physical_size layout in
  let wmax, hmax = 4096, 4096 in
  (* = TODO ? instead clip to ri_max_texture_width,ri_max_texture_height ? *)
  if wmax < w || hmax < h
  then printd debug_error "  The layout has size (%u,%u), which exceeds the max size (%u,%u)." w h wmax hmax;
  let w = min w wmax in
  let h = min h hmax in
  let canvas = Draw.init ?window ~name:layout#name ~w ~h () in
  layout#set_canvas (Some canvas)
