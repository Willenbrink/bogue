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

open Utils
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

let box_bg b =
  Box b

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
  | Some c -> c.Draw.window
  | _ -> begin
      printd debug_error "Cannot get window for layout %s \
                          because no canvas was defined" (sprint_id t);
      raise Not_found
    end

class ['a] t ?id ?name ?(adjust = Fit)
    ?(layer = Draw.get_current_layer ())
    ?mask ?background ?shadow ?keyboard_focus ?(mouse_focus=false)
    ?(clip = false) ?canvas
    ?(is_fresh = false) ?(bogue = false)
    _geometry (content' : 'a W.t) =
  object (self)
    (* FIXME what is the size of a room? We have a resize but no base size *)
    inherit Base.common ?id ?name (0,0) ()

    (* should we adjust the size of this room to fit its content ? *)
    method adjust = adjust

    val mutable is_fresh = is_fresh
    method is_fresh = is_fresh
    method set_fresh b = is_fresh <- b

    val mutable bogue = bogue
    method bogue = bogue
    method set_bogue b = bogue <- b

    (* This field is only useful when t#show = true. Then t#hidden = true if the
       layout is currently not displayed onscreen. (Upon creation, all layouts are
       hidden.)  Only used to correctly detect if animations are running. This
       field is only set by the Layout.display function, it should not be modified
       by user.  Note that t#show has precedence for being hidden: it t#show =
       false, then t is hidden no matter what t#hidden says. *)
    val mutable hidden = true
    method hidden = hidden
    method set_hidden x = hidden <- x

    (* relative geometry wrt the house. All components are dynamic variables,
       that need to be recomputed at each iteration. Note: rooms inside a house
       must be physically inside the geometry of the house. If not, they will
       not be detected by the mouse, for instance. *)
    val mutable geometry = _geometry
    method geometry = geometry
    method set_geometry x = geometry <- x

    (* if clip=true, the room (and its children) will be clipped inside its
       geometry. This should be set whenever one want to scroll the content of
       the layout inside the layout. This is also used (and set) by hide/show
       animations. TODO replace this by a more flexible 'overflow'
       specification *)
    val mutable clip = clip
    method clip = clip
    method set_clip x = clip <- x

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
    method handle_widget ev =
      let widget = self#content in

      (* let w,h = Sdl.get_window_size @@ window (self :> 'a t) in *)
      (* Printf.printf "geom: %iw,%ih\n" w h; *)
      match cc with
      | None ->
        begin
          match widget#execute with
          | _ ->
            print_endline "Root widget terminated";
            (* TODO consider this case *)
            cc <- None;
            widget#update
          | [%effect? (W.Await triggers), k] ->
            cc <- Some (triggers, fun ev geom -> EffectHandlers.Deep.continue k (ev,geom))
        end;
      | Some (triggers, cont) ->
        let widget = self#content in
        if List.mem (Trigger.of_event ev) triggers
        then begin
          print_endline "";
          begin
            match cont ev self#geometry with
            | _ ->
              cc <- None;
              widget#update
            | [%effect? (W.Await triggers), k] ->
              cc <- Some (triggers, fun ev geom -> EffectHandlers.Deep.continue k (ev,geom))
          end
        end
        (* print_endline "handle_w return" *)

    (* initializer *)
    (*   (\* This uses deep continuations. *)
    (*      While I do not understand it in detail, this essentially means that *)
    (*      every Await thrown in the continuation will also be handled here. *)
    (*      I.e. exp will never be evaluated in try continue k with Await k -> exp *\) *)
    (*   match content#perform with *)
    (*   | _ -> failwith "Widget terminated!" *)
    (*   | [%effect? (W.Await triggers), k] -> *)
    (*     print_endline "Set cc in layout"; *)
    (*     cc <- Cc (triggers, (fun ev geom -> EffectHandlers.Deep.continue k (ev,geom))) *)
  end

let x = (None : bool t option)

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

let no_clip = ref false
(* The normal behaviour when a non-zero voffset is specified is to clip the
   layout to the original rectangle. This permits the show/hide
   animation. no_clip = true can be a good idea for debugging graphics. *)

let draw_boxes = Widget.draw_boxes
(* this is only used for debugging. This can slow down rendering quite a bit *)

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

(* return the first resident widget with show=true inside the layout, or
   Not_found *)
let rec first_show_widget layout =
  if layout#show
  then layout#content
  else raise Not_found

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

let delete_backgrounds room =
  delete_background room

let delete_textures room =
  unloads room;
  delete_backgrounds room

let remove_canvas room =
  delete_textures room;
  room#set_canvas None

(* adjust layout size to the inner content in the same layer (but not to the
   larger layouts, neither to the window) *)
(* TODO: treat margins *)
(* not used yet... *)
let rec fit_content ?(sep = Theme.room_margin/2) l =
  if l#adjust = Nothing || l#clip then ()
  else let w,h = l#content#size in
    let g' = match l#adjust with
      | Fit -> { l#geometry with w = w+sep; h = h+sep };
      | Width -> { l#geometry with w = w+sep };
      | Height -> { l#geometry with h = h+sep };
      | Nothing -> failwith "already treated case !" in
    let oldg = l#geometry in
    if g' <> oldg then begin
      printd debug_graphics "ADJUST %s to New SIZE %d,%d" (sprint_id l) w h;
      set_size l (g'.w, g'.h)
    end

let has_keyboard_focus r =
  r#keyboard_focus = Some true

let claim_focus r = Trigger.push_mouse_focus r#id

let claim_keyboard_focus r = Trigger.push_keyboard_focus r#id

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

(* Set the given widget as the new resident of the given room. If w,h are not
   specified, the size of the room will be updated by the size of the widget. *)
let change_resident ?w ?h room widget =
  printd debug_board "Replacing room %s's widget by widget #%d"
    (sprint_id room) widget#id;
  let (w',h') = widget#size in
  let w = default w w' in
  let h = default h h' in
  room#set_content widget;
  room#set_keyboard_focus (widget#guess_unset_keyboard_focus |> (fun b -> if b then None else Some false));
  set_size room (w,h)

(* sets l with the size of the top_house. In principle the (x,y) of the
   top_house should be (0,0), we don't check this here. The (x,y) of l is set to
   (0,0). Should be called dynamically after main loop starts. *)
let maximize l =
  setx l 0;
  sety l 0;

  let w,h = get_size l in
  l#set_geometry { l#geometry with h; w };
  resize_content l

(** check if a sublayer is deeper (= below = Chain.<) than the main layer, which
    (in principle) should not happen *)
let check_layers room =
  let rec loop house r =
    if Chain.(house#layer > r#layer)
    then printd debug_error "The house #%d contains a room #%d with deeper layer! (%d>%d)"
        house#id r#id (Chain.depth (house#layer)) (Chain.depth (r#layer));
  in
  loop room room

(** Set the canvas of the layout. Warning! we assume that if a room has a
    canvas, all smaller rooms already have the same canvas... *)
(* warning: this is also used when we change the layer, but the window stays the
   same *)
(* let rec set_canvas canvas room = *)
(*   if Draw.canvas_equal room#canvas canvas *)
(*   then () *)
(*   else begin *)
(*     printd debug_warning "Changing room canvas"; *)
(*     room#set_canvas canvas; *)
(*     match room#content with *)
(*       | List list -> List.iter (set_canvas canvas) list *)
(*       | Leaf _ -> () *)
(*   end;; *)
let set_canvas canvas room =
  room#set_canvas (Some canvas);
  if !debug then check_layers room

(** Set the canvas for layout and all children *)
let global_set_canvas ?(mustlock=true) room canvas =
  if mustlock then
    room#set_canvas @@ Some canvas;
  if mustlock then ()

(* compute the x,y,w,h that contains all rooms in the list *)
let bounding_geometry = function
  | [] -> printd debug_warning "Trying to find bounding_geometry of empty list";
    0,0,0,0
  | rooms ->
    let rec loop xmin ymin xmax ymax = function
      | [] -> (xmin, ymin, xmax-xmin, ymax-ymin)
      | room :: rest ->
        let x,y = room#layout.x, room#layout.y in
        loop
          (imin xmin x)
          (imin ymin y)
          (imax xmax (width room + x))
          (imax ymax (height room + y))
          rest in
    loop max_int max_int 0 0 rooms

(** ask all the subwidgets to update themselves. *)
(* in fact, this just send the redraw_event, which ask for redrawing the whole
   window. Thus, it would be enough to ask this to only one widget of the layout
   (since all widgets must be in the same window) *)
let rec ask_update room = room#content#update

(** display section *)

let debug_box ~color room x y =
  let w,h = Draw.scale_size (get_size room) in
  let x,y = Draw.scale_pos (x,y) in
  let bg = if room#mouse_focus then Some (Draw.lighter color) else None in
  let rect = Draw.rect_to_layer ?bg ~color (get_canvas room) (room#layer) (x,y) w h in
  Draw.forget_texture rect.Draw.texture;
  rect

let scale_clip clip =
  map_option clip (fun c ->
      Sdl.Rect.(create
                  ~x:(Theme.scale_int (x c))
                  ~y:(Theme.scale_int (y c))
                  ~h:(Theme.scale_int (h c))
                  ~w:(Theme.scale_int (w c))))

(** Display a room: *)
let rec display_loop x0 y0 clip0 (r : 'a t) =
  (* clip contains the rect that should contain the current room r. But of
     course, clip can be much bigger than r. *)
  let g = r#geometry in
  let x = x0 + g.x in
  let y = y0 + g.y + g.voffset in
  (* update current position, independent of clip *)
  r#set_geometry @@ { g with x; y };
  (*print_endline ("ALPHA=" ^ (string_of_float (Avar.old room#geometry.transform.alpha)));*)
  let rect = Sdl.Rect.create ~x ~y ~w:g.w ~h:g.h in

  (* if there is a nonzero offset, we perform a new clip : this is used for
     "show/hide" animation *)
  (* TODO clip should be enlarged in case of shadow *)
  let clip =
    if (*g.voffset = 0*) not r#clip || !no_clip then clip0
    else Draw.intersect_rect clip0 (Some rect)
  in
  let sclip = scale_clip clip in
  match clip with
  | Some clip_rect when not (Sdl.has_intersection clip_rect rect) ->
    (r#set_hidden @@ true;
     printd debug_warning "Room #%u is hidden (y=%d)" r#id y)
  (* because of clip, the rendered size can be smaller than what the geom
     says *)
  (* If the clip is empty, there is nothing to display. Warning: this means
     that all children will be hidden, even if they happen to pop out of
     this rect. *)
  | _ -> begin
      r#set_hidden @@ false;
      (* background (cf compute_background)*)
      let bg = map_option r#background (fun bg ->
          let box = match bg with
            | Solid c ->
              (* let c = Draw.random_color () in *)  (* DEBUG *)
              let b = new Box.t ~size:(g.w,g.h) ~bg:(Style.Solid c) ?shadow:r#shadow () in

              r#set_background @@ (Some (Box b));
              ();
              b
            | Box b -> b in
          let blits = box#display (get_canvas r) (r#layer)
              Draw.(scale_geom {x; y; w = g.w; h = g.h;
                                voffset = - g.voffset}) in
          blits) in
      (* !!! in case of shadow, the blits contains several elements!! *)

      begin
        let w = r#content in
        let blits = w#display (get_canvas r) r#layer Draw.{g with x; y} in
        let blits = match bg with
          | None -> blits
          | Some b -> List.rev_append b blits in

        (* debug boxes *)
        let blits = if !draw_boxes
          then
            let color = (255,0,0,200) in
            let rect = debug_box ~color r x y in
            rect :: blits
          else blits in

        List.iter
          (fun blit ->
             let open Draw in
             let clip = sclip in
             blit_to_layer { blit with clip }) blits
      end;
      if !draw_boxes  (* we print the room number at the end to make sure it's visible *)
      then let label = new Label.t ~font_size:7 ~fg:(Draw.(transp blue))
             (sprint_id r) in
        let geom = Draw.scale_geom {Draw.x; y; w=g.w+1; h=g.h+1; voffset = g.voffset} in
        List.iter
          Draw.blit_to_layer
          (label#display (get_canvas r) (r#layer) geom)
    end

(* this function sends all the blits to be displayed to the layers *)
(* it does not directly interact with the renderer *)
(* pos0 is the position of the house containing the room *)
let display (room : 'a t) =
  display_loop 0 0 None room

let get_focus room =
  room#mouse_focus

(* we don't lock because it will be modified only by the main loop *)
let set_focus room =
  room#set_mouse_focus true

(* we don't lock because it will be modified only by the main loop *)
let unset_focus room =
  room#set_mouse_focus false

let set_cursor roomo =
  let cursor = match roomo with
    | None -> go (Draw.create_system_cursor Sdl.System_cursor.arrow)
    | Some room -> Cursor.get room#content#cursor
  in
  Sdl.set_cursor (Some cursor)

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
  if Draw.window_is_shown (window layout) then display layout
  else printd debug_board "Window (layout #%u) is hidden" layout#id

(* the function to call when the window has been resized *)
let resize_from_window ?(flip=true) layout =
  let w,h = Sdl.get_window_size (window layout)
            |> Draw.unscale_pos in
  let w', h' = get_size layout in
  if (w',h') <> (w,h)
  then begin
    (* TODO in rare occasions, it might happen that this test is different
       from get_physical_size top <> Sdl.get_window_size win*)
    printd debug_graphics "Resize (%d,%d) --> (%d,%d)" w' h' w h;
    set_size ~check_window:false layout (w,h);
    Draw.update_background (get_canvas layout);
    if flip then Draw.sdl_flip (renderer layout)
  end
(* : somehow we need this intermediate flip so that the renderer takes into
    account the new size. Otherwise texture are still clipped to the old
    size... On the other hand it might flicker if triggered to quickly *)
(* fit_content layout;;*) (* not useful *)

let flip ?clear w =
  if not w#is_fresh
  then begin
    render w;
    let clear = default clear w#bogue in
    printd debug_graphics "clear=%b" clear;
    let present = w#bogue in
    flip ~clear ~present w;
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
  global_set_canvas layout canvas;
  layout#set_bogue true

(* adjust the window size to the top layout *)
(* This should enforced all the time *)
(* this is not execute
   Je vous conseille de regarder ce cours en jouant bien le jeu, càd en
   prenant des notes. L'avantage est que vous pouvez mettre sur pause si
   vous êtes en retard, ou si vous n'avez pas compris un passage... et
   vous pouvez accélerer si je n'arrive pas à faire une preuve ;)

   d immediately, but sent to Sync *)
(* TODO move this directly to the render loop, since it has to be done anyway,
   and show not be done more than once per step. *)
(*
let adjust_window ?(display=false) layout =
  Sync.push (fun () ->
      let top = top_house layout in
      if not (Widget.equal layout top)
      then printd debug_error "The layout for resizing window should be the top layout";
      let w,h = get_physical_size top in
      let win = window top in
      printd debug_graphics "SDL set window size %d x %d" w h;
      Sdl.set_window_size win ~w ~h;


      (* resize ~flip:display top; *)
      (* : of course, top didn't really change size, but somehow the texture was
         clipped to the old window size, and if we don't update it, the previous
         clipped texture is stretched to the new window size. *)
      (* render top; *)
      (* flip top; *)
      (* Draw.(flip top.canvas.renderer); *)

      (* Now we render and flip. This is not strictly necessary, as it will surely
         be done by the main loop anyway. But it doesn't hurt to do it twice... *)
      (* it should not be done if the window is hidden, because render targets
         don't work well *)
      if display && Draw.window_is_shown (window top) then begin
        render top;
        flip top
      end)
  *)
(*Draw.destroy_textures ();; *)

(* the display function we export *)
(* NO we need pos for snapshot.ml *)
(*let display r : unit =
  display r;;*)

let inside_geom geometry (x,y) =
  x <= geometry.x + geometry.w && x >= geometry.x &&
  y <= geometry.y + geometry.h && y >= geometry.y

let inside room (x,y) =
  match room#mask with
  | None -> inside_geom room#geometry (x,y)
  | Some mask -> (* TODO vérifier aussi qu'on est dans la dimension du mask *)
    let x0,y0 = room#geometry.x, room#geometry.y in
    let _,_,_,a = Draw.get_pixel_color mask ~x:(x-x0) ~y:(y-y0) in
    a <> 0

(* instead of the first one, get the complete list *)
(* in each layer, the first element of the list has priority (TODO this is not
   consistent with the fact that it is the last displayed) *)
(* cf remarks above *)
let rec focus_list x y t =
  if t#show && (inside t (x,y)) then [t#content]
  else []

(* get the focus element in the top layer *)
let top_focus x y (t : 'a t) =
  if inside t (x,y)
  then Some t#content
  else None

(** get SDL windows id, in case the canvas was created *)
let id w =
  Sdl.get_window_id (window w)
