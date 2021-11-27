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

type background = (* TODO instead we should keep track of how the box was created... in case we want to recreate (eg. use it for another window... ?) *)
  (* TODO use Style.background ? Cependant Style est antérieur (et utilisé par)
     à Box... *)
  | Solid of Draw.color
  | Box of Box.t

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

type transform =
  { angle : float Avar.t;
    center : (Sdl.point option) Avar.t;
    flip : Sdl.flip Avar.t;
    alpha : float Avar.t
  }

type geometry = {
  x : int Avar.t;
  y : int Avar.t;
  (* the (x,y) coords define the position of the layout wrt its container (the
     house). Origin is top-left. *)
  w : int Avar.t;
  h : int Avar.t;
  voffset : (int Avar.t) Var.t;
  (* the voffset is the vertical offset = the y value of where the content of
     the layout will be drawn. It is typically used for scrolling. It is similar
     to the 'y' variable', except that:

     1. the clipping rect (if defined) is *not* translated in case of voffset
     2. the background is not translated either *)
  transform: transform;
}

type current_geom = {
  x : int;
  y : int;
  w : int;
  h : int;
  voffset : int; }

(* convert between same type in Draw... *)
let to_draw_geom (g : current_geom) =
  { Draw.x = g.x; Draw.y = g.y; Draw.w = g.w; Draw.h = g.h; Draw.voffset = g.voffset }

(* in principle, rooms in a house with the same layer should have
   non-intersecting geometries (this can be violated, eg. with
   Layout.superpose). Popups are drawn on a different layer *)
type 'a content = Leaf of Widget.t | List of 'a list

let fresh_id = fresh_int ()

(** make geometry *)
let geometry ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(voffset=0) ?transform () : geometry =
  { x = Avar.var x;
    y = Avar.var y;
    w = Avar.var w;
    h = Avar.var h;
    voffset = Var.create (Avar.var voffset);
    transform = default transform { angle = Avar.var 0.;
                                    center = Avar.var None;
                                    flip = Avar.var Sdl.Flip.none;
                                    alpha = Avar.var 1.}
  }

(** list of all integer dynamical variables *)
let get_int_avars room =
  let g = (room#geometry : geometry) in [g.x; g.y; g.w; g.h; Var.get g.voffset]

let current_geom ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(voffset=0) () : current_geom =
  { x; y; w; h; voffset}

(* transform geometry into current_geom *)
(* lock the layout ? *)
let to_current_geom (g : geometry) : current_geom =
  { x = Avar.get g.x;
    y = Avar.get g.y;
    w = Avar.get g.w;
    h = Avar.get g.h;
    voffset = Avar.get (Var.get g.voffset) }

let sprint_id r =
  Printf.sprintf "#%u%s" r#id (match r#name with
      | "" -> ""
      | s -> Printf.sprintf " (%s)" s)

class room ?id ?name ?(set_house = true) ?(adjust = Fit)
    ?(resize = fun _ -> ()) ?(layer = Draw.get_current_layer ())
    ?mask ?background ?shadow ?house ?keyboard_focus ?(mouse_focus=false)
    ?(show = true) ?(clip = false) ?(draggable = false) ?canvas
    _geometry content =
  object (self)
    inherit Base.common ?id ?name ()

    (* should we adjust the size of this room to fit its content ? *)
    method adjust = adjust

    val mutable resize : (int * int) -> unit = (fun _ -> ())
    method resize = resize
    method set_resize x = resize <- x

    val mutable show = show
    method show = show
    method set_show x = show <- x

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
    val mutable geometry : geometry = _geometry
    method geometry = geometry
    method set_geometry x = geometry <- x

    (* the current *absolute* geometry. Is updated at each display. But because
       of clip, the actual rendered size can be smaller than indicated
       size. Before the start of the main loop, it is equal to the initial
       values of the geometry field *)
    (* a special case of current_geom.(x,y) is to specify window position for
       the top layouts. See set_window_pos *)
    val mutable current_geom = to_current_geom _geometry
    method current_geom = current_geom
    method set_current_geom x = current_geom <- x

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

    val mutable shadow : Style.shadow option = shadow
    method shadow = shadow
    method set_shadow x = shadow <- x

    (* If there is a mask, a position (x,y) will be declared inside the layout
       if it corresponds to a mask pixel with alpha value <> 0. A mask will act
       as a clip if it is uniformly white, and the shape is given by nonzero
       alpha values. (TODO) *)
    method mask : Sdl.surface option = mask

    val mutable content : room content = content
    method content = content
    method set_content x = content <- x

    method children =
      match self#content with
      | Leaf x -> [(x :> Widget.common)]
      | List xs -> List.map (fun x -> (x :> Widget.common)) xs
                   @ List.concat_map (fun x -> (x :> Widget.common)#children) xs

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

    (* house = parent: this is the "room" that contains this room in his "Rooms"
    *)
    (* this is mutable because of cyclic definition: one cannot set the house
       before defining it... It is our responsibility to make sure that the house
       really corresponds to the parent element, in order to avoid cycles etc. *)
    (* cache : Sdlvideo.surface; *) (* ou texture ? mettre un cache pour
                                       accélerer l'affichage, plutôt que d'effacer
                                       tout à chaque itération ? *)
    val mutable house = house
    method house = house
    method set_house x = house <- x

    (* set interactively when has mouse focus *)
    val mutable mouse_focus = mouse_focus
    method mouse_focus = mouse_focus
    method set_mouse_focus x = mouse_focus <- x

    (* None = cannot have focus; Some b = has focus or not *)
    (* TODO: should we move the keyboard_focus to the Widget ? A layout which
       contains a Rooms list cannot really have keyboard_focus...and in fact it
       will not be detected by 'next_keyboard' *)
    (* TODO : mutable draggable : int option; *) (* None = not draggable; Some
                                                    delay = drag after delay (in ms) *)
    val mutable keyboard_focus : bool option = keyboard_focus
    method keyboard_focus = keyboard_focus
    method set_keyboard_focus x = keyboard_focus <- x

    (* TODO keep_focus_on_pressed: bool (default = true) CF. menu2. BUT It's not
       so easy because many layouts can cover a widget. Ideally, this property
       should belong to the widget. *)
    val mutable draggable = draggable
    method draggable = draggable
    method set_draggable x = draggable <- x

    initializer
      (* we update the resident room_id field *)
      (* we update the content's house field *)
      let () = match content with
        | Leaf _ -> ()
        | List list -> if set_house
          then List.iter (fun r -> r#set_house (Some (self :> room))) list
      in
      (*Gc.finalise free room;*)
      (* We don't do this because who knows when the background texture will be
         destroyed.... maybe too late (after renderer was destroyed). TODO: what to
         do to make sure the background is destroyed if the layout is not used
         anymore (and we don't destroy the renderer) ? Only solution I see is to add
         the renderer information to the texture... *)
      printd debug_board "Layout %s created." (sprint_id self)
  end

let equal r1 r2 = r1#id = r2#id
let (==) = equal

module Hash = struct
  type t = room
  let equal = equal
  let hash room = room#id
end

module WHash = Weak.Make(Hash)

(* this is a weak set of all created rooms, searchable by their unique id. *)
(* it is weak in the sense that rooms can be reclaimed by the GC when not
   anymore in use, and automatically disappear from the set *)
let rooms_wtable = WHash.create 50


class t ?id ?name ?(set_house = true) ?(adjust = Fit)
    ?(resize = fun _ -> ()) ?(layer = Draw.get_current_layer ())
    ?mask ?background ?shadow ?house ?keyboard_focus ?(mouse_focus=false)
    ?(show = true) ?(clip = false) ?(draggable = false) ?canvas
    _geometry content =
  object (self)
    inherit room ?id ?name ~set_house ~adjust ~resize ~layer ?mask ?background
        ?shadow ?house ?keyboard_focus ~mouse_focus ~show ~clip ~draggable ?canvas _geometry
        content

    initializer
      (* remove is in principle not necessary *)
      WHash.add rooms_wtable (self :> room)

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


exception Fatal_error of (t * string)

(* a special value used to indicate that the window position should be guessed
   by the program. TODO don't use this nasty trick. *)
let not_specified = -66666


let no_clip = ref false
(* The normal behaviour when a non-zero voffset is specified is to clip the
   layout to the original rectangle. This permits the show/hide
   animation. no_clip = true can be a good idea for debugging graphics. *)

let draw_boxes = Widget.draw_boxes
(* this is only used for debugging. This can slow down rendering quite a bit *)

(* Only for debugging: we insert here the room ids we think are not used
   anymore. Then we can check if the GC did remove them from the rooms_wtable *)
let cemetery = ref []
let send_to_cemetery room =
  cemetery := room#id :: !cemetery
(* TODO: use GC.finalise to automatically unload (but not destroy) textures from
   GCed layouts ? *)
(* (or maybe better for GCed widgets) *)

let rec remove_wtable room =
  if WHash.mem rooms_wtable room
  then begin
    printd debug_memory "Removing room %s from Wtable" (sprint_id room);
    WHash.remove rooms_wtable room;
    if WHash.mem rooms_wtable room
    then begin
      printd debug_error
        "Several instances of room %s are registered in the weak hash table."
        (sprint_id room);
      remove_wtable room;
      (* The hash can host several instances of the room. However this signals
         a bug somewhere. *)
    end;
    send_to_cemetery room;
  end

let clear_wtable () = WHash.clear rooms_wtable

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
let keyboard_focus_before_tab : t option ref = ref None

(* specialized version for creating the list of all windows (= top layouts) *)
let create_win_house windows =
  new t ~set_house:false ~name:"windows_house" (geometry ()) (List windows)

let has_resident layout =
  match layout#content with
  | Leaf _ -> true
  | List _ -> false

let create = new t ~set_house:true

(* the dummy room is only used to search the Weak table *)
let dummy_room id =
  let x = create ~id (geometry ()) (List []) in
  x

let of_id_unsafe id : room =
  try WHash.find rooms_wtable (dummy_room id) with
  | Not_found -> (printd debug_warning "Cannot find room with id=%d" id;
                  raise Not_found)

(* A detached room is a layout that does not belong to the current layout tree.
   Checking house = None is not sufficient, as it is allowed to have a unique
   top layout containing a resident.  Note, we could also add a 'detached' field
   in the layout type. *)
let is_detached room =
  room#house = None && not (has_resident room)

(* This one is more secure: we check if the layout is not detached. *)
let of_id_opt ?not_found id : room option =
  match WHash.find_opt rooms_wtable (dummy_room id) with
  | None -> (printd debug_error "Cannot find room with id=%d" id;
             do_option not_found run;
             None)
  | Some r as o ->
    if is_detached r
    then (printd debug_error "Trying to access the detached room #%d" id; None)
    else o

(* WARNING: in "iter" and in all other search functions below, recall that
   itering though a room is tricky because of mutability and threading. The
   structure of the tree can be changed by another thread while we iter. Most
   dangerous: it can also be changed by the itering itself, hehe. If necessary,
   doing "iter lock room" should minimize the risk (but not 100%: the tree can
   still be modified while we are locking..) *)

(* iter through all the rooms (layouts & widgets) contained in the layout *)
(* top to bottom *)
let rec iter f room =
  f room;
  match room#content with
  | Leaf _ -> ()
  | List list -> List.iter (iter f) list

(* iter through widgets *)
let rec iter_widgets f room =
  match room#content with
  | Leaf (w) -> f (w :> Widget.t)
  | List list -> List.iter (iter_widgets f) list

(* iter the direct children *)
let iter_rooms f house =
  match house#content with
  | Leaf _ -> printd (debug_error + debug_board) "Layout %s has no rooms: cannot iter." (sprint_id house)
  | List list -> List.iter f list

(* returns the list of rooms of the layout, or Not_found if there is a
   resident *)
let get_rooms layout =
  match layout#content with
  | Leaf _ ->
    printd debug_error
      "This layout %s is a leaf, not a node: \
       it does not contain a list of rooms" (sprint_id layout);
    raise Not_found
  | List list -> list

let siblings room =
  match room#house with
  | None -> printd debug_error
              "Cannot get siblings of room %s because it does not belong to any \
               house." (sprint_id room);
    []
  | Some house -> get_rooms house

(* return the resident widget, or Not_found *)
let widget layout =
  match layout#content with
  | List _ ->
    printd debug_error
      "This room %s is a node, not a leaf: \
       it does not contain a resident widget" (sprint_id layout);
    raise Not_found
  (* or, return the first available widget with next_widget ? *)
  | Leaf w -> w

(* return the first resident widget with show=true inside the layout, or
   Not_found *)
let rec first_show_widget layout =
  if layout#show
  then match layout#content with
    | Leaf w ->
      (printd debug_board "first_show_widget selects %u" w#id; w)
    | List rooms ->
      let rec loop = function
        | [] -> raise Not_found
        | r::rest -> try first_show_widget r with Not_found -> loop rest in
      loop rooms
  else raise Not_found

(* only for debugging: *)
(* check if rooms sent to cemetery have effectively been removed by GC *)
let check_cemetery () =
  let check id = try
      let r = of_id_unsafe id in
      printd debug_memory "Dead room %s seems to be living. Beware of zombies." (sprint_id r);
      false
    with
    | Not_found -> printd debug_memory "Dead room #%u was correctly burried by the GC. RIP." id;
      true
  in
  let rec loop list newlist empty = (* easier to use a Queue *)
    match list with
    | [] -> empty, newlist
    | id::rest ->
      if check id
      then loop rest newlist empty
      else loop rest (id :: newlist) false in
  let empty, newlist = loop !cemetery [] true in
  cemetery := newlist;
  empty

(* get the renderer of the layout *)
let renderer t = match t#canvas with
  | Some c -> c.Draw.renderer
  | _ -> failwith "Cannot get renderer because no canvas was defined"

(* get the window of the layout *)
let window t = match t#canvas with
  | Some c -> c.Draw.window
  | _ -> begin
      printd debug_error "Cannot get window for layout %s \
                          because no canvas was defined" (sprint_id t);
      raise Not_found
    end

(* return the top-level layout *)
(* This is relevent only once the main loop has started. Before this, the
   top_house is not even created, so it will not return what you expect. *)
let rec top_house layout =
  match layout#house with
  | None -> layout
  | Some r -> top_house r

let is_top layout =
  layout#house = None

let get_canvas l =
  match l#canvas with
  | Some c -> c
  | None ->
    raise (Fatal_error
             (l, Printf.sprintf "The room #%d is not associated with any canvas"
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

(* this can be used to force recreating the background, for instance after
   changing the size of the room *)
let unload_background room =
  do_option room#background (function
      | Box b -> b#unload
      | Solid _ -> ())

(* force compute background at current size. Canvas must be created *)
let compute_background room =
  do_option room#background (
    fun bg ->
      let g = room#current_geom in
      Sdl.log "COMPUTE BG w=%u h=%u" g.w g.h;
      let box = match bg with
        | Solid c ->
          let b = new Box.t ~size:(g.w,g.h) ~bg:(Style.Solid c) () in
          room#set_background (Some (Box b));
          b
        | Box b -> b#unload; b in
      ignore (box#display (get_canvas room) room#layer
                (Draw.scale_geom (to_draw_geom g))))

(* change background *)
(* can be called by a thread *)
(* TODO it should not be allowed to use a background of type Box in case the box
   already belongs to another room... *)
let set_background l b =
  unload_background l;
  l#set_background b

let set_shadow l s =
  l#set_shadow s

(** get size of layout *)
let get_size l =
  l#current_geom.w, l#current_geom.h

let get_physical_size l =
  get_size l |> Draw.scale_size

(** get width of layout *)
let width l =
  l#current_geom.w

(** get height *)
let height l =
  l#current_geom.h

let resize room =
  do_option room#house (fun house ->
      room#resize (get_size house))

let disable_resize room =
  room#set_resize (fun _ -> ())

let fix_content house =
  iter_rooms disable_resize house

let resize_content room =
  match room#content with
  | List list -> List.iter resize list
  | Leaf (w) -> w#resize (get_size room)

(* l must be the top house *)
let adjust_window_size l =
  if not (is_top l)
  then printd debug_error "[adjust_window_size] should only be called with a top \
                           house, what %s is not." (sprint_id l)
  else if l#canvas <> None
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
let set_size ?(keep_resize = false) ?(check_window = true) ?(update_bg = false) ?w ?h (l : t) =
  let () = match w,h with
    | Some w, Some h ->
      l#set_current_geom { l#current_geom with w; h };
      Avar.set l#geometry.h h;
      Avar.set l#geometry.w w
    | Some w, None ->
      l#set_current_geom { l#current_geom with w };
      Avar.set l#geometry.w w
    | None, Some h ->
      l#set_current_geom { l#current_geom with h };
      Avar.set l#geometry.h h
    | None, None -> () in

  if update_bg && l#canvas <> None then compute_background l;
  (* = ou plutot unload_background ?? *)
  if not keep_resize then disable_resize l;
  if check_window && is_top l then adjust_window_size l;
  resize_content l

let set_height ?keep_resize ?check_window ?update_bg l h =
  set_size ?keep_resize ?check_window ?update_bg ~h l

let set_width ?keep_resize ?check_window ?update_bg l w =
  set_size ?keep_resize ?check_window ?update_bg ~w l

(* The public version of [set_size] *)
let set_size ?keep_resize ?check_window ?update_bg l (w,h) =
  set_size ?keep_resize ?check_window ?update_bg ~w ~h l

(** get voffset *)
let get_voffset (l : t) =
  (* l#current_geom.voffset;; *)
  Avar.get (Var.get l#geometry.voffset)

(** get current absolute x position (relative to the top-left corner of the
    window). Not necessarily up-to-date. *)
let xpos l =
  l#current_geom.x

(** get current absolute y position *)
let ypos l =
  l#current_geom.y

(* left absolute coordinate of the layout's house *)
let x_origin l = match l#house with
  | None -> 0
  | Some h -> xpos h

(* top absolute coordinate of the layout's house *)
let y_origin l = match l#house with
  | None -> 0
  | Some h -> ypos h

(* position of room relative to house *)
let pos_from house room =
  xpos room - xpos house, ypos room - ypos house

(** get current x value.  *)
(* maybe we should lock l# But at this moment it is used in places where l is
   already locked anyway. *)
(* WARNING don't use this inside an animation for x ! It will loop
   forever. Instead use Avar.old l#geometry.x *)
let getx (l : t) =
  Avar.get l#geometry.x

let get_oldx (l : t) =
  Avar.old l#geometry.x

(** get current y value *)
let gety (l : t) =
  Avar.get l#geometry.y

let get_oldy (l : t) =
  Avar.old l#geometry.y

(** change x of layout, without adjusting parent house. Warning, by default this
    disables the resize function. *)
(* this is the x coordinate wrt the containing house *)
(* this won't work if there is an animation running (see Avar.set) *)
let setx ?(keep_resize = false) l x =

  let x0 = getx l in
  l#set_current_geom { l#current_geom with x = l#current_geom.x + x - x0 };
  Avar.set l#geometry.x x;
  if not keep_resize then disable_resize l; (* TODO à vérifier, cf dans "flat" et "tower" *)
  ()

(** change y of layout, without adjusting parent house *)
(* see above *)
let sety ?(keep_resize = false) l y =

  let y0 = get_oldy l in
  l#set_current_geom { l#current_geom with y = l#current_geom.y + y - y0 };
  Avar.set l#geometry.y y;
  if not keep_resize then disable_resize l;
  ()

(* see above *)
(* warning, it the animation is not finished, using Avar.set has almost no
   effect *)
let set_voffset (l : t) vo =

  Avar.set (Var.get l#geometry.voffset) vo;
  l#set_current_geom  { l#current_geom with voffset = vo };
  ()

(* use this to shift the voffset by a constant amount without stopping an
   animation *)
let shift_voffset_generic vset (l : t) dv =
  let av = Var.get l#geometry.voffset in
  if Avar.finished av
  then set_voffset l (Avar.get av + dv)
  else let av_new = Avar.apply (fun y -> y + dv) av in
    (* let _ = Avar.get av_new in *) (* in order to start the animation. Useful ?? *)
    vset l#geometry.voffset av_new

let shift_voffset = shift_voffset_generic Var.set
let shift_voffset_unsafe = shift_voffset_generic Var.unsafe_set

(* not used... *)
let reset_pos l =

  let w,h = get_size l in
  let g = geometry ~w ~h () in (* or modify l#geometry fields in-place ? *)
  l#set_geometry g;
  l#set_current_geom to_current_geom g;
  ()

(* a special use of current_geom is to indicate the desired window position
   within the desktop at startup. It should be set *after* Bogue.make and
   *before* Bogue.run *)
let get_window_pos layout =
  let f x = if x = not_specified then None else Some x in
  f layout#current_geom.x, f layout#current_geom.y

(* see get_window_pos. It should be set *after* Bogue.make and *before*
   Bogue.run. Otherwise it has possibly no effect, or perhaps causes some
   glitches. TODO make a test to ensure this ?? *)
let set_window_pos layout (x,y)=
  let g = layout#current_geom in
  layout#set_current_geom { g with x; y }

(* lock l ? *)
let get_transform l =
  let angle = Avar.get l#geometry.transform.angle in
  let center = Avar.get l#geometry.transform.center in
  let flip = Avar.get l#geometry.transform.flip in
  let alpha = Avar.get l#geometry.transform.alpha in
  Draw.make_transform ~angle ?center ~flip ~alpha ()

let get_alpha l =
  Avar.get l#geometry.transform.alpha

let rec rec_set_show b l =
  l#set_show b;
  match l#content with
  | Leaf _ -> ()
  | List list -> List.iter (rec_set_show b) list

(** return absolute (x,y) position *)
(* TODO optimize: test if x is up_to_date, then one can use current_geom instead ? *)
(* of course this test will fail for hidden rooms *)
let compute_pos room =
  let rec loop x0 y0 (r : t) =
    let x,y = x0 + (Avar.get r#geometry.x),
              y0 + (Avar.get r#geometry.y) + (Avar.get (Var.get r#geometry.voffset)) in
    match r#house with
    | None -> x,y
    | Some h -> loop x y h in
  loop 0 0 room

(** get absolute position of the parent house *)
let house_pos room =
  match room#house with
  | None -> 0,0
  | Some h -> compute_pos h;;

(* not used, just to fix the vocabulary "leaf" *)
let is_leaf room =
  match room#content with
  | Leaf _
  | List [] -> true
  | _ -> false

(* return the first resident *below (or including) room* for which test w =
    true, or None *)
let rec find_resident test room =
  match room#content with
  | Leaf w -> if test w then Some room else None
  | List list -> let rec loop = function
      | [] -> None
      | r :: rest -> let f = find_resident test r in
        if f = None then loop rest else f in
    loop list

(* search through the whole component of the layout (children and parents)
   starting from top house containing room *)
exception Found of t
let search room scan =
  if scan room then Some room
  (* =just in case it might speed-up things: room is the "initial guess" *)
  else let f r = if scan r then raise (Found r) in
    try
      iter f (top_house room);
      raise Not_found
    with
    | Found r -> printd debug_warning "Search OK"; Some r
    | Not_found -> printd debug_error "Search produced no result!"; None
    | e -> raise e

(** find room by id in the connected component of house *)
(* cf Layout.of_id *)
let find_room_old house id =
  printd debug_warning "Search room #%d in %d..." id (house#id);
  let scan r = r#id = id in
  search house scan

(* let set_canvas canvas room = *)
(*   iter (fun r -> r#set_canvas @@ canvas);; *)

(* find the next room in the same level of the house. In circular mode, after
   the last one comes the first. In non circular mode, if room is the last one,
   we return None *)
let next ?(circular=false) room =
  match room#house with
  | None -> (* we must be in top_house *)
    None
  | Some h ->
    let rooms = get_rooms h in (* It should not be empty since room is inside. *)
    let first = List.hd rooms in
    let rec loop list ok = match list with
      | [] -> if ok then if circular then Some first else None
        else failwith "Room should be in the list!"
      | a::rest -> if ok then Some a else loop rest (equal a room) in
    loop rooms false

(* find the "first" (and deepest) room (leaf) contained in the layout by going
   deep and choosing always the first room of a house *)
(* WARNING a room with empty content is considered a leaf too *)
let rec first_room r =
  printd debug_board "Descending to room %s" (sprint_id r);
  match r#content with
  | Leaf _ -> r
  | List [] -> r
  | List (a::_) -> first_room a

(* find a 'uncle': a room next to some parent room, going up in generation. *)
let rec next_up r =
  check_option r#house (fun h ->
      default_option (next h) (next_up h))

(* find the next leaf (=room containing a widget, or empty) in the whole
   layout *)
(* we first look at the same level, then below, then upstairs. *)
(* repeated calls to this function will visit the whole connected component --
   although this is not the optimal way to visit everything, of course -- and
   start over indefinitely. Thus you should check when the returned room is the
   one you started with... (which means you should start with a leaf !) *)
let next_leaf room =
  match room#content with
  | List []
  | Leaf _ -> begin
      match next room with
      | None -> (* last one at this level; we go upstairs *)
        let h = default
            (next_up room)
            (printd debug_board "No next widget was found; \
                                 we start again from top";
             top_house room) in
        first_room h
      | Some n -> (match n#content with
          | Leaf _ -> n
          | List _ -> first_room n)
    end
  | List _ -> first_room room

(** find the next visible room with a widget that can have keyboard_focus *)
(* TODO this is buggy. See example25 *)
let next_keyboard_old room =
  let room = first_room room in
  (* : we make sure we start at a leaf: a room with either en Resident, or with
     an empty Rooms. *)
  let visited = ref [] in (* just for debugging *)
  let rec loop r =
    if List.mem r#id !visited
    then (printd debug_error "Room %s already visited (this should not happen). Quitting search." (sprint_id r); assert(false))
    else begin
      visited := r#id :: !visited;
      printd debug_board "Searching next room %s" (sprint_id r);
      let n = next_leaf r in
      if equal room n then (printd debug_board "Nothing found"; None)
      else if n#keyboard_focus <> None && n#show
      then (printd debug_board "Found %s" (sprint_id n); Some n)
      else loop n
    end in
  loop room

let next_keyboard room =
  let rec loop r visited =
    if List.mem r#id visited then None (* this happens sometimes (why??) *)
    else
      let n = next_leaf r in
      if equal room n then (printd (debug_board+debug_custom) "No keyboard_focus found"; None)
      else if n#keyboard_focus <> None && n#show
      then (printd (debug_board+debug_custom) "Found %s" (sprint_id n); Some n)
      else loop n (r#id :: visited) in
  loop room []

(********************)


(* use this to reset all widget textures (room + all children) for reducing
   memory. The layout can still be used without any impact, the textures will be
   recreated on the fly. If you want to really remove all created textures, you
   have to use delete_backgrounds too; but then the backgrounds will *not* be
   recreated. *)
let unload_widget_textures room =
  unload_background room;
  iter_widgets (fun o -> o#unload) room

(* same, but for all rooms + widgets *)
let unloads room =
  let f r =
    unload_background r;
    match r#content with
    | Leaf (w) -> w#unload
    | _ -> () in
  iter f room

let delete_backgrounds room =
  iter delete_background room

let delete_textures room =
  unloads room;
  delete_backgrounds room

let remove_canvas room =
  delete_textures room;
  iter (fun r -> r#set_canvas None) room

(* What to do when a layout is not used anymore ? *)
(* First check that it is detached from its house. *)
(* If it has children, they will become orphans  *)
(* (because a room should NOT belong to several different houses). *)
(* The resulting orphans are not freed. They are still available to use in a new
   room (see eg. longlist.ml). Use kill_all if you want to recursively free all
   subrooms *)
(* mask and background are not freed because nothing prevents them from being
   shared with another object (which is maybe not a good idea...) We leave this
   to the GC... (?) *)
(* TODO: use GC.finalise ? *)
(* WARNING: be careful it's quite easy to forget that something else points to
   the layout... or its children. This is easily the case for instance with
   Bogue.board fields like windows_house, mouse_focus, keyboard_focus,
   button_down... Not to mention widgets, which refer indirectly to layouts via
   their id... So it's preferable never to use this... *)
(* not used yet *)


(* When to call this ? *)
(* in particular, when this function is called, the layout l in principle has
   already been removed from rooms_wtable *)
let free l =
  printd debug_memory "Freeing Layout %s" (sprint_id l);
  unload_background l;
  begin match l#content with
    | Leaf _ -> ()
    | List list ->
      List.iter (fun r ->
          do_option r#house (fun h ->
              if equal h l
              then printd debug_warning "Room %s is now orphan" (sprint_id r))) list
  end

(**********)

(* Use this to shift all current_geometries before inserting a room inside a
   house. This can be needed because inserting will trigger fit_content which
   uses current_geom *)
let global_translate room dx dy =
  do_option room#house (fun h ->
      printd debug_warning
        "You are translating the current_geom of room #%u which already has a \
         house #%u. This is likely to have no effect, as the current_geom is \
         automatically updated at each display" room#id h#id);
  iter (fun r ->
      r#set_current_geom { r#current_geom with x = r#current_geom.x + dx;
                                               y = r#current_geom.y + dy }) room

(* adjust layout size to the inner content in the same layer (but not to the
   larger layouts, neither to the window) *)
(* TODO: treat margins *)
(* not used yet... *)
let rec fit_content ?(sep = Theme.room_margin/2) l =
  if l#adjust = Nothing || l#clip then ()
  else let w,h = match l#content with
      | Leaf (widget) -> widget#size
      (* | List [r] -> r#geometry.w, r#geometry.h *)
      | List list ->
        let x0 = l#current_geom.x in
        let y0 = l#current_geom.y in
        ( List.fold_left (fun m r ->
              if same_layer r l then imax m (r#current_geom.x - x0 + r#current_geom.w)
              else m)
              0 list,
          List.fold_left (fun m r ->
              if same_layer r l then imax m (r#current_geom.y - y0 + r#current_geom.h)
              else m)
            0 list )
    in
    let g' = match l#adjust with
      | Fit -> { l#current_geom with w = w+sep; h = h+sep };
      | Width -> { l#current_geom with w = w+sep };
      | Height -> { l#current_geom with h = h+sep };
      | Nothing -> failwith "already treated case !" in
    let oldg = l#current_geom in
    if g' <> oldg then begin
      printd debug_graphics "ADJUST %s to New SIZE %d,%d" (sprint_id l) w h;
      set_size l (g'.w, g'.h);
      do_option l#house fit_content  (* we adjust the parent (???) *)
    end

(** return the list of widgets used inside the layout *)
let rec get_widgets layout =
  match layout#content with
  | List h -> List.flatten (List.map get_widgets h)
  | Leaf w -> [w]

let has_keyboard_focus r =
  r#keyboard_focus = Some true

(** set keyboard_focus if possible *)
(* we don't lock because it will be modified only by the main loop *)
let set_keyboard_focus r =
  do_option r#keyboard_focus (fun b ->
      if not b then begin
        printd debug_board "Setting layout keyboard_focus";
        r#set_keyboard_focus @@ Some true;
        match r#content with
        | List _ -> ()
        | Leaf w -> w#set_keyboard_focus
      end
    )

let rec remove_keyboard_focus r =
  do_option r#keyboard_focus (fun b -> if b then r#set_keyboard_focus @@ Some false);
  match r#content with
  | List list -> List.iter remove_keyboard_focus list
  | Leaf w -> w#remove_keyboard_focus

let claim_focus r =
  if has_resident r then Trigger.push_mouse_focus r#id
  else printd debug_error "Cannot claim focus on room %s without resident."
      (sprint_id r)

let claim_keyboard_focus r =
  if has_resident r then Trigger.push_keyboard_focus r#id
  else printd debug_error "Cannot claim keyboard_focus on room %s without \
                           resident." (sprint_id r)

(* center vertically the rooms of the layout (first generation only) *)
let v_center layout y0 h =
  match layout#content with
  | Leaf _ -> ()
  | List rs -> List.iter
                 (fun r -> let h0 = height r in
                   let y = Draw.center y0 h h0 in
                   sety r y)
                 rs

(** vertical align *)
(* v_center is the same as v_align ~align:Draw.Center *)
let v_align ~align layout y0 h =
  match layout#content with
  | Leaf _ -> ()
  | List rs -> List.iter
                 (fun r -> let h0 = height r in
                   let y = Draw.align align y0 h h0 in
                   sety r y)
                 rs

(** create a room (=layout) with a unique resident (=widget), no margin possible *)
(* x and y should be 0 if the room is the main layout *)
(* warning, the widget is always centered *)
(* x,y specification will be overwritten if the room is then included in a flat
   or tower, which is essentially always the case... *)
let resident ?name ?(x = 0) ?(y = 0) ?w ?h ?background ?draggable ?canvas ?layer
    ?keyboard_focus widget =
  let widget = (widget :> Widget.t) in
  let (w',h') = widget#size in
  let w = default w w' in
  let h = default h h' in
  let keyboard_focus = match keyboard_focus with
    | Some true -> Some false
    | Some false -> None
    | None -> widget#guess_unset_keyboard_focus |> (fun b -> if b then None else Some false) in
  let geometry = geometry ~x ~y ~w ~h () in
  create ?name ?background ?keyboard_focus ?draggable ?layer ?canvas
    geometry (Leaf widget)
let of_widget = resident

(* Set the given widget as the new resident of the given room. If w,h are not
   specified, the size of the room will be updated by the size of the widget. *)
let change_resident ?w ?h room widget =
  match room#content with
  | Leaf (resid) ->
    printd debug_board "Replacing room %s's widget by widget #%d"
      (sprint_id room) widget#id;
    let (w',h') = widget#size in
    let w = default w w' in
    let h = default h h' in
    room#set_content (Leaf widget);
    room#set_keyboard_focus (widget#guess_unset_keyboard_focus |> (fun b -> if b then None else Some false));
    set_size room (w,h)
  | _ -> printd debug_event "[change_resident]: but target room has no resident!"

(* An empty layout can reserve some space without stealing focus (and has no
   keyboard_focus) *)
(* WARNING TODO in the search functions, we have assumed rooms where never
   empty... *)
let empty ?name ?background ~w ~h () =
  let geometry = geometry ~w ~h () in
  create ?name ?background geometry (List [])

(* simple resize function that scales the room with respect to the given original house size (w,h) *)
let scale_resize ?(scale_width=true) ?(scale_height=true)
    ?(scale_x=true) ?(scale_y=true) (w,h) r =
  let x = xpos r in
  let y = ypos r in
  let rw,rh = get_size r in
  let keep_resize = true in
  let resize (hw,hh) =
    let scalex z = z * hw / w in
    let scaley z = z * hh / h in
    if scale_x then setx ~keep_resize r (scalex x);
    if scale_y then sety ~keep_resize r (scaley y);
    if scale_height then set_voffset r (scaley (get_voffset r));
    if scale_height && scale_width then set_size ~keep_resize r (scalex rw, scaley rh)
    else if scale_height then set_height ~keep_resize r (scaley rh)
    else if scale_width then set_width ~keep_resize r (scalex rw) in
  r#set_resize @@ resize

(* convenience function for scaling all rooms by the same factor -- to be use for rooms in the same house *)
let scale_resize_list ?scale_width ?scale_height ?scale_x ?scale_y (w,h) rooms =
  List.iter (scale_resize ?scale_width ?scale_height ?scale_x ?scale_y (w,h))
    rooms

(* Overrides scale_resize to retreive the house size automatically. Can only be
   applied if the room is already in a house. *)
let scale_resize ?scale_width ?scale_height ?scale_x ?scale_y room =
  match room#house with
  | None -> printd debug_error
              "Cannot compute the resize function of room %s since it does not \
               belong to a house" (sprint_id room)
  | Some h ->
    let s = get_size h in
    scale_resize ?scale_width ?scale_height ?scale_x ?scale_y s room

let auto_scale house =
  let (w,h) = get_size house in
  match house#content with
  | List rooms -> scale_resize_list (w,h) rooms
  | Leaf _ -> printd debug_warning "TODO: auto_scale resident"

let resize_follow_house room =
  room#set_resize (fun size ->
      set_size ~keep_resize:true room size)

(* Not very smart, but [resize_fix_x/y] is currently used by Space. Another
   possibility would be to have two resize functions: horizontally and
   vertically *)
let resize_fix_x room =
  let f = room#resize in
  let keep_resize = true in
  room#set_resize (fun size ->
      let x,w = getx room, width room in
      f size;
      setx ~keep_resize room x;
      set_width ~keep_resize room w
    )

let resize_fix_y room =
  let f = room#resize in
  let keep_resize = true in
  room#set_resize (fun size ->
      let y,h = gety room, height room in
      f size;
      sety ~keep_resize room y;
      set_height ~keep_resize room h
    )

(* sets l with the size of the top_house. In principle the (x,y) of the
   top_house should be (0,0), we don't check this here. The (x,y) of l is set to
   (0,0). Should be called dynamically after main loop starts. *)
let maximize l =
  setx l 0;
  sety l 0;

  let w,h = get_size (top_house l) in
  l#set_current_geom { l#current_geom with h; w };
  Avar.set l#geometry.h h;
  Avar.set l#geometry.w w;
  scale_resize_list (w,h) [l];
  ();
  resize_content l

(** check if a sublayer is deeper (= below = Chain.<) than the main layer, which
    (in principle) should not happen *)
let check_layers room =
  let rec loop house r =
    match r#content with
    | Leaf _ -> if Chain.(house#layer > r#layer)
      then printd debug_error "The house #%d contains a room #%d with deeper layer! (%d>%d)"
          house#id r#id (Chain.depth (house#layer)) (Chain.depth (r#layer));
    | List h -> List.iter (loop r) h
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
  if !debug then check_layers room;
  ()

(** Set the canvas for layout and all children *)
let global_set_canvas ?(mustlock=true) room canvas =
  if mustlock then
    iter (fun r -> r#set_canvas @@ Some canvas) room;
  if mustlock then ()

let check_layer_error room house =
  if not (Chain.same_component room#layer house#layer)
  then printd debug_error
      "The replacement room %s belongs to a separate set of layers disjoint \
       from the house %s. Beware that it will probably never be displayed"
      (sprint_id room) (sprint_id house)

(* use this only if you know what you are doing... *)
(* remember that a room with no house will be considered a "top layout" *)
(* see WARNING of the "kill" fn. If the detached room is still pointed to by the
   board (eg. mouse_focus...=> the mouse will not find what you expect) *)
(* TODO lock ? *)
(* not used *)
let detach_rooms layout =
  match layout#content with
  | Leaf _ -> printd debug_error "No rooms to detach from layout %s" (sprint_id layout)
  | List rooms -> List.iter (fun r ->
      if r#house <> None then
        (r#set_house @@ None;
         printd debug_warning "Room %s was detached from House %s" (sprint_id r) (sprint_id layout))) rooms

(* Detach a room from its house. See detach_rooms *)
let detach room =

  let () = match room#house with
    | None -> printd debug_error "Cannot detach because room %s has no house"
                (sprint_id room)
    | Some h ->

      room#set_house None;
      let rooms = List.filter (fun r -> not (r == room)) (get_rooms h) in
      h#set_content (List rooms);
      printd debug_warning "Room %s was detached from House %s."
        (sprint_id room) (sprint_id h);
      () in
  ()

(* modify the layout content by setting new rooms *)
(* old ones are *not* freed, but they are *detached* from house *)
(* Note that setting rooms that are already there is legal. Then they are not
   detached, of course. *)
(* With sync=false, this is highly non thread safe. Locking layout is not enough
   (or, we should lock all layouts in the main loop too... Therefore, it is
   better to set sync=true, which delays the execution to Sync (the main loop
   Queue) *)
let set_rooms layout ?(sync=true) rooms =
  match layout#content with
  | Leaf _ -> printd debug_error "Cannot transform a leaf (Resident #%u) to a node (Rooms) because the resident widget would be lost" layout#id
  | List old_rooms ->
    (if sync then Sync.push else run)
      (fun () ->
         rooms
         |>  List.iter (fun r ->
             if !debug && mem equal r old_rooms
             then printd debug_warning
                 "Trying to insert a room (%s) that is already there \
                  (%s). But we can do this, no problem."
                 (sprint_id r) (sprint_id layout)
             else r#set_house @@ Some layout;
             check_layer_error r layout;
             (* if there is a canvas in layout, we copy it to all rooms *)
             do_option layout#canvas (global_set_canvas ~mustlock:false r));

         (* Now we rescan the list to detach unused rooms. This could have been
            done in the first iteration, but there is a [if !debug] there,
            so... *)
         old_rooms
         |> List.iter (fun r ->
             if not (mem equal r rooms) then detach r);
         (* detach_rooms layout; *) (* we don't detach because some orphans may
                                       want to survive longer than you
                                       think... see WARNING in 'kill' *)
         layout#set_content @@ List rooms;
         (*fit_content layout*)
      )
(* Hum. the adjust should NOT be done at this point, because display didn't
   happen yet, hence the current_geometry is not updated. Morover there is no
   way to know the 'sep' optional argument *)
(* TODO the example/ls example should be reviewed then ... *)

(* copy the 'relocatable content' of src into dst.  Of course, this should be
   avoided when writing in functional style, but can be handy sometimes *)
(* Warning: size will change, and this is not transmitted to the parent house *)
(* Warning: the old content is not freed from memory *)
(* TODO: move everything to Sync (not only set_rooms) ? *)
let copy ~src ~dst =
  let dx = dst#current_geom.x - src#current_geom.x in
  let dy = dst#current_geom.y - src#current_geom.y in
  global_translate src dx dy;
  dst#set_geometry { dst#geometry with w = src#geometry.w; h = src#geometry.h };
  let w,h = get_size src in
  dst#set_current_geom { dst#current_geom with w; h };
  dst#set_clip src#clip;
  dst#set_background src#background;
  begin match src#content with
    | Leaf (r) as c -> dst#set_content c
    | List rooms -> set_rooms dst rooms
  end;
  dst#set_keyboard_focus src#keyboard_focus;
  dst#set_draggable src#draggable

(* Add a room to the dst layout content (END of the list). The resize function
   of the room is cancelled. *)
(* This is used to add a pop-up *)
(* warning: the room should NOT already belong to some house# *)
(* TODO: write a "remove_room" function *)
let add_room ?valign ?halign ~dst room =
  (* check oversize. But this should not happen. The user should not rely on
     this. If the added room is too large, beware that nothing outside of the
     geometry of the destination room will never have mouse focus (mouse focus
     is detected per house, and THEN into the children rooms. *)
  if room#house <> None
  then printd debug_error "Room %s should not be added to %s because it already \
                           belongs to a house." (sprint_id room) (sprint_id dst);
  check_layer_error room dst;


  let wmax = (getx room) + (width room) in
  if wmax > width dst
  then (printd debug_error "The attached Room #%u is too wide" room#id;
        (*set_width dst wmax*));
  let hmax = (gety room) + (height room) in
  if hmax > height dst
  then (printd debug_error "The attached Room #%u is too tall" room#id;
        (*set_height dst hmax*));

  let x = default (map_option halign (fun a ->
      Draw.align a 0 (width dst) (width room))) (getx room) in
  let y = default (map_option valign (fun a ->
      Draw.align a 0 (height dst) (height room))) (gety room) in
  setx room x;
  sety room y;
  room#set_house (Some dst);
  do_option dst#canvas (global_set_canvas ~mustlock:false room);

  let rooms = get_rooms dst in
  (* we cannot add room to layout which already contains a Resident *)
  dst#set_content @@ List (List.rev (room :: (List.rev rooms)));
  (*  fit_content dst;; *)
  ();
  ()

let set_layer ?(debug = !debug) room layer =

  room#set_layer layer;
  ();
  if debug then check_layers room

(* TODO: do some "move layer" or translate layer instead *)
let global_set_layer room layer =
  iter (fun r -> set_layer ~debug:false r layer) room

(** construct a horizontal house from a list of rooms *)
(* sep = horizontal space between two rooms *)
(* hmargin = horizontal margin (left and right). *)
(* vmargin = vertical margin (top and bottom). *)
(* if margins is set, then sep, hmargin and vmargin are all set to this value *)
(* WARNING: resulting layout has position (0,0). *)
let flat ?name ?(sep = Theme.room_margin / 2) ?(adjust=Fit)
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin)
    ?margins ?align ?background ?shadow ?canvas rooms =
  (* List.iter (set_canvas canvas) rooms; *)
  (* TODO check layers ? *)
  let sep, hmargin, vmargin = match margins with
    | Some m -> m,m,m
    | None -> sep, hmargin, vmargin in
  let rec loop list x y =
    match list with
    | [] -> (x - sep + hmargin, y)
    | r::rest ->
      setx r x;
      sety r vmargin;
      loop rest (x + sep + (width r)) (max y ((height r) + 2*vmargin)) in
  let w,h = loop rooms hmargin vmargin in
  let layout = create ?name ?background ?shadow (geometry ~w ~h ()) ~adjust (List rooms) ?canvas in
  do_option align (fun align -> v_align ~align layout vmargin (h-2*vmargin));
  (* Now that the geometry is finalized, we may compute the resize function for
     each room: *)
  scale_resize_list (w,h) rooms;
  layout

let hbox = flat

(* Construct a flat directly from a list of widgets that we convert to
   Leafs. By default it uses smaller margins than [flat]. *)
let flat_of_w ?name ?(sep = Theme.room_margin) ?h ?align ?background ?widget_bg
    ?canvas widgets =
  let rooms =
    List.map (fun wg ->
        let name = map_option name (fun s -> "Resident of [" ^ s ^ "]") in
        resident ?name ?h ~x:0 ~y:sep ?background:widget_bg ?canvas (wg :> Widget.t)) widgets
  in
  flat ?name ~margins:sep ?align ?background ?canvas rooms

let h_center layout x0 w =
  match layout#content with
  | Leaf _ -> ()
  | List rs -> List.iter
                 (fun r -> let w0 = width r in
                   let x = Draw.center x0 w w0 in
                   setx r x)
                 rs

let h_align ~align layout x0 w =
  match layout#content with
  | Leaf _ -> ()
  | List rs -> List.iter
                 (fun r -> let w0 = width r in
                   let x = Draw.align align x0 w w0 in
                   setx r x)
                 rs

(* create a vertical layout ("tower") from a list of widgets *)
let tower_of_w_old ?name ?(sep = Theme.room_margin) ?w ?align
    (* ?(adjust = Fit) *) ?background ?widget_bg
    ?canvas widgets =
  let rec loop list rooms (x,y) =
    match list with
    | [] -> rooms, (x,y)
    | wg::rest ->
      let room = resident ?w ~x:sep ~y ?background:widget_bg ?canvas wg in
      let w,h = get_size room in
      loop rest (room::rooms) (max x (w+2*sep), y+sep+h) in
  let rooms, (w,h) = loop widgets [] (sep,sep) in
  (* let background = Solid (Draw.(lighter (opaque grey))) in *)
  let layout = create (geometry ~w ~h ()) ?name ?background (List (List.rev rooms)) ?canvas in
  do_option align (fun align -> h_align ~align layout sep (w-2*sep));
  scale_resize_list (w,h) rooms;
  layout

(** create a tower from a list of rooms *)
(* sep = vertical space between two rooms *)
(* hmargin = horizontal margin (left and right). *)
(* vmargin = vertical margin (top and bottom). *)
(* TODO set layer *)
let tower ?name ?(sep = Theme.room_margin/2) ?margins
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin)
    ?align ?(adjust = Fit) ?background ?shadow ?canvas ?clip rooms =
  (* List.iter (set_canvas canvas) rooms; TODO *)
  let sep, hmargin, vmargin = match margins with
    | Some m -> m,m,m
    | None -> sep, hmargin, vmargin in
  let rec loop list x y =
    match list with
    | [] -> (x, y - sep + vmargin)
    | r::rest ->
      setx r hmargin;
      sety r y;
      loop rest (max x ((width r) + 2*hmargin)) (y + sep + (height r)) in
  let w,h = loop rooms hmargin vmargin in
  let layout = create ~adjust ?name ?background ?shadow ?clip
      (geometry ~w ~h ()) (List rooms) ?canvas in
  do_option align (fun align -> h_align ~align layout hmargin (w-2*hmargin));
  if clip = None then scale_resize_list (w,h) rooms else fix_content layout;
  (* TODO ce n'est pas la peine de scaler la largeur si elle ne dépasse pas le
     layout. Voir par exemple la demo/demo *)
  layout

(* Construct a tower directly from a list of widgets that we convert to
   Residents. *)
let tower_of_w ?name ?(sep = Theme.room_margin) ?w ?align ?background ?widget_bg
    ?canvas widgets =
  let rooms =
    List.map (fun wg ->
        let name = map_option name (fun s -> "Leaf of [" ^ s ^ "]") in
        resident ?name ?w ~x:sep ~y:0 ?background:widget_bg ?canvas (wg :> Widget.t)) widgets
  in
  tower ?name ~margins:sep ?align ?background ?canvas rooms

(* compute the x,y,w,h that contains all rooms in the list *)
let bounding_geometry = function
  | [] -> printd debug_warning "Trying to find bounding_geometry of empty list";
    0,0,0,0
  | rooms ->
    let rec loop xmin ymin xmax ymax = function
      | [] -> (xmin, ymin, xmax-xmin, ymax-ymin)
      | room :: rest ->
        let x,y = getx room, gety room in
        loop
          (imin xmin x)
          (imin ymin y)
          (imax xmax (width room + x))
          (imax ymax (height room + y))
          rest in
    loop max_int max_int 0 0 rooms


(* Superpose a list of rooms without changing their relative (x,y) positions.
   Unless specified by ~w ~h, the resulting layout has the *size* of the total
   bounding box of all rooms. Its (x,y) *position* is such that, when displayed
   at this position, all rooms should be located at the positions they claimed.
   The *layer* of the first room is selected. *)
(* TODO it seems that only the first one gets focus... *)
let superpose ?w ?h ?name ?background ?canvas ?(center=false) rooms =
  let x,y,bw,bh = bounding_geometry rooms in
  (* We translate the rooms: *)
  List.iter (fun r -> setx r (getx r - x); sety r (gety r - y)) rooms;
  let w = default w bw in
  let h = default h bh in
  if center then List.iter (fun r ->
      setx r (Draw.center (getx r) w (width r));
      sety r (Draw.center (gety r) h (height r))) rooms;
  let geometry = geometry ~x ~y ~w ~h () in
  let layer = match rooms with
    | [] -> Draw.get_current_layer ()
    | r::_ -> r#layer in
  scale_resize_list (w,h) rooms;
  create ?name ~layer ?background ?canvas geometry (List rooms)

(** save the layout_id in the user event *)
(* not used anymore *)
let save_to_event_OLD event room =
  Sdl.Event.(set event Trigger.room_id room#id)
(* TODO: since we only use one global event for mouse_enter or mouse_leave, it
   would be more efficient to store directly the room in a global variable,
   rather that storing the id, and then painfully search the room corresponding
   to id... *)

(** ask all the subwidgets to update themselves. *)
(* in fact, this just send the redraw_event, which ask for redrawing the whole
   window. Thus, it would be enough to ask this to only one widget of the layout
   (since all widgets must be in the same window) *)
let rec ask_update room =
  match room#content with
  | Leaf w -> w#update
  | List list -> List.iter ask_update list

(** animations: *)
(* animations with Anim are deprecated, use Avar instead *)

let animate_x room x =

  let g : geometry = room#geometry in
  Avar.stop g.x;
  room#set_geometry { g with x };
  ()

let animate_y room y =

  let g : geometry = room#geometry in
  Avar.stop g.y;
  room#set_geometry { g with y };
  ()

let animate_w room w =

  let g : geometry = room#geometry in
  Avar.stop g.w;
  room#set_geometry { g with w };
  ()

let animate_h room h =

  let g : geometry = room#geometry in
  Avar.stop g.h;
  room#set_geometry { g with h };
  ()

let animate_voffset room voffset =

  let g : geometry = room#geometry in
  let avar = Var.get g.voffset in
  let is_current = Avar.started avar && not (Avar.finished avar) in
  Avar.stop avar;
  Var.set g.voffset voffset;
  (* if the animation was already running we need to start immediately,
     otherwise the value that we set here will be valid only for the next
     iteration, which may cause non-immediate transitions: useful ???*)
  if is_current then ignore (get_voffset room);
  ()

let animate_alpha room alpha =

  let g : geometry = room#geometry in
  Avar.stop g.transform.alpha;
  room#set_geometry { g with transform = { g.transform with alpha }};
  ();
  ask_update room

let animate_angle room angle =

  let g : geometry = room#geometry in
  Avar.stop g.transform.angle;
  room#set_geometry { g with transform = { g.transform with angle }};
  ()

let stop_pos room =
  printd debug_graphics "Stop position animation for layout %s." (sprint_id room);

  let g : geometry = room#geometry in
  Avar.stop g.x;
  Avar.stop g.y;
  ()

(** get desired room (relative) geometry after applying animation *)
let geom r =
  let g = r#geometry in
  to_current_geom g (* the calculation is there *)


(** some predefined animations: *)
(* warning, these animations can be set on-the-fly, so be careful with other
   existing animations *)
let default_duration = 300

(* add a show animation (vertical sliding) to the room; however: *)
(* 1. if the room is already animated, we replace the old animation by the show,
   and the duration is reduced proportionally to the current voffset of the old
   animation *)
(* 2. if the room is shown and without animation, we do nothing *)
let show ?(duration=default_duration) ?from room =
  if room#show && Avar.finished (Var.get (room#geometry : geometry).voffset)
  then printd (debug_board + debug_warning)
      "Room %s is already shown, we don't run the show animation"
      (sprint_id room)
      (* it is ok to show a room that currently is performing a hide animation. *)
  else begin
    let clip = ref false in
    let init () =
      clip := room#clip;
      room#set_clip true
    in
    (* it is important to do this AFTER the ending() of the previous
       animation. *)
    let h = height room in
    if not room#show && (get_voffset room <> -h)
    then (printd debug_warning
            "Using a 'show' animation on a room that was not previously in a \
             'hidden' state. Forcing voffset to %d." (-h);
          set_voffset room (-h));
    let h, duration = match from with
      | None ->
        let current_vo = get_voffset room in
        let d' = abs ((current_vo * duration) / h) in
        current_vo, d'
      | Some Avar.Bottom -> h, duration
      | Some Avar.Top -> -h, duration
      | Some _ -> printd debug_board "Layout.show direction not implemented";
        h, duration in
    let ending () =
      printd debug_board "End of show for %s" (sprint_id room);
      room#set_clip !clip in
    let voffset = Avar.show ~init ~ending ~duration h 0 in
    animate_voffset room voffset;
    rec_set_show true room;
  end

(* add a hide animation to the room *)
let hide ?(duration=default_duration) ?(towards = Avar.Bottom) room =
  if (not room#show) (*&& Avar.finished (Var.get room#geometry.voffset)*) then ()
  else begin
    let clip = ref false in
    let init () =
      clip := room#clip;
      room#set_clip true
    in
    let h = height room in
    let current_vo = get_voffset room in
    let d' = abs ((h + current_vo)*duration) / (abs h+1) in (* DEBUG *)
    let vo = match towards with
      | Avar.Bottom -> h
      | Avar.Top -> -h
      | _ -> printd debug_board "Layout.show direction not implemented"; h in
    let ending _ =
      printd debug_board "End of hide";
      room#set_clip !clip;
      rec_set_show false room in
    (* WARNING: if the room contains subrooms with animations, they will remain
       forever because a layout with show=false is not displayed and hence not
       updated: the anim is not removed. Even more so with Avar. Thus compute
       has_anim during display ? *)
    let voffset = Avar.show ~init ~ending ~duration:d' current_vo vo in
    animate_voffset room voffset
  end

(** scrolling to a particular vertical position y, for a prescribed duration. *)
(* y should be between 0 and the total height *)
(* Warning: in principle, room#house#clip should be set to true for this *)
let scroll_to ?(duration=1000) (*?(weight=0.5)*) y room =
  match room#house with
  | None -> printd debug_error "Cannot scroll the top layout (need a house)"
  | Some house ->
    let current_vo = get_voffset house in
    let y' = max 0 (min y (height room - height house)) in
    let duration = duration * (abs(current_vo + y') + 1) / (abs (current_vo + y) + 1) in
    printd debug_graphics "Scroll room#%u in house #%u, from %d to %d, duration=%d"
      room#id house#id current_vo (-y') duration;
    let voffset = Avar.fromto ~duration current_vo (-y') in
    (* TODO: make a special animation when reaching bounds *)
    animate_voffset house voffset

(** relative scrolling *)
(* the specifications are: the scrolling must be continuous (no jump), and n
   calls to (scroll dy) should end up in the same position as (scroll (n*dy)),
   even if they are triggered before the previous animation is not finished. In
   case of rapid mouse wheel events, it is not obvious to have the scrolling
   look smooth. Thus we add the following spec: the scroll curve should be
   epsilon-close to the h-translated starways curve that would be obtained with
   immediate jumps, where epsilon is the jump height (50 for scroll wheel) and h
   is a small time amout, that we choose to be 2*dt here (it should be less than
   duration, otherwise the second part of the animation g2 is never executed) *)
let scroll_delay = ref 0.5
let scroll_old2 ?(duration=default_duration) dy room =
  do_option room#house (fun house ->
      let current_vo = get_voffset house in
      let avar = Var.get house#geometry.voffset in
      let jump_vo = Avar.final_value avar in
      let final_vo = - (max 0 (min (dy - jump_vo) (height room - height house))) in
      let duration = duration * (abs(final_vo - current_vo) + 1) / (abs (jump_vo - dy - current_vo) + 1) in
      let dt = Avar.progress avar in
      printd debug_graphics "Scroll room #%u: dy=%d, current_vo=%d, jump_vo=%d, final_vo=%d, duration=%d, progress=%f"
        room#id dy current_vo jump_vo final_vo duration dt;
      let voffset = if current_vo = jump_vo || duration = 0
        then (scroll_delay := 0.5;
              Avar.fromto ~duration current_vo final_vo)
        else (* the previous animation was not finished, we need to catch up *)
          let dv = final_vo - jump_vo in (* warning, opposite sign to dy *)
          let h = if dt < !scroll_delay
            then scroll_delay := max 0.3 !scroll_delay /. 1.5
            else if dt > 1.5 *. !scroll_delay
            then scroll_delay := min 0.5 (1.5 *. !scroll_delay);
            !scroll_delay in
          let slope = (float (jump_vo - current_vo)) *. (1. -. h) /.
                      (h *. (float (final_vo - jump_vo))) in
          (* rem we may have (slope < 0.) in case of changing direction *)
          let g1 = Avar.affine (float current_vo) (float jump_vo) in
          let g2 u = Avar.initial_slope ~slope:(max 1. (abs_float slope)) u
                     |> Avar.affine (float jump_vo) (float final_vo) in

          let update _ u = Avar.concat ~weight:h g1 g2 u
                           |> round in
          printd debug_graphics "Scroll: dv=%d, h=%f slope=%f" dv h slope;
          Avar.create ~duration ~update current_vo in
      animate_voffset house voffset)

let last_time = ref (Time.now ());; (* TODO replace this by the time of the Avar *)
let scroll ?(duration=default_duration) dy room =
  do_option room#house (fun house ->
      let current_vo = get_voffset house in
      let avar = Var.get house#geometry.voffset in
      let jump_vo = Avar.final_value avar in (* à vérifier *)
      let final_vo = - (max 0 (min (dy - jump_vo) (height room - height house))) in
      let elapsed = Time.(now () - !last_time) in
      let duration =
        if (elapsed = 0)
        || (elapsed > 300)
        || current_vo = final_vo
        then duration
        else let speed = (float (-dy)) /. (float elapsed) in
          (* = the speed that user expects to have, because she's rolling the
             mouse wheel *this* fast. In pixels per ms. Now we need to adjust
             the duration so that the expected final value is indeed reached at
             that speed. *)
          let final = current_vo + (round (speed *. (float duration))) in
          if final = current_vo then duration
          else abs (duration * (final_vo - current_vo) / (final - current_vo)) in
      printd debug_graphics
        "Scroll room #%u: current:%d, final_vo=%d, duration=%d"
        room#id current_vo final_vo duration;
      let voffset = Avar.fromto ~duration current_vo final_vo in
      last_time := Time.now ();
      animate_voffset house voffset)

let scroll_old ?(duration=300) dy room =
  do_option room#house (fun house ->
      Avar.finish (Var.get (house#geometry : geometry).voffset); (* TODO: do something smoother *)
      let previous_vo = get_voffset house in
      printd debug_graphics "Scroll room #%u, dy=%d, previous_vo=%d" room#id dy previous_vo;
      scroll_to ~duration (dy - previous_vo) room)

(* find a parent whose house has the 'clip' property *)
let rec find_clip_house room =
  match room#house with
  | None -> None
  | Some h ->
    if h#clip then Some room
    else find_clip_house h

(** add fade_in transform to the existing animation of the room *)
let fade_in ?duration ?(from_alpha = 0.) ?(to_alpha = 1.) room =
  let alpha = Avar.fade_in ?duration ~from_alpha ~to_alpha () in
  animate_alpha room alpha

(** add fade_out transform to the existing animation of the room *)
(* WARNING: fading out to alpha=0 results in a completely transparent room, but
   the room is *still there*. (it's not "hidden"). Which means it can still get
   mouse focus. If you want to hide it, then use hide=true *)
let fade_out ?duration ?from_alpha ?(to_alpha = 0.) ?(hide=false) room =
  let from_alpha = default from_alpha (get_alpha room) in
  let ending _ =
    printd debug_board "End of complete fade_out => hiding room";
    rec_set_show false room in
  let ending = if hide then Some ending else None in
  let alpha = Avar.fade_out ?duration ?ending ~from_alpha ~to_alpha () in
  animate_alpha room alpha

(* angle in degree *)
(* WARNING: it's not a global rotation. All widgets in the room will rotate
   separately about their own center *)
(* If you want to rotate a complete layout, use a Snapshot *)
let rotate ?duration ?(from_angle = 0.) ~angle room =
  let angle = Avar.fromto_float ?duration from_angle (from_angle +. angle) in
  animate_angle room angle

(* Zoom works for Resident, but for a general List it will not work *)
(* moreover, only few resident widgets will be ok. (image, box...) *)
(* In order to zoom a general layout, use a Snapshot *)
(* TODO: add zoom center *)
let zoom_x ?duration ~from_factor ~to_factor room =
  let w0 = round (float (width room) *. from_factor) in
  let w1 = round (float (width room) *. to_factor) in
  let w = Avar.fromto ?duration w0 w1 in
  printd debug_graphics "ZOOM width from %d to %d" w0 w1; (* DEBUG *)
  animate_w room w

let zoom_y ?duration ~from_factor ~to_factor room =
  let h0 = round (float (height room) *. from_factor) in
  let h1 = round (float (height room) *. to_factor) in
  let h = Avar.fromto ?duration h0 h1 in
  printd debug_graphics "ZOOM height from %d to %d" h0 h1; (* DEBUG *)
  animate_h room h

let zoom ?duration ~from_factor ~to_factor room =
  zoom_x ?duration ~from_factor ~to_factor room;
  zoom_y ?duration ~from_factor ~to_factor room

(** oscillate (for fun) *)
let oscillate ?(duration = 10000) ?(frequency=5.) amplitude room =
  let x = Avar.oscillate ~duration ~frequency amplitude (getx room)in
  animate_x room x

(** add a slide_in animation to the room *)
let slide_in ?from ~dst room =
  let x,y = Avar.slide_in ?from ~size:(get_size dst)
      ~pos:(getx room, gety room) () in
  animate_x room x;
  animate_y room y

(** translation animation *)
let slide_to ?(duration=default_duration) room (x0,y0) =
  let x1 = getx room in
  let y1 = gety room in
  let x = Avar.fromto ~duration x1 x0 in
  let y = Avar.fromto ~duration y1 y0 in
  animate_x room x;
  animate_y room y

(** follow mouse animation. *)
(* Note that the window is not available before running the layout... *)
(* TODO this doesn't work for touch, because get_mouse_state only captures when
   the finger touches the screen, but not when it moves. One should use
   pointer_pos instead. *)

let mouse_motion_x ?dx ?modifier room =
  let x0 = ref 0 in (* we store here the dist between mouse and room *)
  let init () =
    x0 := default dx (fst (Mouse.window_pos (window room)) - xpos room) in
  let update _ _ =
    let x = fst (Mouse.window_pos (window room)) - (x_origin room) - !x0 in
    match modifier with
    | None -> x
    | Some f -> x + f x in
  Avar.create ~duration:(-1) ~update ~init 0

let mouse_motion_y ?dy ?modifier room =
  let y0 = ref 0 in
  let init () =
    y0 := default dy (snd (Mouse.window_pos (window room)) - ypos room) in
  let update _ _ =
    let y = snd (Mouse.window_pos (window room)) - (y_origin room) - !y0 in
    match modifier with
    | None -> y
    | Some f -> y + f y in
  Avar.create ~duration:(-1) ~update ~init 0

(* set the origin of the room at the mouse position - (dx,dy) *)
(* if dx or dy is not specified, the default is the current distance between
   room and mouse *)
(* modifierx and modifiery are executed continuously during the animation, and
   return an integer offset, which can typically be used to obtain a "magnetic"
   effect. See examples/displays *)
(* TODO merge into unique function "action" *)
let follow_mouse ?dx ?dy ?modifierx ?modifiery room =
  let x = mouse_motion_x ?dx ?modifier:modifierx room in
  let y = mouse_motion_y ?dy ?modifier:modifiery room in
  animate_x room x;
  animate_y room y

(* Clip a room inside a smaller container and make it scrollable, and optionally
   add a scrollbar widget (which should appear only when necessary). Currently
   only vertical scrolling is implemented.  *)
let make_clip ?w ?(scrollbar = true) ?(scrollbar_inside = false) ?(scrollbar_width = 10)
    ~h room =
  (* iter_rooms disable_resize room; *)
  let name = room#name ^ ":clip" in
  if w <> None
  then printd debug_error "Horizontal scrolling is not implemented yet";
  let w = default w (width room) in
  let y0 = gety room in
  sety room 0;
  let active_bg = new Empty.t (width room,height room) in
  (* We add an invisible box to make the whole area selectable by the mouse
     focus. Otherwise, only the parts of the room that contain a widget will
     react to the mouse wheel event. Of course, if the room was full of widgets,
     this is superfluous... *)
  let container = tower ~margins:0 ~clip:true
      [superpose [room; resident (active_bg)]] in
  (* The container should be a room with a unique subroom (and the active
     background); the subroom can then be scrolled with respect to the container
  *)
  set_size container (w,h);

  let result =
    if scrollbar
    then begin
      (* We first initialize the bar layout with a dummy widget, so that the
         var is able to use it. This is only useful if the height of the
         container is modified after creation, for instance when the user
         resizes the window. *)
      let bar = resident ~background:(Solid Draw.(lighter scrollbar_color))
          ((new Empty.t (10,10))) in
      (* The scrollbar is a slider. Its Tvar takes the voffset value into the
         slider value, between 0 and (height room - height container). 0
         corresponds to the bottom position of the slider, so this means the
         *largest* scroll (voffset is the most negative). *)
      let var = Tvar.create container#geometry.voffset
          ~t_from:(fun vo ->
              let dh = height room - height bar in
              if dh <= 0 then 0 (* then the bar should be hidden *)
              else dh + Avar.get vo)
          ~t_to:(fun v ->
              let dh = height room - height bar in
              let v = imin v dh |> imax 0 in
              Avar.var (height bar - height room + v)) in
      let wsli = new Slider.t ~kind:Slider.Vertical ~length:h
        ~thickness:scrollbar_width
        ~tick_size:(h * h / (height room))
        ~var ~m:(imax 0 (height room - h)) () in
      change_resident bar (wsli :> Widget.t);
      if h >= (height room) then hide ~duration:0 bar;
      let r = if scrollbar_inside
        then (setx bar (w - width bar);
              set_layer bar (Chain.insert_after
                               (Chain.last (container#layer))
                               (Draw.new_layer ()));
              (* TODO: is this a bit too much ?? We just want to make
                 sure the scrollbar gets mouse focus. *)
              superpose ~name [container; bar])
        else flat ~name ~margins:0 [container; bar] in
      disable_resize bar;
      (* We register a resize function that simultaneously sets the container
         and the bar sizes. It will hide the bar when the container is large
         enough to display the whole content. *)
      container#set_resize @@ (fun (w,h) ->
          let keep_resize = true in
          set_height ~keep_resize bar h;
          if scrollbar_inside then set_size ~keep_resize container (w,h)
          else begin
            set_size ~keep_resize container (w - width bar, h);
            setx ~keep_resize bar (w - width bar)
          end;
          let dh = height room - height bar in
          let sli = wsli in
          if dh >= 1 then sli#set_max dh
          else set_voffset container 0;
          (* Warning: we set the voffset directly, because when the bar is
             hidden, the Tvar will never be activated -- except if the user
             scrolls with the mouse. *)
          let h = height bar in
          if height room <> 0
          then sli#set_tick_size
              (h * h / (height room));
          let v = sli#update_value; sli#value in
          if v < 0 then sli#set_value 0;
          if dh <= 0
          then rec_set_show false bar
          else rec_set_show true bar);
      r
    end
    else container in

  sety result y0;
  let x0 = getx room in
  setx room 0;
  setx result x0;
  (* We copy the shadow. TODO: this has no effect at the moment, because of the
     'clip' flag, the layout is sharply clipped to its bounding box when
     rendering, so the shadow is hidden. *)
  let shadow = room#shadow in
  result#set_shadow @@ shadow;
  room#set_shadow None;
  result

let relayout createfn ?(duration=200) layout =
  let rooms = get_rooms layout in
  let old_pos = List.map (fun r -> getx r, gety r) rooms in
  (* this will change the rooms positions: *)
  let () = ignore (createfn rooms) in
  if duration <> 0 then
    List.iter2 (fun (oldx,oldy) room ->
        let newx = getx room in
        if oldx <> newx
        then animate_x room (Avar.fromto ~duration oldx newx);
        let newy = gety room in
        if oldy <> newy
        then animate_y room (Avar.fromto ~duration oldy newy))
      old_pos rooms

(* adjust an existing layout to arrange its rooms in a "flat" fashion, as if
   they were created by Layout.flat. Will be animated if duration <> 0 *)
let reflat  (*?(sep = Theme.room_margin / 2)*) ?align
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin) ?margins =
  relayout (fun rooms -> flat ~hmargin ~vmargin ?margins ?align rooms)

(* same as reflat but with Layout.tower *)
let retower (*?(sep = Theme.room_margin / 2)*) ?align
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin) ?margins =
  relayout (fun rooms -> tower ~hmargin ~vmargin ?margins ?align rooms)

(* typically in a tower, enlarge all rooms to have the width of the house.  This
   is not recursive: only rooms of depth 1. *)
let expand_width house =
  let w = width house in
  iter_rooms (fun room ->
      let x = getx room in
      if w-x < 1 then printd debug_warning "Cannot expand_width because house x position is larger than width.";
      set_width room (w-x)) house

(* replace "room" by "by" inside "house" in lieu and place of the intial
   room# No size adjustments are made. Of course this is dangerous, because it
   modifies both the house and "by". beware of circular dependencies... Of
   course this assumes that "room" already belongs to "house". *)
(* TODO copy old (x,y) position *)
let replace_room ?house ~by room =
  match room#house, house with
  | None, None -> printd debug_error
                    "Cannot use \"replace_room\" because room %s \
                     does not belong to a house." (sprint_id room)
  | _, Some h
  | Some h, None -> begin
      printd debug_warning "Replacing room %s by room %s"
        (sprint_id room) (sprint_id by);


      let rooms_before_rev, rooms_after =
        let rec loop rb = function
          | [] ->
            failwith "Room not found in the house, this should not happen!"
          | r :: rest -> if equal r room
            then rb, rest
            else loop (r::rb) rest in
        loop [] (get_rooms h) in
      (* cf "set_rooms" *)
      by#set_house @@ Some h;
      (*global_set_layer by room#layer; (* ok ??? *)*)
      iter (fun r -> r#set_canvas @@ room#canvas) by;
      (* are there other things to copy ?? *)
      h#set_content @@
      List (List.rev_append (by::rooms_before_rev) rooms_after);
      ();
      ()
    end

(* move a room to a new house, trying to keep the same visual
   position. Optionnally adding a scrollbar (in which case the returned layout
   is not the same as the original one). *)
(* This does not NOT change layer, canvas... *)
(* WARNING this doesn't take voffset into account, so it won't work if a 'hide'
   animation was used to the room. *)
let relocate ~dst ?(scroll=true) ?(auto_scale=false) room =

  (* TODO check they have the same top_house? *)
  let x0,y0 = compute_pos dst in
  let x1,y1 = compute_pos room in
  (* 'pos_from' won't work here because this is called (by Select) before rooms
     positions are computed... *)

  if room#house <> None then detach room;
  let room2 = if not scroll then room
    else let y2 = y1-y0 + height room in
      printd debug_board "Relocate room : y2=%i y1=%i y0=%i room=%i \
                          dst=%i" y2 y1 y0 (height room) (height dst);
      if y2 <= height dst then room
      else begin
        (* sety room 0; *)
        make_clip ~h:(height dst - y1 + y0) ~scrollbar_inside:true
          ~scrollbar_width:4 room
      end in

  (* We add it to the dst *)
  add_room ~dst room2;
  setx room2 (x1-x0);
  sety room2 (y1-y0);
  if auto_scale then scale_resize room2;
  ();
  room2


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
(* this function sends all the blits to be displayed to the layers *)
(* it does not directly interact with the renderer *)
(* pos0 is the position of the house containing the room *)
let display ?pos0 room =
  let x0,y0 = match pos0 with
    | None -> house_pos room
    | Some p -> p in
  let rec display_loop x0 y0 h0 clip0 tr0 r =
    (* clip contains the rect that should contain the current room r. But of
       course, clip can be much bigger than r. *)
    if not r#show then ()
    else begin
      let g = geom r in
      let x = x0 + g.x
      and y = y0 + g.y + h0
      and voffset = g.voffset in
      (* update current position, independent of clip *)
      r#set_current_geom @@ { g with x; y };
      (*print_endline ("ALPHA=" ^ (string_of_float (Avar.old room#geometry.transform.alpha)));*)
      let rect = Sdl.Rect.create ~x ~y ~w:g.w ~h:g.h in

      (* if there is a nonzero offset, we perform a new clip : this is used for
         "show/hide" animation *)
      (* TODO clip should be enlarged in case of shadow *)
      let clip = if (*voffset = 0*) not r#clip || !no_clip then clip0
        else Draw.intersect_rect clip0 (Some rect) in
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
          let transform =
            let tr = get_transform r in
            (* printd debug_board "TRANSFORM alpha=%f" tr.Draw.alpha; *)
            let open Draw in
            (* printd debug_board "COMPOSED TRANSFORM alpha=%f" (tr.alpha *. tr0.alpha); *)
            (*{ tr0 with alpha = tr0.alpha *. tr.alpha } in*)
            (* TODO: compose also rotations with centres, flips !! *)
            compose_transform tr0 tr in

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
                                    voffset = - voffset}) in
              blits) in
          (* !!! in case of shadow, the blits contains several elements!! *)

          begin match r#content with
            | List h ->
              (* We only draw the background. Make sure that the layer of the
                 room r is at least as deep as the layers of the List h *)
              do_option bg
                (List.iter
                   (fun blit ->
                      let open Draw in
                      let t = compose_transform transform blit.transform in
                      let clip = sclip in
                      blit_to_layer { blit with clip; transform = t }));
              if !draw_boxes then begin
                let rect = debug_box ~color:(0,0,255,200) r x y in
                let open Draw in
                let t = compose_transform transform rect.transform in
                let clip = sclip in
                blit_to_layer { rect with clip; transform = t }
              end;
              List.iter (display_loop x y voffset clip transform) h
            | Leaf w ->
              let blits = w#display (get_canvas r) r#layer
                  Draw.({x; y; w = g.w; h = g.h; voffset}) in
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
                   let t = compose_transform transform blit.transform in
                   let clip = sclip in
                   blit_to_layer { blit with clip; transform = t }) blits
          end;
          if !draw_boxes  (* we print the room number at the end to make sure it's visible *)
          then let label = new Label.t ~font_size:7 ~fg:(Draw.(transp blue))
                 (sprint_id r) in
            let geom = Draw.scale_geom {Draw.x; y; w=g.w+1; h=g.h+1; voffset} in
            List.iter
              Draw.blit_to_layer
              (label#display (get_canvas r) (r#layer) geom)
        end
    end in
  display_loop x0 y0 0 None (Draw.make_transform ()) room

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
    | Some room -> match room#content with
      | List _ -> go (Draw.create_system_cursor Sdl.System_cursor.arrow)
      | Leaf (w) -> Cursor.get w#cursor in
  Sdl.set_cursor (Some cursor)

(** check is the room has some non-fresh components. *)
(* optimize (Bogue) ? *)
let rec is_fresh room =
  match room#content with
  | List list -> let rec loop = function
      | [] -> true
      | r::h -> if not (is_fresh r) then false
        else loop h in
    loop list
  | Leaf (w) -> Widget.is_fresh w

let room_has_anim room =
  Avar.has_anim room#geometry.transform.alpha ||
  Avar.has_anim room#geometry.transform.center ||
  Avar.has_anim room#geometry.transform.flip ||
  Avar.has_anim room#geometry.transform.angle ||
  List.fold_left (fun b v -> b || (Avar.has_anim v))
    false (get_int_avars room)

(* optimize (Bogue) ? *)
(* TODO one could transfer it into Layout.display, which would return the anim
   status of what was displayed (and not what was hidden...) *)
let rec has_anim room =
  if !debug && (not room#show) && (room#hidden) && (room_has_anim room)
  then printd debug_error "Room %s has unfinished animation but it is not shown."
      (sprint_id room);
  room#show && (not room#hidden) &&
  (room_has_anim room ||
   match room#content with
   | List list -> List.fold_left (fun b r -> b || (has_anim r)) false list
   | Leaf _ -> false)

(* Flip buffers. Here the layout SHOULD be the main layout (house) of the window
*)
(* only one canvas/renderer is used, the one specified by the layout *)
let flip ?(clear=false) ?(present=true) layout =
  printd debug_graphics "flip layout %s" (sprint_id layout);
  (* go (Sdl.set_render_target (renderer layout) None); *)
  if clear then Draw.clear_canvas (get_canvas layout);
  printd debug_graphics "Render layers";
  Var.protect_fn Draw.current_layer (fun () ->
      (* : we assume that the layout layer is in the same component as the
         current_layer... TODO do better *)
      Draw.render_all_layers (layout#layer));
  if present then begin
    printd debug_graphics "Present";
    Draw.(sdl_flip (renderer layout))
  end

(* prerender the layout to the layers *)
let render layout =
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
  let top = top_house layout in
  if not (equal layout top)
  then printd debug_error "The layout for resizing window should be the top \
                           layout";
  let w,h = Sdl.get_window_size (window top)
            |> Draw.unscale_pos in
  let w', h' = get_size top in
  if (w',h') <> (w,h)
  then begin
    (* TODO in rare occasions, it might happen that this test is different
       from get_physical_size top <> Sdl.get_window_size win*)
    printd debug_graphics "Resize (%d,%d) --> (%d,%d)" w' h' w h;
    set_size ~keep_resize:true ~check_window:false top (w,h);
    Draw.update_background (get_canvas top);
    if flip then Draw.sdl_flip (renderer top)
  end
(* : somehow we need this intermediate flip so that the renderer takes into
    account the new size. Otherwise texture are still clipped to the old
    size... On the other hand it might flicker if triggered to quickly *)
(* fit_content layout;;*) (* not useful *)

(** initialize SDL if necessary and create a window of the size of the layout *)
let make_window ?window layout =
  printd debug_graphics "Make window";
  let top = top_house layout in
  if not (equal layout top)
  then printd debug_error "  The layout for creating a window should be the top layout";
  let w,h = get_physical_size top in
  let wmax, hmax = 4096, 4096 in
  (* = TODO ? instead clip to ri_max_texture_width,ri_max_texture_height ? *)
  if wmax < w || hmax < h
  then printd debug_error "  The layout has size (%u,%u), which exceeds the max size (%u,%u)." w h wmax hmax;
  let w = min w wmax in
  let h = min h hmax in
  let x,y = get_window_pos layout in
  let canvas = Draw.init ?window ~name:layout#name ?x ?y ~w ~h () in
  global_set_canvas top canvas

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
let adjust_window ?(display=false) layout =
  Sync.push (fun () ->
      let top = top_house layout in
      if not (equal layout top)
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
  | None -> inside_geom room#current_geom (x,y)
  | Some mask -> (* TODO vérifier aussi qu'on est dans la dimension du mask *)
    let x0,y0 = room#current_geom.x, room#current_geom.y in
    let _,_,_,a = Draw.get_pixel_color mask ~x:(x-x0) ~y:(y-y0) in
    a <> 0

(** get the smallest room (with Resident) containing point (x,y), or None *)
(* not used anymore *)
let rec over_focus x y t =
  let g = to_current_geom t#geometry
  (* one should also take into account transforms, clipping... and also if a
     SUBroom is animated, the geometry can extend beyond the initial (fixed)
     house geometry,... TODO *) in
  if t#show && (inside_geom g (x,y))
  then match t#content with
    | Leaf _ -> Some t
    | List h ->
      list_check (fun r -> over_focus (x - g.x) (y - g.y) r) h
      (* we translate because geometry is relative *)
  else None

(* instead of the first one, get the complete list *)
(* cf remarks above *)
let rec focus_list_old x y t =
  let g = to_current_geom t#geometry in
  if t#show && (inside_geom g (x,y)) then match t#content with
    | Leaf _ -> [ t ]
    | List h -> List.flatten (List.map (fun r -> focus_list_old (x - g.x) (y - g.y) r) h)
  else []

(* instead of the first one, get the complete list *)
(* in each layer, the first element of the list has priority (TODO this is not
   consistent with the fact that it is the last displayed) *)
(* cf remarks above *)
let rec focus_list x y t =
  if t#show && (inside t (x,y)) then match t#content with
    | Leaf _ -> [ t ]
    | List h -> List.flatten (List.map (fun r -> focus_list x y r) h)
  else []

(* get the focus element in the top layer *)
let top_focus x y t =
  let flist = focus_list x y t in
  printd debug_graphics "Number of layers detected under mouse: %u (%s)" (List.length flist) (String.concat " " (List.map (fun r -> "#" ^ (string_of_int r#id)) flist));
  let compare r1 r2 = Chain.compare (r1#layer) (r2#layer) in
  list_max compare flist

(** get the smallest room (with Room or Resident) containing (x,y), or None *)
(* only used for testing *)
(* TODO à fusionner avec le précédent pour retourner une paire ? *)
(* TODO vérifier qu'on est dans le même calque (layer) *)
let rec hover x y t =
  let g = to_current_geom t#geometry in
  if t#show && (inside_geom g (x,y)) then match t#content with
    | Leaf _ -> Some t
    | List h -> (match
                   list_check (fun r -> hover (x - g.x) (y - g.y) r) h
                 with
                 | None -> Some t
                 | o -> o)
  else None
