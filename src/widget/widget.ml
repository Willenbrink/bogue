(* each widget contains its personal data, and the list of connections from
   it *)

open Tsdl
open Utils

type _ kind =
  | Box : Box.t -> Box.t kind
  | Button : Button.t -> Button.t kind
  | Check : Check.t -> Check.t kind
  | Label : Label.t -> Label.t kind
  | TextDisplay : Text_display.t -> Text_display.t kind
  | Image : Image.t -> Image.t kind
  | Slider : Slider.t -> Slider.t kind
  | TextInput : Text_input.t -> Text_input.t kind
  | Other : 'a kind

(** what to do when the same action (= same connection id) is already running ? *)
type action_priority =
  | Forget (** discard the new action *)
  | Join (** execute the new after the first one has completed *)
  | Replace (** kill the first action (if possible) and execute the second one *)
  | Main (** run in the main program. So this is blocking for all subsequent actions *)

type active = {
  thread : Thread.t; (** this is the thread launched by the connection with given id *)
  event : Sdl.event; (* this is the event passed to the "action".  It is used
                        also for communication *)
  connect_id : int
};;

let fresh_id = fresh_int ();;
let fresh_wid = fresh_int ();;

type action = any -> any -> Sdl.event -> unit

and connection = {
  source : any;
  target : any;
  action : action;
  priority : action_priority;
  triggers : Trigger.t list;
  id : int;
}

and 'a t = <
  wid : int;
  set_wid : int -> unit;
  kind : 'a kind;
  (*  receiver : action Event.channel; *) (* TODO: pas nécessaire ? *)
  actives : (active list) Var.t;
  (** all active threads/connections for this widget. Most recent come first in
      the list *)
  connections : connection list;
  set_connections : connection list -> unit;
  (** all possible connections from this widget. In the order to be
      executed. Particular case: the local actions are connection from
      and to the same widget. *)
  fresh : bool Var.t; (* is the display up-to-date ? *)
  (* not really used anymore. TODO: check if this flag is still used *)
  room_id : int option; (* will be filled by the room id when inserted in that room *)
  set_room_id : int option -> unit;
  cursor : Cursor.t;
  set_cursor : Cursor.t -> unit;
  unload_texture : unit;
  size : int * int;
  resize : int * int -> unit;
  get_text : string;
  set_text : string -> unit;
  get_state : bool;
  display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list;
  typ : string;
>
and any = Any : 'a t -> any

class virtual tc_anon size =
    object
      val mutable wid_ = fresh_wid ()
      method wid = wid_
      method set_wid wid = wid_ <- wid

      method virtual typ : string

      val mutable actives_ : active list Var.t = Var.create []
      method actives = actives_

      method fresh = Var.create false;

      val mutable connections_ : connection list = []
      method connections = connections_
      method set_connections c = connections_ <- c

      val mutable room_id_ : int option = None
      method room_id = room_id_
      method set_room_id rid = room_id_ <- rid

      val virtual mutable cursor_ : Cursor.t
      method cursor = cursor_
      method set_cursor c = cursor_ <- c

      val mutable size_ : int * int = size
      method size = size_
      method resize : int * int -> unit = failwith "Unimplemented"

      method unload_texture = ()
      method display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list = failwith "Unimplemented"
      method get_text : string = failwith "Unimplemented"
      method set_text : string -> unit = failwith "Unimplemented"
      method get_state : bool = failwith "Unimplemented"

    end

class type ['a] tc' =
  object
    inherit tc_anon
    method kind : 'a kind
    method typ : string

    val mutable cursor_ : Cursor.t
  end

class ['a] tc kind : ['a] tc' =
  let typ (type a) (w : a tc') =
    match w#kind with
    | Box _ -> "Box"
    | Button _ -> "Button"
    | Check _ -> "Check"
    | TextDisplay _ -> "TextDisplay"
    | Label l -> "Label [" ^ Utils.xterm_red ^ (Label.text l) ^ Utils.xterm_nc ^ "]"
    | Image _ -> "Image"
    | Slider _ -> "Slider"
    | TextInput _ -> "TextInput"
  in
  let display (type a) (w : a t) canvas layer geom =
    Var.set w#fresh true;
    let geom = Draw.scale_geom geom in
    match w#kind with
    (* | Empty e -> printd debug_board "empty box";
     *   Empty.display canvas layer e geom *)
    | Box b -> printd debug_board "draw box";
      Box.display canvas layer b geom
    | Check b -> printd debug_board "check button: %b" (Check.state b);
      Check.display canvas layer b geom
    | Button b -> printd debug_board "button [%s]" (Button.text b);
      Button.display canvas layer b geom
    | TextDisplay td -> printd debug_board "text display: %s" (Text_display.text td);
      Text_display.display canvas layer td geom
    | Image img -> printd debug_board "image: %s" (Var.get img.Image.file);
      Image.display canvas layer img geom
    | Label l -> printd debug_board "label: %s" (Label.text l);
      Label.display canvas layer l geom
    | Slider s -> printd debug_board "slider: %d" (Slider.value s);
      Slider.display canvas layer s geom
    | TextInput ti -> printd debug_board "Input: %s" (Text_input.text ti);
      Text_input.display canvas layer ti geom
  in

  (* unload all textures but the widget remains usable. (Rendering will recreate
     all textures) *)
  let unload_texture (type a) (w : a tc') =
    printd debug_memory "Unloading texture for widget #%u" w#wid;
    match w#kind with
    | Box b -> Box.unload b
    | Check b -> Check.unload b
    | Button b -> Button.unload b
    | TextDisplay t -> Text_display.unload t
    | Image img -> Image.unload img
    | Label l -> Label.unload l
    | Slider s -> Slider.unload s
    | TextInput ti -> Text_input.unload ti
  in
  let resize (type a) (w : a tc') size =
    match w#kind with
    | Box b -> Box.resize size b
    | Button b -> Button.resize size b
    | Check c -> Check.resize size c
    | Label l -> Label.resize size l
    | TextDisplay t -> Text_display.resize size t
    | Image i -> Image.resize size i
    | Slider s -> Slider.resize size s
    | TextInput ti -> Text_input.resize size ti
  in
  (* let default_size (type a) (w : a tc') =
   *   match w#kind with
   *   | Empty e -> Empty.size e
   *   | Check b -> Check.size b
   *   | Box b -> Box.size b
   *   | TextDisplay td -> Text_display.size td
   *   | Label l -> let x,y = Label.size l in (x+2,y+2)
   *   | Image img -> Image.size img
   *   | Button b -> Button.size b
   *   | Slider s -> Slider.size s
   *   | TextInput ti -> Text_input.size ti *)

  let get_cursor (type a) (w : a kind) =
      match w with
       | Box _
       | Label _
       | TextDisplay _
       | Image _ -> Cursor.Arrow
       | Button _
       | Check _
       | Slider _ -> Cursor.Hand
       | TextInput _ -> Cursor.Ibeam
  in
  let get_text (type a) (w : a tc') =
    match w#kind with
    | Button b -> Button.text b
    | TextDisplay td -> Text_display.text td
    | Label l -> Label.text l
    | TextInput ti -> Text_input.text ti
    | _ -> (printd debug_error "This type of widget does not have a text function";
            "")
  in
  let set_text (type a) (w : a tc') text =
    match w#kind with
    | Button b -> Button.set_label b text
    | TextDisplay td -> let pa = Text_display.paragraphs_of_string text in
      Text_display.update td pa
    | Label l -> Label.set l text
    | TextInput ti -> let k = Utf8.split text in
      Text_input.set ti k
    | _ -> printd debug_error "Cannot set text to this type of widget"
  in
  let get_state (type a) (w : a tc') =
    match w#kind with
    | Button b -> Button.state b
    | Check c -> Check.state c
    | _ -> (printd debug_error "This type of widget does not have a state function";
            false)
  in
  object (self)
    inherit tc_anon (0,0) (* TODO *)
    method kind : 'a kind = kind
    val mutable cursor_ = get_cursor kind
    method typ = typ (self :> 'a tc')

    method! resize = resize (self :> 'a tc')
    method! unload_texture = unload_texture (self :> 'a tc')
    method! get_state = get_state (self :> 'a tc')
    method! get_text = get_text (self :> 'a tc')
    method! set_text text = set_text (self :> 'a tc') text
    method! display canvas layer geom = display (self :> 'a tc') canvas layer geom
  end

(* an empty widget. Does not draw anything, but can be used to get mouse focus *)
class empty size : [unit] tc' =
  object (self)
    inherit tc_anon size
    method kind = Other
    method typ = "Empty"
    val mutable cursor_ = Cursor.Arrow

    method! resize x = size_ <- x
    method! unload_texture = ()
    end

let draw_boxes = ref false;;
(* for debugging: draws a red rect rectangle around each widget layout, (fill
   when it has mouse focus (might need a redraw: CTRL-l) and a blue rect around
   container layouts *)

let id (Any w) = w#wid;;

let get_room_id w = match w#room_id with
  | None -> failwith "The widget does not belong to a room yet"
  | Some id -> id;;

let equal (Any w1) (Any w2) =
  w1#wid = w2#wid;;
let (==) = equal;;

module Hash = struct
  type t = any
  let equal = equal
  let hash = id
end

module WHash = Weak.Make(Hash);;
let widgets_wtable = WHash.create 100;;

let is_fresh w = Var.get w#fresh;;

(* let canvas w = match w.canvas with *)
(*   | Some c -> c *)
(*   | None -> failwith "Canvas not defined";; *)

(* let renderer w = *)
(*   (canvas w).Draw.renderer;; *)

(* let set_canvas canvas w = *)
(*   w.canvas <- Some canvas;; *)

let dummy_widget wid =
  let dummy = new empty (0,0) in
  WHash.add widgets_wtable (Any dummy);
  dummy#set_wid wid;
  dummy

(*let of_id wid = Hashtbl.find widgets_table wid;;*)
let of_id wid : any =
  try WHash.find widgets_wtable (Any (dummy_widget wid)) with
  | Not_found -> (printd debug_error "Cannot find widget with wid=%d" wid;
                  raise Not_found);;

(** ask for refresh *)
(* Warning: this is frequently called by other threads *)
(* Warning: this *resets to 0* the user_window_id *)
(* anyway, it is not clear if the user_window_id field for created event types
   is really supported by (T)SDL *)
let update w =
  printd debug_board "Please refresh";
  Var.set w#fresh false;
  (* if !draw_boxes then Trigger.(push_event refresh_event) *)
  (* else *)
  Trigger.push_redraw w#wid;; (*TODO... use wid et/ou window_id...*)
(* refresh is not used anymore. We redraw everyhting at each frame ... *)
(* before, it was not very subtle either: if !draw_boxes is false, we ask for
   clearing the background before painting. Maybe some widgets can update
   without clearing the whole background. But those with some transparency
   probably need it. This should not be necessary in case we draw a solid
   background -- for instance if draw_boxes = true *)


(** create new connection *)
(* if ~join:c, on donne le même id que la connexion c, ce qui permet
   d'effectuer l'action conjointement avec celle de c (avec en général
   la priorité Join pour effectuer à la suite de c). Attention dans ce
   cas, ne pas déclancher plein de ces connexions à la suite... elles
   s'attendent ! *)
let connect source target action ?(priority=Forget) ?(update_target=true) ?join triggers =
  if update_target && (List.mem Sdl.Event.user_event triggers)
  then printd debug_warning "one should not 'connect' with 'update_target'=true if the trigger list contains 'user_event'. It may cause an infinite display loop";
  let action = if update_target
    then fun (Any w1) (Any w2) ev -> (action (Any w1) (Any w2) ev; update w2) (* TODO ajouter Trigger.will_exit ev ?? *)
    else action in
  let action = if !debug
    then fun w1 w2 ev ->
      (printd debug_thread "Executing action";
       let t = Unix.gettimeofday () in
       action w1 w2 ev;
       printd debug_thread "End of action with time=%f" (Unix.gettimeofday () -. t))
    else action in
  let id = match join with
    | None -> fresh_id ()
    | Some c -> c.id in
    let source,target = Any source, Any target in
  { source;
    target;
    action;
    priority;
    triggers;
    id }

let connect_after source target action triggers =
  let action _ _ _ = () in (* TODO FIXME *)
  match List.rev source#connections with
    | [] -> connect source target action ~priority:Join triggers
    | c::_ -> connect source target action ~priority:Join ~join:c triggers;;

let connect_main = connect ~priority:Main;;

let connections t =
  t#connections;;

(* TODO: vérifier qu'on n'ajoute pas deux fois la même *)
(* TODO à faire automatiquement après "connect" ? *)
let add_connection w c =
  w#set_connections (List.rev (c :: List.rev w#connections))

(* TODO: remove connection *)
let get_box w =
  match w#kind with
    | Box b -> b
    | _ -> failwith "Expecting a box";;

let get_check w =
  match w#kind with
    | Check b -> b
    | _ -> failwith "Expecting a check box";;

let get_label w =
 match w#kind with
    | Label l -> l
    | _ -> failwith "Expecting a label";;

let get_button w =
  match w#kind with
    | Button b -> b
    | _ -> failwith "Expecting a button";;

let get_slider w =
 match w#kind with
    | Slider s -> s
    | _ -> failwith "Expecting a slider";;

let get_text_display w =
 match w#kind with
    | TextDisplay td -> td
    | _ -> failwith "Expecting a text display";;


let get_text_input w =
 match w#kind with
    | TextInput ti -> ti
    | _ -> failwith "Expecting a text input";;

(** creation of simple widgets *)
let create_empty kind =
  let w = new tc kind in
  WHash.add widgets_wtable (Any w);
  w

let check_box ?state ?style () =
  let b = create_empty  (Check (Check.create ?state ?style ())) in
  (* let action = fun (Any w) _ _ -> Check.action (get_check w) in *)
  let action _ _ _ = () in  (* TODO FIXME Disabled connections *)
  let c = connect_main b b action Trigger.buttons_down in
  add_connection b c;
  b;;


(*let get_check_state b =
  Check.state (get_check b);;
*)

let set_check_state b s =
  Check.set (get_check b) s;;

let text_display ?w ?h text =
  create_empty (TextDisplay (Text_display.create_from_string ?w ?h text));;

let rich_text ?size ?w ?h paragraphs =
  create_empty (TextDisplay (Text_display.create ?size ?w ?h paragraphs));;

let lines_display ?w ?h lines =
  create_empty (TextDisplay (Text_display.create_from_lines ?w ?h lines));;

let verbatim text =
  create_empty (TextDisplay (Text_display.create_verbatim text));;

let html text =
  create_empty (TextDisplay (Text_display.create_from_html text));;

let box ?w ?h ?background ?border ?shadow () =
  create_empty (Box (Box.create ?width:w ?height:h ?background ?border ?shadow ()));;

let label ?size ?fg ?font text =
  create_empty (Label (Label.create ?size ?fg ?font text));;

(* alias for fontawesome icon labels *)
let icon ?size ?fg name =
  create_empty (Label (Label.icon ?size ?fg name));;

let image ?w ?h ?bg ?noscale file =
  create_empty (Image (Image.create ?width:w ?height:h ?bg ?noscale file));;

let image_from_svg ?w ?h ?bg file =
  let svg = Draw.convert_svg ?w ?h file in
  let w,h = Draw.unscale_size (Draw.image_size svg) in
  image ~w ~h ?bg svg;;

let button ?(kind = Button.Trigger) ?label ?label_on ?label_off
    ?fg ?bg_on ?bg_off ?bg_over ?state
    ?border_radius ?border_color text =
  let b = create_empty
      (Button (Button.create ?label ?label_on ?label_off ?fg
                 ?bg_on ?bg_off ?bg_over
                 ?border_radius ?border_color ?state text)) in
  let press = fun _ _ _ -> Button.press (get_button b) in
  let c = connect_main b b press Trigger.buttons_down in
  add_connection b c;
  let release = match kind with (* move this test to Button ? *)
    | Button.Trigger -> fun _ _ _ -> Button.release (get_button b)
    | Button.Switch -> fun _ _ ev -> Button.switch (get_button b) ev
  in
  let c = connect_main b b release Trigger.buttons_up in
  add_connection b c;
  (* TODO FIXME disabled connections*)
  (* let c = connect_main b b (fun b _ _ -> Button.mouse_enter (get_button b))
   *     [Trigger.mouse_enter] in
   * add_connection b c;
   * let c = connect_main b b (fun b _ _ -> Button.mouse_leave (get_button b))
   *     [Trigger.mouse_leave] in
   * add_connection b c; *)
  b;;
(* TODO: actions *)

(* use ~lock if the user is not authorized to slide *)
let slider ?(priority=Main) ?step ?value ?kind ?var ?length ?thickness
      ?tick_size ?(lock = false) ?w ?h maxi =
  let w = create_empty (Slider (Slider.create ?step ?value ?kind ?var ?length
                                  ?thickness ?tick_size ?w ?h maxi)) in
  if not lock then begin
    (* TODO FIXME disabled connections *)
      (* let onbutton_down = fun w _ ev -> Slider.click (get_slider w) ev in
       * let c = connect_main w w onbutton_down Trigger.buttons_down in
       * add_connection w c; *)
      (* let onclick = fun w _ ev -> Slider.click_focus (get_slider w) ev in *)
      (* let c = connect_main w w onclick [Sdl.Event.mouse_button_up] in *)
      (* add_connection w c; *)
    (* TODO FIXME disabled connections*)
      (* let on_release = fun w _ _ -> Slider.release (get_slider w) in
       * let c = connect_main w w on_release Trigger.buttons_up in
       * add_connection w c; *)
      let slide = fun w _ ev ->
        let ti = get_slider w in
        if Trigger.mm_pressed ev || Trigger.event_kind ev = `Finger_motion
        then (Slider.slide ti ev; update w)
      in
      (* let c = connect ~priority ~update_target:false w w slide Trigger.pointer_motion in
       * add_connection w c; *)
      let get_keys = fun w _ ev -> Slider.receive_key (get_slider w) ev
      in
      (* let c = connect ~priority w w get_keys [Sdl.Event.key_down] in
       * add_connection w c *)
      ()
    end;
  w;;

(* create a slider with a simple Tvar that executes an action each time the
   local value of the slider is modified by the slider *)
let slider_with_action ?priority ?step ?kind ~value ?length ?thickness ?tick_size
    ~action max =
  let v = Var.create (Avar.var value) in
  let t_from a = Avar.get a in
  let t_to x = action x; Avar.var x in
  let var = Tvar.create v ~t_from ~t_to in
  slider ?priority ?step ?kind ~var ?length ?thickness ?tick_size max;;

let text_input ?(text = "") ?prompt ?size ?filter ?max_size () =
  let ti = Text_input.create ?size ?prompt ?filter ?max_size text in
  let w = create_empty (TextInput ti) in
  let onbutton_down = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.button_down ti ev in
  (* let c = connect_main w w onbutton_down Trigger.buttons_down in
   * add_connection w c; *)
  let onclick = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.click ti ev in
  (* let c = connect_main w w onclick Trigger.buttons_up in
   * add_connection w c; *)
  let ontab = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.tab ti ev in
  (* let c = connect_main w w ontab [Sdl.Event.key_down] in
   * add_connection w c; *)
  let selection = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    if Trigger.mm_pressed ev then (Text_input.mouse_select ti ev; update w)
  in
  (* let c = connect_main ~update_target:false w w selection [Sdl.Event.mouse_motion] in
   * add_connection w c; *)
  let get_keys = fun w _ ev -> Text_input.receive_key (get_text_input w) ev
  in
  (* let c2 = connect_main w w get_keys [Sdl.Event.text_editing; Sdl.Event.text_input; Sdl.Event.key_down; Sdl.Event.key_up] in
   * add_connection w c2; *)
  w;;
(* TODO *)


(** creation of combined widgets *)
let check_box_with_label text =
  let b = check_box () in
  let l = label text in
  let action = fun _ w _ -> Check.action (get_check w) in
  (* let c = connect_main l b action Trigger.buttons_down in
   * add_connection l c; *)
  b,l;;

(****)

(* some useful connections *)
(* the disadvantage is that these functions do not take advantage of the two
   widgets + event entry. Thus they are less 'functional' and require more
   global variables. Also, they all work with "connect_main", so are ok only for
   very fast actions. *)

let mouse_over ?(enter = nop) ?(leave = nop) w =
  let c = connect w w (fun w _ _ -> enter w) [Trigger.mouse_enter] in
  add_connection w c;
  let c' = connect w w (fun w _ _ -> leave w) [Trigger.mouse_leave] in
  add_connection w c';;

let on_click ~click w =
  let c = connect_main w w (fun w _ _ -> click w) Trigger.buttons_down in
  add_connection w c;;

let on_release ~release w =
  let c = connect_main w w (fun w _ _ -> release w) Trigger.buttons_up in
  add_connection w c;;

(****)

(** check if connection is in the active list, and return the most
    recent (=first in list) active, or None *)
let is_active alist c =
  let rec loop = function
    | [] -> None
    | a::rest -> if a.connect_id = c.id then Some a else loop rest
  in loop alist;;

(** remove an 'active' from the active list of the widget *)
(* it should occur only once in the list *)
let remove widget thread_id =
  let rec loop list acc = match list with
    | [] -> acc
    | a::rest -> (* if a.connect_id = active.connect_id *)
      (* test inutile, le suivant suffit *)
      if Thread.id a.thread = thread_id
      then List.concat [List.rev rest; acc]
      else loop rest (a::acc)
  in Var.set widget#actives (List.rev (loop (Var.get widget#actives) []));;

let add widget active =
  Var.set widget#actives (active :: (Var.get widget#actives));;

(** ask a thread to remove itself from a widget *)
let remove_me c_id widget =
  printd debug_thread "Removing connection #%d" c_id;
  remove widget (Thread.id (Thread.self ()));
  decr threads_created;;

(* check if connection is terminated *)
(* (only if the thread decided to signal this, for instance by setting the event
   to Trigger.stopped) *)
let has_terminated active =
  Sdl.Event.(get active.event typ) <> Trigger.stop;;

(* indicate to an active connection that its thread should terminate *)
(* TODO protect this with mutex or Var *)
let terminate ?(timeout = 50) active =
  printd debug_thread "Ask for terminating connection #%u" active.connect_id;
  Sdl.Event.(set active.event typ) Trigger.stop;
  ignore (Timeout.add timeout (fun () ->
              if not (has_terminated active)
              then printd debug_thread "Cannot terminate thread for connection #%u after %u ms." active.connect_id timeout
    ));;

(* ask for terminate and wait (blocking) until it really terminates by itself *)
let wait_terminate active =
  terminate active;
  Thread.join active.thread;;

(** activate an action (via a thread) on the connection *)
let add_action c action ev =
  printd debug_thread "Create thread for connection #%d" c.id;
  (* Trigger.renew_my_event (); *)
  (* we used to create a new event for the main loop, so that "ev" can be safely
     sent to the thread, and the thread can examine later, even after several
     main loops, without it being altered (except when exiting is required) *)
  (* Now we use a more natural, solution would be to copy the event before
     sending it to the thread, but there is no "copy_event" function
     available... *)
  (* WARNING: at this point it is not possible to copy the drop_file_file field *)
  let e_copy = Trigger.copy_event ev in
  incr threads_created;
  let Any src = c.source in
  add src
    { thread = Thread.create (action c.source c.target) e_copy;
      event = e_copy;
      connect_id = c.id };;

(** check if the trigger can wake up a connection, and if so, run the action *)
let wake_up event c =
  if List.mem (Trigger.of_event event) c.triggers then
    begin
      printd debug_thread "Activating connection #%d" c.id;
      (* TODO add a more precise ~test before launching the thread ? *)
      if c.priority = Main then c.action c.source c.target event
      (* = direct action, no thread !. Should we still add it to the active list
         ? *)
      else begin
        let action = fun (Any w1) w2 ev ->
          c.action (Any w1) w2 ev;
          remove_me c.id w1 in
        let Any src = c.source in
        let alist = Var.get src#actives in
        let tho = is_active alist c in
        if alist = [] || tho = None then add_action c action event
        else match c.priority, tho with
          | Forget, _ -> printd debug_thread "Forgetting connection #%d" c.id
          | Join, Some a ->
            let action = fun w1 w2 ev -> (Thread.join a.thread; action w1 w2 ev) in
            add_action c action event
          | Replace, Some a -> begin
              (*printd debug_thread "Killing connection #%d" a.connect_id;*)
              (* Thread.kill a.thread; *) (* Thread.kill is in fact NOT
                                             implemented... ! *)
              terminate a;
              let Any src = c.source in
              remove src (Thread.id a.thread);
              add_action c action event
            end
          | _ -> failwith "This should not happen"
      end
    end;;

let wake_up_all ev w =
  List.iter (wake_up ev) w#connections;;

(** remove all active connections from this widget and ask for the threads to
    terminate *)
let remove_active_connections widget =
  let actives = Var.get widget#actives in
  List.iter wait_terminate actives;
  Var.set widget#actives [];;


(*******************)

(* some widgets directly react to a click event to activate themselves. Some,
   like text_input, even react to the TAB key. In fact, keyboard_focus is
   treated globally by the main loop, therefore one could (should ?) rely on
   this function below instead of adding new reactions to TAB & click *)
let set_keyboard_focus (type a) (w : a t) =
  match w#kind with
  | TextInput _ -> () (* already done by the widget *)
  | Slider s -> Slider.set_focus s
  | _ -> ();;

let remove_keyboard_focus (type a) (w : a t) =
  match w#kind with
  | TextInput ti -> Text_input.stop ti
  | Slider s -> Slider.unfocus s
  | _ -> ();;


let guess_unset_keyboard_focus (type a) (w : a t) =
  match w#kind with
  | TextInput _ -> Some false
  | Slider _ -> Some false
  | _ -> None;;
(* TODO: buttons could have keyboard focus... to activate them with TAB or ENTER
   or SPACE... *)
