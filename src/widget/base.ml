open Utils

(** what to do when the same action (= same connection id) is already running ? *)
type action_priority =
  | Forget (** discard the new action *)
  | Join (** execute the new after the first one has completed *)
  | Replace (** kill the first action (if possible) and execute the second one *)
  | Main (** run in the main program. So this is blocking for all subsequent actions *)

type base_obj = < id : int; name : string >

let equal x y = (x#id = y#id)

module Hash = struct
  type t = base_obj
  let equal = equal
  let hash x = x#id
end

module WHash = Weak.Make(Hash)

let common_wtable = WHash.create 100

let of_id id =
  try WHash.find common_wtable (object method id = id method name = "DUMMY" end) with
  | Not_found ->
    printd debug_error "Cannot find widget with id=%d" id;
    raise Not_found

let of_id_opt id =
  try Some (of_id id) with
  | Not_found -> None

class connection ?target action ?(priority=Forget) ?join triggers =
  object
    method action (ev : Sdl.event) : unit =
      if !debug
      then
        (printd debug_thread "Executing action";
         let t = Unix.gettimeofday () in
         action ev;
         printd debug_thread "End of action with time=%f" (Unix.gettimeofday () -. t))
      else
        action ev;

      (* TODO ajouter Trigger.will_exit ev ?? *)
      do_option target (fun x -> x#update)

    method priority = priority
    method triggers = triggers

    method id = match join with
      | None -> fresh_int ()
      | Some c -> c#id

    initializer
      if Option.is_some target && (List.mem Sdl.Event.user_event triggers)
      then printd debug_warning "one should not 'connect' with 'update_target'=true if the trigger list contains 'user_event'. It may cause an infinite display loop";
  end

and virtual common ?id ?(name = "UNNAMED") size () =
  let id = match id with None -> fresh_int () | Some id -> id in
  object (self)
    method id : int = id
    method name : string = name

    method virtual children : common list
    method virtual canvas : Draw.canvas option

    val mutable connections : connection list = []
    method connections = connections
    method add_connection c : unit = connections <- connections @ [c]

    val mutable size : int * int = size
    method size = size
    method resize x =
      self#unload;
      size <- x

    (* unload all textures but the widget remains usable. (Rendering will recreate
       all textures) *)
    method virtual unload : unit

    initializer WHash.add common_wtable (self :> < id : int; name : string >)



    method focus_with_keyboard = ()
    method remove_keyboard_focus = ()
    method guess_unset_keyboard_focus = true
  end

(* A virtual class storing a state. By convention, a widget has a state =/= unit when it is
   an interactive widget. A label therefore has unit state, even though it can be changed.
   TODO Investigate how this interacts with inheritance. What about a clickable label?
   It should work well as the type of inheritance can be specified.
*)
class virtual ['a] stateful init =
  object
    val mutable state : 'a = init
    method state = state
  end

class virtual ['a] w ?id size name cursor =
  object (self)
    inherit common ?id ~name size ()

    method children = []
    method canvas = None

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    (* The set of interesting events. TODO change type to variant *)
    method virtual triggers : int list
    method handle (ev : Sdl.event) (geom : Draw.geometry) : unit = Printf.printf "Handle %i at %s\n" (Trigger.of_event ev) name

    method update =
      (* ask for refresh *)
      (* Warning: this is frequently called by other threads *)
      (* Warning: this *resets to 0* the user_window_id *)
      (* anyway, it is not clear if the user_window_id field for created event types
         is really supported by (T)SDL *)
      printd debug_board "Please refresh";
      (* if !draw_boxes then Trigger.(push_event refresh_event) *)
      (* else *)
      Trigger.push_redraw self#id
    (* refresh is not used anymore. We redraw everyhting at each frame ... *)
    (* before, it was not very subtle either: if !draw_boxes is false, we ask for
       clearing the background before painting. Maybe some widgets can update
       without clearing the whole background. But those with some transparency
       probably need it. This should not be necessary in case we draw a solid
       background -- for instance if draw_boxes = true *)
    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list
  end

(** create new connection *)
(* if ~join:c, on donne le même id que la connexion c, ce qui permet
   d'effectuer l'action conjointement avec celle de c (avec en général
   la priorité Join pour effectuer à la suite de c). Attention dans ce
   cas, ne pas déclancher plein de ces connexions à la suite... elles
   s'attendent ! *)
let connect self ?target action ?priority ?join triggers =
  let target = Option.map (fun x -> (x :> 'a w)) target in
  let c = new connection action ?priority ?target ?join triggers in
  self#add_connection c

let connect_after self target action triggers =
  match List.rev self#connections with
  | [] -> connect self ?target action ~priority:Join triggers
  | c::_ -> connect self ?target action ~priority:Join ~join:c triggers

let connect_main = connect ~priority:Main

type any = Any : 'a #w -> any
