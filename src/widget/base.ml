open Utils

(** what to do when the same action (= same connection id) is already running ? *)
type action_priority =
  | Forget (** discard the new action *)
  | Join (** execute the new after the first one has completed *)
  | Replace (** kill the first action (if possible) and execute the second one *)
  | Main (** run in the main program. So this is blocking for all subsequent actions *)

let equal x y = (x#id = y#id)

module Hash = struct
  type t = < id : int >
  let equal = equal
  let hash x = x#id
end

module WHash = Weak.Make(Hash)

let common_wtable = WHash.create 100

let of_id id =
  try WHash.find common_wtable (object method id = id end) with
  | Not_found ->
    printd debug_error "Cannot find widget with id=%d" id;
    raise Not_found

class virtual common ?id ?(name = "") () =
  let id = match id with None -> fresh_int () | Some id -> id in
  object (self)
    method id : int = id
    method name : string = name

    method virtual children : common list
    method virtual canvas : Draw.canvas option

    initializer WHash.add common_wtable (self :> < id : int >)
  end

class virtual w ?id size name cursor =
  object (self)
    inherit common ?id ~name ()

    method children = []
    method canvas = None

    val mutable _size : int * int = size
    method size = _size
    method resize x = _size <- x

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    method fresh = Var.create false;

    val mutable connections : connection list = []
    method connections = connections
    method add_connection c = connections <- connections @ [c]

    method set_keyboard_focus = ()
    method remove_keyboard_focus = ()
    method guess_unset_keyboard_focus = true

    method update =
      (** ask for refresh *)
      (* Warning: this is frequently called by other threads *)
      (* Warning: this *resets to 0* the user_window_id *)
      (* anyway, it is not clear if the user_window_id field for created event types
         is really supported by (T)SDL *)
      printd debug_board "Please refresh";
      Var.set self#fresh false;
      (* if !draw_boxes then Trigger.(push_event refresh_event) *)
      (* else *)
      Trigger.push_redraw self#id (*TODO... use wid et/ou window_id...*)
    (* refresh is not used anymore. We redraw everyhting at each frame ... *)
    (* before, it was not very subtle either: if !draw_boxes is false, we ask for
       clearing the background before painting. Maybe some widgets can update
       without clearing the whole background. But those with some transparency
       probably need it. This should not be necessary in case we draw a solid
       background -- for instance if draw_boxes = true *)


    (* unload all textures but the widget remains usable. (Rendering will recreate
       all textures) *)
    method unload = ()
    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list
  end

and connection src ?target action ?(priority=Forget) ?join triggers =
  object
    method src : w = src
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

(** create new connection *)
(* if ~join:c, on donne le même id que la connexion c, ce qui permet
   d'effectuer l'action conjointement avec celle de c (avec en général
   la priorité Join pour effectuer à la suite de c). Attention dans ce
   cas, ne pas déclancher plein de ces connexions à la suite... elles
   s'attendent ! *)
let connect self ?target action ?priority ?join triggers =
  let target = Option.map (fun x -> (x :> w)) target in
  let c = new connection (self :> w) action ?priority ?target ?join triggers in
  self#add_connection c

let connect_after self target action triggers =
  match List.rev self#connections with
  | [] -> connect self ?target action ~priority:Join triggers
  | c::_ -> connect self ?target action ~priority:Join ~join:c triggers

let connect_main = connect ~priority:Main
