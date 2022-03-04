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

exception%effect Await : Trigger.t list -> (Sdl.event * Draw.geometry)
exception Reset

type 'a cc =
  | Cc : Trigger.t list * (Sdl.event -> Draw.geometry -> 'a) -> 'a cc
  | Init

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

    (* method virtual perform : 'a *)
    (* Display self (including all content)
       Wait for event + Perform state update
       call display on self again (If params change, discontinue continuation) *)
    (* method show canvas layer geom : 'a = *)
    (*   let bl = self#display canvas layer geom in *)
    (*   begin *)
    (*     try *)
    (*       self#perform |> ignore *)
    (*     (\* This should likely only be done at the top widget/layout as we DONT want to *)
    (*        ignore actual values. A window consisting only of a text input is invalid. *)
    (*        What happens on input? Therefore show should only exist where perform has type unit*\) *)
    (*     with [%effect? (Await triggers), k] -> *)
    (*       EffectHandlers.perform (Await_Draw (triggers, bl)) *)
    (*       |> EffectHandlers.Deep.continue k *)
    (*   end; *)
    (*   self#show canvas layer geom *)

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

    method canvas = None

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    (* The set of interesting events. TODO change type to variant *)
    (* FIXME this is obsolete and should be removed.
       Instead, use the triggers provided by the Await effect.
       There should be no triggers inherent to a widget except those defined in Await. *)
    method virtual triggers : Trigger.t list
    method virtual handle : Sdl.event -> Draw.geometry -> 'a option

    method perform =
      Printf.printf "%s ???\n" self#name;
      let ev, geom = EffectHandlers.perform (Await self#triggers) in
      Printf.printf "%s <--\n" self#name;
      match self#handle ev geom with
      | Some res ->
        Printf.printf "%s succeeds\n" self#name;
        res
      | None ->
        Printf.printf "%s fails\n" self#name;
        self#perform

    method update =
      Printf.printf "%s updates\n" self#name;
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
let gen w = (w :> 'a w)
