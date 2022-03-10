include Interop
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

exception%effect Await : Event.t list -> (Event.t_rich * Draw.geometry)
exception Repeat
exception Reset
let rec await triggers handler =
  try handler @@
    (fun (ev,g) -> print_endline (Event.show_t_rich ev); (ev,g)) @@
    EffectHandlers.perform (Await triggers) with
  | Repeat -> await triggers handler
  | Match_failure _ -> await triggers handler

(* Note that the type system is not smart enough
    to know that this function is correct.
   If the handler worked for await, it will also work for await_loop. *)
let await_loop (type a) await triggers_ext handler_ext triggers (handler : _ -> a) : a =
  await (triggers_ext @ triggers) @@ function
  | ev,g ->
    match
      List.mem (Event.strip ev) triggers_ext,
      List.mem (Event.strip ev) triggers
    with
    | true,true ->
      handler_ext (ev,g);
      handler (ev,g)
    | false,true ->
      handler (ev,g)
    | true,false ->
      handler_ext (ev,g);
      raise Repeat
    | false,false -> assert false

let await_unit triggers_ext handler_ext triggers handler =
  await (triggers_ext @ triggers) @@ function
  | ev,g ->
    match
      List.mem (Event.strip ev) triggers_ext,
      List.mem (Event.strip ev) triggers
    with
    | true,true ->
      handler_ext (ev,g);
      handler (ev,g)
    | false,true ->
      handler (ev,g)
    | true,false ->
      handler_ext (ev,g)
    | false,false -> assert false

class virtual common ?id ?(name = "UNNAMED") size () =
  let id = match id with None -> fresh_int () | Some id -> id in
  object (self)
    method id : int = id
    method name : string = name

    method virtual canvas : Draw.canvas option

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
  object
    inherit common ?id ~name size ()

    method canvas = None

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    method virtual execute : 'a

    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list
  end

type any = Any : 'a #w -> any
let gen w = (w :> 'a w)
