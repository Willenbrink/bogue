include Interop
open Utils

exception Repeat
exception Reset

module type R = sig type t end

module type A = functor (Result : R) -> sig
  type res = Result.t
  type _ EffectHandlers.eff +=
      Await : Event.t list * res option -> (Event.t_rich * Draw.geometry) EffectHandlers.eff
  val await : Event.t list -> res option -> (Event.t_rich * Draw.geometry -> 'a)
    -> 'a
end

type 'a t = { f: Event.t_rich * Draw.geometry -> 'a }

type ('a,'b) await =
  Event.t list -> 'a option ->
  (Event.t_rich * Draw.geometry -> 'b) -> 'b

(* module type Await_sig = sig *)
(*   type res *)
(*   type _ EffectHandlers.eff += *)
(*       Await : Event.t list * res option -> (Event.t_rich * Draw.geometry) EffectHandlers.eff *)
(*   val await : < f : 'b. (res, 'b) await > *)
(* end *)

module Await
  (* : R -> Await_sig *)
  = functor (Result: R) -> struct
    exception%effect Await : Event.t list * Result.t option -> (Event.t_rich * Draw.geometry)
    (* module M = struct *)
    (*   type t *)
    (*   exception%effect Await : Event.t list * t -> (Event.t_rich * Draw.geometry) *)

    let await = object (self)
      method f : 'b. (Result.t, 'b) await =
        fun triggers result handler ->
        try handler @@
          (* (fun (ev,g) -> print_endline (Event.show_t_rich ev); (ev,g)) @@ *)
          EffectHandlers.perform (Await (triggers, result)) with
        | Repeat
        (* TODO This might catch other failures.
           Also the match statements are (obviously) shown as incomplete
           Solvable by either always including a _ -> raise Repeat case or
           by writing a ppx *)
        | Match_failure _ -> self#f triggers None handler
    end
  end

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

    method focus_with_keyboard = ()
    method remove_keyboard_focus = ()
    method guess_unset_keyboard_focus = true
  end

class virtual ['a] stateful init =
  object
    val mutable state : 'a = init
    method state = state
  end

type bottom = |

class virtual ['a] w ?id size name cursor =
  object
    inherit common ?id ~name size ()

    method canvas = None

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    (* TODO think about initialization and nontermination.
       Empty should never get the opportunity to return something.
       Right now we rely on it returning None on its first execution *)
    method virtual execute : <f:'b. ('a,'b) await> -> bottom

    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list
  end

let gen w = (w :> 'a w)
