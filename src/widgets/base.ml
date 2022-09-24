include Interop
open Utils

type bottom = Utils.bottom = |

exception Repeat
exception Reset

module type R = sig type t end

module type A = functor (Result : R) -> sig
  type res = Result.t
  type _ EffectHandlers.eff +=
      Await : Event.t list -> (Event.t_rich * Draw.geometry) EffectHandlers.eff
  type _ EffectHandlers.eff +=
      Yield : res -> unit EffectHandlers.eff
  val await : Event.t list -> (Event.t_rich * Draw.geometry -> 'a) -> 'a
  val yield : res -> unit
end

type 'a t = { f: Event.t_rich * Draw.geometry -> 'a }

type 'a await =
  Event.t list ->
  (Event.t_rich * Draw.geometry -> 'a) -> 'a

(* module type Await_sig = sig *)
(*   type res *)
(*   type _ EffectHandlers.eff += *)
(*       Await : Event.t list * res option -> (Event.t_rich * Draw.geometry) EffectHandlers.eff *)
(*   val await : < f : 'b. (res, 'b) await > *)
(* end *)

module Await
  (* : R -> Await_sig *)
  = functor (Result: R) -> struct
    exception%effect Await : Event.t list -> (Event.t_rich * Draw.geometry)
    exception%effect Yield : Result.t -> unit
    (* module M = struct *)
    (*   type t *)
    (*   exception%effect Await : Event.t list * t -> (Event.t_rich * Draw.geometry) *)

    let await = object (self)
      method f : 'a. 'a await =
        fun triggers handler ->
        try handler @@
          (* (fun (ev,g) -> print_endline (Event.show_t_rich ev); (ev,g)) @@ *)
          EffectHandlers.perform (Await triggers) with
        | Repeat
        (* TODO This might catch other failures.
           Also the match statements are (obviously) shown as incomplete
           Solvable by either always including a _ -> raise Repeat case or
           by writing a ppx *)
        | Match_failure _ -> self#f triggers handler

      method forever : bottom =
        match EffectHandlers.perform (Await []) with
        | _ -> failwith "Empty trigger list was resumed"
    end

    let yield result = EffectHandlers.perform (Yield result)
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

class virtual ['a] w ?id size name cursor =
  object
    inherit common ?id ~name size ()

    method canvas = None

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    method virtual execute : <f:'b. 'b await; forever: bottom> -> ('a -> unit) -> bottom

    method virtual display : Draw.canvas -> Draw.blit list -> Draw.geometry -> Draw.blit list
  end

let gen w = (w :> 'a w)
