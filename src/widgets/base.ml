include Interop

type bottom = |

module Cursor = struct
  type t =
    | Arrow
    | Hand
    | Ibeam
end

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

class virtual ['a] stateful init =
  object
    val mutable state : 'a = init
    method state = state
  end

class virtual ['a] w name cursor =
  object (self)
    method name : string = name

    method virtual min_size : int * int

    val mutable cursor : Cursor.t = cursor
    method cursor = cursor
    method set_cursor x = cursor <- x

    method virtual execute : <f:'b. 'b await; forever: bottom> -> ('a -> unit) -> bottom

    method virtual render : Draw.geometry -> (Draw.geometry * Raylib.Texture.t) list

    (* Can be unset by execute if a render must be forced *)
    val mutable render_cache : (Draw.geometry * (Draw.geometry * Raylib.Texture.t) list) option = None

    (* Caches the result of self#render *)
    method display geometry =
      let rerender () =
        let blits = self#render geometry in
        (* render_cache <- Some (geometry, blits); *)
        blits
      in
      match render_cache with
      | Some (geom, blits) when geom = geometry -> blits
      | Some (geom, blits) ->
        List.iter (fun (_, tex) -> Raylib.unload_texture tex) blits;
        rerender ()
      | _ -> rerender ()
  end

let gen w = (w :> 'a w)
