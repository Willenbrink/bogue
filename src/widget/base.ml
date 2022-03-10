include Interop
open Utils

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

    method virtual execute : 'a

    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list
  end

let gen w = (w :> 'a w)
