type _ Effect.t +=
    Register : ('a Effect.t * ('a -> 'b)) -> unit Effect.t

let perform = Effect.perform
let continue = Effect.Deep.continue

module State (V : sig type t end) = struct
  type t = V.t
  type _ Effect.t +=
      Get : t Effect.t
  type _ Effect.t +=
      Set : t -> unit Effect.t

  let get () = perform Get
  let set x = perform (Set x)

  (* Installs a deep handler around f. Different from the example *)
  (* which uses shallow handlers! https://github.com/ocamllabs/ocaml-effects-tutorial *)
  let install f (init : t) =
    let s = ref init in
    match f () with
    | () -> ()
    | [%effect? Get, k] ->
      continue k !s
    | [%effect? (Set s'), k] ->
      s := s';
      continue k ()

  module Let = struct
    let (let*) init f = install f init
  end
end
