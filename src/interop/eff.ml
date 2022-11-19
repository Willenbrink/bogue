type _ EffectHandlers.eff +=
    Register : ('a EffectHandlers.eff * ('a -> 'b)) -> unit EffectHandlers.eff

let perform = EffectHandlers.perform
let continue = EffectHandlers.Deep.continue

module State (V : sig type t end) = struct
  type t = V.t
  type _ EffectHandlers.eff +=
      Get : t EffectHandlers.eff
  type _ EffectHandlers.eff +=
      Set : t -> unit EffectHandlers.eff

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
