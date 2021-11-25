(* Keyboard shortcuts for the main loop *)
open Tsdl

module K = Sdl.K

module Kmod =
struct
  include Sdl.Kmod
  open Int

  let relevant_kmods = [ctrl; shift; alt; gui]

  let (+) = logor
  let test field mask = compare (logand field mask) 0 <> 0
  let compare x y =
    let cats x = List.map (fun category -> (Int.logand x category) <> 0) relevant_kmods in
    let x,y = cats x, cats y in
    (* FIXME compare is bad because its not total! *)
    List.fold_left2 (fun acc x y -> acc * 2 + if x = y then 0 else 1) 0 x y

  let eq x y =
    let cats x = List.map (fun category -> Int.logand x category <> 0) relevant_kmods in
    let x,y = cats x, cats y in
    List.fold_left2 (fun acc x y -> acc && (x = y)) true x y
end

module KeyPair =
struct
  type t = Sdl.keycode * Sdl.keymod

  let compare (k1,kmod1) (k2,kmod2) =
    match compare k1 k2 with
    | 0 -> Kmod.compare kmod1 kmod2
    | c -> c
end

module PairsMap = Map.Make(KeyPair)

type 'a action = 'a -> unit

type 'a t = ('a action) PairsMap.t

type kmod =
  | Ctrl
  | Shift
  | Alt
  | Super

let sdl_of_kmod = function
  | Ctrl -> Kmod.ctrl
  | Shift -> Kmod.shift
  | Alt -> Kmod.alt
  | Super -> Kmod.gui

let empty = PairsMap.empty

(* Bind a new action to the keycode. If the keycode was already present, the
   previous action is disregarded. *)
let add ?(kmod = []) kcode action map =
  let kmod =
    List.map sdl_of_kmod kmod
    |> List.fold_left Kmod.(+) 0
  in
  Printf.printf "Adding shortcut of kcode: %i, kmod: %i \n" kcode kmod;
  PairsMap.add (kcode, kmod) action map

let add_map (kmod,kcode,action) map = add ~kmod kcode action map

let remove pair map = PairsMap.remove pair map

(* Return the action bound to the keycode, or None. *)
(* FIXME uses Sdl.Kmod directly, encapsulate *)
let find pair map =
  Printf.printf "Finding shortcut of kcode: %i, kmod: %i \n" (fst pair) (snd pair);
  PairsMap.find_opt pair map

(* Add new entries from a list of triples (keycode, keymod, action) *)
let add_list alist map = List.fold_right add_map alist map

(* Create a Shortcut map for a list of triples (keycode, keymod, action). *)
let create alist : 'a t = add_list alist empty

(** Common shortcuts *)

let exit_on_escape = ([], K.escape, fun _ -> raise Exit)
