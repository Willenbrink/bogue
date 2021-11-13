open Utils
open Tsdl

type t =
  | Arrow
  | Hand
  | Ibeam

let get cursor =
  let shape =
    let open Sdl.System_cursor in
    match cursor with
    | Arrow -> arrow
    | Hand -> hand
    | Ibeam -> ibeam
  in
  go (Draw.create_system_cursor shape)
