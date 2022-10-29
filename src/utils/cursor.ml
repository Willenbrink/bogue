open Utils

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
  go (Sdl.create_system_cursor shape)

let set_system_cursor sdl_cursor =
  Sdl.set_cursor (Some (go (Sdl.create_system_cursor sdl_cursor)))
