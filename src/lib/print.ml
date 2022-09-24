open Printf
open Interop

module L = Layout
module W = Widget

let widget w = sprintf "wid: #%u, name: %s" w#id (w#name)

let color (r,g,b,a) =
  sprintf "(r=%u, g=%u, b=%u, a=%u)" r g b a

let geometry g =
  let open L in
  sprintf "(x=%d, y=%d, w=%d, h=%d, vo=%d)"  g.x g.y g.w g.h g.voffset

let content c = widget c

let house h =
  sprintf "%s" (L.sprint_id h)

let canvas c =
  sprintf "Window id=%u" (Draw.window_id c)

let option f o =
  match o with
  | Some x -> "Some " ^ (f x)
  | None -> "None"

let bool b = sprintf "%b" b

let background = function
  | L.Solid c -> color c
  | L.Box _ -> "Box"

let layout_up ?(indent = "") r =
  let list = [
    sprintf "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
    sprintf "%s┃ geometry: %s\n" indent (geometry r#geometry);
    sprintf "%s┃ background: %s\n" indent (option background r#background);
    sprintf "%s┃ content: %s\n" indent (content r#content);
    sprintf "%s┃ canvas: %s\n" indent (option canvas r#canvas);
    sprintf "%s┃ keyboard_focus: %s\n" indent (option bool r#keyboard_focus);
    sprintf "%s┗━" indent ] in
  String.concat "" list

let layout_down ?(indent = "") r =
  let list = [
    sprintf "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
    sprintf "%s┃ geometry: %s\n" indent (geometry r#geometry);
    sprintf "%s┃ background: %s\n" indent (option background r#background);
    sprintf "%s┃ canvas: %s\n" indent (option canvas r#canvas);
    sprintf "%s┃ keyboard_focus: %s\n" indent (option bool r#keyboard_focus);
    sprintf "%s┃ content: %s\n" indent (widget r#content);
    sprintf "%s┗━" indent ] in
  String.concat "" list

let dump r =
  let file, ch = Filename.open_temp_file "bogue" ".dump" in
  print_endline ("Saving dump to " ^ file);
  output_string ch (layout_down r);
  close_out ch

let layout_error_kf room msg =
  Printf.kfprintf (fun ch -> flush ch) stderr msg;
  dump room

let layout_error room msg =
  output_string stderr msg;
  dump room
