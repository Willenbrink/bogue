open Printf

module L = Layout

let widget w =
  let open Widget in
  sprintf "wid: #%u, kind: %s" w#id (w#typ)

let color (r,g,b,a) =
  sprintf "(r=%u, g=%u, b=%u, a=%u)" r g b a

let geometry g =
  let open L in
  sprintf "(x=%d, y=%d, w=%d, h=%d, vo=%d)"  g.x g.y g.w g.h g.voffset

let content c =
  match c with
  | L.List list -> sprintf "Rooms (%u)" (List.length list) (* do not call recursively all rooms, because we have a circular type *)
  | L.Leaf w -> sprintf "Resident (%s)" (widget w)

let house h =
  sprintf "%s" (L.sprint_id h)

let canvas c =
  sprintf "Window id=%u" (Draw.window_id c)

let option f o =
  match o with
  | Some x -> "Some " ^ (f x)
  | None -> "None"

let bool b =
  sprintf "%b" b

let anim _ =
  "Anim" (* TODO*)

let background = function
  | L.Solid c -> color c
  | L.Box _ -> "Box"

let rec layout_up ?(indent = "") r =
  let list = [
    sprintf "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
    sprintf "%s┃ layer: %d\n" indent (Chain.depth (r#layer));
    sprintf "%s┃ current geometry: %s\n" indent (geometry r#current_geom);
    sprintf "%s┃ background: %s\n" indent (option background r#background);
    sprintf "%s┃ content: %s\n" indent (content r#content);
    sprintf "%s┃ canvas: %s\n" indent (option canvas r#canvas);
    sprintf "%s┃ has anim: %s\n" indent (bool (L.has_anim r));
    sprintf "%s┃ clip: %s\n" indent (bool (r#clip));
    sprintf "%s┃ keyboard_focus: %s\n" indent (option bool r#keyboard_focus);
    sprintf "%s┃ house: %s\n" indent (option (layout_up ~indent:(indent^"┃  ")) r#house);
    sprintf "%s┗━" indent ] in
  String.concat "" list

let rec layout_down ?(indent = "") r =
  let list = [
    sprintf "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
    sprintf "%s┃ layer: %d\n" indent (Chain.depth (r#layer));
    sprintf "%s┃ current geometry: %s\n" indent (geometry r#current_geom);
    sprintf "%s┃ background: %s\n" indent (option background r#background);
    sprintf "%s┃ canvas: %s\n" indent (option canvas r#canvas);
    sprintf "%s┃ anim: %s\n" indent (bool (L.has_anim r));
    sprintf "%s┃ clip: %s\n" indent (bool (r#clip));
    sprintf "%s┃ keyboard_focus: %s\n" indent (option bool r#keyboard_focus);
    sprintf "%s┃ house: %s\n" indent (option house r#house);
    sprintf "%s┃ content: %s\n" indent (full_content ~indent:(indent^"┃  ") r#content);
    sprintf "%s┗━" indent ] in
  String.concat "" list

and full_content ?(indent = "") = function
  | L.Leaf (w) -> sprintf "Resident (%s)" (widget w)
  | L.List list -> "Rooms:" ^ (String.concat "," (List.map (layout_down ~indent) list));;



(* Print error messages *)

let dump r =
  let file, ch = Filename.open_temp_file "bogue" ".dump" in
  print_endline ("Saving dump to " ^ file);
  output_string ch (layout_down r);
  close_out ch;;

let layout_error_kf room msg =
  Printf.kfprintf (fun ch -> flush ch) stderr msg;
  dump (L.top_house room);;

let layout_error room msg =
  output_string stderr msg;
  dump (L.top_house room);;
