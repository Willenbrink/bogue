(* each widget contains its personal data, and the list of connections from
   it *)

open Interop
open Utils
include Base

class type virtual ['a] t = ['a] w

(* let canvas w = match w.canvas with *)
(*   | Some c -> c *)
(*   | None -> failwith "Canvas not defined";; *)

(* let renderer w = *)
(*   (canvas w).Draw.renderer;; *)

(* let set_canvas canvas w = *)
(*   w.canvas <- Some canvas;; *)

(** creation of simple widgets *)
let check_box ?init ?style () =
  (* let b = create_empty  (Check (Check.create ?state ?style ())) in *)
  new Check.t ?init ?style ()

let rich_text ?font_size ?size paragraphs = new Text_display.t ?font_size ?size paragraphs

(*
let text_display ?w ?h text =
  create_empty (TextDisplay (Text_display.create_from_string ?w ?h text));;

let lines_display ?w ?h lines =
  create_empty (TextDisplay (Text_display.create_from_lines ?w ?h lines));;

let verbatim text =
  create_empty (TextDisplay (Text_display.create_verbatim text));;

let html text =
  create_empty (TextDisplay (Text_display.create_from_html text));;

let box ?w ?h ?background ?border ?shadow () =
  create_empty (Box (Box.create ?width:w ?height:h ?background ?border ?shadow ()));;

let label ?size ?fg ?font text =
  create_empty (Label (Label.create ?size ?fg ?font text));;

(* alias for fontawesome icon labels *)
let icon ?size ?fg name =
  create_empty (Label (Label.icon ?size ?fg name));;

let image ?w ?h ?bg ?noscale file =
  create_empty (Image (Image.create ?width:w ?height:h ?bg ?noscale file));;

let image_from_svg ?w ?h ?bg file =
  let svg = Draw.convert_svg ?w ?h file in
  let w,h = Draw.unscale_size (Draw.image_size svg) in
  image ~w ~h ?bg svg;;
 * FIXME Removed constructors *)

(** creation of combined widgets *)
(* let check_box_with_label text = *)
(*   let b = check_box () in *)
(*   let l = new Label.t text in *)
(*   (\* new Row.t [b |> gen; l |> gen] *\) *)
(*   connect_main l ~target:b (fun ev -> b#handle ev (Draw.make_geom ()) |> ignore) Trigger.buttons_down; *)
(*   b,l *)
