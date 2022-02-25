open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger
open Printf

let example () =
  let b = W.check_box () in
  let l = new Label.t "Init" in
  let comb = object
    inherit ['a] Row.t [b |> W.gen; l |> W.gen]
    method! perform =
      let bool = b#perform in
      if bool
      then l#set_text "Checked"
      else l#set_text "Unchecked"
  end
  in
  (* let comb = new Row.t [b |> W.gen; l |> W.gen] in *)
  let board = make @@ L.resident comb in
  run board

let _ =
  Printexc.record_backtrace true;
  (try
     example ();
     Draw.quit ()
   with
   | Layout.Fatal_error (_,str) -> Printexc.print_backtrace stdout; print_endline str
   | e -> Printexc.print_backtrace stdout; Printexc.to_string e |> print_endline);

  Stdlib.exit 0
