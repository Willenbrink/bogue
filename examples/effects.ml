open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger

let example () =
  let b = new Check.t () in
  let b' = new Check.t () in
  let b'' = new Check.t () in
  let l = new Label.t "Init" in
  let l' = new Label.t "Init2" in
  let l'' = new Label.t "Init3" in
  let comb b l = object (self)
    inherit ['a] Row.t [b |> W.gen; l |> W.gen]
    method! perform =
      if b#perform
      then l#set_text "Checked"
      else l#set_text "Unchecked";
      self#perform
  end
  in
  let col = new Col.t [comb b l; comb b' l'; comb b'' l''] in
  let board = make @@ L.resident col in
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
