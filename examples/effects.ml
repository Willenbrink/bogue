open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger

exception%effect Test : bool -> int

let example () =
  (* let p i x = *)
  (*   Printf.printf "%i\n" i; x *)
  (* in *)

  (* let cc = (ref None : (int, unit) EffectHandlers.Deep.continuation option ref) in *)
  (* begin *)
  (*   match EffectHandlers.perform (Test true) |> p 1 with *)
  (*   | i -> p i () *)
  (*   | [%effect? (Test b), k] -> *)
  (*     p 3 (); *)
  (*     cc := Some k; *)
  (*     10 *)
  (* end; *)




  let b = new Check.t () in
  let b' = new Check.t () in
  let b'' = new Check.t () in
  let l = new Label.t "Init" in
  let l' = new Label.t "Init2" in
  let l'' = new Label.t "Init3" in
  let comb b l = object (self)
    inherit ['a] Row.t [b |> W.gen; l |> W.gen] as super
    method! perform =
      Printf.printf "  Row starts!\n";
      let b = super#perform in
      Printf.printf "  Row returned %b!\n" b;
      if b
      then l#set_text "Checked"
      else l#set_text "Unchecked";
      self#perform
  end
  in
  let row = new Row.t [b |> W.gen;l |> W.gen] in
  let col = new Col.t [comb b l; comb b' l'; comb b'' l''] in
  let layout = L.resident col in
  (* let layout = L.resident row in *)
  (* let layout = L.resident (comb b l) in *)
  let board = make layout in
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
