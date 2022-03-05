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
  let ti = new Text_input.t "Text field" in
  let comb b l = object (self)
    inherit ['a] Row.t [b |> W.gen; l |> W.gen] as super
    method! execute =
      let res = super#execute in
      if res
      then l#set_text "Checked"
      else l#set_text "Unchecked";
      self#execute
  end
  in
  let comb' b l = object (self)
    inherit ['a] Row.t [b |> W.gen; l |> W.gen] as super
    method! execute =
      let res = super#execute in
      l#set_text res;
      self#execute
  end
  in
  let row_b = new Row.t [b |> W.gen; l |> W.gen] in
  let row_ti = new Row.t [ti |> W.gen; l |> W.gen; new Single.loop b] in
  let col = new Col.t [comb b l; comb b' l'; comb b'' l''] in
  (* let layout = L.resident col in *)
  (* let layout = L.resident (comb b l) in *)
  (* let layout = L.resident row_b in *)
  let layout = L.resident row_ti in
  let board = make layout in
  run board
let _ =
  Printexc.record_backtrace true;
  (* Sys.( *)
  (*   let handler = Signal_handle (fun _ -> Printexc.print_backtrace stdout; failwith "") in *)
  (*   signal sigint handler |> ignore; *)
  (*   signal sigsegv handler |> ignore; *)
  (* ); *)
  (try
     example ();
     Draw.quit ()
   with
   | Layout.Fatal_error (_,str) -> Printexc.print_backtrace stdout; print_endline str
   | e -> Printexc.print_backtrace stdout; Printexc.to_string e |> print_endline);

  Stdlib.exit 0
