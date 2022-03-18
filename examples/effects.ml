open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger

let example () =
  let sl = new Slider.t ~kind:Slider.Vertical in
  let c = new Check.t in
  let l = new Label.t in
  let ti = new Text_input.t  in
  let comb b l = object (self)
    inherit ['a] Row.pair b l as super
    method! execute await =
      let res = super#execute await in
      if res
      then l#set_text "Pressed"
      else l#set_text "Released";
      self#execute
  end
  in
  let row_ti =
    let label =
      l "Enter some text on the right and press enter/tab."
    in
    new Single.t (fun self child ->
        let text = child#execute in
        label#set_text text;
        self#execute
      ) @@
    new Row.t [
      label |> W.gen;
      ti "Text input" |> W.gen;

      (* TODO for some reason not working right now. The click never reaches the slider *)
      (* (Single.loop *)
      (*    (sl () |> W.gen) *)
      (*  |> W.gen) *)
      (* |> W.gen; *)
    ]
  in
  (* let col = new Col.t [comb b' l'; comb b'' l''] in *)
  (* let layout = L.resident col in *)
  (* let layout = L.resident (comb b l) in *)
  (* let layout = L.resident row_b in *)
  let layout = L.resident (Single.loop @@ new Col.t [
      Single.loop (row_ti |> W.gen) |> W.gen;
      Single.loop (comb (new Button.t ~switch:true "Switch") (l "Init") |> W.gen) |> W.gen;
      Single.loop (comb (new Button.t "Hold") (l "Init") |> W.gen) |> W.gen
    ])
  in
  run layout

let _ =
  run @@ fun () ->
  let* checked = check ||| label in
  if checked
  then label#set_text "Checked"
  else label#set_text "Unchecked";
  raise Repeat


let _ =
  Printexc.record_backtrace true;
  (* Sys.( *)
  (*   let handler = Signal_handle (fun _ -> Printexc.print_backtrace stdout; failwith "") in *)
  (*   signal sigint handler |> ignore; *)
  (*   signal sigsegv handler |> ignore; *)
  (* ); *)
  (try
     example ();
     Interop.Draw.quit ()
   with
   | Layout.Fatal_error (_,str) -> Printexc.print_backtrace stdout; print_endline str
   | e -> Printexc.print_backtrace stdout; Printexc.to_string e |> print_endline);

  Stdlib.exit 0
