open Bogue
open Main
module W = Widget

let example () =
  let sl = new Slider.t in
  let c = new Check.t in
  let l = new Label.t in
  let ti = new Text_input.t  in
  let comb b l =
    new Row.pair_id ~logic:(fun self await yield res ->
        l#set_text (if res then "Pressed" else "Released")
      ) b l
  in
  let row_ti =
    let label = l "Enter some text on the right and press enter/tab." in
    let ti = ti "Text Input" in
    new Row.pair_id ~logic:(fun self await yield res ->
        label#set_text res
      ) label ti
  in
  (* let col = new Col.t [comb b' l'; comb b'' l''] in *)
  (* let layout = L.resident col in *)
  (* let layout = L.resident (comb b l) in *)
  (* let layout = L.resident row_b in *)
  let layout =
        new Row.pair ~flip:true
          row_ti
          begin
            new Row.pair ~flip:true
              (comb (new Button.t ~switch:true "Switch") (l "Init"))
              (comb (new Button.t "Hold") (l "Init"))
          end
  in
  enter layout

(* let _ = *)
(*   run @@ fun () -> *)
(*   let* checked = check ||| label in *)
(*   if checked *)
(*   then label#set_text "Checked" *)
(*   else label#set_text "Unchecked"; *)
(*   raise Repeat *)


let _ =
  Printexc.record_backtrace true;
  (* Sys.( *)
  (*   let handler = Signal_handle (fun _ -> Printexc.print_backtrace stdout; failwith "") in *)
  (*   signal sigint handler |> ignore; *)
  (*   signal sigsegv handler |> ignore; *)
  (* ); *)
  try
    example ()
  with
  | e ->
    Printexc.print_backtrace stdout;
    Printexc.to_string e
    |> print_endline
