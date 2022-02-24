(* each widget contains its personal data, and the list of connections from
   it *)

open Utils
include Base

class type virtual ['a] t = ['a] w

let draw_boxes = ref false;;
(* for debugging: draws a red rect rectangle around each widget layout, (fill
   when it has mouse focus (might need a redraw: CTRL-l) and a blue rect around
   container layouts *)

let test () = [
  `Empty (new Empty.t (0,0));
  `Box (new Box.t ());
  `Slider (new Slider.t ());
  `Button (new Button.t "Press Me");
  `Image (new Image.t "/non_existing.png")
]

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
let check_box_with_label text =
  let b = check_box () in
  let l = new Label.t text in
  connect_main l ~target:b (fun ev -> b#handle ev (Draw.make_geom ())) Trigger.buttons_down;
  b,l

(****)

(* some useful connections *)
(* the disadvantage is that these functions do not take advantage of the two
   widgets + event entry. Thus they are less 'functional' and require more
   global variables. Also, they all work with "connect_main", so are ok only for
   very fast actions. *)

let mouse_over ?(enter = nop) ?(leave = nop) w =
  connect w ~target:w (fun _ -> enter w) [Trigger.mouse_enter];
  connect w ~target:w (fun _ -> leave w) [Trigger.mouse_leave]

let on_click ~click w =
  connect_main w ~target:w (fun _ -> click w) Trigger.buttons_down

let on_release ~release w =
  connect_main w ~target:w (fun _ -> release w) Trigger.buttons_up

(****)

(** check if the trigger can wake up a connection, and if so, run the action *)
let wake_up event (c : connection) =
  if List.mem (Trigger.of_event event) c#triggers then
    begin
      printd debug_thread "Activating connection #%d" c#id;
      c#action event
    end

let wake_up_all ev w = List.iter (wake_up ev) w#connections

(*******************)

(* some widgets directly react to a click event to activate themselves. Some,
   like text_input, even react to the TAB key. In fact, keyboard_focus is
   treated globally by the main loop, therefore one could (should ?) rely on
   this function below instead of adding new reactions to TAB & click *)
(* TODO: buttons could have keyboard focus... to activate them with TAB or ENTER
   or SPACE... *)
