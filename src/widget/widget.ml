(* each widget contains its personal data, and the list of connections from
   it *)

open Tsdl
open Utils
include Base

class type virtual t = w

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

let get_room_id w = match w#room_id with
  | None -> failwith "The widget does not belong to a room yet"
  | Some id -> id;;

let is_fresh w = Var.get w#fresh;;

(* let canvas w = match w.canvas with *)
(*   | Some c -> c *)
(*   | None -> failwith "Canvas not defined";; *)

(* let renderer w = *)
(*   (canvas w).Draw.renderer;; *)

(* let set_canvas canvas w = *)
(*   w.canvas <- Some canvas;; *)

let dummy_widget id =
  let open Base in
  let dummy = new Empty.t ~id (0,0) in
  dummy

(*let of_id id = Hashtbl.find widgets_table id;;*)
let of_id id =
  let open Base in
  try WHash.find widgets_wtable (dummy_widget id) with
  | Not_found -> (printd debug_error "Cannot find widget with id=%d" id;
                  raise Not_found);;

(** creation of simple widgets *)
let check_box ?state ?style () =
  (* let b = create_empty  (Check (Check.create ?state ?style ())) in *)
  let b = new Check.t ?state ?style () in
  let action _ = b#action in
  connect_main (b :> t) ~target:(b :> t) action Trigger.buttons_down;
  b

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
 * FIXME *)

(** creation of combined widgets *)
let check_box_with_label text =
  let b = check_box () in
  let l = new Label.t text in
  let action = fun _ w _ -> w#action in
  connect_main l ~target:b (action () b) Trigger.buttons_down;
  b,l

(****)

(* some useful connections *)
(* the disadvantage is that these functions do not take advantage of the two
   widgets + event entry. Thus they are less 'functional' and require more
   global variables. Also, they all work with "connect_main", so are ok only for
   very fast actions. *)

let mouse_over ?(enter = nop) ?(leave = nop) w =
  connect w ~target:w (fun ev -> enter w) [Trigger.mouse_enter];
  connect w ~target:w (fun ev -> leave w) [Trigger.mouse_leave]

let on_click ~click w =
  connect_main w ~target:w (fun ev -> click w) Trigger.buttons_down

let on_release ~release w =
  connect_main w ~target:w (fun ev -> release w) Trigger.buttons_up

(****)

(** check if connection is in the active list, and return the most
    recent (=first in list) active, or None *)
let is_active alist c =
  let rec loop = function
    | [] -> None
    | a::rest -> if a.connect_id = c#id then Some a else loop rest
  in loop alist;;

(** remove an 'active' from the active list of the widget *)
(* it should occur only once in the list *)
let remove widget thread_id =
  let rec loop list acc = match list with
    | [] -> acc
    | a::rest -> (* if a.connect_id = active.connect_id *)
      (* test inutile, le suivant suffit *)
      if Thread.id a.thread = thread_id
      then List.concat [List.rev rest; acc]
      else loop rest (a::acc)
  in Var.set widget#actives (List.rev (loop (Var.get widget#actives) []));;

let add widget active =
  Var.set widget#actives (active :: (Var.get widget#actives));;

(** ask a thread to remove itself from a widget *)
let remove_me c_id widget =
  printd debug_thread "Removing connection #%d" c_id;
  remove widget (Thread.id (Thread.self ()));
  decr threads_created;;

(* check if connection is terminated *)
(* (only if the thread decided to signal this, for instance by setting the event
   to Trigger.stopped) *)
let has_terminated active =
  Sdl.Event.(get active.event typ) <> Trigger.stop;;

(* indicate to an active connection that its thread should terminate *)
(* TODO protect this with mutex or Var *)
let terminate ?(timeout = 50) active =
  printd debug_thread "Ask for terminating connection #%u" active.connect_id;
  Sdl.Event.(set active.event typ) Trigger.stop;
  ignore (Timeout.add timeout (fun () ->
      if not (has_terminated active)
      then printd debug_thread "Cannot terminate thread for connection #%u after %u ms." active.connect_id timeout
    ));;

(* ask for terminate and wait (blocking) until it really terminates by itself *)
let wait_terminate active =
  terminate active;
  Thread.join active.thread;;

(** activate an action (via a thread) on the connection *)
let add_action c action ev =
  printd debug_thread "Create thread for connection #%d" c#id;
  (* Trigger.renew_my_event (); *)
  (* we used to create a new event for the main loop, so that "ev" can be safely
     sent to the thread, and the thread can examine later, even after several
     main loops, without it being altered (except when exiting is required) *)
  (* Now we use a more natural, solution would be to copy the event before
     sending it to the thread, but there is no "copy_event" function
     available... *)
  (* WARNING: at this point it is not possible to copy the drop_file_file field *)
  let e_copy = Trigger.copy_event ev in
  incr threads_created;
  let src = c#src in
  add src
    { thread = Thread.create action e_copy;
      event = e_copy;
      connect_id = c#id };;

(** check if the trigger can wake up a connection, and if so, run the action *)
let wake_up event (c : connection) =
  if List.mem (Trigger.of_event event) c#triggers then
    begin
      printd debug_thread "Activating connection #%d" c#id;
      (* TODO add a more precise ~test before launching the thread ? *)
      if c#priority = Main then c#action event
      (* = direct action, no thread !. Should we still add it to the active list
         ? *)
      else begin
        let action ev =
          c#action ev;
          remove_me c#id c#src
        in
        let alist = Var.get c#src#actives in
        let tho = is_active alist c in
        if alist = [] || tho = None then add_action c action event
        else match c#priority, tho with
          | Forget, _ -> printd debug_thread "Forgetting connection #%d" c#id
          | Join, Some a ->
            let action ev = Thread.join a.thread; action ev in
            add_action c action event
          | Replace, Some a -> begin
              (*printd debug_thread "Killing connection #%d" a.connect_id;*)
              (* Thread.kill a.thread; *) (* Thread.kill is in fact NOT
                                             implemented... ! *)
              terminate a;
              remove c#src (Thread.id a.thread);
              add_action c action event
            end
          | _ -> failwith "This should not happen"
      end
    end;;

let wake_up_all ev w =
  List.iter (wake_up ev) w#connections;;

(** remove all active connections from this widget and ask for the threads to
    terminate *)
let remove_active_connections widget =
  let actives = Var.get widget#actives in
  List.iter wait_terminate actives;
  Var.set widget#actives [];;


(*******************)

(* some widgets directly react to a click event to activate themselves. Some,
   like text_input, even react to the TAB key. In fact, keyboard_focus is
   treated globally by the main loop, therefore one could (should ?) rely on
   this function below instead of adding new reactions to TAB & click *)
(* TODO: buttons could have keyboard focus... to activate them with TAB or ENTER
   or SPACE... *)
