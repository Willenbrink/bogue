open Utils
open Tsdl

(** what to do when the same action (= same connection id) is already running ? *)
type action_priority =
  | Forget (** discard the new action *)
  | Join (** execute the new after the first one has completed *)
  | Replace (** kill the first action (if possible) and execute the second one *)
  | Main (** run in the main program. So this is blocking for all subsequent actions *)

type active = {
  thread : Thread.t; (** this is the thread launched by the connection with given id *)
  event : Sdl.event; (* this is the event passed to the "action".  It is used
                        also for communication *)
  connect_id : int
}

let fresh_id = fresh_int ()

class virtual w size typ cursor =
  object (self)
    val mutable _id = fresh_id ()
    method id = _id
    method set_id x = _id <- x

    method typ : string = typ

    val mutable _size : int * int = size
    method size = _size
    method resize x = _size <- x

    val mutable _cursor : Cursor.t = cursor
    method cursor = _cursor
    method set_cursor x = _cursor <- x

    val mutable actives_ : active list Var.t = Var.create []
    method actives = actives_

    method fresh = Var.create false;

    val mutable connections_ : connection list = []
    method connections = connections_
    method set_connections c = connections_ <- c

    val mutable room_id_ : int option = None
    method room_id = room_id_
    method set_room_id rid = room_id_ <- rid

    method set_keyboard_focus = ()
    method remove_keyboard_focus = ()
    method guess_unset_keyboard_focus = true

    method update =
      (** ask for refresh *)
      (* Warning: this is frequently called by other threads *)
      (* Warning: this *resets to 0* the user_window_id *)
      (* anyway, it is not clear if the user_window_id field for created event types
         is really supported by (T)SDL *)
      printd debug_board "Please refresh";
      Var.set self#fresh false;
      (* if !draw_boxes then Trigger.(push_event refresh_event) *)
      (* else *)
      Trigger.push_redraw self#id (*TODO... use wid et/ou window_id...*)
    (* refresh is not used anymore. We redraw everyhting at each frame ... *)
    (* before, it was not very subtle either: if !draw_boxes is false, we ask for
       clearing the background before painting. Maybe some widgets can update
       without clearing the whole background. But those with some transparency
       probably need it. This should not be necessary in case we draw a solid
       background -- for instance if draw_boxes = true *)


    (* unload all textures but the widget remains usable. (Rendering will recreate
       all textures) *)
    method unload = ()
    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list

  end

and connection src dst action ?(priority=Forget) ?(update_target=true) ?join triggers =
  object
    method src : w = src
    method dst : w = dst
    method action (ev : Sdl.event) : unit =
      if !debug
      then
        (printd debug_thread "Executing action";
         let t = Unix.gettimeofday () in
         action ev;
         printd debug_thread "End of action with time=%f" (Unix.gettimeofday () -. t))
      else
        action ev;

      (* TODO ajouter Trigger.will_exit ev ?? *)
      if update_target then dst#update

    method priority = priority
    method triggers = triggers

    method id = match join with
      | None -> fresh_id ()
      | Some c -> c#id

    initializer
      if update_target && (List.mem Sdl.Event.user_event triggers)
      then printd debug_warning "one should not 'connect' with 'update_target'=true if the trigger list contains 'user_event'. It may cause an infinite display loop";
  end
