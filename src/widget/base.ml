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
let fresh_wid = fresh_int ()

type action = t -> t -> Sdl.event -> unit

and connection = {
  source : t;
  target : t;
  action : action;
  priority : action_priority;
  triggers : Trigger.t list;
  id : int;
}

and t = <
  wid : int;
  set_wid : int -> unit;
  (*  receiver : action Event.channel; *) (* TODO: pas nécessaire ? *)
  actives : (active list) Var.t;
  (** all active threads/connections for this widget. Most recent come first in
      the list *)
  connections : connection list;
  set_connections : connection list -> unit;
  (** all possible connections from this widget. In the order to be
      executed. Particular case: the local actions are connection from
      and to the same widget. *)
  fresh : bool Var.t; (* is the display up-to-date ? *)
  (* not really used anymore. TODO: check if this flag is still used *)
  room_id : int option; (* will be filled by the room id when inserted in that room *)
  set_room_id : int option -> unit;
  cursor : Cursor.t;
  set_cursor : Cursor.t -> unit;
  unload : unit;
  size : int * int;
  resize : int * int -> unit;
  display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list;
  typ : string;
>

class virtual w size typ cursor =
  object
    val mutable _wid = fresh_wid ()
    method wid = _wid
    method set_wid x = _wid <- x

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

    (* unload all textures but the widget remains usable. (Rendering will recreate
       all textures) *)
    method unload = ()
    method virtual display : Draw.canvas -> Draw.layer -> Draw.geometry -> Draw.blit list

  end
