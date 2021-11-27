(** the queue of actions to be executed before everything in the main loop *)
(* NOTE: if some actions take too much time, the remaining ones are postponed to
   next iteration of the main loop. *)
(* FIFO queue: order of actions is preserved *)

open Utils

type action = unit -> unit

let queue : action Queue.t = Queue.create ()

let is_empty () =
  Queue.is_empty queue

(* Warning: a Sync action cannot push another Sync action, because it will wait
   forever the release of the mutex *)
(* TODO: add a test of this using Mutex.try_lock *)
let push action =
  printd debug_thread "Sync push action";
  Queue.push action queue;
  Trigger.push_action ()

(** returns true if some action was executed *)
let execute timeout =
  if is_empty () then false (* a quick test in order to avoid lock *)
  else let t = Time.now () in
    let q = queue in
    let rec loop () =
      if Queue.is_empty q then () (* we exit *)
      else if Time.(now () - t > timeout)
      then (* we exit but also send a action event to finish the rest of the
              queue at next iteration. *)
        Trigger.push_action ()
      else let action = Queue.pop q in
        printd debug_thread "Popping one action from the Sync Queue";
        action ();
        loop ();
    in
    loop ();
    true
