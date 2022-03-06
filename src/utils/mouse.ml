open Utils
module E = Sdl.Event

(* mouse position. If the mouse goes over a second window, the new origin
   immediately shifts
   it doesn't work for touchscreen (only the first touch, not motion) *)
let pos = Trigger.mouse_pos

(* return the SDL window where the mouse focus is. If not, return the last
   one. TODO check if window does exist (was not destroyed) *)
let get_window =
  let last_win = ref None in
  fun () ->
    do_option (Sdl.get_mouse_focus ()) (fun win -> last_win := Some win);
    !last_win

let compute_finger_pos ev =
  (* WARNING as of tsdl version??? this is now normalized in 0..1 *)
  let fx = E.(get ev touch_finger_x) in
  let fy = E.(get ev touch_finger_y) in
  match get_window () with
  | None -> failwith "Cannot find window for finger position"
  (* TODO don't fail for this? *)
  | Some win ->
    let w,h = Sdl.get_window_size win in
    Theme.(unscale_f (fx *. float w),unscale_f (fy *. float h))

(* guess where the pointer is, trying mouse first and then touch *)
(* in logical pixels *)
let pointer_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion
  | `Mouse_button_down
  | `Mouse_button_up ->
    let x = E.(get ev mouse_button_x) in
    let y = E.(get ev mouse_button_y) in
    Theme.(unscale_int x, unscale_int y)
  | `Finger_down
  | `Finger_up
  | `Finger_motion ->
    let x,y = compute_finger_pos ev in
    (round x, round y)
  | _ -> begin
      printd debug_error
        "The event for pointer_pos should be a mouse or touch event";
      (* TODO why use not only this call? *)
      Trigger.mouse_pos ()
    end
