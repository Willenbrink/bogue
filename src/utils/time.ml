open Utils

(* we use this instead *)
let delay x = Thread.delay (float x /. 1000.)

let now () = Int32.to_int (Sdl.get_ticks ())

let make_fps () =
  let start = ref 0 in
  fun fps ->
    if !start = 0
    then begin
      delay 5;
      start := now ();
    end
    else
      let round_trip = now () - !start in
      begin
        let wait = max 5 ((1000 / fps) - round_trip) in
        printd debug_graphics "FPS:%u (round_trip=%u)\n" (1000 / (round_trip + wait)) round_trip;
        delay wait;
        start := now ();
      end

let adaptive_fps fps =
  let start = ref 0 in
  let frame = ref 1 in
  let total_wait = ref 0 in (* only for debugging *)

  (* the start function *)
  (fun () ->
     start := now ();
     total_wait := 0;
     frame := 1),

  (* the main function *)
  fun () ->
    if !start = 0 then (delay 5; start := now (); assert(false))
    else
      let elapsed = now () - !start in
      let theoric = 1000 * !frame / fps in (* theoric time after this number of frames *)
      let wait = theoric - elapsed in
      total_wait := !total_wait + wait;
      let wait = if wait < 5
        then (printd debug_graphics "Warning: cannot keep up required FPS=%u (wait=%d)" fps wait;
              (* this can also happen when the animation was stopped; we reset
                 the counter *)
              frame := 0;
              total_wait := 0;
              start := now ();
              5)
        else (printd debug_graphics "Wait=%u, Avg. =%u" wait (!total_wait / !frame);
              wait) in
      delay wait;
      incr frame;
      if !frame > 1000000 (* set lower? *)
      then (printd debug_graphics "Reset FPS counter";
            frame := 1;
            total_wait := 0;
            start := now ());;
