open Base

(* TODO Unify row and col? Pay attention to performance! ´
   We must quickly map a point to the correct child for event handling *)

(* TODO
   Handle alignment
*)
class ['a] t ?(flip = false) ?(sep = Theme.room_margin)
    ?(align) ?(name = if flip then "Col" else "Row") (children : 'a Base.w list) =
  let children' =
    let f (x,cs) c =
      ((x + (if flip then snd else fst) c#size + sep), (x, (c :> 'b w))::cs)
    in
    let res = List.fold_left f (0,[]) children |> snd in
    assert (List.length res > 0);
    res
  in
  let size =
    let f (x,y) c =
      let (x_c,y_c) = c#size in
      if flip
      then (max x x_c, y+y_c)
      else (x+x_c, max y y_c)
    in
    List.fold_left f (0,0) children
  in
  object (self)
    inherit ['a] w size name Cursor.Arrow

    val mutable children
    (* : (int * 'a w * unit cc option) list *)
      =
      List.map (fun (o,c) -> (o, c, ref None)) children'

    method unload = List.iter (fun (_,c,_) -> c#unload) children

    (* TODO horrible code. Cleanup! *)
    method private handle (ev, (g : Draw.geometry)) =
      match ev with
      | `Key_press _ | `Key_repeat _
      | `Key_release _ | `Codepoint _ ->
        let base = if flip then g.y else g.x in
        let res = ref None in
        let f (o,c, cco) =
          let offset' = base + o in
          let x = if flip then g.x else g.x + offset' in
          let y = if flip then g.y + offset' else g.y in
          let g = {g with x; y} in
          match !cco with
          | None -> failwith @@ "Child " ^ c#name ^ " of row has no continuation"
          | Some (ts,cc) ->
            if List.mem (Event.strip (ev :> Event.t_rich)) ts
            then begin
              Printf.printf "%s --> %s\n" self#name name;
              res := cc ev g
            end
        in
        begin
          try
            List.iter f children;
            None
          with Reset -> !res
        end

      | `Scroll -> assert false (* TODO *)
      | `Mouse_motion pos
      | `Mouse_enter pos
      | `Mouse_leave pos
      | `Mouse_press (pos,_)
      | `Mouse_release (pos,_) ->
        let x_m, y_m = pos in
        (* Printf.printf "%s, geom: %ix,%iy %iw,%ih\n" name g.x g.y g.w g.h; *)
        (* Printf.printf "%s, mouse: %i,%i\n" name x_m y_m; *)
        assert (g.x <= x_m && x_m <= g.x + g.w);
        assert (g.y <= y_m && y_m <= g.y + g.h);
        let x_m, y_m = x_m - g.x, y_m - g.y in
        let base = if flip then g.y else g.x in
        let res = ref None in
        let f (o,c, cco) =
          let offset' = base + o in
          if (if flip then y_m else x_m) >= offset'
          then if (if flip then x_m else y_m) < (if flip then fst else snd) c#size
            then begin
              let x = if flip then g.x else g.x + offset' in
              let y = if flip then g.y + offset' else g.y in
              (* Printf.printf "Success at offset %i with geom %i %i\n" offset' x y; *)
              res := Some (c#name,{g with x; y}, !cco);
              raise Reset
            end
            else raise Reset
          else ()
        in
        match List.iter f children with
        | () ->
          None
        | exception Reset ->
          match !res with
          | None -> None
          | Some (n,_,None) -> failwith @@ "Child " ^ n ^ " of row has no continuation"
          | Some (name,g, Some (ts,cc)) ->
            if List.mem (Event.strip ev) ts
            then begin
              Printf.printf "%s --> %s\n" self#name name;
              let res = cc ev g in
              res
            end
            else begin
              (* Printf.printf "Click not in %s triggers\n" c#name; *)
              None
            end

    method execute =
      let ret = ref None in
      (* TODO This currently automatically restarts any widget that terminated.
         What is row supposed to do? Automatically remove that widget?
         Replace by empty? *)
      List.iter (fun (_,c,ccor) ->
          match !ccor with
          | Some _ -> () (* TODO consider what to do here. Keep the cc or reset it?
                            If we reset it, how? Storing only k in ccor is somehow
                            not directly possible *)
          | None ->
            Printf.printf "%s performing %s\n" self#name c#name;
            let _ = match c#execute with
              | x ->
                Printf.printf "%s <<< %s\n" self#name c#name;
                ret := Some x;
                ccor := None; (* This continuation is done *)
                Some x
              | [%effect? (Await triggers), k] ->
                ccor := Some (triggers, fun ev geom ->
                    EffectHandlers.Deep.continue k (ev,geom));
                None
            in
            ()
        ) children;
      match !ret with
      | Some x -> x
      | None ->
        let triggers () =
          List.concat_map (fun (_,_,ccor) -> match !ccor with Some (ts,_) -> ts | _ -> []) children
        in
        let rec loop () =
          match await (triggers ()) self#handle with
          | Some res ->
            Printf.printf "Result obtained\n%!";
            res
          | None ->
            Printf.printf "Looping\n%!";
            loop ()
        in
        Printf.printf "Starting loop\n%!";
        let res = loop () in
        Printf.printf "Ending loop\n%!";
        res

    method display canvas layer geom =
      let f (x,c,_) =
        c#display canvas layer Draw.{geom with x = geom.x + (if flip then 0 else x);
                                               y = geom.y + (if flip then x else 0);
                                               w = min geom.w (fst c#size);
                                               h = min geom.h (snd c#size);
                                    }
      in
      let blits = List.concat_map f children in
      blits

  end
