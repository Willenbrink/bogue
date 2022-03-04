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
    inherit ['a] w size name Cursor.Arrow as super

    val mutable children
    (* : (int * 'a w * unit cc option) list *)
      =
      List.map (fun (o,c) -> (o, c, ref None)) children'

    method unload = List.iter (fun (_,c,_) -> c#unload) children

    method triggers = List.concat_map
        (* (fun r -> *)
        (* match !r with *)
        (* | None -> [] *)
        (* | Some (ts,_) -> ts) *)
        (* ccs *)
        (fun (_,c) -> c#triggers) children'

    method handle ev (g : Draw.geometry) =
      let x_m, y_m = Mouse.pointer_pos ev in
      (* Printf.printf "%s, geom: %i,%i\n" name g.x g.y; *)
      assert (g.x <= x_m && x_m <= g.x + g.w);
      assert (g.y <= y_m && y_m <= g.y + g.h);
      let x_m, y_m = x_m - g.x, y_m - g.y in
      let base = if flip then g.y else g.x in
      (* Printf.printf "%s, mouse: %i,%i\n" name x_m y_m; *)
      let f acc (o,(c : bool w), cco) = match acc with
        | Some _ -> acc
        | None ->
          let offset' = base + o in
          if (if flip then y_m else x_m) >= offset'
          && (if flip then x_m else y_m) < (if flip then fst else snd) c#size
          then begin
            let x = if flip then g.x else g.x + offset' in
            let y = if flip then g.y + offset' else g.y in
            (* Printf.printf "Success at offset %i with geom %i %i\n" offset' x y; *)
            Some (c,{g with x; y}, cco)
          end
          else acc
      in
      match List.fold_left f (None) children with
      | None ->
        Printf.printf "Click missed target\n";
        None
      | Some (c,g,cco) ->
        match !cco with
        | None -> failwith "Child of row has no continuation"
        | Some (ts,cc) ->
          if List.mem (Trigger.of_event ev) ts
          then begin
            Printf.printf "%s --> %s\n" self#name c#name;
            let res = cc ev g in
            self#update;
            res
          end
          else begin
            Printf.printf "Click not in %s triggers\n" c#name;
            None
          end

    method! perform =
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
            let _ = match c#perform with
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
        super#perform


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

(* TODO *)
let x = (None : bool t option)
