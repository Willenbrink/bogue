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
      List.map (fun (o,c) -> (o, c)) children'

    val mutable keyboard_focus = None

    method unload = List.iter (fun (_,c) -> c#unload) children

    method private handle children ((ev : Event.t_rich), (g : Draw.geometry)) =
      let base = if flip then g.y else g.x in
      let child_of_pos children (x_m,y_m) =
        (* Printf.printf "%s, geom: %ix,%iy %iw,%ih\n" name g.x g.y g.w g.h; *)
        (* Printf.printf "%s, mouse: %i,%i\n" name x_m y_m; *)
        assert (g.x <= x_m && x_m <= g.x + g.w);
        assert (g.y <= y_m && y_m <= g.y + g.h);
        let x_m, y_m = x_m - g.x, y_m - g.y in

        let rec iterate = function
          | [] -> None
          | cc::cs ->
            let offset' = base + !cc#offset in
            if (if flip then y_m else x_m) >= offset'
            then if (if flip then x_m else y_m) < (if flip then fst else snd) !cc#size
              then begin
                (* Printf.printf "Success at offset %i with geom %i %i\n" offset' x y; *)
                Some (cc);
              end
              else None
            else iterate cs
        in
        iterate children
      in

      let child_delegate =
        match ev with
        | `Key_press _ | `Key_repeat _
        | `Key_release _ | `Codepoint _ ->
          begin match keyboard_focus with
            | None -> failwith "No child is listening for keyboard events but row is!"
            | Some c -> Some c
          end
        | `Scroll -> assert false (* TODO *)
        | `Mouse_motion pos | `Mouse_enter pos
        | `Mouse_leave pos | `Mouse_press (pos,_)
        | `Mouse_release (pos,_) ->
          child_of_pos children pos
      in
      match child_delegate with
      | None -> None
      | Some cc ->
        let offset' = base + !cc#offset in
        let x = if flip then g.x else g.x + offset' in
        let y = if flip then g.y + offset' else g.y in
        (* Printf.printf "Success at offset %i with geom %i %i\n" offset' x y; *)
        let geom = {g with x; y} in
        if not (List.mem (Event.strip ev) !cc#triggers)
        then None
        else begin
          Printf.printf "%s --> %s\n" self#name !cc#name;
          match !cc#continue ev geom with
          | Either.Left x -> Some x
          | Either.Right cc' ->
            cc := cc';
            None
        end

    method execute =
      let child_conts =
        List.map (fun (o,c) ->
            Printf.printf "%s executing %s\n" self#name c#name;
            match c#execute with
            | x ->
              Printf.printf "%s <-- %s\n" self#name c#name;
              Either.Left x
            | [%effect? (Await triggers), k] ->
              (* We use objects to model the recursive nature of continue *)
              Either.Right (object (_ : 'self)
                method offset = o
                method triggers = triggers
                method size = c#size
                method name = c#name
                method continue ev geom : ('a,'self) Either.t = EffectHandlers.Deep.continue k (ev,geom)
                method discontinue exn = EffectHandlers.Deep.discontinue k exn
              end)
          ) children
      in
      match List.filter_map (Either.find_left) child_conts with
      | x :: _ -> x
      | [] ->
        let child_conts =
          List.map (function
              | Either.Left _ -> failwith "Impossible" (* TODO write this in a type safe way *)
              | Either.Right x ->
                let ref_cell = ref x in
                if List.exists (fun t -> List.mem t Event.keyboard_events) x#triggers
                then keyboard_focus <- Some ref_cell;
                ref_cell)
            child_conts
        in
        let triggers () =
          List.concat_map (fun cc -> !cc#triggers) child_conts
        in
        let rec loop () =
          match await (triggers ()) (self#handle child_conts) with
          | Some res ->
            (* Printf.printf "Result obtained\n%!"; *)
            res
          | None ->
            (* Printf.printf "Looping\n%!"; *)
            loop ()
        in
        (* Printf.printf "Starting loop\n%!"; *)
        let res = loop () in
        (* Printf.printf "Ending loop\n%!"; *)
        res

    method display canvas layer geom =
      let f (x,c) =
        c#display canvas layer Draw.{geom with x = geom.x + (if flip then 0 else x);
                                               y = geom.y + (if flip then x else 0);
                                               w = min geom.w (fst c#size);
                                               h = min geom.h (snd c#size);
                                    }
      in
      let blits = List.concat_map f children in
      blits

  end
