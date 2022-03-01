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
      List.map (fun (o,c) -> (o, c)) children'

    method unload = List.iter (fun (_,c) -> c#unload) children

    method triggers = List.concat_map (fun (_,c) -> c#triggers) children

    method handle ev (g : Draw.geometry) =
      let x_m, y_m = Mouse.pointer_pos ev in
      Printf.printf "%s, geom: %i,%i\n" name g.x g.y;
      assert (g.x <= x_m && x_m <= g.x + g.w);
      assert (g.y <= y_m && y_m <= g.y + g.h);
      let x_m, y_m = x_m - g.x, y_m - g.y in
      let base = if flip then g.y else g.x in
      Printf.printf "%s, mouse: %i,%i\n" name x_m y_m;
      let f acc (o,(c : bool w)) = match acc with
        | Some _ -> acc
        | None ->
          let offset' = base + o in
          Printf.printf "At offset %i\n" offset';
          if (if flip then y_m else x_m) >= offset'
          then
            if (if flip then x_m else y_m) < (if flip then fst else snd) c#size
            && List.mem (Trigger.of_event ev) c#triggers
            then Some (c,{g with x = (if flip then g.x else g.x + offset');
                                 y = (if flip then g.y + offset' else g.y)})
            else acc
          else acc
      in
      match List.fold_left f None children with
      | None ->
        None
      | Some (c,g) ->
        let ret = c#handle ev g in
        self#update;
        ret

    (* method! perform = *)
    (*   children <- List.map (fun (o,(c : bool w),cco) -> *)
    (*       match cco with *)
    (*       | Some _ -> (o,c,cco) *)
    (*       | None -> *)
    (*         let cc = ref None in *)
    (*         begin *)
    (*           match c#perform with *)
    (*           | _ -> failwith (Printf.sprintf "Widget %s: terminated" c#name) *)
    (*           | [%effect? (Await triggers), k] -> *)
    (*             cc := Some (triggers, fun ev geom -> *)
    (*                 Printf.printf "Continuing form row!\n%!"; *)
    (*                 EffectHandlers.Deep.continue k (ev,geom) |> ignore; *)
    (*                 assert false) *)
    (*         end; *)
    (*         (o,c,!cc) *)
    (*     ) children; *)
    (*   super#perform *)

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

(* TODO *)
let x = (None : bool t option)
