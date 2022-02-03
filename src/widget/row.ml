open Base

(* TODO Unify row and col? Pay attention to performance! ´
   We must quickly map a point to the correct child for event handling *)

class ['a] t ?(flip = false) ?(sep = Theme.room_margin) ?(name = "Row") children =
  let children' =
    let f (x,cs) (Any c) =
      ((x + (if flip then snd else fst) c#size + sep), (x, (c :> 'a w))::cs)
    in
    List.fold_left f (0,[]) children |> snd
  in
  let size =
    let f (x,y) (Any c) =
      let (x_c,y_c) = c#size in
      if flip
      then (max x x_c, y+y_c)
      else (x+x_c, max y y_c)
    in
    List.fold_left f (0,0) children
  in
  object (self)
    inherit ['a] w size name Cursor.Arrow

    val children = children'

    method unload = List.iter (fun (_,x) -> x#unload) children

    method triggers = List.concat_map (fun (_,c) -> c#triggers) children

    method! handle ev (g : Draw.geometry) =
      let (x_m,y_m) = Mouse.pointer_pos ev in
      assert (g.x <= x_m && x_m <= g.x + g.w);
      assert (g.y <= y_m && y_m <= g.y + g.h);
      let base = if flip then g.y else g.x in
      let f offset (x,c) =
        if offset < x_m
        then base + x
        else (if List.mem (Trigger.of_event ev) c#triggers
              then c#handle ev g;
              raise Not_found)
      in
      match
        List.fold_left f base children
      with
      | exception Not_found -> ()
      | _ -> ()

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
