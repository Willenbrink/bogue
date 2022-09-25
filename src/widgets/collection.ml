open Base

class virtual ['a] t ?(name = "Collection") size children =
  let children = List.map (fun (g,c) -> (g,(c :> 'b w))) children in
  (* TODO only necessary if not virtual
     let size =
     let f (x,y,w,h) ((x',y'),c) =
      (min x x',
       min y y',
       max (x + w) (x' + fst c#size),
       max (y + h) (y' + snd c#size))
     in
     List.fold_left f (0,0,0,0) children
     |> (fun (x,y,w,h) -> (w - x, h - y))
     in
  *)
  object
    inherit ['a] w size name Cursor.Arrow
    val children = children

    method unload = List.iter (fun (_,c) -> c#unload) children

    method display canvas geom =
      let f ((x,y),c) =
        c#display canvas Draw.{geom with x = geom.x + x;
                                               y = geom.y + y;
                                               w = min geom.w (fst c#size);
                                               h = min geom.h (snd c#size);
                                    }
      in
      let blits = List.concat_map f children in
      blits
  end
