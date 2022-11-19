open Base

class virtual ['a] t ?(name = "Collection") children =
  let children = List.map (fun (g,c) -> (g,(c :> 'b w))) children in
  object
    inherit ['a] w name Cursor.Arrow
    val children = children

    method render geom =
      let f ((x,y),c) =
        (* FIXME w/h is not correctly handled *)
          c#display Draw.{geom with x = geom.x + x;
                                    y = geom.y + y;
                         }
      in
      List.concat_map f children
  end
