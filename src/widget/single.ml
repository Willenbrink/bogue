(* Contains a single widget. Useful to implement logic without overwriting the
   #execute of other widgets. Also offers a basic loop functionality that
   restarts the contained widget whenever it returns. *)
open Base

class ['a] t ?id child execute =
  object (self)
    inherit ['a] w ?id child#size ("Single " ^ child#name) Cursor.Arrow

    method unload = child#unload

    method execute = execute self

    method display = child#display
  end

let printer child pp = new t child (fun self -> child#execute |> pp; self#execute)
let loop ?id child = new t ?id child (fun self -> child#execute |> ignore; self#execute)
