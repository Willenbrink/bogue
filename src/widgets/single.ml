(* Contains a single widget. Useful to implement logic without overwriting the
   #execute of other widgets. Also offers a basic loop functionality that
   restarts the contained widget whenever it returns. *)
open Base

class ['a] t ?id execute child =
  object (self)
    inherit ['a] w ?id child#size ("Single " ^ child#name) Cursor.Arrow

    method unload = child#unload

    method execute = execute self child

    method display = child#display
  end

let printer child pp = new t (fun self child -> child#execute |> pp; self#execute) child
let loop ?id child = new t ?id (fun self child -> child#execute |> ignore; self#execute) child
