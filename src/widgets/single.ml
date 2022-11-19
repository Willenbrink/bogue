(* Contains a single widget. Useful to implement logic without overwriting the
   #execute of other widgets. Also offers a basic loop functionality that
   restarts the contained widget whenever it returns. *)
open Base

class ['a] t execute child =
  object (self)
    inherit ['a] w ("Single: " ^ child#name) Cursor.Arrow

    method min_size = child#min_size

    method execute = execute self child

    method render = child#render
  end

let printer child pp = new t (fun self child -> child#execute |> pp; self#execute) child
let loop child = new t (fun self child -> child#execute |> ignore; self#execute) child
