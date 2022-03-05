(* Contains a single widget. Useful to implement logic without overwriting the
   #execute of other widgets. Also offers a basic loop functionality that
   restarts the contained widget whenever it returns. *)
open Base

class ['a] t ?id child =
  object
    inherit ['a] w ?id child#size "Single" Cursor.Arrow

    method unload = child#unload

    method execute =
      child#execute

    method display = child#display
  end

class ['a] loop child =
  object (self)
    inherit ['a] w child#size ("Single " ^ child#name) Cursor.Arrow

    method unload = child#unload

    method execute =
      child#execute
      |> ignore;
      self#execute

    method display = child#display
  end
