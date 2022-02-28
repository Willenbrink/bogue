(* an empty widget. Does not draw anything, but can be used to get mouse focus *)
open Base

class ['a] t ?id size =
  object (self)
    inherit ['a] w ?id size "Empty" Cursor.Arrow

    method unload = ()

    method triggers = []

    method handle _ _ = failwith "Empty#handle called"

    method display _ _ _ = []
  end
