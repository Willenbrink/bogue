(* an empty widget. Does not draw anything, but can be used to get mouse focus *)
open Base

class ['a] t size =
  object (self)
    inherit ['a] w size "Empty" Cursor.Arrow

    method unload = ()

    method execute await _ =
      await#forever

    method display _ = []
  end
