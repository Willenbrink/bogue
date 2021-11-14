(* an empty widget. Does not draw anything, but can be used to get mouse focus *)
open Base

class t size =
  object (self)
    inherit w size "Empty" Cursor.Arrow

    method display _ _ _ = []
  end
