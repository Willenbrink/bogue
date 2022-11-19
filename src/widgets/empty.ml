(* an empty widget. Does not draw anything, but can be used to get mouse focus *)
open Base

class ['a] t =
  object (self)
    inherit ['a] w "Empty" Cursor.Arrow

    method min_size = (0,0)

    method execute await _ =
      await#forever

    method render _ = []
  end
