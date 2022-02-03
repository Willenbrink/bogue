open Base

class ['a] t ?(sep = Theme.room_margin) ?(name = "Col") children =
  object (self)
    inherit ['a] Row.t ~flip:true ~sep ~name children
  end
