open Base

class ['a] t ?(sep = Theme.room_margin)
    ?(align) ?name (children : 'a Base.w list) =
  object
    inherit ['a] Row.t ~flip:true ~sep ?align ?name children
  end
