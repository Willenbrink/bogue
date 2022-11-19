(* each widget contains its personal data, and the list of connections from
   it *)

include Base

class type virtual ['a] t = ['a] w

let (|||) left right =
  let pair = new Row.pair left right in
  pair#execute
