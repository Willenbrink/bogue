(** a transform variable of type ('a,'b) is a variable of type 'b attached to a
    variable of type 'a Var.t by a bi-directional transformation *)
(* there is no caching *)

type ('a, 'b) t =
  { mutable var : 'a;
    t_from : 'a -> 'b;
    t_to : 'b -> 'a
  }

(* Setting/getting the a value should be done directly via the variable [var] *)

(* Get the b value *)
let get v =
  v.var
  |> v.t_from

(* Set the b value *)
let set v value =
  v.var <- v.t_to value

let create var ~t_from ~t_to =
  { var;
    t_from;
    t_to
  }
