open Utils

type 'a t = 'a ref

let create data = ref data

let release v = ()


(* Execute an action on the given variable if it is not locked by *another*
   thread. Can be used in recursions. When the action wants to access v, it
   should use the unsafe_ versions (although this is not necessary, only
   faster). *)
let protect_fn v action =
  let result = action () in
  result

(* usually we don't need to protect when getting the value. But warning, if the
   value itself is a reference, then one should explicitely protect it *)

(* TODO in fact one should. See
   https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem *)
let get v = !v

let unsafe_get = get

let set v value = v := value

let unsafe_set = set

let protect v = ()

let incr v = v := !v + 1

let decr v = v := !v - 1

(*******)
(* for initialization of global constant by a lazy eval *)
(* TODO: use Lazy module ? *)

exception Not_initialized

type 'a init = {
  mutable init : unit -> 'a; (* the function which creates the value *)
  var : ('a option) t
}

let init init =
  { init; (* ou Var ? *)
    var = create None
  }

let create_init () =
  init (fun () -> raise Not_initialized)

let set_init i f =
  i.init <- f;
  set i.var None

let init_get i = match get i.var with
  | None -> let data = i.init () in set i.var (Some data); data
  | Some d -> d

(*
ocamlmktop -thread -custom -o threadtop unix.cma threads.cma -cclib -lthreads
*)


(*
   Local Variables:
   tuareg-interactive-program:"ocaml unix.cma"
   typerex-interactive-program:"./threadtop -I +threads"
   compile-command:"make -k"
   End:
*)
