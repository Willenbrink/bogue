(* TODO turn this into a normal variant? Using polymorphic variants is a major hassle
   because it seems that any interesting feature requires a lot of type shenanigans.
   It seems we would need excessive GADTs, polymorphic annotations etc. Would enable us to:
   - Encode possible rich events in the await type. Major problem: type escaping scope
   - Encode bottom: Empty polymorphic variants are not typable
*)
type t = [
  (* TODO currently there is no way to write an empty polymorphic variant, see
     https://github.com/ocaml/ocaml/issues/10687
  *)
  (* | `Bottom *)
  | `Key_press
  | `Key_repeat
  | `Key_release
  | `Codepoint
  | `Mouse_motion
  | `Mouse_enter
  | `Mouse_leave
  | `Mouse_press
  | `Mouse_release
  | `Scroll
]
[@@deriving show]

(* TODO we need some form of global listening. Press slider -> Move mouse off -> Release *)
(* These events can normally only be listened to by one widget *)
let keyboard_events : t list =
  [`Key_press; `Key_repeat; `Key_release; `Codepoint]

type button =
  (* Left/Right/Middle Mouse Button *)
  | LMB
  | RMB
  | MMB
  | Other of int
[@@deriving show]

type modifier = GLFW.key_mod =
  | Shift
  | Control
  | Alt
  | Super
[@@deriving show]

type key = GLFW.key

let pp_key fmt _ = Format.pp_print_string fmt "<opaque>"

(* TODO currently there is no way to write an empty polymorphic variant, see
   https://github.com/ocaml/ocaml/issues/10687
*)

  (* TODO use a UTF8 library of some sort instead of using string? *)
type t_rich =
  | Key_press : key * modifier list -> t_rich
  | Key_repeat : key * modifier list ->  t_rich
  | Key_release : key * modifier list ->  t_rich
  | Codepoint : string -> t_rich
  | Mouse_motion : (int * int) -> t_rich
  | Mouse_enter : t_rich
  | Mouse_leave : t_rich
  | Mouse_press : (int * int) * button -> t_rich
  | Mouse_release : (int * int) * button -> t_rich
  | Scroll : (float * float) -> t_rich
[@@deriving show]

let strip (ev : t_rich) : t = match ev with
  | Key_press _ -> `Key_press
  | Key_repeat _ -> `Key_repeat
  | Key_release _ -> `Key_release
  | Codepoint _ -> `Codepoint
  | Mouse_motion _ -> `Mouse_motion
  | Mouse_enter -> `Mouse_enter
  | Mouse_leave -> `Mouse_leave
  | Mouse_press _ -> `Mouse_press
  | Mouse_release _ -> `Mouse_release
  | Scroll _ -> `Scroll

type t_win = [
  | `Init
  | `Exit
  | `Resize of int * int
]

let cursor_pos = ref (0,0)

let inputs : (t_rich, t_win) Either.t Queue.t = Queue.create ()

let wait () =
  GLFW.waitEvents ();
  let res = Queue.fold (fun acc x -> x::acc) [] inputs in
  Queue.clear inputs;
  res

let init () =
  let open GLFW in
  let window = Window.window () in
  Queue.push (Either.right `Init) inputs;
  setWindowSizeCallback ~window ~f:(Some (fun _ w h ->
      Queue.push (Either.right (`Resize (w,h))) inputs
    ))
  |> ignore;
  setWindowCloseCallback ~window ~f:(Some (fun _ ->
      Queue.push (Either.right `Exit) inputs
    ))
  |> ignore;
  let f ev = Queue.push (Either.left ev) inputs in
  setKeyCallback ~window ~f:(Some (fun _ key int action mods ->
      let ev =
        match action with
        | Press -> Key_press (key, mods)
        | Release -> Key_release (key, mods)
        | Repeat -> Key_repeat (key, mods)
      in
      f ev
    ))
  |> ignore;
  setCharCallback ~window ~f:(Some (fun _ char ->
      let (*the unicdoe odyssey begin*) uchar = Uchar.of_int char in
      let buffer = Buffer.create 1 in
      Buffer.add_utf_8_uchar buffer uchar;
      Codepoint (Buffer.contents buffer)
      |> f))
  |> ignore;
  setCursorPosCallback ~window ~f:(Some (fun _ float1 float2 ->
      let pos = (int_of_float float1, int_of_float float2) in
      cursor_pos := pos;
      Mouse_motion pos
      |> f
    ))
  |> ignore;
  setCursorEnterCallback ~window ~f:(Some (fun _ bool ->
      f (if bool then Mouse_enter else Mouse_leave)))
  |> ignore;
  setMouseButtonCallback ~window ~f:(Some (fun _ button action mods->
      let button =
        if button = 0 then LMB
        else if button = 1 then RMB
        else if button = 2 then MMB
        else Other button
      in
      f (if action
         then Mouse_press (!cursor_pos, button)
         else Mouse_release (!cursor_pos, button))
    ))
  |> ignore;
  setScrollCallback ~window ~f:(Some (fun _ x y -> Scroll (x, y) |> f))
  |> ignore
