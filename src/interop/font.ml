module State = Eff.State (struct type t = Raylib.Font.t * float end)

type fonts =
  | Default
  | Ubuntu
  | Font_awesome
  | Custom of string

let font_files = [
Ubuntu, "share/themes/common/fonts/Ubuntu-R.ttf";
Font_awesome, "share/themes/common/font-awesome-4.6.3/fonts/fontawesome-webfont.ttf";
]


let fonts = Hashtbl.create 1

let find f =
  match Hashtbl.find_opt fonts f with
  | Some f -> f
  | None ->
    Printf.printf "Font not found. Falling back to the default.\n";
    Hashtbl.find fonts Default

let default () = find Default, 14.

let init () =
  let default = Raylib.get_font_default () in
  Hashtbl.add fonts Default default;

  List.iter (fun (name,file) ->
      Raylib.load_font file
      |> Hashtbl.add fonts name
    ) font_files;
  ()

let set_default () =
  State.set (find Ubuntu, 14.)

let draw_text img ?(tint = Raylib.Color.black) ?(spacing = 0.) text x y =
  let pos = Raylib.Vector2.create x y in

  let font, size = State.get () in

  Raylib.image_draw_text_ex img font text pos size spacing tint
