open Str
open Base

let split_line line =
  full_split (regexp " ") line
  |> List.map (function
      | Text w -> w
      | Delim _ -> " ")

(* convert a full text including '\n' into paragraphs *)
let paragraphs_of_string text =
  split (regexp "\n") text
  |> List.map split_line

let unsplit_words words =
  words
  |> String.concat ""

let unsplit pars =
  List.map unsplit_words pars
  |> String.concat "\n"

class ['a] t paragraphs =
  object (self)
    inherit ['a] w "TextDisplay" Cursor.Arrow

    val mutable _paragraphs =
      paragraphs

    method paragraphs = _paragraphs

    method text = unsplit self#paragraphs

    method set_text x =
      let x = paragraphs_of_string x in
      _paragraphs <- x

    method min_size = (10 * String.length self#text, 20)

    method execute await _ =
      await#forever

    method render geom =
      let open Draw in
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.blank in
      Raylib.image_draw_text (Raylib.addr img) (unsplit paragraphs) 0 0 14 Raylib.Color.black;
      let tex = Raylib.load_texture_from_image img in
      Raylib.unload_image img;
      [geom, tex]

  end
