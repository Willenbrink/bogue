(** a simple text display in one line *)
open Base
open Utils

type style = Ttf.Style.t

let physical_size_text font text =
  go (Ttf.size_utf8 font text)

class ['a] t ?(font_size = Theme.label_font_size) ?(font = Theme.label_font)
    ?(style = Ttf.Style.normal) ?(fg = Draw.(opaque label_color)) text =
  let typ = "Label [" ^ Utils.xterm_red ^ text ^ Utils.xterm_nc ^ "]" in
  (* Previous implementation:
   * let default_size (type a) (w : a tc') =
   *   match w#kind with
   *   | Label l -> let x,y = Label.size l in (x+2,y+2)*)
  let () = Draw.ttf_init () in
  let size = physical_size_text (Draw.get_font font font_size) text (* TODO missing calculation *) in
  object (self)
    inherit ['a] w size typ Cursor.Arrow
    initializer Draw.ttf_init ()

    val mutable text = text
    method text = text
    method set_text x =
      text <- x

    method execute await _ =
      await#forever

    val mutable fg = fg
    method set_fg_color x = fg <- x

    method display geom =
      let img = Raylib.gen_image_color geom.w geom.h Raylib.Color.blank in
      Raylib.image_draw_text (Raylib.addr img) text 0 0 14 Raylib.Color.black;

      let tex = Raylib.load_texture_from_image img in
      Raylib.unload_image img;
      [geom, tex]

  end

(*

(* let default_size = (128,32);;*)
(* "/home/san/public_html/7h09/sites/all/themes/drupal_7h09/css/museo.ttf";; *)

let physical_size l =
  match Var.get l.render with
    | Some tex -> Draw.tex_size tex
    | None -> physical_size_text (font l) (text l)

(* a first order approximation of the "logical" size is obtained by dividing by
   the scale; this is not ideal because the final physical scale of the layout
   will be calculated by multiplying this by the scale, resulting in a +/- 1
   pixel error. The size can be larger than the geometry, see
   [center_tex_to_layer]. *)
let size l =
  physical_size l |> Draw.unscale_size
  ()

  *)
