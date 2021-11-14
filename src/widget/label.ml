(** a simple text display in one line *)
open Utils
open Tsdl_ttf
open Base

type font =
  | File of string
  | Font of Ttf.font

type style = Tsdl_ttf.Ttf.Style.t

(* open font with specified size. Here this is the true size, it will not be
         scaled. *)
(* This can be used by all widgets requiring a font. *)
let get_font_var v size =
  match Var.get v with
  | Font f -> f
  | File file -> let f = Draw.open_font file size in
    Var.set v (Font f); f

let physical_size_text font text =
  (* Attention, SDL_ttf n'est peut-être pas encore initialisé... *)
  go (Ttf.size_utf8 font text)

class t ?(font_size = Theme.label_font_size) ?(font = File Theme.label_font)
    ?(style = Ttf.Style.normal) ?fg text =
  let typ = "Label [" ^ Utils.xterm_red ^ text ^ Utils.xterm_nc ^ "]" in
  let size = (0,font_size) (* TODO missing calculation *) in
  object (self)
    inherit w size typ Cursor.Arrow
    initializer Draw.ttf_init ()

    val _text = Var.create text
    val render : (Draw.texture option) Var.t = Var.create None
    val font = Var.create font
    val style = style
    val font_size = font_size
    val fg = Var.create fg

    method! unload =
      do_option (Var.get render) Draw.forget_texture;
      Var.set render None

    method text = Var.get _text
    method set_text x =
      if Var.get _text <> x
      then begin
        Var.set _text x;
        do_option (Var.get render) Draw.forget_texture;
        Var.set render None
      end

    method set_fg_color x = Var.set fg (Some x)

    method display canvas layer geom =


      let ttffont = get_font_var font (Theme.scale_int font_size) in
      (* physical size *)


      let render_text_surf ?fg font style text =
        let text = if text = "" then " " else text in
        printd debug_graphics "render_text:%s" text;
        let color = Draw.create_color (default fg (10,11,12,255)) in
        Draw.ttf_set_font_style font style;
        Draw.ttf_render font text color
      in


      let render_text renderer ?fg font style text =
        let surf = render_text_surf ?fg font style text in
        printd debug_graphics "convert to texture";
        let tex = Draw.create_texture_from_surface renderer surf in
        Draw.free_surface surf;
        tex
      in

      let tex = match Var.get render with
        | Some t -> t
        | None ->
          let fg = default (Var.get fg) Draw.(opaque label_color) in
          let tex = render_text canvas.Draw.renderer ttffont style text ~fg in
          Var.set render (Some tex); tex
      in
      [Draw.center_tex_to_layer canvas layer tex geom]

  end

(*

(************* display ***********)

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

(* Resizing has no effect, since the size of the widget is entirely dictated by
   the font size and possibly the geometry of the housing layout. *)
let resize _size _l =
  ()

  *)
