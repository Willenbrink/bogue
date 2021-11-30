open Tsdl_ttf
open Tsdl_ttf.Ttf

type font = Ttf.font

let render_utf8_blended = render_utf8_blended
let get_font_style = get_font_style
let set_font_style = set_font_style
let was_init = was_init
let init = init
let size_utf8 = size_utf8
let font_line_skip = font_line_skip

module Style = Style

(* module Hash = struct
 *   type t = (string * int) * font
 *   let equal ((a,b),_) ((c,d),_) = (a = c) && (b = d)
 *   let hash ((a,b),_) = String.length a + b
 * end
 *
 * module WHash = Weak.Make(Hash)
 *
 * let font_cache : WHash.t = WHash.create 10 *)
let font_cache : ((string * int), font) Hashtbl.t = Hashtbl.create 10

let rec open_font file size =
  match Hashtbl.find_opt font_cache (file,size) with
  | Some x -> x
  | None ->
    match Ttf.open_font file size with
    | Result.Ok font -> font
    | Result.Error _ -> raise Not_found
(* match default with
 * | None -> raise Not_found
 * | Some file -> open_font file size *)
