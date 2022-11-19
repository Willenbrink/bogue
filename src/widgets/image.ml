(** a simple image display *)

open Base

type resize =  (* not implemented *)
  | Crop of int (* cut the image at origin x *)
  | Fit (* fit in given area *)
  | KeepRatio (* keep aspect ratio and fit inside area *)
  | Expand (* expand if too small. Do not shrink *)
  | Shrink (* shrink if too big. Do not expand *)
  | Size of int (* make it this size *)

class ['a] t ?(noscale = false) file =
  object (self)
    inherit ['a] w "Image" Cursor.Arrow

    val mutable min_size = (0,0)

    method min_size = min_size

    method execute await _ =
      await#forever

    method render geom =
      let tex = Raylib.load_texture file in
      let w = Raylib.Texture2D.width tex in
      let h = Raylib.Texture2D.height tex in
      min_size <- (w,h);

      [geom, tex]
  end
