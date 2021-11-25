(** a simple image display *)

open Base
open Utils

type resize =  (* not implemented *)
  | Crop of int (* cut the image at origin x *)
  | Fit (* fit in given area *)
  | KeepRatio (* keep aspect ratio and fit inside area *)
  | Expand (* expand if too small. Do not shrink *)
  | Shrink (* shrink if too big. Do not expand *)
  | Size of int (* make it this size *)

(* use noscale = true to keep original pixel size. *)
(* TODO: noscale=true is not completely accurate because this leads to
 * performing scale (size / scale), thus we loose some units due to integer
   rounding. To be exact, we should keep a flag "original size" and modify the
   blit to use exact size *)
class t ?w ?h ?(noscale = false)
    ?(bg = Draw.(opaque black)) file =
  let size = match w, h with
    | Some w, Some h -> (w,h)
    | _ -> begin let (w0,h0) = Draw.image_size file in
        Printf.printf "Image size is: %i %i" w0 h0;
        match w, h with
        | None, Some h -> (w0 * h) / h0, h
        | Some w, None -> w, (h0 * w) / w0
        | _ -> w0, h0
      end in
  let size = if noscale
    then Draw.unscale_size size
    else size in
  object (self)
    inherit w size "Image" Cursor.Arrow

    val file = Var.create file
    val background = bg (* idem *)
    val render : (Draw.texture option) Var.t = Var.create None
    method render = Var.get render

    method! unload =
      match Var.get render with
      | None -> ()
      | Some tex -> begin
          Draw.forget_texture tex;
          Var.set render None
        end

    (* FIXME somehow, this is flipped*)
    method display canvas layer g =
      let open Draw in
      let tex = match Var.get render with
        | Some t -> t
        | None ->
          let file = Theme.get_path (Var.get file) in
          (* printd debug_io "Image: Loading image file %s" file; *)
          (* let surf = sdl_image_load file in *)
          (* let box = create_surface ~like:surf ~color:img.background g.w g.h in *)
          (* let sw,sh = Sdl.get_surface_size surf in *)
          (* let bw, bh = match img.xsize, img.ysize with *)
          (*   | Fit, Fit -> g.w, g.h *)
          (*   | KeepRatio, KeepRatio -> let ratio = float sh /. float sw in *)
          (*     if ratio *. (float g.w) <= float g.h then (g.w, round (float g.w *. ratio)) *)
          (*     else (round (float g.h /. ratio), g.h) *)
          (*   | _ -> failwith "resizing not implemented" in *)
          (* let x = align img.xpos 0 g.w bw in *)
          (* let y = align img.ypos 0 g.h bh in *)
          (* let r1 = Sdl.get_clip_rect surf in *)
          (* let r2 = Sdl.Rect.create ~x ~y ~w:bw ~h:bh in *)
          (* go (Sdl.blit_scaled ~src:surf r1 ~dst:box (Some r2)); *)
          (* let tex = create_texture_from_surface canvas.renderer box in *)
          (* Var.set img.render (Some tex); *)
          (* tex *)
          let tex = Draw.load_image canvas.renderer file in
          Var.set render (Some tex);
          tex
          (* TODO render on background *)

      (* it is better to render first the image at full resolution and then
         scale it, in case we later use some zoom animation. If one has a zoom
         from 0 to 1, then the first time the image will be rendered, the
         required size would be zero. So we have to be careful not to render at
         this size... *)
      in
      let dst = geom_to_rect g in
      [make_blit ~dst ~voffset:g.voffset canvas layer tex]
  end

(* NOTE once we have a more recent version (>= 2.0.2) of SDL_image, we should be
   able to directly load SVG. HOWEVER, it currently it doesn't scale the image,
   so it's not recommended. *)
let create_from_svg ?w ?h ?(bg = Draw.(opaque black)) file =
  let svg = Draw.convert_svg ?w ?h file in
  let w,h = Draw.unscale_size (Draw.image_size svg) in
  new t ~w ~h ~bg svg


(************* display ***********)

