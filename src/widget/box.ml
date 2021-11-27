(* A Box.t is a passive widget that contains a rectangular texture, which can be
   specified by a color or a Style.background --- which means it can contain an
   Image.t. *)
(* The rectangle can have rounded corners, and a border. *)
(* A Box.t can be used directly as a background for layouts that support
   Layout.background *)

(* These various background types are a bit confusing. Maybe one should unify
   them. *)

open Utils

open Base

let default_size = (256,64)
let default_bg = Style.Solid Draw.(opaque pale_grey)

class t ?(size = default_size) ?(bg = default_bg) ?border ?shadow () =
  object (self)
    inherit w size "Box" Cursor.Arrow

    val border : Style.border option = border
    val shadow : Style.shadow option = shadow
    val mutable render : Draw.texture option = None

    val mutable bg : Style.background = bg
    method set_bg x =
      self#unload;
      bg <- x (* TODO This can't be right? We are also deleting our texture *)

    method! resize x =
      self#unload;
      _size <- x

    method! unload =
      begin
        match render with
        | None -> ()
        | Some tex ->
          Draw.forget_texture tex;
          render <- None
      end;
      match bg with
      | Style.Image img -> img#unload
      | _ -> ()

    (* As all widget display functions, the geometry g must be already scaled. *)
    method display canvas layer geom=
      let open Draw in
      (* TODO: make sure hoffset <= h *)
      let tex = match render with
        | Some t -> t
        | None ->
          let target = create_target canvas.renderer geom.w geom.h in
          let save_target = push_target canvas.renderer target in

          (* draw background *)
          begin match bg with
            | Style.Image img ->
              printd debug_graphics "Create pattern background";
              let pattern = match img#render with
                | Some tex -> tex
                | None -> begin
                    let (w,h) = img#size in
                    ignore (img#display canvas layer
                              (make_geom ~w ~h ()));
                    match img#render with
                    | Some tex -> tex
                    | None -> failwith "Image should have been rendered before"
                  end in
              fill_pattern canvas.renderer (Some target) pattern

            | Style.Solid color ->
              set_color canvas.renderer color;
              go (Sdl.render_clear canvas.renderer);
              (* B_border.essai canvas.renderer; *)
              (* essai corner_gradient2 *)
              (* corner_gradient2 canvas.renderer (opaque black) (set_alpha 0 black);
              *)

            | Style.Gradient { Style.colors; angle } ->
              gradientv3 canvas.renderer ~angle colors;

              (* ESSAI circle *)
              (* print_endline "CIRCLE";
               * let c = transp black in
               * (\*disc canvas.renderer c (g.w/2) (g.h/2) (g.h/2-5);*\)
               * (\*annulus_octants canvas.renderer ~octants:(1+2) c (g.w/2) (g.h/2) 20 (g.h/2-5);*\)
               * rounded_box canvas.renderer c
               *   ~w:(g.w/2) ~h:(g.h/2) ~thick:10 ~radius:25 (g.w/2) (g.h/2);
               * let c = opaque cyan in
               * circle canvas.renderer ~width:4 c (g.w/4) (g.h/4) (g.h/2); *)
              (* FIN ESSAI *)

          end;
          pop_target canvas.renderer save_target;

          (* need to clip in case of rounded corners *)
          (* TODO unite with do_option b.border *)
          let tex = match border with
            | Some ({ Style.radius = Some radius; _ } as b) ->
              let thick = imax 0 ((Theme.scale_int b.Style.down.Style.width) -1) in
              (* avec ou sans le "-1" sont acceptables. "avec" crée un petit liseré
                 entre les deux couleurs transparentes. "sans" laisse un peu trop de
                 "transparent" aux coins. Si on évite les bordures transparentes (ce
                 qui est à conseiller), "avec" est mieux. *)
              (* we have a choice here. If both the border and the background have
                 alpha components, do we draw the border on top of the background
                 (blending the 2 alphas) (thick=0 or 1), or do we draw them
                 non-intersecting (thick = width or width -1, very difficult to be
                 exact), so that on a white page, they both appear the way the user
                 probably wanted to...? In inkscape they have chosen half-way: the
                 background extends to _half_ the width of the border (thick =
                 width/2)... In our case it's even more difficult because we may
                 have an image instead of a plain background color, so we have to
                 clip it rounded... (with "mask_texture" below) *)
              let radius = max 0 (Theme.scale_int radius - thick) in
              (* TODO treat case line width < 0 *)
              let shape = create_target canvas.renderer geom.w geom.h in
              let bg = set_alpha 0 cyan in
              let save_target = push_target ~clear:true ~bg canvas.renderer shape in
              go (Sdl.set_render_draw_blend_mode canvas.renderer Sdl.Blend.mode_none);
              filled_rounded_box ~antialias:true canvas.renderer (opaque green)
                (* any opaque color will do *)
                ~w:(geom.w-2*thick) ~h:(geom.h-2*thick) ~radius (thick) (thick);
              (* TODO check if this works when Solid background has alpha channel
                 and thick = 0 ... *)
              pop_target canvas.renderer save_target;
              let t = mask_texture ~mask:shape canvas.renderer target in
              forget_texture target;
              forget_texture shape;
              t
            | _ -> target
          in


          (* draw border *)
          (* => TODO use Draw.rectangle (but for now only works if line width is
             constant) . For the moment we use the style of the bottom
             border. *)
          (* TODO The texture tex has been alpha-masked but the color still remains
             hidden... thus if we blend the border onto the texture, because of the
             blending formula, the hidden color might show up again, see example
             1h. But setting mode_none is not good either because there will be some
             white in the inner side of the border... The best way would be to ask
             "rounded" to use "blend" inside and "none" outside... TODO *)
          (* TODO? use the new https://wiki.libsdl.org/SDL_ComposeCustomBlendMode*)
          do_option border (fun brd ->
              let save_target = push_target ~clear:false canvas.renderer tex in
              go (Sdl.set_render_draw_blend_mode canvas.renderer Sdl.Blend.mode_blend);
              let open Style in
              begin
                match brd.radius with
                | None -> begin
                    box canvas.renderer ~bg:brd.up.color 0 0 geom.w
                      (Theme.scale_int brd.up.width);
                    let dw = Theme.scale_int brd.down.width in
                    box canvas.renderer ~bg:brd.down.color 0 (geom.h-dw) geom.w dw;
                    box canvas.renderer ~bg:brd.left.color 0 0
                      (Theme.scale_int brd.up.width) geom.h;
                    let rw = Theme.scale_int brd.right.width in
                    box canvas.renderer ~bg:brd.right.color (geom.w-rw) 0 rw geom.h;
                  end
                | Some radius ->
                  let radius = Theme.scale_int radius in
                  let thick = Theme.scale_int brd.down.width in
                  rounded_box canvas.renderer brd.down.color
                    ~w:geom.w ~h:geom.h ~radius ~thick 0 0
              end;
              pop_target canvas.renderer save_target
            );

          render <- Some tex;
          tex

      in
      (* essai shadow *)
      let dst = geom_to_rect geom in
      let shadow_blits = match shadow with
        | None -> []
        | Some s ->
          if default s.Style.radius 0 > s.Style.width then (
            printd (debug_graphics + debug_warning)
              "Shadow with rounded corner not implemented yet.";
            [] (* TODO *)
          ) else (
            box_shadow ~voffset:geom.voffset canvas layer ~color:black
              ~radius:(Theme.scale_int s.Style.width)
              ~size:(Theme.scale_int s.Style.size)
              ~offset:(Draw.scale_pos s.Style.offset) dst
          ) in

      List.rev ((make_blit ~voffset:geom.voffset ~dst canvas layer tex)::shadow_blits)

  end
