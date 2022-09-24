(** One-line text-editor widget *)

(* bug bizarre: après execution de l'exemple 12 (et lorsqu'on joue un peu avec),
   emacs freeze complètement (obligé de tuer) dès qu'on sélectionne une
   région *)

(* cf:
   https://wiki.libsdl.org/Tutorials/TextInput#CandidateList
*)

open Base
open Utils

type selection =
  | Point of int
  (* Area (x,y) is the selection starting at pos x and ending at pos y
     x is therefore the first value temporally, but not necessarily smaller than y*)
  | Area of (int * int)

(* we cannot use Sdl.color type here if we want to memoize, since colors are
       typically recreated by Sdl.Color.create... *)
let render_key font key color =
  let color = Draw.create_color color in
  let surf = Draw.ttf_render font key color in
  incr Draw.ttf_surfaces_in_memory;
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
  (* If we use blend, the semitransparent pixels will acquire the color of the
         surface on which we blit (usually black, even if its alpha=0), which is not
         good because that's not the final blitting. *)
  go (Sdl.set_surface_rle surf true);
  (* "If RLE is enabled, color key and alpha blending blits are much faster, but
         the surface must be locked before directly accessing the pixels." (SDL
         doc) *)
  surf

let text_dims font text =
  if text = "" then (print_endline "ERROR: text empty !"; 0,0) (* OK ? or use 1,1 ?? *)
  else let w,h = (* if !memo *)
         (* (\* if !memo, this is (maybe ?) faster to get surface_size than calling *)
         (*    TTF.size_utf8. BUT this will save another surface (with color 0,0,0,0) i *)
         (*    the table... *\) *)
         (*   then let surf = render_key font text (0,0,0,0) in *)
         (*     (\* : no need to free in case of memo *\) *)
         (*     Sdl.get_surface_size surf *)
         (*   else *) Label.physical_size_text font text
    in
    printd debug_graphics "Size of '%s' = (%d,%d)." text w h;
    w,h

(* we use all-purpose memo to memoize the kerning values. One could do
       something more optimized, of course. *)
let text_dims = memo2 text_dims

let text_width font s =
  let w,_ = text_dims font s in w

(* TODO max_size is not respected *)
class t ?(max_size = 2048) ?(prompt = "Enter text")
    ?(font_size = Theme.text_font_size)
    ?(font = Theme.text_font) text =
  let size = (0,font_size) (* TODO missing calculation *) in
  object (self)
    inherit [string] w size "TextInput" Cursor.Ibeam

    val mutable keys = Utf8.split text

    initializer Draw.ttf_init ()
    val mutable cursor = None
    val cursor_char = Theme.fa_symbol "tint"
    val mutable render = None
    val mutable offset = 0
    method font = Draw.get_font font (Theme.scale_int font_size)

    val mutable selection = Point 0
    val mutable active = false

    method stop =
      printd debug_event "Stopping text input";
      if Sdl.is_text_input_active ()
      then Sdl.stop_text_input ();
      active <- false

    val mutable room_x = 0

    method unload =
      (* Option.iter Draw.forget_texture render; *)
      (* render <- None; *)
      ()

    method select x =
      let x = self#px_to_ind x in
      let sel = match selection with
        | Point a -> Area (a,x)
        | Area (a,_) -> Area (a,x)
      in
      selection <- sel

    method start_input =
      Sdl.start_text_input ();
      active <- true

    method stop_input =
      Sdl.stop_text_input ();
      active <- false

    method execute await yield =
      let pressed (x,_) =
        selection <- Point (self#px_to_ind x);
        await#f [`Mouse_release; `Mouse_motion] @@ function
        | Mouse_release (_, Event.LMB), _ ->
          ()
        | Mouse_motion (x,_), _ ->
          self#select x;
          raise Repeat
        | _ ->
          raise Repeat
      in

      (* Unfocused *)
      begin
        await#f [`Mouse_press; `Key_press] @@ function
        | Mouse_press (pos, Event.LMB), _ -> pressed pos
        | Key_press (keycode, []), _ when keycode = Sdl.K.tab ->
          selection <- Area (0, List.length keys)
        | _ -> raise Repeat
      end;

      (* Focused *)
      self#start_input;
      begin
        await#f [`Codepoint; `Key_press; `Mouse_press] @@ function
        | Mouse_press (pos, Event.LMB), _ ->
          pressed pos;
          raise Repeat
        | Key_press (keycode, []), _ when keycode = Sdl.K.tab || keycode = Sdl.K.return ->
          ()
        | Codepoint c, _ ->
          self#insert c;
          raise Repeat
        (* TODO this line does not work when we use one k to represent the type.
           Seems like a compiler limitation? *)
        | Key_press (k,mods), _ ->
          self#handle_key (k,mods);
          raise Repeat
        | _ -> raise Repeat
      end;
      self#stop_input;
      selection <- Point self#cursor_pos;
      yield self#state;
      self#execute await yield

    method state = String.concat "" keys

    method set_text x = keys <- Utf8.split x

    (************* display ***********)

    (* In the "display" function, the input text is drawn on the "surf" surface.
       Then surf is copied onto a larger surface, "box", to accommodate for
       underline. Then, box is clipped to its visible part (due to scrolling if the
       text exceed the size of the widget) into the "visible" surface.  The visible
       surface gives the final widget texture.  Nothing can be drawn outside the
       "visible" surface.  box, and visible have same height. The cursor is a
       separate texture (it's difficult to pre-blend everything due to SDL current
       limitations on blend modes. Maybe soon we will have
       https://wiki.libsdl.org/SDL_ComposeCustomBlendMode). *)

(*
                                                     ^
                                                     | bottom_margin (to center
<---------left_margin------------>                   v text vertically)
   (put in the blit geom)         |---visible---------------------------|
               --------------box--|-------------------------------------|-----
               |       -----------|-----surf-------------------------   |    |
               |       |          |                                 |        |
               |<----->|BLA BLA                                     |<------>|
               |cursor ---------------------------------------------- cursor |
               |width/2 _______  <--- underline      ^                width  |
               |         ^       <--- cursor         | bottom_margin         |
               |                                     v                       |
               -------------------|-------------------------------------|-----

The "cursor_xpos" is computed wrt the origin of the surface "surf"

*)


    val mutable memo = true (* use more memory (memoization) for speeding up display *)
    val default_size = (128,32)
    val left_margin = 2 (* in logical pixels *)
    val bottom_margin = 5 (* used for underline *)
    (* TODO replace bottom_margin by cursor height (to compute) *)
    (* let cursor_width = 10
     * let cursor_height = 9 *)
    (* let cursor_thickness = 2 *)

    (* memoize. Warning: do NOT free the resulting surfaces !! *)
    (* NOTE: it seems that this memoing does not really improve speed, but at least
       it does not degrade speed... *)
    (* Warning: the arguments should not be mutable, otherwise memo is likely to
       fail (equality problem). For instance, do not use Sdl.Color type instead of
       (r,g,b,a) *)

    method get_render_key =
      if memo then let f,table = memo3 render_key in
        let cleanup () =
          printd debug_graphics "Cleaning up %u SDL_TTF surfaces..."
            (Hashtbl.length table);
          Hashtbl.iter (fun _ surf ->
              Draw.free_surface surf;
              decr Draw.ttf_surfaces_in_memory) table;
          Hashtbl.clear table in
        Draw.at_cleanup cleanup;
        f
      else render_key

    (** return size of rendered text. It seems that Sdl.TTF.size_utf8 does not always
        give the exact same result as size of blended-rendered surface. Warning:
        thus, should use this only on single letters ! *)
    (* from http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf_frame.html : *)
    (* Kerning is the process of spacing adjacent characters apart depending on the
       actual two adjacent characters. This allows some characters to be closer to
       each other than others. When kerning is not used, such as when using the
       glyph metrics advance value, the characters will be spaced out at a constant
       size that accomodates all pairs of adjacent characters. This would be the
       maximum space between characters needed. There's currently no method to
       retrieve the kerning for a pair of characters from SDL_ttf, However correct
       kerning will be applied when a string of text is rendered instead of
       individual glyphs. *)
    (* initial size of the widget *)
    (* not scaled, in order to conform to all widgets size functions *)
    method! size = (* TODO *)
      let w,h =
        match render with
        | Some tex -> Draw.tex_size tex
        | None -> text_dims (self#font) (prompt) in
      let w,h = Draw.unscale_size (w,h) in
      (w + 2*left_margin (* this should probably be left_margin + cursor_width/2 *),
       h + 2*bottom_margin)
    (* The bottom margin is also added at the top, in order to keep the text
       vertically centered. *)

    method cursor_pos =
      match selection with
      | Point x -> x
      | Area (_,x) -> x

    method cursor_pos_px ?n () =
      let x = match n with
        | Some x -> x
        | None -> self#cursor_pos
      in
      let head, _ = split_list keys x in
      List.fold_left (fun s key -> s + text_width (self#font) key) 0 head

    (** Return cursor_pos corresponding to the x position *)
    method px_to_ind x0 =
      let char_offset = font_size/3 in
      (* TODO, this should be roughly the half size of a char *)
      let x0 = x0 - room_x - (Theme.scale_int (left_margin + char_offset))
               + (offset) in
      let rec loop list cx n =
        match list with
        | [] -> n
        | key::rest ->
          if cx >= x0 then n else
            let advance, _ = text_dims (self#font) key in
            loop rest (cx + advance) (n+1) in
      loop (keys) 0 0


    (* Recall that none of the functions that are called by threads should call
       video functions directly. *)

    (** treat the click event to position the cursor, once the widget is active *)
    (* method click_cursor ev = *)
    (*   printd debug_event "Click cursor"; *)
    (*   let x0u, _ = Mouse.pointer_pos ev in *)
    (*   let x0 = Theme.scale_int x0u in (\* on pourrait éviter de faire unscale-scale *\) *)
    (*   cursor_pos <- self#x_to_cursor x0; *)
    (*   self#clear *)

    method kill_selection =
      match selection with
      | Area (n1,n2) ->
        let n1, n2 = min n1 n2, max n1 n2 in
        let head1, tail1 = split_list keys n1 in
        let _, tail2 = split_list tail1 (n2-n1) in
        keys <- (List.flatten [head1; tail2]);
        selection <- Point n1
      | Point _ -> ()

    method select_all =
      let l = List.length (keys) in
      selection <- Area (0,l)

    (* insert a list of letters *)
    method insert_list list =
      self#kill_selection;
      let x = self#cursor_pos in
      let head, tail = split_list (keys) x in
      selection <- Point (x + (List.length list));
      keys <- List.flatten [head; list; tail];

      (** insert a letter *)
    method insert c = self#insert_list [c]

    (* find a word containg the cursor position *)
    (* method find_word = *)
    (*   let n = cursor_pos in *)
    (*   let daeh, tail = split_list_rev (keys) n in *)
    (*   let rec find_sep ~complement list pos = *)
    (*     match list with *)
    (*     | [] -> pos *)
    (*     | key::rest -> if (not complement && List.mem key seps) || (complement && not (List.mem key seps)) *)
    (*       then pos *)
    (*       else find_sep ~complement rest (pos + 1) in *)
    (*   if tail = [] then ( *)
    (*     printd debug_board "No word found: we are at the end"; *)
    (*     Empty) *)
    (*   else let cursor_key = List.hd tail in *)
    (*     let complement = List.mem cursor_key seps in *)
    (*     (\* if 'complement' is true, then we are on a separator, so we must *)
    (*        find a "word of separators" *\) *)
    (*     let left = find_sep ~complement daeh 0 in *)
    (*     let right = find_sep ~complement tail 0 in *)
    (*     printd debug_board "Word found (%d,%d)" left right; *)
    (*     Active (n-left, n+right) *)

    method! remove_keyboard_focus = self#stop

    method! guess_unset_keyboard_focus = false

    (* render letter by letter so that x position is precise *)
    method draw_keys ?fg font keys =
      let color = if keys = [] then Draw.(transp faint_color) (* inutile ? *)
        else default fg (10,11,12,255) in
      printd debug_graphics "Renders keys";
      let rec loop keys surfs w h =
        match keys with
        | [] -> surfs, w, h
        | key::rest -> let surf = render_key font key color in
          let dw,h = Sdl.get_surface_size surf in
          loop rest ((surf, w) :: surfs) (w+dw) h in
      let keys = if keys = [] then [" "] else keys in
      let surfs, tw, h = loop keys [] 0 0 in
      let surf, _ = List.hd surfs in
      printd debug_graphics "Create total surface";
      let total_surf = Draw.create_surface ~like:surf tw h in
      printd debug_graphics "Blit the letters on the surface";
      let rec draw_loop = function
        | [] -> ()
        | (surf, w) :: rest ->
          let dst_rect = Sdl.Rect.create ~x:w ~y:0 ~w:0 ~h:0 in
          go (Sdl.blit_surface ~src:surf None ~dst:total_surf (Some dst_rect));
          (* no free in case of memo: *)
          if not memo then Draw.free_surface surf;
          draw_loop rest in
      draw_loop surfs;
      total_surf

    (* REMARK: instead of blitting surfaces, one could also use textures and SDL
       RenderTarget ? *)
    method display canvas layer g = (* TODO mettre un lock global ? *)
      Option.iter Draw.forget_texture render;
      render <- None;
      let cursor = match cursor with
        | Some s -> s
        | None ->
          let csize = 2*(Theme.scale_int font_size)/3 in
          let cursor_font = Draw.get_font Theme.fa_font csize in
          let s = self#draw_keys cursor_font [cursor_char] ~fg:Draw.(opaque cursor_color) in
          (* TODO use render_key, it should be faster *)
          let tex = Draw.create_texture_from_surface canvas.Draw.renderer s in
          cursor <- (Some tex);
          Draw.free_surface s;
          tex
      in
      let cw, _ = Draw.tex_size cursor in
      let tex = match render with
        | Some t -> t
        | None ->
          let start_time = Unix.gettimeofday () in (* =for debug only *)
          let keys = keys in
          let fg = if keys <> [] then Draw.(opaque text_color) else
              (* if active then Draw.(opaque pale_grey) else *) Draw.(opaque faint_color) in
          let keys = if keys = [] && not active then [prompt] else keys in
          let surf = self#draw_keys (self#font) keys ~fg in
          (* TODO: draw only the relevent text, not everything. *)
          let tw,th = Sdl.get_surface_size surf in
          (* we need to make a slightly larger surface in order to have room for
             underline and cursor *)
          let box = Draw.create_surface ~like:surf
              (tw + cw + cw/2) (th + Theme.scale_int bottom_margin) in
          go (Sdl.set_surface_blend_mode box Sdl.Blend.mode_none);

          (* draw text on the larger surface, at (0,0) (upper-left corner)
             preserving transparency information *)
          let rect = Draw.rect_translate (Sdl.get_clip_rect surf) (cw/2, 0) in
          go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
          go (Sdl.blit_surface ~src:surf None ~dst:box (Some rect));

          (* draw selection background: this will erase the corresponding text... *)
          (match selection with
           | Area (n1,n2) ->
             let n1, n2 = min n1 n2, max n1 n2 in
             let x1 = self#cursor_pos_px ~n:n1 () in
             let x2 = self#cursor_pos_px ~n:n2 () in
             let sel_rect = Sdl.Rect.create ~x:x1 ~y:0 ~w:(x2-x1) ~h:th in
             let sel_rect_cw = Draw.rect_translate sel_rect (cw/2, 0) in
             Draw.fill_rect box (Some sel_rect_cw) Draw.(opaque sel_bg_color);
             (* now we reblit the text on the selection rectangle, this time with
                blending *)
             let sel = self#draw_keys (self#font) keys ~fg:Draw.(opaque sel_fg_color) in
             (* TODO: draw only the relevent text, not everything. *)
             go (Sdl.set_surface_blend_mode sel Sdl.Blend.mode_blend);
             go (Sdl.blit_surface ~src:sel (Some sel_rect) ~dst:box (Some sel_rect_cw))
           | Point _ -> ());

          Draw.free_surface surf;
          if active then begin
            (* draw underline *)
            let thick = Theme.scale_int 1 in
            let hline = Sdl.Rect.create ~x:(cw/2) ~y:(th (*+ bmargin - thick*)) ~w:tw ~h:thick in
            (* Sdl.fill_rect : If the color value contains an alpha
               component then the destination is simply filled with that
               alpha information, no blending takes place. *)
            Draw.fill_rect box (Some hline) Draw.(transp grey);

            (* move the offset to have the cursor in the visible area *)
            let cx = self#cursor_pos_px () in
            let _offset = offset in
            let _offset = if cx <= _offset+cw then max 0 (cx-cw)
              else if cx - _offset >= g.Draw.w - cw - cw/2
              then min tw (cx - g.Draw.w + cw + cw/2)
              else _offset in
            offset <- _offset
          end;
          (* we extract the visible part and save it as a texture, with all
             transparency info (no blending) *)
          (* note: if we don't clip to the visible part, it is easy to reach the max
             allowed texure width = 4096 *)
          let bw, bh = Sdl.get_surface_size box in
          let _offset = offset in
          let rect_b = Sdl.Rect.create ~x:_offset ~y:0 ~w:(min g.Draw.w (bw - _offset)) ~h:bh in
          let visible = Draw.create_surface ~like:box ~color:Draw.none (Sdl.Rect.w rect_b) bh in
          (* this surface (converted to texture) will be *blended* on the canvas *)
          go (Sdl.blit_surface ~src:box (Some rect_b) ~dst:visible None);
          let tex = Draw.create_texture_from_surface canvas.Draw.renderer visible in
          Draw.free_surface box;
          Draw.free_surface visible;
          render <- (Some tex);
          printd debug_graphics "Time for creating texture = %f s" (Unix.gettimeofday () -.  start_time);
          tex
      in

      (* finally we copy onto the canvas *)
      let open Draw in
      let area = geom_to_rect g in
      Sdl.set_text_input_rect (Some area);
      room_x <- g.x;
      let text_blit = copy_tex_to_layer ~overlay:(Draw.Xoffset 0) ~voffset:g.voffset
          canvas layer tex area (g.x + (Theme.scale_int left_margin))
          (g.y + (Theme.scale_int bottom_margin)) in
      (* we could instead have used a box surface of larger size, including margins,
         and use tex_to_layer instead of copy_tex_to_layer *)

      if active
      then   (* (re...)compute cursor position *)
        (* The cursor is an additional blit. We don't pre-blend the two textures
           (text+cursor) into a single blit, because the SDL current blend modes
           don't allow this...
           http://www.adriancourreges.com/blog/2017/05/09/beware-of-transparent-pixels/
        *)
        let _,bh = tex_size tex in
        let voff = Theme.scale_int 4 in
        let cursor_g = { g with x = g.x + Theme.scale_int left_margin +
                                    self#cursor_pos_px () - offset;
                                y = g.y + bh - voff } in
        [text_blit; tex_to_layer canvas layer cursor cursor_g]
      else [text_blit]

    method shift_check_sel ~shift ~one ~right =
      let new_sel x =
        selection <-
          if shift
          then match selection with
            | Point a | Area (a,_) -> Area (a,x)
          else Point x
      in
      match one, right with
      | false, false -> new_sel 0
      | true, false -> new_sel @@ max 0 (self#cursor_pos - 1)
      | false, true -> new_sel (List.length keys) (* TODO off by one error? *)
      | true, true -> new_sel @@ min (List.length keys) (self#cursor_pos + 1)

    method left = self#shift_check_sel ~one:true ~right:false
    method right = self#shift_check_sel ~one:true ~right:true
    method home = self#shift_check_sel ~one:false ~right:false
    method last = self#shift_check_sel ~one:false ~right:true

    method delete ~right =
      match selection with
      | Area _ ->
        self#kill_selection
      | Point _ ->
        (* extend selection by one, then delete *)
        self#shift_check_sel ~shift:true ~one:true ~right;
        self#kill_selection

    (* copy to clipboard *)
    method copy =
      match selection with
      | Area (n1,n2) ->
        let n1,n2 = min n1 n2, max n1 n2 in
        let _, tail = split_list (keys) n1 in
        let head, _ = split_list tail (n2-n1) in
        go (Sdl.set_clipboard_text (String.concat "" head))
      | _ -> ()

    (* copy and kill *)
    method kill =
      self#copy;
      self#kill_selection

    (* paste from clipboard *)
    method paste =
      if Sdl.has_clipboard_text () then
        let text = go (Sdl.get_clipboard_text ()) in
        let list = Utf8.split text in
        self#insert_list list


    (* treat the text events *)
    (* DOC: *)
    (* SDL_Scancode values are used to represent the physical location of a keyboard
       key on the keyboard. *)
    (* SDL_Keycode values are mapped to the current layout of the keyboard and
       correlate to an SDL_Scancode *)
    method handle_key k =
      match k with
      | c, Event.[Ctrl] -> begin match c with
          | c when c = Sdl.K.a -> self#select_all
          | c when c = Sdl.K.c -> self#copy (* : desactivate this for debugging the emacs problem *)
          | c when c = Sdl.K.x -> self#kill
          | c when c = Sdl.K.v -> self#paste
          | _ -> ()
        end
      | _, [] | _, [Event.Shift] -> begin
          let shift = snd k = [Event.Shift] in
          match k with
          | c,[] when c = Sdl.K.backspace -> self#delete ~right:false
          | c,[] when c = Sdl.K.delete -> self#delete ~right:true
          | c,_ when c = Sdl.K.left -> self#left ~shift
          | c,_ when c = Sdl.K.right -> self#right ~shift
          | c,_ when c = Sdl.K.up -> self#home ~shift
          | c,_ when c = Sdl.K.home -> self#home ~shift
          | c,_ when c = Sdl.K.down -> self#last ~shift
          | c,_ when c = Sdl.K.kend -> self#last ~shift
          | c,_ when c = Sdl.K.return -> self#stop
          | _ -> ()
        end
      | _ -> ()
  end
