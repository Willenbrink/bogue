open Base

(* TODO Unify row and col? Pay attention to performance! ´
   We must quickly map a point to the correct child for event handling *)

(* TODO
   Handle alignment
*)

(* We use objects to model the recursive nature of continue *)
class ['a] continuation k =
  object
    method continue (ev : Event.t_rich) (geom : Draw.geometry)
      : 'a * 'a continuation
      =
      EffectHandlers.Deep.continue k (ev,geom)
      (* TODO *)
      (* method discontinue exn = EffectHandlers.Deep.discontinue k exn *)
  end

class t ?(flip = false) ?(sep = Theme.room_margin)
    ?(align) ?(name = if flip then "Col" else "Row") (children : bottom w list)
  =
  let children' =
    let f (x,cs) c =
      ((x + (if flip then snd else fst) c#size + sep), (x, (c :> 'b w))::cs)
    in
    let res = List.fold_left f (0,[]) children |> snd in
    assert (List.length res > 0);
    res
  in
  let size =
    let f (x,y) c =
      let (x_c,y_c) = c#size in
      if flip
      then (max x x_c, y+y_c)
      else (x+x_c, max y y_c)
    in
    List.fold_left f (0,0) children
  in
  let children = children' in
  object
    inherit [bottom] w size name Cursor.Arrow

    method execute await =
      let module Await = Await (struct type t = bottom end) in
      let exec_child (o, (c : _ w)) =
        match c#execute Await.await with
        | _ -> .
        | [%effect? Await.Await (triggers, res), k]  ->
          match triggers, res with
          | _, Some _ -> .
          | [], None -> (), new continuation k
          | _ :: _, _ -> failwith "bottom widget listens for events"
      in
      let _ = List.map exec_child children in
      (* TODO find a way to encode nontermination here *)
      await#f [] None @@ function
      | _ -> raise Repeat

    method unload = List.iter (fun (_,c) -> c#unload) children

    method display canvas layer geom =
      let f (x,c) =
        c#display canvas layer Draw.{geom with x = geom.x + (if flip then 0 else x);
                                               y = geom.y + (if flip then x else 0);
                                               w = min geom.w (fst c#size);
                                               h = min geom.h (snd c#size);
                                    }
      in
      let blits = List.concat_map f children in
      blits
  end

class ['a] pair ?(flip = false) ?(sep = Theme.room_margin)
    ?(align) ?(name = if flip then "VPair" else "HPair") (left : 'a w) (right : 'a w)
  =
  let offset = (if flip then snd else fst) left#size in
  let size =
    let lx,ly = left#size in
    let rx,ry = right#size in
    if flip
    then (max lx rx, ly + ry)
    else (lx + rx, max ly ry)
  in
  object (self)
    inherit ['a] w size name Cursor.Arrow

    val mutable keyboard_focus = None

    method ev_targets_lr (ts_l, ts_r) ((ev : Event.t_rich), (g : Draw.geometry)) =
      match ev with
      | `Key_press _ | `Key_repeat _
      | `Key_release _ | `Codepoint _ ->
        List.mem (Event.strip ev) ts_l,
        List.mem (Event.strip ev) ts_r
      | `Scroll -> assert false (* TODO *)
      | `Mouse_motion pos | `Mouse_enter pos
      | `Mouse_leave pos | `Mouse_press (pos,_)
      | `Mouse_release (pos,_) ->
        let x_m,y_m = pos in
        let base = if flip then g.y else g.x in
        (* Printf.printf "%s, geom: %ix,%iy %iw,%ih\n" name g.x g.y g.w g.h; *)
        (* Printf.printf "%s, mouse: %i,%i\n" name x_m y_m; *)
        assert (g.x <= x_m && x_m <= g.x + g.w);
        assert (g.y <= y_m && y_m <= g.y + g.h);
        let x_m, y_m = x_m - g.x, y_m - g.y in

        let ev_l =
          let offset' = base in
          (if flip then y_m else x_m) >= offset'
          && (true || (if flip then x_m else y_m) < offset)
          && List.mem (Event.strip ev) ts_l
        in
        let ev_r =
          let offset' = base + offset in
          (if flip then y_m else x_m) >= offset'
          && (true || (if flip then x_m else y_m) < offset)
          && List.mem (Event.strip ev) ts_r
        in
        assert (not (ev_l && ev_r));
        ev_l, ev_r

    method execute await = (fun (type a) (await : <f: 'b. (a,'b) await>) (left : a w) (right : a w) ->
        let module Await = Await (struct type t = a end) in
        let extract opt1 opt2 = match opt1, opt2 with
          | None, None -> None
          | Some res, None | None, Some res -> Some res
          | _, _ -> failwith "Badly defined row. Both childs returned Some _ values"
        in

        (* TODO use await directly *)
        let (res_l, ts_l), cc_l =
          match left#execute Await.await with
          | _ -> .
          | [%effect? Await.Await (triggers, res), k] ->
            (res, triggers), new continuation k
        in
        let (res_r, ts_r), cc_r =
          match right#execute Await.await with
          | _ -> .
          | [%effect? Await.Await (triggers, res), k] ->
            (res, triggers), new continuation k
        in

        let rec loop ts_l ts_r res_l res_r cc_l cc_r =
          await#f (ts_l @ ts_r) (extract res_l res_r) @@ fun (ev,g) ->
          let l,r = self#ev_targets_lr (ts_l, ts_r) (ev,g) in
          let (res_l, ts_l), cc_l =
            if l
            then cc_l#continue ev g
            else ((None, ts_l), cc_l)
          in
          let (res_r, ts_r), cc_r =
            if r
            then cc_r#continue ev g
            else ((None, ts_r), cc_r)
          in
          loop ts_l ts_r res_l res_r cc_l cc_r
        in
        loop ts_l ts_r res_l res_r cc_l cc_r
      ) await left right

    method unload = left#unload; right#unload

    method display canvas layer geom =
      let blit_l =
        left#display canvas layer Draw.{geom with
                                        w = min geom.w (fst left#size);
                                        h = min geom.h (snd left#size)}
      in
      let blit_r =
        right#display canvas layer Draw.{geom with
                                         x = geom.x + (if flip then 0 else offset);
                                         y = geom.y + (if flip then offset else 0);
                                         w = min geom.w (fst right#size);
                                         h = min geom.h (snd right#size)}
      in
      blit_l @ blit_r
  end
