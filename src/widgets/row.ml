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

class ['a] discontinuation k =
  object
    method discontinue exn : 'a = EffectHandlers.Deep.discontinue k exn
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

    method execute await yield =
      let module Await = Await (struct type t = bottom end) in
      let exec_child (o, (c : _ w)) =
        match c#execute Await.await Await.yield with
        | _ -> .
        | [%effect? Await.Await triggers, k]  ->
          match triggers with
          | [] -> new discontinuation k
            (* TODO with separate wait and yield a bottom widget might listen, just not yield. *)
          | _ :: _ -> failwith "bottom widget listens for events"
      in
      let _ = List.map exec_child children in
      await#forever

    method display geom =
      let f (x,c) =
        c#display Draw.{geom with x = geom.x + (if flip then 0 else x);
                                               y = geom.y + (if flip then x else 0);
                                               w = min geom.w (fst c#size);
                                               h = min geom.h (snd c#size);
                                    }
      in
      let blits = List.concat_map f children in
      blits
  end

class ['l,'r,'res] pair ?(flip = false) ?(sep = Theme.room_margin)
    ?(align) ?(name = if flip then "VPair" else "HPair")
    ?(logic = fun self await yield -> fun res -> ()) (left : 'left #w) (right : 'right #w)
  =
  let left = (left :> 'left w) in
  let right = (right :> 'right w) in
  let offset = (if flip then snd else fst) left#size in
  let size =
    let lx,ly = left#size in
    let rx,ry = right#size in
    if flip
    then (max lx rx, ly + ry)
    else (lx + rx, max ly ry)
  in
  object (self)
    inherit ['res] w size name Cursor.Arrow

    method private ev_targets_lr (ts_l, ts_r) ((ev : Event.t_rich), (g : Draw.geometry)) =
      match ev with
      | Scroll _ -> failwith "Scroll not implemented" (* TODO *)
      (* Events that can affect both childs *)
      | Mouse_enter | Mouse_leave
      | Key_press _ | Key_repeat _
      | Key_release _ | Codepoint _ ->
        List.mem (Event.strip ev) ts_l,
        List.mem (Event.strip ev) ts_r
      (* Events than can affect only the child the mouse is above *)
      | Mouse_motion pos
      | Mouse_press (pos,_)
      | Mouse_release (pos,_) ->
        let x_m,y_m = pos in
        (* Disabled because of Mouse_leave TODO document! *)
        (* assert (g.x <= x_m && x_m <= g.x + g.w); *)
        (* assert (g.y <= y_m && y_m <= g.y + g.h); *)
        let x_m, y_m = x_m - g.x, y_m - g.y in
        (* Printf.printf "%s, geom: %ix,%iy %iw,%ih %io\n" name g.x g.y g.w g.h offset; *)
        (* Printf.printf "%s, mouse: %i,%i\n" name x_m y_m; *)

        match (if flip then y_m else x_m) < offset with
        | true ->
          List.mem (Event.strip ev) ts_l, false
        | false ->
          false, List.mem (Event.strip ev) ts_r

    method execute await yield =
      (fun (type a b)
        (logic : (a,b) Either.t -> unit)
        (left : a w) (right : b w) ->
        (* TODO use await directly *)
        let ts_l, cc_l =
          let module A = Await (struct type t = a end) in
          match left#execute A.await A.yield with
          | _ -> .
          | [%effect? A.Await triggers, k] ->
            triggers, new continuation k
          | [%effect? A.Yield res, k] ->
            logic (Either.left res);
            EffectHandlers.Deep.continue k ()
        in
        let ts_r, cc_r =
          let module A = Await (struct type t = b end) in
          match right#execute A.await A.yield with
          | _ -> .
          | [%effect? A.Await triggers, k] ->
            triggers, new continuation k
          | [%effect? A.Yield res, k] ->
            logic (Either.right res);
            EffectHandlers.Deep.continue k ()
        in

        let rec loop ts_l ts_r cc_l cc_r =
          await#f (ts_l @ ts_r)
          begin
            fun (ev,geom) ->
            let l,r = self#ev_targets_lr (ts_l, ts_r) (ev,geom) in
            let tss_l, tss_r = [%show: Event.t list] ts_l, [%show: Event.t list] ts_r in
            (* Printf.printf "Received event:\n\tl: %b #ev %s\n\tr: %b #ev %s\n%!" l tss_l r tss_r; *)
            let ts_l, cc_l =
              if l
              then cc_l#continue ev Draw.{geom with
                                         w = min geom.w (fst right#size);
                                         h = min geom.h (snd right#size)}
              else ts_l, cc_l
            in
            let ts_r, cc_r =
              if r
              then cc_r#continue ev Draw.{geom with
                                         x = geom.x + (if flip then 0 else offset);
                                         y = geom.y + (if flip then offset else 0);
                                         w = min geom.w (fst right#size);
                                         h = min geom.h (snd right#size)}
              else ts_r, cc_r
            in
            loop ts_l ts_r cc_l cc_r
          end
        in
        loop ts_l ts_r cc_l cc_r
      ) (logic self await yield) left right

    method display geom =
      let blitr_l =
        left#display Draw.{geom with
                                        w = min geom.w (fst left#size);
                                        h = min geom.h (snd left#size)}
      in
      let blitr_r =
        right#display Draw.{geom with
                                         x = geom.x + (if flip then 0 else offset);
                                         y = geom.y + (if flip then offset else 0);
                                         w = min geom.w (fst right#size);
                                         h = min geom.h (snd right#size)}
      in
      blitr_l @ blitr_r
  end

class ['a] pair_id ?(flip = false) ?(sep = Theme.room_margin)
    ?(align) ?(name = if flip then "VPair" else "HPair")
    ?(logic = fun self await yield res -> yield res) (left : 'a #w) (right : 'a #w)
  =
  let logic self await yield res =
      match res with
      | Either.Left res
      | Either.Right res -> logic self await yield res
        in
  let left = (left :> 'a w) in
  let right = (right :> 'a w) in
  object (self)
    inherit ['a,'a,'a] pair ~flip ~sep ~align ~name ~logic left right
  end

let x : string w = new pair_id (new Text_input.t "Test") (new Empty.t (0,0))
