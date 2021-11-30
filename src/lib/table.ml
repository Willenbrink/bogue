(** Table layout. *)

(* il faut pouvoir changer la largeur des colonnes. Donc garder accès aux
   layouts... ou au moins les recréer *)
(* TODO add line number/label *)
(* TODO it raises exception if length = 0, but we should make it work (?) *)

open Printf
open Utils
module W = Widget

(* this is the public, non-mutable type *)
type 'a column =
  { title : string;
    length : int;
    rows : int -> 'a Layout.t;
    compare : (int -> int -> int) option;
    (* use "compare i1 i2" in order to compare entries i1 and i2 *)
    width : int option;
  }

type sort =
  | Ascending (* increasing *)
  | Descending (* decreasing *)

type 'a column_private = {
  title : string;
  rows : int -> 'a Layout.t;
  compare : (int -> int -> int) option;
  mutable width : int;
  mutable sort : sort option
}

type 'a t = {
  length : int;
  data : 'a column_private array;
  mutable selection : Selection.t; (* selection of rows *)
  mutable last_selected : int option;
  order : int array; (* we keep here the bijection ith entry --> jth displayed *)
  titles : 'a Layout.t array;
  row_height : int;
  layout : 'a Layout.t option ref(* the global layout *)
}

let title_margin = 5;;
let title_background = Layout.Solid Draw.(set_alpha 40 blue);;
let row_hl = Layout.Solid Draw.(set_alpha 30 blue);;
let row_selected = Layout.Solid Draw.(opaque (pale (pale (pale blue))));;
let icon_color = Draw.(set_alpha 40 grey);;

(* max_width returns the max width of the first n_max entries of the column c *)
let max_width ?(n_max = 50) (c : 'a column) =
  let n_max = imin n_max c.length in
  let rec loop i m =
    if i = n_max then m
    else let w = Layout.width (c.rows i) in
      loop (i+1) (imax m w) in
  loop 0 0

let make_title (c : 'a column) =
  (* we compute the label widget *)
  let label = new Label.t c.title in
  (* if no width is specified, we compute the max width of the first entries of
     the column *)
  let lw,_ = label#size in
  let w = match c.width with
    | Some w -> w
    | None -> (imax lw (max_width c)) in
  let layout =
    if c.compare = None
    then (* first encapsulate in order to then left-align *)
      Layout.flat_of_w ~sep:0 [(label :> 'a W.t)]
    else begin (* add icon for sorting *)
      let sort_indicator = new Icon.t ~fg:icon_color "sort" in
      let sw,_ = sort_indicator#size in
      let lw,lh = label#size in
      let w' = w - sw - lw in
      if w' >= 0
      then (* we can add sort_indicator *)
        Layout.flat_of_w ~sep:0 ~align:(Draw.Max) [ (label :> 'a W.t);
                                                    (new Empty.t (w',lh) :> 'a W.t);
                                                    (sort_indicator :> 'a W.t)]
      else Layout.flat_of_w ~sep:0 [ (label :> 'a W.t)]
    end in
  Layout.set_width layout w; (* not necessary in the case of sort_indicator *)
  let (_,h) = label#size in
  let click_area = new Empty.t (w,h) in
  let title = Layout.(superpose [resident ( click_area); layout]) in
  title;;

(* extracts the click_area widget from the title layout *)
(* Warning: this depends on the way title is created in make_title *)
let get_area title =
  let open Layout in
  match title#content with
  | List [area; _] -> widget area
  | _ -> failwith "table.ml: The title layout should contain [area;layout]"

(* extracts the sort_indicator widget from the title layout *)
(* Warning: this depends on the way title is created in make_title *)
let get_indicator title =
  let open Layout in
  match title#content with
  | List [_; obj; _ ]
    -> begin match obj#content with
          List [_;_; sort_indicator] -> Some (widget sort_indicator)
        | _ -> None end
  | _ -> None

let get_row _ _ (* t i *) =
  () (* ??? *)

let make_column (c: 'a column) w : 'a column_private =
  { title = c.title;
    rows = c.rows;
    compare = c.compare;
    width = w;
    sort = None
  };;

let make_columns (columns : 'a column list) widths =
  List.map2 make_column columns widths;;

let make_table ?row_height (columns : 'a column list) =
  let length, rw = match columns with
    | [] -> failwith "Cannot create empty table"
    | c0::_ -> List.iter (fun (c : 'a column) ->
        if c.length <> c0.length
        then failwith "Table columns must have same length")
        columns; c0.length, Layout.height (c0.rows 0) in
  let row_height = default row_height rw in
  let titles = List.map make_title columns in
  let widths = List.map Layout.width titles in
  let data = make_columns columns widths
             |> Array.of_list in
  {
    length;
    data;
    selection = Selection.empty;
    last_selected = None;
    order = Array.init length (fun i -> i);
    titles = Array.of_list titles; (* useful ? we have the layout below *)
    row_height;
    layout = ref None (* will be computed afterwards *)
  }

(* entry number i in the original array and in position ii in the display *)
let get_background t i ii =
  if Selection.mem t.selection i
  then Some row_selected
  else
  if ii mod 2 = 1
  then Some (Layout.Solid Draw.(set_alpha 20 grey))
  else None;;

let make_long_list ~w ~h t  =
  (* generate row #i: *)
  let generate = fun ii ->
    let i = t.order.(ii) in
    let background = get_background t i ii in
    let left_margin = new Empty.t (title_margin,t.row_height) in
    let click_area = new Empty.t (w,t.row_height) in
    let ca = Layout.resident ~name:(sprintf "click_area %u(%u)" i ii) ( click_area) in
    let row =
      Array.mapi (fun j c ->
          let width = Layout.width t.titles.(j) in
          let name = sprintf "entry[%u,%u]" i j in
          let r = Layout.flat ~sep:0 ~hmargin:0 ~vmargin:0 ~name [c.rows i] in
          Layout.set_width r (width  + title_margin); r) t.data
      |> Array.to_list
      |> cons (Layout.resident ( left_margin))
      |> (Layout.flat ~sep:0 ~hmargin:0 ~vmargin:0 ?background) in
    let enter _ = (Layout.set_background ca (Some row_hl)
    (* Layout.fade_in ca ~duration:150 *)) in
    let leave _ = Layout.set_background ca None
    (* Layout.fade_out ca ~duration:150 *) in
    (* TODO: PROBLEM if one adds Layout.fade_in/out animations here, it becomes
       very slow when one tries to scroll at the same time ==> cf
       "check_mouse_motion board" dans bogue.ml *)
    Widget.mouse_over ~enter ~leave click_area;
    (* TODO click is not good with touchscreen *)
    let click _ =
      (t.last_selected <- Some i;
       let new_sel =
         (* if Trigger.ctrl_pressed () *)
         (* then Selection.toggle t.selection i *)
         (* else if Trigger.shift_pressed () *)
         (* then (match t.last_selected with *)
         (*       | Some i0 -> *)
         (*          Selection.(union t.selection [Range (min i i0, max i i0)]) *)
         (*       | None -> Selection.[Range (i,i)]) *)
         (* else Selection.[Range (i,i)] in *)
         (* TODO: At this point this (standard) selection mechanism
                     with CRTL and SHIFT does not work because we need to
                     recompute the how long list to update all backgrounds. This
                     will have to be added afterwards. For the moment we only
                     toggle: *)
         Selection.toggle t.selection i in
       t.selection <- new_sel;
       Layout.set_background row (get_background t i ii);
      ) in
    Widget.on_click ~click click_area;
    Layout.(superpose [ca; row])
  in
  let height_fn _ = Some t.row_height in
  Long_list.create ~w ~h ~generate ~height_fn ~length:t.length ();;

let make_layout ?w ~h t =
  let align = Draw.Max in (* bottom align *)
  let titles_list = Array.to_list t.titles in
  let w = default w (title_margin +
                     (List.fold_left (fun y r -> y + title_margin + Layout.width r)
                        0 titles_list)) in
  let titles_row = Layout.flat ~sep:title_margin ~hmargin:title_margin
      ~vmargin:title_margin ~background:title_background ~align titles_list in
  let long = make_long_list t ~w ~h:(h - Layout.height titles_row) in
  titles_row, long;;

(* in-place reverse bijection of array *)
let reverse_array a =
  let l = Array.length a - 1 in
  for i = 0 to l/2 do
    let x = Array.unsafe_get a i in
    Array.unsafe_set a i (Array.unsafe_get a (l-i));
    Array.unsafe_set a (l-i) x
  done;;

(* sets the sort-indicator symbol. Does nothing if column is not sortable *)
let set_indicator t j =
  if t.data.(j).compare = None then ()
  else begin
    let sort = t.data.(j).sort in
    do_option (get_indicator t.titles.(j)) (fun indicator ->
        (* let label = Widget.get_label indicator in
         * Label.set label (match sort with
         *                  | None -> Theme.fa_symbol "sort"
         *                  (\* terminology in font_a is reversed *\)
         *                  | Some Ascending -> Theme.fa_symbol "sort-desc"
         *                  | Some Descending -> Theme.fa_symbol "sort-asc") *)
        ())
  end;;


(* refreshes the table by creating a new long_list *)
let refresh t =
  match !(t.layout) with
  | None -> failwith "table.ml: field t.layout should not be None"
  (* TODO don't crash here and provide a default ? But this should never
       happen *)
  | Some r ->
    let w,h,g,titles_row =
      let open Layout in
      match r#content with
      | List [titles_row; long_old] ->
        width titles_row, height long_old,
        long_old#geometry, titles_row
      | _ -> failwith "table.ml: layout content is corrupted"
      (* TODO don't crash ? *)
    in
    let long = make_long_list ~w ~h t in

    (* this is the dangerous part: *)
    Layout.(long#set_geometry g);
    Layout.(long#set_current_geom @@ to_current_geom g);
    (* = not really necessary, because I have removed do_adjust in set_rooms *)
    Layout.set_rooms r [titles_row; long]

(* changes sorting order. We don't try to modify the long_list in-place, we
   create a new one *)
let change_order t j sort =
  let column = t.data.(j) in
  do_option column.compare (fun compare ->
      Array.stable_sort compare t.order;
      if sort = Descending then reverse_array t.order;
      (* TODO modify t.titles.(j) *)
      refresh t;
      column.sort <- Some sort;
      for i = 0 to Array.length t.titles - 1 do
        if i <> j then t.data.(i).sort <- None;
        set_indicator t i
      done
    );;

let connect_title t j =
  if t.data.(j).compare = None then ()
  else begin
    (* FIXME Segfault incoming *)
    let widget = get_area t.titles.(j) |> Obj.magic in
    let click _ =
      let sort = match t.data.(j).sort with
        | None -> Ascending
        | Some Ascending -> Descending
        | Some Descending -> Ascending in
      change_order t j sort in
    Widget.on_click ~click widget;
    let enter _ =
      let title = t.titles.(j) in
      Layout.set_background title (Some title_background) in
    let leave _ =
      let title = t.titles.(j) in
      Layout.set_background title None in
    Widget.mouse_over ~enter ~leave widget;
  end;;


(* we just share the selection variable via a Tvar to automatically update the
   layout when the selection is changed. *)
let make_selection_tvar t =
  let t_from sel = sel in (* the user can access the selection via this *)
  let t_to sel = (* this is what is done when the user will modifiy the
                    selection using Tvar.set *)
    t.selection <- sel; (* this is redundant with Tvar.set, but we need it
                                to be done *before* refresh... *)
    refresh t;
    sel in
  Tvar.create t.selection ~t_from ~t_to


(* this returns the main layout and the selection variable *)
let create ?w ~h ?row_height ?(name="table") (columns : 'a column list) =
  let t = make_table columns ?row_height in
  let titles_row, long = make_layout ?w ~h t in
  let layout = Layout.tower ~sep:0 ~hmargin:0 ~vmargin:0 ~name
      [titles_row; long] in
  t.layout := (Some layout);
  for j = 0 to List.length columns - 1 do
    connect_title t j
  done;
  layout, make_selection_tvar t;;

(* Create table from text array a.(i).(j) : row i, column j *)
let of_array ?w ~h ?widths ?row_height ?name headers a =
  let head = Array.of_list headers in
  let ni = Array.length a in
  if ni = 0 then failwith "Cannot create table with empty array."
  else let nj = Array.length a.(0) in
    if nj <> Array.length head
    then failwith "Cannot create table: \
                   headers size does not fit the number of columns."
    else let widths = match widths with
        | None -> List.map (fun _ -> None) headers
        | Some list -> list in
      let widths = Array.of_list widths in
      if Array.length widths <> nj
      then failwith "Cannot create table: \
                     list of widths does not fit the number of columns."
      else let columns =
             head
             |> Array.mapi (fun j title ->
                 { title;
                   length = ni;
                   rows = (fun i -> Layout.resident (new Label.t a.(i).(j) :> 'a W.t));
                   compare = Some (fun i1 i2 -> compare a.(i1).(j) a.(i2).(j));
                   width = widths.(j) })
             |> Array.to_list in
        create ?w ~h ?row_height ?name columns;;

(* From a Csv.t style list of rows (first row must be the header). Warning: this
   functions first converts to an array, ie. the data is likely to be duplicated
   in memory *)
let of_list ?w ~h ?widths ?row_height ?name = function
  | [] -> failwith "Cannot create table with empty list."
  | headers::rows ->
    let a = List.map Array.of_list rows
            |> Array.of_list in
    of_array ?w ~h ?widths ?row_height ?name headers a

(* * * * *)
