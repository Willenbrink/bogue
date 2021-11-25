(** group layouts into tabs *)
open Utils

(* warning: not thread safe ? we modify the dest_room *)
module W = Widget

let bg_on = Style.gradient Draw.[opaque Button.color_off; opaque Button.color_off; opaque Button.color_on]
(* Style.gradient Draw.[opaque white;opaque pale_grey;opaque pale_grey;opaque pale_grey];; *)
let bg_off = Style.Solid Draw.(opaque Button.color_off)
(* Style.gradient Draw.[opaque pale_grey;opaque grey;opaque grey;opaque grey];; *)

(* On attache tous les rooms dans le layout (en mode "superposition"), et on met
   en show=true celui qu'on veut. ça permet de les nettoyer correctement tous à
   l'extinction.  Cependant, ce n'est pas optimisé au cas où il y a des millions
   de tabs (peu probable ...) car la boucle de display va les passer tous en
   revue pour trouver celui à montrer. Au départ on ne mettait dans le layout
   que le tab actif; l'inconvénient était que tous les autres n'étaient pas
   "visibles" par bogue, et donc n'étaient pas nettoyés en quittant. D'autre
   part, ça forçait à "modifier" le contenu du layout à chaque changement de tab
   actif, ce qui n'est pas très joli. *)
let create_one ?slide title room dest_room =
  (* TODO create_empty adds to WHash table *)
  let b = new Button.t ~bg_on ~bg_off title in
  (* the first action sets the button to 'pressed' when we click (button_down)
     on it. Below we will add another action to reset all other buttons to 'not
     pressed' *)
  let onpress _ =
    (* TODO skip this if the tab is already selected *)
    if not b#state
    then begin
      b#press;
      Layout.iter_rooms (fun l -> Layout.set_show l false) dest_room;
      Layout.set_show room true;

      do_option slide (fun from -> Layout.slide_in ~from ~dst:dest_room room);
      b#update (* or refresh only layout ? *)
    end
  in
  let c = W.connect_main b b onpress Trigger.buttons_down in
  b#add_connection c;
  b

(** create tabs from a assoc list ("title"; layout) *)
(* TODO, return a function that can be called to activate tab #i *)
let create (*?(circular = true)*) ?slide ?(adjust = Layout.Fit) ?(expand = true)
    ?canvas ?(name="tabs") list =
  if list = [] then failwith "Cannot create empty tabs"
  else begin
    let _, first_room = List.hd list in
    let all_rooms = List.map snd list in
    List.iter (fun l -> Layout.set_show l false) all_rooms;
    Layout.set_show first_room true;
    let dest_room = (* Layout.flat ~sep:0 ~adjust ?canvas [first_room] in *)
      Layout.superpose ?canvas all_rooms in
    (* begin (\* we fix initial geometry *\)
     *   let open Layout in
     *   let maxw, maxh =
     *     List.fold_left (fun m (_,r) -> max m (getx r + width r)) 0 list,
     *     List.fold_left (fun m (_,r) -> max m (gety r + height r)) 0 list in
     *   match adjust with
     *   | Nothing -> (\* we fix the size to the maximum of all tabs *\)
     *      set_width dest_room maxw;
     *      set_height dest_room maxh
     *   | Width ->
     *      set_height dest_room maxh
     *   | Height ->
     *      set_width dest_room maxw
     *   | Fit -> ()
     * end; *)
    let labels = List.map (fun (title, layout) ->
        create_one ?slide title layout dest_room) list in
    let reset_other_labels w _ =
      List.iter (fun b ->
          if not (W.equal w b) then b#release) labels;
      (* + refresh ? *) in
    List.iter (fun l ->
        let c =  W.connect_main l l (reset_other_labels l) Trigger.buttons_down in
        l#add_connection c) labels;
    (* we activate the first label (TODO: choose which one) *)
    let first_l = List.hd labels in
    first_l#press;

    let menu = Layout.flat_of_w ?canvas ~sep:0 (List.map (fun x -> (x :> Widget.t)) labels) in
    if expand (* we set the same width to all labels... do better? *)
    then begin
      let w = Layout.width dest_room in (* TODO utiliser relayout *)
      Layout.set_width menu w;
      List.iter (fun r -> Layout.set_width r (w/3)) (Layout.get_rooms menu)
      (* TODO ajuster le dernier w - n(w/3)... *)
    end;
    Layout.reflat ~margins:0 menu;
    (* TODO since reflat uses an animation, it's not so easy to insert a
       correct resize function. *)

    (* on ajoute une ligne ?? *)
    (* let hline = Layout.empty ~w:(Layout.width menu) ~h:1 ~background:(Layout.color_bg Draw.(opaque dark_grey)) () in *)
    Layout.tower ~name ~sep:0 ~adjust ?canvas [menu; (*hline;*) dest_room]
  end


(* TODO mutually recursive modules in separate files
   http://www.davehking.com/2011/05/23/mutually-recursive-modules-in-ocaml-and-why-you-might-care.html
*)
