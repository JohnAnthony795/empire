open Events
open Events_timed
open Common.Small
open Common
open Gg

type ('a, 'b) wpair =
  { v: 'a ;
    w: 'b Gg.widg }

(* By default, some objects are apparently in 'prelight' mode. We put them in normal mode.
 * Strangely, the state we receive is 'normal'. We re-emit the same signal ?!? *)
let normal_state widg = ignore(widg#misc#connect#state_changed ~callback:(fun st -> if st = `NORMAL then widg#misc#set_state `NORMAL))

let gnum ~digits ~v ?prefix ?postfix ?packing () =
  (* Note: the ~digits argument implies that the spin value (in adjustment) is set, thus it triggers a callback (?!) *)
  let spin = GEdit.spin_button ~adjustment:v#adjustment ~digits ~numeric:true ~snap_to_ticks:false ~wrap:false () in
  (* Disable scrolling *)
  ignore(spin#event#connect#scroll ~callback:(fun _ -> true)) ;
  { v ;
    w = new Gg.txthbox ?prefix ?postfix ?packing spin }

let efloat ?(name="efloat") ?prefix ?postfix ?packing ?cb ?target ?setter ?(digits=2) ~lower ~upper ~step_incr ~page ~init () =
  let adjfloat = new Evt_val.adjfloat ~name ?cb ?target ?setter ~lower ~upper ~step_incr ~page ~init () in
  gnum ~digits ~v:adjfloat ?prefix ?postfix ?packing ()

let eint ?(name="eint") ?prefix ?postfix ?packing ?cb ?target ?setter ~lower ~upper ?(step_incr=1) ?(page=10) ~init () =
  let adjint = new Evt_val.adjint ~name ?cb ?target ?setter ~lower ~upper ~step_incr ~page ~init () in
  gnum ~digits:0 ~v:adjint ?prefix ?postfix ?packing ()

let eslider ?(name="eslider") ~packing ?cb ?target ?setter ?digits ~lower ~upper ?draw_value orientation ~init () =
  let adjfloat = new Evt_val.adjfloat ~name ?cb ?target ?setter ~lower ~upper ~step_incr:0.5 ~page:1.0 ~init () in
  { v = adjfloat ;
    w = GRange.scale orientation ~adjustment:adjfloat#adjustment ?digits ?draw_value ~packing () }

let ebool ?(name="ebool") ?prefix ?label ?postfix ~packing ?cb ?target ?setter styl ~init () =
  let label = Common.option_map label to_utf in

  let widg =
    let mk = match styl with
      | `TOGGLE -> GButton.toggle_button
      | `CHECK -> GButton.check_button
    in
    mk ?label ~active:init ~draw_indicator:true ~packing ()
  in
  { v = new Evt_val.simple_val ~name ?cb ?target ~gtk:widg#connect#toggled ?setter ~fset:widg#set_active ~fget:(fun () -> widg#active) () ;
    w = widg }

let llabel ?time_rate tos ?xalign ?yalign ?justify ~packing prod =
  let lbl = GMisc.label ?xalign ?yalign ?justify ~packing () in
  let producer = match time_rate with
    | None -> prod
    | Some r -> limit (cst_cat r) prod
  in
  (* This stupid label does not refresh its alignment !??! *)
  Events.to_cons producer (fun s -> lbl#set_text (tos s)) ;
  lbl

let lfloat ?time_rate ~prefix ~postfix ?(digits=2) ~packing prod =
  llabel ?time_rate ~packing (fun f -> Printf.sprintf "%s%.*f%s" prefix digits f postfix) prod

let lint ?time_rate ~prefix ~postfix ~packing prod =
  llabel ?time_rate ~packing (fun i -> prefix ^ string_of_int i ^ postfix) prod

type string_evt = [`Value_Changed of string | `Submit ]
type string_evt_val = (string, [`Value_Changed of string | `Submit ]) Evt_val.evt_val

let estring ?(name="estring") ?prefix ?postfix ~packing ?cb ?target ?setter ?width_chars ?max_length ?(has_frame=true) ~style init =
  let entry = GEdit.entry ~text:(Gg.to_utf init) ?width_chars ?max_length ~editable:true ~has_frame () in
  (*  let () = normal_state entry in *)
  if style = `PASSWD then entry#set_visibility false ;

  let v =
    object(self)
      inherit [string, string_evt] Evt_val.evt_val ~name ?cb ?target ?setter ~gtk:entry#connect#changed ()
      method value = entry#text
      method apply_event = function
        | `Submit as e -> self#send e (* Nothing to do except send the event. *)
        | `Value_Changed s -> entry#set_text (Gg.to_utf s) ; self#vchanged

      initializer
        ignore(entry#connect#activate ~callback:(fun _ -> self#apply_event `Submit)) ;
    end
  in
  { v ;
    w = new Gg.txthbox ?prefix ?postfix ~packing entry }

class ['a] combo_list ?(name="combo_list") ?(fail_notfound=true) ?cmp ~packing ?cb ?target ?setter ~all ~v2s ?init () =
  let (combo, (store, _)) as tcombo = GEdit.combo_box_text ~strings:(List.map (Gg.to_utf ++ v2s) all) ~packing () in

  (* Disable scrolling *)
  let _ = combo#event#connect#scroll ~callback:(fun evt -> true) in

  (* Reversed list of all elements *)
  let elements = ref (List.rev all)
  and nelts    = ref (List.length all) in

  let fset = function
    | None -> combo#set_active (-1)
    | Some v ->
      try 
        let index = !nelts - 1 - (list_index_of ?cmp v !elements) in
        combo#set_active index
      with Not_found -> if fail_notfound then failwith ("Ggval.combo_list: elt not in list = " ^ (v2s v)) else combo#set_active (-1)
  in

  let get_nth index = 
    try List.nth !elements (!nelts - index - 1)
    with _ -> assert false
  in

  let fget () = 
    let index = combo#active in
    if index = -1 then None
    else Some (get_nth index)
  in

  object(self)
    inherit ['a option] Evt_val.simple_val ~name ?cb ?target ?setter ~gtk:combo#connect#changed ~fset ~fget ()

    initializer self#set_value init

    method get_all = List.rev !elements

    method combo = combo

    method clear =
      elements := [] ;
      nelts := 0 ;
      store#clear () ;

    method add_element elt =
      elements := elt :: !elements ;
      nelts := !nelts + 1 ;
      GEdit.text_combo_add tcombo (Gg.to_utf (v2s elt)) ;

  end

let combo_list ?name ?fail_notfound ?cmp ~packing ?cb ?target ?setter ~all ~v2s ?init () =
  let v = new combo_list ?name ?fail_notfound ?cmp ~packing ?cb ?target ?setter ~all ~v2s ?init () in
  { v ;
    w = v#combo }


let choice_list ?(name="choice_list") ?(editable=true) ?style ?border_width ?bigpadding ?padding ?cmp ?packing ?cb ?target ?setter ~all ~v2s ~init () =

  let colbox = new Ggpack.cols ?border_width ?bigpadding ?padding ?style ?packing () in
  let packing = colbox#add in
  let sall = Array.map (to_utf ++ v2s) all in

  let chosen = ref 0
  and butfset = ref [] in

  let fset v =
    try let index = array_index_of ?cmp v all in
      List.iter (fun butf -> butf index) !butfset
    with Not_found -> failwith ("Ggval.choice_list: elt not in list = " ^ (v2s v))
  in

  let evtval = new Evt_val.simple_val ~name ?cb ?target ?setter ~fset ~fget:(fun () -> all.(!chosen)) () in

  (* Build radio buttons *)
  let () =
    let add_button ?group index =
      let but = GButton.radio_button ~label:sall.(index) ?group ~packing () in
      let _ = but#connect#toggled (fun () -> if but#active then (chosen := index ; evtval#vchanged)) in
      if not editable then but#misc#set_sensitive false ;
      butfset := (function i -> if i = index then but#set_active true) :: !butfset ;
      but
    in

    let button0 = add_button 0 in 
    for i = 1 to Array.length all - 1 do
      ignore(add_button ~group:button0#group i) ;
    done ;

    evtval#set_value init ;
  in

  { v = evtval ;
    w = colbox }

type 'a result =
  | WellFormed of 'a
  | BadFormed of string

class ['a] val_popup ?parent ?(name="val_popup") ~title ?width ?height ?packing ~val2s ?(apply_button=true) ~editval ?target ?cb ?setter ~init () =

  let but = GButton.button ~label:"" ~relief:`NONE ?packing () in

  object(self)
    inherit ['a] Evt_val.ref_val ~name ?cb ?setter ~init ()

    val mutable opened = false

    initializer
      ignore(but#connect#clicked (fun () -> Lwt.async self#popup)) ;
      to_cons self#vprod (but#set_label ++ to_utf ++ val2s) ;

      (* Initialise the init value. *)
      self#vchanged ;

      option_iter target (to_prod self#vprod) ;
      ()

    method button = but

    method popup () =
      if opened then Lwt.return_unit
      else
        begin

          opened <- true ;

          (* Stops Lwt events. Don't know why exactly. *)
          let win = GWindow.dialog ?parent ~title:(to_utf title) ?width ?height () in
          win#add_button_stock `CANCEL `CANCEL ;
          if apply_button then win#add_button_stock `APPLY `APPLY ;
          win#add_button_stock `OK `OK ;

          let read_val = editval ~vbox:win#vbox self#value in

          let rec loop () =
            match win#run () with
            | `DELETE_EVENT
            | `CANCEL -> win#destroy () ; Lwt.return_unit
            | `APPLY ->
              begin match read_val () with
                | WellFormed v -> self#set_value v ; loop ()
                | BadFormed s ->
                  popup_err (to_utf s) >>
                  loop ()
              end
            | `OK ->
              begin match read_val () with
                | WellFormed v -> self#set_value v ; win#destroy () ; Lwt.return_unit
                | BadFormed s ->
                  popup_err (to_utf s) >>
                  loop ()
              end
          in
          let%lwt () = loop () in
          opened <- false ;
          Lwt.return_unit
        end
  end

let val_popup ?parent ?name ~title ?width ?height ?packing ~val2s ?apply_button ~editval ?target ?cb ?setter ~init () =
  let v = new val_popup ?parent ?name ~title ?width ?height ?packing ?apply_button ~val2s ~editval ?target ?cb ?setter ~init () in
  { v ;
    w = v#button }

open Ggimage

let show_image ?dir ?packing prod ~assoc ?(default=Sized_stock(`DIALOG_QUESTION, `SMALL_TOOLBAR)) () =

  let image = GMisc.image ?packing () in

  (* Create all the pixbufs. *)
  let newassoc = List.map (fun (p, w) -> (p, get_opixb ?dir image w)) assoc in
  let default = get_opixb ?dir image default in

  set_opixb image default ;

  Events.to_cons prod
    begin fun v ->
      let pixb =
        match Common.mapfind (fun (p, i) -> if p v then Some i else None) newassoc with
        | None -> default
        | Some i -> i
      in
      set_opixb image pixb
    end ;
  image




