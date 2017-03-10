type title =
  { weight: Pango.Tags.weight option ;
    scale: float option ;
    style: Pango.Tags.style option ;
    fg: (int*int*int) option ;
    family: string option ;
    content: string }

let mk_title ?weight ?scale ?style ?fg ?family s =
  { weight ; scale ; style ; fg ; family ; content = Gg.to_utf s }

type t =
  { mutable enabled: bool ;
    gmenu: GMenu.menu ;
    mutable items: (string * GMenu.menu_item) list ;
  }

and menu_elt =
  | Title of title
  | Sep
  | Action of string * (unit -> unit)
  | Lab of string * menu_elt
  | Submenu of menu_elt * t 

let gmenu menu = menu.gmenu

let menu_elements menu = menu.items

let rec add_elt ~packing = function
  | Sep -> ("", GMenu.separator_item ~packing ())

  | Title tit ->
    let tit_item = GMenu.menu_item ~label:tit.content ~packing () in
    let () =
      match tit_item#children with
      | [ ch ] -> Gg.set_widget_font ?weight:tit.weight ?scale:tit.scale ?style:tit.style ?fg:tit.fg ?family:tit.family ch
      | _ -> ()
    in
    ("", tit_item)
               
  | Action (s, cb) ->
    let gitem = GMenu.menu_item ~label:(Gg.to_utf s) ~packing () in
    ignore (gitem#connect#activate ~callback:cb) ;      
    ("", gitem)
    
  | Lab (l, elt) ->
    let  (_, gitem) = add_elt ~packing elt in
    (l, gitem)

  | Submenu (elt, sub) ->
    let (l, gitem) = add_elt ~packing elt in
    gitem#set_submenu sub.gmenu ;
    (l, gitem)


let create_popup_menu elements =
  let gmenu = GMenu.menu () in
  let items = List.map (add_elt ~packing:gmenu#append) elements in
  { enabled = true ;
    gmenu ;
    items }

let attach_menu (w:#GObj.widget) ?(button=3) menu =
  (* Afficher le popup menu au clic *)
  ignore (w#event#connect#button_press ~callback:
            (fun event ->
               let () =
                 if menu.enabled && GdkEvent.Button.button event = button then
                   menu.gmenu#popup ~button ~time:(GdkEvent.get_time event)
                 else ()
               in
               false)) ;
  ()

  
let enable menu flag = menu.enabled <- flag

(* Returns (position, rev_head, tail), where the first element of rev_head is the element we want. *)
let find name pos label l =

  if label = Some "" then failwith (Printf.sprintf "Ggmenu.%s: cannot look for a menu item with empty label." name) ;
  
  let rec loop index acu l =
    match (pos, label, l) with
    | None, None, _ -> assert false
    | Some p, _, l when p = index -> (index, acu, l)
    | Some (-1), _, [] -> (index, acu, [])
    | Some p, _, x :: xs -> loop (index+1) (x :: acu) xs
    | Some p, _, [] -> failwith (Printf.sprintf "Ggmenu.%s: incorrect position (%d), not in menu" name p)
    | _, Some s, [] -> failwith (Printf.sprintf "Ggmenu.%s: menu item not found (label : %s)" name s)
    | _, Some s, ((nm, _) as x) :: xs ->  if nm = s then (index + 1, x :: acu, xs) else loop (index+1) (x :: acu) xs
  in
  loop 0 [] l

let check name pos label =
  match (pos, label) with
  | None, None -> failwith (Printf.sprintf "Ggmenu.%s: must specify label or position." name)
  | Some _, Some _ -> failwith (Printf.sprintf "Ggmenu.%s: must not specify both label and position." name)
  | _ -> ()

let remove menu ?pos ?label () =
  check "remove" pos label ;
  match find "remove" pos label menu.items with
  | (_,[],_) -> failwith "Ggmenu.remove: incorrect position (0)"
  | (_,(nm,gelt)::xs,rest) ->
    menu.gmenu#remove gelt ;
    menu.items <- List.rev_append xs rest ;
    ()

let replace menu ?pos ?label elt =
  check "replace" pos label ;
  match find "replace" pos label menu.items with
  | (_,[],_) -> failwith "Ggmenu.remove: incorrect position (0)"
  | (pos,(nm,gelt)::xs,rest) ->
    let x = add_elt ~packing:(menu.gmenu#insert ~pos) elt in
    menu.gmenu#remove gelt ;
    menu.items <- List.rev_append xs (x :: rest) ;
    ()

let insert_after menu ?pos ?label elt =
  check "insert_after" pos label ;
  let (pos, head, tail) = find "insert_after" pos label menu.items in

  let x = add_elt ~packing:(menu.gmenu#insert ~pos) elt in
  menu.items <- List.rev_append head (x :: tail) ;
  ()

let get_menu_item menu ?pos ?label () =
  check "get_menu_item" pos label ;
  match find "get_menu_item" pos label menu.items with
  | (_,[],_) -> failwith "Ggmenu.get_menu_item: incorrect position(0)"
  | (_,(_,gelt)::_,_) -> gelt
    
