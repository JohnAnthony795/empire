open GnoCanvas
open Gcanvas
open Ggmenu

open Ebase
open Emap
open Connect

type gintf =
  { config: Connect.config ;
    gstate: game_state ;
    select_pos: hxy option Evt_val.simple_val ;
    current_info: info option Evt_val.simple_val ;
    popup: canvas_click Events.prod ;
    add_visual: xyprod:xy Events.prod -> GdkPixbuf.pixbuf -> (int*int) -> pixbuf g_item ;
    pack: Gg.pack }

(* Submenu for a city (build...) *)
let build_menu gintf city_id =
  let cb pt () =
    Lwt.async (fun () -> Connect.set_city_production gintf.config.connection city_id pt.a_id)
  in
  create_popup_menu (List.map (fun pt -> Action (Printf.sprintf "%s (%d turns)" pt.a_name pt.a_build_time, cb pt)) (get_piece_types gintf.gstate))

(* Dynamically populate the popup contextual menu, depending on units, city, ... *)
let populate_menu gintf menu =
  let%lwt info =
    match gintf.current_info#value with
    | Some i -> Lwt.return i
    | None ->
      match%lwt Events.wait_on ~pr:(fun v -> v <> None) gintf.current_info#vprod with
      | None -> assert false (* contradicts the predicate above *)
      | Some i -> Lwt.return i
  in

  let append txt = insert_after menu ~pos:(-1) (Title (mk_title txt)) in
  
  let () =
    match info.city_id, info.build with
    | _, Not_a_city -> ()
    | _, Not_your_city -> append "(Not your city)"
    | _, Free_city     -> append "(Unoccupied city)"
    | _, Not_available -> append "No action now (not your turn)"
    | Some city_id, Unit None     -> insert_after menu ~pos:(-1) (Submenu (Title (mk_title "Build..."), build_menu gintf city_id))
    | Some city_id, Unit (Some _) -> insert_after menu ~pos:(-1) (Submenu (Title (mk_title "Replace build..."), build_menu gintf city_id))
    | None, Unit _ -> assert false (* This is not a city, it cannot build something. *)
  in
  Lwt.return_unit

(* Install selection handler *)
let handle_selection gintf = 

  let dims = (gintf.config.width, gintf.config.height) in
  let mypid = gintf.config.player_id in
  
  (* The selection frame *)
  let it_select = Gcanvas.get_item (gintf.add_visual ~xyprod:(Events.mapfilter gintf.select_pos#vprod (fun op -> Common.option_map op (Coords.ortho_of_hexa dims)))
                                      (Ggimage.pixbuf_from_file "TILES/frame.png") (0,0))
  in

  (* Display information about the selected tile *)
  let item2s = function
    | City (cid, None) -> Printf.sprintf "Free City (#%d)" cid
    | City (cid, Some pl) ->
      if pl = mypid then Printf.sprintf "Your city (#%d)" cid
      else Printf.sprintf "Enemy city (#%d), belongs to player #%d" cid pl

    | Piece (player_id, pid, ptid, hits) ->
      let ptyp = piece_type_of_int gintf.gstate ptid in
      if player_id = mypid then Printf.sprintf "Your %s (%d / %d)" ptyp.a_name hits ptyp.a_max_hits
      else Printf.sprintf "Enemy %s (%d / %d), belongs to player #%d" ptyp.a_name hits ptyp.a_max_hits pid
  in
  let info2s = function
    | None -> "(asking for data...)"
    | Some i ->
      let s1 = Common.sep item2s "\n" i.items in
      s1 ^ build_info2s i.build
  in

  let vbox = GPack.vbox ~packing:gintf.pack () in

  (* Label which displays items *)
  Gg.set_widget_font ~scale:1.2 ~fg:(80,80,240) (Ggval.llabel info2s ~packing:vbox#add gintf.current_info#vprod) ;

  it_select#hide () ;
  Events.to_cons gintf.select_pos#vprod
    begin function
      | None -> it_select#hide () ;
      | Some hxy ->
        (* When the select_hexa receives coordinates, ... 
         * Shows the selection frame. *)
        it_select#show () ;
    end ;

  (*** Popup menu ***)
  Events.to_cons ~last:true gintf.popup
    begin fun cc ->
      if GdkEvent.Button.button cc.event = 3 then
        begin
          (* Create a new menu each time.
           * This menu is populated asynchronously (and blockingly).
           * This would be painful if the menu was shared by several threads (multiple clicks). *)
          let menu = create_popup_menu
              [ Title (mk_title ~weight:`BOLD ~scale:1.2 ~fg:(100,120,240) "Actions") ;
                Sep ]
          in          
          let _ = populate_menu gintf menu in
          (gmenu menu)#popup ~button:3 ~time:(GdkEvent.get_time cc.event)
        end
      else ()
    end ;
  
  ()

  (* Debug : show click coordinates. *)
  (*
    let show_rectangle = ref false in
    Events.to_cons gintf.orthoclicks
begin fun cl ->
let (hx,hy) = hexa_of_ortho dims (cl.xpos,cl.ypos) in
Printf.printf "Click : ortho = %f,%f   hexa = %d,%d\n%!" cl.xpos cl.ypos hx hy ; 

if !show_rectangle then
(* Fill a rectangle with color to visualize coordinates *)
begin
let ix = iof cl.xpos
and iy = iof cl.ypos in

for x = ix to ix + 480 do
for y = iy to iy + 480 do
let fx = foi x
and fy = foi y in
let (hx2,hy2) = hexa_of_ortho dims (fx, fy) in
let delta = hx2 + 5 * hy2 in
let col = Gg.rgba (20 + delta * 192) (-delta) (180 + delta * 17) 180 in
let _ = GnoCanvas.line ~points:[| fx ; fy ; fx +. 1.0 ; fy |]
~props:[`FILL_COLOR_RGBA col ; `SMOOTH false ; `WIDTH_PIXELS 1] units_gr
in
()
done ;
done ;
show_rectangle := false ;
end ;    
end ;
     *)
