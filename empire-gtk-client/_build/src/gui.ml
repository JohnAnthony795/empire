open Common.Small
open Gcanvas
open Ggval

open Coords
open Connect
open Gui_interaction
open Ebase
open Emap
open Emap_graphics

let get_adjlength adj = int_of_float (adj#upper -. adj#lower)

let go ~observe_only intf =

  let (finished, finish) = Lwtplus.fwait () in

  let window = GWindow.window ~kind:`TOPLEVEL ~title:(Gg.to_utf "Empire") ~width:1000 ~height:680 ~border_width:2 ~show:true () in

  let _ = window#connect#destroy ~callback:finish in

  (* Main content *)
  let hbox = GPack.hbox ~packing:window#add ~homogeneous:false () in

  (* Canvas. Click, keys -> focus *)
  let icanv = Gcanvas.icanvas ~with_scroll:true ~arrow_keys:[`SHIFT] ~aa:false ~packing:(hbox#pack ~expand:true ~fill:true)
      ~pixb:(Ggimage.pixbuf_from_file "rome.jpg") () in
  Events.(to_cons (ujoin icanv.clicks icanv.keystrokes) (fun _ -> icanv.canvas#misc#grab_focus ())) ;

  (* Config will be sent by the communication interface. *)
  let config = Lwtboard.waitformap intf.board (function Config cf -> Some cf | _ -> None) in

  let sidebox = GPack.paned `VERTICAL ~packing:hbox#add () in
  Guitext.add_textview ~packing:sidebox#pack2 intf config ;

  (*** Top-right box. Contains a label : "your turn", a button "end of turn", the current date. ***)
  let topbox = GPack.vbox ~packing:sidebox#pack1 ~homogeneous:false () in

  (* Bottom hbox in topbox. *)
  let botbox = GPack.hbox ~packing:(topbox#pack ~from:`END) () in
  
  (* Zoom level *)
  let zoom = Ggval.eint ~prefix:"Zoom : " ~postfix:"%" ~name:"zoom" ~lower:5 ~upper:200 ~step_incr:10 ~page:40 ~init:100 ~packing:botbox#pack () in

  (* Track / do not track *)
  let track = Ggval.ebool ~name:"track" ~label:" Center canvas on each move" ~packing:(botbox#pack ~padding:20) `CHECK ~init:true () in
  
  (* Date *)
  Gg.set_widget_font ~weight:`BOLD ~scale:1.2 ~fg:(90,220,60)
    (Ggval.lint ~prefix:"Date : " ~postfix:"" ~packing:topbox#pack
       (Events.mapfilter intf.rcv_msg (function Date d -> Some d | _ -> None))) ;

  (* Label: your turn / observer mode *)
  Gg.set_widget_font ~weight:`BOLD ~scale:1.6 ~fg:(240,120,80)
    (if observe_only then GMisc.label ~xalign:0.5 ~text:(Gg.to_utf "OBSERVER MODE") ~packing:topbox#pack ()
     else (Ggval.llabel ~xalign:0.5 (function true -> "Your TURN" | false -> "Waiting for your turn...") ~packing:topbox#pack
         (Events.mapfilter intf.rcv_msg (function Connection_active b -> Some b | _ -> None)))) ;

  (* Button end turn *)
  let but_end = Ggbutton.bt ~txt:"End turn!" ~packing:topbox#pack () in
  if not observe_only then Events.to_cons intf.rcv_msg (function Connection_active flag -> but_end.w#misc#set_sensitive flag | _ -> ()) ;

  (* Auto button *)
  if observe_only then
    begin
      let autobut = Ggval.ebool ~name:"auto-end" ~label:" Auto end turn" ~packing:(topbox#pack ~padding:6) `CHECK ~init:false () in
      Events.to_cons intf.rcv_msg (function Expect_end_turn -> if autobut.v#value then but_end.w#clicked () | _ -> ())
    end ;

  Lwt.async begin fun () ->
    (* Graphical interaction is possible only after initialisation *)
    let%lwt config = config in

    (* Memorize the game state (position of items, cities, ...) *)
    let gstate = init_game config in
    let () = dump_piece_types gstate in (* DEBUG *)

    let dx = config.width
    and dy = config.height in

    let dims = (dx, dy) in

    (* Layers *)
    let mk_group () = GnoCanvas.group icanv.canvas#root in
    let back_gr   = mk_group ()
    and ground_gr = mk_group ()
    and decor_gr  = mk_group ()
    and city_gr   = mk_group ()
    and units_gr  = mk_group ()
    and select_gr = mk_group () in

    (* End turn *)
    Events.to_cons but_end.v (fun () -> Lwtplus.aasync Connect.end_turn config.connection) ;

    (* Something happens at hexagonal coordinates xy. If it is not visible on the canvas, center the canvas on it. *)
    let check_visible ?(force=false) hxy =
      if force || track.v#value then ignore(Gcanvas.is_visible ~do_center:true ~sys:orthosys ~margin:120.0 icanv (ortho_of_hexa dims hxy))
    in
    
    (* Add a visual element. Group indicates the layer. 
     * xyprod produces the coordinates that moves the element. 
     * hxy is the initial position. Overriden by xy if given (used for positioning the background). *)
    let add_visual group ?xyprod pixbuf ?xy hxy =
      let (x,y) =
        match xy with
        | None -> ortho_of_hexa dims hxy
        | Some xy -> xy
      in
      let gpix = GnoCanvas.pixbuf ~pixbuf ~props:[`ANCHOR `CENTER] group in
      Gcanvas.mk_item ~sys:orthosys ?xyprod ~x ~y ~ang:0.0 gpix
    in

    let create_gitem item pix hxy = match item with
      | City (cid, _) -> add_visual city_gr pix hxy
      | Piece _ -> add_visual units_gr ~xyprod:(Events.map (Emap.get_hxyprod gstate item) (ortho_of_hexa dims)) pix hxy
    in

    (* Table which associates items to their graphical object (gitem). *)
    let gr_table = Emap_graphics.new_table gstate create_gitem in

    (* Show an item (city or unit) *)
    let show_item hxy item =
      Emap.set_hxy gstate item hxy ;
      update gr_table item (* => The gitem becomes visible. *)
    in

    let show_tile hxy tile =
      check_visible hxy ;
      match Emap.get_tile gstate hxy with
      | None ->
        (* This tile was unknown *)
        Emap.set_tile gstate hxy tile ;
        ignore(add_visual ground_gr (Emap_graphics.find_pixbuf gstate hxy tile) hxy) ;

        (* When a tile is added, we may also add some decoration. *)
        Common.myiter (Emap_graphics.decoration gstate hxy) (fun (dxy, dpx) -> ignore(add_visual decor_gr dpx dxy)) ;
        ()

      | Some _ -> () (* Already visible, nothing to do *)
    in

    (* Events = coordinates of a selected tile. *)
    let orthoclicks = Gcanvas.map_clicks ~new_sys:orthosys icanv.clicks in
    let select_pos = Evt_val.vref ~setter:(Events.map orthoclicks (fun cl -> Some (hexa_of_ortho dims (cl.xpos, cl.ypos)))) None in

    (* Arrows : move selection *)
    let add_pos (dx,dy) = match select_pos#value with
      | None -> ()
      | Some (x,y) -> select_pos#set_value (Some (x+dx, y+dy))
    in
    Events.to_cons icanv.keys
      begin function
        | (0, Up) -> add_pos (1,-1)
        | (0, Up_left) -> add_pos (0,-1)
        | (0, Up_right) -> add_pos (1,0)
        | (0, Down) -> add_pos (-1,1)
        | (0, Down_left) -> add_pos (-1,0)
        | (0, Down_right) -> add_pos (0,1)
        | (0, Left) -> add_pos (-1,-1)
        | (0, Right) -> add_pos (1,1)
        | _ -> ()
      end ;

    (* Set zoomlevel & keep the selection visible. *)
    let set_zoomlevel n =
      Gcanvas.set_zoom icanv n ;
      Common.option_iter select_pos#value (check_visible ~force:true) ;
    in
    Events.to_cons zoom.v#vprod set_zoomlevel ;

    
    (* Get information about the current selection. *)
    let current_info = Evt_val.vref None in

    (* Reset value (to avoid unrelated contextual menu) & to avoid recursion. *)
    let () = Events.to_cons select_pos#vprod (fun _ -> current_info#set_value None) in

    (* Used a stabilized version of select_pos, in case the selection moves too quickly. *)
    let () = Events.to_cons Events_timed.(stabilized (cst_cat 0.1) select_pos#vprod)
        (function
          | None -> current_info#set_value None
          | Some p ->
            check_visible ~force:true p ;
            (* Delay request to avoid loop on rcv_msg -- (unnecessary if the event source is stabilized). *)
            Lwtplus.later (fun () ->
                let%lwt info = watzere gstate p in
                let () = current_info#set_value (Some info) in
                Lwt.return_unit))
    in

    let gintf =
      { config ;
        gstate ;
        select_pos ;
        current_info ;
        popup = orthoclicks ;
        add_visual = (fun ~xyprod pixb hxy -> add_visual select_gr ~xyprod pixb hxy) ;
        pack = topbox#add }
    in
    Gui_interaction.handle_selection gintf ;

    (* Set canvas size & new background *)
    Gcanvas.set_bb icanv ~sys:orthosys (List.map (ortho_of_hexa dims) [ (-1,-1) ; (-1,dy+1) ; (dx+1,-1) ; (dx+1,dy+1) ]) ;

    (* Add background *)
    let resized = (get_adjlength icanv.canvas#hadjustment, get_adjlength icanv.canvas#vadjustment) in
    let _ = add_visual back_gr (Ggimage.pixbuf_from_file ~resized "rome.jpg") ~xy:(0.0,0.0) (0,0) in
    (* Let the canvas appear *)
    let%lwt () = Lwt_unix.sleep 0.4 in

    Events.to_cons intf.rcv_msg
      begin function
        | Connection_active false | Expect_end_turn -> ()
        | Date _ -> ()

        | Connection_active true ->
          (* Something has changed. Re-select current cell if info was shown.
           * Also, info is set to None when asking information to the server. Hence, this test avoids recursion. *)
          if current_info#value <> None then select_pos#vchanged

        | Set_explored (hxy, tile) ->
          (* Hide units visible on this tile. *)
          let items = itemzere gstate hxy in
          List.iter (Emap_graphics.hide gr_table) items ;
          show_tile hxy tile

        | Set_visible (hxy, tile, oitem) ->
          show_tile hxy tile ;
          Common.option_iter oitem (show_item hxy) ;
          ()

        | Move (pid, hxy) ->
          show_item hxy (Emap.get_item gstate (Piece (0,pid,0,0)))

        | Delete_piece pid -> ()

        | Invasion (success, city_id, hxy) ->
          check_visible hxy

        | Create_piece (pid, ptyp_id, city_id, hits) ->
          let hxy = Emap.get_hxy gstate (City (city_id, None)) in
          let item = Piece (config.player_id, pid, ptyp_id, hits) in
          show_item hxy item        

        | Error e -> ()
      end ;

    (* GUI is ready *)
    Lwtboard.send intf.board Gui_ready

  end ;

  finished
