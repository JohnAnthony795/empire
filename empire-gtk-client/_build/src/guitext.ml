open Connect
open Ggval

let add_textview ~packing intf config =

  let vbox = GPack.vbox ~packing ~homogeneous:false () in

  (*** Text window with raw messages ***)
  let scrolled = GBin.scrolled_window ~packing:(vbox#pack ~fill:true ~expand:true) () in

  (* Do not use #add_with_viewport. Breaks auto-scrolling of buffer. *)
  let textview = GText.view ~cursor_visible:false ~editable:false ~packing:scrolled#add () in
  let () = textview#set_left_margin 15
  and () = textview#set_right_margin 15 in

  let buffer = textview#buffer in

  let insert tag msg =
    buffer#insert ~iter:buffer#end_iter ~tags:[tag] msg ;
    textview#scroll_mark_onscreen `INSERT
  in

  let mk_tag = buffer#create_tag in
  let rcv_tag  = mk_tag [`FOREGROUND "green"]
  and emit_tag = mk_tag [`FOREGROUND "orange"]
  and error_tag = mk_tag [`FOREGROUND "red" ; `WEIGHT `BOLD] in

  (* Editable text area to send commands directly to server. *)
  let edit = Ggval.estring ~name:"edit-server-commands" ~prefix:"Command : " ~packing:vbox#pack ~width_chars:48 ~has_frame:true ~style:`ENTRY "" in
  (* We reset the text when it is submitted (creates a one-time loop). *)
  Events.(set_loop edit.v#prod (fixloop ~limit:2 ())) ;

  (*** Handling events ***)

  Events.to_cons intf.raw_received (fun msg -> insert rcv_tag ("< " ^ msg ^ "\n")) ;
  Events.to_cons intf.raw_emitted (fun msg -> insert emit_tag ("> " ^ msg ^ "\n")) ;
  Events.to_cons intf.rcv_msg (function Error msg -> insert error_tag ("*** " ^ msg ^ "\n") | _ -> ()) ;

  (* Edit area is active iff the connection is ready. *)
  Events.to_cons intf.rcv_msg (function Connection_active act -> edit.w#widget#misc#set_sensitive act | _ -> ()) ;

  Lwt.async (fun () ->
      let%lwt config = config in

      (* Send orders directly *)
      Events.to_cons edit.v#prod
        begin function
          | `Submit ->
            let msg = edit.v#value in
            edit.v#set_value "" ;
            Lwtplus.aasync (Connect.send_raw config.connection) msg
          | _ -> ()
        end ;
      Lwt.return_unit) ;

  ()

