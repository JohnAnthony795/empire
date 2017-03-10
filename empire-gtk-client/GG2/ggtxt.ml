open Events

let hash (txt, tag) = Hashtbl.hash (txt, tag#get_oid)

let times = function
  | 1 -> "once" (* should not happen *)
  | 2 -> "twice"
  | n -> string_of_int n ^ " times"
		
				   
class view ?frame ?(scrolls=true) ?scroll_placement ?side_margin ?width ?height ?append ?(always_nl=true) ?(max_repeat=0) ~editable ~packing () =

  let (prod, _) = new_prod ~name:"Ggtxt.view" append in

  let textview = GText.view ~cursor_visible:editable ?width ?height ~editable () in
  let buffer = textview#buffer in

  (* May be a scrolled window. *)
  let widg2 =
    if scrolls then
      begin
	let scrolled = GBin.scrolled_window ?placement:scroll_placement ?width ?height () in
	(* Arrrr! *)
	if scroll_placement <> None then scrolled#misc#set_property "window-placement-set" (`BOOL true) ;
	
	scrolled#add textview#coerce ;
	scrolled#coerce
      end
    else textview#coerce
  in

  (* May be a frame *)
  let mainwidget =
    match frame with
    | None -> widg2
    | Some label ->
       let textframe = GBin.frame ~label:"Messages" () in
       textframe#add widg2 ;
       textframe#coerce
  in

  let () = packing mainwidget in

  (* Margins *)
  let () = Common.option_iter side_margin (fun m -> textview#set_left_margin m ;
						    textview#set_right_margin m)
  in

  (* Appends *)
  let write_append (txt, tag) =
    (* All this stupid stuff to have the view following the last line without getting spurious selection of text.*)
    let enditer = buffer#end_iter in
    buffer#move_mark `INSERT enditer ;
    buffer#select_range enditer enditer ;
    textview#scroll_mark_onscreen `INSERT ;
    
    let txt = if always_nl then txt ^ "\n" else txt in
    buffer#insert ~iter:buffer#end_iter ~tags:[tag] txt ;
  in


  (* Contains a hash of the last message + number of occurrences. *)
  let last_append = ref (0,0) in

  (* Mark corresponding to the start of "repeated..." message. 
   * Used to remove the line *)
  let last_mark = `MARK (buffer#create_mark buffer#end_iter) in
  
  let () = to_cons prod
		   begin fun arg ->
			 (* Check previous occurrences *)
			 let hh = hash arg
			 and (lh, num) = !last_append in
			 
			 if lh = hh then
			   begin
			     last_append := (lh, num+1) ;
			     (* Remove last line *)
			     if num > 1 then buffer#delete ~start:(buffer#get_iter_at_mark last_mark) ~stop:buffer#end_iter ;
			     buffer#move_mark last_mark buffer#end_iter ;
			     write_append ("Last message repeated " ^ times (num+1), snd arg) ;
			   end
			 else
			   begin
			     last_append := (hh, 1) ;
			     write_append arg
			   end
		   end
  in
  object
    method append = prod
    method buffer = buffer
    method coerce = mainwidget#coerce
    method textview = textview
    method mk_tag l = textview#buffer#create_tag l
  end

