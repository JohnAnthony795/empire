open Gg
open Ggval
open Common
open Common.Small
open Evt_val
open Ggimage
       
let mk_icon xpad widg im =
  let pixb = get_opixb widg im in
  let image = GMisc.image ~xpad ~ypad:0 ~show:true () in
  set_opixb image pixb ;
  image#coerce

									    
let configure_button ?border_width ?char_width ?char_height ?txt ?a2s set_icon ?icon but sval osend res =
  option_iter border_width but#set_border_width ;
  option_iter txt (fun t -> if a2s = None then but#set_label (Gg.to_utf t)) ;
  option_iter icon (fun i -> let ico = mk_icon (if txt = None then 0 else 6) but i in set_icon ico ; ico#misc#show ()) ;
  option_iter osend (fun send -> ignore(but#connect#clicked ~callback:(fun () -> send sval#value))) ;
  option_iter char_width (fun w -> but#misc#set_size_chars ~width:w ()) ;
  option_iter char_height (fun h -> but#misc#set_size_chars ~height:h ()) ;
  
  let () = match a2s with
    | None -> ()
    | Some f ->
       let up v = but#set_label (Gg.to_utf (f v)) in
       Events.to_cons sval#vprod up ;
       up sval#value
  in
  
  { v = res ;
    w = but }

(* Note: don't try set-image: the image is not shown, it is a gnome global setting. *)
    
let vbt ?border_width ?relief ?icon ?char_width ?char_height ?txt ?a2s ?packing ?btarget sval =
  let (target, send) = Events.new_prod ~name:"button" btarget
  and but = GButton.button ?relief ?packing ()
  in
  configure_button ?char_width ?char_height ?txt ?a2s but#set_image ?icon but sval (Some send) target

let bt ?border_width ?relief ?icon ?char_width ?char_height ?txt ?packing ?btarget ?cb v =
  let (target, send) = Events.new_prod ~name:"button" btarget in
  let but = GButton.button ?relief ?packing () in
  option_iter cb (fun cb -> Events.to_cons target cb) ;
  configure_button ?border_width ?char_width ?char_height ?txt but#set_image ?icon but (cst_val v) (Some send) target
		   
let toolbt ?icon ?border_width ?char_width ?char_height ?txt ?a2s ?packing ?btarget sval =
  let (target, send) = Events.new_prod ~name:"toolbutton" btarget
  and but = GButton.tool_button ?packing ()
  in
  configure_button ?border_width ?char_width ?char_height ?txt ?a2s but#set_icon_widget ?icon but sval (Some send) target

		   
let toggle style ?relief ?icon ?char_width ?char_height ?txt ?a2s ?packing sval =
  let but = match style with
    | `TOGGLE -> GButton.toggle_button ?relief ?packing ()
    | `CHECK -> GButton.check_button ?relief ?packing ()
  in
  but#set_active sval#value ;
  Events.to_cons sval#vprod but#set_active ;
  
  ignore(but#connect#toggled ~callback:(fun () -> sval#set_value but#active)) ;
  configure_button ?char_width ?char_height ?txt ?a2s but#set_image ?icon but sval None sval

let iconbt ?packing ?btarget ?cb wimg v =
  let evtbox = GBin.event_box ?packing () in
  let (target, send) = Events.new_prod ~name:"iconbt" btarget in
  option_iter cb (fun cb -> Events.to_cons target cb) ;
  let _ = Ggimage.image ~packing:evtbox#add wimg in

  ignore(evtbox#event#connect#button_release (fun ev -> GdkEvent.Button.button ev = 1 && (send v ; true)) ) ;
  
  { v = target ;
    w = evtbox }
  
