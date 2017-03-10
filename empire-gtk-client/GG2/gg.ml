(* Logging module *)
module Log = Mylog.MkSection (struct let section_name = "gg.ml" end)
open Log

open Common.Small
       
type utf = string

type icon = GtkStock.id
type pack = (GObj.widget -> unit)
type 'a widg = < coerce: GObj.widget ; .. > as 'a
						 
(* Txt conversion *)	      
let to_utf s =
  let open Transcode in
  if is_valid Utf8 s then s
  else match guess_encoding s with
       | Ascii | Utf8 -> assert false
       | Latin1 as enc -> to_utf enc s
       | Unknown m -> ignore(log_f Error "Invalid string (unknown encoding) : %s\n" m) ; s


(* Init *)			    
let () =
  Lwt_glib.install () ;
  ignore (GMain.init ()) ;
  ()

(* Style *)
let scalefont f x = 
  let size = Pango.Font.get_size f in
  let size' = iof (x *. (foi size)) in
  Pango.Font.set_size f size'

let set_widget_font ?weight ?scale ?style ?fg ?bg ?family (wid:#GObj.widget) =
  if weight = None && scale = None && style = None && family = None then ()
  else
    begin
      let nameFont = Pango.Font.copy wid#misc#pango_context#font_description in
      Common.option_iter weight (fun w -> Pango.Font.set_weight nameFont w) ;
      Common.option_iter style (fun s -> Pango.Font.set_style  nameFont s) ;
      Common.option_iter family (fun f -> Pango.Font.set_family nameFont f) ;
      Common.option_iter scale (fun s -> scalefont nameFont s) ;
      wid#misc#modify_font nameFont
    end ;
  
  Common.option_iter fg (fun (r, g, b) -> wid#misc#modify_fg [(`NORMAL, Ggcolor.rgb r g b)]) ;
  Common.option_iter bg (fun (r, g, b) -> wid#misc#modify_bg [(`NORMAL, Ggcolor.rgb r g b)]) ;
  ()

    

(**********************  POPUPS, Lwt compliant ************************)
    
(*** ................. COPIED FROM GToolbox WITH A SMALL UPDATE TO ALLOW MARKUP & LWT ............ ***)
let question_box ?parent ~title  ~buttons ?(default=1) ?(markup=false) ?icon message =
  let (finished, finish) = Lwtplus.fwait () in
  
  let button_nb = ref 0 in

  let window = GWindow.dialog ~modal:true ?parent ~title () in
  let hbox = GPack.hbox ~border_width:10 ~packing:window#vbox#add () in
  let bbox = window#action_area in
  begin match icon with
    None -> ()
  | Some i -> hbox#pack i#coerce ~padding:4
  end;
  ignore (if markup then GMisc.label ~markup:message ~packing:hbox#add ()
                    else GMisc.label ~text:message ~packing:hbox#add ()) ;
  (* the function called to create each button by iterating *)
  let rec iter_buttons n = function
      [] ->
        ()
    | button_label :: q ->    
        let b = GButton.button ~label: button_label ~packing:(bbox#pack ~expand:true ~padding:4) ()
        in
        ignore(b#connect#clicked ~callback: (fun () -> button_nb := n; window#destroy ()));
        (* If it's the first button then give it the focus *)
        if n = default then b#grab_default () else ();

        iter_buttons (n+1) q
  in
  iter_buttons 1 buttons;
  ignore(window#connect#destroy ~callback:finish) ;
  window#set_position `CENTER;
  window#show ();

  let%lwt () = finished in
  Lwt.return !button_nb


let message_box ?parent ~title ?icon ?markup ?(ok="Ok") message =
  let%lwt _ = question_box ?parent ?icon ?markup ~title message ~buttons:[ ok ] in
  Lwt.return_unit

let message_box ?parent ?markup ~title ?icon msg = 
  let icon = Common.option_map icon (fun s -> GMisc.image ~stock:s ~icon_size:`DIALOG ()) in
  message_box ?parent ?markup ?icon ~title:(to_utf title) (to_utf (Common.wrap 80 msg))

let popup_err ?parent ?markup ?(title="ERROR!") ?(icon=`DIALOG_ERROR) msg = message_box ?parent ~icon ?markup ~title msg
let popup_ok ?parent ?markup ?(title="") ?icon msg = message_box ?parent ?icon ?markup ~title msg

let popup_ok_cancel ?parent ?(title="") ?(ok="OK") ?(cancel="Cancel") ?(default=1) msg = 
  let%lwt nb = question_box ?parent ~title:(to_utf title) ~buttons:[to_utf ok ; to_utf cancel] ~default (to_utf msg) in
  Lwt.return (nb = 1)

let confirm ?parent ?(title="Confirm?") ?(msg="Do you confirm this action?") f =
  match%lwt popup_ok_cancel ?parent ~title ~default:2 msg with
  | true -> f ()
  | false -> Lwt.return_unit

(*********************   USEFUL WIDGETS  ******************)		   

(* Label *)
let label ?justify ?(markup=false) ?wrap ?packing txt = 
  let txt = match wrap with
  | None -> to_utf txt
  | Some c -> Common.wrap c (to_utf txt)
  in
  if markup then GMisc.label ?justify ~markup:txt ?packing ()
  else GMisc.label ?justify ~text:txt ?packing ()

let image ~packing (fname, width, height) = GMisc.image ~file:fname ~xpad:0 ~ypad:0 ~width ~height ~packing ()

let vsep (box:GPack.box) size = GPack.hbox ~packing:(box#pack ~padding:size) ()
let hsep (box:GPack.box) size = GPack.vbox ~packing:(box#pack ~padding:size) ()
		   
class ['a] txthbox ?prefix ?postfix ?(spacing=8) ?packing (middle: 'a widg) =
  let hbox = GPack.hbox ?packing ~spacing () in

  let prelbl  = Common.option_map prefix (label ~packing: (hbox#pack ~from:`START ~expand: false)) in
  let ()      = hbox#pack ~from:`START ~expand:false middle#coerce in
  let postlbl = Common.option_map postfix (label ~packing: (hbox#pack ~from:`START ~expand: false)) in

  object
    method hbox    = hbox
    method coerce  = hbox#coerce
    method prelbl  = try Common.option_get prelbl with Not_found -> assert false
    method postlbl = try Common.option_get postlbl with Not_found -> assert false
    method widget  = middle
  end

