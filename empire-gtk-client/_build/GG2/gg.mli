open GWindow

type utf = string

(* Returns a UTF string, whatever the initial format. *)
val to_utf: string -> utf

type icon = GtkStock.id
type pack = (GObj.widget -> unit)	     
type 'a widg = < coerce: GObj.widget ; .. > as 'a
				
(* Style *)
val set_widget_font:
  ?weight:Pango.Tags.weight ->
  ?scale:float ->
  ?style:Pango.Tags.style ->
  ?fg:int*int*int ->
  ?bg:int*int*int ->
  ?family:string ->
  #GObj.widget -> unit
						 
(*** POPUPs ***)

(* The ?parent argument is required to be 'modal'. *)

val message_box: ?parent:#window_skel -> ?markup:bool -> title:utf -> ?icon:icon -> utf -> unit Lwt.t
								     
val popup_err: ?parent:#window_skel -> ?markup:bool -> ?title:utf -> ?icon:icon -> utf -> unit Lwt.t
val popup_ok:  ?parent:#window_skel -> ?markup:bool -> ?title:utf -> ?icon:icon -> utf -> unit Lwt.t

val popup_ok_cancel: ?parent:#window_skel -> ?title:utf -> ?ok:utf -> ?cancel:utf -> ?default:int -> utf -> bool Lwt.t

val confirm: ?parent:#window_skel -> ?title:utf -> ?msg:utf -> (unit -> unit Lwt.t) -> unit Lwt.t


(*********************   USEFUL WIDGETS  ******************)

val label: ?justify:Gtk.Tags.justification -> ?markup:bool -> ?wrap:int -> ?packing:pack -> utf -> GMisc.label
							   
(* val image: packing:pack (fname, width, height) ... *)

(* Should'nt need these *)
val vsep: GPack.box -> int -> GPack.box
val hsep: GPack.box -> int -> GPack.box
  
class ['a] txthbox: ?prefix:utf -> ?postfix:utf -> ?spacing:int -> ?packing:pack -> 'a widg ->
    object
      method hbox: GPack.box
      method coerce: GObj.widget
      method prelbl: GMisc.label
      method postlbl: GMisc.label
      method widget: 'a
    end

							   
