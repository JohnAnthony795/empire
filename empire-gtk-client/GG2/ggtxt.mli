open Gg
open Events
       
(* Create a text view 
 * TODO : associate with a ggval *)

(* max_repeat : after n occurrences, write only : 'message repeted k times'. 0 by default (== no max_repeat). *)
       
class view: ?frame:string -> ?scrolls:bool -> ?scroll_placement:Gtk.Tags.corner_type -> ?side_margin:int -> ?width:int -> ?height:int ->
	    ?append:(string*GText.tag) prod -> ?always_nl:bool -> ?max_repeat:int ->
	    editable:bool -> packing:pack -> unit ->
    object
      method append: (string*GText.tag) prod
      method buffer: GText.buffer
      method coerce: GObj.widget
      method textview: GText.view
      method mk_tag: GText.tag_property list -> GText.tag
    end
