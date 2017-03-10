open Ggval
open Evt_val
open Array_evt_val

(* delete_on_label: if mk_label is defined, inserts a destroy icon. *)
       
val tabs: ?enable_popup:bool -> ?homogeneous_tabs:bool -> ?scrollable:bool -> ?show_border:bool ->
	  ?tab_border:int -> ?tab_pos:Gtk.Tags.position -> ?packing:Gg.pack ->
	  ?mk_label:((('b, 'evt) #evt_val as 'a) -> _ Gg.widg) -> ?delete_on_label:Ggimage.which_image ->
	  ('a -> _ Gg.widg) ->
	  'a array_val -> ('a array_val, GPack.notebook) wpair 
															     
