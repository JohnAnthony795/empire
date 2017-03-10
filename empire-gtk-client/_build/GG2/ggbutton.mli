open Gg
open Ggval
open Evt_val
open Events
open Ggimage

(* Images are not shown... this is a gnome global setting... don't ask...*)

       
(* Use it as follows:
 * bt ~txt:"Click me!" ~packing @@ vref 200
 *)

(* Classical button.
 * txt is overriden by a2s, if provided.
 * On click, the current value of the simple val is sent to the target prod (was created if necessary). 
 * The wpair contains the btarget prod. 
 * 
 * char_width: width of the widget, expressed as a number of characters (in the default current font).
 *)
val vbt: ?border_width:int -> ?relief:Gtk.Tags.relief_style -> ?icon:which_image -> ?char_width:int -> ?char_height:int -> ?txt:utf -> ?a2s:('a -> string) ->
	?packing:pack -> ?btarget:'a prod -> 'a simple_val -> ('a prod, GButton.button) wpair

(* Simple button with a constant value *)
val bt: ?border_width:int -> ?relief:Gtk.Tags.relief_style -> ?icon:which_image -> ?char_width:int -> ?char_height:int -> ?txt:utf ->
	?packing:pack -> ?btarget:'a prod -> ?cb:('a -> unit) -> 'a -> ('a prod, GButton.button) wpair
													      
(* Button in toolbox *)
val toolbt: ?icon:which_image -> ?border_width:int -> ?char_width:int -> ?char_height:int -> ?txt:utf -> ?a2s:('a -> string) ->
	    ?packing:(GButton.tool_item_o -> unit) -> ?btarget:'a prod -> 'a simple_val -> ('a prod, GButton.tool_button) wpair

													      
(* Toogle button *)
val toggle: [ `TOGGLE | `CHECK ] -> ?relief:Gtk.Tags.relief_style -> ?icon:which_image ->
	    ?char_width:int -> ?char_height:int -> ?txt:utf -> ?a2s:(bool -> string) ->
	    ?packing:pack -> bool simple_val -> (bool simple_val, GButton.toggle_button) wpair


(* For the moment, it is not possible to make a button which is exactly the size of an image.
 * the xthickness and ythickness default values cannot be set in ocaml. 
 *
 * This is a button which has exactly the size of an image. *)
val iconbt: ?packing:pack -> ?btarget:'a prod -> ?cb:('a -> unit) -> which_image -> 'a -> ('a prod, GBin.event_box) wpair
