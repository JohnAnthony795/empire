open GWindow
open Gg
open Evt_val
open Events
open Common.Small

(* Principles
 *
 * Widgets work in different ways.
 *  - eint/efloat use an adjustment, which takes care of sending notifications.
 *  - estring contains an 'entry' which contains the current string value.
 *
 * - an 'event' value can always be shared between several widgets.
 *
 *)

       
(* Note: "suspicion of segfault".
 * When a gtk object is destroyed (e.g. because the window is closed), 
 * if a method is invoked on the underlying C-gtk-object, it may segfault.
 * If it occurs, all gtk objects should be protected in order to do nothing if they have
 * already been destroyed. *)
       
(*** Gtk widgets associated with an event value (only a pair actually). ***)
type ('a, 'b) wpair =
  { (* The evt value *)
    v: 'a ;
    
    (* The widget *)
    w: 'b widg }
    
val gnum: digits:int -> v:(< adjustment : GData.adjustment; .. > as 'a) -> ?prefix:utf -> ?postfix:utf -> ?packing:pack -> unit ->
	  ('a, GEdit.spin_button txthbox) wpair 

(* All name attributes are optional debug information. *)
					  

(* Editable float (int). *)	
val efloat: ?name:string -> ?prefix:utf -> ?postfix:utf -> ?packing:pack -> ?cb:(float -> unit) -> ?target:float prod -> ?setter:float prod ->
	    ?digits:int -> lower:float -> upper:float -> step_incr:float -> page:float -> init:float -> unit -> (adjfloat, GEdit.spin_button txthbox) wpair

val eint: ?name:string -> ?prefix:utf -> ?postfix:utf -> ?packing:pack -> ?cb:(int -> unit) -> ?target:int prod -> ?setter:int prod ->
	  lower:int -> upper:int -> ?step_incr:int -> ?page:int -> init:int -> unit -> (adjint, GEdit.spin_button txthbox) wpair

(* Checkbox or toggle button *)
val ebool: ?name:string -> ?prefix:utf -> ?label:utf -> ?postfix:utf -> packing:pack -> ?cb:(bool -> unit) -> ?target:bool prod -> ?setter:bool prod ->
	   [ `CHECK | `TOGGLE ] -> init:bool -> unit -> (bool simple_val, GButton.toggle_button) wpair
															   
(* A simple non-editable label showing a float (an int). *)
val lfloat: ?time_rate:float -> prefix:utf -> postfix:utf -> ?digits:int -> packing:pack -> float Events.prod -> GMisc.label
val lint: ?time_rate:float -> prefix:utf -> postfix:utf -> packing:pack -> int Events.prod -> GMisc.label

val llabel: ?time_rate:float -> ('a -> string) -> ?xalign:float -> ?yalign:float -> ?justify:Gtk.Tags.justification -> packing:pack -> 'a Events.prod -> GMisc.label

(* Editable slider (aka scale, range) 
 * Careful: if the slider is HORIZONTAL, it must be put in a vertical box with some minimal width.
 *          and conversely. *)
val eslider: ?name:string -> packing:pack -> ?cb:(float -> unit) -> ?target:float prod -> ?setter:float prod ->
	     ?digits:int -> lower:float -> upper:float -> ?draw_value:bool -> [ `HORIZONTAL | `VERTICAL ] -> init:float -> unit -> (adjfloat, GRange.scale) wpair
										      
(* Editable string
 * Style:
 *   `ENTRY  = editable entry
 *   `PASSWD = passwd entry *)
type string_evt = [`Value_Changed of string | `Submit ]
type string_evt_val = (string, string_evt) evt_val
										      
val estring: ?name:string -> ?prefix:utf -> ?postfix:utf -> packing:pack -> ?cb:(string_evt -> unit) -> ?target:string_evt prod -> ?setter:string prod ->
	     ?width_chars:int -> ?max_length:int -> ?has_frame:bool
	     -> style:[`ENTRY | `PASSWD] -> string -> (string_evt_val, GEdit.entry txthbox) wpair

(* One element taken in a combo list (or None if nothing is selected).
 * fail_notfound: raise Failure when trying to set_value with a non-existing element. Otherwise, just select none. *)
class ['a] combo_list: ?name:string -> ?fail_notfound:bool -> ?cmp:('a -> 'a -> bool) -> packing:pack ->
      ?cb:('a option -> unit) -> ?target:'a option prod -> ?setter: 'a option prod -> all:'a list -> v2s:('a -> string) -> ?init:'a -> unit ->
      object
	inherit ['a option] simple_val

	(* Get all elements in the list *)
	method get_all: 'a list
			    
	(* Removes all elements from the list. *)
	method clear: unit

        (* Adds a new choice at the end of the list. *)		 
	method add_element: 'a -> unit

	method combo: GEdit.combo_box
      end

(* Ditto, but encapsulated into a wpair presentation. *)	
val combo_list: ?name:string -> ?fail_notfound:bool -> ?cmp:('a -> 'a -> bool) -> packing:pack ->
		 ?cb:('a option -> unit) -> ?target:'a option prod -> ?setter: 'a option prod -> all:'a list ->
		 v2s:('a -> string) -> ?init:'a -> unit -> ('a combo_list, GEdit.combo_box) wpair

(* Choice list: radio buttons. 
 * @raise Failure when setting to a value not in the list. *)											    
val choice_list: ?name:string -> ?editable:bool -> ?style:Ggpack.col_style -> ?border_width:int -> ?bigpadding:int -> ?padding:int ->
    ?cmp:('a -> 'a -> bool) -> ?packing:pack -> ?cb:('a -> unit) -> ?target:'a prod -> ?setter: 'a prod ->
    all:'a array -> v2s:('a -> string) -> init:'a -> unit -> ('a simple_val, Ggpack.cols) wpair


(******** A button displaying a value. When clicked, popups a window used to edit the value. ************)

type 'a result =
  (* Well-formed value 'a *)
  | WellFormed of 'a

  (* Bad-formed value (a message explains). *)
  | BadFormed of string
											  
(* editval builds a widget allowing to edit the value. It returns a function (unit -> 'a result) that will be invoked to get the value just before
 * the container is destroyed. 
 * the packing argument can be invoked several times.
 *
 * parent: window that is disabled while the editval is opened. *)
class ['a] val_popup: ?parent:#window_skel -> ?name:string -> title:utf -> ?width:int -> ?height:int -> ?packing:pack -> val2s:('a -> string) ->
      ?apply_button:bool -> editval:(vbox:GPack.box -> 'a -> (unit -> 'a result)) ->
      ?target:'a prod -> ?cb:('a -> unit) -> ?setter:'a prod -> init:'a -> unit ->
      object
	inherit ['a] ref_val
	method popup: unit -> unit Lwt.t
	method button: GButton.button
      end

val val_popup: ?parent:#window_skel -> ?name:string -> title:utf -> ?width:int -> ?height:int -> ?packing:pack -> val2s:('a -> string) ->
	       ?apply_button:bool -> editval:(vbox:GPack.box -> 'a -> (unit -> 'a result)) ->
	       ?target:'a prod -> ?cb:('a -> unit) -> ?setter:'a prod -> init:'a -> unit -> ('a val_popup, GButton.button) wpair


open Ggimage
															   
(* One image widget that may change its display depending on the value it represents.				   
 * Show the first image which satisfies the predicate in assoc. *)			     
val show_image: ?dir:path Lwt.key -> ?packing:pack -> 'a prod -> assoc:(('a -> bool) * which_image) list -> ?default:which_image -> unit -> GMisc.image
													      
