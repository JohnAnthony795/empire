(*** Popup menu on left-click or right-click ***)

type title

val mk_title: ?weight:Pango.Tags.weight -> ?scale:float -> ?style:Pango.Tags.style ->
  ?fg:int*int*int -> ?family:string -> string -> title


(* Type of a menu, or submenu *)
type t

and menu_elt =
  | Title of title
  | Sep
  | Action of string * (unit -> unit)
  | Lab of string * menu_elt
  | Submenu of menu_elt * t 

(* If multiple labels are put on the same element, only the first label is kept. *)


val gmenu: t -> GMenu.menu

(* List of (label, items). Label is "" if undefined. *)
val menu_elements: t -> (string * GMenu.menu_item) list    

(* Enabled by default *)
val create_popup_menu: menu_elt list -> t

val attach_menu: GButton.button -> ?button:int -> t -> unit

(* enable / disable this menu (appears/does not appear on click) *)
val enable: t -> bool -> unit



(* Operations on menu elements.
 *   pos or label must be specified.
 *   pos is counted from 1. *)


val get_menu_item: t -> ?pos:int -> ?label:string -> unit -> GMenu.menu_item 

(************** Untested yet..... does it work as expected ? ****************)

(* Remove an element. Subsequent positions are decreased accordingly. *)
val remove: t -> ?pos:int -> ?label:string -> unit -> unit

(* If the new element in unlabeled, keeps the current label. 
 * Use Lab "" to remove the label. *)
val replace: t -> ?pos:int -> ?label:string -> menu_elt -> unit

(* Use pos:0 to insert at top.
 * Use pos:-1 to insert at bottom.
 * Subsequent positions are increased accordingly.  *)
val insert_after: t -> ?pos:int -> ?label:string -> menu_elt -> unit
  


