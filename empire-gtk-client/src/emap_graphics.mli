open Ebase
open Emap

(*** Graphical part of emap ***)


(* Preload images *)
val init: unit -> unit Lwt.t

(* Returns the pixbuf corresponding to the given tile. 
 * We need the map dimensions & coordinates to estimate the latitude. 
 * We need the map information (info) to draw 'borders'. *)
val find_pixbuf: game_state -> hxy -> tile -> GdkPixbuf.pixbuf

(* Finds the decoration (forest) at this coordinates. *)
val decoration: game_state -> hxy -> (hxy * GdkPixbuf.pixbuf) list                  

type gitem = GnoCanvas.pixbuf Gcanvas.g_item

(* Stores gitems associated to items *)
type gr_table

(* Receives a function to create canvas items. *)
val new_table: game_state -> (item -> GdkPixbuf.pixbuf -> hxy -> gitem) -> gr_table

(* Creates or update the graphical item corresponding to item (make it is visible). *)
val update: gr_table -> item -> unit

val hide: gr_table -> item -> unit
