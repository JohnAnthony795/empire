open Gcanvas
open Ebase

type gintf =
  { config: Connect.config ;

    gstate: Emap.game_state ;
    
    (* Current selection. *)
    select_pos: hxy option Evt_val.simple_val ;
    
    (* Information to be shown. select_pos events must be mapped to this. 
     * None means the up-to-date info has not arrived yet. *)
    current_info: Emap.info option Evt_val.simple_val ;

    (* Popup menu on right-click. Uses the current selection. *)
    popup: canvas_click Events.prod ;
    
    (* Add an item on the canvas (selection frame).
     * xyprod is a producer that moves the item.
     * (int*int): initial position *)
    add_visual: xyprod:xy Events.prod -> GdkPixbuf.pixbuf -> (int*int) -> GnoCanvas.pixbuf g_item ;

    (* packing function for displaying tile information. *)
    pack: Gg.pack ;
  }

(* Install the selection handler *)    
val handle_selection: gintf -> unit
				 
