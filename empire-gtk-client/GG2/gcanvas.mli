open Events


type xy = float * float
       
(* Coordinate system *)
type coord_sys


(* The default coordinate system: 
 * y goes downward
 * trigonometric orientation, angles in degrees *)
val default_sys: coord_sys

(* Build a new coordinate system relatively to another coordinate system ('old', default_sys by default). 
 * You must provide the mapping of two points, by giving their coordinates in both system.
 * Coordinates are called: o1 (old system, point 1), n2 (new system, point 2) ...
 * You may also give a function mapping angles in the new system to angles in the old system.
 * The new coordinate system will preserve shape ratios (that is, a square remains a square, the mapping is a translation, rotation, & dilatation).
 * Note: the distance between the two new points should be significant (approximations or division by zero otherwise).
 * *)
val mk_sys: ?old_sys:coord_sys -> o1:xy -> n1:xy -> o2:xy -> n2:xy ->
	    ?map_angle:(float -> float) -> unit -> coord_sys
  
(* An object on the canvas *)		   
type 'a g_item constraint 'a = _ GnoCanvas.item 

val get_item: 'a g_item -> 'a
				 
(*
 * x,y: initial position
 * ang: initial angle
 * ?xyprod: a producer setting positions
 * ?aprod: producer setting the angle
 *
 *)
val mk_item: ?sys:coord_sys -> ?xyprod:xy prod -> ?aprod:float prod -> x:float -> y:float -> ang:float -> 'b -> 'b g_item

(* Set the item position. *)
val set_pos: 'a g_item -> x:float -> y:float -> unit

(* Set the item angle (the angle is normalized). *)
val set_angle: 'a g_item -> float -> unit

(* Show / Hide *)
val set_visible: 'a g_item -> bool -> unit

(* Getters *)
val get_pos: 'a g_item -> xy
			    
val get_angle: 'a g_item -> float

type canvas_click =
  { csys: coord_sys ;
    xpos: float ;
    ypos: float ;
    event: GdkEvent.Button.t }

type keys =
  | Up
  | Up_left
  | Up_right
  | Down
  | Down_left
  | Down_right
  | Left
  | Right
  | Char of string

type icanvas =
  { canvas: GnoCanvas.canvas ;
    
    (* Clicks on the canvas are produced on this event channel (using the default coordinate system). *)
    clicks: canvas_click prod ;

    (* Keystrokes are reported here, except arrows + move-canvas modifier if ?arrow_keys is set. *)
    keystrokes: GdkEvent.Key.t prod ;

    (* keystrokes mapped to 'keys' & modifier mask (int), see next. *)
    keys: (int * keys) prod }

(* Key modifiers *)
type modifiers = [ `SHIFT | `CONTROL | `META | `MOD1 ] list

val get_mask: modifiers -> int
						       
(* Creates a new canvas.
 * If an image is given, the canvas is bounded by the image dimensions.
 *
 * with_scroll: add a scrolled window -- the container should have a minimum size, otherwise the canvas will be too small (invisible). 
 *
 * arrow_keys + modifiers move the canvas (modifiers can be empty).
 * *)
val icanvas: ?with_scroll:bool -> ?arrow_keys:modifiers -> ?aa:bool -> ?border_width:int -> packing:Gg.pack -> ?pixb:GdkPixbuf.pixbuf -> unit -> icanvas

(* Set the canvas bounds as the smallest bounding box containing those points *)
val set_bb: icanvas -> ?sys:coord_sys -> (float*float) list -> unit

(* Zoom level in pcent. 200% = 2 times larger. *)
val set_zoom: icanvas -> int -> unit

(* Maps a click event channel to a different coordinate system .*)									     
val map_clicks: new_sys:coord_sys -> canvas_click prod -> canvas_click prod
								 
(* Check if the given point is visible 
 * do_center: center the canvas on this point if it is not visible. 
 * margin: see get_visible *)
val is_visible: ?do_center:bool -> ?sys:coord_sys -> ?margin:float -> icanvas -> float * float -> bool
													    
(* Center the canvas on this coordinate. *)													    
val center_on: ?sys:coord_sys -> icanvas -> float * float -> unit
													    
(* Returns the global/visible part of this canvas in the given coordinate system. *)
type frame =
  { y_max: float ;
    y_min: float ;
    x_max : float ;
    x_min : float }

val get_global: ?sys:coord_sys -> icanvas -> frame

(* Frame of visible portion of the canvas.
 * Include a margin if necessary to consider that objects next to the border are not visible. 
 * The margin is expressed in the sys coordinate system. *)
val get_visible: ?sys:coord_sys -> ?margin:float -> icanvas -> frame
						

