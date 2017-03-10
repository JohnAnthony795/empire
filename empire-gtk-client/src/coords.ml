open Common.Small

(* The center of the canvas is in (0,0). *)
let orthosys = Gcanvas.default_sys
		 
(* Maps hexagonal coordinates to orthosys coordinates.
 * Easy. We compute the coordinates of the tile center. *)
let ortho_of_hexa (dimx, dimy) (hx,hy) =
  (* Hexagonal coordinate relatives to the center tile *)
  let hx = foi (hx - dimx / 2)
  and hy = foi (hy - dimy / 2) in

  (* Maps to orthogonal coordinates 
   * Axis hx has polar coordinates (1:30) (upper right)
   * Axis hy has polar coordinated (1:-30) (lower right) *)
  ( (hx +. hy) *. 54.0, (hy -. hx) *. 36.0)

(* Maps orthosys coordinates to hexagonal coordinates.
 * Hard : it depends on the (hexagonal) shape of tiles.
 * We compute a first approximation (where tiles are considered as rhombuses.
 * Then, certains parts of the current rhombus actually belongs to an adjacent hexagon.
 *)
let hexa_of_ortho (dimx, dimy) (ox,oy) =

  (*** Rhombus approximation == compute the converse mapping of ortho_of_hexa. ***)
  
  (* ox =  (hx +. hy) *. 54.0
     oy =  (hy -. hx) *. 36.0 *)
  let oox = ox /. 54.0
  and ooy = oy /. 36.0 in

  let hy = (oox +. ooy) /. 2.0
  and hx = (oox -. ooy) /. 2.0 in

  (* Careful, the rounding made by int_of_float depends of the sign (to the left, or to the right). *)
  let hx = hx +. 0.5 +. foi (dimx / 2)
  and hy = hy +. 0.5 +. foi (dimy / 2) in

  (* ix, iy are the rhombux approximation coordinates. *)
  let ix = iof hx
  and iy = iof hy in

  (* Tile center *)
  let (chx, chy) = ortho_of_hexa (dimx, dimy) (ix,iy) in
  
  (* rx, ry are the ortho coordinates relative to the rhombus center. *)
  let rx = ox -. chx
  and ry = oy -. chy
  in
  
  (* Factorize using symetry. *)
  let (kx,rx) = if rx < 0.0 then (-1, -. rx) else (1, rx)
  and (ky,ry) = if ry < 0.0 then (-1, -. ry) else (1, ry)
  in

  (* Are we inside the tile ? 
   * Slope of an hexagon side : (18,36) *)
  if 2.0 *. rx +. ry > 72.0 then
    (* We are in an adjacent tile *)
    let dy = if ky > 0 then max kx 0 else min kx 0
    and dx = (kx - ky) / 2
    in
    (ix+dx, iy+dy)
      
  else (ix, iy)

(* Check that the given hexagonal coordinates are visible. *)
let check_visible icanv dims hxy =
  let xy = ortho_of_hexa dims hxy in
  if Gcanvas.is_visible ~sys:orthosys icanv xy then ()
  else Gcanvas.center_on ~sys:orthosys icanv xy
			 
      
