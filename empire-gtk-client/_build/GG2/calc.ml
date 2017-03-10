type degree = float
type radian = float
type fpair = float * float

let square x = x *. x

let pi = acos (-1.0)
let onedeg = 2.0 *. pi /. 360.0

let deg2rad a = a /. 180. *. pi
let rad2deg a = a *. 180.0 /. pi

let degtan x = tan (deg2rad x)
let degsin x = sin (deg2rad x)
let degcos x = cos (deg2rad x)

let (++) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let (--) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let ( ** ) k (x, y) = (k *. x, k *. y)

let unit_an an = (degcos an, -. degsin an)

let norm2 (x, y) = (square x) +. (square y)
let norm  v = sqrt (norm2 v)
let dist2 (x1, y1) (x2, y2) = norm2 (x2 -. x1, y2 -. y1)

let normed_angle angle =
  let angle = mod_float angle 360.0 in
  if angle < 0.0 then angle +. 360.0
  else angle

let delta_angle org dest =
  let delta = normed_angle (dest -. org) in
  if delta > 180.0 then delta -. 360.0 else delta


type affine = float array

(* The array is read as follows:
 *   [| a ; b ; c ; d ; e ; f |]
 *
 * means
 *
 *   X' = AX + B
 *
 * where B is (e
 *             f)
 *
 * and A is (a c
 *           b d)
 *)
		    
let translation ~dx ~dy = [| 1. ; 0. ; 0. ; 1. ; dx ; ~-.dy |]

let rotation ?(rad=false) ?(dx=0.0) ?(dy=0.0) angle =
  let rangle = if rad then angle else deg2rad angle in
  let cos_a = cos rangle in
  let sin_a = sin rangle in
  [| cos_a ; ~-.sin_a ; sin_a ; cos_a ; dx ; (*~-.*) dy |]
    
let compose a1 a2 =
  [| a1.(0) *. a2.(0) +. a1.(1) *. a2.(2) ;
     a1.(0) *. a2.(1) +. a1.(1) *. a2.(3) ;
     a1.(2) *. a2.(0) +. a1.(3) *. a2.(2) ;
     a1.(2) *. a2.(1) +. a1.(3) *. a2.(3) ;
     a1.(4) *. a2.(0) +. a1.(5) *. a2.(2) +. a2.(4) ;
     a1.(4) *. a2.(1) +. a1.(5) *. a2.(3) +. a2.(5) ; |]
    
let compose3 a1 a2 a3 = compose (compose a1 a2) a3

