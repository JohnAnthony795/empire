
type degree = float
type radian = float
		       
val pi: radian

(* Conversions between degrees and radians *)
val deg2rad: degree -> radian
val rad2deg: radian -> degree

(* Trigonometric functions taking degrees as argument. *)			
val degtan: degree -> float
val degsin: degree -> float
val degcos: degree -> float

(* Operation on pairs. *)
type fpair = float * float
val (++): fpair -> fpair -> fpair
val (--): fpair -> fpair -> fpair
val ( ** ): float -> fpair -> fpair

(* Squared norm. *)				
val norm2: fpair -> float
		      
val norm: fpair -> float

(* Squared distance *)		     
val dist2: fpair -> fpair -> float


(* Normed angle in degrees: return a value in [0, 360[ *)			       
val normed_angle: float -> float

(* Normed angle difference: returns a value in ]-180, 180] *)			     
val delta_angle: float -> float -> float

(* Unit vector, given an angle in degrees. *)
val unit_an: float -> fpair

(*** Build float array compatible with the 'affine' arrays of GnomeCanvas. ***)
type affine = float array

(* Unlike gnome canvas, y goes upwards. *)		    
val translation: dx:float -> dy:float -> affine

(* Angle in degrees by default. Trigonometric orientation. 
 * You may indicate a translation which is done after the rotation. 
 *)
val rotation: ?rad:bool -> ?dx:float -> ?dy:float -> float -> affine

(* Compose A B means: A, then B (that is, B o A). *)					    
val compose: affine -> affine -> affine
val compose3: affine -> affine -> affine -> affine
					      
				     
