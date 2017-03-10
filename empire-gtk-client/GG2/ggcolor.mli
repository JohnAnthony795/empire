
(* Color (values in 0..255) *)			
val rgb: int -> int -> int -> [> `RGB of int*int*int]

(* and #ff is done on values *)				
val rgba: int -> int -> int -> int -> int32


val mk_color: string -> Gdk.color
val mk_rgb: int -> int -> int -> Gdk.color
			  
val white: Gdk.color Lazy.t
val black: Gdk.color Lazy.t
		     
	     
