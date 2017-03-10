
(* Converts a size in bytes into a readable string, with the appropriate unit *)
val show_size: int -> string

(* Find homedir (/tmp if it fails). *)
val find_homedir: unit -> string
			    
(* FIXME : check that it exists. *)
