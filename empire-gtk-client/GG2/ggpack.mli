open Gg

(********* Convenience **********)
       
(* Hbox with space_left or space_right with space *)
val hbox: ?space_left:bool -> ?space_right:bool -> ?spacing:int -> ?packing:pack -> unit -> GPack.box

val notepack: GPack.notebook -> utf -> pack
       
(********* New containers **********)

(* Columns side by side. *)

(* Style: `Col nb_colonnes ou `NMax nb_max_elements_par_colonne *)
type col_style =
  (* Fixed number of columns. WARNING: Currently, only (Col 1) is implemented!!! *)
  | Col of int

  (* Maximal number of elements per column. New columns are added when needed. *)
  | Nmax of int

class cols: ?style:col_style -> ?border_width:int -> ?bigpadding:int -> ?padding:int -> ?packing:pack -> unit ->
    object

      (* Curiously, annotating with 'a widg does not work. *)			   
      method add: 'a . (<coerce : GObj.widget ; ..> as 'a) -> unit
      method mainbox: GPack.box
      method coerce: GObj.widget
    end

			   
