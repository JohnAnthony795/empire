
open Types

 (* fonction qui retourne la profondeur d'un arbre *)
 let rec depth = function
| Leaf _ -> 0
| Node (a,b,c) -> 1 + max (depth a) (depth c)

(* fonction qui remplace quand le prédicat est vrai*)
 let rec replace pred sub tree =
  if pred tree then sub
  else match tree with
       | Leaf _ -> tree
       | Node (a,b,c) -> Node (replace pred sub a, b,replace pred sub c)
;;

let read_population () = [(arbre0,arbre0,arbre0,arbre0,arbre0,arbre0), 10.0]

let write_population popu = ()

(** TODO **)
(* 

- read_population : unit -> t_population
- write_population : t_population -> unit

*)

