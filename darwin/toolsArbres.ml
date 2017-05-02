<<<<<<< HEAD
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
=======

(** TODO **)
(* 

- read_population : unit -> t_population
- write_population : t_population -> unit


*)


>>>>>>> 209e60a983e993d81ada1a9845392822818f5755
