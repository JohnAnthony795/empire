(* 2 possiblities : 
from 2 trees we select one random node on each, and then either
- we exchange the predicates
- we exchange the subtrees
In both case we have 2 trees as output
 *)
 
 open Types, TypesGen
 
 let eval = 0.25
 ;;
 
 let ratio = 
 Random.self_init();
 Random.float 1.0
 ;;
 
  let bool = 
   Random.self_init();
   (Random.bool)
 ;;
 
 let rec depth = function
| Leaf _ -> 0
| Node (a,b,c) -> 1 + max (depth a) (depth c)
 
 (* get_random_node : t_arbre -> Node of (t_arbre * t_predicat * t_arbre)  équivalent à t_arbre -> t_arbre *) 
 let rec get_random_node arbre = 
 match arbre with
 | Node (Leaf l1,b,Leaf l2) -> Node (Leaf l1,b,Leaf l2) (* 2 feuilles donc on retourne ce node *)
 | Node (Leaf l,b,c) -> if (ratio < eval) then Node(Leaf l,b,c) else get_random_node c (*1/2 on retourne cette node sinon on poursuit*)
 | Node (a,b,Leaf l) -> if(ratio < eval) then Node(a,b,Leaf l) else get_random_node a (*1/2 on retourne cette node sinon on poursuit*)
 | Node (a,b,c) -> if (ratio < eval) then Node (a,b,c) else (if (bool) then get_random_node a else get_random_node c) (*1/2 on retourne cette node sinon 1/2 sur chaque branche*)
 | Leaf _ -> failwith ("rip");
 ;;
 
 let rec replace pred sub tree =

  if pred tree then sub
  else match tree with
    | Leaf _ -> tree
    | Node (a,b,c) -> Node (replace pred sub a, b,replace pred sub c)

let nodequal node1 node2 =
node1 = node2 
;;

(*cross_subtree : t_arbre -> t_arbre -> t_arbre * t_arbre retourne deux arbres*)
let cross_subtree arbre1 arbre2 =
  let node1 = get_random_node arbre1 in
  let node2 = get_random_node arbre2 in
  (replace (nodequal node1) node2 arbre1 , replace (nodequal node2) node1 arbre2)
;;

(*cross_subtree_uniq : t_arbre -> t_arbre -> t_arbre  retourne un arbre unique *)
let cross_subtree_uniq arbre1 arbre2 =
  let node1 = get_random_node arbre1 in
  let node2 = get_random_node arbre2 in
  if (bool) then replace (nodequal node1) node2 arbre1 else replace (nodequal node2) node1 arbre2
;;


(* cross_foret : t_foret -> t_foret *) (* /!\ a changer en dur si la foret grandit *)
let cross_foret foret1 foret2 = 
let (a11,a12,a13,a14,a15,a16) = foret1   in
let (a21,a22,a23,a24,a25,a26) = foret2   in
(cross_subtree_uniq a11 a21,cross_subtree_uniq a12 a22,cross_subtree_uniq a13 a23,cross_subtree_uniq a14 a24,cross_subtree_uniq a15 a25,cross_subtree_uniq a16 a26) (* /!\ a changer en dur si la foret grandit *)

;;
(* main_cross : t_population -> t_population retourne la population croisée *)
let rec main_cross pop =
match pop with
 |(foret1,score1)::(foret2,score2)::Tl-> (cross_foret foret1 foret2 , -1) :: main_cross Tl  
|[] -> []
| _ -> failwith("weird list to cross")
;;

