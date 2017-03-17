(*
Définition des types customs utilisés:    
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)

Définir des fonctions d'affichage des types.
Il semble que les types doivent avoir des majuscules.
*)

directory "../empire-server/_build/sources/"
open Empire ;;   (*... fichier avec les types*)

type direction = Up | Down | Right | Left | Upleft | Downright ;; (* 0 | 1 | 2 | 3 | 4 | 5;;*) (* 2|1|
						   3| |0
					            |4|5 *)
type move = piece_id * direction ;;  (* si pas déjà défini*)

type set_city_production = 
	city_id * piece_type_id
	;;  (*définition dans Server/Empire.ml*)

type t_action = 
	 (*move 
	|set_city_production 
	|*)End_turn 
	;;

type t_predicat= (* rajouter des prédicats en masse*)
	 uniteProche * int * comparateur
	|int * comparateur * int 
	|float * comparateur * float
	|int *  

type t_arbre = 
	Leaf of t_action 
	|Node of (t_arbre * t_predicat * t_arbre)
	;;



(* Fonction qui renvoie un arbre basique *)

let arbre0 = Leaf of end_turn;;

(*print arbre*)
let pred_to_string p = "predicat"
let print_action a = "action"
let print_pred p depth =
	printf "%d:%s\n" depth ((String.make depth ' ' ^ (pred_to_string p)) 
let rec print_tree t depth =
	match t with
		Leaf a -> print_action a
	       |Node (t1,p,t2) -> print_pred p (depth);print_tree t1 (depth+1) ;print_tree t2 (depth+1);;


print_tree arbre0 0;;


