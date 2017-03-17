(*
Définition des types customs utilisés:    
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)

Définir des fonctions d'affichage des types.
Il semble que les types doivent avoir des majuscules.
*)

open ../empire-server/Empire ;;   (*... fichier avec les types*)

type direction = Up | Down | Right | Left | Upleft | Downright ;; (* 0 | 1 | 2 | 3 | 4 | 5;;*) (* 2|1|
						   3| |0
					            |4|5 *)
type move: piece_id * direction ;;  (* si pas déjà défini*)

type set_city_production = 
	city_id * piece_type_id
	;;  (*définition dans Server/Empire.ml*)

type t_action = 
	 move 
	|set_city_production 
	|end_turn 
	;;

type t_predicat= (* rajouter des prédicats en masse*)
	 uniteProche * int * comparateur
	|int * comparateur * int 
	|float * comparateur * float
	|int *  

type t_arbre = 
	Leaf of t_action 
	|Node of (t_predicat * t_arbre * t_arbre)
	;;



(* Fonction qui renvoie un arbre basique *)

let Arbre0 = Leaf of end_turn;;
