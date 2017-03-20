(*
Définition des types customs utilisés:    
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)

Définir des fonctions d'affichage des types.
Il semble que les types doivent avoir des majuscules.
*)

(*#directory "../empire-server/_build/sources/"*)
(*open Ebase (*... fichier avec les types*) *)

open Printf;;

type direction = Up | Down | Right | Left | Upleft | Downright ;; (* 0 | 1 | 2 | 3 | 4 | 5;;*) (* 2|1|
						   3| |0
					            |4|5 *)
type piece_id= int;;

type city_id= int;;

type piece_type_id= int;;

type move = piece_id * direction ;;  (* si pas déjà défini*)

type set_city_production = 
	city_id * piece_type_id
	;;  (*définition dans Server/Empire.ml*)

type t_action = 
	 Move of move 
	|Set_city_prod of set_city_production 
	|End_turn 
	;;

type comparateur = Inf | Sup | Eq | InfEq | SupEq;;

type unites = ARMY | TRANSPORT | FIGHT | BATTLESHIP | PATROL ;;

type t_predicat= (* rajouter des prédicats en masse*)
	 Nb_unite_allie_proche of (unites * int * comparateur)
	|Nb_ville_allie_proche of (int * comparateur)
	;;

type t_arbre = 
	Leaf of t_action 
	|Node of (t_arbre * t_predicat * t_arbre)
	;;

(*type arbre_de =
	 Army_tree of t_arbre
	|Patrol_tree of t_arbre
	;;                     A VOIR*)

type t_foret =  t_arbre * t_arbre * t_arbre * t_arbre * t_arbre * t_arbre ;; (* 5 uniés , ville , (stratégie ?) *)


(* Fonction qui renvoie un arbre basique *)

let arbre0 = Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Leaf End_turn));;

(*print arbre*)
let pred_to_string p = "predicat";;
let print_action a d = printf "%s action" (String.make d ' ');;
let print_pred p depth =
	printf "%d:%s\n" depth (String.make depth ' ' ^ (pred_to_string p)) ;;
let rec print_tree t depth =
	match t with
		Leaf a -> print_action a depth
	       |Node (t1,p,t2) -> (print_pred p (depth); print_tree t1 (depth+1) ;print_tree t2 (depth+1)) ;;



