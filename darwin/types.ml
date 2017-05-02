(*
Définition des types customs utilisés:
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)

Définir des fonctions d'affichage des types.
Il semble que les types doivent avoir des majuscules.
*)

(*#directory "../empire-server/_build/sources/"*)
(*open Ebase (*... fichier avec les types*) *)

open Printf;;

type direction = Up | Down | Right | Left | Upleft | Downright ;; (* 0 | 1 | 2 | 3 | 4 | 5;;*)
(* 2|1|
   						   3| |0
   					        |4|5 *)
type piece_id= int;;

type city_id= int;;

type piece_type_id= int;;

type unites = ARMY | TRANSPORT | FIGHT | BATTLESHIP | PATROL ;;

type move = piece_id * direction ;;  (* si pas déjà défini*)

type set_city_production =
  city_id * unites
;;  (*définition dans Server/Empire.ml*)

type t_action =
  | Move of move
  | Set_city_prod of set_city_production
  | End_turn
;;
(* TODO : rajouter l'action pass pour qu'une unité ne joue pas immédiatement *)

type comparateur = Inf | Sup | Eq | InfEq | SupEq;;

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

type t_foret =  t_arbre * t_arbre * t_arbre * t_arbre * t_arbre * t_arbre ;; (* 5 unités , ville , (stratégie ?) *)


(* Fonction qui renvoie un arbre basique *)

let arbre0 = Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Leaf End_turn));;

(*print arbre   ### A PIMPER ###*)
(*EN particulier compléter prédicats_tostring  + les sous-call*)
(*Note: Point positif, les arbres sont bien verbeux. Par contre cela pose t'il des problèmes des parsing dans les fichiers?
  	Peut-etre creer une fonction "code_arbre" similaire mais avec des formats genre  INSTR:val1:val2   par exemple ET pour End_Turn  ou NBUAP:A|5|<  pour "moins de 5 ARMY aliée proche" *)

(*fonctions de print verbeux des types:*)

let comparateur_to_string c = match c with
    Inf -> "<"
  | Sup -> ">"
  | Eq -> "="
  | InfEq -> "<="
  | SupEq -> ">="
;;

let unite_to_string u = match u with
    ARMY -> "ARMY"
  | TRANSPORT -> "TRANSPORT"
  | FIGHT -> "FIGHT"
  | BATTLESHIP -> "BATTLESHIP"
  | PATROL -> "PATROL"
;;

let direction_to_string d = match d with
    Up -> "UP"
  | Down -> "DOWN"
  | Right -> "RIGHT"
  | Left -> "LEFT"
  | Upleft -> "UPLEFT"
  | Downright -> "DOWNRIGHT" (*darius*)
;;


(*fonctions publiques de print/string*)
let pred_to_string p = match p with   (*tenir à jour avec les prédicats ;)*)
    Nb_unite_allie_proche (u,n,c) -> "Nb de "^ (unite_to_string u) ^ " allies proche " ^ (comparateur_to_string c) ^ " " ^ (string_of_int n) ^ "?"
  |Nb_ville_allie_proche (n,c) -> "Nb de villes allies proche " ^ (comparateur_to_string c) ^ " " ^ (string_of_int n) ^ "?"
  |_ -> "predicat"
;;
let action_to_string a = match a with
    Move (id,dir) -> "Move id°" ^ (string_of_int id) ^ " " ^ (direction_to_string dir)
  |Set_city_prod (id,p_type) -> "Set_city_prod id°" ^ (string_of_int id) ^ " " ^ (unite_to_string p_type) (*clarifier le piece type id avec un piece type id to string??*)
  |End_turn -> "End_turn"
  |_ -> "action"
;;


(*variantes de print moins verbeuses*)
let unite_to_code u = match u with
    ARMY -> "A"
  | TRANSPORT -> "T"
  | FIGHT -> "F"
  | BATTLESHIP -> "B"
  | PATROL -> "P"
;;

let direction_to_code d = match d with
    Up -> "U"
  | Down -> "D"
  | Right -> "R"
  | Left -> "L"
  | Upleft -> "UL"
  | Downright -> "DR" 
;;

let pred_to_code p = match p with   (*tenir à jour avec les prédicats ;)  STANDARD : mettre un '?' au debut *)
    Nb_unite_allie_proche (u,n,c) -> "?NBUAP:" ^ (unite_to_code u) ^ ":" ^ (string_of_int n) ^ ":" ^ (comparateur_to_string c) 
  |Nb_ville_allie_proche (n,c) -> "?NBVAP:" ^ (string_of_int n) ^ ":" ^ (comparateur_to_string c) 
  |_ -> "predicat"
;;

let action_to_code a = match a with (* tenir a jour aussi  STANDARD : mettre un '!' au debut *)
    Move (id,dir) -> "!MV:" ^ (string_of_int id) ^ ":" ^ (direction_to_code dir)
  |Set_city_prod (id,p_type) -> "!SCP:°" ^ (string_of_int id) ^ ":" ^ (unite_to_code p_type) (*clarifier le piece type id avec un piece type id to string??*)
  |End_turn -> "!ET"
  |_ -> "action"
;;

(*fonctions principales*)
let print_action a d = printf "%s\n" ((String.make d '~') ^ (String.make d '~') ^ (action_to_string a));; (*moche mais on peut mettre que un carac*)
let print_pred p depth =
  printf "%s\n" ((String.make depth '~') ^ (String.make depth '~') ^ (pred_to_string p)) ;;
let rec print_tree t depth =
  match t with
    Leaf a -> print_action a depth
  |Node (t1,p,t2) -> (print_tree t1 (depth+1) ; print_pred p (depth);print_tree t2 (depth+1)) ;;
