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

type t_action = (*pass*)
  | Move of move
  | Set_city_prod of set_city_production
  | End_turn

(* TODO : rajouter l'action pass pour qu'une unité ne joue pas immédiatement *)

type comparateur = Inf | Sup | Eq | InfEq | SupEq;;

type t_predicat= (* rajouter des prédicats en masse*)
  | Nb_unite_allie_proche of ( int *  unites * int * comparateur) (* distance proximité/ type unité /Nb unités/ plus ou moins de Nb unités proche *)
  | Nb_ville_allie_proche of (int  * int * comparateur) (*distance proximité/Nb ville/plus ou moins de Nb ville allie proche*)
  | Nb_ville_ennemie_proche of (int  * int * comparateur) (*distance proximité/Nb ville/plus ou moins de Nb ville ennemie proche*)
  | Littoral_adjacent (* presence de littoral dans une case adjacente*)
  | Transport (*présence de l'unité dans un transport*)
  | Fog_proche of (int) (* distance proximité / plus ou moins loin *)

type t_arbre =
  | Leaf of t_action
  | Node of (t_arbre * t_predicat * t_arbre)

type t_foret =  t_arbre * t_arbre * t_arbre * t_arbre * t_arbre * t_arbre ;; (* 5 unités , ville , (stratégie ?) *)


(*fonctions de print verbeux des types:*)

let comparateur_to_string c = match c with
  | Inf -> "<"
  | Sup -> ">"
  | Eq -> "="
  | InfEq -> "<="
  | SupEq -> ">="

let unite_to_string u = match u with
  | ARMY -> "ARMY"
  | TRANSPORT -> "TRANSPORT"
  | FIGHT -> "FIGHT"
  | BATTLESHIP -> "BATTLESHIP"
  | PATROL -> "PATROL"

let direction_to_string d = match d with
  | Up -> "UP"
  | Down -> "DOWN"
  | Right -> "RIGHT"
  | Left -> "LEFT"
  | Upleft -> "UPLEFT"
  | Downright -> "DOWNRIGHT" (*darius*)

(*fonctions publiques de print/string*)
let pred_to_string p = match p with   (*tenir à jour avec les prédicats *)
  | Nb_unite_allie_proche (d,u,n,c) -> "Nb de "^ (unite_to_string u) ^ " allies proches d'une distance de" ^ (string_of_int d) ^ " " ^ (comparateur_to_string c) ^ " " ^ (string_of_int n) ^ "?"
  | Nb_ville_allie_proche (d,n,c) -> "Nb de villes allies proches d'une distance de " ^  (string_of_int d) ^ " " ^ (comparateur_to_string c) ^ " " ^ (string_of_int n) ^ "?"
  | Nb_ville_ennemie_proche (d,n,c) -> "Nb de villes ennemies proches d'une distance de "  ^ (string_of_int d) ^ " " ^ (comparateur_to_string c) ^ " " ^ (string_of_int n) ^ "?"
  | Littoral_adjacent -> "Presence de littoral dans une case adjacente ?"
  | Transport -> "Présence de l'unité dans un transport ?"
  | Fog_proche d -> "Présence du brouillard de guerre proche d'une distance de" ^ (string_of_int d) ^"?"


let action_to_string a = match a with
  | Move (id,dir) -> "Move id°" ^ (string_of_int id) ^ " " ^ (direction_to_string dir)
  | Set_city_prod (id,p_type) -> "Set_city_prod id°" ^ (string_of_int id) ^ " " ^ (unite_to_string p_type) (*clarifier le piece type id avec un piece type id to string??*)
  | End_turn -> "End_turn"

(*variantes de print moins verbeuses*)
let unite_to_code u = match u with
  | ARMY -> "A"
  | TRANSPORT -> "T"
  | FIGHT -> "F"
  | BATTLESHIP -> "B"
  | PATROL -> "P"

let direction_to_code d = match d with
  | Up -> "U"
  | Down -> "D"
  | Right -> "R"
  | Left -> "L"
  | Upleft -> "UL"
  | Downright -> "DR" 

let pred_to_code p = match p with   (*tenir à jour avec les prédicats ;)  STANDARD : mettre un '?' au debut *)
  | Nb_unite_allie_proche (d,u,n,c) -> "?NBUAP:" ^ (string_of_int d) ^ ":" ^ (unite_to_code u) ^ ":" ^ (string_of_int n) ^ ":" ^ (comparateur_to_string c) 
  | Nb_ville_allie_proche (d,n,c) -> "?NBVAP:" ^ (string_of_int d) ^ ":" ^ (string_of_int n) ^ ":" ^ (comparateur_to_string c) 
  | Nb_ville_ennemie_proche (d,n,c) -> "?NBVEP:" ^ (string_of_int d) ^ ":" ^ (string_of_int n) ^ ":" ^ (comparateur_to_string c) 
  | Littoral_adjacent -> "?LIADJ" 
  | Transport -> "?TR"
  | Fog_proche (d) -> "?FOG:" ^ (string_of_int d)


let action_to_code a = match a with (* tenir a jour aussi  STANDARD : mettre un '!' au debut *)
  | Move (id,dir) -> "!MV:" ^ (string_of_int id) ^ ":" ^ (direction_to_code dir)
  | Set_city_prod (id,p_type) -> "!SCP:°" ^ (string_of_int id) ^ ":" ^ (unite_to_code p_type) (*clarifier le piece type id avec un piece type id to string??*)
  | End_turn -> "!ET"

(*fonctions principales*)
let print_action a d = printf "%s\n" ((String.make d '~') ^ (String.make d '~') ^ (action_to_string a));; (*moche mais on peut mettre que un carac*)
let print_pred p depth =
  printf "%s\n" ((String.make depth '~') ^ (String.make depth '~') ^ (pred_to_string p)) ;;
let rec print_tree t depth =
  match t with
    Leaf a -> print_action a depth
  |Node (t1,p,t2) -> (print_tree t1 (depth+1) ; print_pred p (depth);print_tree t2 (depth+1)) ;;
