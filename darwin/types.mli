type direction = Up | Down | Right | Left | Upleft | Downright ;; (* 0 | 1 | 2 | 3 | 4 | 5;;*) (* 2|1|
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

type comparateur = Inf | Sup | Eq | InfEq | SupEq;;

type t_predicat= (* rajouter des prédicats en masse*)
    Nb_unite_allie_proche of (unites * int * comparateur)
  |Nb_ville_allie_proche of (int * comparateur)
;;

type t_arbre = 
    Leaf of t_action 
  |Node of (t_arbre * t_predicat * t_arbre)
;;

(* Fonction qui renvoie un arbre basique *)

val arbre0 : t_arbre

type t_foret =  t_arbre * t_arbre * t_arbre * t_arbre * t_arbre * t_arbre ;; (* 5 unités , ville , ( ajouter stratégie globale???) *)

(*print arbre*)
val pred_to_string : t_predicat -> string
val action_to_string : t_action -> string
val pred_to_code : t_predicat -> string
val action_to_code : t_action -> string

val print_action : t_action -> int -> unit
val print_pred : t_predicat -> int -> unit
val print_tree : t_arbre -> int -> unit



