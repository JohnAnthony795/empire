(*
Quand on découvre une ville ennemie, a-t-on l'information sur ce qu'elle contient ? L'a-t-on en temps réel ?

Contient des fonction de manipulation et lecture de données (en mémoire)

Données stockées:
taille à récupérer via get_width get_height
-Carte du terrain
-Carte des ennemis (villes ennemies / unités ennemis ( +suppositions?))

-Tableau listes d'unités (piece * Coordonnees)
-Tableau liste villes alliées (city)
-Tableau liste villes neutres (city_id, coords, visible ou non, plus visible depuis x tours)
-Tableau liste villes ennemies (city_id, coords)


Fonctions à develloper:

publiques :
- traiter_set_visible

privées :
-Set_tile: Coordonnees * type -> unit()  // maj de la map case par case 
-Set_city:
-Manipulation des tableaux d'unte
	-Ajouter_unite: piece -> Coordonnes -> unit()
	-Update_unite : piece_id -> unit() //modifie les points de vie ou le mouvement ou autre
	-Retirer_unite: piece_id -> unit() //retire une unité du tableau
-Manipulation des tableaux de ville
	-Capturer_ville
	-Perdre_ville
	-Update_ville //depuis combien de tours on ne la voit plus
- Update_presence : unit() -> unit() //estime les positions ennemies
- 

*)
let map_width = 45 ;; (* TODO à récupérer par requête *)

let map_height = 45 ;; (* TODO à récupérer par requête *)

let our_jid = 1 ;; (* TODO à récupérer par requête *)

let current_turn = 0 ;; (* TODO à récupérer par requête *)

(* Carte du terrain (terrain ou ville ou autre) *)
let type terrain = Ground | Water | Ally | Ennemy | Neutral | Unknown  ;;

let map_terrain = Array.make_matrix map_width map_height Unknown ;;

let fill_terrain terrain q r =
match terrain with 
| "water" -> map_terrain.(q).(r) <- Water
| "ground"-> map_terrain.(q).(r) <- Ground
| "our_city" -> map_terrain.(q).(r) <- Ally
| "their_city" -> map_terrain.(q).(r) <- Ennemy
| "city" -> map_terrain.(q).(r) <- Neutral
;;

(* Carte ennemis *)
let type ennemi = City | Unit | Other ;;

let map_ennemi = Array.make_matrix map_width map_height Other ;;

(*Liste unités *)


(*Liste villes alliées*)

type unite = Army | Fight | Battleship | Patrol | Transport | None ;;

type allie = {q : int ;r : int ;cid : int ; prod : unite ; tours_restants : int} ;;

let liste_allie = [] ;;

(* a utiliser avec listealliee = add_alliee *)
let add_ville_allie q r cid = 
	let cid_is cid element = element.cid == cid in
	if List.exists (cid_is cid) liste_allie then liste_allie
	else liste_allie :: { q =q ; r =r ; cid = cid ; prod = None ; tours_restants = -1}
 ;;

(* a utiliser avec listealliee = rm_alliee *)
let rec rm_allie rmcid =
let pred id alpha = alpha.cid != id in
List.filter (pred rmcid) liste_allie
;;  

(* Liste villes ennemies *)

(* TODO 
	add, remove, structure de données *)

(*Traitement des informations*)
let traiter_set_visible args =
	match args with
	| [ q ; r ; terrain ; "none" ] -> fill_terrain terrain map_terrain q r
    | [ q ; r ; terrain ; "city" ; cid ] -> fill_terrain "city" q r 
    | [ q ; r ; terrain ; "owned_city" ; cid ; jid ] -> if jid = our_jid then (fill_terrain "our_city" q r ; add_ville_allie q r cid) else (fill_terrain "their_city" q r ; add_ville_ennemy q r cid)
    | [ q ; r ; terrain ; "piece" ; jid ; pid ; ptid ; hits ] -> 

(* TODO : 
    | "set_explored" -> traiter_set_explored tlMsg
    |  "delete_piece" -> traiter_delete_piece tlMsg
    |  "create_piece" -> traiter_create_piece tlMsg
    |  "move" -> traiter_move tlMsg
    |  "lose_city" -> traiter_lose_city tlMsg
    |  "leave_terrain" -> traiter_leave_terrain tlMsg
    |  "enter_city" -> traiter_enter_city tlMsg
    |  "enter_piece" -> traiter_enter_piece tlMsg
    |  "leave_city" -> traiter_leave_city tlMsg
    |  "leave_piece" -> traiter_leave_piece tlMsg
    |  "ok-invasion" -> traiter_ok-invasion tlMsg
    |  "ko-invasion" -> traiter_ko-invasion tlMsg
    |  "city-units-limit" -> traiter_city-units-limit tlMsg
    |  "created-units-limit" -> traiter_created-units-limit tlMsg


*)
