(*
Quand on découvre une ville ennemie, a-t-on l'information sur ce qu'elle contient ? L'a-t-on en temps réel ?

Contient des fonction de manipulation et lecture de données (en mémoire)

Données stockées:
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

let traiter_set_visible args =
	match args with
	| [ q ; r ; terrain ; "none" ] -> 
    | [ q ; r ; terrain ; "city" ; cid ] ->
    | [ q ; r ; terrain ; "owned_city" ; cid ; jid ] ->
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
