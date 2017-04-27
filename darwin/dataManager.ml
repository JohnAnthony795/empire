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

open Types

(** TAILLE MAP **)
let map_width = 45 ;; (* TODO à récupérer par requête *)

let map_height = 45 ;; (* TODO à récupérer par requête *)

let our_jid = 1 ;; (* TODO à récupérer par requête *)

let current_turn = 0 ;; (* TODO à récupérer par requête *)

(***** CARTES *****)

(* Carte du terrain (terrain ou ville ou autre) *)
type terrain = Ground | Water | Ally | Ennemy | Neutral | Unknown  ;;

let map_terrain = Array.make_matrix map_width map_height Unknown ;;

let fill_terrain terrain q r =
  match terrain with 
  | "water" -> map_terrain.(q).(r) <- Water
  | "ground"-> map_terrain.(q).(r) <- Ground
  | "our_city" -> map_terrain.(q).(r) <- Ally
  | "their_city" -> map_terrain.(q).(r) <- Ennemy
  | "city" -> map_terrain.(q).(r) <- Neutral
  | _ -> failwith "erreur fill_terrain"

;;


(** Non pertinent ? **)

(* Carte ennemis 
   type ennemi = City | Unit | Other ;;

   let map_ennemi = Array.make_matrix map_width map_height Other ;;
*)

(***** LISTES *****)

(** Listes unités **)

(* Unités alliées *)
type unite_list = {q:int; r:int; pid : int ; unite_type : unites ; hp : int};;

let liste_unites = [];;

let update_unite_alliee q r pid unite_type hp = 
  let pid_is_not pid element = element.pid <> pid in
  {q=q; r=r ; pid=pid; unite_type=unite_type; hp=hp} :: (List.filter (pid_is_not pid) liste_unites) ;;

(* Unités ennemies *)

(* TODO : liste d'unites (pos, pid, type unité, hp? (pour battleship)) *)

(** Listes villes **)
(*Liste villes alliées*)

type (*ville*) allie = {q : int ;r : int ;cid : int ; prod : unites option; tours_restants : int} ;;

let liste_ville_alliee = ref ([]) ;;

(* a utiliser avec listealliee = add_alliee *)
let update_ville_allie q r cid = 
  let cid_is cid element = element.cid = cid in
  if List.exists (cid_is cid) !liste_ville_alliee then ()
  else liste_ville_alliee := {q =q ; r =r ; cid = cid ; prod = None ; tours_restants = -1} :: !liste_ville_alliee ;;

(* a utiliser avec listealliee = rm_alliee *)
let rec rm_allie rmcid =
  let pred id alpha = alpha.cid <> id in
  liste_ville_alliee := List.filter (pred rmcid) !liste_ville_alliee
;;  

(*TODO : set production ville alliée *)

(* Liste villes ennemies *)
type (*ville*) ennemi = {q : int ;r : int ;cid : int } ;;

let liste_ville_ennemie = ref([]) ;;

let add_ville_ennemi q r cid = 
  let cid_is cid element = element.cid = cid in
  if List.exists (cid_is cid) !liste_ville_ennemie then ()
  else liste_ville_ennemie := {q=q ; r=r ; cid=cid} :: !liste_ville_ennemie ;;

let rec rm_ennemi rmcid =
  let pred id alpha = alpha.cid <> id in
  liste_ville_ennemie := List.filter (pred rmcid) !liste_ville_ennemie
;;  

(* Liste ville neutres *)

(***** OUTILS *****)

(* permet de calculer la vraie distance  entre deux cases *)
let tiles_distance (qa, ra) (qb, rb) = (abs (qa - qb) + abs (qa + ra - qb - rb) + abs (ra - rb)) / 2
;;

(* TODO ajouter d'autres *)

(***** TRAITEMENT *****)
(*Traitement des informations*)
let traiter_set_visible args =
  let ios = int_of_string in
  match args with
  | [ q ; r ; terrain ; "none" ] -> fill_terrain terrain (ios q) (ios r)
  | [ q ; r ; terrain ; "city" ; cid ] -> fill_terrain "city" (ios q) (ios r) 
  | [ q ; r ; terrain ; "owned_city" ; cid ; jid ] -> if (ios jid) = our_jid then (fill_terrain "our_city" (ios q) (ios r) ; update_ville_allie (ios q) (ios r) (ios cid)) else (fill_terrain "their_city" (ios q) (ios r) ; add_ville_ennemi (ios q) (ios r) (ios cid))
  | [ q ; r ; terrain ; "piece" ; jid ; pid ; ptid ; hits ] -> if (ios jid) = our_jid then (update_unite_alliee (ios q) (ios r) (ios pid) (ios ptid) (ios hp)) else (update_unite_ennemie (ios q) (ios r) (ios pid) (ios ptid) (ios hp))
  | _ -> failwith "erreur traiter_set_visible"
;;

let traiter_set_explored args = () 
let traiter_delete_piece args = ()
let traiter_create_piece args = ()
let traiter_move args = ()
let traiter_lose_city args = ()
let traiter_leave_terrain args = ()
let traiter_enter_city args = ()
let traiter_enter_piece args = ()
let traiter_leave_city args = ()
let traiter_leave_piece args = ()
let traiter_ok_invasion args = ()
let traiter_ko_invasion args = ()
let traiter_city_units_limit args = ()
let traiter_created_units_limit args = ();;

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





