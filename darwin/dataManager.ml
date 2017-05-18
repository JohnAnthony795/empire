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


Fonctions à développer:

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
let map_width = ref(44)

let map_height = ref(44)

let our_jid = ref(0)

let current_turn = ref(0)

(***** SETTERS *****)

let set_our_jid = function
  | [new_jid] -> our_jid := int_of_string new_jid
  | _ -> failwith "erreur set_our_jid"

let set_map_width = function
  | [new_width] -> map_width := int_of_string new_width
  | _ -> failwith "erreur set_map_width"

let set_map_height = function
  | [new_height] -> map_height := int_of_string new_height
  | _ -> failwith "erreur set_map_height"
  
let increment_turn_counter () =
	current_turn := !current_turn + 1

(***** OUTILS *****)

(* permet de calculer la vraie distance  entre deux cases *)
let tiles_distance (qa, ra) (qb, rb) = (abs (qa - qb) + abs (qa + ra - qb - rb) + abs (ra - rb)) / 2

let ptid_to_unites ptid = match ptid with
  | 0 -> ARMY
  | 1 -> FIGHT
  | 2 -> TRANSPORT
  | 3 -> PATROL
  | 4 -> BATTLESHIP
  | _ -> failwith "Erreur ptid_to_unites : entrée non gérée"

(* TODO : récupérer infos dynamiquement *)
(* renvoie la portee de deplacement d'un type d'unité *)
let ptid_to_move ptid =
  match ptid with
  | 0 -> 1
  | 1 -> 8
  | 2 -> 2
  | 3 -> 4
  | 4 -> 2 
  | _ -> failwith "Erreur ptid_to_move : entrée non gérée"

(* TODO : récupérer infos dynamiquement *)
let unite_to_productionTime unite_type = match unite_type with
  | ARMY -> 5
  | FIGHT -> 10
  | TRANSPORT -> 30
  | PATROL -> 15
  | BATTLESHIP -> 40

(***** CARTES *****)

(* Carte du terrain (terrain ou ville ou autre) *)
type terrain = Ground | Water | Ally | Ennemy | Neutral | Unknown

let map_terrain = Array.make_matrix !map_width !map_height Unknown

let fill_terrain terrain q r =
  match terrain with 
  | "water" -> map_terrain.(q).(r) <- Water
  | "ground"-> map_terrain.(q).(r) <- Ground
  | "our_city" -> map_terrain.(q).(r) <- Ally
  | "their_city" -> map_terrain.(q).(r) <- Ennemy
  | "city" -> map_terrain.(q).(r) <- Neutral
  | _ -> failwith "erreur fill_terrain"

let terrain_is terrain_type q r =
	if (q<0 || q> !map_width || r<0 || r> !map_height) then false
	else map_terrain.(q).(r) = terrain_type

(***** LISTES *****)

(** Listes unités **)

(* Unités alliées *)
type unite_list = {q:int; r:int; pid : int ; unite_type : Types.unites ; hp : int ; mov : int}

let liste_unites = ref([])

let update_unite_alliee q r pid unite_type hp mov = 
  let pid_is_not pid element = element.pid <> pid in
  liste_unites := {q=q; r=r ; pid=pid; unite_type=unite_type; hp=hp ; mov=mov} :: (List.filter (pid_is_not pid) !liste_unites)

(* Unités ennemies *)
type unite_ennemies_list = {q:int; r:int; pid : int ; unite_type : Types.unites ; hp : int}

let liste_ennemis = ref([])

let update_unite_ennemie q r pid unite_type hp = 
  let pid_is_not pid element = element.pid <> pid in
  liste_ennemis := {q=q; r=r ; pid=pid; unite_type=unite_type; hp=hp} :: (List.filter (pid_is_not pid) !liste_ennemis)

(** Listes villes **)
(*Liste villes alliées*)

type (*ville*) allie = {q : int ;r : int ;cid : int ; prod : unites option; tours_restants : int}

let liste_ville_alliee = ref ([]) ;;

(* USAGE: add_ville_allie q r cid; *)
let add_ville_allie q r cid = 
  let cid_is cid element = element.cid = cid in
  if List.exists (cid_is cid) !liste_ville_alliee then ()
  else liste_ville_alliee := {q =q ; r =r ; cid = cid ; prod = None ; tours_restants = -1} :: !liste_ville_alliee

(* USAGE: rm_ville_allie cid; *)
let rm_ville_allie rmcid =
  let cid_is_not id alpha = alpha.cid <> id in
  liste_ville_alliee := List.filter (cid_is_not rmcid) !liste_ville_alliee


(* set_city_production filtre la liste pour enlever la ville concernée puis la rajoute en modifiée *)
let set_city_production cid unite_type =
  let cid_is cid element = element.cid = cid in
  let cid_is_not cid element = element.cid <> cid in
  let ville = List.find (cid_is cid) !liste_ville_alliee in (* ville à update *)
  let autresVilles = List.filter (cid_is_not cid) !liste_ville_alliee in (* toutes les villes sauf celle à udpate *)
  let tours = unite_to_productionTime unite_type in (* nombre de tours de production *)

  liste_ville_alliee := {q= ville.q ; r= ville.r ; cid = cid ; prod = Some(unite_type) ; tours_restants = tours} :: autresVilles


(* Liste villes ennemies *)
type (*ville*) ennemi = {q : int ;r : int ;cid : int }

let liste_ville_ennemie = ref([])

let add_ville_ennemi q r cid = 
  let cid_is cid element = element.cid = cid in
  if List.exists (cid_is cid) !liste_ville_ennemie then ()
  else liste_ville_ennemie := {q=q ; r=r ; cid=cid} :: !liste_ville_ennemie

let rec rm_ennemi rmcid =
  let pred id alpha = alpha.cid <> id in
  liste_ville_ennemie := List.filter (pred rmcid) !liste_ville_ennemie

(* Liste ville neutres *)

(***** GETTERS *****)
(* permet de récupérer une unite à partir d'un pid *)
let get_unite pid =
  List.find (fun (element:unite_list) -> element.pid = pid) !liste_unites

(* Ajouté distance en parametre*)
let get_nb_unite_proche unites pid distance=
  let unite = get_unite pid in
  List.length (List.filter (fun (element:unite_list) -> ((element.pid <> pid) && (element.unite_type =unites) && ((tiles_distance (unite.q,unite.r) (element.q,element.r))<distance))) !liste_unites) ;;

let get_nb_ville_proche_allie pid distance =
  let unite = get_unite pid in
  List.length (List.filter (fun (element:allie) -> (tiles_distance (unite.q,unite.r) (element.q,element.r))<distance) !liste_ville_alliee) ;;

let get_nb_ville_proche_ennemi pid distance =
  let unite = get_unite pid in
  List.length (List.filter (fun (element:ennemi) -> (tiles_distance (unite.q,unite.r) (element.q,element.r))<distance) !liste_ville_ennemie) ;;


(* littoral dans une des 6 cases adjacentes *)
let littoral_adj pid =
  let unite = get_unite pid in
  let (q,r) = (unite.q, unite.r) in
  terrain_is Water (q+1) r || terrain_is Water (q+1) (r-1) || terrain_is Water q (r-1) || terrain_is Water (q-1) r || terrain_is Water (q-1) (r+1) || terrain_is Water q (r+1)

let a_gagne = ref(None)

let set_victoire msg =
  let msgHd = match msg with 
    | hd :: tail -> hd
    | [] -> failwith "dataManager.set_victoire"
  in
  let victoire = (if (int_of_string msgHd) = !our_jid then 1 else 0) in
  a_gagne := Some(victoire)

let set_draw () = 
  a_gagne := Some(2)

let get_score () =
  match !a_gagne with
  | Some (victoire) -> float_of_int victoire
  | None -> -1.0

(* piece dans un transport*)
let transport pid = 
  let unite = get_unite pid in
  List.length (List.filter (fun (element:unite_list) -> ((element.pid <> pid) && (element.unite_type == TRANSPORT) && (unite.q == element.q) && (unite.r==element.r))) !liste_unites) > 0


let fog_proche pid distance =
  (* TODO *)
  let unite = get_unite pid in
  false

(* TODO *)
let get_next_playable () =
  match !liste_ville_alliee with
  | hd :: tail -> hd.cid
  | [] -> failwith "dataManager: get_next_playable error"

let get_next_movable () = 
  let liste_movable = List.filter (fun (element:unite_list) -> element.mov > 0) !liste_unites in
  match liste_movable with
  | hd :: tail -> (hd.pid,hd.unite_type)
  | [] -> (-1,ARMY)

let reset_move_all () =
  let reset_move (unite:unite_list) = {q=unite.q;r=unite.r;pid=unite.pid;unite_type=unite.unite_type;hp=unite.hp;mov=(ptid_to_move unite.mov)} in
  liste_unites := List.map reset_move !liste_unites

let init_data () =
  a_gagne := None;
  map_width := 0;
  map_height := 0;
  our_jid := 0;
  current_turn := 0;
  liste_unites := [];
  liste_ennemis := [];
  liste_ville_alliee := [];
  liste_ville_ennemie := [];
  ()


(***** TRAITEMENT *****)
(*Traitement des informations*)
let traiter_set_visible args =
  let ios = int_of_string in
  match args with
  | [ q ; r ; terrain ; "none" ] -> fill_terrain terrain (ios q) (ios r)
  | [ q ; r ; terrain ; "city" ; cid ] -> fill_terrain "city" (ios q) (ios r) 
  | [ q ; r ; terrain ; "owned_city" ; cid ; jid ] -> if (ios jid) = !our_jid then 
      (fill_terrain "our_city" (ios q) (ios r) ; 
       add_ville_allie (ios q) (ios r) (ios cid)) 
    else 
      (fill_terrain "their_city" (ios q) (ios r) ; 
       add_ville_ennemi (ios q) (ios r) (ios cid))
  | [ q ; r ; terrain ; "piece" ; jid ; pid ; ptid ; hp ] -> if (ios jid) = !our_jid then (update_unite_alliee (ios q) (ios r) (ios pid) (ptid_to_unites (ios ptid)) (ios hp) (ptid_to_move (ios ptid))) else (update_unite_ennemie (ios q) (ios r) (ios pid) (ptid_to_unites (ios ptid)) (ios hp))
  | _ -> failwith "erreur traiter_set_visible"

(*Faut-il gérer visible et explored pour l'algo génétique? *)
let traiter_set_explored args = ()

(*Supprime la piece de pid = args dans liste_unites ou liste_ennemis *)
let traiter_delete_piece args =
  let ios = int_of_string in
  match args with
  | [pid] -> liste_unites := List.filter (fun (element:unite_list) -> element.pid <> (ios pid)) !liste_unites ;
    liste_ennemis := List.filter (fun (element:unite_ennemies_list) -> element.pid <> (ios pid)) !liste_ennemis
  | _ -> failwith "erreur traiter_delete_piece";;

(*Ajoute une unite alliée*)
let traiter_create_piece args = 
  let ios = int_of_string in
  match args with
  | [pid ; ptid ; cid ; hp] -> let city = List.find (fun (element:allie) -> element.cid = (ios cid)) !liste_ville_alliee in 
    update_unite_alliee city.q city.r (ios pid) (ptid_to_unites (ios ptid)) (ios hp) (ptid_to_move (ios ptid))
  | _ -> failwith "erreur traiter_create_piece";;

(*Déplace une piece*)
let traiter_move args = 
  let ios = int_of_string in
  match args with
  | [pid ; q ; r] -> let piece = List.find (fun (element:unite_list) -> element.pid = (ios pid)) !liste_unites in 
    update_unite_alliee (ios q) (ios r) (ios pid) piece.unite_type piece.hp (piece.mov-1)
  | _ -> failwith "erreur traiter_move";;

(*Une ville alliee est prise par l'ennemi*)
let traiter_lose_city args = 
  let ios = int_of_string in
  match args with
  | [cid] -> liste_ville_alliee := List.filter (fun (element:allie) -> element.cid <> (ios cid)) !liste_ville_alliee
  | _ -> failwith "erreur traiter_lose_city";;

(*Inutiles?*)
let traiter_leave_terrain args = ()
let traiter_leave_city args = ()
let traiter_leave_piece args = ()

(*Une unite alliee entre dans une ville*)
let traiter_enter_city args = 
  let ios = int_of_string in
  match args with
  | [pid ; cid] ->  let city = List.find (fun (element:allie) -> element.cid = (ios cid)) !liste_ville_alliee in
    let piece = List.find (fun (element:unite_list) -> element.pid = (ios pid)) !liste_unites in
    update_unite_alliee city.q city.r (ios pid) piece.unite_type piece.hp piece.mov
  | _ -> failwith "erreur traiter_enter_city";;

(*Une unite alliee entre dans un transport*)
let traiter_enter_piece args = 
  let ios = int_of_string in
  match args with
  | [pid ; tid] ->  let transport = List.find (fun (element:unite_list) -> element.pid = (ios tid)) !liste_unites in
    let piece = List.find (fun (element:unite_list) -> element.pid = (ios pid)) !liste_unites in
    update_unite_alliee transport.q transport.r (ios pid) piece.unite_type piece.hp piece.mov
  | _ -> failwith "erreur traiter_enter_piece";;

(*On prend une ville ennemie*)
let traiter_ok_invasion args = 
  let ios = int_of_string in
  match args with
  | [cid ; q ; r] ->  add_ville_allie (ios q) (ios r) (ios cid);
    rm_ennemi (ios cid)
  | _ -> failwith "erreur traiter_ok_invasion";;

(*On rate une invasion, la piece est supprimée dans un autre message*)
let traiter_ko_invasion args = ()

(*Une ville est pleine*)
let traiter_city_units_limit args = ()

(*Le joueur a atteint son quota d'unites*)
let traiter_created_units_limit args = ()





