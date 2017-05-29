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
open Astar

(** DECLARATIONS **)
let map_width = ref(44)

let map_height = ref(44)

let our_jid = ref(0)

let current_turn = ref(0)

let directions =
  [(+1,  0); (+1, -1); ( 0, -1); (-1,  0); (-1, +1); ( 0, +1)]

let directions_array =
  [| (+1,  0); (+1, -1); ( 0, -1);
     (-1,  0); (-1, +1); ( 0, +1)
  |] ;;

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

let tile_delta (q1, r1) (q2, r2) = q2 - q1, r2 - r1

let ptid_to_unites ptid = match ptid with
  | 0 -> ARMY
  | 1 -> FIGHT
  | 2 -> TRANSPORT
  | 3 -> PATROL
  | 4 -> BATTLESHIP
  | _ -> failwith "Erreur ptid_to_unites : entrée non gérée"

let unite_to_ptid (unite:unites) = match unite with
  | ARMY -> 0
  | FIGHT -> 1
  | TRANSPORT -> 2
  | PATROL -> 3
  | BATTLESHIP -> 4

(* TODO : récupérer infos dynamiquement *)
(* renvoie la portee de deplacement d'un type d'unité *)
let ptid_to_move ptid =
  match ptid with
  | 0 -> 1
  | 1 -> 8
  | 2 -> 2
  | 3 -> 4
  | 4 -> 2 
  | a -> failwith ("Erreur ptid_to_move : entrée non gérée "^(string_of_int a))

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

(* Il s'agit d'une matrice (type_de_case, visible) *)
let map_terrain = Array.make_matrix !map_width !map_height (Unknown, false)

let set_terrain terrain q r visibleBool =
  match terrain with 
  | "water" -> map_terrain.(q).(r) <- (Water, visibleBool)
  | "ground"-> map_terrain.(q).(r) <- (Ground, visibleBool)
  | "our_city" -> map_terrain.(q).(r) <- (Ally, visibleBool)
  | "their_city" -> map_terrain.(q).(r) <- (Ennemy, visibleBool)
  | "city" -> map_terrain.(q).(r) <- (Neutral, visibleBool)
  | _ -> failwith "erreur set_terrain"

(* Renvoie VRAI ssi la case n'est pas en dehors de la map *)
let case_sur_map (q,r) =
  q >= 0 && q < !map_width && r >= 0 && r < !map_height

(* Renvoie TRUE si la case en (q,r) est de type terrain *)
let terrain_is terrain_type q r =
  if Opt.doPrint then Printf.printf "terrain is %d %d\n%!" q r else ();
  if not (case_sur_map (q,r)) then false
  else ( map_terrain.(q).(r) = (terrain_type, true) || map_terrain.(q).(r) = (terrain_type, false))

(* Renvoie toutes les cases autour de (qa,ra), mais pas la case elle-même *)
let get_cases_proches qa ra distance =
  let rec loop qb rb acu =
    if qb >= !map_width then loop 0 (rb + 1) acu else
    if rb >= !map_height then acu else
      let d = tiles_distance (qa, ra) (qb, rb) in
      if (d > 0 && d <= distance) then loop (qb + 1) rb (map_terrain.(qb).(rb) :: acu) else loop (qb + 1) rb acu
  in
  loop 0 0 []

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

type (*ville*) allie = {q : int ;r : int ;cid : int ; prod : unites option; tours_restants : int; mov : int}

let liste_ville_alliee = ref ([]) ;;

(* USAGE: add_ville_allie q r cid; *)
let add_ville_allie q r cid = 
  let cid_is cid element = element.cid = cid in
  if List.exists (cid_is cid) !liste_ville_alliee then ()
  else liste_ville_alliee := {q =q ; r =r ; cid = cid ; prod = None ; tours_restants = -1; mov = 1} :: !liste_ville_alliee

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

  liste_ville_alliee := {q= ville.q ; r= ville.r ; cid = cid ; prod = Some(unite_type) ; tours_restants = tours ; mov = 0} :: autresVilles

let get_city_production cid =
  let cid_is cid element = element.cid = cid in
  let ville = List.find (cid_is cid) !liste_ville_alliee in
  ville.prod


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
  try (List.find (fun (element:unite_list) -> element.pid = pid) !liste_unites) with
    Not_found -> {q=(-1);r=(-1);pid =(-1) ; unite_type =ARMY;hp=(-1);mov = 0}

let get_ville_allie cid =
  try (List.find (fun (element:allie) -> element.cid = cid) !liste_ville_alliee) with
    Not_found -> {q=(-1) ;r=(-1) ;cid=(-1) ; prod =None; tours_restants =0; mov =0}

(* Ajouté distance en parametre*)
let get_nb_unite_proche unites pid distance=
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    List.length (List.filter (fun (element:unite_list) -> ((element.pid <> pid) && (element.unite_type =unites) && ((tiles_distance (unite.q,unite.r) (element.q,element.r))<distance))) !liste_unites)
  else 
    let ville = get_ville_allie pid in
    List.length (List.filter (fun (element:unite_list) -> ((element.pid <> pid) && (element.unite_type =unites) && ((tiles_distance (ville.q,ville.r) (element.q,element.r))<distance))) !liste_unites)


let get_nb_ville_proche_allie pid distance =
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    List.length (List.filter (fun (element:allie) -> (tiles_distance (unite.q,unite.r) (element.q,element.r))<distance) !liste_ville_alliee) 
  else
    let ville = get_ville_allie pid in
    List.length (List.filter (fun (element:allie) -> (tiles_distance (ville.q,ville.r) (element.q,element.r))<distance) !liste_ville_alliee) 

(* comprend les villes neutres *)
let get_nb_ville_proche_ennemi pid distance =
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    (List.length (List.filter (fun (element:ennemi) -> (tiles_distance (unite.q,unite.r) (element.q,element.r))<distance) !liste_ville_ennemie)) +
    (let acu = ref(0) in
     for i=0 to (!map_width-1) do
       for j=0 to (!map_height-1) do
        (*Printf.printf "gnvep %d %d\n%!" i j;*)
        try
         match (map_terrain.(i).(j)) with
         | (Neutral, _) -> if ((tiles_distance (unite.q,unite.r) (i,j))<distance) then acu := !acu+1
         | _ -> ()
       with
       Not_found -> Printf.printf "%d %d\n" i j;()
       done; 
     done;
     !acu)
  else
    let ville = get_ville_allie pid in
    (List.length (List.filter (fun (element:ennemi) -> (tiles_distance (ville.q,ville.r) (element.q,element.r))<distance) !liste_ville_ennemie)) +
    (let acu = ref(0) in
     for i=0 to (!map_width-1) do
       for j=0 to (!map_height-1) do
         match (map_terrain.(i).(j)) with
         | (Neutral, _) -> if ((tiles_distance (ville.q,ville.r) (i,j))<distance) then acu := !acu+1
         | _ -> ()
       done; 
     done;
     !acu)

let get_nb_ville_proche_ennemi_old pid distance =
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    List.length (List.filter (fun (element:ennemi) -> (tiles_distance (unite.q,unite.r) (element.q,element.r))<distance) !liste_ville_ennemie)
  else
    let ville = get_ville_allie pid in
    List.length (List.filter (fun (element:ennemi) -> (tiles_distance (ville.q,ville.r) (element.q,element.r))<distance) !liste_ville_ennemie)


(* littoral dans une des 6 cases adjacentes *)
let littoral_adj pid =
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    let (q,r) = (unite.q, unite.r) in
    terrain_is Water (q+1) r || 
    terrain_is Water (q+1) (r-1) ||
    terrain_is Water q (r-1) ||
    terrain_is Water (q-1) r || 
    terrain_is Water (q-1) (r+1) || 
    terrain_is Water q (r+1)
  else
    let ville = get_ville_allie pid in
    let (q,r) = (ville.q, ville.r) in
    terrain_is Water (q+1) r || 
    terrain_is Water (q+1) (r-1) ||
    terrain_is Water q (r-1) ||
    terrain_is Water (q-1) r || 
    terrain_is Water (q-1) (r+1) || 
    terrain_is Water q (r+1)

(**Fonctions de scores fin de partie**)

let a_gagne = ref(None)

(*
SCORE: le score est la somme de valeurs concrete en mémoire fois un coefficient d'importance.
Les quantités considérées sont:
-Valeur de l'arsenal créé (nombre de pièce fois leur cout de production)
-Nombre de villes possédées
-taille de la zone explorée
-taille de la zone visible
*)

(*Getteurs de quantitées:*)
let get_nb_unite type_unite = 
  match type_unite with
  |ARMY -> List.length (List.filter (fun (x : unite_list) -> (x.unite_type) = ARMY) (!liste_unites)) (*egalité de contenu*)
  |PATROL ->  List.length (List.filter (fun (x : unite_list) -> x.unite_type = PATROL) (!liste_unites))
  |BATTLESHIP ->  List.length (List.filter (fun (x : unite_list) -> x.unite_type = BATTLESHIP) (!liste_unites))
  |FIGHT ->  List.length (List.filter (fun (x : unite_list) -> x.unite_type = FIGHT) (!liste_unites))
  |TRANSPORT ->  List.length (List.filter (fun (x : unite_list) -> x.unite_type = TRANSPORT) (!liste_unites))

let get_arsenal_value () = 	
  (get_nb_unite ARMY) * 5		
  +(get_nb_unite PATROL) * 15
  +(get_nb_unite BATTLESHIP) * 40
  +(get_nb_unite FIGHT) * 10
  +(get_nb_unite TRANSPORT) * 30

let get_nb_ville () = List.length (!liste_ville_alliee)

let get_explored_size() =
  let flatten matrix = (*turns a matrix (package Array) to a list. Loses info but easier to handle*)
    let v1 = Array.to_list matrix in (*we should have a list of arrays here*)
    let v2 = List.map (Array.to_list) v1 in (*flattening all arrays insinde the list*)
    List.concat v2  (*merging the 'terrain List List' into a 'terrain List'*)		
  in 
  (!map_height * !map_width) - List.length (List.filter (fun x-> match x with (t,_) -> t = Unknown) (flatten map_terrain)) (*TODO _ bizarre pas ltemp*)

let get_visible_size() = 
  let flatten matrix = (*turns a matrix (package Array) to a list. Loses info but easier to handle*)
    let v1 = Array.to_list matrix in (*we should have a list of arrays here*)
    let v2 = List.map (Array.to_list) v1 in (*flattening all arrays insinde the list*)
    List.concat v2  (*merging the 'terrain List List' into a 'terrain List'*)		
  in 
  List.length (List.filter (fun x-> match x with (_,visible) -> visible = true) (flatten map_terrain))

let calculate_score () =  (*Set coefs here*)
let (ars_val,nb_ville,explored_size,visible_size) = (get_arsenal_value(),get_nb_ville(),get_explored_size(),get_visible_size()) in
(*let f = float_of_int in
	Printf.printf "Arsenal value : %f \nNb_Ville : %f \nExplored_size : %f \nVisible_size : %f \n"  
			(f ars_val *. 0.1) (f nb_ville*. 10.0) (f explored_size *. 0.2)  (f visible_size *. 0.05)  ;*)
	
     float_of_int   ( ars_val) *. 0.1 
  +. float_of_int( nb_ville)   *. 10.0
  +. float_of_int( explored_size) *. 0.2
  +. float_of_int( visible_size ) *. 0.05

(* format du message : tl = jid vainqueur *)
let set_victoire msg =
  let msgHd = match msg with 
    | hd :: tail -> hd
    | [] -> failwith "dataManager.set_victoire"
  in
  let prescore = calculate_score () in 
  let victoire = (if (int_of_string msgHd) = !our_jid then (Printf.printf "WIN : Score total : %f\n" (prescore*.1.3) ;1.3) 
else (Printf.printf "LOSE : Score total : %f\n" (prescore*.0.7) ; 0.7)) in (*En tant que multiplicateur*)
  a_gagne := Some(victoire *. prescore)     

(* score pour draw *)
let set_draw () = 
  let prescore = calculate_score () in
  Printf.printf "DRAW : Score total : %f\n" prescore ;
  a_gagne := Some(1.0 *.prescore)

let get_score () =
  match !a_gagne with
  | Some (victoire) -> victoire
  | None -> -1.0

(* piece dans un transport*)
let transport pid = 
  let unite = get_unite pid in
  List.length (List.filter (fun (element:unite_list) -> ((element.pid <> pid) && (element.unite_type == TRANSPORT) && (unite.q == element.q) && (unite.r==element.r))) !liste_unites) > 0

(* Renvoie true s'il y a des cases Unknown à X cases ou moins *)
let unknown_proche pid distance =
  let unite = get_unite pid in
  let cases_proches = get_cases_proches unite.q unite.r distance in
  let case_est_unknown (typecase, visible) = typecase = Unknown in
  List.exists (case_est_unknown) cases_proches

(* Renvoie true s'il y a du fog à X cases ou moins
   	fog = cases explorées mais non visibles *)	
let fog_proche pid distance =
  let unite = get_unite pid in
  if (unite.pid <> -1) then
    let cases_proches = get_cases_proches unite.q unite.r distance in
    let case_est_fog (typecase, visible) = not visible in
    List.exists (case_est_fog) cases_proches
  else
    let ville = get_ville_allie pid in
    let cases_proches = get_cases_proches ville.q ville.r distance in
    let case_est_fog (typecase, visible) = not visible in
    List.exists (case_est_fog) cases_proches

let fog_adjacent q r =
  let cases_proches = get_cases_proches q r 1 in
  let case_est_fog (typecase, visible) = not visible in
  List.exists (case_est_fog) cases_proches

let unite_en_production cid =
  let ville = get_ville_allie cid in 
  match ville.prod with 
  | None -> false
  | _ -> true

let rec get_coords_ville (lst:ennemi list) =
  match lst with
  | [] -> []
  | x::tail -> (x.q,x.r)::(get_coords_ville tail)

let rec get_coords_unite (lst:unite_ennemies_list list) =
  match lst with
  | [] -> []
  | x::tail -> (x.q,x.r)::(get_coords_unite tail)

let get_coords_fog pid =
  let unite = get_unite pid in
  match unite.unite_type with
  | ARMY -> ( 
      let acu = ref([]) in
      for i=0 to (!map_width-1) do
        for j=0 to (!map_height-1) do
          match (map_terrain.(i).(j)) with
          | (Ground, true) -> if(fog_adjacent i j) then acu := (i,j)::!acu
          | _ -> ()
        done; 
      done; 
      !acu)
  | FIGHT -> ( 
      let acu = ref([]) in
      for i=0 to (!map_width-1) do
        for j=0 to (!map_height-1) do
          match (map_terrain.(i).(j)) with
          | (Ground, true) -> if(fog_adjacent i j) then acu := (i,j)::!acu
          | (Water, true) -> if(fog_adjacent i j) then acu := (i,j)::!acu
          | _ -> ()
        done; 
      done; 
      !acu)
  | _ -> ( 
      let acu = ref([]) in
      for i=0 to (!map_width-1) do
        for j=0 to (!map_height-1) do
          match (map_terrain.(i).(j)) with
          | (Water, true) -> if(fog_adjacent i j) then acu := (i,j)::!acu
          | _ -> ()
        done; 
      done; 
      !acu)



(*TODO Get closest coords visible of unexplored area*)
(*Récupère les coords de l'unité ennemi la plus proche que l'on peut attaquer *)
let get_closest_ennemy_coords pid =
  let unite = get_unite pid in
  (*if (unite.pid <> -1) then*)
  let list_coords = (match unite.unite_type with
      | ARMY -> (get_coords_unite (List.filter (fun (x:unite_ennemies_list) -> (match (map_terrain.(x.q).(x.r)) with 
          | (Ground,_) -> true
          | _ -> false))
          !liste_ennemis))
      | FIGHT -> (get_coords_unite !liste_ennemis)
      | _ -> (get_coords_unite (List.filter (fun (x:unite_ennemies_list) -> match (map_terrain.(x.q).(x.r)) with 
          | (Water,_) -> true
          | _ -> false)
          !liste_ennemis))
    ) in
  match list_coords with
  | [] -> (-1,-1)
  | _ -> ( List.fold_left (fun (qa,ra) (qb,rb) 
                            -> if ((tiles_distance (unite.q,unite.r) (qa,ra)) < (tiles_distance (unite.q,unite.r) (qb,rb))) then 
                                (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords)

let get_closest_ennemy_city_coords pid =
  let unite = get_unite pid in
  (*if (unite.pid <> -1) then*)
  let list_coords = get_coords_ville !liste_ville_ennemie in
  match list_coords with
  | [] -> (let acu = ref([]) in
           for i=0 to (!map_width-1) do
             for j=0 to (!map_height-1) do
               match (map_terrain.(i).(j)) with
               | (Neutral, _) -> acu := (i,j)::!acu
               | _ -> ()
             done; 
           done; 
           match !acu with
           | [] -> (-1,-1)
           | _ -> (  List.fold_left (fun (qa,ra) (qb,rb) 
                                      -> if ((tiles_distance (unite.q,unite.r) (qa,ra)) < (tiles_distance (unite.q,unite.r) (qb,rb))) then 
                                          (qa,ra) else (qb,rb)) (List.hd !acu) !acu))
  | _ -> (  List.fold_left (fun (qa,ra) (qb,rb) 
                             -> if ((tiles_distance (unite.q,unite.r) (qa,ra)) < (tiles_distance (unite.q,unite.r) (qb,rb))) then 
                                 (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords)
(*else
  let ville = get_ville_allie pid in
  List.fold_left (fun (qa,ra) (qb,rb) 
  -> if ((tiles_distance (ville.q,ville.r) (qa,ra)) < (tiles_distance (ville.q,ville.r) (qb,rb))) then 
  (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords*)

(** Renvoie les coords de la case visible, adjacente a une case fog, la plus pres *)
let get_closest_fog_coords pid =
  let list_coords = get_coords_fog pid in
  match list_coords with 
  | [] -> (-1,-1)
  | _ -> (
      let unite = get_unite pid in
      (*if (unite.pid <> -1) then*)
      (List.fold_left (fun (qa,ra) (qb,rb) 
                        -> if ((tiles_distance (unite.q,unite.r) (qa,ra)) < (tiles_distance (unite.q,unite.r) (qb,rb))) then 
                            (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords))

let get_closest_transport_coords pid =
  let unite = get_unite pid in
  (*if (unite.pid <> -1) then*)
  let list_coords = (match unite.unite_type with
      | ARMY -> (get_coords_unite (List.filter (fun (x:unite_ennemies_list) -> (match (map_terrain.(x.q).(x.r)) with 
          | (Ground,_) -> true
          | _ -> false))
          !liste_ennemis))
      | FIGHT -> (get_coords_unite !liste_ennemis)
      | _ -> (get_coords_unite (List.filter (fun (x:unite_ennemies_list) -> match (map_terrain.(x.q).(x.r)) with 
          | (Water,_) -> true
          | _ -> false)
          !liste_ennemis))
    ) in
  match list_coords with
  | [] -> (-1,-1)
  | _ -> ( List.fold_left (fun (qa,ra) (qb,rb) 
                            -> if ((tiles_distance (unite.q,unite.r) (qa,ra)) < (tiles_distance (unite.q,unite.r) (qb,rb))) then 
                                (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords)

(*else
  let ville = get_ville_allie pid in
  List.fold_left (fun (qa,ra) (qb,rb) 
  -> if ((tiles_distance (ville.q,ville.r) (qa,ra)) < (tiles_distance (ville.q,ville.r) (qb,rb))) then 
  (qa,ra) else (qb,rb)) (List.hd list_coords) list_coords*)


(*Cherche le chemin le plsu court vers l'unité ennemi la plus proche et renvoie des coordonnées possible pour moves*)
let get_coords_attaquer pid =
  let coords = get_closest_ennemy_coords pid in
  match coords with
  | (-1,-1) -> (-1,-1)
  | _ -> (
      let unite = get_unite pid in
      let current_move = ref(unite.mov) in
      let last_coord = ref((0,0)) in
      let old_loc = (unite.q,unite.r) in
      let heuristic = tiles_distance in 
      let neighbors (q_loc, r_loc) =
        (* XXX Printf.printf "neighbors: %d %d\n" q_loc r_loc ;*)
        let test_direction neighbors (q_delta, r_delta) =
          let (q1, r1) as loc = q_delta + q_loc, r_delta + r_loc in
          (* Ce voisin n'est pas considere si il est hors de la carte ou si la piece ne peut pas marcher sur
           * ce terrain ou si il y a quelque chose alors que ce n'est pas la destination finale (q, r) ou
           * encore si cette position n'est pas visible par le joueur.
          *)
          (*TODO Ajout test de terrain*)
          if not (case_sur_map loc) || 
             (match unite.unite_type with 
              | ARMY -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                         && loc <> ((fst coords), (snd coords)))
              | FIGHT -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                          && loc <> ((fst coords), (snd coords)))
              | _ -> ((match (map_terrain.(q1).(r1)) with
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                      && loc <> ((fst coords), (snd coords)))) 
          then neighbors else (q1, r1) :: neighbors in
        Array.fold_left test_direction [] directions_array in
      let cost _ _ = 1 in
      (*Printf.printf "neighbors %d %d\n%!" (fst coords) (snd coords);
        Printf.printf "neighbors %d %d\n%!" (fst old_loc) (snd old_loc);*)
      let path = Astar.astar_goal old_loc ((fst coords), (snd coords)) 30 neighbors cost heuristic in
      let rec do_path = function
        | [] -> !last_coord
        | h :: t -> (*Printf.printf "suceeees %d\n%!" !current_move;*)
          if(!current_move>0) then
            (current_move := (!current_move-1);
             last_coord := h;
             (do_path t))
          else
            !last_coord
      in
      match path with
      | None -> (*Printf.printf("Erreur moves !!!\n%!");*)(-1,-1)
      | Some l -> do_path l)

let get_coords_explorer pid =
  let coords = get_closest_fog_coords pid in              
  match coords with
  | (-1,-1) -> (-1,-1)
  | _ -> (
      let unite = get_unite pid in
      let current_move = ref(unite.mov) in
      let last_coord = ref((0,0)) in
      let old_loc = (unite.q,unite.r) in
      let heuristic = tiles_distance in 
      let neighbors (q_loc, r_loc) =
        (* XXX Printf.printf "neighbors: %d %d\n" q_loc r_loc ;*)
        let test_direction neighbors (q_delta, r_delta) =
          let (q1, r1) as loc = q_delta + q_loc, r_delta + r_loc in
          (* Ce voisin n'est pas considere si il est hors de la carte ou si la piece ne peut pas marcher sur
           * ce terrain ou si il y a quelque chose alors que ce n'est pas la destination finale (q, r) ou
           * encore si cette position n'est pas visible par le joueur.
          *)
          (*TODO Ajout test de terrain*)
          if not (case_sur_map loc) || 
             (match unite.unite_type with 
              | ARMY -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                         && loc <> ((fst coords), (snd coords)))
              | FIGHT -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                          && loc <> ((fst coords), (snd coords)))
              | _ -> ((match (map_terrain.(q1).(r1)) with
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                      && loc <> ((fst coords), (snd coords)))) 
          then neighbors else (q1, r1) :: neighbors in
        Array.fold_left test_direction [] directions_array in
      let cost _ _ = 1 in
      (*Printf.printf "neighbors %d %d\n%!" (fst coords) (snd coords);
        Printf.printf "neighbors %d %d\n%!" (fst old_loc) (snd old_loc);*)
      let path = Astar.astar_goal old_loc ((fst coords), (snd coords)) 30 neighbors cost heuristic in
      let rec do_path = function
        | [] -> !last_coord
        | h :: t -> (*Printf.printf "suceeees %d\n%!" !current_move;*)
          if(!current_move>0) then
            (current_move := (!current_move-1);
             last_coord := h;
             (do_path t))
          else
            !last_coord
      in
      match path with
      | None -> (*Printf.printf("Erreur moves explorer!!!\n%!");*)(-1,-1)
      | Some l -> do_path l)

let get_coords_envahir pid =
  let first_coords = get_closest_ennemy_city_coords pid in
  let coords = (
    match first_coords with
    | (-1,-1) -> (let fog_coords = get_closest_fog_coords pid in 
                  match fog_coords with 
                  | (-1,-1) -> (-1,-1)
                  | _ -> fog_coords)
    | (a,b) -> (a,b)) in
  match coords with
  | (-1,-1) -> (-1,-1)
  | _ -> (
      let unite = get_unite pid in
      let current_move = ref(unite.mov) in
      let last_coord = ref((0,0)) in
      let old_loc = (unite.q,unite.r) in
      let heuristic = tiles_distance in 
      let neighbors (q_loc, r_loc) =
        (* XXX Printf.printf "neighbors: %d %d\n" q_loc r_loc ;*)
        let test_direction neighbors (q_delta, r_delta) =
          let (q1, r1) as loc = q_delta + q_loc, r_delta + r_loc in
          (* Ce voisin n'est pas considere si il est hors de la carte ou si la piece ne peut pas marcher sur
           * ce terrain ou si il y a quelque chose alors que ce n'est pas la destination finale (q, r) ou
           * encore si cette position n'est pas visible par le joueur.
          *)
          (*TODO Ajout test de terrain*)
          if not (case_sur_map loc) || 
             (match unite.unite_type with 
              | ARMY -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                         && loc <> ((fst coords), (snd coords)))
              | FIGHT -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                          && loc <> ((fst coords), (snd coords)))
              | _ -> ((match (map_terrain.(q1).(r1)) with
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                      && loc <> ((fst coords), (snd coords)))) 
          then neighbors else (q1, r1) :: neighbors in
        Array.fold_left test_direction [] directions_array in
      let cost _ _ = 1 in
      (*Printf.printf "neighbors %d %d\n%!" (fst coords) (snd coords);
        Printf.printf "neighbors %d %d\n%!" (fst old_loc) (snd old_loc);*)
      let path = Astar.astar_goal old_loc ((fst coords), (snd coords)) 30 neighbors cost heuristic in
      let rec do_path = function
        | [] -> !last_coord
        | h :: t -> (*Printf.printf "suceeees %d\n%!" !current_move;*)
          if(!current_move>0) then
            (current_move := (!current_move-1);
             last_coord := h;
             (do_path t))
          else
            !last_coord
      in
      match path with
      | None -> (*Printf.printf("Erreur moves !!!\n%!");*)(-1,-1)
      | Some l -> do_path l)

let get_coords_transporter pid =
  let first_coords = get_closest_transport_coords pid in
  let coords = (
    match first_coords with
    | (-1,-1) -> (let fog_coords = get_closest_fog_coords pid in 
                  match fog_coords with 
                  | (-1,-1) -> (-1,-1)
                  | _ -> fog_coords)
    | (a,b) -> (a,b)) in
  match coords with
  | (-1,-1) -> (-1,-1)
  | _ -> (
      let unite = get_unite pid in
      let current_move = ref(unite.mov) in
      let last_coord = ref((0,0)) in
      let old_loc = (unite.q,unite.r) in
      let heuristic = tiles_distance in 
      let neighbors (q_loc, r_loc) =
        (* XXX Printf.printf "neighbors: %d %d\n" q_loc r_loc ;*)
        let test_direction neighbors (q_delta, r_delta) =
          let (q1, r1) as loc = q_delta + q_loc, r_delta + r_loc in
          (* Ce voisin n'est pas considere si il est hors de la carte ou si la piece ne peut pas marcher sur
           * ce terrain ou si il y a quelque chose alors que ce n'est pas la destination finale (q, r) ou
           * encore si cette position n'est pas visible par le joueur.
          *)
          (*TODO Ajout test de terrain*)
          if not (case_sur_map loc) || 
             (match unite.unite_type with 
              | ARMY -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                         && loc <> ((fst coords), (snd coords)))
              | FIGHT -> ((match (map_terrain.(q1).(r1)) with
                  | (Ground,_) -> false
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                          && loc <> ((fst coords), (snd coords)))
              | _ -> ((match (map_terrain.(q1).(r1)) with
                  | (Water,_) -> false
                  | (Unknown,_) -> false
                  | _ -> true)
                      && loc <> ((fst coords), (snd coords)))) 
          then neighbors else (q1, r1) :: neighbors in
        Array.fold_left test_direction [] directions_array in
      let cost _ _ = 1 in
      (*Printf.printf "neighbors %d %d\n%!" (fst coords) (snd coords);
        Printf.printf "neighbors %d %d\n%!" (fst old_loc) (snd old_loc);*)
      let path = Astar.astar_goal old_loc ((fst coords), (snd coords)) 30 neighbors cost heuristic in
      let rec do_path = function
        | [] -> !last_coord
        | h :: t -> (*Printf.printf "suceeees %d\n%!" !current_move;*)
          if(!current_move>0) then
            (current_move := (!current_move-1);
             last_coord := h;
             (do_path t))
          else
            !last_coord
      in
      match path with
      | None -> (*Printf.printf("Erreur moves !!!\n%!");*)(-1,-1)
      | Some l -> do_path l)

(* TODO *)
let get_next_playable () =
  let liste_ville_playable = List.filter (fun (element:allie) -> element.mov > 0) !liste_ville_alliee in
  match liste_ville_playable with
  | hd :: tail -> hd.cid
  | [] -> -1

let get_next_movable () = 
  let liste_movable = List.filter (fun (element:unite_list) -> element.mov > 0) !liste_unites in
  match liste_movable with
  | hd :: tail -> (hd.pid,hd.unite_type)
  | [] -> (-1,ARMY)

let reset_move_all () =
  let reset_move (unite:unite_list) = {q=unite.q;r=unite.r;pid=unite.pid;unite_type=unite.unite_type;hp=unite.hp;mov=(ptid_to_move (unite_to_ptid unite.unite_type))} in
  let reset_move_ville (ville:allie) = {q=ville.q;r=ville.r;cid=ville.cid;prod=ville.prod;tours_restants=ville.tours_restants;mov=1} in
  if Opt.doPrint then print_endline "reset_move_all" else ();
  liste_unites := List.map reset_move !liste_unites;
  liste_ville_alliee := List.map reset_move_ville !liste_ville_alliee

let set_move_to_zero cid =
  let piece = get_unite cid in
  if (piece.pid <> -1) then
  update_unite_alliee piece.q piece.r cid piece.unite_type piece.hp 0
else
  let cid_is cid (element:allie) = element.cid = cid in
  let cid_is_not cid (element:allie) = element.cid <> cid in
  let ville = List.find (cid_is cid) !liste_ville_alliee in (* ville à update *)
  let autresVilles = List.filter (cid_is_not cid) !liste_ville_alliee in (* toutes les villes sauf celle à udpate *)

  liste_ville_alliee := {q= ville.q ; r= ville.r ; cid = cid ; prod = ville.prod ; tours_restants = ville.tours_restants ; mov = 0} :: autresVilles

let set_move_to_zero_unite pid =
  let piece = List.find (fun (element:unite_list) -> element.pid = pid) !liste_unites in 
  update_unite_alliee piece.q piece.r pid piece.unite_type piece.hp 0

let init_data () =
  for i=0 to (!map_width-1) do
    for j=0 to (!map_height-1) do
      map_terrain.(i).(j) <- (Unknown,false)
    done;
  done;
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
  | [ q ; r ; terrain ; "none" ] -> set_terrain terrain (ios q) (ios r) true
  | [ q ; r ; terrain ; "city" ; cid ] -> set_terrain "city" (ios q) (ios r) true
  | [ q ; r ; terrain ; "owned_city" ; cid ; jid ] -> if (ios jid) = !our_jid then 
      (set_terrain "our_city" (ios q) (ios r) true ; 
       add_ville_allie (ios q) (ios r) (ios cid)) 
    else 
      (set_terrain "their_city" (ios q) (ios r) true ; 
       add_ville_ennemi (ios q) (ios r) (ios cid))
  | [ q ; r ; terrain ; "piece" ; jid ; pid ; ptid ; hp ] -> set_terrain terrain (ios q) (ios r) true; if (ios jid) = !our_jid then 
      let piece = List.find (fun (element:unite_list) -> element.pid = (ios pid)) !liste_unites in
      (update_unite_alliee (ios q) (ios r) (ios pid) (ptid_to_unites (ios ptid)) (ios hp) (piece.mov)) else (update_unite_ennemie (ios q) (ios r) (ios pid) (ptid_to_unites (ios ptid)) (ios hp))
  | _ -> failwith "erreur traiter_set_visible"

let traiter_set_explored args =
  let ios = int_of_string in
  match args with
  | [ q ; r ; terrain] -> set_terrain terrain (ios q) (ios r) false
  | _ -> failwith "ERREUR traiter_set_explored"

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
    if Opt.doPrint then Printf.printf "traiter move %d\n%!" (piece.mov-1) else ();
    update_unite_alliee (ios q) (ios r) (ios pid) piece.unite_type piece.hp (piece.mov-1)
  | _ -> failwith "erreur traiter_move";;

(*Une ville alliee est prise par l'ennemi*)
let traiter_lose_city args = 
  let ios = int_of_string in
  match args with
  | [cid] -> let city = List.find (fun (element:allie) -> element.cid = (ios cid)) !liste_ville_alliee in add_ville_ennemi city.q city.r city.cid; liste_ville_alliee := List.filter (fun (element:allie) -> element.cid <> (ios cid)) !liste_ville_alliee
  | _ -> failwith "erreur traiter_lose_city";;

let traiter_invalid_terrain () =
  let pid = fst (get_next_movable ()) in
  if pid <> -1 then
    set_move_to_zero_unite (pid)

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
    update_unite_alliee city.q city.r (ios pid) piece.unite_type piece.hp (piece.mov-1)
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
let traiter_city_units_limit args = 
  let pid = fst (get_next_movable ()) in
  if pid <> -1 then
    set_move_to_zero_unite (pid)

(*Le joueur a atteint son quota d'unites*)
let traiter_created_units_limit args = ()





