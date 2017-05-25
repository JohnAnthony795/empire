(*
Le main lit et exploite les arbres, puis utilise interfaceCom pour communiquer les décisions au serveur. Il utilise aussi le dataManager pour LIRE des infos sur la carte (interfaceCom s'occupe de peupler le dataManager).
@Params: optionnels: 1 arbre  -> sélection (affronte un opposant random)
			   rien	-> on lit l'arbre depuis le fichier IA.ads et on affronte un 						   autre joueur sur le serveur
@Retour:	score d'adaptabilité (fonction de : victoire, couverture, nombre d'unité, villes...)


- read_arbre : string/FILE -> t_foret (Lecture d'arbre from fichier)
- compute_Action : t_ID -> t_action (Parcours d'arbre:(appelle le datamanager pour avoir des infos précises))


note : mettre la lecture/écriture de fichier dans un fichier à part?

*)


(* TODO: autoriser le passage de l'IP server + du port en argument de ligne de commande (cf. main() dans empire-client/sources/Main.ml) *)

open Types
open Printf
open InterfaceCom
open DataManager

type uniteville = ARMY | FIGHT | TRANSPORT | PATROL | BATTLESHIP | CITY ;;

let unite_to_uniteville (unite:unites) :uniteville =
  match unite with 
  |ARMY -> ARMY | TRANSPORT -> TRANSPORT | FIGHT -> FIGHT | BATTLESHIP -> BATTLESHIP | PATROL -> PATROL

let get_arbre foret (ptid:uniteville) =
  let (a1,a2,a3,a4,a5,a6) = foret in
  match ptid with
  | ARMY -> a1
  | FIGHT -> a2
  | TRANSPORT -> a3
  | PATROL -> a4
  | BATTLESHIP -> a5
  | CITY -> a6
(*Parcours d'arbre ( prise de décision ) --------------------------------------------*)

let compute_Action id unite_type foret = (* prend une id t_ID de piece et return une action t_action à jouer *)
  let evaluate_pred pred piece_id = 
    if Opt.doPrint then print_endline (pred_to_string pred) else ();
    match pred with (* prend un predicat retourne un booleen  TENIR A JOUR voir directement mettre dans type*)
    | Nb_unite_allie_proche ( d, u, n, c ) -> let nbproche = (get_nb_unite_proche u piece_id d) in
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)

    | Nb_ville_allie_proche (d, n, c) -> let nbproche = (get_nb_ville_proche_allie piece_id d) in
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)
    | Nb_ville_ennemie_proche (d,n,c) -> let nbproche = (get_nb_ville_proche_ennemi piece_id d) in
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)
    | Littoral_adjacent -> littoral_adj piece_id (* presence de littoral dans une case adjacente*)
    | Transport -> transport piece_id (*présence de l'unité dans un transport*)
    | Fog_proche (d) -> fog_proche piece_id d (* distance proximité *)
    | Unite_en_production -> unite_en_production piece_id


  in
  let rec action_from_tree t id = match t with
    | Leaf a -> (match a with 
        | Move (pid,dir) -> Move (id,dir)
        | Moves (pid,q,r) -> (let coords = (get_coords_moves id) in 
                  match coords with
                  | (-1,-1) -> Move (id,Up) 
                  | _ -> Moves (id,(fst coords),(snd coords)))
        | Set_city_prod (cid,unite) -> Set_city_prod (id,unite)
        | End_turn -> End_turn
        | Do_nothing (cid) -> Do_nothing (id)) 
    | Node (t1,p,t2) -> if evaluate_pred p id then action_from_tree t1 id else action_from_tree t2 id
  in
  let decision_tree = get_arbre foret unite_type (*TODO obtenir l'arbre qui concerne cette unité : get_type_by_id()? puis arbre n *)
  in
  action_from_tree decision_tree id


(* id = 0 -> on est la ref
   id = 1 -> on est un candidat *)
let main id =
  init_data ();
  let foret = if id = 0 then ToolsArbres.read_arbre "foret_ref.frt"
    else ToolsArbres.read_arbre "foret_cand.frt"
  in
  (*print_endline (ToolsArbres.forest_tocode foret);
    Unix.sleep 10;*)
  (*init socket*)
  init_socket "127.0.0.1" 9301;

  receive (); (* on reçoit les infos du début *)
  while (get_score () = -1.0) do
    (* get next unité/ville à jouer *)
    (*Printf.printf  "nouveau tour : %d et next playable %d \n%!" id (get_next_playable ());*)
    while(match get_next_playable () with
        | -1 -> false
        | _ -> (get_score () = -1.0)) do
      if Opt.doPrint then Printf.printf "VILLE ------------------ %d\n%!" (get_next_playable ()) else ();
      handle_action (compute_Action (get_next_playable ()) CITY foret);
    done;
    while(match get_next_movable () with
        | (-1,ARMY) -> false
        | _ -> (get_score () = -1.0)) do

      let next_unite = get_next_movable () in
      handle_action (compute_Action (fst next_unite) (unite_to_uniteville (snd next_unite)) foret);
    done;
    (*Fin du tour*)
    if (get_score () = -1.0) then handle_action (End_turn);
    reset_move_all ();
  done;
  if Opt.doPrint then Printf.printf "fin de partie : %d \n%!" id else ();
  get_score ()

let () = if ((Array.length Sys.argv) > 1) then let _ = main (int_of_string Sys.argv.(1)) in ()


