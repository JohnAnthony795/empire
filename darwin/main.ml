
open Types
open Printf
open InterfaceCom
open DataManager

let () = Random.self_init()

(** TOOLS **)

(* Fonctions de sérialisation pour lecture et écriture depuis un channel (par exemple un fichier)
   	 Nous avons eu des SEGFAULT inattendues, c'est pourquoi nous avons arrêté de les utiliser pour revenir au
   	 parser lex/yacc qui fonctionnait 
   	 Elles sont ici pour archive (et potentielle réutilisation). *)

(*
(* ex: marshal_read "marshaled_pop" TypesGen.t_population *)
let marshal_read filename type =
  let ic = open_in_bin filename in
	try begin	
		let return = (Marshal.from_channel ic : type) in
		close_in ic;
		return
	end
  with End_of_file -> 
		close_in ic;
		failwith "Erreur main.marshal_read_popu"

let marshal_write filename element =
  let oc = open_out_bin filename in
  Marshal.to_channel oc element [Marshal.Closures; Marshal.Compat_32];
  close_out oc;
	()
*)	

type uniteville = ARMY | FIGHT | TRANSPORT | PATROL | BATTLESHIP | CITY ;;

let unite_to_uniteville (unite:unites) :uniteville =
  match unite with 
  | ARMY -> ARMY
  | TRANSPORT -> TRANSPORT
  | FIGHT -> FIGHT
  | BATTLESHIP -> BATTLESHIP
  | PATROL -> PATROL

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

let random_direction =
  match (Random.int 6) with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Right
  | 3 -> Left
  | 4 -> Upleft
  | 5 -> Downright
  | _ -> failwith "muter_candidat : valeur non attendue"

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
    | Nb_ville_neutre_proche (d,n,c) -> let nbproche = (get_nb_ville_proche_neutre piece_id d) in
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)
    | Littoral_adjacent -> littoral_adj piece_id (* presence de littoral dans une case adjacente*)
    | Transport -> transport piece_id (*présence de l'unité dans un transport*)
    | Fog_proche (d) -> fog_proche piece_id d (* distance proximité *)
    | Unknown_proche (d) -> unknown_proche piece_id d (* distance proximité *)
    | Unite_en_production -> unite_en_production piece_id


  in
  let rec action_from_tree t id = match t with
    | Leaf a -> (match a with 
        | Move (pid,dir) -> Move (id,dir)
        | Attaquer (pid,q,r) -> (let coords = (get_coords_attaquer id) in 
                                 match coords with
                                 | (-1,-1) -> (Move (id,random_direction))
                                 | _ -> Attaquer (id,(fst coords),(snd coords)))
        | Explorer (pid,q,r) -> (let coords = (get_coords_explorer id) in 
                                 match coords with
                                 | (-1,-1) -> (Move (id,random_direction))
                                 | _ -> Explorer (id,(fst coords),(snd coords)))
        | Envahir (pid,q,r) -> (let coords = (get_coords_envahir id) in 
                                match coords with
                                (*| (-1,-1) -> (Move (id,random_direction))*)
                                | (-1,-1) -> Do_nothing (id)
                                | _ -> Envahir (id,(fst coords),(snd coords)))
        | Envahir_neutre (pid,q,r) -> (let coords = (get_coords_envahir_neutre id) in 
                                       match coords with
                                       (*| (-1,-1) -> (Move (id,random_direction))*)
                                       | (-1,-1) -> Do_nothing (id)
                                       | _ -> Envahir (id,(fst coords),(snd coords)))
        | Transporter (pid,q,r) -> (let coords = (get_coords_transporter id) in 
                                    match coords with
                                    | (-1,-1) -> (Move (id,random_direction))
                                    | _ -> Transporter (id,(fst coords),(snd coords)))
        | Set_city_prod (cid,unite) -> let prod = (get_city_production id) in
          if((compare (Some(unite)) prod)=0) then (Do_nothing (id)) else Set_city_prod (id,unite)

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
      if Opt.doPrint then Printf.printf "UNITE ------------------ %d\n%!" (fst next_unite) else ();
      handle_action (compute_Action (fst next_unite) (unite_to_uniteville (snd next_unite)) foret);
    done;
    (*Fin du tour*)
    if (get_score () = -1.0) then handle_action (End_turn);
    reset_move_all ();
  done;
  if Opt.doPrint then Printf.printf "fin de partie : %d \n%!" id else ();
  shutdown_connection ();
  get_score ()

let () = if ((Array.length Sys.argv) > 1) then let _ = main (int_of_string Sys.argv.(1)) in ()


