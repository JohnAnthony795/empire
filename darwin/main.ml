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

type uniteville = ARMY | FIGHT | TRANSPORT | PATROL | BATTLESHIP | CITY

let get_arbre foret ptid =
  let (a1,a2,a3,a4,a5,a6) = foret in
  match ptid with
  | ARMY -> a1
  | FIGHT -> a2
  | TRANSPORT -> a3
  | PATROL -> a4
  | BATTLESHIP -> a5
  | CITY -> a6

(*Parcours d'arbre ( prise de décision ) --------------------------------------------*)

let compute_Action id foret = (*prend une id t_ID de piece et return une action t_action à jouer        (prendre aussi la foret?????? )*)
  let evaluate_pred pred piece_id = match pred with (* prend un predicat retourne un booleen  TENIR A JOUR voir directement mettre dans type*)
    | Nb_unite_allie_proche ( d, u, n, c ) -> let nbproche = (get_nb_unite_proche u piece_id d) in (*il manque une quantification de "proche" en fait *) 
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)

    | Nb_ville_allie_proche (d , n, c) -> let nbproche = (get_nb_ville_proche_allie piece_id d) in (*il manque une quantification de "proche" en fait *) 
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)
    | Nb_ville_ennemie_proche (d,n,c) -> let nbproche = (get_nb_ville_proche_ennemi piece_id d) in (*il manque une quantification de "proche" en fait *) 
      (match c with
       | Inf -> nbproche < n
       | Sup -> nbproche > n
       | Eq -> nbproche = n
       | InfEq -> nbproche <= n
       | SupEq -> nbproche >= n)
    | Littoral_adjacent -> littoral_adj piece_id (* presence de littoral dans une case adjacente*)
    | Transport -> transport piece_id (*présence de l'unité dans un transport*)
    | Fog_proche (d) -> fog_proche piece_id d (* distance proximité *)


  in
  let rec action_from_tree t id = match t with
    | Leaf a -> a
    | Node (t1,p,t2) -> if evaluate_pred p id then action_from_tree t1 id else action_from_tree t2 id
  in
  let decision_tree = get_arbre foret CITY (*TODO obtenir l'arbre qui concerne cette unité : get_type_by_id()? puis arbre n *)
  in
  action_from_tree decision_tree id;;


let main foret =
  Printf.printf "START MAIN\n%!";
  (*init socket*)
  init_socket "127.0.0.1" 9301;

  receive (); (* on reçoit les infos du début *)
  (* TODO true -> partie terminée ? *)
  while (get_score () = -1.0) do
    (* get next unité/ville à jouer *)
    send (compute_Action (get_next_playable ()) foret);
    receive ()
  done;
  get_score ()

(* let () = print_endline (string_of_float (main (9301, (read_arbre "IA.ads")))); () *)

(*
let () =
  write_arbre file (Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn) ;
  let (t1,_,_,_,_,t6) = (read_arbre file)  in printf "1er Arbre lu dans le fichier :\n"; print_tree t1 0 ;
  printf "\nDernier Arbre lu dans le fichier :\n"; print_tree t6 0
;;*)

