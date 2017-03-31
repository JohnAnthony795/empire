(* Objectif: -assurer la communication entre le serveur ( prend des strings)
   					et  le decision tree ( sort des actions)

   		     -Lire les data envoyées par le serveur et les envoyer au DataManager *)


(* Fonctions publiques à faire :
   	- send : t_action -> unit 	//reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket
   	- init_socket : unit -> unit 

   	Fonctions privées :
   	- send_to_server : string -> unit	//l'envoi concret du message par le socket
   	- action_to_string : t_action -> string
   	- traiter_message : string -> unit //parser qui appelle les fonctions appropriées (du package DATA) ou qui se termine s'il recoit get_action (pour rendre la main au main)

*) 

(* Actions possibles :
   	- end_game
   	- end_turn
   	- dump_map
   	- fog_off (cheat qui sera désactivé)
   	- moves ; pid ; q ; r
   	- move ; pid ; dir_id
   	- set_city_production ; cid ; pid
   	- get_width
   	- get_height
   	- get_piece_types_names
   	- can_move ; pid ; did
   	- can_enter ; pid ; did
   	- can_attack ; pid ; did
   	- get_piece_id_by_loc ; q ; r
   	- get_transported_names_by_loc ; q ; r
   	- get_info_city ; cid
   	- get_info_piece ; pid
   	- get_city_production ; cid
   	- get_list_cities
   	- get_list_pieces
   	- get_list_movables
   	- get_city_id_by_loc ; q ; r
*)

let rec action_to_string action = 
  match action with
  | head :: [] -> head
  | head :: tail -> head ^ " " ^ (action_to_string tail)
  | _ -> failwith "Fail action_to_string"
;;

(* Création d'un socket client et connexion au serveur *)
let init_socket server port =
  let server_addr = (gethostbyname server).h_addr_list.(0) in
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.connect socket (ADDR_INET(server_addr, port)) ;
  socket
;;

(* TODO: autoriser le passage de port en argument (cf. main() dans empire-client/sources/Main.ml) *)
(* variable globale pour stocker le socket *)
let socket_client = init_socket "localhost" 9301 ;;

(*  pid : piece_id
    	ppid : parent_piece_id
    	tp_pid: transport_piece_id
    	cid : city_id
    	jid : numéro d'un joueur (0 ou 1)
    	ptid : piece_type_id (0-> ARMY, 1-> FIGHT, 2-> TRANSPORT, 3-> PATROL, 4-> BATTLESHIP)
    	hits : piece.p_hits  (points de vie restants)
*)
let traiter_message message =
  let listeMsg = split message in
  let tlMsg = List.tl listeMsg in

  match List.hd listeMsg with
    | "set_visible" -> traiter_set_visible tlMsg
    | "set_explored" -> traiter_set_explored tlMsg
    | "get_action" -> ()
    | "delete_piece" -> traiter_delete_piece tlMsg
    | "create_piece" -> traiter_create_piece tlMsg
    | "move" -> traiter_move tlMsg
    | "lose_city" -> traiter_lose_city tlMsg
    | "leave_terrain" -> traiter_leave_terrain tlMsg
    | "enter_city" -> traiter_enter_city tlMsg
    | "enter_piece" -> traiter_enter_piece tlMsg
    | "leave_city" -> traiter_leave_city tlMsg
    | "leave_piece" -> traiter_leave_piece tlMsg
    | "ok-invasion" -> traiter_ok-invasion tlMsg
    | "ko-invasion" -> traiter_ko-invasion tlMsg
    | "city-units-limit" -> traiter_city-units-limit tlMsg
    | "created-units-limit" -> traiter_created-units-limit tlMsg
    | Hd :: _ -> Printf.printf "Erreur dans traiter_message : %s non reconnu" Hd
  	| _ -> failwith "LeCamlEstMortViveLeCaml"
;;

(*  SEND : t_action -> unit						Fonction "publique"
	Reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket *)
let send action =
	send_to_server (action_to_string action)
	(* bloquant : traiter_message jusqu'au prochain get_action *)
;;

(*  SEND_TO_SERVER : string -> unit
	L'envoi concret du message par le socket *)
let send_to_server message =
  let channel_out = Unix.out_channel_of_descr socket_client in
  output_string channel_out (message ^ "\n") ;
  flush channel_out
;;

  (* Fonctions auxiliaires pour extraire le début ou la fin d'un String *)
  let str_start str len = String.sub str 0 len ;;

let str_end str offset = String.sub str offset (String.length str - offset) ;;

(* Fonction auxiliaire pour séparer un message (str) en liste de strings
   Délimiteur : ' ' *)
let split str =
  let delim = ' ' in
  let rec aux str toks =
    if String.contains str delim then begin
      let i = String.index str delim in
      aux (str_end str (i + 1)) (str_start str i :: toks)
    end else
    if String.length str = 0 then List.rev toks else
      List.rev (str :: toks) in
  aux str 
;;


