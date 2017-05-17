(* Objectif: -assurer la communication entre le serveur ( prend des strings)
            et  le decision tree ( sort des actions)
           -Lire les data envoyées par le serveur et les envoyer au DataManager *)


(* Fonctions publiques à faire :
    - send : t_action -> unit   //reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket
    - init_socket : unit -> unit 
    Fonctions privées :
    - send_to_server : string -> unit //l'envoi concret du message par le socket
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

(** TODO : au début de la partie, le serveur envoie un (ou plusieurs) messages d'init, tels que "width %d"
      Il faut les découvrir et traiter, peut être cf. empire-server/Main.ml **)

open Unix
open DataManager
open Types

(*** SOCKETS ***)

(* Canaux de communication en variables globales *)
(* On utilise des refs pour pouvoir modifier leur valeur *)
(* On utilise le type option pour pouvoir les initialiser à None *)
(* Pour accéder à un canal, il faut matcher "Some c" et "None" puis utiliser "!c" pour accéder au canal lui-même *)
let input_channel = ref(None)
let output_channel = ref(None)
let socket = ref(None)

let get_socket () =
  match !socket with
  | Some (sock) -> sock
  | None -> failwith "interfaceCom : socket non initialisé"

(* Fonctions auxiliaires d'ouverture et de fermeture de connexion *)
(* sockaddr -> socket *)
let open_connection sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 
  in try Unix.connect sock sockaddr ;
    sock
  (*(Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)*)
  with exn -> Unix.close sock ; raise exn

let shutdown_connection inchan =
  Printf.printf "Closing client socket\n\n";
  Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND


(* Création d'un socket client et connexion au serveur *)
let init_socket server port =
  (* On détermine l'addresse du serveur passé en argument, par 2 moyens à l'aide du try/with *)
  let server_addr = try Unix.inet_addr_of_string server  (* si c'est une adresse IP en string *)
    with Failure("inet_addr_of_string") ->
    try (Unix.gethostbyname server).Unix.h_addr_list.(0) (* si c'est un nom de machine *)
    with Not_found ->
      Printf.eprintf "%s : Unknown server\n" server ;
      exit 2
  in try
    let sockaddr = Unix.ADDR_INET(server_addr,port) in
    socket := Some(open_connection sockaddr); (* On crée le socket pour affecter les canaux in/out *)
    Printf.printf "Socket created\n\n";
    input_channel := Some (Unix.in_channel_of_descr (get_socket ()));
    output_channel := Some (Unix.out_channel_of_descr (get_socket ()))
  with Failure("int_of_string") -> Printf.eprintf "bad port number";
    exit 2
    
let close_socket () =
  Unix.close (get_socket ())

(** AUXILIAIRES **)

(* Fonctions auxiliaires pour extraire le début ou la fin d'un String *)
let str_start str len = String.sub str 0 len

let str_end str offset = String.sub str offset (String.length str - offset)

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
  aux str []

let dir_to_string direction = match direction with 
  | Up -> "1"
  | Down -> "4"
  | Right -> "0"
  | Left -> "3"
  | Upleft -> "2"
  | Downright -> "5" (*darius*)

let unites_to_ptid u = match u with
  | ARMY -> "0"
  | FIGHT -> "1"
  | TRANSPORT -> "2"
  | PATROL -> "3"
  | BATTLESHIP -> "4"

let action_to_string action =
  let soi = string_of_int in
  match action with
  | End_turn -> "end_turn"
  | Set_city_prod (cid, ptid) -> "set_city_prod " ^ (soi cid) ^ " " ^ (unites_to_ptid ptid)
  | Move (pid, did) -> "move " ^ (soi pid) ^ " " ^ (dir_to_string did)

(*  pid : piece_id
      ppid : parent_piece_id
      tp_pid: transport_piece_id
      cid : city_id
      jid : numéro d'un joueur (0 ou 1)
      ptid : piece_type_id (0-> ARMY, 1-> FIGHT, 2-> TRANSPORT, 3-> PATROL, 4-> BATTLESHIP)
      hits : piece.p_hits  (points de vie restants)
*)

(***** RECEPTION *****)

let traiter_message message =
  let listeMsg = split message in
  let tlMsg = List.tl listeMsg in

  match List.hd listeMsg with
  | "player_id" -> set_our_jid tlMsg
  | "width" -> set_map_width tlMsg
  | "height" -> set_map_height tlMsg
  | "piece_types" -> () (* TODO : Peupler une structure de données avec *)
  | "random_seed" -> Printf.printf "Seed de la map : %s\n" (List.hd tlMsg)
  | "draw" -> set_draw ()
  | "winner" -> set_victoire tlMsg
  | "error" -> Printf.printf "Received error : %s" (List.hd tlMsg)
  | "set_visible" -> traiter_set_visible tlMsg
  | "set_explored" -> traiter_set_explored tlMsg
  | "get_action" -> Printf.printf "get_action recu \n" (* TODO A ENLEVER *)
  | "delete_piece" -> traiter_delete_piece tlMsg
  | "create_piece" -> traiter_create_piece tlMsg
  | "move" -> traiter_move tlMsg
  | "lose_city" -> traiter_lose_city tlMsg
  | "leave_terrain" -> traiter_leave_terrain tlMsg
  | "enter_city" -> traiter_enter_city tlMsg
  | "enter_piece" -> traiter_enter_piece tlMsg
  | "leave_city" -> traiter_leave_city tlMsg
  | "leave_piece" -> traiter_leave_piece tlMsg
  | "ok-invasion" -> traiter_ok_invasion tlMsg
  | "ko-invasion" -> traiter_ko_invasion tlMsg
  | "city-units-limit" -> traiter_city_units_limit tlMsg
  | "created-units-limit" -> traiter_created_units_limit tlMsg
  | x -> Printf.printf "traiter_message: message serveur imprévu : \"%s\", d'argument \"%s\"\n" x (List.hd tlMsg)

let receive_next () =
  match !input_channel with
  | Some (ic) -> input_line ic
  | None -> "Input_channel not initialized"

let rec receive () =
  match receive_next () with
  | "" -> failwith "receive: Empty message"
  | "get_action" -> traiter_message "get_action"
  | "draw" -> traiter_message "draw"
  | "winner" -> traiter_message "winner"
  | m -> print_endline (string_of_int (Thread.id (Thread.self ())) ^ " : " ^ m); traiter_message m; receive ()

(*  SEND_TO_SERVER : string -> unit
      L'envoi concret du message par le socket *)
let send_to_server message =
  Printf.printf "Sending \"%s\" to the server\n %!" message;
  match !output_channel with
  | Some (oc) -> output_string oc (message ^ "\n");
    flush oc
  | None -> failwith "Output_channel not initialized"

(*  SEND : t_action -> unit           Fonction "publique"
      Reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket *)
let send action =
  send_to_server (action_to_string action)
  (* bloquant : traiter_message jusqu'au prochain get_action *)
