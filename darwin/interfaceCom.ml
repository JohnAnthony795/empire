(* Objectif: -assurer la communication entre le serveur ( prend des strings)
   					et  le decision tree ( sort des actions)

   		     -Lire les data envoyées par le serveur et les envoyer au DataManager *)


(* Fonctions publiques à faire :
   	- send : t_action -> unit 	//reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket

   	Fonctions privées :
   	- send_to_server : string -> unit	//l'envoi concret du message par le socket
   	- action_to_string : t_action -> string
   	- traiter_message : string -> unit //parser qui appelle les fonctions appropriées (du package DATA) ou qui se termine s'il recoit get_action (pour rendre la main au main)


*) 

let rec action_to_string action = 
  match action with
  | Head :: [] -> Head
  | Head :: Tail -> Head ^ " " ^ (action_to_string Tail)
;;
 
(*  pid : piece_id
	ppid : parent_piece_id
	tp_pid: transport_piece_id
	cid : city_id
	jid : numéro d'un joueur (0 ou 1)
	ptid : piece_type_id (TODO : quelles valeurs possibles ?)
	hits : piece.p_hits  (TODO : points de vie restants ? points de vie perdus ?)
*)
let traiter_message message =
  let listeInfo = split message in
  
  try match listeInfo with
  | [ "set_visible" ; q ; r ; terrain ; "none" ]
  | [ "set_visible" ; q ; r ; terrain ; "city" ; cid ]
  | [ "set_visible" ; q ; r ; terrain ; "owned_city" ; cid ; jid ]
  | [ "set_visible" ; q ; r ; terrain ; "piece" ; jid ; pid ; ptid ; hits ]
  | [ "set_explored" ; q ; r ; terrain ]
  | [ "get_action" ]
  | [ "delete_piece" ; pid ]
  | [ "create_piece" ; pid ; ptid ; cid ; hits ]
  | [ "move" ; pid ; q ; r ]
  | [ "lose_city" ; cid ]
  | [ "leave_terrain" ; pid ; q ; r ]
  | [ "enter_city" ; pid ; cid ]
  | [ "enter_piece" ; pid ; tp_pid ]
  | [ "leave_city" ; pid ; cid ]
  | [ "leave_piece" ; pid ; ppid ]
  | [ "ok-invasion" ; cid ; q ; r ]
  | [ "ko-invasion" ; cid ; q ; r ]
  | [ "city-units-limit" ; cid ]
  | [ "created-units-limit" ; cid ]
  | Hd :: _ -> Printf.printf "Erreur dans traiter_message : %s non reconnu" Hd
  with 
	| (* catch erreur *)

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
  aux str [] ;;














