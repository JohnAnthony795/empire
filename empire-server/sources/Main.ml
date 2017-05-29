open Actions
open Connection
open Empire
open Mailbox
open Misc

let ios = int_of_string
let soi = string_of_int

let piece_types = Hashtbl.create 16

let config =
  { m_width = 22
  ; m_height = 22
  ; m_sea_level = -0.3 (*-0.3*)
  ; m_amplitude = 1.0
  ; m_octaves = 4
  ; m_persistence = 0.2
  ; m_frequency = 0.06
  ; m_cities_density = 0.2
  ; m_nb_players = 2
  ; m_piece_types = piece_types
  }

let init_tables () =
  (*Random.self_init ();*)

  Hashtbl.add piece_types 0
    { a_id = 0
    ; a_name = "ARMY"
    ; a_symbol = 'A'
    ; a_terrain = [Ground]
    ; a_build_time = 5
    ; a_strength = 1
    ; a_max_hits = 1
    ; a_speed = 1
    ; a_capacity = 0
    ; a_autonomy = None
    ; a_transportable = []
    ; a_visibility = 2
    ; a_can_invade = true
    } ;

  Hashtbl.add piece_types 1
    { a_id = 1
    ; a_name = "FIGHT"
    ; a_symbol = 'F'
    ; a_terrain = [Ground ; Water]
    ; a_build_time = 10
    ; a_strength = 1
    ; a_max_hits = 1
    ; a_speed = 8
    ; a_capacity = 0
    ; a_autonomy = Some 4
    ; a_transportable = []
    ; a_visibility = 4
    ; a_can_invade = false
    } ;

  Hashtbl.add piece_types 2
    { a_id = 2
    ; a_name = "TRANSPORT"
    ; a_symbol = 'T'
    ; a_terrain = [Water]
    ; a_build_time = 30
    ; a_strength = 1
    ; a_max_hits = 1
    ; a_speed = 2
    ; a_capacity = 4
    ; a_autonomy = None
    ; a_transportable = [0]
    ; a_visibility = 2
    ; a_can_invade = false
    } ;

  Hashtbl.add piece_types 3
    { a_id = 3
    ; a_name = "PATROL"
    ; a_symbol = 'P'
    ; a_terrain = [Water]
    ; a_build_time = 15
    ; a_strength = 1
    ; a_max_hits = 1
    ; a_speed = 4
    ; a_capacity = 0
    ; a_autonomy = None
    ; a_transportable = []
    ; a_visibility = 4
    ; a_can_invade = false
    } ;

  Hashtbl.add piece_types 4
    { a_id = 4
    ; a_name = "BATTLESHIP"
    ; a_symbol = 'B'
    ; a_terrain = [Water]
    ; a_build_time = 40
    ; a_strength = 2
    ; a_max_hits = 10
    ; a_speed = 2
    ; a_capacity = 0
    ; a_autonomy = None
    ; a_transportable = []
    ; a_visibility = 4
    ; a_can_invade = false
    } ;

  ()

let () = init_tables ()

let game =
  Printf.printf "Generating game\n%!" ;
  GameGen.create_game config

(* Attente de la connexion des joueurs. *)
(* TODO: le premier joueur doit configurer la partie. *)
let port =
  let rec loop i =
    if i >= Array.length Sys.argv - 1 then 9301 else
    if Sys.argv.(i) = "-sport" then ios (Sys.argv.(i + 1)) else
      loop (i + 1) in
  loop 0

let server_sock =
  let addr = Unix.inet_addr_loopback in
  Printf.printf "Server listening on 127.0.0.1:%d\n%!" port ;
  server_open game.g_nb_players addr port

(* Attente de tous les clients. *)
let connections =  
  let connect_client _ =
    let socket = fst (Unix.accept server_sock) in
    { socket = socket
    ; in_channel = Unix.in_channel_of_descr socket
    ; out_channel = Unix.out_channel_of_descr socket
    }
  in
  Array.init game.g_nb_players connect_client

(* Émet un message vers le client player_index. Utilise un format. *)
let output player_index fmt =
  assert (player_index >= 0 && player_index < game.g_nb_players) ;
  Printf.kprintf (fun s -> output_string connections.(player_index).out_channel (s ^ "\n")) fmt

(* Envoi de la configuration a tous les joueurs, sans passer par Mailbox
   pour etre sur que ces messages partent en premier (sinon ils partiront apres
   les messages lies a la visibilite de la premiere ville). *)
let () =
  let rec send_config i =
    if i < game.g_nb_players then begin
      output i "width %d" game.g_width ;
      output i "height %d" game.g_height ;
      output i "player_id %d" i ;
      output i "piece_types %s" (info_get_info_piece_types game) ;
      output i "random_seed %d" game.g_random ;
      flush connections.(i).out_channel ;
      send_config (i + 1)
    end
  in
  send_config 0

(* i = indice du joueur *)
let handle_request game i message =
  let tokens = split message ' ' in
  let post response = post_message game (i, response) in
  
  try match tokens with
    | [ "end_game" ] -> game.g_end <- true
    | [ "end_turn" ] -> turn_end_turn game
    | [ "dump_map" ] -> turn_dump_map game
    | [ "fog_off"  ] -> fog_off game (* Attention, cette commande sera supprimée pendant les tests de l'IA. *)
                          
    | [ "moves" ; pid ; q ; r ] -> turn_moves game (ios pid) (ios q) (ios r)
    | [ "move" ; pid ; did ]    -> turn_move game (ios pid) (ios did)
    | [ "set_city_production" ; cid ; pid ] -> turn_set_city_production game (ios cid) (ios pid)
                                                 
    (* Requetes dont le traitement peut etre realise directement. Il ne
       necessite pas de tests particuliers. *)
    | [ "get_width" ] -> post (soi game.g_width)
    | [ "get_height" ] -> post (soi game.g_height)
    | [ "get_piece_types_names" ] ->
      let walk piece_type_id piece_type acu =
        (soi piece_type_id ^ ":" ^ piece_type.a_name) :: acu in
      post (String.concat "," (Hashtbl.fold walk game.g_piece_types []))
        
    (* Requetes dont le traitement demande des tests sur ce que le joueur
       peut demander. *)
    | [ "can_move" ; pid ; did ] -> post (string_of_bool (info_can_move game (ios pid) (ios did)))
    | [ "can_enter" ; pid ; did ] -> post (string_of_bool (info_can_enter game (ios pid) (ios did)))
    | [ "can_attack" ; pid ; did ] -> post (string_of_bool (info_can_attack game (ios pid) (ios did)))
    | [ "get_piece_id_by_loc" ; q ; r ] ->
      begin match info_get_piece_id_by_loc game (ios q) (ios r) with
        | Some i -> post (soi i)
        | None -> post "none"
      end
      
    | [ "get_transported_names_by_loc" ; q ; r ] ->
      let walk piece_id =
        let piece = Hashtbl.find game.g_pieces piece_id in
        let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
        soi piece_id ^ ":" ^ piece_type.a_name in
      let pieces = info_get_transported_pieces_by_loc game (ios q) (ios r) in
      post (sep walk "," pieces)
        
    | [ "get_info_city" ; cid ] -> post (info_get_info_city game (ios cid))
    | [ "get_info_piece" ; pid ] -> post (info_get_info_piece game (ios pid))
    | [ "get_city_production" ; cid ] ->
      begin match info_get_city_production game (ios cid) with
        | Some (a, b, c) -> post (sep soi ":" [a; b; c])
        | None -> post "none"
      end
      
    | [ "get_list_cities" ] ->
      let f (a, b, c) = sep soi ":" [a; b; c] in
      post (sep f "," (info_get_list_cities game))
        
    | [ "get_list_pieces" ] ->
      let f (a, b, c) = sep soi ":" [a; b; c] in
      post (sep f  "," (info_get_list_pieces game))
        
    | [ "get_list_movables" ] ->
      let f (a, b, c) = sep soi ":" [a; b; c] in
      post (sep f "," (info_get_list_movables game))
        
    | [ "get_city_id_by_loc" ; q ; r ] ->
      begin match info_get_city_id_by_loc game (ios q) (ios r) with
        | Some i -> post (soi i)
        | None -> post "none"
      end
      
    | _ -> post ("error \"invalid message: " ^ message ^ "\"")
             
  with
  | ActionError message -> post ("error \"" ^ message ^ "\"")


(* Envoie les messages de la mailbox (dans game) vers les joueurs. *)
let rec handle_mailbox game connections =
  if have_message game then begin
    let player_id, message = read_message game in
    Printf.printf "snd: %d <- %s\n%!" player_id message ;
    output player_id "%s" message ;
    handle_mailbox game connections
  end ;
  Array.iter (fun connection -> flush connection.out_channel) connections

;;

let rec handle_requests game connections =
  (* Demande d'action au joueur suivant. *)
  post_message game (game.g_turn, "get_action") ;

  (* Envoi des mails. *)
  handle_mailbox game connections ;
  
  (* Lecture des donnees reçues du joueur dont c'est le tour, et traitement. *)
  let message = read_client connections.(game.g_turn) in
  Printf.printf "rcv: %d -> %s\n%!" game.g_turn message ;
  handle_request game game.g_turn message ;

  (* Test de la fin de partie et rebouclage si on continue. *)

  (* Si le nombre de tours max a ete atteint, c'est le nombre de villes qui fait gagner. *)
  if game.g_round = game.g_max_round then
      begin
        let best_players =
          let get_best_players (best, acu) player =
            let size = Misc.Set.size player.player_cities in
            if size > best then (size, [player]) else
            if size = best then (size, player :: acu) else
            (best, acu) in
          Array.fold_left get_best_players (-1, []) game.g_players
        in
      
        match best_players with
        | (_, [ best_player ]) -> 
          let message = Printf.sprintf "winner %d" best_player.player_id in
          Array.iteri (fun i _ -> post_message game (i, message)) game.g_players ;
          handle_mailbox game connections
  
        | _ ->
          Array.iteri (fun i _ -> post_message game (i, "draw")) game.g_players ;
          handle_mailbox game connections
      end
  (* Sinon, on regarde si il ne reste aucun (possible si chaque joueur possede uniquement un FIGHT chacun a la
   * fin et que ces FIGHT arrivent en meme temps en fin d'autonomie) ou un seul survivant.
   * Si c'est le cas, c'est une fin de partie, sinon on passe a l'action suivante.
   *)
    else
      begin
        let survivors =
          let get_survivors acu player =
            if (Misc.Set.empty player.player_pieces) && (Misc.Set.empty player.player_cities)
            then acu
            else player :: acu in   
          Array.fold_left get_survivors [] game.g_players
        in
      
        match survivors with
        | [] ->
          Array.iteri (fun i _ -> post_message game (i, "draw")) game.g_players ;
          handle_mailbox game connections
      
        | [ surviv ] -> 
          let message = Printf.sprintf "winner %d" surviv.player_id in
          Array.iteri (fun i _ -> post_message game (i, message)) game.g_players ;
          handle_mailbox game connections

        (* Passage a l'action suivante. *)
        | _ -> if not game.g_end then handle_requests game connections
      end
      
let () =
  handle_requests game connections ;
  Array.iter (fun x -> close x.socket) connections
