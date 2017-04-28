open Empire ;;

exception ActionError of string ;;

let soi = string_of_int

(* Partie getter et tests. *)

let get_piece_by_piece_id game piece_id =
  if not (Hashtbl.mem game.g_pieces piece_id) then
    raise (ActionError "invalid piece id") ;
  Hashtbl.find game.g_pieces piece_id ;;

let get_piece_type_by_piece_type_id game piece_type_id =
  if not (Hashtbl.mem game.g_piece_types piece_type_id) then
    raise (ActionError "invalid piece type id") ;
  Hashtbl.find game.g_piece_types piece_type_id

let get_city_by_city_id game city_id =
  if not (Hashtbl.mem game.g_cities city_id) then
    raise (ActionError "invalid city id") ;
  Hashtbl.find game.g_cities city_id ;;

let check_piece_owner piece player_id =
  if piece.p_owner <> player_id then
    raise (ActionError "not owner of the piece") ;;

let check_piece_can_move piece =
  if piece.p_rem_moves = 0 then raise (ActionError "piece can't move") ;;

let check_city_owner city player_id =
  if city.c_owner <> Some player_id then
    raise (ActionError "not owner of the city") ;;

let check_loc game location =
  if not (Coords.in_map game location) then
    raise (ActionError "invalid location") ;;

let check_piece_type_terrain piece_type terrain =
  if not (List.mem terrain piece_type.a_terrain) then
    raise (ActionError "invalid terrain") ;;

let check_visibility player location =
  if not (View.is_visible player location) then
    raise (ActionError "tile not visible") ;;

let check_piece_can_invade game piece =
  let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
  if not piece_type.a_can_invade then
    raise (ActionError "piece can't invade a city") ;;

(* Partie calcul d'informations. *)

let get_new_id game =
  let rec walk i =
    if Hashtbl.mem game.g_cities i
      || Hashtbl.mem game.g_pieces i
      then walk (i + 1) else i in
  game.g_counter <- walk game.g_counter ;
  game.g_counter ;;

let rec get_piece_loc game piece =
  match piece.p_parent with
  | ContainerPiece parent_piece_id ->
      get_piece_loc game (Hashtbl.find game.g_pieces parent_piece_id)
  | ContainerCity parent_city_id ->
      let parent_city = Hashtbl.find game.g_cities parent_city_id in
      parent_city.c_loc
  | ContainerTerrain (q, r) -> (q, r) ;;

(* Partie fonction de modification. *)

let update_players_view game loc =
  let update_player_view player = View.update_view game player loc in
  ignore (Array.map update_player_view game.g_players) ;;

let remove_piece_from_parent game piece =
  let parent = piece.p_parent in
  match parent with
  | ContainerPiece parent_piece_id ->
      let parent_piece = Hashtbl.find game.g_pieces parent_piece_id in
      Misc.Set.del_value parent_piece.p_transport piece.p_id ;
      let message = Printf.sprintf "leave_piece %d %d" piece.p_id parent_piece_id in
      Mailbox.post_message game (piece.p_owner, message)
  | ContainerCity parent_city_id ->
      let parent_city = Hashtbl.find game.g_cities parent_city_id in
      Misc.Set.del_value parent_city.c_transport piece.p_id ;
      let message = Printf.sprintf "leave_city %d %d" piece.p_id parent_city_id in
      Mailbox.post_message game (piece.p_owner, message)
  | ContainerTerrain (q, r) ->
      game.g_map_items.(q).(r) <- None ;
      let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
      let visibility = piece_type.a_visibility in
      let around = Coords.tiles_around game visibility (q, r) in
      let message = Printf.sprintf "leave_terrain %d %d %d" piece.p_id q r in
      Mailbox.post_message game (piece.p_owner, message) ;
      View.dec_visibility game game.g_players.(piece.p_owner) around ;;

let insert_piece_in_city game piece city =
  remove_piece_from_parent game piece ;
  piece.p_parent <- ContainerCity city.c_id ;
  Misc.Set.add city.c_transport piece.p_id ;
  let message = Printf.sprintf "enter_city %d %d" piece.p_id city.c_id in
  Mailbox.post_message game (piece.p_owner, message) ;;

let insert_piece_in_piece game piece container_piece =
  remove_piece_from_parent game piece ;
  piece.p_parent <- ContainerPiece container_piece.p_id ;
  Misc.Set.add container_piece.p_transport piece.p_id ;
  let message = Printf.sprintf "enter_piece %d %d" piece.p_id container_piece.p_id in
  Mailbox.post_message game (piece.p_owner, message) ;;

(* Suppression d'une piece. Il faut supprimer les pieces transportees qui se
   retireront a ce moment de la liste des pieces transportees, supprimer la
   piece du parent (avec mise a jour des vues si necessaire) et retirer la
   piece des listes de pieces. *)
let rec delete_piece game piece_id =
  let piece = Hashtbl.find game.g_pieces piece_id in
  while Misc.Set.size piece.p_transport > 0 do
    delete_piece game (Misc.Set.get piece.p_transport 0)
  done ;
  remove_piece_from_parent game piece ;
  let message = Printf.sprintf "delete_piece %d" piece.p_id in
  Mailbox.post_message game (piece.p_owner, message) ;
  Misc.Set.del_value game.g_players.(piece.p_owner).player_pieces piece.p_id ;
  Hashtbl.remove game.g_pieces piece.p_id ;;

let clear_city game city =
  match city.c_owner with
  | Some city_owner ->
      while Misc.Set.size city.c_transport > 0 do
        delete_piece game (Misc.Set.get city.c_transport 0)
      done ;
      let message = Printf.sprintf "lose_city %d" city.c_id in
      Mailbox.post_message game (city_owner, message) ;
      let around = Coords.tiles_around game city.c_visibility city.c_loc in
      View.dec_visibility game game.g_players.(city_owner) around ;
      Misc.Set.del_value game.g_players.(city_owner).player_cities city.c_id ;
      city.c_owner <- None ;
      city.c_production <- None
  | None -> () ;;

let own_city game city player_id =
  let player = game.g_players.(player_id) in
  let around = Coords.tiles_around game city.c_visibility city.c_loc in
  clear_city game city ;
  city.c_owner <- Some player_id ;
  Misc.Set.add player.player_cities city.c_id ;
  View.update_view game player city.c_loc ;
  View.inc_visibility game player around ;;

(* Deplacement de la piece en reduisant ses mouvements, la retirant du parent
   et en la placant sur le plateau. La visibilite est alors mise a jour. *)
let move_in_map game piece q r =
  let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
  let player = game.g_players.(piece.p_owner) in
  let visibility = piece_type.a_visibility in
  let around = Coords.tiles_around game visibility (q, r) in
  let message = Printf.sprintf "move %d %d %d" piece.p_id q r in
  check_piece_type_terrain piece_type game.g_map.(q).(r) ;
  remove_piece_from_parent game piece ;
  piece.p_rem_moves <- piece.p_rem_moves - 1 ;
  piece.p_parent <- ContainerTerrain (q, r) ;
  game.g_map_items.(q).(r) <- Some (Piece piece.p_id) ;
  Mailbox.post_message game (player.player_id, message) ;
  View.inc_visibility game player around ;;

(* Une piece entre dans une ville si elle appartient au meme joueur, sinon il
   s'agit d'une invasion qui a une chance sur deux de reussir, sous reserve que
   le terrain de la ville puisse accueillir la piece. La diminution du nombre
   de mouvements et le retrait aupres du parent ne se fait que si le mouvement
   peut reellement avoir lieu (en fonction du terrain par exemple). *)
let move_in_city game piece city_id =
  let city = Hashtbl.find game.g_cities city_id in
  let q, r = city.c_loc in
  let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
  (* Le nombre de mouvements restants n'est pas decremente ici car, dans le cas
     d'un deplacement vers une ville qui n'appartient pas au joueur, il faut
     egalement verifier la possibilite de la piece de se placer sur le terrain.
     Si ce n'est pas possible, il ne faut pas avoir decremente le nombre de
     mouvements. *) 
  if city.c_owner = Some piece.p_owner then begin
      (* Le deplacement dans une ville n'est possible que si il reste de la place
         dans la ville ! *)
      if Misc.Set.size city.c_transport = game.g_max_units_per_city then
        begin
          let message = Printf.sprintf "city-units-limit %d" city_id in
          Mailbox.post_message game (piece.p_owner, message)
        end
      else
        begin
          piece.p_rem_moves <- piece.p_rem_moves - 1 ;
          insert_piece_in_city game piece city
        end
    end else begin
      check_piece_type_terrain piece_type game.g_map.(q).(r) ;
      check_piece_can_invade game piece ;
      piece.p_rem_moves <- piece.p_rem_moves - 1 ;
      if Random.int 2 = 0 then begin
          (* Le message ok-invasion est envoye avant enter_city (cf. insert_piece_in_city). *)
          let message = Printf.sprintf "ok-invasion %d %d %d" city_id q r in
          Mailbox.post_message game (piece.p_owner, message) ;
          own_city game city piece.p_owner ;
          insert_piece_in_city game piece city
      end else
        begin
          (* Le message ko-invasion est envoye avant delete_piece (cf. delete_piece). *)
          let message = Printf.sprintf "ko-invasion %d %d %d" city_id q r in
          Mailbox.post_message game (piece.p_owner, message) ;
          delete_piece game piece.p_id
        end
    end ;;

(* Une piece rencontre une autre piece. Si elles sont du meme camps, la
   premiere se fait transporter si possible. Sinon, il y a combat. Si la
   premiere remporte le combat, elle prend la place de la seconde qui est alors
   supprimee, sinon la premiere est simplement supprimee. Si la premiere piece
   existe toujours, son nombre de mouvements est diminue. *)
let move_in_piece game piece s_piece_id =
  let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
  let s_piece = Hashtbl.find game.g_pieces s_piece_id in
  let s_piece_type = Hashtbl.find game.g_piece_types s_piece.p_type in
  if s_piece.p_owner = piece.p_owner then begin
      if List.mem piece_type.a_id s_piece_type.a_transportable then begin
          piece.p_rem_moves <- piece.p_rem_moves - 1 ;
          insert_piece_in_piece game piece s_piece
        end else begin
          let fmt = Printf.sprintf "error \"not (%d can transport %d)\"" in
          let message = fmt s_piece_id piece.p_id in
          Mailbox.post_message game (piece.p_owner, message)
        end
    end else begin
      let q, r = get_piece_loc game s_piece in
      check_piece_type_terrain piece_type game.g_map.(q).(r) ;
      (* TODO: faire plus deterministe ? *)
      while s_piece.p_hits > 0 && piece.p_hits > 0 do
        let who = Random.int 2 in (* 1 si c'est piece qui attaque sinon 0. *)
        s_piece.p_hits <- s_piece.p_hits - who * piece_type.a_strength ;
        piece.p_hits <- piece.p_hits - (1 - who) * s_piece_type.a_strength
      done ;
      if s_piece.p_hits > 0 then
          delete_piece game piece.p_id
        else begin
          (* On ne decremente pas le nombre de mouvements, cela sera fait par move_in_map. *)
          delete_piece game s_piece.p_id ;
          move_in_map game piece q r
        end
    end ;;

(* Partie actions du joueur. *)

let info_get_info_city game city_id =
  let city = get_city_by_city_id game city_id in
  check_city_owner city game.g_turn ;
  let production =
    match city.c_production with
      | None -> "n"
      | Some (a, b) -> Printf.sprintf "s:%d,%d" a b in
  let transport =
    let ids = Misc.Set.to_list city.c_transport in
    let ids = List.map soi ids in
    String.concat "#" ids in
  let q, r = city.c_loc in
  Printf.sprintf "%d %d %s %d %s" q r production city.c_visibility transport ;;

let info_get_info_piece game piece_id =
  let piece = get_piece_by_piece_id game piece_id in
  check_piece_owner piece game.g_turn ;
  let transport =
    let ids = Misc.Set.to_list piece.p_transport in
    let ids = List.map soi ids in
    String.concat "#" ids in
  let container =
    match piece.p_parent with
      | ContainerTerrain (q, r) -> Printf.sprintf "t:%d,%d" q r
      | ContainerCity a -> Printf.sprintf "c:%d" a
      | ContainerPiece a -> Printf.sprintf "p:%d" a in
  let autonomy =
    match piece.p_autonomy with
      | None -> "n"
      | Some a -> Printf.sprintf "s:%d" a in
  Printf.sprintf "%s %d %d %d %s %s" container piece.p_type piece.p_hits piece.p_rem_moves autonomy transport ;;

let info_get_info_piece_types game =
  let piece_types = game.g_piece_types in
  let fmt _ piece_type strings =
    let a_autonomy =
      match piece_type.a_autonomy with
        | None -> ""
        | Some i -> soi i in
    let terrain_to_string = function
      | Water -> "water"
      | Ground -> "ground" in
    let a_terrain = Misc.sep terrain_to_string ":" piece_type.a_terrain in
    let a_transportable = Misc.sep soi ":" piece_type.a_transportable in
    (Printf.sprintf "%d#%s#%c#%s#%d#%d#%d#%d#%d#%s#%s#%d#%b"
      piece_type.a_id          piece_type.a_name
      piece_type.a_symbol      a_terrain
      piece_type.a_build_time  piece_type.a_strength
      piece_type.a_max_hits    piece_type.a_speed
      piece_type.a_capacity    a_autonomy
      a_transportable          piece_type.a_visibility
      piece_type.a_can_invade) :: strings in
  String.concat ";" (Hashtbl.fold fmt piece_types []) ;;

(* Pour pouvoir se placer sur un carreau, l'unite doit pouvoir encore bouger
   durant le tour, le carreau de destination ne doit pas etre hors plateau,
   l'unite doit pouvoir se placer sur le terrain de ce carreau et le
   carreau de destination doit etre vide. *)
let info_can_move game piece_id direction_id =
  let piece = get_piece_by_piece_id game piece_id in
  let piece_type = get_piece_type_by_piece_type_id game piece.p_type in
  let old_loc = get_piece_loc game piece in
  let q, r = Coords.tile_neighbor direction_id old_loc in
  check_piece_owner piece game.g_turn ;
  piece.p_rem_moves > 0
    && Coords.in_map game (q, r)
    && List.mem game.g_map.(q).(r) piece_type.a_terrain
    && game.g_map_items.(q).(r) = None ;;

(* Pour pouvoir entrer dans un autre item, l'unite doit pouvoir encore bouger
   durant le tour, le carreau de destination ne doit pas etre hors plateau, le
   carreau de destination doit contenir un item allie qu idoit pouvoir
   accueillir l'unite. *)
let info_can_enter game piece_id direction_id =
  let piece = get_piece_by_piece_id game piece_id in
  let piece_type = get_piece_type_by_piece_type_id game piece.p_type in
  let old_loc = get_piece_loc game piece in
  let q, r = Coords.tile_neighbor direction_id old_loc in
  check_piece_owner piece game.g_turn ;
  if piece.p_rem_moves > 0 && Coords.in_map game (q, r) then
      match game.g_map_items.(q).(r) with
      | Some (Piece s_piece_id) ->
          let s_piece = Hashtbl.find game.g_pieces s_piece_id in
          let s_piece_type = Hashtbl.find game.g_piece_types s_piece.p_type in
          s_piece.p_owner = game.g_turn
            && List.mem piece_type.a_id s_piece_type.a_transportable
      | Some (City city_id) ->
          let city = Hashtbl.find game.g_cities city_id in
          city.c_owner = Some game.g_turn
      | _ -> false
    else false ;;

(* Pour pouvoir attaquer un item, une piece doit pouvoir encore bouger durant
   le tour, le carreau de destination ne doit pas etre hors plateau, la piece
   doit pouvoir se placer sur le terrain de ce carreau et le carreau de
   destination doit contenir un item ennemi. *)
let info_can_attack game piece_id direction_id =
  let piece = get_piece_by_piece_id game piece_id in
  let piece_type = get_piece_type_by_piece_type_id game piece.p_type in
  let old_loc = get_piece_loc game piece in
  let q, r = Coords.tile_neighbor direction_id old_loc in
  check_piece_owner piece game.g_turn ;
  let can_go =
    piece.p_rem_moves > 0 && Coords.in_map game (q, r)
      && List.mem game.g_map.(q).(r) piece_type.a_terrain in
  if can_go then
      match game.g_map_items.(q).(r) with
      | Some (City city_id) ->
          let city = Hashtbl.find game.g_cities city_id in
          city.c_owner = None || city.c_owner <> Some game.g_turn
      | Some (Piece s_piece_id) ->
          let s_piece = Hashtbl.find game.g_pieces s_piece_id in
          s_piece.p_owner <> game.g_turn
      | _ -> false
    else false ;;

let info_get_transported_pieces_by_loc game q r =
  check_loc game (q, r) ;
  check_visibility game.g_players.(game.g_turn) (q, r) ;
  match game.g_map_items.(q).(r) with
  | Some (City city_id) ->
      let city = Hashtbl.find game.g_cities city_id in
      check_city_owner city game.g_turn ;
      Misc.Set.to_list (city.c_transport)
  | Some (Piece piece_id) ->
      let piece = Hashtbl.find game.g_pieces piece_id in
      check_piece_owner piece game.g_turn ;
      Misc.Set.to_list (piece.p_transport)
  | _ -> raise (ActionError "no item at this location") ;;

let info_get_list_cities game =
  let player = game.g_players.(game.g_turn) in
  let fmt city_id =
    let city = Hashtbl.find game.g_cities city_id in
    let q, r = city.c_loc in
    city.c_id, q, r in
  List.map fmt (Misc.Set.to_list player.player_cities) ;;

let info_get_list_pieces game =
  let player = game.g_players.(game.g_turn) in
  let fmt piece_id =
    let piece = Hashtbl.find game.g_pieces piece_id in
    let q, r = get_piece_loc game piece in
    piece.p_id, q, r in
  List.map fmt (Misc.Set.to_list player.player_pieces) ;;

let info_get_list_movables game =
  let player = game.g_players.(game.g_turn) in
  let is_movable piece_id =
    let piece = Hashtbl.find game.g_pieces piece_id in
    piece.p_rem_moves > 0 in
  let fmt piece_id =
    let piece = Hashtbl.find game.g_pieces piece_id in
    let q, r = get_piece_loc game piece in
    piece.p_id, q, r in
  let pieces_ids = Misc.Set.to_list player.player_pieces in
  List.map fmt (List.filter is_movable pieces_ids) ;;

let info_get_city_id_by_loc game q r =
  check_loc game (q, r) ;
  check_visibility game.g_players.(game.g_turn) (q, r) ;
  match game.g_map_items.(q).(r) with
  | Some (City city_id) -> Some city_id
  | _ -> None ;;

let info_get_piece_id_by_loc game q r =
  check_loc game (q, r) ;
  check_visibility game.g_players.(game.g_turn) (q, r) ;
  match game.g_map_items.(q).(r) with
  | Some (Piece piece_id) -> Some piece_id
  | _ -> None ;;

let info_get_city_production game city_id =
  let city = get_city_by_city_id game city_id in
  check_city_owner city game.g_turn ;
  match city.c_production with
  | Some (remaining_work, piece_type_id) ->
      let piece_type = get_piece_type_by_piece_type_id game piece_type_id in
      Some (piece_type.a_build_time, remaining_work, piece_type_id)
  | None -> None ;;

(* Passage au tour du joueur suivant. Si il s'agit d'une fin de round, nous
   mettons a jour la production des villes, decrementons l'autonomie des pieces
   qui sont hors des villes et restaurons la sante et l'autonomie des pieces
   qui sont transportees. S'il s'agit d'une fin de tour, nous mettons
   simplement a jour les quotas de mouvements possibles du nouveau joueur. *)
let turn_end_turn game =
  game.g_turn <- game.g_turn + 1 ;
  if game.g_turn = game.g_nb_players then begin
      let pieces_to_delete = Queue.create () in
      (* Fonction de mise a jour de la production d'une ville. Il faut noter
         que la nouvelle unite est effectivement produite si il reste encore
         de la place dans la ville et si le joueur n'a pas atteint sont quota ! *)
      let update_city_round _ city =
        match city.c_owner, city.c_production with
        | _, None -> ()
        | Some owner, Some (1, piece_type_id) ->
            (* Recuperation du joueur pour tester et modifier le nombre d'unites qu'il a produit. *)
            let player = game.g_players.(owner) in
            (* Pas de production si le totale d'unites est atteint. *)
            if player.player_nb_created_units = game.g_max_nb_created_units_per_player then
                begin
                  let fmt = Printf.sprintf "created-units-limit %d" in
                  let message = fmt city.c_id in
                  Mailbox.post_message game (owner, message) ;
                end
              else
                begin
                  (* Une unite en plus est creee. Par contre, elle "meurt" directement si il n'y
                     a pas de places dans la ville ! *)
                  player.player_nb_created_units <- player.player_nb_created_units + 1 ;
                  if Misc.Set.size city.c_transport = game.g_max_units_per_city then
                      begin
                        city.c_production <- Some (0, piece_type_id) ;
                        let fmt = Printf.sprintf "city-units-limit %d" in
                        let message = fmt city.c_id in
                        Mailbox.post_message game (owner, message) ;
                      end
                    else
                      begin
                        let new_id = get_new_id game in
                        let piece_type = Hashtbl.find game.g_piece_types piece_type_id in
                        let piece =
                          { p_id = new_id
                          ; p_parent = ContainerCity city.c_id
                          ; p_owner = owner
                          ; p_type = piece_type_id
                          ; p_hits = piece_type.a_max_hits
                          ; p_rem_moves = piece_type.a_speed
                          ; p_transport = Misc.Set.of_array [||]
                          ; p_autonomy = piece_type.a_autonomy
                          } in
                        Misc.Set.add city.c_transport piece.p_id ;
                        Misc.Set.add game.g_players.(owner).player_pieces piece.p_id ;
                        Hashtbl.add game.g_pieces piece.p_id piece ;
                        city.c_production <- Some (0, piece_type_id) ;
                        let fmt = Printf.sprintf "create_piece %d %d %d %d" in
                        let message = fmt piece.p_id piece.p_type city.c_id piece.p_hits in
                        Mailbox.post_message game (piece.p_owner, message) ;
                      end
                end
(* TODO: fusionner ces deux cas? Pourquoi deux? *)
        | _, Some (0, piece_type_id) ->
            let piece_type = Hashtbl.find game.g_piece_types piece_type_id in
            city.c_production <- Some (piece_type.a_build_time, piece_type_id)
        | _, Some (n, piece_type_id) ->
            city.c_production <- Some (n - 1, piece_type_id) in
      let update_piece_round _ piece =
        match piece.p_parent with
        | ContainerTerrain (_, _) ->
            begin match piece.p_autonomy with
            | Some i when i < 0 -> Queue.add piece pieces_to_delete
            | Some i -> piece.p_autonomy <- Some (i - 1)
            | None -> ()
            end
        | _ ->
            let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
            piece.p_hits <- min piece_type.a_max_hits (piece.p_hits + 1) ;
            piece.p_autonomy <- piece_type.a_autonomy in
      game.g_turn <- 0 ;
      game.g_round <- game.g_round + 1 ;
      Hashtbl.iter update_city_round game.g_cities ;
      Hashtbl.iter update_piece_round game.g_pieces ;
      (* Lors de la suppression d'une piece, d'autres pieces peuvent egalement
         etre supprimees par lien de parente. Il faut, lors de la suppression
         d'une piece, verifier qu'elle ne l'a pas deja ete. *)
      let do_delete_piece _ piece =
        if Hashtbl.mem game.g_pieces piece.p_id then begin
            delete_piece game piece.p_id ;
            match piece.p_parent with
            | ContainerTerrain (q, r) -> update_players_view game (q, r)
            | _ -> ()
          end in
      Queue.fold do_delete_piece () pieces_to_delete
    end ;
  let update_piece_turn piece_id =
    let piece = Hashtbl.find game.g_pieces piece_id in
    let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
    piece.p_rem_moves <- piece_type.a_speed in
      (* ALTERNATIVE: (piece.p_hits * piece_type.a_speed + piece_type.a_max_hits) / piece_type.a_max_hits in *)
  ignore (List.map update_piece_turn
    (Misc.Set.to_list game.g_players.(game.g_turn).player_pieces)) ;;

let turn_dump_map game =
  let tile_to_string = function
  | Ground, None -> "\027[32;1m+\027[0m"
  | Water, None -> "\027[34;1m.\027[0m"
  | Ground, Some _ -> "\027[33;1m+\027[0m"
  | Water, Some _ -> "\027[35;1m.\027[0m" in
  for r = 0 to game.g_height - 1 do
    print_string (String.make r ' ') ;
    for q = 0 to game.g_width - 1 do
      let terrain = game.g_map.(q).(r) in
      let item = game.g_map_items.(q).(r) in
      print_string (tile_to_string (terrain, item))
    done ;
    print_string "\n"
  done ;
  print_endline "\n" ;;

let fog_off game =
  for r = 0 to game.g_height - 1 do
    for q = 0 to game.g_width - 1 do  
      View.inc_visibility game game.g_players.(game.g_turn) [ (q,r) ] ;
      (*   View.dec_visibility game game.g_players.(game.g_turn) [ (q,r) ] *)
    done ;
  done ;
  ()

let turn_set_city_production game city_id piece_type_id =
  let piece_type = get_piece_type_by_piece_type_id game piece_type_id in
  let city = get_city_by_city_id game city_id in
  check_city_owner city game.g_turn ;
  city.c_production <- Some (piece_type.a_build_time, piece_type_id) ;;

let turn_move game piece_id direction_id =
  let piece = get_piece_by_piece_id game piece_id in
  let old_loc = get_piece_loc game piece in
  let q, r = Coords.tile_neighbor direction_id old_loc in
  check_piece_owner piece game.g_turn ;
  check_piece_can_move piece ;
  check_loc game (q, r) ;
  begin match game.g_map_items.(q).(r) with
  | None -> move_in_map game piece q r
  | Some (City city_id) -> move_in_city game piece city_id
  | Some (Piece s_piece_id) -> move_in_piece game piece s_piece_id
  end ;
  (* Les fonctions move_in_XXX ne forcent pas le rafraichissement des cases
   * deja visibles (uniquement celles qui changent de visibilite). Du coup, si le
   * contenu change, il faut forcer la generation d'un evenement set_visible. Dans
   * cette situation, il est aussi simple de generer cet evenement pour les cases
   * de depart et d'arrivee, quoi qu'il arrive. Cela risque, par contre de generer
   * deux fois, dans certains cas l'evenement set_visible (dans une zone visible
   * uniquement par la piece qui bouge, un retrait de la piece de la case de depart
   * genere un evenement set_visible_xxx_none, l'arrivee genere un evenement
   * set_visible_xxx_piece et ce meme evenement est regenere par les deux fonctions
   * suviantes). XXX Toutefois, il est necessaire de generer au moins une fois l'evenement,
   * meme si l'unite sur la case n'a pas change car ses points de vie peuvent, eux,
   * avoir changes.
   *)
  update_players_view game old_loc ;
  update_players_view game (q, r) ;;

let turn_moves game piece_id q r =
  let piece = get_piece_by_piece_id game piece_id in
  let piece_type = Hashtbl.find game.g_piece_types piece.p_type in
  let old_loc = get_piece_loc game piece in
  let heuristic = Coords.tiles_distance in
  let neighbors (q_loc, r_loc) =
    (* XXX Printf.printf "neighbors: %d %d\n" q_loc r_loc ;*)
    let test_direction neighbors (q_delta, r_delta) =
      let (q1, r1) as loc = q_delta + q_loc, r_delta + r_loc in
      (* Ce voisin n'est pas considere si il est hors de la carte ou si la piece ne peut pas marcher sur
       * ce terrain ou si il y a quelque chose alors que ce n'est pas la destination finale (q, r) ou
       * encore si cette position n'est pas visible par le joueur.
       *)
      if not (Coords.in_map game loc) ||
         not (View.is_visible game.g_players.(game.g_turn) loc) ||
         not (List.mem game.g_map.(q1).(r1) piece_type.a_terrain) ||
         (game.g_map_items.(q1).(r1) <> None && loc <> (q, r))
      then neighbors else (q1, r1) :: neighbors in
    Array.fold_left test_direction [] Coords.directions in
  let cost _ _ = 1 in
  let path = Astar.astar_goal old_loc (q, r) piece.p_rem_moves neighbors cost heuristic in
  let rec do_path = function
    | [] -> ()
    | h :: t ->
        begin
          (* La piece bouge, il faut recuperer ses nouvelles coordonnees. *)
          let loc = get_piece_loc game piece in
          let direction = Coords.tile_delta loc h in
          let direction_id = Coords.direction_to_direction_id direction in
          turn_move game piece_id direction_id ;
          do_path t
        end in
  match path with
    | None -> raise (ActionError "impossible moves")
    | Some l -> do_path l
