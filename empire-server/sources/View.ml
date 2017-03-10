open Empire ;;

let string_of_terrain = function
  | Water -> "water"
  | Ground -> "ground" ;;

let item_to_string game item =
  match item with
  | Some (City city_id) ->
      let city = Hashtbl.find game.g_cities city_id in
      begin match city.c_owner with
      | Some i -> Printf.sprintf "owned_city %d %d" city_id i
      | None -> Printf.sprintf "city %d" city_id
      end
  | Some (Piece piece_id) ->
      let piece = Hashtbl.find game.g_pieces piece_id in
      Printf.sprintf "piece %d %d %d %d" piece.p_owner piece_id piece.p_type piece.p_hits
  | None -> "none" ;;

let update_view game player (q, r) =
  match player.player_view.(q).(r) with
  | Visible (n, terrain, _) ->
      (* Le mouvement a change le contenu du carreau. Il faut avertir ceux
         ont une visibilite sur le carreau. Attention, si il s'agit d'une ville
         et qu'elle etait donc deja visible, l'item ne change pas mais le
         proprietaire change ! On ne peut donc pas simplement tester l'egalite
         entre l'item de la vue et l'item de la carte : ce sera forcement les
         memes avec un changement du champs mutable owner ! *)
      let item = game.g_map_items.(q).(r) in
      let terrain_str = string_of_terrain terrain in
      Mailbox.post_message game (player.player_id,
        Printf.sprintf "set_visible %d %d %s %s"
          q r terrain_str (item_to_string game item)) ;
      player.player_view.(q).(r) <- Visible (n, terrain, item)
  | _ -> () ;;

let is_visible player (q, r) =
  match player.player_view.(q).(r) with                                         
  | Visible (_, _, _) -> true
  | _ -> false ;;

let inc_visibility game player locs =
  let f (q, r) =
    match player.player_view.(q).(r) with
    | Visible (n, terrain, item) ->
        player.player_view.(q).(r) <- Visible (n + 1, terrain, item)
    | _ -> (* Unknown ou Explored *)
        let item = game.g_map_items.(q).(r) in
        let terrain = game.g_map.(q).(r) in
        let terrain_str = string_of_terrain terrain in
        Mailbox.post_message game (player.player_id,
          Printf.sprintf "set_visible %d %d %s %s"
            q r terrain_str (item_to_string game item)) ;
        player.player_view.(q).(r) <- Visible (1, terrain, item) in
  ignore (List.map f locs) ;;

let dec_visibility game player locs =
  let f (q, r) =
    match player.player_view.(q).(r) with
    | Visible (n, terrain, _) when n = 1 ->
        let terrain_str = string_of_terrain terrain in
        Mailbox.post_message game (player.player_id,
          Printf.sprintf "set_explored %d %d %s"
            q r terrain_str) ;
        player.player_view.(q).(r) <- Explored terrain
    | Visible (n, terrain, item) ->
        player.player_view.(q).(r) <- Visible (n - 1, terrain, item)
    | _ -> failwith "can't decrease visibility" in
  ignore (List.map f locs) ;;
