open Connection ;;

exception StubError of string ;;

let get_piece_types_names connection =
  let response = ask_to_server connection "get_piece_types_names" in
  let fmt fields =
    if List.length fields <> 2 then raise (StubError "invalid format") ;
    int_of_string (List.nth fields 0), List.nth fields 1 in
  List.map fmt (List.map (Misc.split ':') (Misc.split ',' response)) ;;

let get_city_production connection city_id =
  let request = Printf.sprintf "get_city_production %d" city_id in
  let response = ask_to_server connection request in
  if response <> "none" then begin
      let fields = Misc.split ':' response in
      if List.length fields <> 3 then raise (StubError "invalid format") ;
      let fields = List.map int_of_string fields in
      Some (List.nth fields 0, List.nth fields 1, List.nth fields 2)
    end else None ;;

let set_city_production connection city_id piece_type_id =
  let fmt = Printf.sprintf "set_city_production %d %d" in
  send_to_server connection (fmt city_id piece_type_id) ;;

let can_move connection piece_id direction_id =
  let request = Printf.sprintf "can_move %d %d" piece_id direction_id in
  (ask_to_server connection request) = "true" ;;

let can_enter connection piece_id direction_id =
  let request = Printf.sprintf "can_enter %d %d" piece_id direction_id in
  (ask_to_server connection request) = "true" ;;

let can_attack connection piece_id direction_id =
  let request = Printf.sprintf "can_attack %d %d" piece_id direction_id in
  (ask_to_server connection request) = "true" ;;

let move_piece connection piece_id direction_id =
  let request = Printf.sprintf "move %d %d" piece_id direction_id in
  send_to_server connection request ;;

let get_list_cities connection =
  let fmt s =
    let fields = List.map int_of_string (Misc.split ':' s) in
    if List.length fields <> 3 then raise (StubError "invalid format") ;
    List.nth fields 0, (List.nth fields 1, List.nth fields 2) in
  let response = ask_to_server connection "get_list_cities" in
  List.map fmt (Misc.split ',' response) ;;

let get_list_movables connection =
  let fmt s =
    let fields = List.map int_of_string (Misc.split ':' s) in
    if List.length fields <> 3 then raise (StubError "invalid format") ;
    List.nth fields 0, (List.nth fields 1, List.nth fields 2) in
  let response = ask_to_server connection "get_list_movables" in
  List.map fmt (Misc.split ',' response) ;;

let get_list_pieces connection =
  let fmt s =
    let fields = List.map int_of_string (Misc.split ':' s) in
    if List.length fields <> 3 then raise (StubError "invalid format") ;
    List.nth fields 0, (List.nth fields 1, List.nth fields 2) in
  let response = ask_to_server connection "get_list_pieces" in
  List.map fmt (Misc.split ',' response) ;;

let get_width connection =
  int_of_string (ask_to_server connection "get_width") ;;

let get_height connection =
  int_of_string (ask_to_server connection "get_height") ;;

let get_piece_id_by_loc connection q r =
  let request = Printf.sprintf "get_piece_id_by_loc %d %d" q r in
  let response = ask_to_server connection request in
  if response = "none" then None else Some (int_of_string response) ;;

let get_transported_names_by_loc connection q r =
  let fmt s =
    let fields = Misc.split ':' s in
    if List.length fields <> 2 then raise (StubError "invalid format") ;
    int_of_string (List.nth fields 0), List.nth fields 1 in
  let request = Printf.sprintf "get_transported_names_by_loc %d %d" q r in
  let response = ask_to_server connection request in
  List.map fmt (Misc.split ',' response) ;;

let end_turn connection = send_to_server connection "end_turn" ;;

let dump_map connection = send_to_server connection "dump_map" ;;

let get_city_id_by_loc connection q r =
  let request = Printf.sprintf "get_city_id_by_loc %d %d" q r in
  let response = ask_to_server connection request in
  if response = "none" then None else Some (int_of_string response) ;;
