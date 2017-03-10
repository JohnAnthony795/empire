open Curses ;;

open Connection ;;
open Console ;;
open Empire ;;
open Misc ;;
open Stub ;;
open Tilemap ;;

let create_tile_to_pair () =
  let tile_to_pair = Hashtbl.create 16 in
  let colors =
    [ 1, Color.blue, Color.black, false, false, None
    ; 2, Color.green, Color.black, false, true, None
    ; 3, Color.white, Color.blue, true, false, None
    ; 4, Color.white, Color.green, true, true, None
    ; 5, Color.yellow, Color.blue, true, false, Some 0
    ; 6, Color.yellow, Color.green, true, true, Some 0
    ; 7, Color.red, Color.blue, true, false, Some 1
    ; 8, Color.red, Color.green, true, true, Some 1
    ; 9, Color.magenta, Color.blue, true, false, Some 2
    ; 10, Color.magenta, Color.green, true, true, Some 2
    ; 15, Color.white, Color.black, false, false, Some 2
    ] in
  let f (i, fg, bg, is_visible, is_ground, player) =
    assert (init_pair i fg bg) ;
    Hashtbl.add tile_to_pair (is_visible, is_ground, player) i in
  List.iter f colors ;
  tile_to_pair ;;

let create_console_pair () =
  assert (init_pair 11 Color.white Color.black) ;
  assert (init_pair 12 Color.green Color.black) ;
  assert (init_pair 13 Color.red Color.black) ;;

let get_configuration connection =
  (* XXX: L'ordre de reception des messages de configuration est important. *)
  let get_int key =
    let message = wait_server_message connection in
    let fmt = Scanf.sscanf message "%s %d" in
    let k, v = fmt (fun k v -> k, v) in
    assert (k = key) ;
    v in
  let get_str key =
    let message = wait_server_message connection in
    let fmt = Scanf.sscanf message "%s %s" in
    let k, v = fmt (fun k v -> k, v) in
    assert (k = key) ;
    v in
  let width = get_int "width" in
  let height = get_int "height" in
  let player_id = get_int "player_id" in
  let piece_types = get_str "piece_types" in
  let fmt str =
    let fields = split '#' str in
    Printf.printf "GET %s" str ;
    int_of_string (List.nth fields 0), Char.code (List.nth fields 2).[0] in
  let piece_symbols = List.map fmt (split ';' piece_types) in
  width, height, piece_symbols, player_id

let configure window server port =
  let connection = create_connection server port in
  let width, height, piece_symbols, player_id = get_configuration connection in
  create_console_pair () ;
  let map =
    { map_window = newwin 0 0 0 0
    ; viewport_q = 0
    ; viewport_r = 0
    ; viewport_w = width
    ; viewport_h = height
    ; current_q = 0
    ; current_r = 0
    ; step_scale_q = 10
    ; step_scale_r = 2
    ; offset_q = int_of_float (floor (log10 (float_of_int height) +. 1.0))
    ; offset_r = 1
    ; view = Array.make_matrix width height (int_of_char ' ', A.normal, 15)
    ; tile_to_pair = create_tile_to_pair ()
    ; width = width
    ; height = height
    } in
  let console =
    { console_window = newwin 0 0 0 0
    ; histlen = 16
    ; console_height = 7
    ; messages = []
    ; styles = []
    } in
  let game =
    { window = window
    ; action = false
    ; map = map
    ; console = console
    ; connection = connection
    ; stop = false
    ; end_message = ""
    ; player_id = player_id
    ; observer = false
    ; piece_symbols = piece_symbols
    } in
  let rec parse_opt game i =
    if i >= Array.length Sys.argv then game else
    if Sys.argv.(i) = "-sport" then parse_opt game (i + 2) else
    if Sys.argv.(i) = "-obs" then { game with observer = true } else
    failwith ("invalid option: " ^ Sys.argv.(i)) in
  parse_opt game 1 ;;

let update_windows game =
  let h, w = getmaxyx game.window in
  let console_height = game.console.console_height in
  let h_map = h - console_height in
  assert (h_map > 10 && w > 10) ; (* Test la taille minimale. *)
  assert (wresize game.map.map_window h_map w) ;
  assert (wresize game.console.console_window console_height w) ;
  assert (mvwin game.map.map_window 0 0) ;
  assert (mvwin game.console.console_window (h - console_height) 0) ;
  resize_map game.map ;
  update_console game.console ;
  assert (wrefresh game.window) ;;

let debug_with_curses game message =
  assert (mvwaddstr game.window 0 0 message) ;
  assert (wrefresh game.window) ;;

let handle_info_list_cities game =
  let cities = get_list_cities game.connection in
  if List.length cities = 0 then
      add_error_message game.console "no cities"
    else begin
      let str_cities =
        List.map
          (fun (id, (q, r)) -> Printf.sprintf "%d:(%d,%d)" id q r) cities in
      let focus_city i =
        let q, r = snd (List.nth cities i) in
        center_map_on game.map q r in
      ignore (menu game.console 0 str_cities focus_city)
    end ;;

let handle_info_list_pieces game =
  let pieces = get_list_pieces game.connection in
  if List.length pieces = 0 then
      add_error_message game.console "no pieces"
    else begin
      let str_pieces =
        List.map
          (fun (id, (q, r)) -> Printf.sprintf "%d:(%d,%d)" id q r) pieces in
      let focus_piece i =
        let q, r = snd (List.nth pieces i) in
        center_map_on game.map q r in
      ignore (menu game.console 0 str_pieces focus_piece)
    end ;;

let handle_info_list_movables game =
  let movables = get_list_movables game.connection in
  if List.length movables = 0 then
      add_error_message game.console "no pieces to move"
    else begin
      let str_movables =
        List.map
          (fun (id, (q, r)) -> Printf.sprintf "%d:(%d,%d)" id q r) movables in
      let focus_piece i =
        let q, r = snd (List.nth movables i) in
        center_map_on game.map q r in
      ignore (menu game.console 0 str_movables focus_piece)
    end ;;

let handle_action_move_piece game direction_id =
  let q, r = game.map.current_q, game.map.current_r in
  let piece_id = get_piece_id_by_loc game.connection q r in
  match piece_id with
  | None -> add_error_message game.console "no pieces at this location"
  | Some piece_id -> move_piece game.connection piece_id direction_id ;;

let handle_action_move_transported game =
  let q, r = game.map.current_q, game.map.current_r in
  let pieces = get_transported_names_by_loc game.connection q r in
  if List.length pieces = 0 then
      add_error_message game.console "no transported pieces at this location"
    else begin
      let piece_to_str (a, b) = Printf.sprintf "%d:%s" a b in
      let str_pieces = List.map piece_to_str pieces in
      let nothing _ = () in
      let choice = menu game.console 0 str_pieces nothing in
      match choice with
      | None -> ()
      | Some choice ->
          let piece = fst (List.nth pieces choice) in
          let key = get_key () in
          if key = int_of_char 'd' then move_piece game.connection piece 0 else
          if key = int_of_char 'w' then move_piece game.connection piece 1 else
          if key = int_of_char 'q' then move_piece game.connection piece 2 else
          if key = int_of_char 'a' then move_piece game.connection piece 3 else
          if key = int_of_char 'x' then move_piece game.connection piece 4 else
          if key = int_of_char 'c' then move_piece game.connection piece 5 else
          add_error_message game.console "bad direction"
    end ;;

let handle_action_end_turn game =
  if game.action then begin
      game.action <- false ;
      end_turn game.connection
    end else
      add_error_message game.console "not your turn" ;;

let handle_action_next_movables game =
  let movables = get_list_movables game.connection in
  if List.length movables = 0 then
      add_error_message game.console "no movable pieces"
    else begin
      let piece = fst (List.hd movables) in
      let key = get_key () in
      if key = int_of_char 'd' then move_piece game.connection piece 0 else
      if key = int_of_char 'w' then move_piece game.connection piece 1 else
      if key = int_of_char 'q' then move_piece game.connection piece 2 else
      if key = int_of_char 'a' then move_piece game.connection piece 3 else
      if key = int_of_char 'x' then move_piece game.connection piece 4 else
      if key = int_of_char 'c' then move_piece game.connection piece 5 else
      add_error_message game.console "bad direction"
    end ;;

let handle_action_dump_map game =
  if game.action then begin
      dump_map game.connection
    end else
      add_error_message game.console "not your turn" ;;

let handle_action_set_city_production game =
  if game.action then begin
      let q, r = game.map.current_q, game.map.current_r in
      match get_city_id_by_loc game.connection q r with
      | None -> add_error_message game.console "not a city owned"
      | Some city_id ->
          let piece_types_names = get_piece_types_names game.connection in
          assert (List.length piece_types_names > 0) ;
          let values = List.map snd piece_types_names in
          let production = get_city_production game.connection city_id in
          let production =
            match production with
            | None -> 0
            | Some production ->
                findi piece_types_names
                  (fun (i, _) -> i = (let _, _, j = production in j)) in
          let nothing _ = () in
          let choice = menu game.console production values nothing in
          match choice with
          | None -> ()
          | Some choice ->
              let piece_type_id =
                fst (List.nth piece_types_names choice) in
              set_city_production game.connection city_id piece_type_id 
    end else
      add_error_message game.console "not your turn" ;;

let differed_reprint = ref 0 ;;

let handle_message_set_visible game message =
  let fmt = Scanf.sscanf message "set_visible %d %d %s %s %n" in
  let q, r, terrain_str, item, i = fmt (fun a b c d e -> a, b, c, d, e) in
  let get_pair = Hashtbl.find game.map.tile_to_pair in
  begin match item with
  | "none" ->
      let is_ground = terrain_str = "ground" in
      let symbol = if is_ground then int_of_char '+' else int_of_char '.' in
      let pair = get_pair (true, is_ground, None) in
      game.map.view.(q).(r) <- symbol, A.normal, pair ;
  | "city" ->
      let pair = get_pair (true, true, None) in
      game.map.view.(q).(r) <- int_of_char 'O', A.bold, pair ;
  | "owned_city" ->
      let message = str_end message i in
      let owner = Scanf.sscanf message "%d %d" (fun _ b -> b) in
      let pair = get_pair (true, true, Some owner) in
      game.map.view.(q).(r) <- int_of_char 'O', A.bold, pair ;
  | "piece" ->
      let message = str_end message i in
      let fmt = Scanf.sscanf message "%d %d %d %d" in
      (* XXX: unused piece_id and piece_hits *)
      let owner, _, piece_type_id, _ = fmt (fun a b c d -> a, b, c, d) in
      let symbol = List.assoc piece_type_id game.piece_symbols in
      let is_ground = terrain_str = "ground" in
      let pair = get_pair (true, is_ground, Some owner) in
      game.map.view.(q).(r) <- symbol, A.bold, pair ;
  | _ ->
      add_error_message game.console ("not handled: " ^ message) ;
  end ;
  if game.action && not game.observer then reprint_map game.map q r else incr differed_reprint ;;

let handle_message_set_explored game message =
  let fmt = Scanf.sscanf message "set_explored %d %d %s" in
  let q, r, terrain_str = fmt (fun a b c -> a, b, c) in
  let get_pair = Hashtbl.find game.map.tile_to_pair in
  let is_ground = terrain_str = "ground" in
  let symbol = if is_ground then int_of_char '+' else int_of_char '.' in
  game.map.view.(q).(r) <- symbol, A.normal, get_pair (false, is_ground, None) ;
  if game.action && not game.observer then reprint_map game.map q r else incr differed_reprint ;;

let handle_message_error game message =
  if not game.observer then
    let message = Scanf.sscanf message "error %S" (fun a -> a) in
    add_error_message game.console message ;;

(* Pour eviter de faire scintiller l'affichage en raison d'un grand nombre de reprint/center_map/etc.,
 * ce message n'est traite que lorsque c'est un joueur non-observer qui joue et que c'est son tour.
 * On peut donc esperer qu'il joue assez lentement pour ne pas faire trop scintiller l'affichage.
 *)
let handle_message_move game message =
  if game.action && not game.observer then
    let q, r = Scanf.sscanf message "move %d %d %d" (fun _ a b -> a, b) in
    center_map_on game.map q r ;;

let handle_message_get_action game =
  (* Finalement, meme si on est oberveur, on intervient dans les actions end_turn.
   ( Cela permet de controler l'avancement de l'IA. *)
  (* if not game.observer then add_message game.console "action?" ; *)
  add_message game.console "action?" ;
  game.action <- true ;;

let handle_message_ko_invasion game message =
  if not game.observer then
    let q, r = Scanf.sscanf message "ko-invasion %d %d" (fun a b -> a, b) in
    let message = Printf.sprintf "invasion of city at (%d, %d) failed" q r in
    add_info_message game.console message ;;

let handle_message_ok_invasion game message =
  if not game.observer then
    let q, r = Scanf.sscanf message "ok-invasion %d %d" (fun a b -> a, b) in
    let message = Printf.sprintf "invasion of city at (%d, %d) successful" q r in
    add_info_message game.console message ;;

let handle_message_draw game =
  let message = "no winner" in
  game.end_message <- message ;
  add_info_message game.console message ;
  game.stop <- true ;;

let handle_message_winner game message =
  let a = Scanf.sscanf message "winner %d" (fun a -> a) in
  let message = Printf.sprintf "winner is player %d" a in
  game.end_message <- message ;
  add_info_message game.console message ;
  game.stop <- true ;;

let process_server_message game message =
  match List.hd (split ' ' message) with
  | "set_visible" -> handle_message_set_visible game message
  | "set_explored" -> handle_message_set_explored game message
  | "get_action" -> handle_message_get_action game
  | "ok-invasion" -> handle_message_ok_invasion game message
  | "ko-invasion" -> handle_message_ko_invasion game message
  | "error" -> handle_message_error game message
  | "move" -> handle_message_move game message
  | "winner" -> handle_message_winner game message
  | "draw" -> handle_message_draw game
  | _ -> if not game.observer then add_error_message game.console ("message not handled: " ^ message) ;;

let rec process_server_messages game =
  try
    match read_server game.connection with
    | Some message ->
      process_server_message game message ;
      (* Le message qui vient juste d'etre traite peut correspondre a une
       * fin de partie. A ce moment, le serveur est clot et il ne faut plus lire
       * de messages. Du coup, nous testons le booleen game.stop.
       *)
      if not game.stop
      then process_server_messages game
    | None -> ()
  with ConnectionError message ->
    begin
      handle_message_error game message ;
      process_server_messages game
    end ;;
  
let process_user_key game ch =
  (* Affichage du message pour indiquer au joueur que la touche pressee est en cours de traitement. *)
  if ch <> -1 then add_message game.console (Printf.sprintf "processing key '%c'" (Char.chr ch)) ;
  (*add_message game.console (Printf.sprintf "processing key '%c'" (Char.chr ch)) ;*)
  if ch = -1 then () else
  if ch = 27 then game.stop <- true else
  if ch = Key.resize then update_windows game else
  if ch = Key.down then move_viewport game.map (0, 1) (* fleche bas *) else
  if ch = Key.up then move_viewport game.map (0, -1) (* fleche haut *) else
  if ch = Key.left then move_viewport game.map (-1, 0) (* fleche gauche *) else
  if ch = Key.right then move_viewport game.map (1, 0) (* fleche droite *) else
  if ch = int_of_char 'r' then print_map game.map else
  if game.action then begin
    try
      if ch = int_of_char 'e' then handle_action_end_turn game else
      if not game.observer then begin
        if ch = int_of_char 'p' then handle_info_list_pieces game else
        if ch = int_of_char 'l' then handle_info_list_cities game else
        if ch = int_of_char 'm' then handle_info_list_movables game else
        (* factory *)
        if ch = int_of_char 'f' then handle_action_set_city_production game else
        if ch = int_of_char 'n' then handle_action_next_movables game else
        if ch = int_of_char 'M' then handle_action_dump_map game else
        if ch = int_of_char 'd' then handle_action_move_piece game 0 else
        if ch = int_of_char 'w' then handle_action_move_piece game 1 else
        if ch = int_of_char 'q' then handle_action_move_piece game 2 else
        if ch = int_of_char 'a' then handle_action_move_piece game 3 else
        if ch = int_of_char 'x' then handle_action_move_piece game 4 else
        if ch = int_of_char 'c' then handle_action_move_piece game 5 else
        if ch = int_of_char 't' then handle_action_move_transported game else
        add_error_message game.console "bad key"
      end else
          add_error_message game.console "bad key"
    with ConnectionError message ->
      begin
        handle_message_error game message ;
        process_server_messages game
      end
  end else
      add_error_message game.console "bad key" ;;

let setup_curses () =
  Sys.set_signal Sys.sigint Sys.Signal_ignore ;
  let window = initscr () in
  assert (start_color ()) ;
  assert (use_default_colors ()) ;
  assert (init_pair 1 Color.white Color.red) ;
  assert (cbreak ()) ;
  assert (noecho ()) ;
  assert (intrflush window false) ;
  assert (keypad window true) ;
  assert (curs_set 0) ; (* hide the cursor. *)
  winch_handler_on () ;
  timeout 1 ;
  nonl () ;
  window ;;

let minisleep (sec: float) =
  ignore (Unix.select [] [] [] sec) ;;

(* TODO: Voir si un mutex doit proteger les fonctions reprint/etc. *)
let thread_gui game =
  let rec fun_gui () =
    let differed_reprint_save = !differed_reprint in
    Thread.delay 1.0 ;
    if differed_reprint_save <> !differed_reprint then print_map game.map ;
    fun_gui () in
  Thread.create fun_gui ()

let main () =
  let server_port =
    let rec loop i =
      if i >= Array.length Sys.argv - 1 then 9301 else
      if Sys.argv.(i) = "-sport" then int_of_string (Sys.argv.(i + 1)) else
      loop (i + 1) in
    loop 0 in
  let window = setup_curses () in
  let game = configure window "localhost" server_port in
  let _ = thread_gui game in
  update_windows game ;
  print_map game.map ;
  add_info_message game.console (Printf.sprintf "Welcome player %d" game.player_id) ;
  while not game.stop do
    process_server_messages game ;
    process_user_key game (getch ())
  done ;
  endwin () ;
  Printf.printf "%s\n" game.end_message ;;

main () ;;
