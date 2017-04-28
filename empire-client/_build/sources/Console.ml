open Curses ;;

open Empire ;;

(* La derniere ligne de la console est dediee aux saisies. *)

let clear_console console = wclear console.console_window ;;

let print_console console =
  let h, w = getmaxyx console.console_window in
  let rec process_messages n messages styles =
    if n >= 0 && List.length messages > 0 then begin
        let s = List.hd styles in
        let m = List.hd messages in
        let m = if String.length m > w then String.sub m 0 w else m in
	(* Affichage d'une ligne vide pour effacer le message precedent. *)
        assert (wmove console.console_window n 0) ;
        wclrtoeol console.console_window ;
	(* Affichage du nouveau message. *)
        let fmt i = int_of_char m.[i] + A.color_pair s in
        let str = Array.init (String.length m) fmt in
        assert (mvwaddchstr console.console_window n 0 str) ;
        process_messages (n - 1) (List.tl messages) (List.tl styles)
      end in
  assert (wmove console.console_window (h - 1) 0) ;
  wclrtoeol console.console_window ;
  process_messages (h - 2) console.messages console.styles ;
  assert (wrefresh console.console_window) ;;

let add_info_message console message =
  console.messages <- message :: console.messages ;
  console.styles <- 12 :: console.styles ;
  print_console console ;;

let add_error_message console message =
  console.messages <- message :: console.messages ;
  console.styles <- 13 :: console.styles ;
  print_console console ;;

let add_message console message =
  console.messages <- message :: console.messages ;
  console.styles <- 11 :: console.styles ;
  print_console console ;;

let ask_continue console =
  add_message console "Press 'n' to continue" ;
  while getch () <> int_of_char 'n' do () done ;;

let rec get_key () =
  let key = getch () in
  if key = -1 then get_key () else key ;;

let menu console id values callback =
  (* Affichage de l'aide. *)
  add_message console "[n]ext value; [p]revious value; [v]alidate; [e]scape" ;
  (* Recuperation et test du nombre de valeurs du menu. *)
  let nb_values = List.length values in
  assert (nb_values > 0) ;
  (* Recuperation de la taille de la console. *)
  let h, _ = getmaxyx console.console_window in
  (* Fonction d'affichage d'une valeur. *)
  let print_value id =
    let value = List.nth values id in
    assert (wmove console.console_window (h - 1) 0) ;
    wclrtoeol console.console_window ;
    assert (mvwaddstr console.console_window (h - 1) 0 value) ;
    assert (wrefresh console.console_window) in
  (* Boucle de saisie du menu. *)
  let rec ask id =
    let roll step =
        let new_id = (id + step + nb_values) mod nb_values in
        print_value new_id ;
        callback new_id ;
        ask new_id in
    let key = getch () in
    if key = int_of_char 'n' then roll (+1) else
    if key = int_of_char 'p' then roll (-1) else
    if key = int_of_char 'v' then begin
        add_message console (List.nth values id) ;
        Some id
      end else
    if key = int_of_char 'e' then begin
        add_message console "none" ;
        None
      end else
    ask id in
  print_value id ;
  callback id ;
  ask id ;;

let update_console console =
  print_console console ;;
