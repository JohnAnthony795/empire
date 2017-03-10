open Unix ;;

exception ConnectionError of string ;;

let read_server connection =
  let readers, _, _ = Unix.select [connection] [] [] 0.0 in
  if List.length readers > 0 then begin
      let s = " " in
      let r = ref "" in
      (* TODO: read connection.socket s 0 1<> 1 ? *)
      while (read connection s 0 1 > 0) && s.[0] <> '\n' do
        r := !r ^ s
      done ;
      Some !r
    end else None ;;

let wait_server_message connection =
  let s = " " in
  let r = ref "" in
  (* TODO: read connection.socket s 0 1<> 1 ? *)
  while (read connection s 0 1 > 0) && s.[0] <> '\n' do
    r := !r ^ s
  done ;
  !r ;;

let send_to_server connection message =
  let oc = Unix.out_channel_of_descr connection in
  output_string oc (message ^ "\n") ;
  flush oc ;;

let ask_to_server connection request =
  send_to_server connection request ;
  let response = wait_server_message connection in
  if wait_server_message connection <> "get_action" then
    raise (ConnectionError "get_action expected") ;
  let len_response = String.length response in
  let len_err = String.length "error " in
  if len_response >= len_err && Misc.str_start response len_err = "error " then
    raise (ConnectionError response) ;
  response ;;

let rec procces_until_get_action connection callback =
  let message = wait_server_message connection in
  if message <> "get_action" then begin
      callback message ;
      procces_until_get_action connection callback
    end ;;

let create_connection server port =
  let server_addr = (gethostbyname server).h_addr_list.(0) in
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.connect socket (ADDR_INET(server_addr, port)) ;
  socket ;;
