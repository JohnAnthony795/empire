open Unix ;;

type connection =
  { socket : file_descr
  ; in_channel : in_channel
  ; out_channel : out_channel
  } ;;

let server_open max_listen inet_addr port =
  let saddr = Unix.ADDR_INET (inet_addr, port) in
  let ssock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* SO_REUSEADDR pour pouvoir redemarrer rapidement le serveur. *)
  Unix.setsockopt ssock Unix.SO_REUSEADDR true ;
  Unix.bind ssock saddr ;
  Unix.listen ssock max_listen ;
  ssock ;;

let read_client connection =
  input_line connection.in_channel ;;

let close sock =
  try Unix.shutdown sock Unix.SHUTDOWN_ALL with _ -> () ;
  Unix.close sock ;;
