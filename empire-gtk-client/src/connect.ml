open Events
open Common.Small
open Myscan

open Ebase

module Log = Mylog.MkSection (struct let section_name = __FILE__ end)
open Log

exception Scan_error of exn

type rcv_msg =
  | Connection_active of bool
  | Expect_end_turn
  | Set_explored of hxy * tile
  | Set_visible of hxy * tile * (item option)
  | Move of piece_id * hxy
  | Create_piece of piece_id * piece_type_id * city_id * hit_points
  | Delete_piece of piece_id
  | Invasion of bool * city_id * hxy
  | Date of int
  | Error of string

type t =
  { rcv_prod: rcv_msg prod ;
    emit: 'a . ('a, unit, string, unit Lwt.t) format4 -> 'a ;
    read: 'b . string -> 'b Myscan.t -> 'b Lwt.t ;
    ready: bool ref ; (* Cannot use a mutable field: this reference must be created before the record. *)
    observe: bool ;
    mutable date: int }

type config =
  { width: int ;
    height: int ;
    player_id: int ;
    ptypes: piece_type list ;
    random: int ;
    connection: t }

type sync_msg =
  | Gui_ready
  | Config of config

type intf =
  { raw_received: string prod ;
    raw_emitted:  string prod ;
    rcv_msg: rcv_msg prod ;
    board: sync_msg Lwtboard.t }

open Lwt_unix
open Lwt_io

let is_ready cn = (not cn.observe) && !(cn.ready)

(* silent: do not emit 'ready' event when get_action is received (because we are an observer only). *)
let set_ready ?(silent=false) cn flag =
  cn.ready := flag ;
  if not silent then Events.send cn.rcv_prod (Connection_active flag)
  else
  if flag then Events.send cn.rcv_prod Expect_end_turn
  else ()

let incr_date cn =
  cn.date <- cn.date + 1 ;
  Events.send cn.rcv_prod (Date cn.date) ;
  ()

let fmt_seq3 fmt sc1 sc2 f = Myscan.(map (seq3 (fmt_scan fmt f) sc1 sc2) (fun (f1, v1, v2) -> f1 v1 v2))

(* Wait for 'get_action'. Can receive informational messages meanwhile. *)
let rec wait_for_turn cn = 
  let send v = Events.send cn.rcv_prod v in

  let%lwt got_action =
    try%lwt
      cn.read "wait_for_turn"
        (union
           (* Beware of partial matches: a prefix of a matching pattern must not satisfy previous patterns. *)
           [ kw_scan  "get_action" true ; (* OK, done. *)
             fmt_seq  "set_explored %d %d" scan_tile (fun x y tile -> send (Set_explored ((x,y), tile)) ; false) ;
             fmt_seq3 "set_visible %d %d" scan_tile scan_item (fun x y tile it -> send (Set_visible ((x,y), tile, it)) ; false) ;             
             fmt_scan "move %d %d %d" (fun pid x y -> send (Move (pid, (x,y))) ; false) ;
             fmt_scan "ok-invasion %d %d %d" (fun a x y -> send (Invasion (true, a, (x,y))) ; false) ;
             fmt_scan "ko-invasion %d %d %d" (fun a x y -> send (Invasion (false, a, (x,y))) ; false) ;
             fmt_scan "create_piece %d %d %d %d" (fun a b c d -> send (Create_piece (a,b,c,d)) ; false) ;
             fmt_scan "error %s@$" (fun m -> false) ;
             fmt_scan "delete_piece %d" (fun pid -> send (Delete_piece pid) ; false) ;
             
             (* This messages can probably be ignored. *)
             fmt_scan "city-units-limit %_d" false ;
             fmt_scan "created-units-limit %_d" false ;
             fmt_scan "lose_city %_d" false ;
             fmt_scan "enter_city %_d %_d" false ;
             fmt_scan "leave_piece %_d %_d" false ;
             fmt_scan "leave_city %_d %_d" false ;
             fmt_scan "leave_terrain %_d %_d %_d" false ;
           ])        
    with Scan_error e -> send (Error (Printexc.to_string e)) ; Lwt.return_false
    (* Otherwise, let the program fail on a socket error. *)
                                                                 
  in
  if got_action then (set_ready ~silent:cn.observe cn true ; Lwt.return_unit)
  else wait_for_turn cn

(* Get the server message 'get_action', which indicates the server is ready to handle requests,
 * then returns the given value. *)
let receive_your_turn cn result =
  let%lwt () = cn.read "Connect.receive_your_turn" (kw_scan "get_action" ()) in
  set_ready cn true ;
  Lwt.return result

let connect ~ip4 ~port ?(observe_only=false) intf =
  let sock = socket PF_INET SOCK_STREAM 0 in

  let%lwt () = log Info "Connecting..." in
  Events.send intf.raw_received (Printf.sprintf "Waiting for the server (port %d)" port) ;
  connect sock (ADDR_INET (Unix.inet_addr_of_string ip4, port)) >>
  let%lwt () = log Info "Connected." in

  let inch = Lwt_io.of_fd ~mode:Input sock
  and ouch = Lwt_io.of_fd ~mode:Output sock in

  let ready = ref false in

  (* Emit a message on the socket *)
  let emit fmt = Printf.ksprintf
      begin fun s ->
        let rec loop () =
          if not !ready then
            begin
              log_f Warning "Connect: server is not ready. Cannot emit %s" s >>
              let%lwt _ = Events.wait_on ~pr:(fun e -> e = Connection_active true) intf.rcv_msg in
              (* Delay to avoid recursion *)
              Lwtplus.async loop
            end
          else
            begin
              Events.send intf.raw_emitted s ;
              (* Block further connections _before_ sending. *)
              ready := false ; (* Cannot use set_ready, the connection cn is not defined yet. *)
              Events.send intf.rcv_msg (Connection_active false) ;
              write_line ouch s >>
              flush ouch
            end
        in
        loop ()
      end
      fmt
  in

  (* Receive something from the socket *)
  let read debug_title sc =
    let%lwt () = Lwt_unix.yield () in (* Avoid blocking if the socket keeps on receiving stuff. *)
    let%lwt line = read_line inch in
    Events.send intf.raw_received line ;

    match scan (mk_full sc) line with
    | Some res -> Lwt.return res

    | None -> 
      let error_msg = debug_title ^ " cannot read this data: " ^ line in
      log Error error_msg >> 
      [%lwt raise (Scan_error (Failure error_msg))]
        
    | exception e ->
      log ~exn:e Error "Connect.read" >>
      [%lwt raise (Scan_error e)]
  in

  (* Start protocol : receives width, height, player id, random seed. *)
  let%lwt width  = read "Width" (fm1_scan "width %d") in
  let%lwt height = read "Height" (fm1_scan "height %d") in
  let%lwt player_id = read "Player id" (fm1_scan "player_id %d") in
  let%lwt ptypes = read "Pieces types" (fmt_seq "piece_types " (list ~split:";" scan_full_piece_type) id) in
  let%lwt random = read "Random seed" (fm1_scan "random_seed %d") in
  let%lwt () = log_f Info "Size is %d, %d" width height in
  
  (* Send the config to the GUI *)
  let cn =
    { ready ;
      rcv_prod = intf.rcv_msg ;
      emit ;
      read ;
      observe = observe_only ;
      date = 0 }
  in

  let config = { width ;
                 height ;
                 player_id ;
                 ptypes ;
                 random ;
                 connection = cn }
  in
  (* ... and wait until the config has been received. *)
  let%lwt () = Lwtboard.send intf.board (Config config)
  and () = Lwtboard.waitforvalue intf.board Gui_ready in

  (* ready should still be false, we have not received 'get_action' *)
  assert (!ready = false) ;
  assert (cn.date = 0) ;
  Events.send intf.rcv_msg (Connection_active !ready) ;
  Events.send intf.rcv_msg (Date cn.date) ;

  (* We are ready when we receive the first 'get_action' *)
  wait_for_turn cn >>
  Lwt.return_unit


(*** In observer mode, no request should be sent (and no answer should be expected). **)

(* Generic request, handles answers of type "id, hxy" list *)
let get_list request name cn =
  cn.emit request >>
  let%lwt result = cn.read name (list ~split:"," (fmt_scan "%d:%d:%d" (fun a b c -> (a,(b,c))))) in
  receive_your_turn cn result

let get_city_list = get_list "get_list_cities" "get_city_list"
let get_piece_list = get_list "get_list_pieces" "get_piece_list"
let get_movable_list = get_list "get_list_movables" "get_movable_list"

let get_id request name cn (x,y) =
  cn.emit "%s %d %d" request x y >>
  let%lwt result = cn.read name int_scan in
  receive_your_turn cn result

let get_piece_id_by_loc = get_id "get_piece_id_by_loc" "get_piece_id_by_loc"
let get_city_id_by_loc = get_id "get_city_id_by_loc" "get_city_id_by_loc"

let get_pieces_types cn = 
  cn.emit "get_piece_types_names" >>
  let%lwt result = cn.read "get_pieces_types" (list ~split:"," (fmt_scan "%d:%[A-Za-z]" (fun i n -> i,n))) in
  receive_your_turn cn result

(* Returns (build_time, remaining, piece_type) *)
let get_city_production cn city_id =
  cn.emit "get_city_production %d" city_id >>
  let%lwt result = cn.read "get_city_production" (union [ kw_scan "none" None ;
                                                          fmt_scan "%d:%d:%d" (fun a b c -> Some (a,b,c)) ]) in
  receive_your_turn cn result

let can_whatever action_name cn pid dir =
  cn.emit "%s %d %d" action_name pid (int_of_dir dir) >>
  let%lwt result = cn.read action_name bool_scan in
  receive_your_turn cn result

let can_move   = can_whatever "can_move"
let can_enter  = can_whatever "can_enter"
let can_attack = can_whatever "can_attack"


(* Asynchronous messages. You must wait for 'Your_turn' before sending the next message. *)

let end_turn cn =
  cn.emit "end_turn" >>
  (incr_date cn ;
   wait_for_turn cn)

let set_city_production cn city_id ptid = 
  cn.emit "set_city_production %d %d" city_id ptid >>
  wait_for_turn cn

let move cn pid dir = 
  cn.emit "move %d %d" pid (int_of_dir dir) >>
  wait_for_turn cn

let send_raw cn msg =
  cn.emit "%s" msg >>
  wait_for_turn cn

