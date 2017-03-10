(*** Connection with the Eric's Empire server. ***)

open Ebase

(* Type of a connection to the server. *)
type t
       
(* Messages that can be received asynchronously from the server. *)
type rcv_msg =
  (* Indicate if the server is ready to handle requests. *)
  | Connection_active of bool

  (* In observer mode, indicate if the server is expecting end_turn. *)
  | Expect_end_turn
  
  (* A known tile is no longer visible. It is only "explored". *)    
  | Set_explored of hxy * tile

  (* Something is visible *)
  | Set_visible of hxy * tile * (item option)
			       
  (* A piece has moved to this new coordinate. *)
  | Move of piece_id * hxy

  (* A new unit appears in a city. *)
  | Create_piece of piece_id * piece_type_id * city_id * hit_points

  (* A unit disappears. *)
  | Delete_piece of piece_id

  (* The invasion of the city at these coordinates has succeeded or failed. *)
  | Invasion of bool * city_id * hxy

  (* Current date *)
  | Date of int
			
  | Error of string

(* Config sent by the server (transmitted to the GUI). *)
type config =
  { width: int ;
    height: int ;
    player_id: int ;
    ptypes: piece_type list ;
    random: int ;
    connection: t }

(* Messages used to synchronize the connection & the GUI *)
type sync_msg =
  | Gui_ready
  | Config of config
    
(* Interface of the connection :
 *   - low-level producers with received/emitted strings (only for logging messages, do not send data on them).
 *   - message producer for received events. 
 * The config is received from the server at the first connection. It is then written on the board.
 * To send commands, use the functions below. *)
type intf =
  { raw_received: string Events.prod ;
    raw_emitted:  string Events.prod ;
    rcv_msg: rcv_msg Events.prod ;
    board: sync_msg Lwtboard.t }

(* The interface must be created, and listeners added to it, before the connection starts. 
 * The new connection is put in a config value, then sent on the board. *)
val connect: ip4:string -> port:int -> ?observe_only:bool -> intf -> unit Lwt.t

(* Is the server ready to handle commands? *)
val is_ready: t -> bool

(* Probably useless, since the client local map already contains this information. *) 
val get_city_list: t -> (city_id*hxy) list Lwt.t			     
val get_piece_list: t -> (piece_id*hxy) list Lwt.t
			      
val get_movable_list: t -> (piece_id*hxy) list Lwt.t
  
val get_piece_id_by_loc: t -> hxy -> piece_id Lwt.t
val get_city_id_by_loc:  t -> hxy -> city_id Lwt.t

val get_city_production: t -> city_id -> (int * int * piece_type_id) option Lwt.t
					      
(* val get_transported_names_by_loc: t -> hxy -> *)
					  
val can_move:   t -> piece_id -> direction -> bool Lwt.t
val can_enter:  t -> piece_id -> direction -> bool Lwt.t
val can_attack: t -> piece_id -> direction -> bool Lwt.t
			       

(* For these messages, send the command, and process the messages received in response.
 * When the thread returns, the server is ready to handle new requests (your turn to play). *)
val end_turn: t -> unit Lwt.t

val set_city_production: t -> city_id -> piece_type_id -> unit Lwt.t
val move: t -> piece_id -> direction -> unit Lwt.t

val send_raw: t -> string -> unit Lwt.t
    
