(*** Emap:  Empire Map ***)

open Ebase

(*** Game local state : cities, units, available unit types. ***)
type game_state

(* Creates a new empty initial game_state. 
 *  (game_state is stateful.) *)
val init_game: Connect.config -> game_state

val get_config: game_state -> Connect.config

val get_piece_types: game_state -> piece_type list
val dump_piece_types: game_state -> unit

(* Gets a tile on the map. *)
val get_tile: game_state -> hxy -> tile option
val set_tile: game_state -> hxy -> tile -> unit

type build_info =
  | Not_a_city
  | Not_your_city
  | Free_city
  | Not_available (* Server not ready *)
  | Unit of (int * int * piece_type) option (* Total / remaining / unit *)

type info =
  { items: item list ;
    city_id: int option ; (* None if Not_a_city *)    
    build: build_info }

val watzere: game_state -> hxy -> info Lwt.t
val itemzere: game_state -> hxy -> item list

(* Only the identifier is used in the item given to get_hxy or get_item. The other fields are ignored. 
 * Fail if the item is not already in game_state. *)
val get_hxy: game_state -> item -> hxy
val get_hxyprod: game_state -> item -> hxy Events.prod
val get_item: game_state -> item -> item


(* The item replaces the item currently stored in game_state and having the same identifier
 * (or inserts a new one).  *)
val set_hxy: game_state -> item -> hxy -> unit


val piece_type_of_int: game_state -> piece_type_id -> piece_type

val build_info2s: build_info -> string



(* Convenience *)

module As: Assoc.ASSOC with type key = item

