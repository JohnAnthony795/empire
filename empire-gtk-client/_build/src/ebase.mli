(*** Ebase: Empire base types ***)

(* Hexagonal coordinates *)
type hxy = int * int

(* Directions *)
type direction = N | S | E | W | NW | SE

val int_of_dir: direction -> int  

(* Identifiers *)
type city_id   = int
type player_id = int
type piece_id  = int
type piece_type_id = int

type hit_points = int

type tile =
  | Water
  | Ground

type item =
  | City of city_id * (player_id option)
  | Piece of player_id * piece_id * piece_type_id * hit_points


(* Piece type. Cf empire-server/Empire.ml *)
type piece_type =
  { a_id : piece_type_id ;
    a_name : string ;
    a_symbol : char ;
    a_terrain : tile list ;
    a_build_time : int ;
    a_strength : int ;
    a_max_hits : int ;
    a_speed : int ;
    a_capacity : int ;
    a_autonomy : int option ;
    a_transportable : piece_type_id list ;
    a_visibility : int ;
    a_can_invade : bool }


(*** Scanners ***)

val scan_full_piece_type: piece_type Myscan.t
val scan_tile: tile Myscan.t
val scan_item: (item option) Myscan.t
  
val int_of_ptype: piece_type -> piece_type_id



(* to_string *)
val piece_type2s: piece_type -> string
