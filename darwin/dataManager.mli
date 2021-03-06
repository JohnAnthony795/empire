open Types

val get_nb_unite_proche: unites -> piece_id -> int -> int
val get_nb_ville_proche_allie: piece_id -> int -> int
val get_nb_ville_proche_ennemi : piece_id -> int -> int
val get_nb_ville_proche_neutre : piece_id -> int -> int
val littoral_adj : piece_id -> bool
val transport : piece_id -> bool
val fog_proche : piece_id -> int -> bool
val unknown_proche : piece_id -> int -> bool
val unite_en_production : piece_id -> bool
val get_coords_attaquer : piece_id -> (int*int)
val get_coords_explorer : piece_id -> (int*int)
val get_coords_envahir : piece_id -> (int*int)
val get_coords_envahir_neutre : piece_id -> (int*int)
val get_coords_transporter : piece_id -> (int*int)
val get_next_playable: unit -> int
val get_next_movable: unit -> (int*unites)
val get_score : unit -> float

val set_our_jid: string list -> unit
val set_map_width: string list -> unit
val set_map_height: string list -> unit
val set_victoire : string list -> unit
val set_draw : unit -> unit
val reset_move_all : unit -> unit 
val set_move_to_zero : int -> unit
val set_move_to_zero_unite : int -> unit
val init_data : unit -> unit
val set_city_production : Types.city_id -> Types.unites -> unit
val get_city_production : Types.city_id -> Types.unites option
val increment_turn_counter : unit -> unit

val traiter_set_visible: string list -> unit
val traiter_set_explored: string list -> unit
val traiter_delete_piece: string list -> unit
val traiter_create_piece: string list -> unit
val traiter_move: string list -> unit
val traiter_lose_city: string list -> unit
val traiter_leave_terrain: string list -> unit
val traiter_enter_city: string list -> unit
val traiter_enter_piece: string list -> unit
val traiter_leave_city: string list -> unit
val traiter_leave_piece: string list -> unit
val traiter_ok_invasion: string list -> unit
val traiter_ko_invasion: string list -> unit
val traiter_city_units_limit: string list -> unit
val traiter_created_units_limit: string list -> unit
val traiter_invalid_terrain : unit -> unit

