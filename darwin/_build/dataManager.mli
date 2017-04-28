open Types

val get_nb_unite_proche: unites -> piece_id -> int
val get_nb_ville_proche: piece_id -> int
val get_next_playable: unit -> int

val set_our_jid: int -> unit

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


