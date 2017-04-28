type terrain = Water | Ground ;;

type piece_id = int ;;

type piece_type_id = int ;;

type city_id = int ;;

type player_id = int ;;

type container =
  | ContainerPiece of piece_id
  | ContainerCity of city_id
  | ContainerTerrain of int * int
  ;;

type piece_type =
  { a_id : piece_type_id
  ; a_name : string
  (* Symbol conseille pour l'affichage de la piece. Eviter les symboles
   * non alphabetiques car ils peuvent etre utilises comme delimiteurs
   * (cf. info_get_info_piece_types).
   *)
  ; a_symbol : char
  (* Cases sur lesquelles la piece peut se deplacer. *)
  ; a_terrain : terrain list
  ; a_build_time : int (* i avec i >= 0 *)
  (* Force d'attaque. *)
  ; a_strength : int (* i avec i >= 0 *)
  (* Points de vie maximum. *)
  ; a_max_hits : int (* i avec i >= 0 *)
  ; a_speed : int (* i avec i >= 0 *)
  ; a_capacity : int (* i avec i >= 0 *)
  ; a_autonomy : int option (* None ou Some i avec i > 0 *)
  ; a_transportable : piece_type_id list
  ; a_visibility : int (* i avec i >= 0 *)
  ; a_can_invade : bool (* indique si l'unite peut prendre des villes *)
  } ;;

type piece =
  { p_id : piece_id
  ; mutable p_parent : container
  ; p_owner : player_id
  ; p_type : int
  ; mutable p_hits : int
  ; mutable p_rem_moves : int
  ; mutable p_autonomy : int option
  ; p_transport : piece_id Misc.Set.t
  } ;;

type city =
  { c_id : city_id
  ; c_loc : int * int
  ; mutable c_owner : player_id option
  ; mutable c_production : (int * piece_type_id) option
  ; c_transport : piece_id Misc.Set.t
  ; c_visibility : int
  } ;;

type item =
  | City of city_id
  | Piece of piece_id
  ;;

type tile =
  | Unknown
  | Explored of terrain
  (* (Nombre d'item qui ont une visibilite sur le carreau, terrain, contenu) *)
  | Visible of int * terrain * (item option)
  ;;

type player =
  { player_id : player_id
  ; player_view : tile array array
  ; player_pieces : piece_id Misc.Set.t
  ; player_cities : city_id Misc.Set.t
  (* Nombre d'unites crees par le joueur. Cf. config.m_max_units_per_player. *)
  ; mutable player_nb_created_units : int
  } ;;

type game =
  { g_width : int
  ; g_height : int
  ; g_nb_players : int
  ; g_map : terrain array array
  ; g_map_items : item option array array
  ; g_cities : (city_id, city) Hashtbl.t
  ; g_pieces : (piece_id, piece) Hashtbl.t
  ; g_piece_types : (piece_type_id, piece_type) Hashtbl.t
  ; g_players : player array
  ; mutable g_round : int
  ; g_max_round : int    (* Nombre de tours maximum de la partie. *)
  ; mutable g_turn : int (* Joueur qui a le jeton. *)
  ; mutable g_counter : int (* Pour la creation de nouveaux id. *)
  ; g_mailbox : (int * string) Queue.t (* (Jouer, message) *)
  ; mutable g_end : bool (* Indicateur de fin de partie. *)
  ; g_random: int (* Graine aléatoire, pour affichage graphique cohérent sur tous les clients. *)
  ; g_max_nb_created_units_per_player : int (* Nombre d'unites maximum qu'un joueur peut creer. *)
  ; g_max_units_per_city : int   (* Nombre d'unites maximum qu'une ville peut accueillir. *)
  } ;;

type config =
  (* Taille de la carte. *)
  { m_width : int
  ; m_height : int
  (* Parametres pour la generation de la carte. *)
  ; m_sea_level : float
  ; m_amplitude : float
  ; m_octaves : int
  ; m_persistence : float
  ; m_frequency : float
  (* Parametres pour le placement des villes. *)
  ; m_cities_density : float
  (* Nombre de joueurs. *)
  ; m_nb_players : int
  ; m_piece_types : (piece_type_id, piece_type) Hashtbl.t
  } ;;
