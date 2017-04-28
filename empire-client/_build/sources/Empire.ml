open Curses ;;
open Unix ;;

type console =
  { console_window : window
  ; histlen : int
  ; console_height : int
  ; mutable messages : string list
  ; mutable styles : int list
  } ;;

type map =
  { map_window : window
  ; mutable viewport_q : int
  ; mutable viewport_r : int
  ; mutable viewport_w : int
  ; mutable viewport_h : int
  ; mutable current_q : int
  ; mutable current_r : int
  ; step_scale_q : int
  ; step_scale_r : int
  ; offset_q : int
  ; offset_r : int
  ; view : (int * int * int) array array
  ; tile_to_pair : (bool * bool * int option, int) Hashtbl.t
  ; width : int
  ; height : int
  } ;;

type game =
  { window : window
  ; mutable action : bool (* Indique si le joueur a la main. *)
  ; map : map
  ; console : console
  ; connection : file_descr
  ; mutable stop : bool
  ; mutable end_message : string
  ; player_id : int
  ; observer : bool
  ; piece_symbols : (int * int) list
  } ;;
