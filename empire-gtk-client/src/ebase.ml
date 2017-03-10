type hxy = int * int
type direction = N | S | E | W | NW | SE

let int_of_dir = function
  | N -> 1
  | S -> 4
  | E -> 0  (* Graphically, it should be NE *)
  | W -> 3  (* Graphically, it should be SW *)
  | NW -> 2
  | SE -> 5

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


open Myscan

let scan_tile = trim (union [ kw_scan "water" Water ;
                              kw_scan "ground" Ground ])

let int_of_ptype ptyp = ptyp.a_id
let piece_type2s ptyp = ptyp.a_name

let scan_full_piece_type =
  fmt_scan "%d#%s@#%c#%s@#%d#%d#%d#%d#%d#%s@#%s@#%d#%b"
    begin fun a_id a_name a_symbol tiles a_build_time a_strength
      a_max_hits a_speed a_capacity auto transport
      a_visibility a_can_invade ->

      let a_terrain = match scan (mk_full (list ~split:":" scan_tile)) tiles with
        | None -> failwith (Printf.sprintf "scan_full_piece_type error : this is not a terrain list '%s'" tiles)
        | Some l -> l
      in

      let a_transportable = match scan (mk_full (list ~split:":" int_scan)) transport with
        | None -> failwith (Printf.sprintf "scan_full_piece_type error : this is not a transportable list '%s'" transport)
        | Some l -> l
      in
      
      { a_id ;
        a_name ;
        a_symbol ;
        a_terrain ;
        a_build_time ;
        a_strength ;
        a_max_hits ;
        a_speed ;
        a_capacity ;
        a_autonomy = if auto = "" then None else Some (int_of_string auto) ;
        a_transportable ;
        a_visibility ;
        a_can_invade }
    end

let scan_item =
  union [ kw_scan "none" None ;
          fmt_scan "city %d" (fun cid -> Some (City (cid, None))) ;
          fmt_scan "owned_city %d %d" (fun cid pid -> Some (City (cid, Some pid))) ;
          fmt_scan "piece %d %d %d %d" (fun owner pid ptyp phits -> Some (Piece (owner, pid, ptyp, phits))) ;
        ]
