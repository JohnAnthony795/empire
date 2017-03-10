open ExtArray
open Ebase
open Connect
    
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

type item_pos =
  { hxy: (int*int) Evt_val.ref_val ;
    item: item }

type key = KCity of city_id | KUnit of piece_id

let get_key = function
  | City (cid, _) -> KCity cid
  | Piece (_, _, pid, _) -> KUnit pid

module As = Assoc.Mk_Assoc
    (Hashtbl.Make (struct
       type t = item
       let equal i1 i2 = (get_key i1) = (get_key i2)
       let hash i = Hashtbl.hash (get_key i)
     end))

type game_state =
  { dims: int * int ;

    items: item_pos As.t ;
    
    (* Piece types *)
    catalog: (piece_type_id, piece_type) Hashtbl.t ;

    (* Known tiles *)
    tiles: tile option Mat.mat ;
    
    config: config }

let get_config gs = gs.config

let init_game config =

  let dx = config.width
  and dy = config.height in

  let init_item item =
    { hxy = Evt_val.vref (0,0) ;
      item }
  in
  
  let gstate =
    { dims = (dx,dy) ;
      items = As.create ~size:100 ~init:init_item () ;
      catalog = Hashtbl.create 40 ;
      tiles = Mat.create dy dx None ;
      config }
  in

  List.iter (fun ptyp ->
      let cat = gstate.catalog in
      if Hashtbl.mem cat ptyp.a_id then
        Printf.printf "Warning: emap.ml, redefining piece type #%d\n%!" ptyp.a_id ;
      Hashtbl.add cat ptyp.a_id ptyp)
    config.ptypes ;

  gstate


let dump_piece_types gstate =
  Hashtbl.iter (fun pid ptyp -> Printf.printf "  cat(%d) -> %s\n%!" pid (piece_type2s ptyp)) gstate.catalog

let get_piece_types gstate = Hashtbl.fold (fun _ pt acu -> pt :: acu) gstate.catalog []

let piece_type_of_int gstate id =
  if Hashtbl.mem gstate.catalog id then Hashtbl.find gstate.catalog id
  else failwith ("ERROR: emap.ml unknown piece type = #" ^ string_of_int id)

let get_tile gstate (hx,hy) =
  if hx < 0 || hy < 0 then None
  else Mat.get gstate.tiles hy hx

let set_tile gstate (hx,hy) tile =
  Mat.set gstate.tiles hy hx (Some tile)

let get_item_pos gstate item =
  try (As.get_existing gstate.items item)
  with Not_found -> assert false (* Item not in table? *)

let get_item gstate item = (get_item_pos gstate item).item
let get_hxy gstate item = (get_item_pos gstate item).hxy#value
let get_hxyprod gstate item = (get_item_pos gstate item).hxy#vprod

let set_hxy gstate item hxy = As.update gstate.items item (fun ipos -> ipos.hxy#set_value hxy ; { ipos with item = item } )

(* Find items located at the given coordinates. *)
let itemzere gstate hxy = As.fold gstate.items [] (fun _ gritem acu -> if gritem.hxy#value = hxy then gritem.item :: acu else acu)
  
let watzere gstate hxy =
  let its = itemzere gstate hxy in

  (* Is it a city building something ? *)
  let%lwt (city_id, build) =
    match Common.mapfind (function City (cid, po) -> Some (cid, po) | _ -> None) its with
    | None -> Lwt.return (None, Not_a_city)
    | Some (city_id, None) -> Lwt.return (Some city_id, Free_city)
    | Some (city_id, Some pid) ->
      if pid = gstate.config.player_id then

        (* What is building this city ? *)
        if is_ready gstate.config.connection then (* Possible race condition here, if the connection is used for something else. *)
          Lwt.map
            (fun u -> (Some city_id, Unit (Common.option_map u (fun (a,b,c) -> (a,b,piece_type_of_int gstate c)))))
            (get_city_production gstate.config.connection city_id)

        else Lwt.return (Some city_id, Not_available)

      else Lwt.return (Some city_id, Not_your_city)
  in
  Lwt.return { items = its ;
               city_id ;
               build }
      

let build_info2s = function
  | Not_a_city -> ""
  | Not_your_city -> "\n\n(Not your city)\n"
  | Free_city -> "\n\nThis city is waiting for you\n"
  | Not_available -> "\n\n(Build info is available when it is your turn)\n"
  | Unit None ->  "\n\nBuilding nothing\n"
  | Unit (Some (tot, rem, ptyp)) -> Printf.sprintf "\n\nBuilding %s (done %d / %d)\n" ptyp.a_name (tot - rem) tot

