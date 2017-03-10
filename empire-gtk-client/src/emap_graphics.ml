open Common.Small
open Ebase
open Connect

(* Config *)
let terrain_path = "TILES/terrains"
let water_path = "TILES/water"
let mask_path = "TILES/masks"
let forest_path = "TILES/foret"
let town_path num = Printf.sprintf "CITIES/town-%d.png" num
let unit_path pl pt = Printf.sprintf "UNITS/%s-%d.png" (String.lowercase pt.a_name) (pl+1)

let add_dir (x,y) = function
  | N  -> (x+1, y-1)
  | S  -> (x-1, y+1)
  | E  -> (x+1, y)
  | W  -> (x-1, y)
  | NW -> (x, y-1)
  | SE -> (x, y+1)

let get_mask_name = function
  | N -> "mask_north"
  | S -> "mask_south"
  | E -> "mask_east"
  | W -> "mask_west"
  | NW -> "mask_nw"
  | SE -> "mask_se"


(* Directions :
   2 1 .
   3 . 0
   . 4 5
*)

(* Deal with variants of a given picture (e.g. desert, snow, green, ...) *)
type variant_pic =
  { files: string list ; (* All the images of the given variant. *)
    len: int (* List length *) }

(* This hashtable maps a prefix (e.g. "desert") to a variant_pic *)
let all_tiles = Hashtbl.create 60

(* Choose a picture in a variant. *)
let get_pix prefix random =
  let variants = try Hashtbl.find all_tiles prefix with Not_found -> failwith ("Emap: unknown tile prefix : " ^ prefix) in
  let name = List.nth variants.files (random mod variants.len) in
  (*  Printf.printf "Choosing %s\n%!" name ; *)
  Ggimage.pixbuf_from_file name

let add_tiles dir prefix =
  let imgdir = Ggimage.find_imgpath ~check_exists:false dir in

  (* This is a lwt_stream *)
  let files_in_imgdir = Lwt_unix.files_of_directory imgdir in
  let relevant_files = Lwt_stream.filter (Common.starts_with prefix) files_in_imgdir in 
  let%lwt files = Lwt_stream.to_list relevant_files in

  let variants = { files = List.map (fun s -> dir // s) files ; len = List.length files } in
  assert (variants.len > 0) ;
  Hashtbl.add all_tiles prefix variants ;
  Lwt.return_unit

(* Init *)
let all_terrains = [ "desert" ; "desert-road" ; "dry" ; "green" ; "regular" ; "semi-dry" ; "snow" ]
let all_foret = ["bigtrees"]

let init () =
  Lwt.join [ Lwt_list.iter_p (add_tiles terrain_path) all_terrains ;
             Lwt_list.iter_p (add_tiles forest_path) all_foret ;
             add_tiles water_path "water" ;
             Lwt_list.iter_p (add_tiles mask_path) (List.map get_mask_name [ N ; S ; E ; W ; SE ; NW ]) ]

let pick x l = List.nth l (x mod List.length l)

(* Pseudo-randomize (must be deterministic). *)
let randomize r m =
  let h = Hashtbl.hash r in
  (h * h / m) mod (m + 1)

let find_name ~random ~dims:(dx,dy) (x,y) tile =
  match tile with
  | Water -> "water"
  | Ground ->
    begin
      let lat = y - x
      and minlat = -dx
      and maxlat = dy in

      (* Latitude expressed as a number in [0 .. 12] *)
      let high = 12 * (lat - minlat) / (maxlat - minlat) in

      (* Modulate with random +1/-1 *)
      let high = (high - 1) + (random mod 3) in

      (* Check bounds *)
      let high = max 0 (min high 12) in

      match high with
      | 0 | 1 | 2 | 12 | 11 | 10 -> "snow"
      | 3 | 4 | 5 -> pick random [ "dry" ; "desert" ; "desert-road" ]
      | 6 | 7 | 8 | 9 -> pick random [ "green" ; "regular" ; "semi-dry" ]
      | _ -> assert false
    end

let find_raw_pixbuf ?(copy=false) config xy tile =
  let dims = (config.width, config.height) in
  let random = randomize (config.random, xy, dims) 1111 in
  let pixb = get_pix (find_name ~random ~dims xy tile) random in
  if copy then GdkPixbuf.copy pixb else pixb

let find_pixbuf gstate xy tile =
  let config = Emap.get_config gstate in
  
  (* Raw pixbuf, no borders. *)
  let pix = find_raw_pixbuf ~copy:true config xy tile in

  let width = GdkPixbuf.get_width pix
  and height = GdkPixbuf.get_height pix in

  (* Add borders *)
  let add_border dir border_tile newxy =
    let neighb = find_raw_pixbuf ~copy:true config newxy border_tile in
    Ggimage.apply_alpha neighb (get_pix (get_mask_name dir) (randomize (config.random, dir, xy) 987)) ;
    GdkPixbuf.composite ~dest:pix ~alpha:255 ~dest_x:0 ~dest_y:0 ~ofs_x:0.0 ~ofs_y:0.0 ~width ~height neighb ;
  in

  let check_border dir =
    let newxy = add_dir xy dir in 
    match Emap.get_tile gstate newxy with
    | None -> () (* Unknown tile, the border will be drawn on the opposite tile. *)
    | Some t -> add_border dir t newxy
  in

  List.iter check_border [N ; S ; E ; W ; SE ; NW] ;

  pix


let div2 (x,y) (n1,n2) = (x/n1, y/n2)

let decoration gstate xy =
  let config = Emap.get_config gstate in
  let dims = (config.width, config.height) in

  (* Choose forest "size" *)
  let fsizex = 1 + randomize (config.random, div2 xy (18,18), dims) 10
  and fsizey = 1 + randomize (config.random, div2 xy (18,18), dims) 10 in

  let random2 = randomize (config.random, div2 xy (fsizex,fsizey), dims) 11 in
  let random1 = randomize (config.random, xy, dims) 871 in

  (* We may add some forest provided NW,N,W,C are grounds. *)
  if random2 = 0 && random1 mod 7 <> 0  then
    let xyN = add_dir xy N
    and xyNW = add_dir xy NW
    and xyW = add_dir xy W in

    let inc c = if Emap.get_tile gstate c = Some Ground then 1 else 0 in

    if inc xy + inc xyN + inc xyNW + inc xyW = 4 then
      [ xyNW, get_pix "bigtrees" random1 ]

    (* FIX : the bigtrees depend on the order in which tiles are discovered. 
     * When a new tile appears, check the neighbouring tiles : forest might appear there too. *)

    else []

  else []


type gitem = GnoCanvas.pixbuf Gcanvas.g_item

type entry =
  { item: item ;
    gitem: gitem }

module As = Emap.As

type gr_table =
  { tbl: entry As.t ;
    gstate: Emap.game_state }

let get_item_pix gstate = function
  | City (_, opid) ->
    let town_num = 1 + Common.option_default opid (-1) in
    Ggimage.pixbuf_from_file (town_path town_num)

  | Piece (player_id, pid, ptid, hits) ->
    Ggimage.pixbuf_from_file (unit_path player_id (Emap.piece_type_of_int gstate ptid))


let new_table gstate add =

  (* Preload unit images. *)
  let config = Emap.get_config gstate in
  Common.myiter config.ptypes
    begin fun pt -> ignore(get_item_pix gstate (Piece (config.player_id, 0, pt.a_id, pt.a_max_hits))) end ;

  let init item =
    (* CREATE GITEM *)
    let pix = get_item_pix gstate item in
    let gitem = add item pix (Emap.get_hxy gstate item) in
    { item ; gitem }
  in
  
  { tbl = As.create ~size:100 ~init () ;
    gstate }

let update grtable item =
  As.update grtable.tbl item
    (fun entry ->
       Gcanvas.set_visible entry.gitem true ;
       if entry.item = item then entry (* UNCHANGED *)
       else
         begin
           (* UPDATED => change image *)
           let pix = get_item_pix grtable.gstate item in           
           (Gcanvas.get_item entry.gitem)#set [`PIXBUF pix] ;
           { entry with item = item }
         end)

let hide grtable item = As.update_if_exists grtable.tbl item (fun entry -> Gcanvas.set_visible entry.gitem false ; entry)
