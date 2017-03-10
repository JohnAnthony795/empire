open Empire ;;
open Misc ;;
open Coords ;;

(* Implementation de l'algorithme Moore Neighbor pour la recherche des
   contours. Cette recherche debute par un balayage de la carte a la recherche
   d'un terrain encore inexplore. Si un terrain deja explore est rencontre,
   nous enjambons les terrains adjacents pour atteindre l'eau de l'autre cote.
   Si un terrain a explorer est rencontre, alors nous reculons d'une case et
   nous debutons la recherche du contours. *)
let identify_contours game =
  (* L'algorithme implique d'etre capable de tourner autour du terrain qui est
     le pivot. *)
  let directions = [+1, 0; +1, -1; 0, -1; -1, 0; -1, +1; 0, +1] in
  let dir_to_idx =
    List.combine directions (Misc.range 0 (List.length directions - 1)) in
  let clockwise (q1, r1) (q2, r2) =
    let diff = q1 - q2, r1 - r2 in
    let diff_idx = List.assoc diff dir_to_idx in
    let next_diff = (diff_idx + 1) mod (List.length directions) in
    let qd, rd = List.nth directions next_diff in
    q2 + qd, r2 + rd in
  (* Cette matrice identifie les terrains qui doivent etre explores. Nous nous
     focalisons sur les terres et ignorons l'eau. *)
  let remain = Misc.map_matrix game.g_map ((!=) Empire.Water) in
  (* Identification d'un contour. La variable s est le point de depart, p est
     le pivot et (q, r) est le point en cours d'analyse. Nous commencons tout
     d'abord par verifier qu'il ne s'agisse pas d'un continent avec un seul
     terrain. Ce cas de figure engendre une boucle infinie s'il est traite
     directement par l'algorithme de Moore Neighbor (le point (q, r) ne fait
     que tourner autour du pivot sans jamais prendre la valeur s=p, qui
     constitue pourtant la seule condition de terminaison). Ensuite, nous
     tournons autour du terrain pivot. Si en tournant nous tombons sur un
     nouveau terrain, ce dernier est ajoute au contour et il prend la place
     de pivot. Si nous retombons sur le point s, alors le contour est
     entierement trace et nous stoppons. *)
  let mark contour s p n =
    let rec aux contour s p (q, r) =
      if s = (q, r) then contour else
      if not (in_map game (q, r)) || game.g_map.(q).(r) = Empire.Water then
          aux contour s p (clockwise (q, r) p)
        else begin
          remain.(q).(r) <- false ;
          aux ((q, r) :: contour) s (q, r) (clockwise p (q, r))
        end in
    (* Lancement de l'algorithme, precede du test du continent avec un seul
       terrain, en regardant si autour du pivot il n'y a que de l'eau. *)
    let check (qd, rd) =
      let qs, rs = s in
      not (in_map game (qs + qd, rs + rd))
        || game.g_map.(qs + qd).(rs + rd) = Empire.Water in
    if List.for_all check directions then contour else aux contour s p n in
  (* Lorsqu'un contour est analyse, nous reprenons l'analyse en enjambant le
     continent qui vient d'etre analyse. Cette fonction enjambe une ligne
     de terres identifiee par la coordonnee (q, r). *)
  let rec find_water q r =
    if q = game.g_width || game.g_map.(q).(r) = Empire.Water then (q, r) else
    find_water (q + 1) r in
  (* Recherche de tous les contours. Des qu'une terre est identifiee, et si
     cette terre n'a pas encore ete traitee lors de l'analyse d'un precedent
     contour, alors le contour correspondant est recherche. Ensuite, le
     traitement continue en sautant la ligne de terres, a la recherche d'une
     case d'eau. *)
  let rec process contours q r =
    if q = game.g_width then process contours 0 (r + 1) else
    if r = game.g_height then contours else
    if game.g_map.(q).(r) = Empire.Water then process contours (q + 1) r else
    if remain.(q).(r) then begin
        let next = clockwise (q - 1, r) (q, r) in
        let cont = mark [q, r] (q, r) (q, r) next in
        let q, r = find_water q r in
        process (cont :: contours) q r
      end else begin
        let q, r = find_water q r in
        process contours q r
      end in
  process [] 0 0 ;;

(* Cette fonction est l'implementation du bruit de Perlin pour le generation
   de la carte. *)
let generate_map config =
  let lerp t a b = a +. t *. (b -. a) in
  let fade t = t *. t *. t *. (t *. (t *. 6.0 -. 15.0) +. 10.0) in
  let grad hash x y =
    let u = if hash land 2 == 0 then x else 0.0 -. x in
    let v = if hash land 1 == 0 then y else 0.0 -. y in
    u +. v in
  let noise values x y =
    let fx = (int_of_float (floor x)) land 255 in
    let fy = (int_of_float (floor y)) land 255 in
    let x = x -. floor x in
    let y = y -. floor y in
    let u = fade x in
    let v = fade y in
    let a = values.(fx) + fy in
    let b = values.(fx + 1) + fy in
    lerp v
      (lerp u
        (grad values.(a) x y)
        (grad values.(b) (x -. 1.0) y))
      (lerp u
        (grad values.(a + 1) x (y -. 1.0))
        (grad values.(b + 1) (x -. 1.0) (y -. 1.0))) in
  let values =
    let v = Array.of_list (Misc.range 0 255) in
    shuffle v ;
    Array.concat [v; v] in
  let map = Array.make_matrix config.m_width config.m_height Ground in
  for r = 0 to config.m_height - 1 do
    for q = 0 to config.m_width - 1 do
      let x, y = Coords.coords_qr_to_xy (q, r) in
      let v = ref 0.0 in
      for o = 0 to config.m_octaves - 1 do
        let o = float_of_int o in
        let amp = config.m_amplitude *. (config.m_persistence ** o) in
        let freq = config.m_frequency *. (2.0 ** o) in
        v := !v +. amp *. noise values (freq *. x) (freq *. y)
      done ;
      if !v <= config.m_sea_level then map.(q).(r) <- Water
    done
  done ;
  map ;;

(* Placement aleatoire des villes sur la carte. La densite correspond au nombre
   de villes par terre. Les villes sont positionnees aleatoirement en fonction
   de cette densite. Par contre, elle assure que chaque contour de continent
   possede au moins une ville, sinon elle en rajoute, quite a faire defaut a la
   densite sur ce point. *)
let place_cities game config =
  let map = game.g_map in
  (* Recuperation des parametres de config pour le placement. *)
  let width = config.m_width in
  let height = config.m_height in
  let density = config.m_cities_density in
  (* Recuperation des coordonnees des terres. *)
  let grounds =
    List.filter (fun (q, r) -> map.(q).(r) = Ground)
    (Misc.product (Misc.range 0 (width - 1)) (Misc.range 0 (height - 1))) in
  (* Calcul de la plus petite distance entre deux villes, en tenant compte de
     la densite desiree. Pour ce faire, nous calculons la surface des terres de
     la carte et nous considerons un carre de meme surface. Nous determinons
     ensuite le nombre de villes dans ce carre en respecant la densite, ainsi
     que le nombre de ville par ligne si ces villes sont reparties en forme de
     maillage. Avec ces information, nous calculons la distance entre deux
     point de ce maillage, que nous considerons ensuite comme etant la distance
     minimum entre deux villes. *)
  let min_dist =
    let dim = float_of_int (List.length grounds) in
    sqrt dim /. sqrt (dim *. density) in
  (* Fonction permettant de tester que deux villes sont assez eloignees. *)
  let check_dist (xa, ya) (xb, yb) =
    let dx = float_of_int (xb - xa) in
    let dy = float_of_int (yb - ya) in
    sqrt (dx *. dx +. dy *. dy) > min_dist in
  (* Selection aleatoire des villes a partir des terres candidates. *)
  let rec select cities lands =
    let sz_lands = List.length lands in
    if sz_lands = 0 then cities else
      (* Le choix aleatoire d'une ville parmis les terres amene a retirer
         les terres candidates trop proches de cette ville. *)
      let city = List.nth lands (Random.int sz_lands) in
      let lands = List.filter (check_dist city) lands in
      select (city::cities) lands in
  let make_city id (q, r) =
    let city =
      { c_id = id
      ; c_loc = q, r
      ; c_owner = None
      ; c_production = None
      ; c_transport = Misc.Set.of_array [||]
      ; c_visibility = 1
      } in
    Hashtbl.add game.g_cities id city ;
    game.g_map_items.(q).(r) <- Some (City id) ;
    city in
  let cities_loc =
    let locs = select [] grounds in
    (* let has_no_*) (* TODO TODO TODO *)
    let locs_adjust =
      List.map pick_random (List.filter (fun x -> List.length (intersection locs x) = 0) (identify_contours game)) in
    locs @ locs_adjust in
  if game.g_nb_players > List.length cities_loc then
    failwith "Not_enough_cities"
  else
    List.mapi make_city cities_loc ;;

let create_player config player_id =
  { player_id = player_id
  ; player_view = Array.make_matrix config.m_width config.m_height Unknown
  ; player_pieces = Misc.Set.of_array [||]
  ; player_cities = Misc.Set.of_array [||]
  ; player_nb_created_units = 0
  } ;;

let create_game config =
  let game =
    { g_map = generate_map config
    ; g_width = config.m_width
    ; g_height = config.m_height
    ; g_players = Array.init config.m_nb_players (create_player config)
    ; g_map_items = Array.make_matrix config.m_width config.m_height None
    ; g_cities = Hashtbl.create 128
    ; g_pieces = Hashtbl.create 128
    ; g_piece_types = config.m_piece_types
    ; g_round = 0
    ; g_max_round = 100
    ; g_turn = 0
    ; g_nb_players = config.m_nb_players
    ; g_counter = 0
    ; g_mailbox = Queue.create ()
    ; g_random = Random.int 1000000000
    ; g_end = false
    ; g_max_nb_created_units_per_player = 100
    ; g_max_units_per_city = 2
    } in
  (* Ajout des villes a la liste des objets et des positions. *)
  let cities = place_cities game config in
  let place_player i city = Actions.own_city game city i in
  let starts = Misc.rand_select cities config.m_nb_players in
  ignore (List.mapi place_player starts) ;
  game.g_turn <- 0 ;
  game ;;
