open Empire ;;

let directions =
  [| (+1,  0); (+1, -1); ( 0, -1);
     (-1,  0); (-1, +1); ( 0, +1)
  |] ;;

let tile_neighbor direction_id (q, r) =
  let dq, dr = directions.(direction_id mod (Array.length directions)) in
  q + dq, r + dr ;;

let tile_delta (q1, r1) (q2, r2) = q2 - q1, r2 - r1

let tiles_distance (qa, ra) (qb, rb) = (abs (qa - qb) + abs (qa + ra - qb - rb) + abs (ra - rb)) / 2

let direction_to_direction_id direction =
  let rec find index =
    if index = Array.length directions then raise (Failure "Not Found") else
    if directions.(index) = direction then index else find (index + 1) in
  find 0

let in_map game (q, r) =
  0 <= q && 0 <= r && q < game.g_width && r < game.g_height ;;

let tiles_around game n (q, r) =
  (* # http://www.redblobgames.com/grids/hexagons/#range :
     var results = []
     for each -N ≤ dx ≤ N:
       for each max(-N, -dx-N) ≤ dy ≤ min(N, -dx+N):
         var dz = -dx-dy
       results.append(cube_add(center, Cube(dx, dy, dz))) ;; *)
  let g aux (dq, dr) =
    let p = q + dq, r + dr in
    if in_map game p then p::aux else aux in
  let f aux dx =
    let dys = Misc.range (max (-n) (-dx-n)) (min n (-dx+n)) in
    List.fold_left (fun aux dy -> g aux (dx, dy)) aux dys in
  let dxs = Misc.range (-n) (n) in
  List.fold_left f [] dxs ;;

(* Il s'agit de positions hexagonales (pointy topped avec axial coordinates) :
 *   X X X X X
 *    X X X X X
 *     X X X X X
 * La position en x de la premiere position de la premiere ligne est donc 0.
 * Par contre, la position en x de la premiere position de la seconde ligne est
 * fonction du numero de ligne (en raison du decalage a droite - d'ou la somme
 * q+r dans la formule).
 * Notons que cette formule est en particulier utilisee pour la generation d'une
 * carte : un bruit genere un terrain avec des coordonnees cartesiennes. Cette
 * formule permet, pour chaque position axial, de connaitre la position sur le
 * terrain et obtenir ainsi si il s'agit d'eau ou de terre. Du coup, on n'est pas
 * force de respecter la formule de (http://www.redblobgames.com/grids/hexagons/
 * Geometry). L'impact est simplement une carte 'ecrasee' ou 'etiree'.
 *)
let coords_qr_to_xy (q, r) =
  let q = float_of_int q in
  let r = float_of_int r in
  let x = sqrt(3.0) *. (q +. r /. 2.0) in
  let y = 3.0 /. 2.0 *. r in
  x, y ;;
