open Curses ;;

open Empire ;;

let print_map map =
  wclear map.map_window ;
  let h, w = getmaxyx map.map_window in
  (* Affichage des coordonnees en x. *)
  let rec print_q_scale n =
    let x = n * map.step_scale_q + map.offset_q in
    let q = n * map.step_scale_q + map.viewport_q in
    let text = Printf.sprintf "%d" q in
    if q < map.width && x + String.length text - 1 < w then begin
        assert (mvwaddstr map.map_window 0 x text) ;
        print_q_scale (n + 1)
      end in
  (* Affichage des coordonnees en y. *)
  let rec print_r_scale n =
    let r = map.viewport_r + n * map.step_scale_r in
    let text = Printf.sprintf "%d" r in
    let y = n * map.step_scale_r + map.offset_r in
    let x =
      map.offset_q - String.length text +
        if r < map.viewport_q + map.viewport_r then 0 else
          r - map.viewport_q - map.viewport_r in
    if y < h - 1 && r < map.height then begin
        assert (mvwaddstr map.map_window y x text) ;
        print_r_scale (n + 1)
      end in
  (* Affichage de view. *)
  let rec print_content_view rel_y =
    let r = rel_y + map.viewport_r in
    let y = rel_y + map.offset_r in
    let rec f rel_x =
      let x = rel_x + map.offset_q in
      let q = rel_x - rel_y + map.viewport_q in
      if x < w - 1 then begin
          if 0 <= q && q < map.width then begin
              let ch, attr, color = map.view.(q).(r) in
              (* Si le carreau en cours de traitement est sur le curseur du
                 jeu, alors il est affiche avec un clignotement. *)
              if q = map.current_q && r = map.current_r then
                wattrset map.map_window (attr + A.blink + A.reverse)
              else
                wattrset map.map_window attr ;
              assert (mvwaddch map.map_window y x (ch + A.color_pair color)) ;
              wattrset map.map_window A.normal
            end ;
          f (rel_x + 1)
        end in
    if r < map.height && y < h - 1 then begin
        f 0 ;
        print_content_view (rel_y + 1)
      end in
  print_content_view 0 ;
  print_q_scale 0 ;
  print_r_scale 0 ;
  assert (wrefresh map.map_window) ;;

let move_viewport map delta =
  let dq, dr = delta in
  let ncq, ncr = map.current_q + dq, map.current_r + dr in
  if 0 <= ncr && ncr < map.height && 0 <= ncq && ncq < map.width then begin
    map.current_q <- ncq ;
    map.current_r <- ncr ;
    if ncr > map.viewport_r + map.viewport_h * 3 / 4 then
      map.viewport_r <- max 0 (ncr - map.viewport_h * 3 / 4) ;
    if ncr < map.viewport_r + map.viewport_h * 1 / 4 then
      map.viewport_r <- max 0 (ncr - map.viewport_h * 1 / 4) ;
    if ncq > map.viewport_q + map.viewport_w * 3 / 4 then
      map.viewport_q <- max 0 (ncq - map.viewport_w * 3 / 4) ;
    if ncq < map.viewport_q + map.viewport_w * 1 / 4 then
      map.viewport_q <- max 0 (ncq - map.viewport_w * 1 / 4) ;
    print_map map
  end ;;

let resize_map map =
  let h, w = getmaxyx map.map_window in
  map.viewport_h <- min map.height (h - map.offset_r) ;
  map.viewport_w <- min map.width (w - map.offset_q) ;
  assert (wrefresh map.map_window) ;
  print_map map ;;

let reprint_map map q r =
  if map.viewport_q <= q && q < map.viewport_q + map.viewport_w &&
     map.viewport_r <= r && r < map.viewport_r + map.viewport_h then
    print_map map ;;

let center_map_on map q r =
  map.current_q <- q ;
  map.current_r <- r ;
  map.viewport_q <- max (q - map.viewport_w / 2) 0 ;
  map.viewport_r <- max (r - map.viewport_h / 2) 0 ;
  print_map map ;;
