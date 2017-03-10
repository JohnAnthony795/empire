open Common.Small

type xy = float * float

let ( -: ) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)

type coord_sys =
  { (* Get x and y in the canvas coordinates (given coordinates in this sys) *)
    get_xy: xy -> xy ;
    get_a: float -> float ;

    (* Get x and y in this coordinate system (given coordinates in the canvas). *)
    inv_xy: xy -> xy }

let default_sys =
  { get_xy = id ;
    get_a = id ;
    inv_xy = id }

let compute_getxy  ~o1 ~n1 ~o2 ~n2 =
  (* Principle:
   *    o1 is M
   *    n1 is M'
   *    o2 is N
   *    n2 is N'
   *
   *    We have to find the matrix A and vector B such that
   *       M = AM' + B
   *       N = AN' + B
   *
   *    Actually, we know that A is of the form 'a x 'r where 'a is a constant multiplier and 'r a rotation.
   *
   *    'a equals MN / M'N'
   *
   *    'r is of the form (cos -sin
   *                       sin  cos)
   *
   *    Let (dx, dy) be MN  and (dx', dy') be M'N'
   *
   *    We don't compute the angle of rotation, instead we compute directly cos and sin.
   *    After solving some equations, we have
   *       cos = (dx.dx' + dy.dy') / ('a.M'N'²)
   *       sin = (dx'.dy - dx.dy') / ('a.M'N'²)
   *
  *)
  let (dx, dy) = o2 -: o1
  and (dx', dy') = n2 -: n1
  in

  let norm2 = dx' *. dx' +. dy' *. dy'
  and onorm2 = dx *. dx +. dy *. dy in

  let alpha = sqrt (onorm2 /. norm2) in

  let prod = sqrt (norm2 *. onorm2) in

  let cos = (dx *. dx' +. dy *. dy') /. prod
  and sin = (dx' *. dy -. dx *. dy') /. prod in

  let rho_x x y = cos *. x -. sin *. y
  and rho_y x y = sin *. x +. cos *. y in

  let (ox1, oy1) = o1
  and (nx1, ny1) = n1 in
  let bx = ox1 -. alpha *. (rho_x nx1 ny1)
  and by = oy1 -. alpha *. (rho_y nx1 ny1) in

  let get_xy (x, y) =
    let cx = alpha *. (rho_x x y) +. bx
    and cy = alpha *. (rho_y x y) +. by
    in
    (cx, cy)
  in
  get_xy


let mk_sys ?(old_sys=default_sys) ~o1 ~n1 ~o2 ~n2 ?(map_angle=id) () =

  let get_xy1 = compute_getxy ~o1 ~n1 ~o2 ~n2 in
  let get_xy xy = old_sys.get_xy (get_xy1 xy) in

  let inv_xy1 = compute_getxy ~o1:n1 ~n1:o1 ~o2:n2 ~n2:o2 in
  let inv_xy xy = inv_xy1 (old_sys.inv_xy xy) in

  { get_xy ;
    get_a = map_angle ;
    inv_xy }

type 'a g_item =
  { sys: coord_sys ;
    mutable alive: bool ;
    mutable x: float ;
    mutable y: float ;
    mutable angle: float ;
    item: 'a } constraint 'a = _ GnoCanvas.item 

let get_item it = it.item

let update gitem =
  let (dx,dy) = gitem.sys.get_xy (gitem.x, gitem.y)
  and ang = gitem.sys.get_a gitem.angle
  in
  if gitem.alive then gitem.item#affine_absolute (Calc.rotation ~dx ~dy ang)

let set_pos gitem ~x ~y =
  gitem.x <- x ;
  gitem.y <- y ;
  update gitem

let set_angle gitem angle =
  gitem.angle <- Calc.normed_angle angle ;
  update gitem

let set_visible gitem vis = if vis then gitem.item#show () else gitem.item#hide ()

let get_pos gitem = (gitem.x, gitem.y)

let get_angle gitem = gitem.angle

let mk_item ?(sys=default_sys) ?xyprod ?aprod ~x ~y ~ang item =
  let gitem =
    { sys ;
      alive = true ;
      x ;
      y ;
      angle = ang ;
      item }
  in
  update gitem ;

  let _ = item#connect#destroy (fun () -> gitem.alive <- false) in

  Events.oto_cons xyprod (fun (x,y) -> set_pos gitem ~x ~y) ;
  Events.oto_cons aprod (set_angle gitem) ;

  gitem

type canvas_click =
  { csys: coord_sys ;
    xpos: float ;
    ypos: float ;
    event: GdkEvent.Button.t }

open Events

type keys =
  | Up
  | Up_left
  | Up_right
  | Down
  | Down_left
  | Down_right
  | Left
  | Right
  | Char of string

type icanvas =
  { canvas: GnoCanvas.canvas ;
    clicks: canvas_click prod ;
    keystrokes: GdkEvent.Key.t prod ;
    keys: (int * keys) prod }

(* Move an adjustment by its step size *)
let adj_step adj k =
  let upper = adj#upper -. adj#page_size in
  adj#set_value (min upper (adj#value +. k *. adj#page_size *. 0.085)) ;
  true

type modifiers = [ `SHIFT | `CONTROL | `META | `MOD1 ] list

let add_mask acu = function
  | `SHIFT -> 1
  | `CONTROL -> 2
  | `META -> 4
  | `MOD1 -> 8
  | _ -> 0

let get_mask l = List.fold_left add_mask 0 l

let icanvas ?(with_scroll=false) ?arrow_keys ?aa ?border_width ~packing ?pixb () =

  let canvaspack =
    if with_scroll then

      (* Do not use add_with_viewport for a canvas. 
       * It works only by accident for positive-bounded canvas. *)
      let scrolled = GBin.scrolled_window ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS ~packing () in
      scrolled#add

    else packing
  in

  let (width, height) = match pixb with
    | None -> (100, 100)
    | Some p -> (GdkPixbuf.get_width p, GdkPixbuf.get_height p)
  in

  let canv = GnoCanvas.canvas ?aa ?border_width ~width ~height ~packing:canvaspack () in
  let _ = Common.option_map pixb (fun p -> GnoCanvas.pixbuf ~x:0.0 ~y:0.0 ~pixbuf:p ~props:[`ANCHOR `NW] canv#root) in

  canv#set_pixels_per_unit 1.0 ;
  canv#set_scroll_region ~x1:0.0 ~y1:0.0 ~x2:(foi width) ~y2:(foi height) ;

  let clicks = Events.mk_prod ~name:"gcanvas.clicks" ()
  and keystrokes = Events.mk_prod ~name:"gcanvas.keystrokes" () in

  (* Handle clicks & keys *)
  canv#event#add [ `BUTTON_PRESS ; `KEY_PRESS ] ;

  (*
  let _ = canv#hadjustment#connect#notify_value (fun v -> Printf.printf "Canvas hadj = %f\n%!" v) in
  let _ = canv#vadjustment#connect#notify_value (fun v -> Printf.printf "Canvas vadj = %f\n%!" v) in
   *)

  let _ = canv#event#connect#button_press
      ~callback:(fun event ->

          let winx = GdkEvent.Button.x event
          and winy = GdkEvent.Button.y event in

          let (xpos, ypos) = canv#window_to_world ~winx ~winy in

          Events.send clicks
            { csys = default_sys ;
              xpos ;
              ypos ;
              event } ;

          (* Return true if the event was handled. *)
          true
        )
  in

  let event2key event =
    let key = match GdkEvent.Key.keyval event with
      | 65431 | 65362 | 65464 -> Up    (* small arrows, num arrows, shift+num arrows *)
      | 65433 | 65364 | 65458 -> Down
      | 65430 | 65361 | 65460 -> Left
      | 65432 | 65363 | 65462 -> Right
      | 65434 -> Up_right
      | 65429 -> Up_left
      | 65436 -> Down_left
      | 65435 -> Down_right
      | n -> Char (GdkEvent.Key.string event)
    in
    (get_mask (GdkEvent.Key.state event), key)
  in

  ignore(canv#event#connect#key_press
           ~callback:(fun event ->
               let handled =
                 match arrow_keys with
                 | None -> false
                 | Some modf ->
                   (* That's a bit silly, because the low-level events certainly use masks, but 'state' is overriden in GdkEvent.Key. *)
                   let mask1 = get_mask modf in

                   let (mask2, key) = event2key event in
                   if mask1 = mask2 then
                     match key with
                     | Up -> adj_step canv#vadjustment (-1.0)
                     | Down -> adj_step canv#vadjustment 1.0
                     | Left -> adj_step canv#hadjustment (-1.0)
                     | Right -> adj_step canv#hadjustment 1.0
                     | _ -> false
                   else false
               in

               if not handled then Events.send keystrokes event ;
               true)) ;

  { canvas = canv ;
    clicks ;
    keystrokes ;
    keys = Events.map keystrokes event2key }

let map_clicks ~new_sys prod =
  let map_click cl =

    (* Coordinates in canvas default system. *)
    let cxy = cl.csys.get_xy (cl.xpos, cl.ypos) in

    (* Coordinates in new system. *)
    let (xpos, ypos) = new_sys.inv_xy cxy in

    { csys = new_sys ;
      xpos ;
      ypos ;
      event = cl.event }
  in
  Events.map prod map_click


(* xy in sys coordinates *)      
let center_on ?(sys=default_sys) icanv xy =

  let hadj = icanv.canvas#hadjustment
  and vadj = icanv.canvas#vadjustment in

  let (wx,wy) = sys.get_xy xy in
  let (cx,cy) = icanv.canvas#w2c ~wx ~wy in

  let x = iof (max hadj#lower (foi cx -. 0.5 *. hadj#page_size))
  and y = iof (max vadj#lower (foi cy -. 0.5 *. vadj#page_size)) in

  icanv.canvas#scroll_to ~x ~y ;
  ()

let set_bb icanv ?(sys=default_sys) points =
  let (x1, x2, y1, y2) =
    Common.myfold points (0.0, 1.0, 0.0, 1.0)
      (begin fun (x1, x2, y1, y2) pxy ->
         let (nx, ny) = sys.get_xy pxy in
         (min x1 nx, max x2 nx, min y1 ny, max y2 ny)
       end)
  in
  (* In w-coordinates *)
  icanv.canvas#set_scroll_region ~x1 ~y1 ~x2 ~y2 ;
  icanv.canvas#update_now () ;
  ()

let set_zoom icanv level = icanv.canvas#set_pixels_per_unit ((foi level) /. 100.0)

(* In sys-coordinates *)
type frame =
  { y_max: float ;
    y_min: float ;
    x_max : float ;
    x_min : float }

(* Rectangle in w-coordinates => rectangle in sys coordinates. *)
let get_frame canv sys ~wx1 ~wy1 ~wx2 ~wy2 =
  (* Printf.printf "Frame in w-coordinates = %.1f %.1f %.1f %.1f\n%!" wx1 wx2 wy1 wy2 ; *)
  let (x_min, y_min) = sys.inv_xy (wx1, wy1)
  and (x_max, y_max) = sys.inv_xy (wx2, wy2) in

  { x_min ; x_max ; y_min ; y_max }

(* Rectangle of the whole canvas. *)    
let get_global ?(sys=default_sys) icanv =
  let hadj = icanv.canvas#hadjustment
  and vadj = icanv.canvas#vadjustment in

  (**
   **      ATTENTION
   **        - S'il y a une erreur de typage ici (float au lieu de int pour les argument ~cx et ~cy),
   **          c'est que la version de lablgtk n'est pas correcte. Mettre à jour vers lablgtk.2.18.5
   **
   **)
  let (wx1, wy1) = icanv.canvas#c2w ~cx:(iof hadj#lower) ~cy:(iof vadj#lower)
  and (wx2, wy2) = icanv.canvas#c2w ~cx:(iof hadj#upper) ~cy:(iof vadj#upper) in

  (**
   **      Voir le message ci-dessus.
   **)
  

  get_frame icanv.canvas sys ~wx1 ~wy1 ~wx2 ~wy2

(* Visible rectangle in sys coordinates. *)     
let get_visible ?(sys=default_sys) ?(margin=0.0) icanv =
  let hadj = icanv.canvas#hadjustment
  and vadj = icanv.canvas#vadjustment in
  let (wx1, wy1) = icanv.canvas#c2w ~cx:(iof hadj#value) ~cy:(iof vadj#value) in

  let cx = iof (min hadj#upper (hadj#value +. hadj#page_size))
  and cy = iof (min vadj#upper (vadj#value +. vadj#page_size)) in

  (*  Printf.printf "hadj : [%f -- %f -- %f] ++ %f\n" hadj#lower hadj#value hadj#upper hadj#page_size ;
      Printf.printf "vadj : [%f -- %f -- %f] ++ %f\n" vadj#lower vadj#value vadj#upper vadj#page_size ; *)

  let (wx2, wy2) = icanv.canvas#c2w ~cx ~cy in

  get_frame icanv.canvas sys ~wx1:(wx1 +. margin) ~wy1:(wy1 +. margin) ~wx2:(wx2 -. margin) ~wy2:(wy2 -. margin)


let rec is_visible ?(do_center=false) ?sys ?margin icanv (x,y) =

  let frame = get_visible ?sys ?margin icanv in
  let result = frame.x_min <= x && x <= frame.x_max && frame.y_min <= y && y <= frame.y_max in

  (*  Printf.printf "%b %.1f <= wx = %.1f <= %.1f\n" result frame.x_min x frame.x_max ;
      Printf.printf "%b %.1f <= wy = %.1f <= %.1f\n\n%!" result frame.y_min y frame.y_max ; *)

  if do_center && not result then center_on ?sys icanv (x,y) ;

  result
