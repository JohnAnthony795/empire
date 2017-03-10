exception InsaneLoop of string

type 'a whenloop = (name:string -> 'a list -> 'a -> 'a option)

let ignore_loop ~name _ _ = None

let fixloop ?limit () = fun ~name l v ->
  match l with
  | [] -> assert false
  | x :: xs -> if x = v then None
    else
      begin match limit with
	| None -> Some v
	| Some lim ->
	  if List.length l >= lim then raise (InsaneLoop name)
	  else Some v
      end

type 'a prod =
  { mutable output: ('a -> unit) list ;

    (* Stack of recursive calls being processed now by this producer. 
     * Empty stack  = this producer is inactive for now.
     * One elements = this producer is being sending an event.
     * n elements   = this producer is calling itself recursively. *)
    mutable insend: 'a list ;

    mutable loop: 'a whenloop ;

    name: string }

let null () =
  { output = [] ;
    insend = [] ;
    loop = (fun ~name _ _ -> assert false) ;
    name = "empty" }

let unnamed = "(unnamed)"

let mk_prod ~name (* ?(name=unnamed) *) ?(loop=fun ~name _ _ -> raise (InsaneLoop name)) () = { output = [] ; insend = [] ; loop ; name }

let set_loop prod loop = prod.loop <- loop

let send prod v =

  let received =
    match prod.insend with
    | [] -> Some v
    | olds -> prod.loop ~name:prod.name olds v (* No need to catch exceptions here, it will be caught by the enclosing recursive call (below). *)
  in

  match received with
  | None -> () (* Do not propagate any further. *)
  | Some v ->
    let previous_stack = prod.insend in
    prod.insend <- v :: previous_stack ;

    (* No 'finally' in ocaml? *)
    try
      List.iter (fun f -> f v) prod.output ;
      prod.insend <- previous_stack
    with e ->
      prod.insend <- previous_stack ;
      raise e


let to_cons ?(last=false) prod f = 
  if last then prod.output <- prod.output @ [f]
  else prod.output <- f :: prod.output

let remove_cb ?(check=true) prod f =
  assert ( (not check) || List.memq f prod.output) ;
  prod.output <- List.filter (fun cb -> cb != f) prod.output					   

let to_unit prod f = to_cons prod (fun _ -> f ())

let to_prod pa pb = to_cons pa (send pb)

let map_to_prod pa f pb = to_cons pa (fun e -> send pb (f e))


let wait_on ?(pr = fun _ -> true) prod =
  let (res, f) = Lwtplus.fwait () in

  (* Recursive so that it can remove itself from the prod. *)
  let rec mycb v = if pr v then (f v ; remove_cb prod mycb) else () in

  to_cons prod mycb ;
  res

let new_prod ~name target =
  let p = match target with
    | None -> mk_prod ~name (*?name*) ()
    | Some p -> p
  in
  (p, send p)

let flatten ?target pa =
  let (pb, sendb) = new_prod ~name:"flatten" target in
  to_cons pa (fun l -> List.iter sendb l) ;
  pb

let new_cb cb =
  let prod = mk_prod ~name:"new_cb" () in
  to_cons prod cb ;
  prod

let get_name nm pa =
  if pa.name == unnamed then nm ^ "-."
  else nm ^ "-" ^ pa.name

let get_name2 nm pa pb = get_name (get_name nm pa) pb  

let join ?target proda prodb =
  let (prodc, sendc) = new_prod ~name:(get_name2 "join" proda prodb) target in
  to_cons proda sendc ;
  to_cons prodb sendc ;
  prodc

(* A producer created by applying a transformation to a first producer. *)    
let mk_new_prod opname ?target proda app =
  let (prodb, sendb) = new_prod ~name:(get_name opname proda) target in
  to_cons proda (app sendb) ;
  prodb

let only_change ?target ?(eq=(=)) proda =
  let previous = ref None in
  mk_new_prod "only_change"  ?target proda
    begin fun sendb x ->
      let hide =
	match !previous with
	| None -> false
	| Some old -> eq old x
      in

      if hide then ()
      else
	begin
	  previous := Some x ;
	  sendb x
	end
    end

let map ?target prod f = mk_new_prod "map" ?target prod (fun sendb x -> sendb (f x))
let mapfilter ?target prod f = mk_new_prod "mapfilter" ?target prod (fun sendb x -> match f x with None -> () | Some y -> sendb y)

let async_map ?target prod f = mk_new_prod "async_map" ?target prod (fun sendb x -> Lwt.async (fun () -> let%lwt y = f x in sendb y ; Lwt.return_unit))
let async_mapfilter ?target prod f = mk_new_prod "async_mapfilter" ?target prod
    (fun sendb x -> Lwt.async (fun () -> match%lwt f x with None -> Lwt.return_unit
							  | Some y -> sendb y ; Lwt.return_unit))

let ujoin ?target proda prodb = join ?target (map proda (fun _ -> ())) (map prodb (fun _ -> ()))

let invmap prodb f =
  let proda = mk_prod ~name:"invmap" () in
  to_cons proda (fun v -> send prodb (f v)) ;
  proda

let partition ?targeta ?targetb prod pred =
  let (proda, senda) = new_prod ~name:(get_name "partition-1" prod) targeta
  and (prodb, sendb) = new_prod ~name:(get_name "partition-2" prod) targetb
  in
  to_cons prod (fun evt -> if pred evt then senda evt else sendb evt) ;
  (proda, prodb)

let activable ?target prod =
  let active = ref true in
  (active, mapfilter ?target prod (fun x -> if !active then Some x else None))

let switchable switch ?target prod =
  mapfilter ?target prod (fun v -> if Lwt_switch.is_on switch then Some v else None)

let until ?target proda prodb p =
  let (prodc, sendc) = new_prod ~name:(get_name2 "until" proda prodb) target in
  let from_b = ref false in
  to_cons proda (fun evt -> if not !from_b then (sendc evt ; from_b := p evt)) ;
  to_cons prodb (fun evt -> if !from_b then sendc evt) ;
  prodc

let oto_cons ?last oprod f = match oprod with
  | None -> ()
  | Some p -> to_cons ?last p f

