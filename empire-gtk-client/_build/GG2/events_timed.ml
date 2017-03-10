open Events

let sample ?target ?(only_change=false) ~period ~init proda =
  let (prodb, sendb) = new_prod ~name:(get_name "sample" proda) target in

  let last_value = ref init
  and changed = ref false in

  to_cons proda (fun v -> last_value := v ; changed := true) ;
  let f () =
    if only_change then
      if !changed then
        begin
          changed := false ;
          sendb !last_value
        end
      else ()
    else sendb !last_value ;
    Lwt.return_unit
  in
  let _ = Lwtdelay.periodic ~period f in
  prodb



(*** Time rate limiter ***)

type ('a, 'c) rate_limit =
  { category: 'a -> 'c option ;
    delay: 'c -> float }

let cst_cat x = { category = (fun _ -> Some ()) ; delay = (fun () -> x) }

(* Status of a category: date of last emission, or indicates that a timer is already running.
 * The timer will send the event in the given reference. *)    
type 'a limit_status = LastEmit of float | Timer of 'a ref

type ('a, 'b) cat =
  { get_delay: 'a -> float option ;
    get_status: 'a -> 'b ;
    set_status: 'a -> 'b -> unit }

let alongtimeago = Unix.gettimeofday () -. 20.0 *. 365.0 *. 24.0 *. 3600.0

let create_cat rl default =
  let table = Assoc.create ~size:4 ~init:(fun _ -> default) () in
  { get_delay = (fun evt -> Common.option_map (rl.category evt) rl.delay) ;
    get_status = (fun evt -> Assoc.get table (Common.option_get (rl.category evt))) ;
    set_status = (fun evt v -> Assoc.set table (Common.option_get (rl.category evt)) v) }

let limit ?target rate_limit prod =
  let cat = create_cat rate_limit (LastEmit alongtimeago) in

  mk_new_prod "limit" ?target prod
    begin fun sendb x ->
      match cat.get_delay x with
      | None -> sendb x
      | Some d ->
        begin match cat.get_status x with
          | LastEmit last ->
            let now = Unix.gettimeofday () in
            if now -. last >= d then
              (* this event is clear, we send it at once *)
              ( cat.set_status x (LastEmit now) ;
                sendb x )
            else
              begin
                (* Postpone sending event *)
                let evt_r = ref x in
                cat.set_status x (Timer evt_r) ;
                Lwt.async begin fun () ->
                  let%lwt () = Lwt_unix.sleep (d -. (now -. last)) in
                  cat.set_status x (LastEmit (Unix.gettimeofday ())) ;
                  sendb !evt_r ;
                  Lwt.return_unit
                end
              end

          | Timer evt_r ->
            evt_r := x ; (* Update upcoming event *)
        end
    end

type 'a stable_status =
  (* Delay & expected termination time. *)
  | Sleeping of ('a Lwtdelay.t * float)
  | Awake

let stabilized ?target rate_limit prod = 
  let cat = create_cat rate_limit Awake in

  mk_new_prod "stabilized" ?target prod
    begin fun sendb x ->
      match cat.get_delay x with
      | None -> sendb x
      | Some d ->
        begin match cat.get_status x with
          | Awake ->
            let delay = Lwtdelay.new_delay x d in
            let endtime = Unix.gettimeofday () +. d in
            cat.set_status x (Sleeping (delay, endtime)) ;
            Lwt.async begin fun () ->
              let%lwt y = Lwtdelay.sleep delay in
              cat.set_status x Awake ;
              sendb y ;
              Lwt.return_unit
            end

          | Sleeping (delay, expected_endtime) ->
            let now = Unix.gettimeofday () in
            let new_endtime = now +. d in
            cat.set_status x (Sleeping (delay, new_endtime)) ;
            let delta = new_endtime -. expected_endtime in
            Lwtdelay.increase_delay ~return:x delay delta ;
            ()
        end
    end
