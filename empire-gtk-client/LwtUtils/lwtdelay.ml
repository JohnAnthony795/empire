open Common.Small

type 'a t =
  { mutable terminated: bool ;
    mutable return: 'a ;

    (* Next delay to sleep once the current delay is over. *)
    mutable next_delay: float option ;
  }

let new_delay return dur =
  { terminated = false ;
    return ;
    next_delay = Some dur }

let is_terminated d = d.terminated

let mod_value d v =
  if is_terminated d then failwith "Lwtdelay.mod_value: delay is terminated. " ;
  d.return <- v

let increase_delay ?return delay delta =
  if delta < 0.0 then failwith "Lwtdelay.increase_delay: delay is must be positive." ;
  if is_terminated delay then failwith "Lwtdelay.increase_delay: delay is terminated. " ;
  Common.option_iter return (mod_value delay) ;
  delay.next_delay <- Some (match delay.next_delay with None -> delta | Some x -> x +. delta) ;
  ()

let sleep delay =
  if is_terminated delay then failwith "Lwtdelay.sleep_delay: delay is terminated. " ;

  let rec loop () = match delay.next_delay with
    | None -> Lwt.return_unit
    | Some d ->
      delay.next_delay <- None ;
      Lwt_unix.sleep d >>
      loop ()
  in
  let%lwt () = loop () in
  delay.terminated <- true ;
  Lwt.return delay.return

type control = [ `Pause | `Run ]

let control2s = function
  | `Pause -> "Pause"
  | `Run -> "Run"

let periodic ?switch ?(init=`Run) ?ctl ~period f =

  let state = ref init in

  (* Indicate if a thread is already launched *)
  let is_launched = ref false in

  let rec launch () =
    if !is_launched then ()
    else
      begin
        is_launched := true ;
        Lwt.async
          begin fun () ->
            let%lwt () = Lwt_unix.sleep period in
            is_launched := false ;
            Lwt_switch.check switch ;
            match !state with
            | `Pause -> Lwt.return_unit (* If Paused, we stop the recursion. *)
            | `Run -> launch () ;
              let%lwt _ = f () in Lwt.return_unit
          end
      end
  in
  if init = `Run then launch () ;

  (* Control function *)
  let control = function
    | `Pause -> state := `Pause
    | `Run  -> state := `Run ; launch () (* Works also if `Run is called repeatedly *)
  in
  Events.oto_cons ctl control ;
  control


let ramp ?switch ?ctl prod ~from ~delta ~timestep ~n () =

  (* We do not optimize flat ramps, so that they can be paused. *)

  let state = ref `Run in

  let rec aux v n =
    Lwt_switch.check switch ;
    Events.send prod v ;

    if n <= 0 then Lwt.return_unit
    else
      begin
        (* Sleep & pause if necessary *)
        Lwt_unix.sleep timestep >>
        let%lwt _ = match !state with
          | `Pause ctlp -> let%lwt _ = Events.wait_on ~pr:(fun c -> c = `Run) ctlp in Lwt.return_unit
          | _ -> Lwt.return_unit
        in
        aux (v +. delta) (n-1)
      end
  in

  Common.option_iter ctl (fun ctlp -> Events.oto_cons ctl (function `Run -> state := `Run
                                                                  | `Pause -> state := `Pause ctlp)) ;  
  aux from n

let iramp ?switch ?ctl prod ~from ~delta ~timestep ~n () =
  ramp ?switch ?ctl (Events.invmap prod niof) ~from:(foi from) ~delta:(foi delta) ~timestep ~n ()

let ramps_seq ?switch ?ctl prod ~from ~seq ~loop ~timestep () =

  if seq = [] then Lwt.return_unit
  else
    let rec aux current = function
      | [] -> if loop then aux current seq else Lwt.return_unit (* Finished *)
      | (target, time) :: rest ->
        let n = max 1 (niof (time /. timestep)) in
        let delta = (target -. current) /. (foi n) in

        ramp ?switch ?ctl prod ~from:current ~delta ~timestep ~n () >>
        aux target rest
    in

    aux from seq

let iramps_seq ?switch ?ctl prod ~from ~seq ~loop ~timestep () =
  ramps_seq ?switch ?ctl (Events.invmap prod niof) ~from:(foi from) ~seq:(List.map (fun (a,b) -> (foi a,b)) seq) ~loop ~timestep ()
