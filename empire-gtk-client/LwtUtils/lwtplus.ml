open Lwt

let aasync f arg = Lwt.async (fun () -> f arg)

let later f = Lwt.async (fun () -> let%lwt () = Lwt_unix.yield () in f ())

let async f = (later f ; Lwt.return_unit)

let join2 a b =
  let%lwt va = a
  and vb = b
  in
  return (va, vb)

let choose2 a b = choose [ a ; b ]

let check_failed a =
  match Lwt.state a with
  | Return e -> [%lwt raise e]
  | Sleep -> Lwt.return_unit
  | Fail e -> [%lwt raise e]

let fwait () =
  let (x, w) = wait () in
  (x, wakeup w)

let blocked () = fst (wait ())

let fold_node_r f seq acu =
  let acu = ref acu in
  Lwt_sequence.iter_node_r (fun node -> acu := f node !acu) seq ;
  !acu

let (>>=) key v f = Lwt.with_value key (Some v) f

module Setkeys =
struct
  type 'b config = (unit -> 'b) -> 'b
  let (%=) key v = (fun f -> Lwt.with_value key (Some v) f)
  let (++) cf1 cf2 = (fun f -> cf2 (fun () -> cf1 f))
  let (=>>) cf f = cf f
  let (===>) cf f = cf (fun () -> Lwt_main.run (f ()))
  let noconfig = (fun f -> f ())
end

module Log = Mylog.MkSection (struct let section_name = "Error" end)
open Log

let launch ?(configure_log=true) ~appname ?template ~run arg () =

  (* Handle async exceptions too. *)
  Lwt.async_exception_hook :=
    (fun e -> alog_f ~exn:e Fatal "***************************************** %s error ****************************************************************\n%s\n"
        appname (Printexc.get_backtrace ())) ;
  
  try%lwt
    let%lwt () = if configure_log then Mylog.full_init ~appfile:true ?template appname
      else Lwt.return_unit
    in
    run arg
  with e ->
    let%lwt () = log_f ~exn:e Fatal "***************************************** %s error ****************************************************************\n%s\n"
        appname (Printexc.get_backtrace ()) in
    [%lwt raise e]

let paused f =
  let wait, resume = fwait () in
  let t =
    let%lwt () = wait in
    f ()
  in
  (t, (fun () -> if Lwt.is_sleeping wait then resume () else ()))

let ipaused f =
  let (t, r) = paused f in
  Lwt.ignore_result t ;
  r
