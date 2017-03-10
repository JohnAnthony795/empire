(*
 * Client for Alata's Empire server 
 *
 *  author: Le Botlan
 *)

(* Logging module *)
module Log = Mylog.MkSection (struct let section_name = "Clienmpire.ml" end)
open Log

open Arg
open Connect

let observe = ref false
let port = ref 9301
    
let spec = [ "-obs", Set observe, " \t Observe only." ;
             "-port", Set_int port, " \t Specify port." ]

let usage = "cliempire -obs -port pnum"

let run () =
  log Info "Launching Empire client" >>

  let () = Arg.parse spec (fun _ -> ()) "" in
  
  let emap = Emap_graphics.init () in

  let intf =
    { rcv_msg = Events.(mk_prod ~name:"rcv_msg" ~loop:(fixloop ~limit:2 ()) ()) ;
      raw_received = Events.mk_prod ~name:"received" () ;
      raw_emitted = Events.mk_prod ~name:"emitted" () ;
      board = Lwtboard.create () }
  in

  let observe_only = !observe in
  
  let gui_finished = Gui.go ~observe_only intf in

  (* Emap initialisation must be finished before proceeding. *)
  emap >>
  connect ~ip4:"127.0.0.1" ~port:!port ~observe_only intf >>
  gui_finished


(* Global settings *)
open Lwtplus.Setkeys

let () =

  let main_dir = "./Images" in
  Ggimage.defaultdir %= main_dir

  (* Configures LOG & launch lwt. *)			  
  ===> Lwtplus.launch ~configure_log:true ~appname:"Cliempire" ~run ()
