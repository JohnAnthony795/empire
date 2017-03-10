
(* Like join, but for arbitrary types. *)
val join2: 'a Lwt.t -> 'b Lwt.t -> ('a*'b) Lwt.t

val choose2: 'a Lwt.t -> 'a Lwt.t -> 'a Lwt.t

(* Like wait, but returns a function to wake up the waiting thread. *)					
val fwait: unit -> ('a Lwt.t * ('a -> unit))
		     
(* Returns a thread that is permanently blocked. *)
val blocked: unit -> 'a Lwt.t

(* Like Async.lwt, but guarantees that the thread is not scheduled directly.
 * (Used inside Events consumers, to avoid unexpected InsaneLoops) *)
val later: (unit -> 'a Lwt.t) -> unit

(* Like later, but returns immediately Lwt.return_unit *)			
val async: (unit -> 'a Lwt.t) -> unit Lwt.t

(* Convenience, Lwt.async with argument. *)
val aasync: ('a -> 'b Lwt.t) -> 'a -> unit

(* paused f Makes a thread that is paused ; the evaluation of f is triggered only when the (unit -> unit) function is invoked for the first time. *)
val paused: (unit -> 'a Lwt.t) -> ('a Lwt.t * (unit -> unit))
(* Convenience : paused where we ignore the returned thread *)
val ipaused: (unit -> 'a Lwt.t) -> (unit -> unit)
				     
(* Lwt_sequence.fold_node_r is missing *)
val fold_node_r: ('a Lwt_sequence.node -> 'b -> 'b) -> 'a Lwt_sequence.t -> 'b -> 'b

(* Tests if the given thread contains an exception. If so, raise it immediately. Otherwise, return immediately Lwt.return_unit *)
val check_failed: exn Lwt.t -> unit Lwt.t

						 
(* Similar to Lwt.with_value, but with a different API.
 * Usage : Setkeys.( key1 %= value ++ key2 %= value ++ key3 %= value =>> fun ) 
 *
 * Beware, if a thread is called by a gtk-callback, the key can be unset (because the calling thread is the gtk main loop).
 *)
module Setkeys:
sig
  (* 'b: type returned by the evaluation of the main thread. *)
  type 'b config

  (* Sets a key/value pair *)
  val (%=): 'a Lwt.key -> 'a -> 'b config

  (* Concat two configs *)
  val (++): 'b config -> 'b config -> 'b config

  (* Launches Lwt.main (if keys hold global preferences, this is the recommended way to start the application) *)
  val (===>): 'a config -> (unit -> 'a Lwt.t) -> 'a

  (* Like ===>, but the argument is not a thead. *)
  val (=>>): 'b config -> (unit -> 'b) -> 'b
  
  val noconfig: 'a config
end

(* Configure LOG and handle exceptions. *)						 
val launch: ?configure_log:bool -> appname:string -> ?template:Lwt_log_core.template -> run:('b -> 'a Lwt.t) -> 'b -> unit -> 'a Lwt.t

