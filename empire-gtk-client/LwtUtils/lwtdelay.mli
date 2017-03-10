(* A sleep that can be increased once started. 
 * When terminated, returns a value of type 'a. *)
type 'a t

(* Give the initial delay and return value.
 * Start sleeping using sleep_delay. *)
val new_delay: 'a -> float -> 'a t

(* Indicate if this delay is terminated. *)
val is_terminated: 'a t -> bool
			  
(* Increase this delay by a positive amount. Optionally modifies the return value.
 * Fails if the delay is already terminated. *)
val increase_delay: ?return:'a -> 'a t -> float -> unit

(* Modifies the return value.						      
 * Fails if the delay is already terminated. *)
val mod_value: 'a t -> 'a -> unit
						      
(* Wait for the given delay. It can be increased while sleeping.
 * Fails if the delay is already terminated. *)
val sleep: 'a t -> 'a Lwt.t
				   
					    

(* Used to control periodic functions. 
 * When paused, there is no extra cost for waiting. *)
type control = [ `Pause | `Run ]
val control2s: control -> string

open Events
			    
(* Periodic calls to f. Returns a control function. 
 * The initial state is Run, unless specified otherwise. 
 * The control can also be done using the given producer. *)
val periodic: ?switch : Lwt_switch.t -> ?init:control -> ?ctl: control prod -> period:float -> (unit -> 'a Lwt.t) -> (control -> unit)



(* 'Ramp' function. 
 * Emits float values starting at 'from', increased by 'delta' every 'timestep' seconds.
 * Emits n samples.
 * Returns when the ramp is finished (as soon as the nth sample is emitted). 
 * Can be controlled by ctl (does not pause the current delay, but pauses between steps). *)
val ramp: ?switch : Lwt_switch.t -> ?ctl: control prod -> float prod -> from:float -> delta:float -> timestep:float -> n:int -> unit -> unit Lwt.t

(* Convenience : ramp with integers *)
val iramp: ?switch : Lwt_switch.t -> ?ctl: control prod -> int prod -> from:int -> delta:int -> timestep:float -> n:int -> unit -> unit Lwt.t
																 
(* Ramps_seq: execute each ramp one after another.
 * Each ramp is specified by its target value and the time it takes. (target, delay)
 * The sequence may loop. Then, the last element of the sequence 'seq' is followed by the first element of the sequence 'seq'.
 * Emits intermediate values. float / integer ?
 *  Careful : un plateau est possible
 * utilise une fonction basique : n steps +delta, delay
 *)
val ramps_seq: ?switch : Lwt_switch.t -> ?ctl: control prod -> float prod -> from:float -> seq:(float*float) list -> loop:bool -> timestep:float -> unit -> unit Lwt.t
      


(* Convenience : ramps_seq with integers *)
val iramps_seq: ?switch : Lwt_switch.t -> ?ctl: control prod -> int prod -> from:int -> seq:(int*float) list -> loop:bool -> timestep:float -> unit -> unit Lwt.t
