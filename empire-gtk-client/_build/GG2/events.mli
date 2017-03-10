(*** Events channels ***)

(* Note: seems similar to React, but not sure the semantics corresponds. *)

(* Note also that asynchronous behaviour (time rate limits, async) make use of the Lwt library.
 * Thus, it should be used inside a Lwt main loop. 
 * 
 * Concerning Lwt, note that callbacks (defined in to_prod) should use Lwtplus.later instead of Lwt.async 
 * to avoid unexpected InsaneLoops. *)


(* Exception raised when a producer directly or indirectly feeds itself.
 *    InsaneLoop name    (name of the producer)
 * (By default, this is considered as bad design: most probably an infinite loop.)
 * The behaviour can be customized though, see 'whenloop' below. *)
exception InsaneLoop of string

(* A channel, (a 'producer'), carrying events of type 'a. *)
type 'a prod

(* Function called when a loop is detected inside a producer (a producer feeds itself, possibly indirectly).
 * The function receives the producer name (for debug),
 *   the call stack of previous recursive events as well as the current event. The list is guaranteed to contain at least one element.
 * It may
 *   - raise InsaneLoop (by default), or any other exception. In this case, propagation of the current event is aborted and exception propagated upwards.
 *   - do not propagate further (None),
 *   - or act as if the given value was received (Some v'), which then replaces the current value. *)
type 'a whenloop = (name:string -> 'a list -> 'a -> 'a option)

(* Makes a new producer.
 * The name is used to ease debug. *)
val mk_prod: name:string -> ?loop:'a whenloop -> unit -> 'a prod

(* A null producer. Never produces anything. *)
val null: unit -> 'a prod

(* Replaces previous behaviour. *)
val set_loop: 'a prod -> 'a whenloop -> unit

(*** Note: consider using Lwtplus.later to avoid insane loops. *)

(* Convenience function: ignore loops in whenloop (do not propagate further). *)
val ignore_loop: 'a whenloop
    
(* Convenience whenloop which stop propagation as soon as a fixpoint is observed.
 * @raise InsaneLoop if recursion limit is reached. If limit=1, InsaneLoop is raised at first loop (if the fixpoint is not observed). *)
val fixloop: ?limit:int -> unit -> 'a whenloop

(* Sends a message on this channel (produces a message). *)
val send: 'a prod -> 'a -> unit

(* Convenience function: new_prod None returns a new producer and its sender function.
 * new_prod Some p returns p and its sender function. *)
val new_prod: name:string -> 'a prod option -> 'a prod * ('a -> unit)

(* Link a producer to a consumer (that is, a callback).
 * A producer can be linked to several consumers. 
 * last: add to the end of the queue instead of the beginning. *)
val to_cons: ?last:bool -> 'a prod -> ('a -> unit) -> unit

(* Convenience function: a producer directly connected to a callback. *)
val new_cb: ('a -> unit) -> 'a prod

(* Convenience: to_cons with an optional producer *)
val oto_cons: ?last:bool -> 'a prod option -> ('a -> unit) -> unit
			       
(* Remove a callback added with to_cons. Uses physical equality. 
 * fails if the callback is unknown and check is true. *)
val remove_cb: ?check:bool -> 'a prod -> ('a -> unit) -> unit					  

(* Convenience *)					    
val to_unit: 'a prod -> (unit -> unit) -> unit

(* to_prod pa pb  sends all events from pa to pb. *)
val to_prod: 'a prod -> 'a prod -> unit

(* map_to_prod pa f pb send all events from pa to pb, mapped by f. *)
val map_to_prod: 'a prod -> ('a -> 'b) -> 'b prod -> unit

(* Flatten list to another prod *)
val flatten: ?target:'a prod -> 'a list prod -> 'a prod

(* Wait until an event is received. You may filter by providing a predicate. *)
val wait_on: ?pr:('a -> bool) -> 'a prod -> 'a Lwt.t


(*** Filters ***)

(* Sends only events that are different from the previous event. 
 * (That is, only changes are propagated). *)
val only_change: ?target:'a prod -> ?eq:('a -> 'a -> bool) -> 'a prod -> 'a prod
			   
			   
(*** COMBINATORS ***)

(* All combinators have an optional argument ?target.
 * If target is not specified, those combinators create a new fresh producer.
 * If target is specified, the combinators send their results to it. *)

val join: ?target:'a prod -> 'a prod -> 'a prod -> 'a prod

(* Convenience *)
val ujoin: ?target:unit prod -> 'a prod -> 'b prod -> unit prod

(* Map, synchronous: events are immediately produced *)
val map: ?target:'b prod -> 'a prod -> ('a -> 'b) -> 'b prod
val mapfilter: ?target:'b prod -> 'a prod -> ('a -> 'b option) -> 'b prod

(* Builds an 'a producer whose events will be mapped to an existing 'b producer. *)
val invmap: 'b prod -> ('a -> 'b) -> 'a prod
								     
(* Partition *)
val partition: ?targeta:'a prod -> ?targetb: 'a prod -> 'a prod -> ('a -> bool) -> ('a prod * 'a prod)

(* Map, asynchronous: events are produced when the lwt thread returns *)
val async_map: ?target:'b prod -> 'a prod -> ('a -> 'b Lwt.t) -> 'b prod
val async_mapfilter: ?target:'b prod -> 'a prod -> ('a -> 'b option Lwt.t) -> 'b prod

(* Activable producer. Use the given reference to activate/disactivate the producer.
 * Initially active. *)
val activable: ?target:'a prod -> 'a prod -> (bool ref * 'a prod)

val switchable: Lwt_switch.t -> ?target:'a prod -> 'a prod -> 'a prod
					       

(* Forwards all events from proda until predicate is true, then forwards all events from prodb. *)
val until: ?target: 'a prod -> 'a prod -> 'a prod -> ('a -> bool) -> 'a prod


(*** Unimportant functions ***)
val get_name: string -> 'a prod -> string

val mk_new_prod: string -> ?target:'b prod -> 'a prod -> (('b -> unit) -> 'a -> unit) -> 'b prod
				     
