(*** Event values (values that emit events when something happens. ***)

(*  Events can be replayed on values (by using #apply_event) in order to produce the same effect.
 *  Formally, if v1 --> v2 by emitting event e, then v1#apply_event e leads to value v2.
 *
 *  Thus events are a precise trace of what happened to the value, which can be used to replay the sequence of transformations.
 *  Notice that apply_event re-emits an event.
 *)

(*  class evt_val represents an event value, whose natural representation is of type 'a and whose events are of type 'evt. 
 * 'evt must contain at least Value_Changed of 'a. It indicates that the 'whole' value has changed (local changes should emit more precise events when possible).
 *
 *  gtk: gtk-like cb registering function, which triggers #vchanged when invoked. 
 *  cb: registers a cb to the event producer. 
 *  One must not specify both a cb and a target producer. This is considered as an error.
 *
 *  #vchanged does not invoke #apply_event (called when the value has already changed).
 **)

(* target allows to reuse an existing producer (see Events combinators) *)
open Events

class virtual ['a, 'evt] evt_val: name:string ->
	      ?cb:('evt -> unit) -> ?target:'evt prod -> ?setter: 'a prod -> ?gtk:(callback:(unit -> unit) -> GtkSignal.id) -> unit ->
      object
	constraint 'evt = [> `Value_Changed of 'a ]
			    
	(* The associated event producer. *)
	method prod: 'evt prod

	method send: 'evt -> unit

	(* Reads the current value *)
	method virtual value: 'a
				    
	method virtual apply_event: 'evt -> unit
					      
	(* Signals that the value has changed (emits an appropriate event). *)
	method vchanged: unit
			   
	(* Sets a new value (convenience). *)
	method set_value: 'a -> unit
      end

(* Base class: value with only one event, namely Value_Changed
 * fset: base function to set the underlying representation (reference or whatever). 
 * fget: base function to read the underlying representation *)
class ['a] simple_val: name:string -> ?cb:('a -> unit) -> ?target:'a prod -> ?gtk:(callback:(unit -> unit) -> GtkSignal.id) ->
      ?setter: 'a prod -> fset:('a -> unit) -> fget:(unit -> 'a) -> unit ->
      object
	inherit ['a, [`Value_Changed of 'a]] evt_val
	method apply_event: [`Value_Changed of 'a] -> unit
	method value: 'a
	method vprod: 'a prod
      end

val cst_val: 'a -> 'a simple_val

(* Base class with inner reference. *)
class ['a] ref_val: ?name:string -> ?cb:('a -> unit) -> ?target:'a prod -> ?setter: 'a prod -> init:'a -> unit -> ['a] simple_val

val vref: ?cb:('a -> unit) -> ?target:'a prod -> ?setter: 'a prod -> 'a -> 'a ref_val

	
(*****  GROUND INSTANCES  *****)

(* FLOATS *)	
class adjfloat: ?name:string -> ?cb:(float -> unit) -> ?target:float prod -> ?setter:float prod ->
		lower:float -> upper:float -> step_incr:float -> page:float -> init:float -> unit ->
      object
	inherit [float] simple_val
	method adjustment : GData.adjustment
      end

(* INTS *)
class adjint: ?name:string -> ?cb:(int -> unit) -> ?target:int prod -> ?setter:int prod ->
	      lower:int -> upper:int -> step_incr:int -> page:int -> init:int -> unit ->
      object
	inherit [int] simple_val
	method adjustment : GData.adjustment
      end

