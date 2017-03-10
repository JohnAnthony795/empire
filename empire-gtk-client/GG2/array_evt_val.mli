open Evt_val

(*** An array event value ***)

(* This is a 'deep' structure : each cell is expected to contain an event-value. *)

(* Events emitted by an array_val
 * 'a is the event val contained in each cell. *)
type 'a array_evt =
  [
  (* The whole array has changed. *)
  | `Value_Changed of 'a array

  (* Cell #i produces an event. *)
  | `Cell of int * 'evt

  (* Insert a new cell at position #i. The previous cell at i is now at i+1. 
   * We must have 0 <= i <= array_length.
   * (As expected, the array length is incremented.) *)
  | `Insert of int * 'a

  (* Replace the cell at position #i. *)				
  | `Replace of int * 'a

  (* Delete the cell at position #i. The following cells are shifted backwards. 
   * We must have 0 <= i < array_length
   * (As expected, the array length is decremented.) *)
  | `Delete of int
		 
  ] constraint 'a = ('b, 'evt) #evt_val
    

    
(* Build an array containing values of type 'a = ['b, 'evt] evt_val
 * #value and #varray both allocate a new array. 
 * The init value may be used and modified by the array_val.
 * *)
class ['a] array_val: ?name:string -> ?cb:('a array_evt -> unit) -> ?target:('a array_evt) Events.prod -> init:'a array -> unit ->
      object
	constraint 'a = ('b, 'evt) #evt_val
				   
	inherit ['a array, 'a array_evt] evt_val

	method value: 'a array
	method varray: 'b array

	(* Use these to avoid to allocate arrays with #value or #varray. *)
	method length: int
			 
	method vget: int -> 'b
	method vset: int -> 'b -> unit

	method get: int -> 'a
	method set: int -> 'a -> unit

	method insert: int -> 'a -> unit
	method append: 'a -> unit
	method delete: int -> unit

	method apply_event: 'a array_evt -> unit
				       
	method fold: 'c . 'c -> ('c -> int -> 'a -> 'c) -> 'c
	method vfold: 'c . 'c -> ('c -> int -> 'b -> 'c) -> 'c

      end


