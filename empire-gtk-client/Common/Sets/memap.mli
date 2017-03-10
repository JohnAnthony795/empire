(*** Maps of mergeable keys (see Meset). ***)

(* Keys are mergeable. The content of a key can only 'grow'. 
 * Keys are mapped to values. Values can change, this is the 'mutable' part of the object. *)

module type M =
sig

  type key
    
  type 'a map

  module Keyset: Meset.S with type elt = key

  (* tos: Mandatory printer function. Used in x2string functions. 
   * mergev: when keys are merged, associated values are merged too. *)
  val empty: tos:('a -> string) -> mergev:('a -> 'a -> 'a) -> 'a map

  (* Number of bindings == number of keys in this map. *)
  val size: 'a map -> int
  
  (* Return the set of keys. *)
  val keys: 'a map -> Keyset.set

  (* Merge a key into the map and updates the binding.
   * If keys are merged together, the associated values are merged too. 
   *
   * ?f: function applied to update the value (gets the merged key and the current value, returns the updated value)
   * ?default: value to be passed to f if the key does not exist yet. (default will be given the argument key)
   *
   * @raise Elt_not_found if the key does not exist and default is not specified.
   * @raise same exceptions than Meset.merge_in *)
  val update: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map
  val update_k: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map * key

  (* Derived functions
   * Adds a new binding or replace an existing binding. *)
  val add: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map
  val add_k: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map * key

  val find_result2s: 'a map -> (key * 'a) Meset.mem_result -> string
  val find: no:bool -> 'a map -> key -> (key * 'a) Meset.mem_result
  
  (* remove mp key: the key must be physically equal to a key in the map (as returned by find). *)
  val remove: 'a map -> key -> 'a map

  (* See Set.fold for the meaning of comparekey and compare. *)
  val fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c) -> 'c

  (* to_string *)
  val map2s: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> ?title:string -> 'a map -> string

  (* Maps a map. Must provide a merge function. *)
  val mmap: 'a map -> ('b -> 'b -> 'b) -> ('a -> 'b) -> ('b -> string) -> 'b map
    
end


module Make : functor (Elt: Meset.ELEMENT) -> M with type key = Elt.elt

