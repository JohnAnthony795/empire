(*** Trees as event vals ***)

(* Note: this module provides a tree model which can be slightly customized (type 'a tree),
 * but you cannot use your own tree structure here. (Reason : mutable tree structures are hard to handle correctly).
 * The type 'a tree should be understood as a direct mapping of the tree view (and conversely).
 *
 * More elaborate structures (e.g. your own mutable tree structure, or large trees which are displayed only one part at a time)
 * could be built later on top of this module.
 *
 * Tree_evt_val are "shallow" strucutres : they are not expected to contain evt-values.
 * *)


(* A tree = a node which contains an 'a value and childs.
*  'a can be something like Node of 'b | Leaf of 'c, 
*  or your own tree representation with a getter for childs. 
*
*  Note that the root node is invisible (only its childs are visible). *)
type 'a tree =
  { v : 'a ;
    childs : 'a tree list }

(* Convenience: a tree with no childs. *)    
val leaf: 'a -> 'a tree
    
(*** A path to identify a node in a tree ***)
		      
type 'a path_elt =
  (* Take the nth child, starting at 1. *)
  | Nth of int

  (* Take the first child whose value satisfy this predicate. 
   * The integer is the child position (starting at 1) *)
  | Pred of (int -> 'a -> bool)

(* Empty list = root node, which is invisible ! *)
type 'a path = 'a path_elt list

val path2s: 'a path -> string

exception Bad_path of string

(* @raise Bad_path *)
val get_subtree: 'a tree -> 'a path -> 'a tree
val get_val: 'a tree -> 'a path -> 'a
val get_childs: 'a tree -> 'a path -> 'a tree list

(* Normalize path *)					 
val norm_path: 'a tree -> 'a path -> 'a path

(* Depth: 0 by default (i.e., first level only). -1 means no limit. *)					
val iter_childs_at: ?depth:int -> 'a tree -> 'a path -> ('a path -> 'a -> unit) -> unit
		  
(*** Events emitted by a tree. *)
		  
type 'a tree_evt =
  [
  (* The whole tree has changed. *)
  | `Value_Changed of 'a tree

  (* A subtree is removed.
   * path must not be empty. @raise Bad_path otherwise
   * Use a dummy value at invocation. The event sends the deleted subtree. *)
  | `Node_deleted of ('a path) * ('a tree)

  (* Inserts a subtree at the given path. 
   * If the path points to a child c, the subtree is inserted at the place of c, and c is put next in the child list.
   * If the last element of the path does not point to a child, it appends the subtree at the end of the child list. 
   * When emitted, only contains Nth elements in the path (no predicates). *)
  | `Node_inserted of ('a path) * ('a tree)

  (* The subtree at the given path has changed its value (not its childs). *)				    
  | `Node_value_changed of ('a path) * 'a ]


(* Builds a tree val. You must provide a (useless, invisible) value for the root node. *)
class ['a] tree_val: ?name:string -> ?target:('a tree_evt) Events.prod -> 'a ->
      object
	inherit ['a tree, 'a tree_evt] Evt_val.evt_val

	method value: 'a tree
				    
	method apply_event: 'a tree_evt -> unit
				       
        (* Delete the tree and replace it with a single node having the given (useless) value. *)
	method clear: 'a -> unit

        (* Deletes a node. Returns the deleted subtree. *)			      
	method delete_node: 'a path -> 'a tree

	(* Inserts a subtree (see `Node_inserted above). *)
	method insert_node: 'a path -> 'a tree -> unit

        (* Replaces all the tree (clear + insert_node) *)
	method replace_all: 'a tree -> unit
						    
	(* Change the value of a node *)
	method set_node_val: 'a path -> 'a -> unit

	(* If the node contains mutable values, invoke this method when something changes. *)
	method updated_node: 'a path -> unit

        (* Fold: the function is invoked on every node (every path). *)						
	method fold: 'b . 'b -> ('b -> 'a path -> 'a tree -> 'b) -> 'b

      end

	
(* There is no obvious good solution to handle mutable values for the moment. 
 * It is hard to memoize the positions (paths) of all mutable nodes (because insertions will shift the paths & also there can be duplicates).
 *
 * For the moment, the user has to send himself the updated_node signals. *)
