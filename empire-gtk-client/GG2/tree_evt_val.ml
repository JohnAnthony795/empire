type 'a tree =
  { v : 'a ;
    childs : 'a tree list }
		      
type 'a path_elt =
  | Nth of int
  | Pred of (int -> 'a -> bool)
	     
type 'a path = 'a path_elt list

exception Bad_path of string

let pathelt2s = function
  | Nth i -> string_of_int i
  | Pred p -> "[pr]"
			
let path2s l = Common.sep pathelt2s " : " l

let bad_path s = raise (Bad_path s)

let leaf v = { v ; childs = [] }
		       
let get_cond = function
  | Nth i -> (fun k _ -> k = i)
  | Pred p -> p

(* rb : childs already seen
 * p predicate
 * i position *)		
let rec find_child rb p i = function
  | [] -> bad_path "No more childs"
  | el :: els -> if p i el.v then (el, i, rb, els) else find_child (el :: rb) p (i+1) els

let fst4 (a,b,c,d) = a
							  
let rec get_subtree tree path =
  assert (not (tree.childs = [] && path <> [])) ;
  match path with
  | [] -> tree
  | cond :: rest -> get_subtree (fst4 (find_child [] (get_cond cond) 1 tree.childs)) rest

let get_val tree path = (get_subtree tree path).v
					       
let get_childs tree path = (get_subtree tree path).childs

let rec iter_childs_at ?(depth=0) tree path f =
  let sub = get_subtree tree path in
  Common.myiteri sub.childs (fun i ch -> let subpath = path @ [Nth (i+1)] in
					 f subpath ch.v ;
					 if depth <> 0 then iter_childs_at ~depth:(depth - 1) tree subpath f) ;
  ()

let norm_path tree path =
  let rec aux tree acu = function
    | [] -> List.rev acu
    | cond :: rest ->
       let (el, pos, _, _) = find_child [] (get_cond cond) 1 tree.childs in
       aux el (Nth pos :: acu) rest
  in
  aux tree [] path
    
(* Generic function which modifies the tree at a given path. 
 * f receives the subtree located at path (or path without its last element if ignore_last is true) 
 *            and the last path element (None / Some last depending on ignore_last)
 * f must return a pair of the new subtree and its return value. *)
let rec tree_apply ignore_last rpath f tree = function
  | [] -> if ignore_last then bad_path "Empty path" else (f tree None, rpath)
  | [last] when ignore_last -> (f tree (Some last), rpath)
  | cond :: rest ->
     let (child, pos, rbefore, after) = find_child [] (get_cond cond) 1 tree.childs in
     let ((newchild, res), rpath) = tree_apply ignore_last (Nth pos :: rpath) f child rest in
     (* Build the new child *)
     (( { v = tree.v ; childs = List.rev_append rbefore (newchild :: after) }, res),  rpath)

let tree_delete tree path =
  let dummy_tree = { v = get_val tree [] ; childs = [] } in
  (* Find where is the subtree to be deleted. Replace it with the dummy tree. *)
  let ((newtree1, deleted), rpath) = tree_apply false [] (fun del _ -> (dummy_tree, del)) tree path in
  
  (* Remove the dummy tree from the child list. *)
  let ((newtree2, ()), _) = tree_apply true [] (fun {v ; childs} _ -> ( { v ; childs = List.filter (fun c -> c != dummy_tree) childs }, ())) newtree1 path in
       
  (newtree2, List.rev rpath, deleted)

(* Insert 'sub' inside the list of childs, at the position given by (Some el). *)    
let insert_childs childs sub = function
  | None -> assert false (* tree_apply must return Some last-element because ignore_last is true. *)
  | Some el ->
     let rec aux acu i childs = match (childs, el) with
       | [], _ -> (List.rev (sub :: acu), i)
       | ch :: others, cond -> if get_cond cond i ch.v then ((List.rev (sub :: acu)) @ childs, i)
			       else aux (ch :: acu) (i+1) others
     in
     aux [] 1 childs

(* Insert a subtree. Shifts siblings if necessary. *)	 
let tree_insert tree path sub =
  if path = [] then (sub, [])
  else
    let ((newtree, pos), rpath) =
      tree_apply true []
		 begin fun {v ; childs} last ->
		       let (new_childs, pos) = insert_childs childs sub last in
		       ({ v ; childs = new_childs }, pos)
		 end
		 tree path
    in
    (newtree, List.rev (Nth pos :: rpath))
    
let tree_set_val tree path v =
  let ((newtree, ()), rpath) = tree_apply false [] (fun { childs } _ -> { v ; childs}, ()) tree path in
  (newtree, List.rev rpath)

let rec tree_fold acu f (path: 'a path) (tree: 'a tree) =
  let acu = f acu path tree in
  fst (List.fold_left (fun (a,i) ch -> (tree_fold a f (path @ [Nth i]) ch), i+1) (acu,1) tree.childs)
			       
type 'a tree_evt =
  [ `Value_Changed of 'a tree
  | `Node_deleted of ('a path) * ('a tree)
  | `Node_inserted of ('a path) * ('a tree)
  | `Node_value_changed of ('a path) * 'a ]

class ['a] tree_val ?(name="tree_val") ?target init =

object(self)
      inherit ['a tree, 'a tree_evt] Evt_val.evt_val ~name ?target ()

      val mutable tree = { v = init ; childs = [] }
				     
      method value = tree
			   
      method apply_event ev = match ev with
	| `Value_Changed t -> tree <- t ; self#vchanged
	| `Node_deleted (path, _) -> ignore(self#delete_node path)
	| `Node_inserted (p, s) -> self#insert_node p s
	| `Node_value_changed (p, v) -> self#set_node_val p v
							  
      method clear v = self#set_value { v ; childs = [] }
				      		  
      method insert_node path sub =
	let (t, path) = tree_insert tree path sub in
	tree <- t ;
	self#send (`Node_inserted (path, sub))

      method replace_all tree =
	self#clear tree.v ;
	self#insert_node [] tree
		  
      method set_node_val path v =
	if path = [] then Printf.printf "*Warning* Tree_evt_val: set_node_val has been invoked on the root node, which is invisible. This is useless.\n%!" ;
	let (t, path) = tree_set_val tree path v in
	tree <- t ;
	self#send (`Node_value_changed (path, v))

      method updated_node path =
	self#set_node_val path (get_val tree path) (* Need a canonical path *)
		  
      method delete_node path =
	let (t, path, subtree) = tree_delete tree path in
	tree <- t ; 
	self#send (`Node_deleted (path, subtree)) ;
	subtree

      method fold : 'b . 'b -> ('b -> 'a path -> 'a tree -> 'b) -> 'b =
	fun acu f -> tree_fold acu f [] tree
		      
      initializer self#vchanged (* Send initial signal *)
end
  
