open Common.Small
open Ggval
open Events
open Tree_evt_val
open GTree

module GPath = GTree.Path

let stable_period = 0.4
		 
type ('a,'b) gtree =
  { (* The tree (non-graphical) value *)
    vtree: 'a tree_val ;

    (* The tree widget *)
    wtree: GTree.view ;

    (* The gtk 'model' column (non graphical element). Contains the values of type 'a. *)
    col: 'a column  ;

    (* Associates a category function to each column oid .*)
    cat: (int, 'a -> 'b) Hashtbl.t ;

    mutable view_columns: view_column list ;

    store: GTree.tree_store ;
    
    (* Maps the view path to the model path (different if using a model_filter, when some rows are not visible). *)
    real_path: Gtk.tree_path -> Gtk.tree_path  ;
    visi_path: Gtk.tree_path -> Gtk.tree_path
  }

let get_vtree gt = gt.vtree
let get_view gt = gt.wtree

let update_all gt = gt.store#foreach (fun path iter -> gt.store#row_changed path iter ; false)

type click = Left_click | Right_click
		    
type ('a,'b) gtree_evt =
  | Rows_selected of ('a path * 'a) list (* Can be empty *)
  | Row_activated of ('a path * 'a * 'b)
  | Click of click * 'a path * 'a * 'b

let gtree_evt2s = function
  | Rows_selected l -> Printf.sprintf "Rows_selected [%s]" (Common.sep (fun (p, _) -> path2s p) " | " l)
  | Row_activated (p, _ , _) -> Printf.sprintf "Row_activated (%s)" (path2s p)
  | Click (cl, p, _, _) -> Printf.sprintf "%s at %s" (match cl with Left_click -> "Left-click" | Right_click -> "Right-click") (path2s p)
				      
(*** Renderers ***)

type ('a,'b,'g) rendr =
  { gtk_renderer: ('g, [> `VISIBLE of bool ] as 'b) cell_renderer_skel ;
    set: ('a -> 'b list) ;
    expand: bool }
					  
(* Existential type to capture all renderers. 
 * Cell (render, setter) *)
type 'a cell = Cell: ('a,'b,'g) rendr -> 'a cell
			  
type ('a, 'b) mkrender = ?expand:bool -> init:'b list -> set:('a -> 'b list) -> unit -> 'a cell

let invisible = [ `VISIBLE false ]
let visible   = [ `VISIBLE true ]
									 
let mkr f = fun ?(expand=false) ~init ~set () -> Cell { gtk_renderer = (f init :> (_,_) cell_renderer_skel) ;
							set ;
							expand }

let rn_pixbuf   ?expand ~init ~set () = mkr cell_renderer_pixbuf ?expand ~init ~set ()
let rn_text     ?expand ~init ~set () = mkr cell_renderer_text ?expand ~init ~set ()
let rn_toggle   ?expand ~init ~set () = mkr cell_renderer_toggle ?expand ~init ~set ()
let rn_progress ?expand ~init ~set () = mkr cell_renderer_progress ?expand ~init ~set ()
let rn_combo    ?expand ~init ~set () = mkr cell_renderer_combo ?expand ~init ~set ()
let rn_accel    ?expand ~init ~set () = mkr cell_renderer_accel ?expand ~init ~set ()

let s_invisible = "*(invisible text)*"
				 
let rn_onlytext ?expand ~init ~set () =
  mkr cell_renderer_text ?expand ~init ~set:(fun v -> let x = set v in
						      if x == s_invisible then invisible else [ `TEXT x ] ) ()     
let path_of_gpath gtree gpath =
  let ar = GPath.get_indices (gtree.real_path gpath) in
  Array.to_list (Array.map (fun i -> Nth (i+1)) ar)

let gpath_of_path gtree path =
  let ilist = List.map (function Nth i -> (i-1) | Pred _ -> assert false) (norm_path gtree.vtree#value path) in
  gtree.visi_path (GPath.create ilist)
				 
(*** Build tree ***)
		   
let mygtree ?(with_scroll=true) ?name ?target ?headers_visible ?rules_hint ?(handled=fun _ _ -> false) ?gtree_evts ?visible_rows ~packing init_tree =

  (* My tree *)
  let vtree = new tree_val ?name ?target init_tree.v in

  (*** GTree model ***)

  (* Columns are mandatory. They correspond to fields in a record type. 
   * We just don't care about this. We add a single generic column with values of type 'a. *)
  let col_list = new GTree.column_list in
  let main_col = col_list#add Gobject.Data.caml
  and tree_store = GTree.tree_store col_list in

  (* We have to lock reads because Gtk tries to read the tree structure while it is been built. *)
  let model_readable = ref false in
  
  let (model, real_path, visi_path) = match visible_rows with
    | None -> ((tree_store:>model), id, id)
    | Some f ->
       let mf = model_filter tree_store in
       mf#set_visible_func (fun m it -> if !model_readable then f (m#get ~row:it ~column:main_col) else true) ;

       ((mf:>model), mf#convert_path_to_child_path, mf#convert_child_path_to_path)
  in

  (* Precision : an 'iter' (GTree) is equivalent to a tree node. It is apparently not mutable. *)

  (* valid: assert-checks that the path is valid *)
  let iter_of_path ?(valid=true) path =
    let ilist = List.map (function Nth i -> (i-1) | Pred _ -> assert false ) path in  (* assert false : not supposed to received Pred _ paths in events *)    
    let gpath = GPath.create ilist in
    let iter = tree_store#get_iter gpath in
    assert ((not valid) || tree_store#iter_is_valid iter) ;
    iter
  in

  (* If parent is given, it must be valid. If not given, we build the childs of the root node.
   * build_node sets the node value and creates, recursively, the childs. *)
  let rec build_node ?parent tree =
    Common.option_iter parent (fun row -> tree_store#set ~row ~column:main_col tree.v) ;
    Common.myiter tree.childs (fun child -> build_node ~parent:(tree_store#append ?parent ()) child)
  in
  
  let insert_subtree path tree =
    model_readable := false ;
    
    let () =
      if path = [] then build_node tree
      else
	let iter = iter_of_path ~valid:false path in
	if tree_store#iter_is_valid iter then
	  (* OK, the node exists *)
	  build_node ~parent:(tree_store#insert_after iter) tree
	else
	  (* This node does not exist. We just have to append a child to its parent. *)
	  match tree_store#iter_parent iter with
	  | None -> assert false (* Must have a parent. *)
	  | Some parent -> build_node ~parent:(tree_store#append ~parent ()) tree
    in
    
    model_readable := true ;
  in

  (* Take into account tree events. *)
  to_cons vtree#prod
	  begin function
	    | `Value_Changed tree -> tree_store#clear () ; insert_subtree [] tree
	    | `Node_deleted (path, _) -> ignore (tree_store#remove (iter_of_path path))
	    | `Node_inserted (path, tree) -> insert_subtree path tree
	    | `Node_value_changed (path, v) ->
	       let row = iter_of_path path in
	       tree_store#set ~row ~column:main_col v
	  end ;
  
  (* Init *)
  vtree#replace_all init_tree ;

  (*** GTree view ***)
  let packview = if with_scroll then
		   (* Do not use add_with_viewport for a tree_view. 
		    * It works only by accident for positive-bounded canvas. *)
		   let scrolled = GBin.scrolled_window ~packing () in
		   scrolled#add
		     
		 else packing
  in
			
  let view = GTree.view ~model ?headers_visible ?rules_hint ~packing:packview () in

  view#collapse_all () ;
  view#selection#set_mode `SINGLE ;
  
  let cat = Hashtbl.create 8 in

  let gtree = { vtree ;
		wtree = view ;
		col = main_col ;
		cat ;
		store = tree_store ;
		view_columns = [] ;
		real_path ;
		visi_path }
  in

  (* Avoid flickering of autosized columns *)
  let stable = Events_timed.(stabilized (cst_cat stable_period) vtree#prod) in

  (* None = idle
   * Some l => list of AUTOSIZED columns that have been temporarily set to GROW. *)
  let autosized_columns = ref None in

  (* First event => record autosized columns. *)
  to_cons vtree#prod
	  begin fun _ ->
		match !autosized_columns with
		| Some _ -> ()
		| None ->
		   let l = Common.myfold gtree.view_columns [] (fun l vc -> if vc#sizing = `AUTOSIZE then vc :: l else l) in
		   List.iter (fun vc -> vc#set_sizing `GROW_ONLY) l ;
		   autosized_columns := Some l
	  end ;

  (* Stabilized => back to autosize *)
  to_cons stable
	  begin fun _ ->
		match !autosized_columns with
		| None -> assert false (* How can we receive a stabilized event without having received a first event ? *)
		| Some l ->
		   autosized_columns := None ;
		   List.iter (fun vc -> vc#set_sizing `AUTOSIZE) l ;
	  end ;
  
  (*** User interaction: row-activated / mouse clicks. ***)
  let pair_of_gpath gpath =
    let path = path_of_gpath gtree gpath in
    (path, get_val vtree#value path)
  in

  Common.option_iter gtree_evts
		     begin fun prod ->
			   ignore(view#selection#connect#changed
				    (fun () -> Events.send prod (Rows_selected (List.map pair_of_gpath view#selection#get_selected_rows)))) ;
			   ignore(view#connect#row_activated
				    begin fun gpath gcol ->
					  let (path, v) = pair_of_gpath gpath in
					  let ct = try (Hashtbl.find cat gcol#get_oid) v with Not_found -> assert false in
					  Events.send prod (Row_activated (path, v, ct))
				    end) ;

			   (* Mouse clicks *)
			   ignore(view#event#connect#button_press
				    begin fun ev ->
					  let oclick = match GdkEvent.Button.button ev with
					    | 1 -> Some Left_click
					    | 3 -> Some Right_click
					    | _ -> None
					  in
					  match oclick with
					  | None -> false (* we did not handle this *)
					  | Some click ->
    					     let x = iof (GdkEvent.Button.x ev) in
    					     let y = iof (GdkEvent.Button.y ev) in
					     begin match view#get_path_at_pos ~x ~y with
						   | None -> false
						   | Some (gpath, gcol, dx, dy) ->
						      let (path, v) = pair_of_gpath gpath in
						      let ct = try (Hashtbl.find cat gcol#get_oid) v with Not_found -> assert false in
						      Events.send prod (Click (click, path, v, ct)) ;
						      handled click ct
					     end
				    end) ;
		     end ;

  gtree

let add_to_column gtree (viewc:view_column) cell =
  (* Open existential *)
  let Cell rendr = cell in
  let grend = rendr.gtk_renderer in
  viewc#pack ~expand:rendr.expand grend ;

  viewc#set_cell_data_func grend
			   begin fun model iter ->
				 let props = rendr.set (model#get ~row:iter ~column:gtree.col) in
				 (* We explictly set visible, because the user usually omits it from its properties. *)
				 if props == invisible then () else grend#set_properties visible ; 
				 grend#set_properties props ;
			   end ;
  ()

    
let new_column gtree ?title ?min_width ?sizing ?resizable ?fixed_width ~cat cells =
  
  let viewc = GTree.view_column ?title:(Common.option_map title Gg.to_utf) () in

  Common.option_iter resizable viewc#set_resizable ;
  Common.option_iter min_width viewc#set_min_width ;
  Common.option_iter sizing viewc#set_sizing ;
  Common.option_iter fixed_width (fun w -> viewc#set_min_width w ; viewc#set_sizing `FIXED) ;

  Hashtbl.add gtree.cat viewc#get_oid cat ;
  
  List.iter (add_to_column gtree viewc) cells ;
  ignore(gtree.wtree#append_column viewc) ;
  gtree.view_columns <- viewc :: gtree.view_columns ;
  
  viewc

let rn_icon ?opts img p =
  rn_pixbuf ~expand:false ~init:[ `PIXBUF (Ggimage.pixbuf_from_file img) ]
	    ~set:(fun v -> if p v then Common.option_apply [] opts (fun oo -> oo v) else invisible)
	    ()

let rn_space ?expand fwidth = rn_text ?expand ~init:[`TEXT ""] ~set:(fun v -> let w = fwidth v in if w = 0 then invisible else [`WIDTH w]) ()
				      
