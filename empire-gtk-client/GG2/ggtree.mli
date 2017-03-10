open Gg
open Ggval
open Events
open Tree_evt_val
open GTree

(*** Smoother API than GTree ***)

(* A graphical tree: tree of type 'a tree_val
 * Clicks on the tree emit events of category 'b. 
 * The category 'b can be an integer representing the column number, or just unit if the column is meaningless. *)
type ('a,'b) gtree

val get_vtree: ('a,'b) gtree -> 'a tree_val
val get_view: ('a,'b) gtree -> GTree.view
val update_all: ('a,'b) gtree -> unit
				 					  
(* Cell_renderer which displays values of type 'a. *)
type 'a cell

type click = Left_click | Right_click
	
(* User actions on a gtree. 
 * Row_activated contains the column category (of type 'b). *)
type ('a,'b) gtree_evt =
  | Rows_selected of ('a path * 'a) list (* Can be empty *)
  | Row_activated of ('a path * 'a * 'b)

  (* 'b is the category, it gives information about the view_column been clicked. *)
  | Click of click * 'a path * 'a * 'b

val gtree_evt2s: ('a,'b) gtree_evt -> string
val gpath_of_path: ('a,'b) gtree -> 'a path -> Gtk.tree_path
val path_of_gpath: ('a,'b) gtree -> Gtk.tree_path -> 'a path
				      
(* Builds a graphical tree.
 * 
 * handled: indicate if the click-event associated to this category is considered 'handled' 
 *          (if not, it continues propagation and usually selects the current row or fold/unfold a branch).
 *          Default behaviour is fun _ -> false (you should not 'handle' a click on the first column, since it blocks folding/unfolding. 
 *
 * rules_hint: add some background to every other row
 * visible: indicate which rows are visible (by default every row is visible)
 *)
val mygtree: ?with_scroll:bool -> ?name:string -> ?target:('a tree_evt) prod ->
	     ?headers_visible:bool -> ?rules_hint:bool ->
	     ?handled:(click -> 'b -> bool) -> ?gtree_evts:(('a,'b) gtree_evt prod) ->
	     ?visible_rows:('a -> bool) ->
	     packing:pack -> 'a tree -> ('a,'b) gtree

(* Adds a new column to the view. You must provide a function giving the category for this column.
 * (e.g. fun _ -> n) for the nth column, or just (fun _ -> `Default) if all columns should have the same behaviour.
 *
 * Returns the view_columns that has been added to this gtree (you don't have to add it yourself). 
 *
 * Be careful, the attributes returned by the fset function are 'sticky': they are carelessly applied
 * to lines displayed later in the same column (basically, that means all lines). 
 *
 * fixed_width => min_width & fixed_size
 * sizing: if `AUTOSIZE, a mechanism is inserted to avoid flickering (set sizing to FIXED while data is changing, then to AUTOSIZE when data settles).
 *
 * For each row, this column displays a list of (adjacent) horizontal cells. 
 *   - Note that the last cell is likely to expand to the end.
 *   - Cells seem to expand if needed, but never shrink (?!)
 *)
val new_column: ('a,'b) gtree -> ?title:utf ->
		?min_width:int -> ?sizing:Gtk.Tags.tree_view_column_sizing -> ?resizable:bool ->
		?fixed_width:int ->
		cat:('a -> 'b) -> 'a cell list -> view_column

(* Pack another cell into an existing view_column. 
 * Note that there is apparently no easy way to find which cell is concerned by a click inside a view column. *)
val add_to_column: ('a,'b) gtree -> view_column -> 'a cell -> unit
										      
(* Available cells
 * Look at the properties of renderers in the documentation of GTree *)

(* You will certainly need to use 'invisible'
 * and pack several cells in the same column (presumably more efficient than always resetting the current cell)
 * *)														    

val invisible: [> `VISIBLE of bool ] list

(* Type of cells displaying values of type 'a, and having graphical gtk_properties of type 'b. *)
type ('a, 'b) mkrender = ?expand:bool -> init:'b list -> set:('a -> 'b list) -> unit -> 'a cell
  
val rn_pixbuf:   ('a, cell_properties_pixbuf)   mkrender
val rn_text:     ('a, cell_properties_text)     mkrender
val rn_toggle:   ('a, cell_properties_toggle)   mkrender
val rn_progress: ('a, cell_properties_progress) mkrender
val rn_combo:    ('a, cell_properties_combo)    mkrender
val rn_accel:    ('a, cell_properties_accel)    mkrender

(* Give image path and a predicate telling if the icon is visible. *)						
val rn_icon: ?opts:('a -> cell_properties_pixbuf list) -> string -> ('a -> bool) -> 'a cell

(* The argument function gives the width. *)
val rn_space: ?expand:bool -> ('a -> int) -> 'a cell
						
(* Convenience: text cell which sets only the `TEXT property.
 * Use s_invisible to hide the cell. *)
val rn_onlytext: ?expand:bool -> init: cell_properties_text list -> set:('a -> string) -> unit -> 'a cell

val s_invisible: string												     
						
(*** Notes for developpers ***)

(*						  
   - The tree model is a 'tree_store'.

   - A 'iter' is just a row (= a node) in the tree_store.

   - Avoid confusion between 'column' and 'view_column'.
        column = record field in each tree row.
        view_column = a display column in the tree view

     In Ggtree, we have only one column with an 'a value.
     There can be several view_columns.

   - A view_column works as follows:
         * It contains a widget (aka 'cell renderer')
         * Some attributes of this widget (e.g. "text", "color", ...) can be connected to 'columns' of the tree_store for automatic display.
           => We don't use this, since we have only one column.
         * Finally, a custom display function can be connected to the widget: #set_cell_data_func, which receives the current row.
 *)
