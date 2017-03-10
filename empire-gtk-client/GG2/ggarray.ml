open Ggval
open Array_evt_val
open Ggimage

let tabs ?enable_popup ?homogeneous_tabs ?scrollable ?show_border
	 ?tab_border ?tab_pos ?packing ?mk_label ?(delete_on_label=Clear) gbuild (v_ar: 'a array_val) =

  let nb = GPack.notebook ?enable_popup ?homogeneous_tabs ?scrollable ?show_border ?tab_border ?tab_pos ?packing () in

  (* Number of tabs. *)
  let length = ref 0 in
  
  let delete i =
    assert (0 <= i && i < !length) ; 
    nb#remove_page i ;
    decr length ;
  in

  let clear () = for i = !length - 1 downto 0 do delete i ; done in

  let insert i v =
    let widg = (gbuild v)#coerce in
    
    (* Label with a small close icon.
     * Difficulty: knowing the position of this tab (which may have changed because of insertions / deletions). *)
    let tab_label = Common.option_map mk_label
				      begin fun mk ->
					    let lab = (mk v)#coerce in
					    match delete_on_label with
					    | Clear -> lab
					    | _ ->
					       (* Close icon *)
					       let hbox = GPack.hbox ~border_width:0 () in
					       hbox#pack ~padding:8 lab ;
					       ignore(Ggbutton.iconbt ~packing:(hbox#add) ~cb:(fun () -> v#...) delete_on_label ()) ;
					       hbox#coerce
				      end

comment fermer proprement cte tab ???
				      
    in
    let res = (if i = !length then nb#append_page else nb#insert_page ~pos:i) ?tab_label widg in
    assert (res = i) ;
    incr length
  in

  let add v = insert !length v in
  
  (* Init with current array *)
  v_ar#fold () (fun () _ v -> add v) ;
  
  (* Listen to array_val *)
  Events.to_cons v_ar#prod
		 begin function
		   | `Cell _ -> ()
		   | `Value_Changed n_ar -> clear () ; Array.iter add n_ar
		   | `Insert (i, v) -> insert i v
		   | `Replace (i, v) -> delete i ; insert i v
		   | `Delete i -> delete i
		 end ;
  
  { v = v_ar ;
    w = nb }

															      
