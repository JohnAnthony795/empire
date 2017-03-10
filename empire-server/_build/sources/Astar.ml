module PrioQueue = struct
	type priority = int

	type 'a queue =
		| Empty
		| Node of priority * 'a * 'a queue * 'a queue

	exception Queue_is_empty

	let rec insert queue prio elt =
		match queue with
			| Empty -> Node (prio, elt, Empty, Empty)
			| Node (p, e, left, right) ->
				if prio <= p
				then Node (prio, elt, insert right p e, left)
				else Node (p, e, insert right prio elt, left)

	let rec remove_top = function
		| Empty -> raise Queue_is_empty
		| Node (_, _, left, Empty) -> left
		| Node (_, _, Empty, right) -> right
		| Node (_, _, (Node (lprio, lelt, _, _) as left), (Node(rprio, relt, _, _) as right)) ->
			if lprio <= rprio
			then Node (lprio, lelt, remove_top left, right)
			else Node (rprio, relt, left, remove_top right)

	let extract = function
		| Empty -> raise Queue_is_empty
		| Node (prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

	let is_empty = function
		| Empty -> true
		| Node _ -> false
end

let astar_goal start goal cost_max neighbors cost heuristic =
	let frontier = ref PrioQueue.Empty in
	frontier := PrioQueue.insert !frontier 0 start ;
	let came_from = Hashtbl.create 16 in
	Hashtbl.add came_from start None ;
	let cost_so_far = Hashtbl.create 16 in
	Hashtbl.add cost_so_far start 0 ;
	let found = ref false in

	while not !found && not (PrioQueue.is_empty !frontier) do begin
		let _, current, new_frontier = PrioQueue.extract !frontier in
		frontier := new_frontier ;

		if current = goal then begin
			found := true ;
		end else begin
			let process_next next =
				let new_cost = (Hashtbl.find cost_so_far current) + (cost current next) in
(*XXX let xx, yy = next in
Printf.printf "  >>>> process_next a*: %d %d new_cost=%d cast_max=%d in(cost_so_far)=%b\n" xx yy new_cost cost_max (Hashtbl.mem cost_so_far next) ;
*)
				if new_cost <= cost_max &&
				    (not (Hashtbl.mem cost_so_far next) || new_cost < (Hashtbl.find cost_so_far next))
				then begin
					let priority = new_cost + heuristic goal next in
					Hashtbl.add cost_so_far next new_cost ;
					frontier := PrioQueue.insert !frontier priority next ;
					Hashtbl.add came_from next (Some current)
				end in
			List.iter process_next (neighbors current)
		end
	end done ;

	let rec create_path l = function
		| None -> l
		| Some x when x = start -> l
		| Some x -> create_path (x :: l) (Hashtbl.find came_from x) in
	if not !found then None else Some (create_path [] (Some goal))
