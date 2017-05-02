open ToolsArbres
open Types;;

(*let mutation (population:t_population) = ()*)

Random.self_init();;

let rec depth (arbre:t_arbre) = match arbre with
  | Leaf _ -> 0
  | Node (a,_,b) -> 1 + max (depth a) (depth b);;

let random_depth arbre = Random.int (depth arbre)

let rec muter_arbre (arbre:t_arbre) (chance:int) = 
	let roll_Mutation = ((Random.int chance) > (chance-2)) in
	let random_unit = 
	match (Random.int 5) with
				| 0 -> ARMY
				| 1 -> TRANSPORT
				| 2 -> FIGHT
				| 3 -> BATTLESHIP
				| _ -> PATROL
	in
	let random_direction =
	match (Random.int 6) with
				| 0 -> Up
				| 1 -> Down
				| 2 -> Right
				| 3 -> Left
				| 4 -> Upleft
				| _ -> Downright

	in
	let random_operator =
	match (Random.int 5) with
				| 0 -> Inf
				| 1 -> Sup
				| 2 -> Eq
				| 3 -> InfEq
				| _ -> SupEq
	in
	let muter_action (action:t_action) = match (Random.int 3) with
		| 0 -> Move (1,random_direction)
		| 1 -> Set_city_prod (1,random_unit)
		| _ -> End_turn
	in
	let muter_predicat (predicat:t_predicat) = 
	match (Random.int 2) with
	| 0 -> Nb_unite_allie_proche (random_unit,(Random.int 10),random_operator)
	| _ -> Nb_ville_allie_proche ((Random.int 10),random_operator)
	in 
	match arbre with 
	| Node (a,b,c) 	-> 	if roll_Mutation then
				 		Node ((muter_arbre a (chance-1),(muter_predicat b),(muter_arbre c (chance-1))))
					else 
						Node ((muter_arbre a (chance-1)),b,(muter_arbre c (chance-1)))
	| Leaf action 	-> 	if roll_Mutation then
					Leaf (muter_action action)
					else
					Leaf action;;


let main () =
  Printf.printf "Test tree\n %!";
 
  print_tree arbre0 0;
  Printf.printf "Nouvel arbre :\n %!";
  print_tree (muter_arbre arbre0 10) 0
 
in main ();;

