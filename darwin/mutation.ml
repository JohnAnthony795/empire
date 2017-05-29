open Types;;
open TypesGen;;

(*let mutation (population:t_population) = ()*)

Random.self_init();;

let rec depth (arbre:t_arbre) = match arbre with
  | Leaf _ -> 0
  | Node (a,_,b) -> 1 + max (depth a) (depth b);;

let random_depth arbre = Random.int (depth arbre)

let muter_candidat candidat =
  let rec muter_arbre chance arbre = 
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
      | 0 -> Nb_unite_allie_proche ((Random.int 10),random_unit,(Random.int 10),random_operator)
      | 1 -> Nb_ville_allie_proche ((Random.int 10),(Random.int 10),random_operator)
      | 2 -> Nb_ville_ennemie_proche ((Random.int 10),(Random.int 10),random_operator)
    in 
    match arbre with 
    | Node (a,b,c) 	-> 	if roll_Mutation then
        Node ((muter_arbre (chance-1) a,(muter_predicat b),(muter_arbre (chance-1) c)))
      else 
        Node ((muter_arbre (chance-1) a),b,(muter_arbre (chance-1) c))
    | Leaf action 	-> 	if roll_Mutation then
        Leaf (muter_action action)
      else
        Leaf action
  in
  match candidat with
  | (foret,score) -> match foret with
    | (a,b,c,d,e,f) -> (((muter_arbre 10 a),(muter_arbre 10 b),(muter_arbre 10 c),(muter_arbre 10 d),(muter_arbre 10 e),(muter_arbre 10 f)),score)

let mute population = 
  List.map (muter_candidat) population;;

(*let main () =
  	Printf.printf "Test tree\n %!";

  	Printf.printf "Nouvel arbre :\n %!";

  	print_population (mute [((arbre0,arbre0,arbre0,arbre0,arbre0,arbre0),10)]) *)

(*in main ();;*)

