open Types
open TypesGen

(*let mutation (population:t_population) = ()*)
type uniteville = ARMY | FIGHT | TRANSPORT | PATROL | BATTLESHIP | CITY ;;

let () = Random.self_init()

let rec depth (arbre:t_arbre) = match arbre with
  | Leaf _ -> 0
  | Node (a,_,b) -> 1 + max (depth a) (depth b);;

let random_depth arbre = Random.int (depth arbre)

let muter_candidat candidat =
  let rec muter_arbre chance arbre (unite_type:uniteville)= 
    let roll_Mutation = ((Random.int chance) > (chance-2)) in
    let random_unit : unites = 
      match (Random.int 5) with
      | 0 -> ARMY
      | 1 -> TRANSPORT
      | 2 -> FIGHT
      | 3 -> BATTLESHIP
      | 4 -> PATROL
      | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let random_direction =
      match (Random.int 6) with
      | 0 -> Up
      | 1 -> Down
      | 2 -> Right
      | 3 -> Left
      | 4 -> Upleft
      | 5 -> Downright
      | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let random_operator =
      match (Random.int 5) with
      | 0 -> Inf
      | 1 -> Sup
      | 2 -> Eq
      | 3 -> InfEq
      | 4 -> SupEq
      | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let muter_action (action:t_action) (unite_type:uniteville) = 
    match unite_type with
    | CITY -> Set_city_prod (1,random_unit)
    | _ -> Move (1,random_direction)

    in
    let muter_predicat (predicat:t_predicat) (unite_type:uniteville) = 
    match unite_type with 
    | CITY -> (match (Random.int 2) with 
              | 0 -> Littoral_adjacent
              | 1 -> Fog_proche (Random.int 10)
              | _ -> failwith "muter_candidat : valeur non attendue")
    | _ -> (match (Random.int 2) with
              | 0 -> (match (Random.int 6) with
                | 0 -> Nb_unite_allie_proche (Random.int 10,random_unit,(Random.int 10),random_operator)
                | 1 -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),random_operator)
                | 2 -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,random_operator)
                | 3 -> Littoral_adjacent
                | 4 -> Transport
                | 5 -> Fog_proche (Random.int 10)
                | _ -> failwith "muter_candidat : valeur non attendue")
              | 1 -> (match predicat with
                | Nb_unite_allie_proche (a,b,c,d) -> Nb_unite_allie_proche (Random.int 10,random_unit,(Random.int 10),d)
                | Nb_ville_allie_proche (a,b,c) -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),c)
                | Nb_ville_ennemie_proche (a,b,c) -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,c)
                | Littoral_adjacent -> Littoral_adjacent
                | Transport -> Transport
                | Fog_proche (a) -> Fog_proche (Random.int 10))
              | _ -> failwith "muter_candidat : valeur non attendue")
    in 
    match arbre with 
    | Node (a,b,c) 	-> 	if roll_Mutation then
        Node ((muter_arbre (chance-1) a unite_type,(muter_predicat b unite_type),(muter_arbre (chance-1) c unite_type)))
      else 
        Node ((muter_arbre (chance-1) a unite_type),b,(muter_arbre (chance-1) c unite_type))
    | Leaf action 	-> 	if roll_Mutation then
        Leaf (muter_action action unite_type)
      else
        Leaf action
  in
  match candidat with
  | (foret,score) -> match foret with
    | (a,b,c,d,e,f) -> (((muter_arbre 10 a ARMY),(muter_arbre 10 b FIGHT),(muter_arbre 10 c TRANSPORT),(muter_arbre 10 d PATROL),(muter_arbre 10 e BATTLESHIP),(muter_arbre 10 f CITY)),score)

let mute population  = 
  List.map (muter_candidat) population;;

let main () =
  Printf.printf "Test tree\n %!";

  Printf.printf "Nouvel arbre :\n %!";

  print_population (mute [(arbre0,arbre0,arbre0,arbre0,arbre0,arbre0), 10.0] )


