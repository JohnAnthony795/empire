open Types
open TypesGen

(*let mutation (population:t_population) = ()*)
type uniteville = ARMY | FIGHT | TRANSPORT | PATROL | BATTLESHIP | CITY ;;

let () = Random.self_init()

let mutation_chance = 10 

let rec depth (arbre :t_arbre) = match arbre with
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
    (*let random_direction =
      match (Random.int 6) with
      | 0 -> Up
      | 1 -> Down
      | 2 -> Right
      | 3 -> Left
      | 4 -> Upleft
      | 5 -> Downright
      | _ -> failwith "muter_candidat : valeur non attendue"
    in*)
    let random_operator =
      match (Random.int 5) with
      | 0 -> Inf
      | 1 -> Sup
      | 2 -> Eq
      | 3 -> InfEq
      | 4 -> SupEq
      | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let random_city_pred =
    match (Random.int 2) with 
              | 0 -> Littoral_adjacent
              | 1 -> Fog_proche (Random.int 10)
              | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let random_unit_pred =
    match (Random.int 6) with
                | 0 -> Nb_unite_allie_proche (Random.int 10,random_unit,(Random.int 10),random_operator)
                | 1 -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),random_operator)
                | 2 -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,random_operator)
                | 3 -> Littoral_adjacent
                | 4 -> Transport
                | 5 -> Fog_proche (Random.int 10)
                | _ -> failwith "muter_candidat : valeur non attendue"
    in
    let random_city_action =
    match (Random.int 2) with 
                | 0 -> Leaf (Set_city_prod (1,random_unit))
                | 1 -> Leaf (Do_nothing (1))
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let random_battle_action =
    match (Random.int 2) with (*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let random_patrol_action =
    match (Random.int 2) with (*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let random_army_action =
    match (Random.int 3) with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | 2 -> Leaf (Envahir (1,1,1))
                (*| 4 -> Leaf (Transporter (1,1,1))*)
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let random_fight_action =
    match (Random.int 1) with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Explorer (1,1,1))
                (*| 2 -> Leaf (Do_nothing (1)) *)
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let random_transport_action =
    match (Random.int 1) with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Explorer (1,1,1))
                (*| 2 -> Leaf (Accoster (1,1,1))
                | 3 -> Leaf (Do_nothing (1))*)
                | _ -> failwith "muter_action : valeur non attendue"
    in
    let muter_action (action:t_action) (unite_type:uniteville) :t_arbre= 
    match unite_type with
    | CITY -> (match (Random.int 2) with 
              | 0 -> Node (random_city_action,random_city_pred,random_city_action)
              | 1 -> random_city_action
              | _ -> failwith "muter_candidat : valeur non attendue")
    | ARMY -> (match (Random.int 2) with
            | 0 -> Node (random_army_action,random_unit_pred,random_army_action)
            | 1 -> random_army_action
            | _ -> failwith "muter_candidat : valeur non attendue")
    | FIGHT -> (match (Random.int 2) with
            | 0 -> Node (random_fight_action,random_unit_pred,random_fight_action)
            | 1 -> random_fight_action
            | _ -> failwith "muter_candidat : valeur non attendue")
    | TRANSPORT -> (match (Random.int 2) with
            | 0 -> Node (random_transport_action,random_unit_pred,random_transport_action)
            | 1 -> random_transport_action
            | _ -> failwith "muter_candidat : valeur non attendue")
    | PATROL -> (match (Random.int 2) with
            | 0 -> Node (random_patrol_action,random_unit_pred,random_patrol_action)
            | 1 -> random_patrol_action
            | _ -> failwith "muter_candidat : valeur non attendue")
    | BATTLESHIP -> (match (Random.int 2) with
            | 0 -> Node (random_battle_action,random_unit_pred,random_battle_action)
            | 1 -> random_battle_action
            | _ -> failwith "muter_candidat : valeur non attendue")
    in
    let muter_predicat (predicat:t_predicat) (unite_type:uniteville) = 
    match unite_type with 
    | CITY -> random_city_pred
    | _ -> (match (Random.int 2) with
              | 0 -> random_unit_pred
              | 1 -> (match predicat with
                | Nb_unite_allie_proche (a,b,c,d) -> Nb_unite_allie_proche (Random.int 10,random_unit,(Random.int 10),d)
                | Nb_ville_allie_proche (a,b,c) -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),c)
                | Nb_ville_ennemie_proche (a,b,c) -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,c)
                | Littoral_adjacent -> Littoral_adjacent
                | Transport -> Transport
                | Fog_proche (a) -> Fog_proche (Random.int 10)
                | Unite_en_production -> Unite_en_production)
              | _ -> failwith "muter_candidat : valeur non attendue")
    in 
    match arbre with 
    | Node (a,b,c) 	-> 	if roll_Mutation then
        (match (a,b,c) with
        | (Leaf d,_,Leaf f) -> (match (Random.int 4) with
                                  | 0 -> (muter_action d unite_type)
                                  | 1 -> (muter_action f unite_type)
                                  | _ -> Node ((muter_arbre (chance-1) a unite_type,(muter_predicat b unite_type),(muter_arbre (chance-1) c unite_type))))
        | _ -> Node ((muter_arbre (chance-1) a unite_type,(muter_predicat b unite_type),(muter_arbre (chance-1) c unite_type)))) 
      else 
        Node ((muter_arbre (chance-1) a unite_type),b,(muter_arbre (chance-1) c unite_type))
    | Leaf action 	-> 	if roll_Mutation then
        (muter_action action unite_type)
      else
        Leaf action
  in
  match candidat with
  | (foret,score) -> match foret with
    | (a,b,c,d,e,f) -> (((muter_arbre mutation_chance a ARMY),(muter_arbre mutation_chance b FIGHT),(muter_arbre mutation_chance c TRANSPORT),(muter_arbre mutation_chance d PATROL),(muter_arbre mutation_chance e BATTLESHIP),(muter_arbre mutation_chance f CITY)),score)

let mute population  = 
  List.map (muter_candidat) population;;

let main () =
  Printf.printf "Test tree\n %!";

  Printf.printf "Nouvel arbre :\n %!";

  print_population (mute [(arbre0,arbre0,arbre0,arbre0,arbre0,arbre0), 10.0] )


