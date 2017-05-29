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

let random_unit nombre: unites = 
      match nombre with
      | 0 -> ARMY
      | 1 -> TRANSPORT
      | 2 -> FIGHT
      | 3 -> BATTLESHIP
      | 4 -> PATROL
      | _ -> failwith "muter_candidat : valeur non attendue"
    
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
let random_operator nombre=
      match nombre with
      | 0 -> Inf
      | 1 -> Sup
      | 2 -> Eq
      | 3 -> InfEq
      | 4 -> SupEq
      | _ -> failwith "muter_candidat : valeur non attendue"
    
let random_city_pred nombre=
    match nombre with 
              | 0 -> Littoral_adjacent
              | 1 -> Fog_proche (Random.int 10)
              (*| 2 -> Unite_en_production*)
              | 2 -> Nb_unite_allie_proche (Random.int 10,(random_unit (Random.int 5)),(Random.int 10),(random_operator (Random.int 5)))
              | 3 -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),(random_operator (Random.int 5)))
              | 4 -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,(random_operator (Random.int 5)))
              | _ -> failwith "muter_candidat : valeur non attendue"
    
let random_unit_pred nombre =
    match nombre with
                | 0 -> Nb_unite_allie_proche (Random.int 10,(random_unit (Random.int 5)),(Random.int 10),(random_operator (Random.int 5)))
                | 1 -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),(random_operator (Random.int 5)))
                | 2 -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,(random_operator (Random.int 5)))
                | 3 -> Littoral_adjacent
                | 4 -> Transport
                | 5 -> Fog_proche (Random.int 10)
                | _ -> failwith "muter_candidat : valeur non attendue"
    
let random_city_action nombre=
    match nombre with 
                | 0 -> Leaf (Set_city_prod (1,random_unit (Random.int 5)))
                (*| 1 -> Leaf (Do_nothing (1))*)
                | _ -> failwith "muter_action : valeur non attendue"
    
let random_battle_action nombre=
    match nombre with (*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | _ -> failwith "muter_action : valeur non attendue"
    
let random_patrol_action nombre=
    match nombre with (*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | _ -> failwith "muter_action : valeur non attendue"
    
let random_army_action nombre=
    match nombre with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Attaquer (1,1,1))
                | 1 -> Leaf (Explorer (1,1,1))
                | 2 -> Leaf (Envahir (1,1,1))
                (*| 4 -> Leaf (Transporter (1,1,1))*)
                | _ -> failwith "muter_action : valeur non attendue"
    
let random_fight_action nombre=
    match nombre with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Explorer (1,1,1))
                | 1 -> Leaf (Do_nothing (1)) 
                | _ -> failwith "muter_action : valeur non attendue"
    
let random_transport_action nombre=
    match nombre with(*
                | 0 -> Leaf (Move (1,random_direction))*)
                | 0 -> Leaf (Explorer (1,1,1))
                (*| 2 -> Leaf (Accoster (1,1,1))
                | 3 -> Leaf (Do_nothing (1))*)
                | _ -> failwith "muter_action : valeur non attendue"
    
let muter_action (action:t_action) (unite_type:uniteville) nombre :t_arbre= 
    match unite_type with
    | CITY -> (match nombre with 
              | 0 -> Node ((random_city_action (Random.int 1)),(random_city_pred (Random.int 5)),(random_city_action (Random.int 1)))
              | 1 -> (random_city_action (Random.int 1))
              | _ -> failwith "muter_candidat : valeur non attendue")
    | ARMY -> (match nombre with
            | 0 -> Node ((random_army_action (Random.int 3)),(random_unit_pred (Random.int 6)),(random_army_action (Random.int 3)))
            | 1 -> (random_army_action (Random.int 3))
            | _ -> failwith "muter_candidat : valeur non attendue")
    | FIGHT -> (match nombre with
            | 0 -> Node ((random_fight_action (Random.int 2)),(random_unit_pred (Random.int 6)),(random_fight_action (Random.int 2)))
            | 1 -> (random_fight_action (Random.int 2))
            | _ -> failwith "muter_candidat : valeur non attendue")
    | TRANSPORT -> (match nombre with
            | 0 -> Node ((random_transport_action (Random.int 1)),(random_unit_pred (Random.int 6)),(random_transport_action (Random.int 1)))
            | 1 -> (random_transport_action (Random.int 1))
            | _ -> failwith "muter_candidat : valeur non attendue")
    | PATROL -> (match nombre with
            | 0 -> Node ((random_patrol_action (Random.int 2)),(random_unit_pred (Random.int 6)),(random_patrol_action (Random.int 2)))
            | 1 -> (random_patrol_action (Random.int 2))
            | _ -> failwith "muter_candidat : valeur non attendue")
    | BATTLESHIP -> (match nombre with
            | 0 -> Node ((random_battle_action (Random.int 2)),(random_unit_pred (Random.int 6)),(random_battle_action (Random.int 2)))
            | 1 -> (random_battle_action (Random.int 2))
            | _ -> failwith "muter_candidat : valeur non attendue")
    
let muter_predicat (predicat:t_predicat) (unite_type:uniteville) = 
    match unite_type with 
    | CITY -> (random_city_pred (Random.int 5))
    | _ -> (match (Random.int 2) with
              | 0 -> (random_unit_pred (Random.int 6))
              | 1 -> (match predicat with
                | Nb_unite_allie_proche (a,b,c,d) -> Nb_unite_allie_proche (Random.int 10,(random_unit (Random.int 5)),(Random.int 10),d)
                | Nb_ville_allie_proche (a,b,c) -> Nb_ville_allie_proche (Random.int 10,(Random.int 10),c)
                | Nb_ville_ennemie_proche (a,b,c) -> Nb_ville_ennemie_proche (Random.int 10,Random.int 10,c)
                | Littoral_adjacent -> Littoral_adjacent
                | Transport -> Transport
                | Fog_proche (a) -> Fog_proche (Random.int 10)
								| Unknown_proche (a) -> Unknown_proche (Random.int 10)
                | Unite_en_production -> Unite_en_production)
              | _ -> failwith "muter_candidat : valeur non attendue")
    

let rec muter_arbre chance arbre (unite_type:uniteville)= 
		let chance = if chance > 0 then chance else 1 in
    match arbre with 
    | Node (a,b,c)  ->  
    if ((Random.int chance) = (chance-1)) then
        match (Random.int 4) with
            | 0 -> (muter_arbre (chance-1) a unite_type)
            | 1 -> (muter_arbre (chance-1) c unite_type)
            | 2 -> Node ((muter_arbre (chance-1) a unite_type,(muter_predicat b unite_type),(muter_arbre (chance-1) c unite_type)))
            | 3 -> Node ((muter_arbre (chance-1) a unite_type,b,(muter_arbre (chance-1) c unite_type)))
            | _ -> failwith "muter_arbre"
        
      else 
        Node ((muter_arbre (chance-1) a unite_type),b,(muter_arbre (chance-1) c unite_type))
    | Leaf action   ->  if ((Random.int chance) = (chance-1)) then
        (muter_action action unite_type (Random.int 2))
      else
        Leaf action

let muter_candidat candidat =
  match candidat with
  | (foret,score) -> match foret with
    | (a,b,c,d,e,f) -> (((muter_arbre mutation_chance a ARMY),(muter_arbre mutation_chance b FIGHT),(muter_arbre mutation_chance c TRANSPORT),(muter_arbre mutation_chance d PATROL),(muter_arbre mutation_chance e BATTLESHIP),(muter_arbre mutation_chance f CITY)),score)
  
let mute population  = 
  List.map (muter_candidat) population;;

let main () =
  Printf.printf "Test tree\n %!";

  Printf.printf "Nouvel arbre :\n %!";

  print_population (mute [(arbre0,arbre0,arbre0,arbre0,arbre0,arbre0), 10.0] )


