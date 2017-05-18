(*
  sélection
On à potentiellement plusieurs sélections:
-Apres l'évaluation des individus, une séléection (visant à éliminer les faibles?)
-juste apreès, une séléction des individus pour reproduction
-Apres croisement et mutations, sélection pour former la prochaine génération, a partir de la derniere génération et des enfants.

On fusionne ici les deux premières sélections.

Implementer plusieurs fonctions de sélection pour tests?
Mettre taille de la pop en varaible globales.
Trier la pop pour extraction facile?
*)


(*open Types*)
open TypesGen
open Random (*ajouter*)

(**Fonctions utilitaires:**)
let () =
  Random.self_init ()


(*best beetween 2 individuals, according to score*)
let best ind1 ind2 =
  match (ind1,ind2) with
    ((f1,s1),(f2,s2)) -> if s1 >= s2 then ind1 else ind2

(*best individual within a list*)
let bestOfList lInd = 
  let lIndHd = match lInd with
    | hd :: _ -> print_endline "HD bestoflist"; hd
    | [] -> failwith "selection.bestOfList: liste vide"
  in
  List.fold_left best lIndHd lInd

(*Somme des scores de tout les individus ( bonne indication de fitness globale btw)*)
let scores_sum pool = (*.+?*)
  let sum_ind total ind  =
    match (ind) with
      ((f1,s1)) -> s1 +. total
  in
  List.fold_left sum_ind 0.0 pool


(**sélections pour mating pool**)
(* Sélection basique : les n plus forts *)(*le tri serait peut etre bien....v2?*)
let select_n_best pool n =
  let rec aux_select_n_best acu pool n =
    if n = 0 then acu
    else
      let b = (bestOfList pool) in
      aux_select_n_best (b::acu) (List.filter (fun i -> i <> b) pool) (n-1)  (*définir l'égalité d'individus? pointeur devrait être ok mais TODO tester!*)
  in
  aux_select_n_best [] pool n

(*sélection proportionelle au score /!\ très sensible aux scores nuls.*)
let select_n_proportional pool n = (*version avec possibilité de choisir plusieurs fois le meme.*)
  let total = scores_sum pool in
  let rec aux_select_n_proportional acu pool n =
    let win_nb = (Random.float total) in (*entre 0 et total*)
    let rec find_winner pool number =
      match pool with
      |(f,s)::inds -> if s >= number then (f,s)
        else find_winner inds (number-.s)
      |_ -> failwith "ERREUR: select n proportional pas au point on dirait"
    in
    if n = 0 then acu
    else
      let b = find_winner pool win_nb in
      aux_select_n_proportional (b::acu) pool (n-1)
  in
  aux_select_n_proportional [] pool n

(*sélection proportionelle au score /!\ très sensible aux scores nuls.*)
let select_n_proportional_bis pool n = (*version avec IMpossibilité de choisir plusieurs fois le meme.*)
  let total = scores_sum pool in
  let rec aux_select_n_proportional acu pool n =
    let win_nb = (Random.float total) in 
    let rec find_winner pool number =
      match pool with
      |(f,s)::inds -> if s >= number then (f,s)
        else find_winner inds (number-.s)
      |_ -> failwith "ERREUR: select n proportional pas au point on dirait"
    in
    if n = 0 then acu
    else
      let b = find_winner pool win_nb in
      aux_select_n_proportional (b::acu) (List.filter (fun i -> i <> b) pool) (n-1) (*définir l'égalité d'individus? pointeur devrait être ok mais TODO tester!*)
  in
  aux_select_n_proportional [] pool n

let select_n_parents pool n methode =  (*fonction générique de sélection, donner un int en entrée pour choisir la sélecion. Argument optionnel?*)
  match methode with
  | 1 -> select_n_best pool n
  | 2 -> select_n_proportional_bis pool n
  | 3 -> select_n_proportional pool n
  | x -> failwith ("ERREUR: il n'y a pas de méthode"^ string_of_int x)



(**merge des parents/enfants**)

(*All children and best parents to fill*)
let merge_best_and_child parents children =
  (List.append (select_n_best parents (TypesGen.taille_population - (List.length children))) children)

let merge_generations parents children methode =  (*fonction générique de merge, donner un int en entrée pour choisir la sélecion. Argument optionnel?*)
  match methode with
  | 1 -> merge_best_and_child parents children
  | x -> failwith ("ERREUR: il n'y a pas de méthode"^ string_of_int x)

