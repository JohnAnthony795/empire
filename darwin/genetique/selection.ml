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


let () =
  Printf.printf "Test %!"


(*open Types*)
open TypesGen


(*sélections pour mating pool*)

(*Sélection basique : les n plus forts *)(*le tri serait peut etre bien....v2?*)
let select_n_best pool n =  
	let best ind1 ind2 =
		match (ind1,ind2) with 
			((f1,s1),(f2,s2)) -> if s1 >= s2 then ind1 else ind2
	in
	let bestOfList lInd = List.fold_left best (List.hd lInd) lInd 
	in
	let rec aux_select_n_best acu pool n = 
		if n = 0 then acu 
		else 
		let b = (bestOfList pool) in
			 aux_select_n_best (b::acu) (filter (fun i -> i != b) pool) (n-1)  (*définir l'égalité d'individus? pointeur devrait être ok*)
	in
aux_select_n_best [] pool n



(*merge des parents/enfants*)

let merge_best_and_child parents children = 
	(append (select_n_best parents (taillePop - (length children))) children)
	
	 






