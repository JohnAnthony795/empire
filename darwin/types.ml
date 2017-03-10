(*
Définition des types customs utilisés:    
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)
	- t_action : move | set_city_production |end_turn 
		- move: piece_id * direction  ( si pas déjà défini)
			-direction : 0|1|2|3|4|5 ( 2|1|
									   3| |0
									    |4|5 )
		- set_city_production: city_id * piece_type_id (définition dans Server/Empire.ml)
	- t_arbre : Leaf of t_action 
				|Node of (t_predicat * t_arbre * t_arbre)
	-t_predicat: uniteProche * int * comparateur
				|int * comparateur * int 
				|float * comparateur * float
				|int *  
*)
