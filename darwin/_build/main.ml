(*
Le main lit et exploite les arbres, puis utilise interfaceCom pour communiquer les décisions au serveur. Il utilise aussi le dataManager pour LIRE des infos sur la carte (interfaceCom s'occupe de peupler le dataManager).
@Params: optionnels: 1 arbre  -> sélection (affronte un opposant random)
					 rien	  -> on lit l'arbre depuis le fichier IA.ads et on affronte un autre 									joueur sur le serveur
@Retour:	score d'adaptabilité (fonction de : victoire, couverture, nombre d'unité, villes...)


-read_arbre : string/FILE -> t_foret (Lecture d'arbre from fichier)
-Compute_Action : t_ID -> t_action (Parcour d'arbre:(appelle le datamanager pour avoir des infos précises))

*)


open Types
open Printf

let file = "IA.ads";;

let () =
	printf " \nPrint de l'arbre de base : \n";
	print_tree arbre0 0 ;
	printf "fin: \n";;


let read_arbre fichier =
	let parseline line = (*lit la ligne retourne le t_foret *)
         	   let lexbuf = Lexing.from_string (line^"\n") in
           		 Parser.foret Lexer.token lexbuf 
		(*(Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn)*) (*## CETTE LIGNE VEHICULE UNE VISION SIMPLISTE ET REDUCTRICE DE LA FORET##*)
	in
		let ic = open_in fichier in
		try
		    	let stringarbre = input_line ic in  (* read line from in_channel and discard \n *)
		    		print_endline ("\nstring de Foret lue:" ^ stringarbre ^ "\n");          (* ##TEST## *)
		    		flush stdout;                (* write on the underlying device now *)
				let forest = parseline stringarbre in
		   			close_in ic;                  (* close the input channel *)
					forest

	 	 with e ->                      (* some unexpected exception occurs *)
	   		 close_in_noerr ic;           (* emergency closing *)
	    		 raise e                      (* exit with error: files are closed but
		                            		channels are not flushed *)
;;


let write_arbre fichier forest =
	let forest_tocode (tarmy,tpatrol,tbattleship,ttransporter,tfight,tcity) =
		let rec arbre_tocode t =
			match t with
				 Leaf a -> action_to_code a
				|Node (t1,p,t2) -> "(" ^ (arbre_tocode t1) ^ "," ^ (pred_to_code p) ^ "," ^ (arbre_tocode t2) ^ ")"
		in

		  arbre_tocode tarmy ^ "#"
		^ arbre_tocode tpatrol ^ "#"
		^ arbre_tocode tbattleship ^ "#"
		^ arbre_tocode ttransporter ^ "#"
		^ arbre_tocode tfight ^ "#"
		^ arbre_tocode tcity
	in
	  (* Write message to file *)
	  let oc = open_out fichier in
	  	fprintf oc "%s\n" (forest_tocode forest);
	 	close_out oc             	 (* flush and close the channel *)
;;

let () =
	write_arbre file (Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,arbre0) ;
	let (t1,_,_,_,_,t6) = (read_arbre file)  in printf "1er Arbre lu dans le fichier :\n"; print_tree t1 0 ;
						    printf "\nDernier Arbre lu dans le fichier :\n"; print_tree t6 0
;;
