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
	let parseline line = (*lit la ligne retourne le t_foret ( peut etre regarder ocamllex qui fait de l'analyse syntaxique a part.)*)
		let arbres_string = Str.split (Str.regexp "#") line in   (*tokens est une list de string representant chacun un arbre.*)
			let rec parsetree st = (*convertit un arbre en string en un t_arbre*) 
				(*definitions des parseurs de predicat et action*)
				let parseaction act =
					let dir_of_string s = match s with
						 "UP" -> UP
						|"DOWN" -> DOWN
						|"RIGHT" -> RIGHT
						|"LEFT" -> LEFT
						|"UPLEFT" -> UPLEFT
						|"DOWNRIGHT" -> DOWNRIGHT

					in
						if (Str.string_match (Str.regexp "Move id°\([0-9]*\) \((UP\|DOWN\|RIGHT\|LEFT\|UPLEFT\|DOWNRIGHT)\)$")(*ocaml hait les backslashs mais osef*) act 0) 
							then Move (int_of_string (matched_group 1 act),dir_of_string (matched_group 2 act))  
						else if (Str.string_match (Str.regexp "Set_city_prod id°\([0-9]*\) \([0-9]*\)$") 
							then Set_city_prod (int_of_string (matched_group 1 act),int_of_string (matched_group 2 act)) 
						else if (Str.string_match (Str.regexp "End_turn$") act 0) then End_turn
						else failwith "FICHIER NON CONFORME %!"
				in

				let parsepredicat pred= 
				in

				let (idnode,idaction) = (Str.regexp "(.*)$"(*anything beetween parhenteses*),Str.regexp ".*$") (*definition des patern a matcher (remove idaction? voir mettre tout direct)*)
				in 
			  		if (Str.string_match idnode st 0) then  "(\(..\),\(..\),\(..\))$" (*marche pas non plus. en fait langage pas régulier , obligg de faire un parser? *)
							let elements =(*marche ap. il faut grouper*) Str.split (Str.regexp ",") (match Str.split (Str.regexp "[()]") st with (*il faut cut les parhentheses!*) 
																	x::[]-> x 
																       | _ ->  failwith "FICHIER NON CONFORME %!" ) 								in match elements with
								 td::pred::tg::[] -> Node (parsetree td, parsepredicat pred ,parsetree tg )
								| _ -> failwith "FICHIER NON CONFORME %!"
					else Leaf (parseaction st)
			in
				match arbres_string with
					 st1::st2::st3::st4::st5::st6::[] -> ((parsetree st1),(parsetree st2),(parsetree st3),(parsetree st4),(parsetree st5),(parsetree st6))
					|_ -> failwith "FICHIER NON CONFORME %!"
		(*(Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn) (*## CETTE LIGNE VEHICULE UNE VISION SIMPLISTE ET REDUCTRICE DE LA FORET##*)*)
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
	let forest_toString (tarmy,tpatrol,tbattleship,ttransporter,tfight,tcity) =
		let rec arbre_tostring t =
			match t with
				 Leaf a -> action_to_string a
				|Node (t1,p,t2) -> "(" ^ (arbre_tostring t1) ^ "," ^ (pred_to_string p) ^ "," ^ (arbre_tostring t2) ^ ")"
		in

		  arbre_tostring tarmy ^ "#"
		^ arbre_tostring tpatrol ^ "#"
		^ arbre_tostring tbattleship ^ "#"
		^ arbre_tostring ttransporter ^ "#"
		^ arbre_tostring tfight ^ "#"
		^ arbre_tostring tcity
	in
	  (* Write message to file *)
	  let oc = open_out fichier in
	  	fprintf oc "%s\n" (forest_toString forest);
	 	close_out oc             	 (* flush and close the channel *)
;;

let () =
	write_arbre file (Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,arbre0) ;
	let (t1,_,_,_,_,t6) = (read_arbre file)  in printf "1er Arbre lu dans le fichier :\n"; print_tree t1 0 ;
						    printf "dernier Arbre lu dans le fichier :\n"; print_tree t6 0
;;
