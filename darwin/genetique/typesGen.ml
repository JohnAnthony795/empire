open Types

let print_population pop =
	let rec print_aux p n =
	match p with
		|[]-> Printf.printf ".\n%!"
		|c::cs -> let (f,s) = c in 
			Printf.printf "%s" ((string_of_int n) ^" : "
					^ (string_of_float s) ^" pts\n") ;
			print_aux cs (n+1)
	in
		print_aux pop 1

let print_population_all pop =
  List.map (fun x -> match x with
      | (foret,score) -> match foret with
        | (a,b,c,d,e,f) -> print_tree a 0;
          Printf.printf "\n%!";
          print_tree b 0;
          Printf.printf "\n%!";
          print_tree c 0;
          Printf.printf "\n%!";
          print_tree d 0;
          Printf.printf "\n%!";
          print_tree e 0;
          Printf.printf "\n%!";
          print_tree f 0) pop; ()

(**types**)

type score = float (* Supposé entre 0 et 1 *)

type t_candidat = t_foret * score

type t_population = t_candidat list

(**Constantes**)

let taille_population = 100

let profondeur_max_arbre = 20 

(* structures minimales pour les tests *)

let arbre0 = Leaf End_turn
(* let arbre0 = Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Leaf End_turn)) *)

let foret0 = (arbre0, arbre0, arbre0, arbre0, arbre0, arbre0)

let candidat0 = (foret0, 12.0)

let popu0 = [candidat0;candidat0; candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0; candidat0;]


