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
	let print_candidat cand =
		let ((a,b,c,d,e,f),_) = cand in
		print_tree a 0;
    Printf.printf "\n%!";
    print_tree b 0;
    Printf.printf "\n%!";
    print_tree c 0;
    Printf.printf "\n%!";
    print_tree d 0;
    Printf.printf "\n%!";
    print_tree e 0;
    Printf.printf "\n%!";
    print_tree f 0;
		()
	in let _ = List.map print_candidat pop in
	()

(**types**)

type score = float (* Supposé entre 0 et 1 *)

type t_candidat = t_foret * score

type t_population = t_candidat list

(* on ajoute le nombre de générations *)
type t_stockage = t_population * int 

(**Constantes**)

let taille_population = 100

let profondeur_max_arbre = 20 

(* structures minimales pour les tests *)

let arbre0 = Leaf End_turn 
(* let arbre0 = Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Node (Leaf End_turn,Nb_unite_allie_proche (ARMY,7,Inf),Leaf End_turn)) *)

let arbreUnite = Leaf (Explorer (1,1,1))
let arbreVille0 =  Leaf (Set_city_prod (1,ARMY))
let arbreArmy = Node (Leaf (Envahir (1,1,1)), Nb_ville_ennemie_proche(10,1,SupEq), Leaf (Explorer (1,1,1)))
let arbreVille = Node (Leaf (Do_nothing (1)), Unite_en_production, 
                      Node (Leaf (Set_city_prod (1,ARMY)), Nb_unite_allie_proche (8,ARMY,5,Inf), Leaf (Set_city_prod (1,FIGHT))))

let foret0 = (arbre0, arbre0, arbre0, arbre0, arbre0, arbre0)

let candidat0 = (foret0, 12.0)

let popu0 = [candidat0;candidat0; candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0;candidat0; candidat0;]

(* génère une population de End_turn de la taille passée en argument *)
let genere_popu nombre =
	let rec loop i acu =
		if i >= nombre then
			acu
		else
			loop (i+1) (candidat0 :: acu)
	in
	loop 0 []
