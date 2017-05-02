open Types

type score = float (* Supposé entre 0 et 1 *)

type t_candidat = t_foret * score

type t_population = t_candidat list


let print_population pop =
	List.map (fun x -> match x with 
		| (foret,score) -> match foret with
			| (a,b,c,d,e,f) -> print_tree a 0;Printf.printf "\n%!";print_tree b 0;Printf.printf "\n%!";print_tree c 0;Printf.printf "\n%!";print_tree d 0;Printf.printf "\n%!";print_tree e 0;Printf.printf "\n%!";print_tree f 0) pop;;

