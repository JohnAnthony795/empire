
open Types
open Printf

(* fonction qui retourne la profondeur d'un arbre *)
let rec depth = function
  | Leaf _ -> 0
  | Node (a,b,c) -> 1 + max (depth a) (depth c)

(* fonction qui remplace quand le prédicat est vrai*)
let rec replace pred sub tree =
  if pred tree then sub
  else match tree with
    | Leaf _ -> tree
    | Node (a,b,c) -> Node (replace pred sub a, b,replace pred sub c)
;;


let strSplit strToSplit delim =
  let str_start str len = String.sub str 0 len in
  let str_end str offset = String.sub str offset (String.length str - offset) in	
  let rec aux str toks = 
    if String.contains str delim then begin
      let i = String.index str delim in
      aux (str_end str (i + 1)) (str_start str i :: toks)
    end else
    if String.length str = 0 then List.rev toks else
      List.rev (str :: toks) in
  aux strToSplit []


let file = "IA.ads";;


let parseforet line = (*lit la ligne retourne le t_foret *)
  let lexbuf = Lexing.from_string (line^"\n") in
  Parser.foret Lexer.token lexbuf 
(*(Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn,Leaf End_turn)*) (*## CETTE LIGNE VEHICULE UNE VISION SIMPLISTE ET REDUCTRICE DE LA FORET##*)

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

(**lecture / écriture d'un arbre**)

let read_arbre fichier =
  let ic = open_in fichier in
  try
    let stringarbre = input_line ic in  (* read line from in_channel and discard \n *)
    print_endline ("\nstring de Foret lue:" ^ stringarbre ^ "\n");          (* ##TEST## *)
    flush stdout;                (* write on the underlying device now *)
    let forest = parseforet stringarbre in
    close_in ic;                  (* close the input channel *)
    forest

  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    		channels are not flushed *)


let write_arbre fichier forest =
  (* Write message to file *)
  let oc = open_out fichier in
  fprintf oc "%s\n" (forest_tocode forest);
  close_out oc             	 (* flush and close the channel *)


(**lecture/ecriture de population**)

let read_population fichier =   (*returns a population written in file fichier.*) (* [(arbre0,arbre0,arbre0,arbre0,arbre0,arbre0), 10.0]*)
  let read_file filename = (* renvoie une liste de lignes du fichier *)
    let lines = ref [] in
    let chan = open_in filename in
    try
      while (true); do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines 
  in
  let file_byline = read_file fichier in
  let read_candidat strcand = 
    let parsed = (strSplit strcand '|') in
    match parsed with
    | sscore::sforet::[] -> (parseforet sforet,float_of_string sscore )
    | _ -> failwith "ERREUR: Format de fichier non conforme"
  in
  let rec aux_read acu lines = 
    match lines with
    |[]            -> acu
    |scandidat::xs -> aux_read ((read_candidat scandidat)::acu) xs
  in
  aux_read [] file_byline


let write_population fichier pop =  (*writes population pop to file fichier*)
  let rec pop_tocode  p = 
    match p with 
    |[] -> ""
    |(foret,score)::cs -> string_of_float score ^ "|" ^ forest_tocode foret ^ "\n" ^ pop_tocode cs 

  in
  (* Write message to file *)
  let oc = open_out fichier in
  fprintf oc "%s\n" (pop_tocode pop);
  close_out oc             	 (* flush and close the channel *)



(** TODO **)
(* 

- read_population : unit -> t_population

*)

