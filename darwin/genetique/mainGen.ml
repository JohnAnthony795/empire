
(** ETAPES DE L'ALGO MAINLOOP **)
(*      1. popu est evaluée par combats successifs (appel du main de darwin) -> popu1 (scores mis à jour)
        	2. on sélectionne dans popu1 les "meilleurs" individus -> popu2
        	3. on effectue des recombinaisons sur popu2 -> popu3 (on renvoie les nouveaux individus seulement)
        	4. on fait muter popu3 (les enfants) -> popu4
        	5. on sélectionne dans (popu1 U popu4) des individus pour la prochaine génération -> popu5
*)

open ToolsArbres
open Types
open TypesGen

let individusASelectionner = 26 (* DOIT ETRE PAIR!!!! *)

let iterations = 2000 (* nombre de générations à simuler avant de s'arrêter; on pourrait la mettre en paramètre *)
(* À noter que si le programme est interrompu pour une raison quelconque, la dernière génération est stockée dans current_gen.pop *)

(** TOOLS **)

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

let write_nbreGen configFile nbreGen = 
  let out_chan = open_out configFile in
  let str = "nbreGen=" ^ (string_of_int nbreGen) in
  output_string out_chan str;
  close_out out_chan

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

(* Récupère dans le fichier passé en paramètre la valeur après nbreGen= et la renvoie *)
let nbreGenInitial =
  let findValueOf strToFind lines =
    let rec recfindValueOf strToFind lines = (* renvoie la valeur de nbreGen en String *)
      match lines with
      | [] -> ""
      | line :: remainingLines -> match strSplit line '=' with 
        | id :: tl -> let tlHd = match tl with
            | hd :: _ -> hd
            | [] -> failwith "mainGen.getNbreGenInitial"
          in
          if (id = strToFind) then tlHd else recfindValueOf strToFind remainingLines
        | _ -> recfindValueOf strToFind remainingLines
    in
    let valueFound = recfindValueOf strToFind lines in
    if valueFound = "" then failwith ("findValueOf : could not find \"" ^ strToFind ^ "\" in the lines") else valueFound in
  int_of_string (findValueOf "nbreGen" (read_file "nbreGen.cfg"))

(** MAIN **)

let main () =
  let rec mainLoop popu nbreGen =
    print_endline ("\nGénération n° "^ (string_of_int nbreGen));
    print_population popu;
    let popu1 = Evaluation.evaluer popu in (* Met à jour le score d'adaptabilité de chaque individu *)
    let popu2 = Selection.select_n_parents popu1 individusASelectionner 3 in (* popu2 garde les meilleurs individus ; c'est là que se passent les affrontements *)
    let popu3 = Croisement.main_cross popu2 in (* popu3 sont les nouveaux individus obtenus par recombinaison *)
    let popu4 = Mutation.mute popu3 in (* ces individus recombinés ont ensuite des chances de muter pour donner popu4 *)
    let popu5 = Selection.merge_generations popu1 popu4 1 in (* on garde parmi la population initiale (popu1) et les nouveaux mutants recombinés (popu4) certains individus pour la prochaine génération *)
    ToolsArbres.write_population "current_gen.pop" popu5; (* on sauvegarde notre population actuelle dans des fichiers *)
    write_nbreGen "nbreGen.cfg" nbreGen ; 
    if nbreGen < (nbreGenInitial + iterations) then
      mainLoop popu5 (nbreGen +1)
    else
      ()
  in
  let nbreGen = nbreGenInitial in
  let popu = ToolsArbres.read_population "current_gen.pop" in (* initialisation de la population, lue depuis le disque *) 
  mainLoop popu nbreGen


let () =
  if nbreGenInitial = 0 then
    let _ = ToolsArbres.write_population "current_gen.pop" (genere_popu taille_population) in ();
  else ();
  print_endline "Start mainGen.";
  Random.self_init ();
  main ()

