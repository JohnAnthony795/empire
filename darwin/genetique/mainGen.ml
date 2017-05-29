(*
Algorithme génétique, génère des arbres décisionnels dans des fichiers "IAx.ads":
-Arbres pour chaque unitée + ville
-Arbre stratégie?

(Package de communication avec fichier.ads?)

Lance des mains pour faire s'affronter les arbres générés ( 2 arbres passés en parametres au main)..

Fonctions à développer:
- Algo genétique ( mutation, croisement, sélection ect...))

Remarques / suggestions :
- On dump les arbres retenus toutes les N générations (selon le temps d'exécution)
*)

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

let individusASelectionner = 12 (* DOIT ETRE PAIR!!!! *)

let iterations = 2000 (* nombre de générations à simuler avant de s'arrêter; on pourrait la mettre en paramètre *)

(** TOOLS **)

let marshal_read_popu filename =
  let ic = open_in_bin filename in
  try begin
		let return = (Marshal.from_channel ic : t_stockage) in
		close_in ic;
		return
	end
  with End_of_file -> 
		close_in ic;
		failwith "Erreur mainGen.marshal_read_popu"
		
let marshal_write filename element =
  let oc = open_out_bin filename in
  Marshal.to_channel oc element [Marshal.Closures; Marshal.Compat_32];
  close_out oc;
	()

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
let getNbreGenInitial configFile =
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
  int_of_string (findValueOf "nbreGen" (read_file configFile))

let nbreGenInitial = getNbreGenInitial "nbreGen.cfg" (* nombre de générations simulées depuis le début *)

let shuffle entryList =
  let nd = List.map (fun c -> (Random.bits (), c)) entryList in
  let sond = List.sort compare nd in
  List.map snd sond

let reset_switch () =
  let ic = open_in "RESET_FILE" in
  let value = ref("") in
  try
    value := input_line ic;
    close_in ic;
    !value
  with End_of_file ->
    close_in ic;
    ""

(** MAIN **)

let foretTest = (arbre0,arbre0,arbre0,arbre0,arbre0,arbre0)  
let popuTest = (foretTest,10.0)

let rec first_popu acu =
  if (acu <> taille_population) then
    popuTest::(first_popu (acu+1))
  else
    [popuTest]

let main () =

  let rec mainLoop popu nbreGen =
    print_endline ("\nGénération n° "^ (string_of_int nbreGen));
    print_population popu;
    let popu1 = Evaluation.evaluer popu Evaluation.AFF10 in (* Met à jour le score d'adaptabilité de chaque individu ; c'est là que se passent les affrontements *)
    let popu2 = Selection.select_n_parents popu1 individusASelectionner 1 in (* popu2 garde les meilleurs individus *)
    let popu3 = Croisement.main_cross popu2 in (* popu3 sont les nouveaux individus obtenus par recombinaison *)
    (*print_population popu3 ;*)
    let popu4 = Mutation.mute popu3 in (* ces individus recombinés ont ensuite des chances de muter pour donner popu4 *)
    (*print_population popu4 ;*) 
    let popu5 = Selection.merge_generations popu1 popu4 1 in (* on garde parmi la population initiale (popu1) et les nouveaux mutants recombinés (popu4) certains individus pour la prochaine génération *)
    if nbreGen < (nbreGenInitial + iterations) then
      begin
      ToolsArbres.write_population "current_gen.pop" popu5; (* on sauvegarde notre population actuelle dans des fichiers *)
      (*let _ = marshal_write "marshaled_pop" (popu5, nbreGen) in*)
      write_nbreGen "nbreGen.cfg" nbreGen ; 
      mainLoop popu5 (nbreGen +1)
    end
    else begin
      ToolsArbres.write_population "current_gen.pop" popu5; (* on sauvegarde notre population actuelle dans des fichiers *)
      (*let _ = marshal_write "marshaled_pop" (popu5, nbreGen) in*)
      write_nbreGen "nbreGen.cfg" nbreGen ; (* on sauvegarde notre nombre de générations simulées dans un fichier *)
      ()
    end
  in

  (*let (popu, nbreGen) = marshal_read_popu "marshaled_pop" in*)
  let nbreGen = nbreGenInitial in
  let popu = ToolsArbres.read_population "current_gen.pop" in (* initialisation de la population, lue depuis le disque *) 
  mainLoop popu nbreGen


let () =
  (* let _ = marshal_write "marshaled_pop" (popu0,0) in *)
  if nbreGenInitial = 0 then
		let _ = ToolsArbres.write_population "current_gen.pop" popu0 in ();
	else ();
  print_endline "Start mainGen.";
  Random.self_init ();
  main ()

