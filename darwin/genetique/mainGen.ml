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
(*  1. popu est evaluée par combats successifs (appel du main de darwin) -> popu1 (scores mis à jour)
	2. on sélectionne dans popu1 les "meilleurs" individus -> popu2
	3. on effectue des recombinaisons sur popu2 -> popu3 (on renvoie les nouveaux individus seulement)
	4. on fait muter popu3 (les enfants) -> popu4
	5. on sélectionne dans (popu1 U popu4) des individus pour la prochaine génération -> popu5
*)

open ToolsArbres

let iterations = 20 (* nombre de générations à simuler avant de s'arrêter; on pourrait la mettre en paramètre *)

(* TODO : récupérer depuis un fichier *)
let nbreGenInitial = 0 (* nombre de générations simulées depuis le début *)

let main () =
	let rec mainLoop popu nbreGen =
		let popu1 = Evaluation.evaluer popu in (* Met à jour le score d'adaptabilité de chaque individu *)
		let popu2 = Selection.get_meilleurs popu1 in
		let popu3 = Croisement.recombine popu2 in 
		let popu4 = Mutation.mute popu3 in
		let popu5 = Selection.get_prochaine_generation popu1 popu4 in
		if nbreGen < (nbreGenInitial + iterations) then
			mainLoop popu5 (nbreGen +1)
		else
			ToolsArbres.write_population popu5 nbreGen ; (* on sauvegarde notre population actuelle dans des fichiers *)
			write_nbreGen nbreGen ; (* on sauvegarde notre nombre de générations simulées dans un fichier *)
			()
	in
	
	let popu = ToolsArbres.read_population () in (* initialisation de la population, lue depuis le disque *)
	mainLoop popu nbreGenInitial
in main ()



































