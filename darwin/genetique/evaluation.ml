
(** TOOLS **)

let custom_sleep f = 
  let start = Unix.gettimeofday() in
  let rec loop () = if Unix.gettimeofday() > start +. f then () else loop () in
  loop ()

(* 0 => la ref affronte tout le monde, dont elle-même; son score est donc réévalué à chaque tour
   	 1 => la ref affronte tout le monde, sauf elle-même; son score n'est pas réévalué, mais baisse de 5% à chaque tour
   	 2 => la ref affronte tout le monde, sauf elle-même; elle affronte tout à la fin l'IA ayant le meilleur score pour s'évaluer elle-même
   	 3 => Captain prend le rôle de ref, et tout le monde l'affronte *)

let methode_ref = 2

let eval_candidat_contre_Captain candidat =
  let (foret, _) = candidat in
  let _ = ToolsArbres.write_arbre "foret_cand.frt" foret in
  (*let _ = marshal_write "marshaled_foret_cand.frt" foret in*)
  let _ = Unix.system "../empire-server/Main.native > /dev/null &" in (*pas de sortie serveur*)
  let _ =  custom_sleep 0.1 in (* sleep nécessaire ou non selon le PC qui exécute le programme *)
  let _ = Unix.system "../empire-captain/ai1.py localhost 9301 > /dev/null &" in
  let score = Main.main 1 in (* renvoie le score de ce candidat contre Captain *)
  (foret, score)

let eval_candidat candidat =
  let (foret, _) = candidat in
  let _ = ToolsArbres.write_arbre "foret_cand.frt" foret in
  (*let _ = marshal_write "marshaled_foret_cand.frt" foret in*)
  (*let _ = Unix.system "xterm -hold -e \"../empire-server/Main.native\" &" in (*version de debug, ouvre l'out serveur dans un terminal*)*)
  let _ = Unix.system "../empire-server/Main.native > /dev/null &" in (*pas de sortie serveur*)
  let _ =  custom_sleep 0.1 in (* sleep nécessaire ou non selon le PC qui exécute le programme *)
  let _ = Unix.system "./main.native 0 > /dev/null &" in
  (* un bloc try/with pour relancer le prog s'il rencontre une ECONNREFUSED *)
  try
    let score = Main.main 1 in (* renvoie le score de ce candidat contre la ref *)
    (foret, score)
  with Unix.Unix_error(Unix.ECONNREFUSED,_,_) -> let _ = Unix.system "killall Main.native; echo Crash, on essaie de se relancer tout seul !; sleep 3; ./mainGen.native" in
    failwith "Ceci n'est pas un failwith"

let evaluer popu =
  let pop_ref = Selection.select_n_best popu 1 in (* pour cette génération, le meilleur candidat sera notre candidat de référence *)
  let cand_ref = match pop_ref with 
    | hd :: tail -> hd
    | [] -> failwith "evaluation.evaluer : empty list"
  in 
  let (foret_ref, score_ref) = cand_ref in (* on extrait la forêt *)

  let _ = ToolsArbres.write_arbre "foret_ref.frt" foret_ref in


  (*let _ = marshal_write "marshaled_foret_ref.frt" foret_ref in*)
  if methode_ref = 0 then
    List.map eval_candidat popu 

  else if methode_ref = 1 then
    let popu_sauf_ref = List.filter (fun x -> x != cand_ref) popu in
    (foret_ref,score_ref *. 0.95) :: (List.map eval_candidat popu_sauf_ref) (* on renvoie la ref avec un score baissé de 5% ainsi que les autres candidats réévalués *)

  else if methode_ref = 2 then
    let popu_sauf_ref = List.filter (fun x -> x != cand_ref) popu in
    let popu_sauf_ref_evaluee = List.map eval_candidat popu_sauf_ref in
    let meilleur_cand = match Selection.select_n_best popu 1 with 
      | hd :: tail -> hd
      | [] -> failwith "evaluation.evaluer : empty list"
    in
    let (foret_meilleur_cand,_) = meilleur_cand in
    let _ = ToolsArbres.write_arbre "foret_cand.frt" foret_meilleur_cand in
    let _ = Unix.system "../empire-server/Main.native > /dev/null &" in (*pas de sortie serveur*)
    let _ = Unix.sleep 1 in
    let _ = Unix.system "./main.native 1 > /dev/null &" in
    Printf.printf "ref: %!";
    try
      let score_ref = Main.main 0 in (* renvoie le score de LA REF contre le meilleur candidat *)
      (foret_ref,score_ref) :: popu_sauf_ref_evaluee
    with Unix.Unix_error(Unix.ECONNREFUSED,_,_) -> let _ = Unix.system "killall Main.native; echo Crash, on essaie de se relancer tout seul !; sleep 3; ./mainGen.native" in
      failwith "Ceci n'est pas un failwith"

  else if methode_ref = 3 then
    List.map eval_candidat_contre_Captain popu 

  else
    failwith "ERREUR Evaluation.evaluer: mauvaise methode_ref spécifiée"
