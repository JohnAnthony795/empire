
(* TODO: utiliser ces methodes *)
type methode_evaluation = AFF10 | AFF20 | CAPTAIN

let shuffle entryList =
  let nd = List.map (fun c -> (Random.bits (), c)) entryList in
  let sond = List.sort compare nd in
  List.map snd sond

let evaluer popu methode =
  let pop_ref = Selection.select_n_best popu 1 in (* pour cette génération, le meilleur candidat sera notre candidat de référence *)
  let (foret_ref, _) = match pop_ref with 
    | hd :: tail -> hd
    | [] -> failwith "evaluation.evaluer : empty list"
  in (* on extrait la forêt *)
  ToolsArbres.write_arbre "foret_ref.frt" foret_ref;
  let eval_candidat candidat =
    let (foret, _) = candidat in
    ToolsArbres.write_arbre "foret_cand.frt" foret;
    (*let thread_ref = Thread.create Main.main foret_ref in*)
   let _ = Unix.system "./launchMain.native 0 &" in
    print_endline "Main ref lancé";
    let score = Main.main 1 in (* renvoie le score de ce candidat contre la ref *)
(*   Thread.join thread_ref ; (* attend la fin de l'exécution du thread ref *)*)
(foret, score)
in
List.map eval_candidat popu 

