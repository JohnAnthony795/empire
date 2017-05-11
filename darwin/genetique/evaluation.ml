
type methode_evaluation = AFF10 | AFF20 | CAPTAIN

let shuffle entryList =
  let nd = List.map (fun c -> (Random.bits (), c)) entryList in
  let sond = List.sort compare nd in
  List.map snd sond

let evaluer popu methode =
  let pop_ref = Selection.select_n_best popu 1 in (* pour cette génération, le meilleur candidat sera notre candidat de référence *)
  let (foret_ref, _) = (List.hd pop_ref) in (* on extrait la forêt *)

  let eval_candidat candidat =
  	let (foret, _) = candidat in
    let thread_ref = Thread.create Main.main foret_ref in
    let score = Main.main foret in (* renvoie le score de ce candidat contre la ref *)
    Thread.join thread_ref ; (* attend la fin de l'exécution du thread ref *)
    (foret, score)
  in
  List.map eval_candidat popu 

