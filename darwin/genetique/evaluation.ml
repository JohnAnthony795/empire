

(* TODO: utiliser ces methodes *)
type methode_evaluation = AFF10 | AFF20 | CAPTAIN

let evaluer popu methode =
  let pop_ref = Selection.select_n_best popu 1 in (* pour cette génération, le meilleur candidat sera notre candidat de référence *)
  let (foret_ref, _) = match pop_ref with 
    | hd :: tail -> hd
    | [] -> failwith "evaluation.evaluer : empty list"
  in (* on extrait la forêt *)

  let eval_candidat candidat =
  	let (foret, _) = candidat in
    let _ = Tools.write "foret_cand.frt" foret in
    (*let _ = Unix.system "xterm -hold -e \"../empire-server/Main.native\" &" in (*version de debug, ouvre l'out serveur dans un terminal*)*)
    let _ = Unix.system "../empire-server/Main.native > /dev/null &" in (*pas de sortie serveur*)
    (* let _ = Unix.system "../empire-server/Main.native &" in *)
    let _ = Unix.sleep 1 in
    (*let _ = Unix.system "../empire-captain/ai1.py localhost 9301 > /dev/null &" in*)
    let _ = Unix.system "./main.native 0 &" in
    let score = Main.main 1 in (* renvoie le score de ce candidat contre la ref *)
    (foret, score)
  in
  let _ = Tools.write "marshaled_foret_ref.frt" foret_ref in
  (*let _ = ToolsArbres.write_arbre "foret_ref.frt" foret_ref in*)
  List.map eval_candidat popu 

