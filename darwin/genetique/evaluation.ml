
(** TOOLS **)

let marshal_write filename element =
  let oc = open_out_bin filename in
  Marshal.to_channel oc element [Marshal.Closures; Marshal.Compat_32];
  close_out oc;
	()


(* TODO: utiliser ces methodes *)
type methode_evaluation = AFF10 | AFF20 | CAPTAIN

let evaluer popu methode =
  let pop_ref = Selection.select_n_best popu 1 in (* pour cette génération, le meilleur candidat sera notre candidat de référence *)
  let cand_ref = match pop_ref with 
    | hd :: tail -> hd
    | [] -> failwith "evaluation.evaluer : empty list"
  in (* on extrait la forêt *)
	let (foret_ref, score_ref) = cand_ref in
  let _ = ToolsArbres.write_arbre "foret_ref.frt" foret_ref in
  let eval_candidat candidat =
  	let (foret, _) = candidat in
    let _ = ToolsArbres.write_arbre "foret_cand.frt" foret in
    (*let _ = marshal_write "marshaled_foret_cand.frt" foret in*)
    (*let _ = Unix.system "xterm -hold -e \"../empire-server/Main.native\" &" in (*version de debug, ouvre l'out serveur dans un terminal*)*)
    let _ = Unix.system "../empire-server/Main.native > /dev/null &" in (*pas de sortie serveur*)
    (* let _ = Unix.system "../empire-server/Main.native &" in *)
    (*let _ = Unix.system "../empire-captain/ai1.py localhost 9301 > /dev/null &" in*)
    let _ = Unix.system "./main.native 0 > /dev/null &" in
    let score = Main.main 1 in (* renvoie le score de ce candidat contre la ref *)
    (foret, score)
  in
  (*let _ = marshal_write "marshaled_foret_ref.frt" foret_ref in*)
		
	let popunoref = List.filter (fun x -> x != cand_ref) popu in
  let evaluatedpopunoref = List.map eval_candidat popunoref in
	(foret_ref,score_ref *. 0.95) :: evaluatedpopunoref


