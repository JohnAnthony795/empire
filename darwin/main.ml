(*
Le main lit et exploite les arbres, puis utilise interfaceCom pour communiquer les décisions au serveur. Il utilise aussi le dataManager pour LIRE des infos sur la carte (interfaceCom s'occupe de peupler le dataManager).
@Params: optionnels: 1 arbre  -> sélection (affronte un opposant random)
					 rien	  -> on lit l'arbre depuis le fichier IA.ads et on affronte un autre 									joueur sur le serveur
@Retour:	score d'adaptabilité (fonction de : victoire, couverture, nombre d'unité, villes...)


-Read_Arbre : string/FILE -> t_foret (Lecture d'arbre from fichier)
-Compute_Action : t_ID -> t_action (Parcour d'arbre:(appelle le datamanager pour avoir des infos précises))

*)