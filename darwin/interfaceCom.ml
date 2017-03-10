(* Objectif: -assurer la communication entre le serveur ( prend des strings)
					et  le decision tree ( sort des actions)
					
		     -Lire les data envoyées par le serveur et les envoyer a l'updater de DATA *)
		     
		     
(* Notes :
	- Type de retour pour les fonctions (SUCCES/FAILURE ? unit ? bool ?)
	- t_action : move, set_city_production, end_turn
*)
		     
(* Fonctions publiques à faire :
	- send : t_action -> unit 	//reçoit un type action de Tree/main, le convertit en string et l'envoie au serveur par le socket

	Fonctions privées :
	- send_to_server : string -> unit	//l'envoi concret du message par le socket
	- action_to_string : t_action -> string
	- string_to_action : string -> unit //parser qui appelle les fonctions appropriées (du package DATA) ou qui se termine s'il recoit get_action (pour rendre la main au main)
	
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
		     
