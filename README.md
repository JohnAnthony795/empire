# empire
Projet d'IA pour le RTS TBS-Empire dans le cadre des projets tutorés de 4ème année.

## Contributeurs
	- Jérémy Basso (Loko)
	- Sandor Bügel (JohnAnthony795)
	- Sébastien Lanore (SebLanore)
	- Louis Rivière (Arrakis)

## Make guide :

	- make all pour tout compiler
	- make clean pour effacer tous les exécutables et autres fichiers regénérables
	- make nous pour compiler les fichiers de darwin et de empire-server, permettant de lancer l'entraînement sans interface graphique


## Comment ajouter un prédicat :

### Dans darwin/types.ml : 
	- ajouter le prédicat dans le type t_predicat avec son type, sans oublier de l'ajouter dans darwin/types.mli
	- ajouter le printer du predicat dans pred_to_string
	- ajouter le code du prédicat dans pred_to_code de la forme : "?CODE:" ^ arg1_to_string ^ ":" ^ arg2_to_string
    
### Dans darwin/main.ml :
  - ajouter dans le pattern-matching de compute_Action le nouveau predicat, le faire renvoyer le bool en question 

### Dans darwin/dataManager.ml : 
	- ajouter dans les getters le/les fonctions qui permettent d'évaluer le prédicat (si possible directement renvoyer un bool pour éviter de coder trop dans le main)
 
### Dans darwin/parser.mly : 
	- ajouter en %token le code du prédicat
	- ajouter la grammaire du prédicat tout en bas de cette manière : CODEPREDICAT COLON TYPEARG1 COLON TYPEARG2 {predicat($3,$5)} 
	- si nécessaire, ajouter un nouveau type d'arguments en haut du parser
 
### Dans darwin/lexer.mll :
  - ajouter le code du prédicat dans la partie dédiée (en bas)
	
### Dans darwin/genetique/mutation.ml :
	- ajouter les options de mutation du prédicat.
	- incrementer les valeurs de la sélection aléatoire


## Comment ajouter une action :
### Dans darwin/types.ml : 
	- ajouter l'action dans le type t_predicat avec son type, sans oublier de l'ajouter dans darwin/types.mli
	- ajouter le printer de l'action dans pred_to_string
	- ajouter le code du prédicat dans action_to_code de la forme : "!ACTION:" ^ arg1_to_string ^ ":" ^ arg2_to_string
 
### Dans darwin/parser.mly : 
	- ajouter en %token le code de l'action
	- ajouter la grammaire de l'action tout en bas de cette manière : CODEACTION COLON TYPEARG1 COLON TYPEARG2 {action($3,$5)} 
	- si nécessaire, ajouter un nouveau type d'arguments en haut du parser
 
### Dans darwin/lexer.mll :
	- ajouter le code de l'action dans la partie dédiée (en bas)
  
### Dans darwin/genetique/mutation.ml :
	- ajouter les options de mutation de l'action.
	- incrementer les valeurs de la sélection aléatoire
  
	
## Comment lancer un match :

En lançant un serveur (empire-server/Main.native), on peut ensuite ajouter tous les joueurs souhaités. "darwin/main.native 0" fait participer
Darwin à la partie en utilisant l'arbre stocké dans foret_ref.frt (par défaut, le dernier arbre à avoir été le meilleur de sa génération).
Lancer Darwin avec un autre argument utilise l'arbre stocké dans foret_cand.frt. On peut ensuite faire participer n'importe quel autre joueur 
humain ou IA à la partie.
Alternativement, lancer empire-scripts/execute-darwin-obs-capt.sh lance directement une partie via l'interface GTK entre foret_ref.frt et Captain, avec un observateur.


## Comment lancer une session d'entrainement :
Lancer darwin/mainGen.native lance une session d'entraînement sur plusieurs générations, en utilisant les paramètres fixés en dur dans le code (cf. ci-dessous).

## Comment modifier les options d'entrainement :
### Choisir la taille de la population et le nombre d'individus renouvelés par génération :
	- dans darwin/typeGen.ml  modifier la valeur de "taille_population" 
	- dans darwin/genetique/mainGen.ml  modifier la valeur de "individusASelectionner" (doit rester pair)
 
### Remise à zéro de la population :
  - dans darwin/nbreGen.cfg mettre la valeur de la génération à 0 (a pour effet d'instancier le fichier currentGen.pop en arbre vide au lancement)

### Choisir le type d'évaluation :
	- dans darwin/genetique/evaluation.ml changer la valeur de "methode_ref" en fonction du commentaire situé en amont de sa déclaration.

### Choisir le type de Sélection : 
  - dans darwin/genetique/mainGen.ml Dans la fonction main lors de l'appel de Selection.selection modifier la valeur du dernier paramètre
	
### Comment modifier la taille de la carte d'entraïnement :
  - la définir dans le DataManager et empire-server/Main.ml simultanément
