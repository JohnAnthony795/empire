# empire
Projet d'IA pour le RTS TBS-Empire dans le cadre des projets tutorés de 4ème année.

## Contributeurs
Jérémy Basso (Loko)
Sandor Bügel (JohnAnthony795)
Sébastien Lanore (SebLanore)
Louis Rivière (M. Rivière Louis)

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
  ajouter dans le pattern-matching de compute_Action le nouveau predicat, le faire renvoyer le bool en question 

### Dans darwin/dataManager.ml : 
- ajouter dans les getters le/les fonctions qui permettent d'évaluer le prédicat (si possible directement renvoyer un bool pour éviter de coder trop dans le main)
 
### Dans darwin/parser.mly : 
- ajouter en %token le code du prédicat
 - ajouter la grammaire du prédicat tout en bas de cette manière : CODEPREDICAT COLON TYPEARG1 COLON TYPEARG2 {predicat($3,$5)} 
 - si nécessaire, ajouter un nouveau type d'arguments en haut du parser
 
### Dans darwin/lexer.mll :
  - ajouter le code du prédicat dans la partie dédiée (en bas)

## Comment ajouter une action :

TODO

