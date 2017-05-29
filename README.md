# empire
Projet d'IA pour le RTS TBS-Empire

## TUTO : Comment ajouter un nouveau prédicat sans attraper une maladie mortelle ?

### Dans darwin/Types.ml : 
 - ajouter le prédicat dans le type t_predicat avec son type (commenter de façon explicite)
 - ajouter le printer du predicat dans pred_to_string
 - ajouter le code du prédicat dans pred_to_code de la forme : "?CODE:" ^ arg1_to_string ^ ":" ^ arg2_to_string
 - ajouter le prédicat dans le type t_prédicat du Types.mli
 
 
### Dans darwin/main.ml :
- ajouter dans le pattern-matching de compute_Action le nouveau predicat, le faire renvoyer le bool en question 
 
### Dans darwin/dataManager.ml : - 
ajouter dans les getters le/les fonctions qui permettent d'évaluer le prédicat (si possible directement renvoyer un bool pour éviter de coder trop dans le main)
Ajouter la déclaration des fonctions nécessaires dans le dataManager.mli
 
### Dans darwin/parser.mly : 
- ajouter en %token le code du prédicat
 - ajouter la grammaire du prédicat tout en bas de cette manière : CODEPREDICAT COLON TYPEARG1 COLON TYPEARG2 {predicat($3,$5)} 
 - si nécessaire, ajouter un nouveau type d'arguments en haut du parser
 
### Dans darwin/lexer.mll :
 - ajouter le code du prédicat dans la partie dédiée (en bas)
 
### Dans darwin/mutation.ml :
 - ajouter le prédicat à muter_predicat
