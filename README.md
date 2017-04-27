# empire
Projet d'IA pour le RTS TBS-Empire

Avancée réseau :

Après étude approfondie des interfaces réseau existantes, il paraît logique d'utiliser l'interface tee.py qui fournit une création de sockets pour l'observer et le joueur, et attend la connection de ces derniers. 

####tee.py spec :
"This program waits for a connection on <observer-port> (for the observer) and"
"on <player-port> (for the player). Then, it connectes to the server and"
"passes messages of the server to both the player and the observer. All messages"
"but end_turn messages, from the player, are sent to the server and messages from"
"the observer are ignored. The end_turn message is sent to the server only if"
"received by both the observer and the player."

Ainsi, il ne reste qu'a créer (en Caml) la partie client, pour cela j'ai commencé à étudier ceci : https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html
une sorte de design-pattern pour les connexions(voir en particulier la partie client)

Autre solution questionnée :
une interface réseau en pyhton (plus simple ?) qui récupère les outils caml du data manager ? je pense que c est une surcouche non nécessaire

JB.
