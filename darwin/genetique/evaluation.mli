
open TypesGen

(* methode d'evaluation : 10 affrontements, 20 affrontements, affronter tous les autres, ... *)
type methode_evaluation = AFF10 | AFF20 | CAPTAIN

val evaluer : t_population -> methode_evaluation -> t_population


