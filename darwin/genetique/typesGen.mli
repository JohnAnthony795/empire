open Types

type score = float (* Supposé entre 0 et 1 *)

type t_candidat = t_foret * score

type t_population = t_candidat list

val print_population : t_population -> unit

(**Constantes**)

val taille_population : int

val profondeur_max_arbre : int

