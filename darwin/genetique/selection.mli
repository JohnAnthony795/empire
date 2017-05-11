open TypesGen

val select_n_best : t_population -> int -> t_population

val select_n_proportional : t_population -> int -> t_population

val select_n_proportional_bis : t_population -> int -> t_population

val select_n_parents : t_population -> int -> int -> t_population  (*globale*)




val merge_best_and_child : t_population -> t_population -> t_population

val merge_generations : t_population ->  t_population -> int -> t_population

