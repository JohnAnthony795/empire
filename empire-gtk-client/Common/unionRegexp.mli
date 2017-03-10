
(* Regexp unions, which associates values of type 'a to matches. *)
type 'a union_regexp

type 'a t = 'a union_regexp

(* Create an empty union_regexp. *)
val empty: Pcre.cflag list -> 'a union_regexp

val is_empty: 'a union_regexp -> bool

(* Adds the given regexps to the union_regexp. These regexps are associated to value 'a. *)
val add: 'a union_regexp -> 'a -> string list -> 'a union_regexp

val union: 'a union_regexp -> 'a union_regexp -> 'a union_regexp

(* Convenience function *)
val create: Pcre.cflag list -> ('a * string list) list -> 'a union_regexp

(* Indicates if the given string matches at least one regexp in the union. *)
val matches: 'a union_regexp -> string -> bool

(* Returns all the 'a values associated to matching regexps. An 'a value may occur at most once in the returned list. 
 * [] if none matches. *)
val associated: 'a union_regexp -> string -> 'a list


(* Return a list of all regexps in this union. *)
val get_regexps: 'a union_regexp -> string list
