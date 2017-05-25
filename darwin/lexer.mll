(* File lexer.mll *)
(*notes : pas testé, pas de cas défault*)
{
open Parser        (* The type token is defined in parser.mli *)
open Types
exception Eof

let dir_of_code s = match s with
    "U" -> Up
  |"D" -> Down
  |"R" -> Right
  |"L" -> Left
  |"UL" -> Upleft
  |"DR" -> Downright
  | _ -> failwith "dir_of_code: unhandled case"

let unite_of_code s = match s with
    'A' -> ARMY
  |'T' -> TRANSPORT
  |'F' -> FIGHT
  |'B' -> BATTLESHIP
  |'P' -> PATROL
  | _ -> failwith "unite_of_code: unhandled case"

let comp_of_code s = match s with
    "<" -> Inf 
  | ">" -> Sup
  | "=" -> Eq  
  | "<=" -> InfEq  
  | ">=" -> SupEq  
  | _ -> failwith "comp_of_code: unhandled case"
}

rule token = parse
  [' ' '\t']       { token lexbuf }     (* skip blanks ? *)
| ['\n' ]          { EOL  }
| '#'              { HASH }
| ','              { VIRG }  
| '('              { LPAREN }
| ')'              { RPAREN }
| ':'	             { COLON }
| '?'	             { QMARK }
| '!'              { EMARK }
| ['0'-'9']+ as nb { INT(int_of_string nb) }
| ('A'|'T'|'F'|'B'|'P') as unite { UNITE(unite_of_code unite)}
| ("<"|">"|"="|"<="|">=") as comp { COMP(comp_of_code comp)}
| ("U"|"D"|"L"|"R"|"UL"|"DR") as dir { DIR(dir_of_code dir)}

(*predicats*)
| "NBUAP"          { NBUAP }
| "NBVAP"          { NBVAP }
| "NBVEP"          { NBVEP }
| "LIADJ"          { LIADJ }
| "TR"             { TR }
| "FOG"            { FOG }
| "UEP"            { UEP }

(*actions*)
| "MV" 	     { MV }
| "MVS"       { MVS }
| "SCP"            { SCP }
| "ET"             { ET }
| "DN"      { DN}

| eof              { raise Eof }
|_                 { token lexbuf }

