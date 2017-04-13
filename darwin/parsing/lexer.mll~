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

	let unite_of_code s = match s with
		 "A" -> ARMY
		|"T" -> TRANSPORT
		|"F" -> FIGHT
		|"B" -> BATTLESHIP
		|"P" -> PATROL

	let comp_of_code s = match s with
		  "<" -> Inf 
		| ">" -> Sup
		| "=" -> Eq  
		| "<=" -> InfEq  
		| ">=" -> SupEq  
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
	  | ("{A}"|"{T}"|"{F}"|"{B}"|"{P}") as unite { UNITE(unite_of_code unite)}
	  | ("<"|">"|"="|"<="|">=") as comp { COMP(comp_of_code comp)}
	  | ("U"|"D"|"L"|"R"|"UL"|"DR") as dir { DIR(dir_of_code dir)}

		(*predicats*)
	  | "NBUAP"          { NBUAP }
	  | "NBVAP"          { NBVAP }
		
		(*actions*)
	  | "MV" 	     { MV }
	  | "SCP"            { SCP }
	  | "ET"             { ET }

          | eof              { raise Eof }


