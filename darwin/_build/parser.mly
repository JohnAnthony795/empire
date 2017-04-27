 /*tenir à jour les predicaats !!!*/

%{
  open Types 
  open Printf (*for debug purposes*)
%}
        %token <int> INT
        %token <Types.comparateur> COMP
	%token <Types.unites> UNITE
	%token <Types.direction> DIR

	%token NBUAP NBVAP
	%token MV SCP ET

        %token LPAREN RPAREN HASH VIRG COLON QMARK EMARK
        %token EOL

        %left         /* lowest precedence */
        %nonassoc        /* highest precedence */
        %start foret             /* the entry point */

        %type <Types.t_foret> foret
	%type <Types.t_arbre> arbre

	%type <Types.t_action> action
	%type <Types.t_action> codeaction
	%type <Types.t_predicat> predicat
	%type <Types.t_predicat> codepred
        %%

        foret:
            arbre HASH arbre HASH arbre HASH arbre HASH arbre HASH arbre EOL  { ($1,$3,$5,$7,$9,$11) }
        ;
        arbre:
            action                        { Leaf ($1) }
          | LPAREN arbre VIRG predicat VIRG arbre RPAREN      { Node($2,$4,$6) }
        ;

	action:
	    EMARK codeaction   	       { $2 }
	codeaction:
            MV COLON INT COLON DIR     { Move ($3,$5) }
          | SCP COLON INT COLON INT    { Set_city_prod ($3,$5) }
	  | ET                         { End_turn }
        ;
	predicat:
	    QMARK codepred                             { $2 } 
	codepred:
            NBUAP COLON UNITE COLON INT COLON COMP     { Nb_unite_allie_proche ($3,$5,$7) }
          | NBVAP COLON INT COLON COMP                 { Nb_ville_allie_proche ($3,$5) }
        ;