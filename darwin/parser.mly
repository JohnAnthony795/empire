/*tenir à jour les predicaats !!!*/

%{
open Types 
open Printf (*for debug purposes*)
     %}
%token <int> INT
             %token <Types.comparateur> COMP
                                        %token <Types.unites> UNITE
                                                              %token <Types.direction> DIR

%token NBUAP NBVAP NBVEP LIADJ TR FOG UEP
%token MV AT EX EN TRA SCP ET DN

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
|   AT COLON INT COLON INT COLON INT { Attaquer ($3,$5,$7) }
|   EX COLON INT COLON INT COLON INT { Explorer ($3,$5,$7) }
|   EN COLON INT COLON INT COLON INT { Envahir ($3,$5,$7) }
|   TRA COLON INT COLON INT COLON INT { Transporter ($3,$5,$7) }
| SCP COLON INT COLON UNITE    { Set_city_prod ($3,$5) }
| ET                         { End_turn }
| DN COLON INT                { Do_nothing ($3)}
  ;
  predicat:
    QMARK codepred                             { $2 } 
      codepred:
    NBUAP COLON INT COLON UNITE COLON INT COLON COMP     { Nb_unite_allie_proche ($3,$5,$7,$9) }
| NBVAP COLON INT COLON INT COLON COMP                 { Nb_ville_allie_proche ($3,$5,$7) }
| NBVEP COLON INT COLON INT COLON COMP                 { Nb_ville_ennemie_proche ($3,$5,$7) }
| LIADJ                                                { Littoral_adjacent }
| TR                                                   { Transport }
| FOG COLON INT                                        { Fog_proche ($3)}
| UEP                                                  { Unite_en_production }
  ;

