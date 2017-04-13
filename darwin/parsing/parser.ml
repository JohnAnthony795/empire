type token =
  | INT of (int)
  | COMP of (Types.comparateur)
  | UNITE of (Types.unites)
  | DIR of (Types.direction)
  | NBUAP
  | NBVAP
  | MV
  | SCP
  | ET
  | LPAREN
  | RPAREN
  | HASH
  | VIRG
  | COLON
  | QMARK
  | EMARK
  | EOL

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
  open Types
# 25 "parser.ml"
let yytransl_const = [|
  261 (* NBUAP *);
  262 (* NBVAP *);
  263 (* MV *);
  264 (* SCP *);
  265 (* ET *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* HASH *);
  269 (* VIRG *);
  270 (* COLON *);
  271 (* QMARK *);
  272 (* EMARK *);
  273 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* COMP *);
  259 (* UNITE *);
  260 (* DIR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\004\000\005\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\012\000\001\000\007\000\002\000\005\000\005\000\001\000\002\000\
\007\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\002\000\000\000\
\000\000\000\000\007\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\006\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\009\000\
\000\000\001\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\012\000\019\000\025\000"

let yysindex = "\007\000\
\247\254\000\000\247\254\251\254\000\000\253\254\000\000\254\254\
\255\254\001\255\000\000\000\000\247\254\002\255\011\255\013\255\
\004\255\000\255\005\255\006\255\007\255\247\254\008\255\010\255\
\000\000\247\254\021\255\025\255\015\255\026\255\027\255\019\255\
\000\000\000\000\247\254\017\255\020\255\000\000\023\255\032\255\
\034\255\247\254\024\255\000\000\028\255\035\255\247\254\000\000\
\029\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\000\000\000\000\000\000"

let yytablesize = 46
let yytable = "\008\000\
\003\000\009\000\010\000\011\000\023\000\024\000\004\000\001\000\
\013\000\017\000\014\000\020\000\015\000\021\000\016\000\022\000\
\018\000\026\000\029\000\027\000\028\000\030\000\032\000\031\000\
\033\000\034\000\035\000\037\000\036\000\038\000\040\000\039\000\
\043\000\041\000\042\000\044\000\048\000\046\000\045\000\047\000\
\000\000\000\000\000\000\049\000\000\000\050\000"

let yycheck = "\003\000\
\010\001\007\001\008\001\009\001\005\001\006\001\016\001\001\000\
\012\001\013\000\013\001\001\001\014\001\001\001\014\001\012\001\
\015\001\013\001\022\000\014\001\014\001\014\001\026\000\014\001\
\004\001\001\001\012\001\001\001\003\001\011\001\014\001\035\000\
\001\001\014\001\012\001\002\001\002\001\014\001\042\000\012\001\
\255\255\255\255\255\255\047\000\255\255\017\001"

let yynames_const = "\
  NBUAP\000\
  NBVAP\000\
  MV\000\
  SCP\000\
  ET\000\
  LPAREN\000\
  RPAREN\000\
  HASH\000\
  VIRG\000\
  COLON\000\
  QMARK\000\
  EMARK\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  COMP\000\
  UNITE\000\
  DIR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 11 : Types.t_arbre) in
    let _3 = (Parsing.peek_val __caml_parser_env 9 : Types.t_arbre) in
    let _5 = (Parsing.peek_val __caml_parser_env 7 : Types.t_arbre) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : Types.t_arbre) in
    let _9 = (Parsing.peek_val __caml_parser_env 3 : Types.t_arbre) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : Types.t_arbre) in
    Obj.repr(
# 31 "parser.mly"
                                                                              ( (_1,_3,_5,_7,_9,_11) )
# 142 "parser.ml"
               : Types.t_foret))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.t_action) in
    Obj.repr(
# 34 "parser.mly"
                                          ( Leaf (_1) )
# 149 "parser.ml"
               : Types.t_arbre))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Types.t_arbre) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Types.t_predicat) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Types.t_arbre) in
    Obj.repr(
# 35 "parser.mly"
                                                              ( Node(_2,_4,_6) )
# 158 "parser.ml"
               : Types.t_arbre))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.t_action) in
    Obj.repr(
# 39 "parser.mly"
                                ( _2 )
# 165 "parser.ml"
               : Types.t_action))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Types.direction) in
    Obj.repr(
# 41 "parser.mly"
                                       ( Move (_3,_5) )
# 173 "parser.ml"
               : Types.t_action))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                       ( Set_city_prod (_3,_5) )
# 181 "parser.ml"
               : Types.t_action))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                                ( End_turn )
# 187 "parser.ml"
               : Types.t_action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.t_predicat) in
    Obj.repr(
# 46 "parser.mly"
                                                ( _2 )
# 194 "parser.ml"
               : Types.t_predicat))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Types.unites) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Types.comparateur) in
    Obj.repr(
# 48 "parser.mly"
                                                       ( Nb_unite_allie_proche (_3,_5,_7) )
# 203 "parser.ml"
               : Types.t_predicat))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Types.comparateur) in
    Obj.repr(
# 49 "parser.mly"
                                                       ( Nb_ville_allie_proche (_3,_5) )
# 211 "parser.ml"
               : Types.t_predicat))
(* Entry foret *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let foret (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.t_foret)
