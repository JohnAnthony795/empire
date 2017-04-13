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

val foret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.t_foret
