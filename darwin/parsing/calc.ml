        (* File calc.ml a modifier ( nom + verif la bonne sortie*)
        let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            while true do
              let result = Parser.foret Lexer.token lexbuf in
                print_int 5; print_newline(); flush stdout
            done
          with Lexer.Eof ->
            exit 0

