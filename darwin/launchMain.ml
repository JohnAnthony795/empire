let () = print_endline "START launcher ref";
	let _ = Main.main (int_of_string Sys.argv.(0)) in
	()

