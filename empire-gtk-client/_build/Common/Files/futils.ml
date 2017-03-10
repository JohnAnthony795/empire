open FileUtil

let show_size n = string_of_size ~fuzzy:true (B (Int64.of_int n))

let find_homedir () =
  try Sys.getenv "HOME" with _ -> "/tmp"
						     
