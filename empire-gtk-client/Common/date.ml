open Unix

let to_string date =
  let tm = Unix.localtime date in

  Printf.sprintf "%d-%02d-%02d--%02dH%02dM%02ds"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

