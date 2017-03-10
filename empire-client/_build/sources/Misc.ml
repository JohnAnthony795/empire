let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n::acc) in
  aux j [] ;;

let str_start str len =
  String.sub str 0 len ;;

let str_end str offset =
  String.sub str offset (String.length str - offset) ;;

let split delim str =
  let rec aux str toks =
    if String.contains str delim then begin
        let i = String.index str delim in
        aux (str_end str (i + 1)) (str_start str i :: toks)
      end else
    if String.length str = 0 then List.rev toks else
    List.rev (str :: toks) in
  aux str [] ;;

let findi l f =
  let rec walk i = function
    | x :: _ when f x -> i
    | _ :: v -> walk (i + 1) v
    | _ -> failwith "value not found" in
  walk 0 l ;;
