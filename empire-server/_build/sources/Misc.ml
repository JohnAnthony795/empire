Random.init 400 ;;
(* TODO: Random.self_init () ;; *)

let pick_random l = List.nth l (Random.int (List.length l)) ;;

let intersection la lb =
  let rec walk res = function
  | e :: es -> if List.mem e lb then walk (e :: res) es else walk res es
  | [] -> res in
  walk [] la ;;

let map_matrix m f = Array.map (Array.map f ) m ;;

let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j) ;
  a.(j) <- t ;;

let shuffle a = Array.iteri (fun i _ -> swap a i (Random.int (i + 1))) a ;;

let product l r =
  let g acc a =
    let f acc x = (a, x)::acc in
    List.fold_left f acc r in
  List.rev (List.fold_left g [] l) ;;

let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n::acc) in
  aux j [] ;;

let rand_select l n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n - 1) t in
  let extract_rand l len = extract [] (Random.int len) l in
  let rec aux n acc l len =
    if n = 0 then acc else
      let picked, rest = extract_rand l len in
      aux (n - 1) (picked :: acc) rest (len - 1) in
  let len = List.length l in
  aux (min n len) [] l len ;;

let str_start str len = String.sub str 0 len ;;

let str_end str offset = String.sub str offset (String.length str - offset) ;;

let split str delim =
  let rec aux str toks =
    if String.contains str delim then begin
        let i = String.index str delim in
        aux (str_end str (i + 1)) (str_start str i :: toks)
      end else
    if String.length str = 0 then List.rev toks else
    List.rev (str :: toks) in
  aux str [] ;;

module Set = struct

  type 'a t =
    { mutable content : 'a array
    ; mutable size : int
    } ;;

  let of_array a = { content = a ; size = 0 } ;;

  let to_list a =
    let rec f i r = if i = a.size then r else f (i + 1) (a.content.(i) :: r) in
    f 0 [] ;;

  let clear s = s.size <- 0 ;;

  let empty s = s.size = 0 ;;

  let set s i v =
    if i < 0 || s.size <= i then failwith "invalid index for Set" else
    s.content.(i) <- v ;;

  let get s i =
    if i < 0 || s.size <= i then failwith "invalid index for Set" else
    s.content.(i) ;;

  let iter f s = Array.iter f s.content

  let del s i =
    if i < 0 || s.size <= i then failwith "invalid index for Set" else
    s.content.(i) <- s.content.(s.size - 1) ;
    s.size <- s.size - 1 ;;

  let del_value s v =
    let rec f i =
      if i < s.size && compare s.content.(i) v = 0 then del s i else
      if i < s.size then f (i + 1) in
    f 0 ;;

  let add s v =
    if Array.length s.content = s.size
    then s.content <- Array.append s.content (Array.make (s.size + 1) v)
    else s.content.(s.size) <- v ;
    s.size <- s.size + 1

  let size s = s.size ;;

end ;;

(* Builds a string from a list of items *)
let sep map sp ?(last_sep=sp) l =
  let rec loop acu = function
    | [] -> ""
    | [x] ->
      (* Singleton *)
      if acu = "" then map x
      else
	(* Last element *)
	acu ^ last_sep ^ (map x)
		  
    | x :: xs -> loop (if acu = "" then map x else acu ^ sp ^ (map x)) xs
  in
  loop "" l

