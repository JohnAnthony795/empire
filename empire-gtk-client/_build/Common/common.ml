
(***** CONVENIENT SMALL FUNCTIONS *****)

module Small =
  struct
    type path = string
		  
    let (//) = Filename.concat
    let (++) f g = fun x -> f (g x)
    let (--) f g = g ++ f
    let (-..) a b =
      let rec loop acu x =
        if x < a then acu
        else loop (x :: acu) (x-1)
      in
      loop [] b
			      
    let foi = float_of_int
    let iof = int_of_float
    let soi = string_of_int
		
    let cst x = fun _ -> x
    let id x = x
    let eq x = fun y -> y = x
    let afst = cst
    let asnd x y = y

    let xor a b = if a then not b else b

    (* Conveniently adds an elements to a list 
     *   Some x ^:: l  is x::l
     *   None ^:: l    is l *)
    let ( ^:: ) el l =
      match el with
      | None -> l
      | Some x -> x :: l
    
    (* Round to the nearest integer = "nearest int of float" *)
    let niof x =
      if x < 0.0 then - (iof (0.5 -. x))
      else iof (x +. 0.5)

    exception Msg_exception of string * exn
  end

let () = Printexc.register_printer
	   (function Small.Msg_exception (msg, e) -> Some ("exception (" ^ msg ^ ") " ^ Printexc.to_string e) | _ -> None)
    
let () = Random.self_init ()

(***** OPERATIONS ON OPTION TYPE *****)

let option_iter o f = match o with
  | None -> ()
  | Some s -> f s

let option_map o f = match o with
  | None -> None
  | Some s -> Some (f s)

let option_default o d = match o with
  | None -> d
  | Some x -> x

let option_get = function
  | None -> raise Not_found
  | Some x -> x

let option_apply default o f = match o with
  | None -> default
  | Some y -> f y


(***** OPERATIONS ON LISTS *****)

let myfold l a f      = List.fold_left f a l
let myfold2 l1 l2 a f = List.fold_left2 f a l1 l2
let myiter l f        = List.iter f l
let myiter2 l1 l2 f   = List.iter2 f l1 l2
let myiteri l f       = List.iteri f l

let mymap  l f     = List.map f l
let myrevmap l f   = List.rev_map f l
let mymap2 l1 l2 f = List.map2 f l1 l2
let myrevmap2 l1 l2 f  = List.rev_map2 f l1 l2

(* Maps and append *)
let rec revmapappend f acu = function
  | [] -> acu
  | x :: xs -> revmapappend f ((f x) :: acu) xs

(* Maps and filter. If map returns None, the element is discarded. If Some x, x is appended to the result. *)
let rec revmapfilter' f acu = function
  | [] -> acu
  | x :: xs ->
      begin match f x with
      | None -> revmapfilter' f acu xs
      | Some y -> revmapfilter' f (y :: acu) xs
      end

let revmapfilter l f = revmapfilter' f [] l 

(* Finds an element for which the function returns Some x. 
 * Returns None or Some x.*)
let rec mapfind f = function
  | [] -> None
  | x :: xs ->
      begin match f x with
      | None -> mapfind f xs
      | res -> res
      end

let rec ifold_aux f a index = function
  | [] -> a
  | x :: xs -> ifold_aux f (f index a x) (index + 1) xs

let ifold l a f = ifold_aux f a 0 l

(* Like &&, but for a list of items. *)
let rec and_list l f = match l with
  | [] -> true
  | x :: xs -> f x && and_list xs f

(* Like ||, but for a list of items. Equivalent to List.exists f l *)
let rec or_list l f = match l with
  | [] -> false
  | x :: xs -> f x || or_list xs f

(* Returns the index of an element in a list *)
let list_index_of ?(cmp=(==)) x lst = 

  let rec list_index' index = function
    | [] -> raise Not_found
    | elt :: others ->
	if cmp x elt then index
	else list_index' (index + 1) others
  in

  list_index' 0 lst

(* Returns the last element in a list *)
let rec list_last = function
  | [] -> failwith "list_last"
  | [x] -> x
  | _ :: xs -> list_last xs

(*** Arrays ***)

(* Returns the index of an element in an array *)
(* By default, use physical comparison to avoid expansive deep comparison. *)
let array_index_of ?(cmp=(==)) x ar = 

  let size = Array.length ar in

  let rec array_index' index =
    if index >= size then raise Not_found
    else if cmp ar.(index) x then index
    else array_index' (index + 1)
  in

  array_index' 0

let matrix_init nb_lines nb_cols f =
  Array.init nb_lines (fun line -> Array.init nb_cols (fun col -> f line col))

let matrix_fold mat acu f =
  fst (Array.fold_left
	 begin fun (acu, linenb) row ->
	   let (acu', _) = Array.fold_left (fun (acu, colnb) cell -> (f linenb colnb acu cell, colnb + 1)) (acu, 0) row in
	   (acu', linenb + 1)
	 end
	 (acu, 0) mat)

(* Removes duplicate anywhere in the list. *)
let uniq ?(eq=(=)) l =
  let rec go acu = function
    | [] -> acu
    | [x] -> x :: acu
    | x :: ((y :: _) as xs) ->
       if eq x y then go acu xs else go (x :: acu) xs
  in

  go [] (List.sort Pervasives.compare l)

(* Indicate if two list are disjoint. *)
let disjoint ?(eq=(=)) l1 l2 =
  let rec go = function
    | [] -> true
    | x :: xs ->
	if List.exists (fun y -> eq x y) l2 then false
	else go xs
  in
  go l1

(* Shuffles a list (returns an array) *)
let ashuffle ?(rand=Random.int) l =
  match l with
  | [] -> [||]
  | el :: _ ->
      let src = Array.of_list l in
      let len = Array.length src in

      (* Result *)
      let res = Array.make len el

      (* Bit array to indicate which cells are used. *)
      and used = Bitarray.create len in

      (* Find an unused cell, starting at position pos. 
       * count = number of full cells already considered. *)
      let rec find count pos =
	assert(count < len) ;
	if Bitarray.get used pos then find (count+1) ((pos + 1) mod len)
	else pos	  
      in
      
      (* Fills the result. *)
      for index = 0 to len - 1 do
	let pos = find 0 (rand len) in
	Bitarray.set used pos true ;
	res.(pos) <- src.(index) ;
      done ;

      res

(* Shuffles a list. *)
let shuffle l = Array.to_list (ashuffle l)
  

(***** OPERATIONS ON STRINGS *****)

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

(* Indicates if string b starts with string a, that is, a is a prefix of b. *)
let starts_with a b =
  let l = String.length a in
  (String.length b >= l) && (String.sub b 0 l = a)

let remove_start a b =
  assert (starts_with a b) ;
  let la = String.length a in
  String.sub b la (String.length b - la)

(* Indicates if string b ends with string a, that is, a is a suffix of b. *)
let ends_with a b =
  let la = String.length a
  and lb = String.length b in
  (lb >= la) && (String.sub b (lb - la) la = a)


(* Ad-hoc...
 * Efficiently remove spaces at begin, end, and multiple spaces in the middle of the string. *)
let rec eatable_spaces flag pos len s count =
  if pos >= len then count
  else match s.[pos] with
  | ' ' when flag || pos + 1 = len ->
      let delta =
	if pos + 1 = len && flag && pos > count then 2
	else 1
      in
      eatable_spaces true (pos+1) len s (count + delta)

  | c -> eatable_spaces (c = ' ') (pos+1) len s count

let rec eat_spaces_aux flag pos len s res reslen respos =
  if pos >= len then res
  else match Bytes.get s pos with
  | ' ' when flag || respos = reslen -> eat_spaces_aux true (pos+1) len s res reslen respos
  | c ->
      Bytes.set res respos c ;
      eat_spaces_aux (c = ' ') (pos+1) len s res reslen (respos+1)

let eat_spaces s =
  let len = String.length s in
  let eatable = eatable_spaces true 0 len s 0 in
  if eatable > 0 then eat_spaces_aux true 0 len s (Bytes.create (len - eatable)) (len - eatable) 0
  else s

(* Replace spaces by '\n' every n characters 
 * FIXME : should be UTF-8 compliant. *)
let wrap n s =
  let open Bytes in
  let s = of_string s in
  let compte = ref 0 in
  for i = 0 to length s - 1 do
    if !compte >= n && get s i = ' ' then set s i '\n' ;
    if get s i = '\n' then compte := 0 ;
    compte := !compte + 1 ;
  done ;
  Bytes.to_string s

	 
(* Current time *)
let current_time () =
  let open Unix in
  let time = gettimeofday () in
  let millis = int_of_float (fst (modf time) *. 1000.0) in
  let tm = localtime time in
  Printf.sprintf "%4d-%02d-%02d--%02d.%02d.%02d.%03d"
		 (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
		 tm.tm_hour tm.tm_min tm.tm_sec millis

(* Memoize once, whatever the argument. *)
let memonce f =
  let result = ref None in
  fun arg ->
  match !result with
  | Some r -> r
  | None ->
     let r = f arg in
     result := Some r ;
     r
       
(* Compute mean value *)
type mean =
  { mutable sum: float ;
    mutable nb: int }

let new_mean () = { sum = 0.0 ; nb = 0 }
    
let add_to_mean m x =
  m.sum <- m.sum +. x ;
  m.nb <- m.nb + 1 ;
  ()

let get_mean m = if m.nb > 0 then m.sum /. (float_of_int m.nb) else 0.0
							     

      
