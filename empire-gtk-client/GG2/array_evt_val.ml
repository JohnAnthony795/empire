open Evt_val

type 'a array_evt =
  [ `Value_Changed of 'a array
  | `Cell of int * 'evt
  | `Insert of int * 'a
  | `Replace of int * 'a
  | `Delete of int
  ] constraint 'a = ('b, 'evt) #evt_val

(*** Helper functions on arrays. ***)

(* Delete a cell in an array (just shifts the rightmost cells). *)
let delete ar len index =
  assert (index < len) ;
  if index < len - 1 then Array.blit ar (index+1) ar index (len - 1 - index) ;

  if len = 1 then
    (* The array is now empty. We return a new empty array so that the GC can collect the elements in the previous array. *)
    [| |]
  else
    begin
      ar.(len - 1) <- ar.(0) ; (* The GC can collect the element we have removed. *)
      ar
    end

(* Inserts a cell in an array. Allocates a new array if necessary. *)
let insert ar len index el =
  assert (index <= len) ;
  let size = Array.length ar in
  assert (len <= size) ;
  
  if len = size then
    if len = 0 then Array.make 8 el
    else
      begin
	let nar = Array.make (2 * size + 8) ar.(0) in
	Array.blit ar 0 nar 0 index ;
	Array.blit ar index nar (index+1) (len - index) ;
	nar.(index) <- el ;
	nar
      end

  else
    begin
      Array.blit ar index ar (index+1) (len - index) ;
      ar.(index) <- el ;
      ar
    end
				

class ['a] array_val ?(name="array_val") ?cb ?target ~init () =
  let init_len = Array.length init in
  
  object(self)
    inherit ['a array, 'a array_evt] evt_val ~name ?cb ?target ()
				     
    (* Storage array *)
    val mutable ar = init

    (* How many cells are actually used. *)
    val mutable len = init_len

    (*** Listen to cells (listen to 'active' cells...) ***)

    (* Listeners are stored in this array *)
    val mutable lar = [| |]

    initializer
      lar <- Array.init init_len (fun i -> self#mk_listener i)

    (* A listener is attached to an event_value (not to a cell). *)
    method private mk_listener i =
      let current_cell = ref i in
      let listener ev = self#send (`Cell (!current_cell, ev)) in
      Events.to_cons ar.(i)#prod listener ;
      (current_cell, listener)
	
    method length = len
		      
    method vget i = ar.(i)#value
    method vset i v = ar.(i)#set_value v (* The value emits the event itself. *)
			
    method get i = ar.(i)

    (* Sets an event-val in the array. Updates the listener. *)		   
    method set i v =
      let (_, cb) = lar.(i) in
      Events.remove_cb ~check:true ar.(i)#prod cb ;
      ar.(i) <- v ;
      Events.to_cons v#prod cb ;
      self#send (`Replace(i,v))
		
    method value = Array.sub ar 0 len
    method varray = Array.init len (fun i -> ar.(i)#value)

    method insert i v =
      if not (0 <= i && i <= len) then invalid_arg ("Array_evt.insert " ^ string_of_int i) ;
      ar <- insert ar len i v ;
      lar <- insert lar len i (self#mk_listener i) ;
      len <- len + 1 ;
      
      (* Shifts following listeners *)
      for j = i + 1 to len - 1 do
	incr (fst lar.(j))
      done ;

      self#send (`Insert (i,v)) ;
      ()

    method append v = self#insert len v
		     
    method delete i =
      if not (0 <= i && i < len) then invalid_arg ("Array_evt.delete " ^ string_of_int i) ;

      let (_, cb) = lar.(i) in
      Events.remove_cb ~check:true ar.(i)#prod cb ;
		       
      ar <- delete ar len i ;
      lar <- delete lar len i ;

      len <- len - 1 ;

      (* Shifts following listeners *)
      for j = i to len - 1 do
	decr (fst lar.(j))
      done ;

      self#send (`Delete i) ;
      ()
		     
    method apply_event = function
      | `Value_Changed nar -> ar <- nar ; len <- Array.length nar
      | `Cell (i, ev) -> ar.(i)#apply_event ev
      | `Insert (i, v) -> self#insert i v
      | `Replace (i, v) -> self#set i v
      | `Delete i -> self#delete i
				 
    method fold: 'c . 'c -> ('c -> int -> 'a -> 'c) -> 'c = fun acu f ->
      let rec aux acu i =
	if i = len then acu
	else aux (f acu i ar.(i)) (i+1)
      in
      aux acu 0

    method vfold: 'c . 'c -> ('c -> int -> 'b -> 'c) -> 'c = fun acu f ->
      let rec aux acu i =
	if i = len then acu
	else aux (f acu i ar.(i)#value) (i+1)
      in
      aux acu 0

  end

