open Common.Small
open Common
open Events

let check_opts cb target =
  match (cb, target) with
  | Some _, Some _ -> failwith "Evt_val: cannot specify both a target and a cb."
  | _ -> ()

class virtual ['a, 'evt] evt_val ~name ?cb ?target ?setter ?gtk () =
	let () = check_opts cb target in
	let (prod, send) = Events.new_prod ~name target in
	
	object(self)
	  initializer
	    option_iter gtk (fun reg -> ignore(reg ~callback:(fun () -> self#vchanged))) ;
	    option_iter cb (Events.to_cons prod) ;
	    ()
	      
	  method prod = (prod:'evt Events.prod)
	  method send = send
	  method vchanged = send (`Value_Changed self#value)
	  method set_value x = self#apply_event (`Value_Changed x)
			      
	  method virtual value: 'a
	  method virtual apply_event: 'evt -> unit

	  initializer oto_cons setter self#set_value
	end

(* Do not expose prod, because vprod could get (unexpected) values from outside this object. *)	  
class ['a] simple_val ~name ?cb ?target ?gtk ?setter ~fset ~fget () =
  let () = check_opts cb target in

  (* vprod should be built only once, but it requires self#prod, which is now known yet. *)
  let memo_vprod = memonce (fun self -> Events.map ?target self#prod (fun (`Value_Changed x) -> x)) in
  
  object(self)
    inherit ['a, [`Value_Changed of 'a]] evt_val ~name ?setter ?gtk ()
    method apply_event (`Value_Changed x) = fset x ; self#vchanged
    method value = fget ()
    method vprod = memo_vprod self
			      
    initializer
      option_iter cb (Events.to_cons self#vprod) ;
      self#vchanged (* Send initial signal *)
  end

let cst_val v = new simple_val ~name:"cst_val" ~fset:(fun _ -> ()) ~fget:(fun () -> v) ()
	  
class adjfloat ?(name="adjfloat") ?cb ?target ?setter ~lower ~upper ~step_incr ~page ~init () =
  let adjustment = GData.adjustment ~value:init ~lower ~upper ~step_incr ~page_incr:page ~page_size:0.0 () in
  
  object
    inherit [float] simple_val ~name ?cb ?target ~gtk:adjustment#connect#value_changed ?setter ~fset:adjustment#set_value ~fget:(fun () -> adjustment#value) ()
    method adjustment = adjustment
  end

class adjint ?(name="adjint") ?cb ?target ?setter ~lower ~upper ~step_incr ~page ~init () =
  let myfloat = new adjfloat ~name:("adjfloat-in-" ^ name)
		    ~lower:(foi lower) ~upper:(foi upper) ~step_incr:(foi step_incr) ~page:(foi page) ~init:(foi init) ()
  in
  
  object(self)
    inherit [int] simple_val ~name ?target ?setter ~fset:(fun x -> myfloat#set_value (foi x)) ~fget:(fun () -> iof myfloat#value) ()
    method vchanged = myfloat#vchanged
    method adjustment = myfloat#adjustment

    initializer
      Events.(set_loop myfloat#prod (fixloop ())) ;
      
      (* May be iof x can get wrong because of rounding errors. e.g. adding x to y gives (x+y-1)? *)
      let _ = Events.map ~target:self#prod myfloat#prod (fun (`Value_Changed x) -> `Value_Changed (iof x)) in
      option_iter cb (Events.to_cons self#vprod)
  end

class ['a] ref_val ?(name="refval") ?cb ?target ?setter ~init () =
  let buffer = ref init in
  object
    inherit ['a] simple_val ~name ?cb ?target ?setter ~fset:(fun x -> buffer := x) ~fget:(fun () -> !buffer) ()
  end

let vref ?cb ?target ?setter x = new ref_val ?cb ?target ?setter ~init:x ()
		 
