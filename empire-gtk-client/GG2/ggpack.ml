open Gg


let hbox ?(space_left=false) ?(space_right=false) ?spacing ?packing () =
  let box = GPack.hbox ?spacing ?packing () in
  if space_left then ignore(GPack.hbox ~packing:(box#pack ~expand:true) ()) ;
  if space_right then ignore(GPack.hbox ~packing:(box#pack ~expand:true ~from:`END) ()) ;
  box
  
let notepack (nb:GPack.notebook) title widg = ignore (nb#append_page ~tab_label:(Gg.label ~justify:`CENTER title)#coerce widg)

       
type col_style =
  | Col of int
  | Nmax of int

class cols ?(style=Col 1) ?border_width ?bigpadding ?padding ?packing () =

  (* mainbox containing all columns,
   * and current box being filled. *)
  let (mainbox, curbox) = match style with
  | Col ncol ->
     if ncol = 1 then 
       let vbox = GPack.vbox ?border_width ?packing () in
       (vbox, vbox)

     else assert false (* Pas encore fait *)

  | Nmax nmax ->
     let hbox = GPack.hbox ?border_width ?packing () in
     let vbox = GPack.vbox ~packing:(hbox#pack ?padding:bigpadding) () in
     (hbox, vbox)
       
  in

  object(self)

    (* Number of elements in curbox. *)
    val mutable elt = 0
    val mutable curbox = curbox

    (* Curiously, annotating with 'a widg does not work. *)			   
    method add: 'a . (<coerce : GObj.widget ; ..> as 'a) -> unit =
      fun widg ->
      curbox#pack ?padding widg#coerce ;
      elt <- elt + 1 ;
      
      match style with
      | Col 1 -> ()
      | Col n -> assert false (* Not done yet *)
      | Nmax nmax ->
	 if elt >= nmax then 
	   begin
	     elt <- 0 ;
	     curbox <- GPack.vbox ~packing:(mainbox#pack ?padding:bigpadding) ()
	   end
	     
    method mainbox = mainbox
    method coerce = mainbox#coerce
  end
