open Common.Small

type result =
  { file: path }

type tester_result = OK | Wrong of string

type tester = path -> string -> tester_result Lwt.t

let lwtok = Lwt.return OK
let lwtwrong s = Lwt.return (Wrong s)
			    
let tester_file_exists ~msg path shortname =
  if Sys.file_exists path then lwtok else lwtwrong (msg path shortname)

let tester_suffix ?required_suffixes ?forbidden_suffixes ~msg path shortname =

  let ok_required = match required_suffixes with
    | None -> true
    | Some l -> List.exists (fun suf -> Common.ends_with suf path) l

  and ok_forbid = match forbidden_suffixes with
    | None -> true
    | Some l -> not (List.exists (fun suf -> Common.ends_with suf path) l)
  in
  
  if ok_required && ok_forbid then lwtok
  else lwtwrong (msg path shortname)

let tester_executable ~msg path shortname =	  
  lwt isok = Lwtfile.is_executable path in
  if isok then lwtok else lwtwrong (msg path shortname)

let tester_directory ?(is_dir=true) ~msg path shortname =
  lwt isok = Lwtfile.is_dir path in
  if (xor is_dir isok) then lwtwrong (msg path shortname) else lwtok

let rec apply_testers file short = function
  | [] -> Lwt.return OK
  | tst :: others ->
     begin match_lwt tst file short with
	   | OK -> apply_testers file short others
	   | (Wrong _) as r -> Lwt.return r
     end

let imgdir = Lwt.new_key ()
				   
let launch ~img ~button ~msg ~select testers =

  let chosen_file, chf = Lwt.wait () in
  let set_result v = try Lwt.wakeup chf v with _ -> () in (* Very improbable, but set_result could be called twice if the window is closed & button pressed. *)
  
  let button = Gg.to_utf button
  and msg = Gg.to_utf msg
  and select = Gg.to_utf select
  in

  (*** LAYOUT ***)
  let win = GWindow.window ~kind:`POPUP ~allow_grow: false ~allow_shrink: false ~resizable: false
			   ~type_hint: `SPLASHSCREEN ~position:`CENTER ~border_width: 8 ~show:false ()
  in
  win#misc#modify_bg [(`NORMAL, Ggcolor.rgb 217 220 229)] ;
  let connect_id = win#connect#destroy ~callback: (fun () -> set_result "") in
  
  let vbox = GPack.vbox ~spacing:0 ~border_width:0 ~packing:win#add () in
  let _ = Ggimage.from_file ~dir:imgdir img ~packing:vbox#add () in
		      
  let hbox = GPack.hbox ~spacing:0 ~border_width:0 ~packing:vbox#add () in

  let _ = Gg.label ~packing:(hbox#add) msg in
  let but = GButton.button ~label:button ~packing:(hbox#pack ~from:`END ~expand:false ~fill:false) () in

  (*** CHOOSE button ***)

  (* We return only when an acceptable file has been chosen (or it is canceled). *)
  let choose () =
    win#misc#disconnect connect_id ;
    win#destroy () ;
    let rec loop () =
      match GToolbox.select_file ~title:select () with
      | None -> set_result "" ; Lwt.return_unit
      | Some file ->
	 begin match_lwt apply_testers file (Filename.basename file) testers with
	       | OK -> set_result file ; Lwt.return_unit
	       | Wrong err ->
		  lwt () = Gg.popup_err ~markup:true err in
		  loop ()
	 end
    in
    (* Launch loop as an asynchronous thread. *)
    Lwt.async loop ;
    (* Printf.printf "Debug: splash.ml:: Exiting callback\n%!" ; *)
    ()
  in
  
  let _ = but#connect#clicked ~callback:choose in
 
  win#show () ;
  chosen_file

