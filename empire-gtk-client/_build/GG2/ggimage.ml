open Common.Small

let defaultdir = Lwt.new_key ()

type pixb = GdkPixbuf.pixbuf
			     
module GP = GdkPixbuf
			     
let find_imgpath = Lwtfile.find_path ~defaultdir ~undefined_msg:"Ggimage: undefined default directory. Use defaultdir key to define it." ()

(* Rewrite... use pixbuf_from_file instead *)
let from_file ?dir path ~packing () =
  let img_path = find_imgpath ?dir path in
  GMisc.image ~file:img_path ~xpad:0 ~ypad:0 ~packing ()

let mk_pixbuf ?resized real_path =
  match resized with
  | None -> GP.from_file real_path
  | Some (width, height) -> GP.from_file_at_size real_path ~width ~height
	      
let all_pixbufs = Assoc.create ~size:40 ~init:(fun (path, resized) -> mk_pixbuf ?resized path) ()

			       
let pixbuf_from_file ?(with_alpha=false) ?dir ?(share=true) ?resized path =
  let img_path = find_imgpath ?dir path in
  let pixb =
    if share then Assoc.get all_pixbufs (img_path, resized)
    else mk_pixbuf ?resized img_path
  in
  if with_alpha && not (GP.get_has_alpha pixb) then GP.add_alpha pixb else pixb
		 
let apply_alpha ?(combine=fun _ a -> a) pixb1 pixb2 =
  (* Check compatibility *)
  let check_eq name f =
    let v1 = f pixb1
    and v2 = f pixb2 in
    if v1 = v2 then v1
    else failwith ("Ggimage.apply_alpha: both pixbufs do not have the same " ^ name)
  in
  
  let channels = check_eq "n_channels" GP.get_n_channels
  and has_alpha = check_eq "has_alpha" GP.get_has_alpha
  and width = check_eq "width" GP.get_width
  and height = check_eq "height" GP.get_height
  and bits = check_eq "bits-per-sample" GP.get_bits_per_sample
  and rowstride = check_eq "rowstride" GP.get_rowstride
  and _ = check_eq "region-length" (fun p -> Gpointer.length (GP.get_pixels p)) in

  assert has_alpha ;
  assert (bits = 8) ; (* 8 bits per sample / not implemented otherwise. *)
  assert (channels = 4) ; (* RGBA expected. *)

  let pointer1 = GP.get_pixels pixb1
  and pointer2 = GP.get_pixels pixb2 in

  let delta_bytes = channels in (* channels * bits / 8) *)

  (* Delta to add at the end of each row (possible padding). *)
  let delta = rowstride - (width * delta_bytes) in
  assert (delta >= 0) ; (* Negative padding ? *)

  (* Alpha channel => start at pos = 3. *)
  let pos = ref 3 in
  for y = 1 to height do
    for x = 1 to width do
      let alpha1 = Gpointer.get_byte pointer1 ~pos:!pos
      and alpha2 = Gpointer.get_byte pointer2 ~pos:!pos
      in
      Gpointer.set_byte pointer1 ~pos:!pos (combine alpha1 alpha2) ;
      pos := !pos + delta_bytes ;
    done ;
    pos := !pos + delta ;
  done ;
  ()

    
let set_alpha pixb alpha =
  let channels = GP.get_n_channels pixb
  and has_alpha = GP.get_has_alpha pixb
  and width = GP.get_width pixb
  and height = GP.get_height pixb
  and bits = GP.get_bits_per_sample pixb
  and rowstride = GP.get_rowstride pixb in

  assert has_alpha ;
  assert (bits = 8) ; (* 8 bits per sample / not implemented otherwise. *)
  assert (channels = 4) ; (* RGBA expected. *)

  let pointer = GP.get_pixels pixb in

  let delta_bytes = channels in (* channels * bits / 8) *)

  (* Delta to add at the end of each row (possible padding). *)
  let delta = rowstride - (width * delta_bytes) in
  assert (delta >= 0) ; (* Negative padding ? *)

  (* Alpha channel => start at pos = 3. *)
  let pos = ref 3 in
  for y = 1 to height do
    for x = 1 to width do
      Gpointer.set_byte pointer ~pos:!pos alpha ;
      pos := !pos + delta_bytes ;
    done ;
    pos := !pos + delta ;
  done ;
  ()
  
type which_image =
  | Clear
  | File of path
  | Stock of GtkStock.id
  | Sized_stock of GtkStock.id * Gtk.Tags.icon_size

type opixb =
  | Pix_Clear
  | Pixb of GdkPixbuf.pixbuf

let defaulticon = Lwt.new_key ()
	      
let get_opixb ?(dir=defaulticon) (widget:#GObj.widget) = function
  | Clear -> Pix_Clear
  | File f -> Pixb (pixbuf_from_file ~dir ~share:true f)
  | Stock id -> Pixb (widget#misc#render_icon ~size:`BUTTON id)
  | Sized_stock (id, size) -> Pixb (widget#misc#render_icon ~size id)

let set_opixb image = function
  | Pix_Clear -> image#clear ()
  | Pixb p -> image#set_pixbuf p

let image ?dir ?packing which =
  let img = GMisc.image ~xpad:0 ~ypad:0 ?packing () in
  set_opixb img (get_opixb ?dir img which) ;
  img
