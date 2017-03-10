
module GP = GdkPixbuf
	      
let run () =
  let (finished, finish) = Lwtplus.fwait () in

  let window = GWindow.window ~kind:`TOPLEVEL ~title:(Gg.to_utf "Empire") ~width:800 ~height:680 ~border_width:2 ~show:true () in
  
  let _ = window#connect#destroy ~callback:finish in

  (* Main content *)
  let vbox = GPack.vbox ~packing:window#add ~homogeneous:false () in

  let img = Ggimage.pixbuf_from_file "TILES/terrains/snow1.png" in
  let mask = Ggimage.pixbuf_from_file "TILES/fog/test.png" in
  
  Printf.printf "n_channels = %d  %d\n%!" (GP.get_n_channels img) (GP.get_n_channels mask) ;
  Printf.printf "has_alpha = %b  %b\n%!" (GP.get_has_alpha img) (GP.get_has_alpha mask) ;
  Printf.printf "width = %d  %d\n%!" (GP.get_width img) (GP.get_width mask) ;
  Printf.printf "height = %d  %d\n%!" (GP.get_height img) (GP.get_height mask) ;
  Printf.printf "bits_per_sample = %d  %d\n%!" (GP.get_bits_per_sample img) (GP.get_bits_per_sample mask) ;
  Printf.printf "rowstride = %d  %d\n%!" (GP.get_rowstride img) (GP.get_rowstride mask) ;
  Printf.printf "region length = %d  %d\n%!" (Gpointer.length (GP.get_pixels img)) (Gpointer.length (GP.get_pixels mask)) ;
  
  let show_pixb pixbuf =
    let _ = GMisc.image ~pixbuf:(GdkPixbuf.copy pixbuf) ~packing:vbox#add () in
    ()
  in

  show_pixb img ;
  show_pixb mask ;

  show_pixb img ;
  Ggimage.apply_alpha ~combine:min img mask ;
  show_pixb img ;
  show_pixb img ;

  finished
    

open Lwtplus.Setkeys

let () =

  let main_dir = "./Images" in
  Ggimage.defaultdir %= main_dir

  (* Configures LOG & launch lwt. *)			  
  ===> Lwtplus.launch ~configure_log:true ~appname:"Test_cairo" ~run ()

		      (*
		      Cairo

mask_surface cr surface x y: a drawing operator that paints the current source using the alpha channel of surface as a mask. (Opaque areas of surface are painted with the source, transparent areas are not painted.)
x : X coordinate at which to place the origin of surface.
		       *)
