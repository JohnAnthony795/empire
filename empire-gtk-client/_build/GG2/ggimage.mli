(*** Load images from predefined directories  ***)
open Gg
open Common.Small       

type pixb = GdkPixbuf.pixbuf
       
(* Lwt key used to define the default image directory. *)
val defaultdir: path Lwt.key

val find_imgpath: ?check_exists:bool -> ?dir:path Lwt.key -> path -> path

val from_file: ?dir:path Lwt.key -> path -> packing:pack -> unit -> GMisc.image

(* share: by default the same pixbuf is returned if the same file is used. 
 * with_alpha: adds an alpha channel if required. *)
val pixbuf_from_file: ?with_alpha:bool -> ?dir:path Lwt.key -> ?share:bool -> ?resized:(int*int) -> path -> pixb


(* Combine alpha channels of two pixbufs. 
 * The combine function receives both alpha channels (a byte). By default, it is MIN. 
 * The first pixbuf is modified. 
 * Beware that, when using a custom combine function, invisible parts of the first pixbuf may become visible. 
 *
 * Constraint: both pixbufs must have the same size, same depth, etc., and 8x4-bits RGBA encoding. *)
val apply_alpha: ?combine:(int -> int -> int) -> pixb -> pixb -> unit

(* Set alpha uniformly. *)								   
val set_alpha: pixb -> int -> unit

(* Lwt key used to define the default icon directory. *)
val defaulticon: path Lwt.key


(*** Icons, images for buttons, etc. ***)
type which_image =
  | Clear
  | File of path
  | Stock of GtkStock.id
  | Sized_stock of GtkStock.id * Gtk.Tags.icon_size

type opixb =
  | Pix_Clear
  | Pixb of GdkPixbuf.pixbuf

val get_opixb: ?dir:path Lwt.key -> #GObj.widget -> which_image -> opixb

val set_opixb: GMisc.image -> opixb -> unit

val image: ?dir:path Lwt.key -> ?packing:pack -> which_image -> GMisc.image
