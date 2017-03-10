
let rgb r g b = `RGB (255*r, 255*g, 255*b)

let mask = 0xff
let rgba r g b a = Int32.(logor (shift_left (of_int (r land mask)) 24) (of_int (((g land mask) lsl 16) lor ((b land mask) lsl 8) lor (a land mask))))


let cmap = lazy (Gdk.Color.get_system_colormap ())

let make spec =
  let colormap = Lazy.force cmap in
  Gdk.Color.alloc ~colormap spec

let mk_color nm = make (`NAME nm)
let mk_rgb r g b = make (rgb r g b)
		  
let white = lazy (make `WHITE)
let black = lazy (make `BLACK)
