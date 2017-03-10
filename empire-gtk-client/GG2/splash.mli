open Common.Small
       
(*** Launch a splash screen. Ask to choose a file. ***)

type result =
  { file: path }

(* Wrong msg: msg may contain (gtk-compatible) markup. *)
type tester_result = OK | Wrong of string

(* Function to test the chosen file. 
 * the string is the file's shortname *)
type tester = path -> string -> tester_result Lwt.t

(* A tester which checks that the file exists. 
 * The message function will be applied to path and shortname. *)
val tester_file_exists: msg:(path -> string -> string) -> tester

(* If required_suffix is specified, one suffix of the list must be found.
 * If one suffix of forbidden_suffix matches, it is considered an error.
 *)							    
val tester_suffix: ?required_suffixes:string list -> ?forbidden_suffixes:string list -> msg:(path -> string -> string) -> tester

val tester_executable: msg:(path -> string -> string) -> tester

(* Tests if the selected file is a directory (or not a directory if is_dir is false) *)
val tester_directory: ?is_dir:bool -> msg:(path -> string -> string) -> tester						    
															    
(* img: splash image file. Loaded using the Ggimage module and the Splash.imgdir key (if defined), or the default Ggimage dir.
 * button: text on the load button
 * msg: message to be displayed 
 * select: title when selecting the file. 
 * tester: function which tests the chosen file. 
 * 
 * Returns "" if action is canceled. *)
val launch: img:path -> button:string -> msg:string -> select:string -> tester list -> path Lwt.t

(* Lwt key used to define the splash image directory. 
 * If undefined or if the path is empty, uses the Ggimage default. *)
val imgdir: path Lwt.key



