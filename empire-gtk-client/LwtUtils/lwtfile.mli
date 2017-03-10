(* File utils, lwt compliant! *)

type path = FilePath.filename

exception MkDirFailure of string

(* Indicates if the given path is a directory. 
 * Cannot fail, returns false by default. *)
val is_dir: path -> bool Lwt.t

(* Indicates if the given path exists. *)
val exists: path -> bool Lwt.t

val is_executable: path -> bool Lwt.t

(* Creates the given directory. Creates the parent directories if necessary. *)
val mkdir: ?parents:bool -> path -> Unix.file_perm -> unit Lwt.t

(* Creates a text file with the given content. *)
val mk_text_file: path:path -> string -> unit Lwt.t

(* Writes _all_ the expected data. *)
val mywrite: Lwt_unix.file_descr -> bytes -> int -> int -> unit Lwt.t

(*** Look for files in a given global-configured dir. ***)

exception File_does_not_exist of path
					      
(* The function returned by find_path gets a relative or absolute path ;
 * If the path is relative, it is searched in dir, if defined, or defaultdir otherwise. 
 * undefined_msg is the error message put into a Failure exception if both keys are undefined. 
 * @raise File_does_not_exist if check_exists is true and the file is not here. *)
val find_path: defaultdir:path Lwt.key -> undefined_msg:string -> unit
	       -> ?check_exists:bool -> ?dir:path Lwt.key -> path -> path


(***  ITERATORS  ***)

(* Opens a file and iter a function over every line. *)

(* rm_empty_lines: should we ignore empty lines? 
 * ignore_regexp: if one regexp matches the line, the line is ignored. 
 * cb function is invoked like this:  cb line_number line acu *)
val fold_file : file:path -> ?rm_empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val iter_file : file:path -> ?rm_empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> unit Lwt.t) -> unit Lwt.t
