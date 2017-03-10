(* Makes an 'archive' of several files, that can be converted from and into a single string. *)

(* Type of an archive.
 * Note that an archive contains all file contents. *)
type arch

type path = Lwtfile.path

(*** Build an archive ***)

val empty: arch

(* Appends the file content to an archive. 
 * path_in_arch: path that is associated to this file in the archive. 
 * path_in_arch must be absolute. The root dir (/) will be replaced by the destination dir when the archive is expanded to filesystem. 
 * perm: by default, the current perms of source file. *)
val append_file: arch -> source:path -> path_in_arch:path -> ?perms:int -> unit -> arch Lwt.t

(* Exports an archive to a string. *)
val dump: arch -> string

(* Builds an archive from a string obtained by 'dump'.
 * @raise Failure is the string is not a valid archive. *)
val arch_of_string: string -> arch



(*** Write files from an archive ***)

type file_desc =
  { (* Absolute path (the root dir / will be replaced by the destination dir. *)
    path: path ;

    perm: Unix.file_perm ;

    (* File size *)
    size: int }

(* Read an archive content *)
val content: arch -> file_desc list

(* Builds a file 
 * @raise Not_found if the file is not in the archive. *)
val file_content: arch -> path -> string

(* Write file to filesystem. 
 * new_path: destination file. By default, path /p is written to root/p 
 * perms: permissions for directories 
 * map_content: receives the old path (before new_path), and the current content. Returns the new content to be written into the destination file. 
 * Returns a pair : (/path in archive, path to the corresponding written file). *)
val write_file: root:path -> arch -> path -> ?new_path:path -> ?map_content:(path -> string -> string) -> Unix.file_perm -> unit -> (path * path) Lwt.t

(* Write all files to filesystem. 
 * new_paths map the absolute archive path to a filesystem path. 
 *           by default, new_paths map /p to root/p 
 * perms: permissions for directories 
 * Returns a list of pairs : (/path in archive, path to the written file). *)
val write_all: root:path -> arch -> ?new_paths:(path -> path) -> ?map_content:(path -> string -> string) -> Unix.file_perm -> unit -> (path * path) list Lwt.t


