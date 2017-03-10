type path = Lwtfile.path

type file_desc =
  { path: path ;
    perm: Unix.file_perm ;
    size: int }

type file_content = string

type entry = file_desc * file_content

type arch = entry list

let empty = []

open FilePath
open Lwt_unix

let append_file arch ~source ~path_in_arch ?perms () =
  (* New path (path_in_arch) in canonical form. *)
  let new_path = reduce ~no_symlink:true path_in_arch in
  
  let%lwt () =
    if FilePath.is_relative new_path then Lwt.fail_with ("append_file: path_in_arch=" ^ path_in_arch ^ " must be an absolute path.")
    else if List.exists (fun (desc, _) -> desc.path = new_path) arch then Lwt.fail_with ("append_file: " ^ new_path ^ " is already in the archive.")
    else Lwt.return_unit
  in

  (* Source file *)
  let%lwt stat = Lwt_unix.stat source in
  let size = stat.st_size in
  let file_content = Bytes.create size in

  let%lwt fd = openfile source [ O_RDONLY ] 0 in
  let%lwt bytes_read = Lwt_unix.read fd file_content 0 size in
  close fd >>
  
  assert%lwt (bytes_read = size) >>
  
  let filedesc =
    { path = new_path ;
      perm = (match perms with None -> stat.st_perm | Some p -> p) ;
      size }
  in

  Lwt.return ((filedesc, Bytes.unsafe_to_string file_content) :: arch)

(* Format of the final string structure :
 *    8 means 8 bytes (64 bits)
 *
 *  [header-length]8 [header] [file1] [file2] [...]
 *  [header] = ( [path-length]8 [file-offset]8 [file-length]8 [perm]8 [path] )*
 *  file-offset = 0 for file1
 *              = file-length1 for file2
 *              = ...
 *)

let bytes_of_desc file_offset desc =
  let len = String.length desc.path in
  let bytes = Bytes.create (32 + len) in
  Bytehelp.dump_int bytes 0 len ;
  Bytehelp.dump_int bytes 8 file_offset ;
  Bytehelp.dump_int bytes 16 desc.size ;
  Bytehelp.dump_int bytes 24 desc.perm ;
  Bytes.blit_string desc.path 0 bytes 32 len ;
  (bytes, file_offset + desc.size)

let rec mk_header file_offset = function
  | [] -> Bytes.empty
  | (desc, _) :: rest ->
    let (buf, new_offset) = bytes_of_desc file_offset desc in
    Bytes.cat buf (mk_header new_offset rest)

let dump arch =
  let header = mk_header 0 arch in
  let full_header = Bytes.extend header 8 0 in
  Bytehelp.dump_int full_header 0 (Bytes.length header) ;
  String.concat "" (List.map Bytes.unsafe_to_string (full_header :: (List.map (fun (_, content) -> content) arch)))

let invalid = "The given string is an invalid archive."

let arch_of_string s =
  if String.length s < 8 then failwith invalid ;

  (* Get header *)
  let header_size = Bytehelp.read_int s 0 in
  let startpos = header_size + 8 in

  let rec loop acu offset =
    if offset >= startpos then acu
    else
      let path_length = Bytehelp.read_int s offset
      and file_offset = Bytehelp.read_int s (offset+8)
      and file_length = Bytehelp.read_int s (offset+16)
      and perm = Bytehelp.read_int s (offset+24)
      in
      let path = String.sub s (offset+32) path_length in
      let desc = { path ; perm ; size = file_length } in
      let content = String.sub s (startpos + file_offset) file_length in
      loop ((desc, content):: acu) (offset+32+path_length)
  in
  loop [] 8

let content arch = List.map fst arch

let file_content arch path =
  let (_, ct) = List.find (fun (desc, _) -> desc.path = path) arch in
  ct

let raw_write oldpath ?(map_content=fun _ s -> s) path content dperms perm =

  let content = map_content oldpath content in
  
  (* Ensure the directory exists *)
  let%lwt () = Lwtfile.mkdir ~parents:true (FilePath.dirname path) dperms in

  let%lwt fd = Lwt_unix.openfile path [ O_WRONLY ; O_CREAT ; O_TRUNC ] perm in
  Lwtfile.mywrite fd content 0 (String.length content) >>
  Lwt_unix.close fd >>
  Lwt.return (oldpath, path)

let concat_root root p = FilePath.reparent "/" root p

let write_file ~root arch path ?(new_path=concat_root root path) ?map_content dperms () =
  let (desc, ct) = List.find (fun (desc, _) -> desc.path = path) arch in
  raw_write ?map_content desc.path new_path ct dperms desc.perm

let write_all ~root arch ?(new_paths=concat_root root) ?map_content dperms () =
  Lwt_list.map_p (fun (desc, content) -> raw_write ?map_content desc.path (new_paths desc.path) content dperms desc.perm) arch
  
