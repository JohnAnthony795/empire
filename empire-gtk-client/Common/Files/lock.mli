
(* Acquire a distributed lock, using a distributed file system such as NFS.
 * Soundness is not guaranteed (since it depends on the underlying file system). *)

(* Type of a lock. *)
type t

(* Result of lock acquisition *)
type result =
  (* OK, the given lock is acquired *)
  | OK of t

  (* Nope, the lock is already owned by the given owner. *)
  | Failed of owner_info

and owner_info =
    { owner_name  : string ;
      owner_login : string ;
      owner_ip    : string ;
      acquisition_date : float ;
      dir         : string ;
      lockname    : string }

(* Acquire the lock identified by the given name.
 * The lockname must be compatible with a file name (avoid special characters).
 * Ownername is a string identifying the owner. It does not need to be unique.
 * The lock file is created in the given directory, which is supposed to be shared between different
 * machines (by a NFS file system for instance). *)
val acquire : dir:string -> lockname:string -> ownername:string -> result

val release : t -> unit


(* Forces deletion of an existing lock. Of course, it is unsafe. *)
val unsafe_deletion : owner_info -> unit





