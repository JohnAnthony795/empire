(* GUI *)

(* Returns when the GUI is closed. *)
val go: observe_only:bool -> Connect.intf -> unit Lwt.t
