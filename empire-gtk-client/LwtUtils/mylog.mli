(*** Log functions, lwt compliant ***)

(* Here are the main ideas:
 *   - Libraries directly invoke log functions to log their messages.
 *   - Libraries are not concerned about where the log messages finally go. 
 *   - Each library is expected to define its own section.
 *   - The main application globally configures log destinations, possibly by filtering on log levels and log sections.
 *
 * Hence, the destinations configuration is global for the whole application. It is done once.
 * For instance, all logs may go to the same file, or set of files, or to the same directory. *)


(* None of these functions may fail directly (presumably), except init_config. 
 * Errors will only be logged, sowehow, without failing the calling program. *)


type level = Lwt_log_core.level
	   = Debug | Info | Notice | Warning | Error | Fatal 

(* Given a section name, creates a (new) section. Returns the logging functions. 
 * Remember that sections logging levels are defined in several ways (see Lwt_log_core).
 * Messages under the section level are dropped!
 *
 * To configure global logging level, you may use:
 *    let () = Lwt_log_core.(add_rule "*" Debug)
 * *)
module MkSection (Section : sig val section_name: string end) :
sig
  (* We repeat the type here so that it suffices to open the newly created module. *)
  type level = Lwt_log_core.level
	       = Debug | Info | Notice | Warning | Error | Fatal 
  
  val section: Lwt_log_core.section
		   
  (* Logs a message in the current section. *)
  val log: ?exn:exn -> level -> string -> unit Lwt.t
						 
  (* Like log, but takes a format string *)
  val log_f: ?exn:exn -> level -> ('a, unit, string, unit Lwt.t) Pervasives.format4 -> 'a

  (* Asynchronous log (preserves logs ordering). Will be flushed asap. *)
  val alog: ?exn:exn -> level -> string -> unit
  val alog_f: ?exn:exn -> level -> ('a, unit, string, unit) Pervasives.format4 -> 'a
    
  end


(***  CONFIGURATION of LOG DESTINATIONS ***)

(* The functions below can be used to configure logging destinations, according to sections and level.
 * Remember also that sections may filter message according to their logging level (see above).
 *
 * If log functions (log, log_f) are invoked before the initial configuration, 
 * these log messages are both sent to a set of default loggers (syslog, stdout, /tmp/)
 * and also queued, and will be sent to the expected destinations once configuration is done. *)

(* Full file path. *)
type path = string

(* Possible log destinations *)
type log_destination =
  (* The directories are created if necessary. *)
  | Log_file of path * Unix.file_perm

  | Syslog of Lwt_log.syslog_facility

  | Stdout

  | Stderr

  | Log_fun of (string -> unit Lwt.t)

(* Some default good-looking template *)
val default_template: Lwt_log_core.template

type log_config =
    { (* Failsafe directory to log critical errors when everything else failed. 
       * (Creates the directory if necessary). Prefer an existing directory, though. *)
      failsafe_dir: path ;

      (* If directories need to be created, use these permissions. *)
      directories_perm: Unix.file_perm ;

      (* get_destination  section_name level: indicates where to put logs of this section and this level. *)
      get_destination: string -> level -> log_destination list ;

      (* Template to be used according to the given destination 
       * This ensures that a given destination uses a single template. *)
      get_template: log_destination -> Lwt_log_core.template ;
    }

(* Initial configuration. Shall be called only once.
 * @raise Failure if called twice. *)
val init_config: log_config -> unit Lwt.t


(*** Here is some typical usage:

  let () = Lwt_log_core.(add_rule "*" Info) in
  
  Mylog.(
    let dest = [ Stderr ; Log_file ("/tmp/mylogdir/mylogfile", 0o600)] in
			 
    init_config {
	failsafe_dir = "/tmp" ;
	directories_perm = 0o700 ;
	get_destination = (fun _ _ -> dest) ;
	get_template = (fun _ -> default_template) ;
      })

 *)

(* The above typical example can be invoked with this function: 
 * If appfile is true, a log file ~/.log/appname is created. 
 * string argument is appname. *)
val full_init: ?min_loglevel:level -> ?stdout:bool -> ?stderr:bool -> ?syslog:Lwt_log.syslog_facility ->
	       ?appfile:bool -> ?template:Lwt_log_core.template -> ?failsafe_dir:path -> string -> unit Lwt.t
