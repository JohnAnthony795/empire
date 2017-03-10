type level = Lwt_log_core.level
= Debug | Info | Notice | Warning | Error | Fatal 

type path = string

type log_destination =
  | Log_file of string * Unix.file_perm
  | Syslog of Lwt_log.syslog_facility
  | Stdout
  | Stderr
  | Log_fun of (string -> unit Lwt.t)

type log_config =
  { failsafe_dir: path ;
    directories_perm: Unix.file_perm ;
    get_destination: string -> level -> log_destination list ;
    get_template: log_destination -> Lwt_log_core.template ;
  }

let default_template = "$(date).$(milliseconds) [pid $(pid)] $(section) [$(level)] $(message)"

let default_logfile = Printf.sprintf "/tmp/mylog-default-%d" (Unix.getpid ())

(* Destinations used before configuration is done. *)
let default_destinations = Lwt.return
    [ Stdout ; 
      Syslog `User ; 
      Log_file (default_logfile, 0o644) ]

(* mylog section, used to report logging errors. *)
let mylog_section = Lwt_log_core.Section.make "mylog"

let failsafe_dir = ref "/tmp/"

type configuration_status = Not_configured | Being_configured | Configured of log_config

let config_status = ref Not_configured

(* Queue of logs that were written before configuration. *)
let queue = Queue.create ()

let dest_tostring = function
  | Log_file (f, _) -> "file " ^ f
  | Syslog _ -> "syslog"
  | Stdout -> "stdout"
  | Stderr -> "stderr"
  | Log_fun _ -> "(log function)"

(*** Now, beware of error handling! ***)

let failsafe_name = "mylog-errors-" ^ (Common.current_time ())

let log_failsafe ?exn ~section level msg =
  try
    let logline = Printf.sprintf "*** %s: %s %s" (Lwt_log_core.string_of_level level) msg (Common.option_apply "" exn (fun e -> Printexc.to_string e)) in
    Printf.printf "%s\n%!" logline ;

    (* No lwt-calls here. We want to log immediately. 
     * We log to a unique output file (to avoid creating zillions of failsafe files. *)
    let outfile = Filename.concat !failsafe_dir failsafe_name in
    let chout = open_out_gen [Open_append ; Open_creat] 0o644 outfile in
    Printf.fprintf chout "%s\n%!" logline ;
    close_out chout ;

    Lwt.return_unit

  with e ->
    (* Total failure. What can we do? *)
    Printf.printf "*** MYLOG FAILURE: logging is just impossible on this damn machine: %s\n%!" (Printexc.to_string e) ;
    Lwt.return_unit

let report_error_1 ?exn msg = log_failsafe ?exn ~section:mylog_section Error msg

(* May raise exceptions. *)
let create_dest_logger dest =
  let (template, dir_perm) = 
    match !config_status with
    | Not_configured | Being_configured -> (default_template, 0o755)
    | Configured cf -> (cf.get_template dest, cf.directories_perm)
  in

  match dest with
  | Log_file (file_name, perm) ->
    let dirname = Filename.dirname file_name in
    Lwtfile.mkdir ~parents:true dirname dir_perm >>
    Lwt_log.file ~template ~mode:`Append ~perm ~file_name ()

  | Syslog facility -> Lwt.return (Lwt_log.syslog ~template ~facility ())
  | Stdout -> Lwt.return (Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout ())
  | Stderr -> Lwt.return (Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr ())

  | Log_fun f ->
    let buffer = Buffer.create 120 in
    let output section level lines =
      Lwt_list.iter_s begin fun l -> 
        Lwt_log.render ~buffer ~template ~section ~level ~message:l ;
        f (Buffer.contents buffer)
      end
        lines (* Utiliser le template *)
    in
    Lwt.return (Lwt_log_core.make ~output ~close:(fun () -> Lwt.return_unit))

(* Memoize existing loggers *)
let dest_to_logger = Assoc.create ~size:16
    ~init:create_dest_logger
    ()

let create_directory perm dir =
  try%lwt (Lwtfile.mkdir ~parents:true dir) perm
  with e -> report_error_1 ~exn:e "Mylog.create_directory failed"

(* Errors when invoking get_dest ? *)
let get_dest_error = ref false

let log_to_dests ?exn section level msg dests =

  (* Logs the message on each destination *)
  let failsafe_log = ref false in

  Lwt_list.iter_p begin fun dest ->
    try%lwt
      let%lwt logger = Assoc.get dest_to_logger dest in
      Lwt_log_core.log ?exn ~section ~logger ~level msg
    with e ->
      report_error_1 ~exn:e ("Cannot log to destination " ^ dest_tostring dest) >>
      if not !failsafe_log then 
        begin
          failsafe_log := true ;
          log_failsafe ?exn ~section level msg
        end
      else Lwt.return_unit
  end
    dests

(* Report a logging error. *)
let report_error_2 ?exn msg dests = log_to_dests ?exn mylog_section Error msg dests

let log_raw ?exn section level msg =
  let%lwt destinations = match !config_status with
    | Not_configured | Being_configured -> default_destinations
    | Configured cf ->
      try Lwt.return (cf.get_destination (Lwt_log_core.Section.name section) level)
      with e ->
        (* Cannot get the destinations! We have to log this error too (and only once). *)
        let%lwt () =
          let%lwt dests = default_destinations in
          if not !get_dest_error then report_error_2 ~exn:e "get_destination failed" dests
          else Lwt.return_unit
        in
        get_dest_error := true ;
        default_destinations
  in
  log_to_dests ?exn section level msg destinations


(* Check if logs are awaiting to be dispatched. *)
let rec check_queue () =
  if Queue.is_empty queue then Lwt.return_unit
  else
    (* Needs to be dispatched. *)
    let (exn, section, level, msg) = Queue.take queue in
    log_raw ?exn section level msg >>
    check_queue ()

(* Main log function *)
let choose_log ?exn section level msg =

  match !config_status with
  | Not_configured | Being_configured ->
    (* Register this log for further dispatch. *)
    Queue.add (exn, section, level, msg) queue ;
    log_raw ?exn section level msg

  | Configured cf ->
    (* Already configured. Check if the queue needs to be emptied. *)
    check_queue () >>
    log_raw ?exn section level msg

(* check_queue is thread-safe: it preserves message ordering. *)
let flush () = Lwt.async check_queue

module MkSection (Section : sig val section_name: string end) =
struct
  type level = Lwt_log_core.level
  = Debug | Info | Notice | Warning | Error | Fatal 

  let section = Lwt_log_core.Section.make Section.section_name

  let log ?exn level msg = choose_log ?exn section level msg
  let log_f ?exn level fmt = Printf.ksprintf (fun s -> log ?exn level s) fmt
      
  let alog ?exn level msg = Queue.add (exn, section, level, msg) queue ; flush ()
  let alog_f ?exn level fmt = Printf.ksprintf (fun s -> alog ?exn level s) fmt
end

module LL = MkSection (struct let section_name = "MyLog" end)

(* Sets the configuration (once). *)
let init_config config =

  match !config_status with
  | Being_configured | Configured _ -> LL.log Error "init_config: logs are already configured. Cannot call init_config twice."

  | Not_configured ->
    config_status := Being_configured ;

    (* Create the necessary directories *)
    create_directory config.directories_perm config.failsafe_dir >>
    begin
      failsafe_dir := config.failsafe_dir ;

      (* Remove the loggers for which the template has changed. *)
      let removable = Assoc.fold dest_to_logger []
          begin fun dest logger acu ->
            if config.get_template dest = default_template then acu
            else (dest, logger) :: acu
          end
      in
      let%lwt () = Lwt_list.iter_p
          begin fun (dest, llogger) -> Assoc.remove dest_to_logger dest ; 
            let%lwt logger = llogger in
            try%lwt Lwt_log_core.close logger
            with e -> report_error_1 ~exn:e "Mylog.init_config: close logger failed"
          end
          removable
      in
      
      config_status := Configured config ;
      
      Lwt.return_unit
    end


open Common
open Common.Small

let full_init ?(min_loglevel=Debug) ?(stdout=false) ?(stderr=true) ?syslog ?(appfile=true) ?(template=default_template) ?(failsafe_dir="/tmp") appname =

  let () = Lwt_log_core.(add_rule "*" min_loglevel) in

  let homedir = Futils.find_homedir () in
  let appfile = if appfile then Some (Log_file (homedir ^ "/.log/" ^ appname, 0o600)) else None in

  let dest = (if stdout then Some Stdout else None) ^:: 
             (if stderr then Some Stderr else None) ^::
             (option_map syslog (fun l -> Syslog l)) ^::
             appfile ^:: 
             []
  in

  init_config {
    failsafe_dir ;
    directories_perm = 0o700 ;
    get_destination = (fun _ _ -> dest) ;
    get_template = (fun _ -> template) ;
  }
