type host_info =
    { hostname : string;
      os_name : string;
      cpu_arch : string;
      timestamp : Time.t;
    };;

#require "core_extended";;
open Core_extended.Std;;

let my_host =
  let sh = Shell.sh_one_exn in
    { hostname = sh "hostname";
      os_name = sh "uname -s";
      cpu_arch = sh "uname -p";
      timestamp = Time.now ();
    };;

type 'a timestamped = { item : 'a; time : Time.t };;

let first_timestamped list =
  List.reduce list ~f:(fun a b -> if a.time < b.time then a else b)
;;

let host_info_to_string { hostname = h; os_name = os;
                          cpu_arch = c; timestamp = ts;
} = sprintf "%s (%s / %s, on %s)" h os c (Time.to_sec_string ts);;

host_info_to_string my_host;;

type host_info =
    { hostname : string;
      os_name : string;
      cpu_arch : string;
      os_release : string;
      timestamp : Time.t;
    };;

(* to warn about record change mismatches: *)
#warnings "+9";;

let host_info_to_string { hostname = h; os_name = os;
                          cpu_arch = c; timestamp = ts; _
} = sprintf "%s (%s / %s, on %s)" h os c (Time.to_sec_string ts);;

let host_info_to_string { hostname; os_name; cpu_arch; timestamp; _ } =
  sprintf "%s (%s / %s, on %s)" hostname os_name cpu_arch (Time.to_sec_string timestamp);;

let my_host =
  let sh cmd = Shell.sh_one_exn cmd in
  let hostname   = sh "hostname" in
  let os_name    = sh "uname -s" in
  let cpu_arch   = sh "uname -p" in
  let os_release = sh "uname -r" in
  let timestamp  = Time.now () in
    { hostname; os_name; cpu_arch; os_release; timestamp };;

let create_host_info ~hostname ~os_name ~cpu_arch ~os_release =
  { os_name; cpu_arch; os_release;
    hostname = String.lowercase hostname;
    timestamp = Time.now () };;

(* Functional updates *)

type client_info =
    { addr : Unix.Inet_addr.t;
      port : int;
      user : string;
      credentials : string;
      last_heartbeat_time : Time.t;
    };;

let register_heartbeat t hb =
  { addr = t.addr;
    port = t.port;
    user = t.user;
    credentials = t.credentials;
    last_heartbeat_time = hb.Heartbeat.time;
  };;

let register_heartbeat t hb =
  { t with last_heartbeat_time = hb.Heartbeat.time; };;

module Logon = struct
  type t =
      { session_id : string;
        time : Time.t;
        user : string;
        credentials : string;
      }
        with fields
end;;

let get_users logons = List.dedup (List.map logons ~f: Logon.user);;

Field.get Logon.Fields.user;;

let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record)in
    name ^ " : " ^ field_string
;;

let logon = { Logon.
              session_id = "6655321";
              time = Time.now ();
              user = "mdaines";
              credentials = "Xy2d9W"; }
;;

show_field Logon.Fields.user Fn.id logon;;

show_field Logon.Fields.time Time.to_string logon;;

let print_logon logon =
  let print to_string field =
    printf "%s\n" (show_field field to_string logon)
  in
    Logon.Fields.iter
      ~session_id: (print Fn.id)
      ~time: (print Time.to_string)
      ~user: (print Fn.id)
      ~credentials: (print Fn.id)
;;

print_logon logon;;
