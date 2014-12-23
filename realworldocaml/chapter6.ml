type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White ;;

let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;

let color_by_number number text =
  sprintf "\027[38;5;%dm%s\027[0m" number text;;

let blue = color_by_number (basic_color_to_int Blue) "Blue";;

printf "Hello %s World!\n" blue;;

type weight = Regular | Bold
type color =
  | Basic of basic_color * weight
  | RGB of int * int * int
  | Gray of int
;;

[RGB (250,70,70); Basic (Green, Regular)];;

let color_to_int = function
  | Basic (basic_color, weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
        base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
;;

let color_print color s =
  printf "%s\n" (color_by_number (color_to_int color) s);;

color_print (Basic (Red,Bold)) "A bold red!";;

color_print (Gray 4) "A muted gray...";;

module Log_entry = struct
  type t =
      { session_id: string;
        time:       Time.t;
        important:  bool;
        message:    string;
      }
end
;;

type client_message =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t
;;

module Log_entry = struct
  type t = { important: bool;
             message:   string;
  }
end
module Heartbeat = struct
  type t = { status_message: string; }
end
module Logon = struct
  type t = { user:        string;
             credentials: string;
  }
end ;;

type details =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t
;;

module Common = struct
  type t = { session_id: string;
             time:       Time.t;
  }
end ;;

let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
            let session_id = common.Common.session_id in
              match details with
                | Logon m ->
                    if m.Logon.user = user then
                      (message::messages, Set.add user_sessions session_id)
                    else acc
                | Heartbeat _ | Log_entry _ ->
                    if Set.mem user_sessions session_id then
                      (message::messages,user_sessions)
                    else acc
      )
  in
    List.rev user_messages
;;

(* I'm extracting the function from above to make it more readable, with a curry *)

let extract_user_messages user ((messages,user_sessions) as acc) ((common,details) as message) =
  let session_id = common.Common.session_id in
    match details with
      | Logon m ->
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions session_id)
          else acc
      | Heartbeat _
      | Log_entry _ ->
          if Set.mem user_sessions session_id then
            (message::messages,user_sessions)
          else acc
;;

let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(extract_user_messages user)
  in
    List.rev user_messages
;;

let handle_message server_state (common,details) =
  match details with
    | Log_entry m -> handle_log_entry server_state (common,m)
    | Logon m     -> handle_logon server_state (common,m)
    | Heartbeat m -> handle_heartbeat server_state (common,m)
;;

(* Variant and recursive data structures *)

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr
;;

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field: mail_field;
                        contains: string }
;;

let test field contains = Base { field; contains };;

let doligez_runtime =
  And [ Or [ test To "doligez"; test CC "doligez" ];
      test Subject "runtime";
]
;;

let rec eval expr base_eval =
  let eval' expr = eval expr base_eval in
    match expr with
      | Base base -> base_eval base
      | Const bool -> bool
      | And exprs -> List.for_all exprs ~f:eval'
      | Or exprs -> List.exists exprs ~f:eval'
      | Not expr -> not (eval' expr)
;;

let and_ l =
  if List.mem l (Const false) then Const false
  else
    match List.filter l ~f:((<>) (Const true)) with
      | [] -> Const true
      | [ x ] -> x
      | l -> And l
let or_ l =
  if List.mem l (Const true) then Const true
  else
    match List.filter l ~f:((<>) (Const false)) with
      | [] -> Const false
      | [ x ] -> x
      | l -> Or l
let not_ = function
  | Const b -> Const (not b)
  | e -> Not e
;;

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_ (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)
;;

simplify (Not (And [ Or [Base "it's snowing"; Const true];
                     Base "it's raining"]));;

simplify (Not (And [ Or [Base "it's snowing"; Const true];
                     Not (Not (Base "it's raining"))]));;

let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | (Base _ | And _ | Or _) as e -> Not e
;;

let three = `Int 3;;
let four = `Float 4.;;
let nan = `Not_a_number;;
[three; four; nan];;

let five = `Int "five";;

[three; four; five];;

let is_positive = function
  | `Int x   -> x > 0
  | `Float x -> x > 0.
;;

let exact = List.filter ~f:is_positive [three;four];;

let is_positive = function
  | `Int x   -> Ok (x > 0)
  | `Float x -> Ok (x > 0.)
  | `Not_a_number -> Error "not a number"
;;

List.filter [three; four; nan] ~f:(fun x -> match is_positive x with Error _ -> false | Ok b -> b);;

type extended_color =
  | Basic of basic_color * weight
  | RGB   of int * int * int
  | Gray  of int
  | RGBA  of int * int * int * int
;;

let extended_color_to_int = function
  | RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
;;

let basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7
let color_to_int = function
  | `Basic (basic_color, weight) ->
      let base = match weight with `Bold -> 8 | `Regular -> 0 in
        base + basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i
;;

let extended_color_to_int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color
;;
