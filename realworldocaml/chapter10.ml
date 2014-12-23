module type X_int = sig val x : int end;;

module Three : X_int = struct let x = 3 end;;
Three.x;;

let three = (module Three : X_int);;

module Four = struct let x = 4 end;;
let numbers = [ three; (module Four) ];;

let numbers = [three; (module struct let x = 4 end)];;

module New_three = (val three : X_int) ;;
New_three.x;;

let to_int m =
  let module M = (val m : X_int) in
    M.x
;;

let plus m1 m2 =
  (module struct
     let x = to_int m1 + to_int m2
   end : X_int)
;;

let six = plus three three;;
to_int (List.fold ~init:six ~f:plus [three;three]);;

let to_int (module M : X_int) = M.x;;

module type Bumpable = sig
  type t
  val bump : t -> t
end;;

module Int_bumper = struct
  type t = int
  let bump n = n + 1
end;;
module Float_bumper = struct
  type t = float
  let bump n = n +. 1.
end;;

let int_bumper = (module Int_bumper : Bumpable);;

let (module Bumpable) = int_bumper in Bumpable.bump 3;;

let int_bumper = (module Int_bumper : Bumpable with type t = int);;
let float_bumper = (module Float_bumper : Bumpable with type t = float);;

let (module Bumpable) = int_bumper in Bumpable.bump 3;;
let (module Bumpable) = float_bumper in Bumpable.bump 3.5;;

let bump_list
      (type a)
      (module B : Bumpable with type t = a)
      (l: a list)
      =
  List.map ~f:B.bump l
;;

bump_list int_bumper [1;2;3];;
bump_list float_bumper [1.5;2.5;3.5];;

module type Query_handler = sig
  type config with sexp
  val name : string
  type t
  val create : config -> t
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end;;

module Unique = struct
  type config = int with sexp
  type t = { mutable next_id : int }
  let name = "Unique"
  let create start_at = { next_id = start_at }
  let eval t sexp =
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
      | Error _ as err -> err
      | Ok () ->
          let response = Ok (Int.sexp_of_t t.next_id) in
            t.next_id <- t.next_id + 1;
            response
end;;

let unique = Unique.create 0;;
Unique.eval unique Sexp.unit;;
Unique.eval unique Sexp.unit;;

module List_dir = struct
  type config = string with sexp
  type t = { cwd : string }
  let is_abs p =
    String.length p > 0 && p.[0] = '/'
  let name = "ls"
  let create cwd = { cwd }
  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
      | Error _ as err -> err
      | Ok dir ->
          let dir =
            if is_abs dir then dir
            else Filename.concat t.cwd dir
          in
            Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end;;

let list_dir = List_dir.create "/var";;
List_dir.eval list_dir (sexp_of_string ".");;
List_dir.eval list_dir (sexp_of_string "yp");;

module type Query_handler_instance = sig
  module Query_handler : Query_handler
  val this : Query_handler.t
end;;

let unique_instance =
  (module struct
     module Query_handler = Unique
     let this = Unique.create 0
   end : Query_handler_instance);;

let build_instance
      (type a)
      (module Q : Query_handler with type config = a)
      config
      =
  (module struct
     module Query_handler = Q
     let this = Q.create config
   end : Query_handler_instance)
;;

let unique_instance = build_instance (module Unique) 0;;
let list_dir_instance = build_instance (module List_dir) "/var";;

let build_dispatch_table handlers =
  let table = String.Table.create () in
    List.iter handlers
      ~f:(fun ((module I : Query_handler_instance) as instance) ->
            Hashtbl.replace table ~key:I.Query_handler.name ~data:instance);
    table
;;

let dispatch dispatch_table name_and_query =
  match name_and_query with
    | Sexp.List [Sexp.Atom name; query] ->
        begin match Hashtbl.find dispatch_table name with
          | None -> Or_error.error "Could not find matching handler" name String.sexp_of_t
          | Some (module I : Query_handler_instance) -> I.Query_handler.eval I.this query
        end
  | _ -> Or_error.error_string "malformed query"
;;

let rec cli dispatch_table =
  printf ">>> %!";
  let result =
    match In_channel.input_line stdin with
      | None -> `Stop
      | Some line ->
          match Or_error.try_with (fun () -> Sexp.of_string line) with
            | Error e -> `Continue (Error.to_string_hum e)
            | Ok (Sexp.Atom "quit") -> `Stop
            | Ok query ->
                begin match dispatch dispatch_table query with
                  | Error e -> `Continue (Error.to_string_hum e)
                  | Ok s -> `Continue (Sexp.to_string_hum s)
                end;
  in
    match result with
      | `Stop -> ()
      | `Continue msg ->
          printf "%s\n%!" msg;
          cli dispatch_table
;;
