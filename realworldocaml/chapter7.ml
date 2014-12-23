List.find [1;2;3] ~f:(fun x -> x >= 10);;

let compute_bounds ~cmp list =
  let sorted = List.sort ~cmp list in
    match List.hd sorted, List.last sorted with
      | None, _ | _, None -> None
      | Some x, Some y -> Some (x,y)
;;

compute_bounds ~cmp:Int.compare [1;2;9;4;5;(-4);7];;

compute_bounds ~cmp:Int.compare [];;

let accumulate_mismatches table ~key ~data mismatches =
  match Hashtbl.find table key with
    | Some data' when data' <> data -> key :: mismatches
    | _ -> mismatches 
;;

let find_mismatches table1 table2 =
  Hashtbl.fold table1 ~init:[] ~f:(accumulate_mismatches table2)
;;

Error.of_string "something went wrong";;

Error.of_thunk (fun () ->
                  sprintf "something went wrong: %f" 32.3343);;

Error.create "Something failed a long time ago" Time.epoch Time.sexp_of_t;;

let custom_to_sexp = <:sexp_of<float * string list * int>>;;
custom_to_sexp (3.5, ["a";"b";"c"], 6034);;

Error.create "Something went terribly wrong"
  (3.5, ["a";"b";"c"], 6034)
  <:sexp_of<float * string list * int>> ;;

let compute_bounds ~cmp list =
  let sorted = List.sort ~cmp list in
    Option.bind (List.hd sorted)
      (fun first -> Option.bind (List.last sorted)
                      (fun last -> Some (first,last)))
;;

let compute_bounds ~cmp list =
  let open Option.Monad_infix in
  let sorted = List.sort ~cmp list in
    List.hd sorted >>= fun first ->
      List.last sorted >>= fun last ->
        Some (first,last)
;;

let compute_bounds ~cmp list =
  let sorted = List.sort ~cmp list in
    Option.both (List.hd sorted) (List.last sorted)
;;

(* Exceptions *)

List.map ~f:(fun x -> printf "%d\n%!" x; 100 / x) [1;3;0;4];;

exception Key_not_found of string;;
raise (Key_not_found "a");;

let exceptions = [ Not_found; Division_by_zero; Key_not_found "b" ];;
List.filter exceptions ~f:(function
                             | Key_not_found _ | Not_found -> true
                             | _ -> false);;

let rec find_exn alist key = match alist with
  | [] -> raise (Key_not_found key)
  | (key',data) :: tl -> if key = key' then data else find_exn tl key
;;
let alist = [("a",1); ("b",2)];;
find_exn alist "a";;
find_exn alist "c";;

(* infinite loop *)
let rec forever () = forever ();;

exception Wrong_date of Date.t;;
Wrong_date (Date.of_string "2011-02-23");;

exception Wrong_date of Date.t with sexp;;
Wrong_date (Date.of_string "2011-02-23");;

let merge_lists xs ys ~f =
  if List.length xs <> List.length ys then None
  else
    let rec loop xs ys =
      match xs,ys with
        | [],[] -> []
        | x::xs, y::ys -> f x y :: loop xs ys
        | _ -> assert false
    in
      Some (loop xs ys)
;;

merge_lists [1;2;3] [-1;1;2] ~f:(+);;
merge_lists [1;2;3] [-1;1] ~f:(+);;

let merge_lists xs ys ~f =
  let rec loop xs ys =
    match xs,ys with
      | [],[] -> []
      | x::xs, y::ys -> f x y :: loop xs ys
      | _ -> assert false
  in
    loop xs ys
;;

let reminders_of_sexp =
  <:of_sexp<(Time.t * string) list>>
;;

let load_reminders filename =
  let inc = In_channel.create filename in
  let reminders = reminders_of_sexp (Sexp.input_sexp inc) in
    In_channel.close inc; (* bug ... won't be closed on exception *)
    reminders
;;

let load_reminders filename =
  let inc = In_channel.create filename in
    protect ~f:(fun () -> reminders_of_sexp (Sexp.input_sexp inc))
      ~finally:(fun () -> In_channel.close inc)
;;

let load_reminders filename =
  In_channel.with_file filename
    ~f:(fun inc -> reminders_of_sexp (Sexp.input_sexp inc))
;;

let lookup_weight ~compute_weight alist key =
  try
    let data = List.Assoc.find_exn alist key in
      compute_weight data
  with
      Not_found -> 0.
;;

lookup_weight ~compute_weight:(fun _ -> raise Not_found)
  ["a",3; "b",4] "a" ;;

let lookup_weight ~compute_weight alist key =
  match
    try Some (List.Assoc.find_exn alist key)
    with _ -> None
  with
    | None -> 0.
    | Some data -> compute_weight data
;;

let lookup_weight ~compute_weight alist key =
  match List.Assoc.find alist key with
    | None -> 0.
    | Some data -> compute_weight data
;;

let find alist key =
  Option.try_with (fun () -> find_exn alist key)
;;
find ["a",1; "b",2] "c";;
find ["a",1; "b",2] "b";;

let find alist key =
  Or_error.try_with (fun () -> find_exn alist key)
;;
find ["a",1; "b",2] "c";;

Or_error.ok_exn (find ["a",1; "b",2] "b");;
Or_error.ok_exn (find ["a",1; "b",2] "c");;
