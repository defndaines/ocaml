let rec sum l =
  match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl
;;

sum [1;2;3];;

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let rec drop_value l to_drop =
  match l with
    | [] -> []
    | hd :: tl ->
        let new_tl = drop_value tl to_drop in
          if hd = to_drop then new_tl else hd :: new_tl
;;

drop_value [1;2;3] 2;;

List.reduce ~f:(+) [1;2;3;4;5];;

List.reduce ~f:(+) [];;

List.filter_map (Sys.ls_dir ".") ~f:(fun fname ->
                                       match String.rsplit2 ~on:'.' fname with
                                         | None | Some ("",_) -> None
                                         | Some (_,ext) ->
                                             Some ext)
                   |> List.dedup
;;

let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
    | Some (_,("ml"|"mli")) -> true
    | _ -> false
;;

let (ml_files,other_files) =
  List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source;;

let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec (s ^/ sub))
    |> List.concat
;;

let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.concat_map ~f:(fun sub -> ls_rec (s ^/ sub))
;;

let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl
;;

let make_list n = List.init n ~f:(fun x -> x);;
length (make_list 10);;
length (make_list 10_000_000);;

let rec length_plus_n l n =
  match l with
    | [] -> n
    | _ :: tl -> length_plus_n tl (n + 1)
;;

let length l = length_plus_n l 0;;

let rec destutter = function
  | [] as l -> l
  | [_] as l -> l
  | hd :: (hd' :: _ as tl) ->
      if hd = hd' then destutter tl
      else hd :: destutter tl
;;

destutter [6;4;4;2;1;5;5;3];;

let rec destutter = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) ->
      if hd = hd' then destutter tl
      else hd :: destutter tl
;;

let rec destutter = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
  | hd :: tl -> hd :: destutter tl
;;

(* This throws a compile warning b/c the whens cannot be detected as exhaustive. *)
let rec count_some list =
  match list with
    | [] -> 0
    | x :: tl when Option.is_none x -> count_some tl
    | x :: tl when Option.is_some x -> 1 + count_some tl
;;

count_some [Some 3; None; Some 4];;

(* Better option to avoid warning. *)
let rec count_some list =
  match list with
    | [] -> 0
    | None   :: tl -> count_some tl
    | Some _ :: tl -> 1 + count_some tl
;;

(* But can just use: *)
let count_some l = List.count ~f:Option.is_some l;;
