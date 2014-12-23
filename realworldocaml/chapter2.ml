let area_of_ring inner_radius outer_radius =
  let pi = acos (-1.) in
  let area_of_circle r = pi *. r *. r in
    area_of_circle outer_radius -. area_of_circle inner_radius
;;

let (ints, strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")];;

let upcase_first_entry line =
  match String.split ~on:',' line with
    | [] -> assert false (* String.split returns at least one element. *)
    | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
;;

(fun x -> x + 1) 7;;

List.map ~f:(fun x -> x + 1) [1;2;3];;

let increments = [ (fun x -> x + 1); (fun x -> x + 2)  ];;
List.map ~f:(fun g -> g 5) increments;;

let abs_diff x y = abs (x - y);;

let dist_from_3 = abs_diff 3;;

dist_from_3 8;;

(+) 3 4;;

List.map ~f:((+) 3) [4;5;6];;

let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2);;
(3,2) +! (-2,4);;

let ( *** ) x y = (x ** y) ** y;;
3. *** 4.;;

let path = "/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/git/bin:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/mysql/bin";;
String.split ~on:':' path
  |> List.dedup ~compare:String.compare
  |> List.iter ~f:print_endline
;;

let some_or_zero = function
  | Some x -> x
  | None -> 0
;;
List.map ~f:some_or_zero [Some 3; None; Some 4];;

(* Equavalent to ... *)
let some_or_zero num_opt =
  match num_opt with
    | Some x -> x
    | None -> 0
;;

let some_or_default default = function
  | Some x -> x
  | None -> default
;;

some_or_default 3 (Some 5);;
List.map ~f:(some_or_default 100) [Some 3; None; Some 4];;

let ratio ~num ~denom = float num /. float denom;;

ratio ~num:3 ~denom:10;;
ratio ~denom:10 ~num:3;;

let num = 3 in
let denom = 4 in
  ratio ~num ~denom;;

let apply_to_tuple f (first,second) = f ~first ~second;;
let apply_to_tuple_2 f (first,second) = f ~second ~first;;
let divide ~first ~second = first / second;;

apply_to_tuple_2 divide (3,4);;
apply_to_tuple divide (30,4);;
