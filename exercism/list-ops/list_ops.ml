open Core.Std

let rec length l =
  match l with
    | [] -> 0
    | _::tail -> 1 + length tail

let reverse list =
  let rec l acc = function
    | [] -> acc
    | h::tail -> l (h::acc) tail in
    l [] list

(* stack overflow
 Called from file "//toplevel//", line 4, characters 21-29
 *)
let rec map ~f l =
  match l with
    | [] -> []
    | h::t -> f h :: map ~f t

(* stack overflow:
 Called from file "//toplevel//", line 4, characters 21-29
 *)
let rec filter ~f l =
  match l with
    | [] -> []
    | h::t when f h -> h :: filter ~f t
    | _::t -> filter ~f t

let rec fold ~init ~f l =
  match l with
    | [] -> init
    | h :: t -> fold ~init:(f init h) ~f t

(* Horribly inefficient! This just seems to hang:
 append (List.range 0 1_000_000) (List.range 1_000_000 2_000_000);;
 *)
let rec append base l =
  match l with
    | [] -> base
    | h :: t -> append (base @ [h]) t

(* Called from file "//toplevel//", line 5, characters 20-28
 concat (List.map ~f:(fun x -> [x]) (List.range 0 1_000_000));;
 Interestingly, the last test case does not fail.
 *)
let rec concat lol =
  match lol with
    | [] -> []
    | [[]] -> []
    | h :: t -> h @ concat t