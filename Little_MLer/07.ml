#print_depth 20;;
#use "05.ml";;

(* 7:1 *)
let rec identity = function
    x -> x;;

(* 7:5 *)
let rec true_maker = function
    x -> true;;

(* 7:7 *)
type bool_or_int =
    Hot of bool
  | Cold of int;;

(* 7:11 *)
let rec hot_maker(x) = function
    x -> Hot (x);;
(hot_maker : 'a -> (bool -> bool_or_int));;

(* 7:20 *)
let rec help = function
    f -> Hot (true_maker(
      if true_maker(5)
      then f
      else true_maker));;
(* So this consumes a function *)
(help : ('a -> bool) -> bool_or_int);;

(* 7:38 *)
type chain =
    Link of (int * (int -> chain));;

(* 7:50 *)
let rec ints = function
    n -> Link (n + 1, ints);;

(* 7:58 *)
let rec skips = function
    n -> Link (n + 2, skips);;
(skips : int -> chain);;

(* 7:61 *)
let rec divides_evenly = function
    n, c -> eq_int((n mod c), 0);;
let rec is_mod_5_or_7 = function
    n -> if divides_evenly(n, 5)
    then true
    else divides_evenly(n, 7);;

(* 7:62 *)
let rec some_ints = function
    n ->
      if is_mod_5_or_7(n + 1)
      then Link (n + 1, some_ints)
      else some_ints(n + 1);;

(* And, voila, we have lazy, infinite lists. *)

(* 7:83 *)
let rec chain_item = function
    n, Link (i, f) ->
      if eq_int(n, 1)
      then i
      else chain_item(n - 1, f(i));;

(* 7:93 *)
let rec is_prime = function
    n -> has_no_divisors(n, n - 1)
and has_no_divisors = function
    n, c -> if eq_int(c, 1)
    then true
    else
      if divides_evenly(n, c)
      then false
      else has_no_divisors(n, c - 1);;

(* 7:94 Alternatives. I don't quite understand the direct translation to
 * Ocaml of this first one. *)
let rec has_no_divisors = function
    n, c ->
      if eq_int(c, 1)
      then true
      else
        if divides_evenly(n, c)
        then false
        else has_no_divisors(n, c - 1)
and is_prime = function
    n -> has_no_divisors(n, n - 1);;

let is_prime = function
    n -> let rec has_no_divisors = function
        n, c ->
          if eq_int(c, 1)
          then true
          else
            if divides_evenly(n, c)
            then false
            else has_no_divisors(n, c - 1)
    in has_no_divisors (n, n - 1);;

(* 7:96 *)
let rec primes = function
    n ->
      if is_prime(n + 1)
      then Link (n + 1, primes)
      else primes(n + 1);;

(* 7:98 Curry syntax ... should have been noted in the book but wasn't. *)
let rec fibs n m = Link (n + m, fibs m);;

(* 7:109 *)
let rec fibs_1 m = Link (1 + m, fibs m);;

(* 7:117 *)
let rec fibs_2 m = Link (2 + m, fibs m);;
