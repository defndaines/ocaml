#print_depth 20;;
#use "05.ml";;

(* 10:4 *)
let rec is_zero = function
    n -> eq_int(n, 0);;
exception Too_small;;
let rec pred = function
    n ->
      if eq_int(n, 0)
      then raise Too_small
      else n - 1;;
let rec succ = function
    n -> n + 1;;
let rec plus = function
    n, m ->
      if is_zero(n)
      then m
      else succ(plus(pred(n), m));;

(* 10:6, aka 1:21 *)
type num =
    Zero
  | One_more_than of num;;
let rec is_zero = function
    Zero -> true
  | not_zero -> false;;
let rec pred = function
    Zero -> raise Too_small
  | One_more_than(n) -> n;;
let rec succ = function
    n -> One_more_than(n);;
let rec plus = function
    n, m ->
      if is_zero(n)
      then m
      else succ(plus(pred(n), m));;
