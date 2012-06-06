#print_depth 20;;
#use "05.ml";;
#use "06.ml";;

(* 8:1 *)
type 'a list =
    Empty
  | Cons of 'a * 'a list;;

(* 8:3 *)
type orapl =
    Orange
  | Apple;;

let rec eq_orapl = function
    Orange, Orange -> true
  | Apple, Apple -> true
  | one, another -> false;;

(* 8:4 *)
let rec subst_int = function
    n, a, Empty -> Empty
  | n, a, Cons(e, t) -> if eq_int(a, e)
    then Cons(n, subst_int(n, a, t))
    else Cons(e, subst_int(n, a, t));;

let rec subst_orapl = function
    n, a, Empty -> Empty
  | n, a, Cons(e, t) -> if eq_orapl(a, e)
    then Cons(n, subst_orapl(n, a, t))
    else Cons(e, subst_orapl(n, a, t));;

(* 8:9 *)
let rec subst = function
    rel, n, a, Empty -> Empty
  | rel, n, a, Cons(e, t) ->
      if rel(a, e)
      then Cons(n, subst(rel, n, a, t))
      else Cons(e, subst(rel, n, a, t));;

(* 8:33 *)
let rec in_range = function
    (small, large), x ->
      if less_than(small, x)
      then less_than(x, large)
      else false;;

(* 8:40 *)
let rec subst_pred = function
    pred, n, Empty -> Empty
  | pred, n, Cons(e, t) ->
      if pred(e)
      then Cons(n, subst_pred(pred, n, t))
      else Cons(e, subst_pred(pred, n, t));;

(* 8:48 *)
let rec is_15 = function
    n -> eq_int(n, 15);;

(* 8:52 *)
let rec less_than_15 = function
    x -> less_than(x, 15);;

(* 8:59 *)
let rec in_range_11_16 = function
    x ->
      if less_than(11, x)
      then less_than(x, 16)
      else false;;

(* 8:65 *)
let in_range_c (small, large) x =
  if less_than(small, x)
  then less_than(x, large)
  else false;;
