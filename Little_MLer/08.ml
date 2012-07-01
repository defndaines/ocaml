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

(* 8:79 Using the two-stage emphasis. *)
let rec subst_c pred = function
    (n, Empty) -> Empty
  | (n, Cons(e, t)) ->
      if pred(e)
      then Cons(n, subst_c(pred)(n, t))
      else Cons(e, subst_c(pred)(n, t));;

(* 8:86 *)
let rec subst_c_in_range_11_16 = function
    (n, Empty) -> Empty
  | (n, Cons(e, t)) ->
      if in_range_11_16(e)
      then Cons(n, subst_c_in_range_11_16(n, t))
      else Cons(e, subst_c_in_range_11_16(n, t));;

(* 8:88 *)
let rec combine = function
    (Empty, l2) -> l2
  | (Cons(a, l1), l2) -> Cons(a, combine(l1, l2));;

(* 8:93 *)
let rec combine_c = function
    (Empty) -> (function (l2) -> l2)
  | (Cons(a, l1)) -> (function (l2) -> Cons(a, combine_c(l1)(l2)));;

(* 8:95 *)
let rec prefixer_123 = function
    l2 -> Cons(1, Cons(2, Cons(3, l2)));;

(* 8:97 *)
let rec waiting_prefix_123 = function
    l2 -> Cons(1, combine_c(Cons(2, Cons(3, Empty)))(l2));;

(* 8:104 *)
let rec base = function
    l2 -> l2;;

(* 8:115 *)
let rec combine_s = function
    Empty -> base
  | Cons(a, l1) -> make_cons(a, combine_s(l1))
and make_cons = function
    (a, f) -> (function (l2) -> Cons(a, f(l2)));;

(* 8:117 *)
let rec prefix_3 = function
    l2 -> Cons(3, base(l2));;
