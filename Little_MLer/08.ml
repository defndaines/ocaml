#print_depth 20;;
#use "05.ml";;

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
