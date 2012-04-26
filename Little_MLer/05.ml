#print_depth 20;;
#use "01.ml";;

(* 5:1 *)
type 'a pizza =
    Bottom
  | Topping of ('a * ('a pizza));;

(* 5:3 *)
type fish =
    Anchovy
  | Lox
  | Tuna;;

(* 5:13 *)
let rec rem_anchovy = function
    Bottom -> Bottom
  | Topping(Anchovy, p) -> rem_anchovy(p)
  | Topping(t, p) -> Topping(t, rem_anchovy(p));;
(rem_anchovy : fish pizza -> fish pizza);;

(* 5:14 *)
let rec rem_tuna = function
    Bottom -> Bottom
  | Topping(Tuna, p) -> rem_tuna(p)
  | Topping(t, p) -> Topping(t, rem_tuna(p));;
(rem_tuna : fish pizza -> fish pizza);;

(* 5:28 *)
let rec rem_fish = function
    (x, Bottom) -> Bottom
  | Tuna, Topping(Tuna, p) -> rem_fish(Tuna, p)
  | Tuna, Topping(t, p) -> Topping(t, rem_fish(Tuna, p))
  | Anchovy, Topping(Anchovy, p) -> rem_fish(Anchovy, p)
  | Anchovy, Topping(t, p) -> Topping(t, rem_fish(Anchovy, p))
  | Lox, Topping(Lox, p) -> rem_fish(Lox, p)
  | Lox, Topping(t, p) -> Topping(t, rem_fish(Lox, p));;
(rem_fish : fish * fish pizza -> fish pizza);;

(* 5:38 *)
let rec eq_fish = function
    Anchovy, Anchovy -> true
  | Lox, Lox -> true
  | Tuna, Tuna -> true
  | a_fish, another_fish -> false;;
(eq_fish : fish * fish -> bool);;

(* 5:40 if-then-else construct *)
let rec rem_fish = function
    (x, Bottom) -> Bottom
  | (x, Topping(t, p)) ->
      if eq_fish(t, x)
      then rem_fish(x, p)
      else Topping(t, rem_fish(x, p));;

(* 5:57 *)
let rec (eq_int : int * int -> bool) = function
    n, m -> n = m;;
let rec rem_int = function
    (x, Bottom) -> Bottom
  | (x, Topping(t, p)) ->
      if eq_int(t, x)
      then rem_int(x, p)
      else Topping(t, rem_int(x, p));;
(rem_int : int * int pizza -> int pizza);;

(* 5:66 *)
let rec subst_fish = function
    (n, a, Bottom) -> Bottom
  | (n, a, Topping(t, p)) ->
      if eq_fish(t, a)
      then Topping(n, subst_fish(n, a, p))
      else Topping(t, subst_fish(n, a, p));;
(subst_fish : fish * fish * fish pizza -> fish pizza);;
let rec subst_int = function
    (n, a, Bottom) -> Bottom
  | (n, a, Topping(t, p)) ->
      if eq_int(t, a)
      then Topping(n, subst_int(n, a, p))
      else Topping(t, subst_int(n, a, p));;
(subst_int : int * int * int pizza -> int pizza);;

(* 5:70 *)
let rec eq_num = function
    Zero, Zero -> true
  | One_more_than(n), Zero -> false
  | Zero, One_more_than(m) -> false
  | One_more_than(n), One_more_than(m) -> eq_num(n, m);;

(* 5:71 *)
let rec eq_num = function
    Zero, Zero -> true
  | One_more_than(n), One_more_than(m) -> eq_num(n, m)
  | n, m -> false;;
