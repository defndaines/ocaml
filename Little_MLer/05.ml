#print_depth 20;;

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
