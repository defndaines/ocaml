#print_depth 20;;

(* 4:4 *)
type meza =
    Shrimp
  | Calamari
  | Escargots
  | Hummus;;

(* 4:5 *)
type main =
    Steak
  | Ravioli
  | Chicken
  | Eggplant;;

type salad =
    Green
  | Cucumber
  | Greek;;

(* 4:6 *)
type dessert =
    Sundae
  | Mousse
  | Torte;;

(* 4:25 *)
let rec add_a_steak = function
    Shrimp -> (Shrimp, Steak)
  | Calamari -> (Calamari, Steak)
  | Escargots -> (Escargots, Steak)
  | Hummus -> (Hummus, Steak);;
(add_a_steak : meza -> meza * main);;

(* 4:27 *)
let rec add_a_steak = function
    x -> (x, Steak);;

(* 4:33 *)
(add_a_steak : 'a -> 'a * main);;

(* 4:46 *)
let rec eq_main = function
    (Steak, Steak) -> true
  | (Ravioli, Ravioli) -> true
  | (Chicken, Chicken) -> true
  | (Eggplant, Eggplant) -> true
  | (a_main, another_main) -> false;;
(eq_main : main * main -> bool);;

(* 4:54 *)
let rec has_steak = function
    (a_meza, Steak, a_dessert) -> true
  | (a_meza, a_main, a_dessert) -> false;;
(has_steak : 'a * main * 'b -> bool);;

(* 4:66 *)
let rec (has_steak : meza * main * dessert -> bool) = function
    (x, Steak, d) -> true
  | (x, ns, d) -> false;;

(* 4:67 *)
let rec (add_a_steak : meza -> meza * main) = function
    x -> (x, Steak);;
