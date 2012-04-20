#print_depth 20;;

(* 2:1 *)
type shish_kebab =
    Skewer
  | Onion of shish_kebab
  | Lamb of shish_kebab
  | Tomato of shish_kebab;;

(* 2:15 *)
let rec only_onions = function
      (Skewer) -> true
    | (Onion(x)) -> only_onions(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> false;;
(only_onions : shish_kebab -> bool);;

(* 2:63 *)
let rec is_vegetarian = function
      (Skewer) -> true
    | (Onion(x)) -> is_vegetarian(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> true;;
(is_vegetarian : shish_kebab -> bool);;

(* 2:64 *)
type 'a shish =
    Bottom of 'a
  | Onion of 'a shish
  | Lamb of 'a shish
  | Tomato of 'a shish;;

(* 2:67 *)
type rod =
    Dagger
  | Fork
  | Sword;;

(* 2:68 *)
type plate =
    Gold_plate
  | Silver_plate
  | Brass_plate;;

(* 2:73 *)
let rec is_veggie = function
      (Bottom(x)) -> true
    | (Onion(x)) -> is_veggie(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> is_veggie(x);;
(is_veggie : 'a shish -> bool);;

(* 2:108 *)
let rec what_bottom = function
    (Bottom(x)) -> x
  | (Onion(x)) -> what_bottom(x)
  | (Lamb(x)) -> what_bottom(x)
  | (Tomato(x)) -> what_bottom(x);;
(what_bottom : 'a shish -> 'a);;
