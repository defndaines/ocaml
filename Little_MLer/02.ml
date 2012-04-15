#print_depth 20;;

type shish_kebab =
    Skewer
  | Onion of shish_kebab
  | Lamb of shish_kebab
  | Tomato of shish_kebab;;

let rec only_onions =
  function
      (Skewer) -> true
    | (Onion(x)) -> only_onions(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> false;;
(only_onions : shish_kebab -> bool);;

let rec is_vegetarian =
  function
      (Skewer) -> true
    | (Onion(x)) -> is_vegetarian(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> true;;
(is_vegetarian : shish_kebab -> bool);;

type 'a shish =
    Bottom of 'a
  | Onion of 'a shish
  | Lamb of 'a shish
  | Tomato of 'a shish;;

type rod =
    Dagger
  | Fork
  | Sword;;

type plate =
    Gold_plate
  | Silver_plate
  | Brass_plate;;

let rec is_veggie =
  function
      (Bottom(x)) -> true
    | (Onion(x)) -> is_veggie(x)
    | (Lamb(x)) -> false
    | (Tomato(x)) -> is_veggie(x);;
(is_veggie : 'a shish -> bool);;
