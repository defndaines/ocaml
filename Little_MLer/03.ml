#print_depth 20;;

(* 3:1 *)
type pizza =
    Crust
  | Cheese of pizza
  | Onion of pizza
  | Anchovy of pizza
  | Sausage of pizza;;

(* 3:7 *)
let rec remove_anchovy = function
    Crust -> Crust
  | (Cheese(x)) -> Cheese(remove_anchovy(x))
  | (Onion(x)) -> Onion(remove_anchovy(x))
  | (Anchovy(x)) -> remove_anchovy(x)
  | (Sausage(x)) -> Sausage(remove_anchovy(x));;
(remove_anchovy : pizza -> pizza);;

(* 3:38 *)
let rec top_anchovy_with_cheese = function
    Crust -> Crust
  | (Cheese(x)) -> Cheese(top_anchovy_with_cheese(x))
  | (Onion(x)) -> Onion(top_anchovy_with_cheese(x))
  | (Anchovy(x)) -> Cheese(Anchovy(top_anchovy_with_cheese(x)))
  | (Sausage(x)) -> Sausage(top_anchovy_with_cheese(x));;
(top_anchovy_with_cheese : pizza -> pizza);;
