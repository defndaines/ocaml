#print_depth 20;;

(* 6:6 *)
type fruit =
    Peach
  | Apple
  | Pear
  | Lemon
  | Fig;;

type tree =
    Bud
  | Flat of fruit * tree
  | Split of tree * tree;;

(* 6:11 *)
let rec flat_only = function
    Bud -> true
  | Flat (f, t) -> flat_only(t)
  | Split (s, t) -> false;;
(flat_only : tree -> bool);;

(* 6:18 *)
let rec split_only = function
    Bud -> true
  | Flat (f, t) -> false
  | Split (s, t) ->
      if split_only(s)
      then split_only(t)
      else false;;
(split_only : tree -> bool);;

(* Using && syntax *)
let rec split_only = function
    Bud -> true
  | Flat (f, t) -> false
  | Split (s, t) -> split_only(s) && split_only(t);;

(* 6:22 *)
let rec contains_fruit = function
    Bud -> false
  | Flat (f, t) -> true
  | Split (s, t) ->
      if contains_fruit(s)
      then false
      else contains_fruit(t);;

(* Using || syntax *)
let rec contains_fruit = function
    Bud -> false
  | Flat (f, t) -> true
  | Split (s, t) -> contains_fruit(s) || contains_fruit(t);;

let rec contains_fruit = function
    x -> if split_only(x)
    then false
    else true;;
(contains_fruit : tree -> bool);;

(* Using 'not' syntax *)
let rec contains_fruit = function
    x -> not(split_only(x));;

(* 6:34 *)
let rec (less_than : int * int -> bool) = function
    n, m -> n < m;;
let rec (larger_of : int * int -> int) = function
    n, m ->
      if less_than(n, m)
      then m
      else n;;

(* 6:35 *)
let rec (height : tree -> int) = function
    Bud -> 0
  | Flat (f, t) -> 1 + height(t)
  | Split (s, t) -> 1 + larger_of(height(s), height(t));;

(* 6:39 *)
let rec (eq_fruit : fruit * fruit -> bool) = function
    Peach, Peach -> true
  | Apple, Apple -> true
  | Pear, Pear -> true
  | Lemon, Lemon -> true
  | Fig, Fig -> true
  | a_fruit, another_fruit -> false;;

(* 6:41 *)
let rec (subst_in_tree : fruit * fruit * tree -> tree) = function
    n, a, Bud -> Bud
  | n, a, Flat (f, t) ->
      if eq_fruit(f, a)
      then Flat(n, subst_in_tree(n, a, t))
      else Flat(f, subst_in_tree(n, a, t))
  | n, a, Split (s, t) ->
      Split (subst_in_tree(n, a, s), subst_in_tree(n, a, t));;

(* 6:43 *)
let rec (occurs : fruit * tree -> int) = function
    a, Bud -> 0
  | a, Flat (f, t) ->
      if eq_fruit(f, a)
      then 1 + occurs(a, t)
      else occurs(a, t)
  | a, Split (s, t) -> occurs(a, s) + occurs(a, t);;

(* 6:51 *)
type 'a slist =
    Empty
  | Scons of (('a sexp) * ('a slist))
and 'a sexp =
    An_atom of 'a
  | A_slist of ('a slist);;
