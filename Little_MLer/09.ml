#print_depth 20;;
#use "05.ml";;

(* 9:4 *)
type 'a list =
    Empty
  | Cons of 'a * 'a list;;

(* 9:5 *)
type box =
    Bacon
  | Ix of int;;

(* 9:9 *)
let rec is_bacon = function
    Bacon -> true
  | Ix(n) -> false;;

let rec where_is = function
    Empty -> 0
  | Cons(a_box, rest) ->
      if is_bacon(a_box)
      then 1
      else 1 + where_is(rest);;

(* 9:14 *)
exception No_bacon of int;;
let rec where_is = function
    Empty -> raise (No_bacon 0)
  | Cons(a_box, rest) ->
      if is_bacon(a_box)
      then 1
      else 1 + where_is(rest);;

(* 9:21 *)
try where_is(Cons(Ix(5), Cons(Ix(13), Cons(Ix(8), Empty))))
with No_bacon an_int -> an_int;;

(* 9:66 *)
exception Out_of_range;;
let rec list_item = function
    n, Empty -> raise Out_of_range
  | n, Cons(abox, rest) ->
      if eq_int(n, 1)
      then abox
      else list_item(n - 1, rest);;

(* 9:67 *)
let rec find = function
    n, boxes -> check(n, boxes, list_item(n, boxes))
and check = function
    n, boxes, Bacon -> n
  | n, boxes, Ix(i) -> find(i, boxes);;

(* 9:75 *)
let t = Cons(Ix(5), Cons(Ix(4), Cons(Bacon, Cons(Ix(2), Cons(Ix(7), Empty)))));;

(* 9:67 Stack overflow on some numbers ... 2, 4. *)
let rec find = function
    n, boxes -> try check(n, boxes, list_item(n, boxes))
                with Out_of_range -> find(n / 2, boxes)
and check = function
    n, boxes, Bacon -> n
  | n, boxes, Ix(i) -> find(i, boxes);;

(* 9:98 *)
let rec path = function
    n, boxes -> try Cons(n, (check(boxes, list_item(n, boxes))))
                with Out_of_range -> path(n / 2, boxes)
and check = function
    boxes, Bacon -> Empty
  | boxes, Ix(i) -> path(i, boxes);;
