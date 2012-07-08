#print_depth 20;;
#use "05.ml";;

(* 10:4 *)
let rec is_zero = function
    n -> eq_int(n, 0);;
exception Too_small;;
let rec pred = function
    n ->
      if eq_int(n, 0)
      then raise Too_small
      else n - 1;;
let rec succ = function
    n -> n + 1;;
let rec plus = function
    n, m ->
      if is_zero(n)
      then m
      else succ(plus(pred(n), m));;

(* 10:6, aka 1:21 *)
type num =
    Zero
  | One_more_than of num;;
let rec is_zero = function
    Zero -> true
  | not_zero -> false;;
let rec pred = function
    Zero -> raise Too_small
  | One_more_than(n) -> n;;
let rec succ = function
    n -> One_more_than(n);;
let rec plus = function
    n, m ->
      if is_zero(n)
      then m
      else succ(plus(pred(n), m));;

(* 10:18 *)
module type N =
sig
  type number
  exception Too_small
  val succ : number -> number
  val pred : number -> number
  val is_zero : number -> bool
end

(* 10:26 *)
module NumberAsNum : N =
struct
  type num =
      Zero
    | One_more_than of num
  type number = num
  exception Too_small
  let succ n = One_more_than n
  let pred = function
      Zero -> raise Too_small
    | One_more_than n -> n
  let is_zero = function
      Zero -> true
    | a_num -> false
end
module NumberAsInt : N =
struct
  type number = int
  exception Too_small
  let succ n = n + 1
  let pred n =
    if eq_int(n, 0)
    then raise Too_small
    else n - 1
  let is_zero n = eq_int(n, 0)
end

(* 10:33 *)
module IntStruct = NumberAsInt

(* 10:35 *)
module NumStruct = NumberAsNum

(* 10:40 *)
module type P =
sig
  type number
  val plus : (number * number) -> number
end

(* 10:41 Plus over Number *)
module PON (X : N) : P =
struct
  type number = X.number
  let rec plus (n, m) =
    if X.is_zero n
    then m
    else X.succ(plus(X.pred(n), m))
end

(* 10:47 SML's additional syntax doesn't appear to be needed here. *)
module IntArith = PON

(* 10:62 Numbers with Conceal Reveal *)
module type N_C_R =
sig
  type number
  exception Too_small
  val conceal : int -> number
  val succ : number -> number
  val pred : number -> number
  val is_zero : number -> bool
  val reveal : number -> int
end

(* 10:65 *)
module NumberAsInt : N_C_R =
struct
  type number = int
  exception Too_small
  let conceal n = n
  let succ n = n + 1
  let pred n =
    if eq_int(n, 0)
    then raise Too_small
    else n - 1
  let is_zero n = eq_int(n, 0)
  let reveal n = n
end
module NumberAsNum : N_C_R =
struct
  type num =
      Zero
    | One_more_than of num
  type number = num
  exception Too_small
  let rec conceal n =
    if eq_int(n, 0)
    then Zero
    else One_more_than(conceal(n - 1))
  let succ n = One_more_than n
  let pred = function
      Zero -> raise Too_small
    | One_more_than n -> n
  let is_zero = function
      Zero -> true
    | a_num -> false
  let rec reveal n =
    if is_zero n
    then 0
    else 1 + reveal(pred(n))
end

(* 10:66 *)
module IntStruct = NumberAsInt
module IntArith = PON
module NumStruct = NumberAsNum
module NumArith = PON;;

(* 10:84 *)
module PON (X : N) : P with type number = X.number =
struct
  type number = X.number
  let rec plus (n, m) =
    if X.is_zero n
    then m
    else X.succ(plus(X.pred(n), m))
end

(* 10:88 *)
module IntArith = PON
module NumArith = PON;;
(* And ... this still doesn't work ... *)

(* 10:93 *)
module NumberAsInt2 : N with type number = int =
struct
  type number = int
  exception Too_small
  let succ n = n + 1
  let pred n =
    if eq_int(n, 0)
    then raise Too_small
    else n - 1
  let is_zero n = eq_int(n, 0)
end

(* 10:95 *)
module IntStruct2 = NumberAsInt2

(* 10:96 *)
module IntArith2 = PON
  (* NOTE: Something more is needed, because this still doesn't work. *)
