module type X_int = sig val x : int end;;

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end;;

module Three = struct let x = 3 end;;
module Four = Increment(Three);;
Four.x - Three.x;;

module Three_and_more = struct
  let x = 3
  let y = "three"
end;;
module Four = Increment(Three_and_more);;

module type Comparable = sig
  type t
  val compare : t -> t -> int
end ;;

module Make_interval(Endpoint : Comparable) = struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
      | Empty -> false
      | Interval (l,h) ->
          Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1,h1), Interval (l2,h2) -> create (max l1 l2) (min h1 h2)

end ;;

module Int_interval =
  Make_interval(struct
                  type t = int
                  let compare = Int.compare
                end);;

module Int_interval = Make_interval(Int) ;;
module String_interval = Make_interval(String) ;;

let i1 = Int_interval.create 3 8;;
let i2 = Int_interval.create 4 10;;
Int_interval.intersect i1 i2;;

module Rev_int_interval =
  Make_interval(struct
                  type t = int
                  let compare x y = Int.compare y x
                end);;

let interval = Int_interval.create 4 3;;
let rev_interval = Rev_int_interval.create 4 3;;

Int_interval.is_empty
  (Int_interval.Interval (4,3)) ;;

module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end;;

module Make_interval(Endpoint : Comparable)
  : Interval_intf with type endpoint := Endpoint.t = struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
      | Empty -> false
      | Interval (l,h) ->
          Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1,h1), Interval (l2,h2) -> create (max l1 l2) (min h1 h2)

end ;;

module Int_interval = Make_interval(Int);;

let i = Int_interval.create 3 4;;
Int_interval.contains i 5;;

module type Int_interval_intf =
  Interval_intf with type endpoint := int;;

Int_interval.is_empty (Int_interval.Interval (4,3));;

Sexp.of_string "(This is (an s-expression))";;

type some_type = int * string list with sexp;;
sexp_of_some_type (33, ["one"; "two"]);;
Sexp.of_string "(44 (five six))" |> some_type_of_sexp;;

module type Interval_intf_with_sexp = sig
  type t
  include Interval_intf with type t := t
  include Sexpable      with type t := t
end;;

module Make_interval(Endpoint : sig
                       type t
                       include Comparable with type t := t
                       include Sexpable   with type t := t
                     end)
  : (Interval_intf_with_sexp with type endpoint := Endpoint.t) = struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty
               with sexp

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  let t_of_sexp sexp =
    match t_of_sexp sexp with
      | Empty -> Empty
      | Interval (x,y) -> create x y

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
      | Empty -> false
      | Interval (l,h) ->
          Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1,h1), Interval (l2,h2) -> create (max l1 l2) (min h1 h2)

end ;;

module Int_interval = Make_interval(Int) ;;
