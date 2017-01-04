let rec gcd n m =
  match m with
  | 0 -> n
  | _ -> gcd m (n mod m);;

let multiple_upto n r =
  let rec check x =
    match x with
    | x when x > r -> false
    | x when multiple_of n x -> true
    | _ -> check (x + 1) in
  check 2;;

let is_prime n =
  let sqrt_n = integer_square_root n in
  let rec check x =
    match x with
    | _ when n = 1 -> false
    | x when x > sqrt_n -> true
    | x when n mod x = 0 -> false
    | _ -> check (x + 1) in
  check 2;;
