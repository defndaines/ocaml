let multiple_of n d =
  n mod d = 0;;

let integer_square_root n =
  int_of_float (sqrt (float_of_int n));;

let last_character str =
  let len = String.length str - 1 in
  str.[len];;

let string_of_bool truth =
  if truth then "true" else "false";;
