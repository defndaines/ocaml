#print_depth 20;;

(* 1:16 *)
type seasoning =
    Salt
  | Pepper;;

(* 1:21 *)
type num =
    Zero
  | One_more_than of num;;

(* 1:32 *)
type 'a open_faced_sandwich = 
    Bread of 'a
  | Slice of 'a open_faced_sandwich;;

