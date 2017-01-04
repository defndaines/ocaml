let exchange k =
  let (t, o) = k / 10, k mod 10 in
  o * 10 + t;;

let is_valid_answer (grand_father_age, grand_son_age) =
  (grand_son_age * 4 = grand_father_age)
  && ((exchange grand_father_age) * 3 = (exchange grand_son_age));;

let find answer =
  let (max_grand_father_age, min_grand_son_age) = answer in
  let rec check ages =
    match ages with
    | (age, _) when age > max_grand_father_age -> (-1, -1)
    | ages when is_valid_answer ages -> ages
    | (_, age) -> check ((age + 1) * 4, (age + 1)) in
  check (min_grand_son_age * 4, min_grand_son_age);;
