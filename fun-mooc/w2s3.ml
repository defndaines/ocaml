let min a =
  let rec find acc a =
    match a with
    | [| |] -> acc
    | _ -> find (min a.(0) acc) (Array.sub a 1 (Array.length(a) - 1)) in
  find a.(0) a;;

let min_index a =
  let rec find i best v a =
    match a with
    | [| |] -> best
    | _ -> if a.(0) < v
        then find (i + 1) i a.(0) (Array.sub a 1 (Array.length(a) - 1))
        else find (i + 1) best v (Array.sub a 1 (Array.length(a) - 1)) in
  find 0 0 a.(0) a;;

let is_sorted a =
  let rec find last a =
    match a with
    | [| |] -> true
    | _ -> 
        match String.compare last a.(0) with
        | -1 -> find a.(0) (Array.sub a 1 (Array.length(a) - 1))
        | _ -> false in
  find "" a;;

let find dict word =
	let rec search low high =
		match (low, high) with
		| (-1, _) -> -1
		| _ when low >= high ->
				if String.length dict > low && (String.compare word dict.(low)) = 0
				then low
				else -1
		| _ ->
				let check = low + high / 2 in
				match String.compare word dict.(check) with
				| 0 -> check
				| n when n > 0 -> search (check + 1) high
				| n when n < 0 -> search low (check - 1) in
	search 0 (Array.length(dict) - 1);; 
