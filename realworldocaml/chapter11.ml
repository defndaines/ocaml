let s = object
  val mutable v = [0; 2]
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
end ;;

s#pop ;;
s#push 4 ;;
s#pop ;;

let stack init = object
  val mutable v = init
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
end ;;

let s = stack [3; 2; 1] ;;
s#pop ;;

let area sq = sq#width * sq#width ;;
let minimize sq : unit = sq#resize 1 ;;
let limit sq =
  if (area sq) > 100 then minimize sq ;;

let area_closed (sq : < width : int >) = sq#width * sq#width ;;

let print_pop st = Option.iter ~f:(printf "Popped: %d\n") st#pop ;;
print_pop (stack [5;4;3;2;1]) ;;
let t = object
  method pop = Some (Float.to_int (Time.to_float (Time.now ())))
end;;
print_pop t ;;

let imm_stack init = object
  val v = init
  method pop =
    match v with
      | hd :: tl -> Some (hd, {< v = tl >})
      | [] -> None
  method push hd =
    {< v = hd :: v >}
end ;;

let s = imm_stack [3; 2; 1] ;;
let t = s#push 4 ;;
s#pop ;;
t#pop ;;

type shape = < area : float >
type square = < area : float; width : int >
let square w = object
  method area = Float.of_int (w * w)
  method width = w
end
type circle = < area : float; radius : int >
let circle r = object
  method area = 3.14 *. (Float.of_int r) ** 2.0
  method radius = r
end

let shape w : shape = ( square w :> shape) ;;

let coin = object
  method shape = circle 5
  method color = "silver"
end ;;

let map = object
  method shape = square 10
end ;;

type item = < shape : shape > ;;
let items = [ (coin :> item) ; (map :> item) ] ;;

let squares : square list = [ square 10; square 20 ] ;;
let shapes : shape list = (squares :> shape list) ;;

let shape_to_string : shape -> string =
  fun s -> sprintf "Shape(%F)" s#area ;;
let square_to_string : square -> string =
  (shape_to_string :> square -> string) ;;

type 'a stack = < pop : 'a option; push : 'a -> unit >
let square_stack : square stack = stack [square 30; square 10]
let circle_stack : circle stack = stack [circle 20; circle 40]

let total_area (shape_stacks : shape stack list) =
  let stack_area acc st =
    let rec loop acc =
      match st#pop with
        | Some s -> loop (acc +. s#area)
        | None -> acc
    in
      loop acc
  in
    List.fold ~init:0.0 ~f:stack_area shape_stacks ;;

total_area [(square_stack :> shape stack); (circle_stack :> shape stack)] ;;

type 'a readonly_stack = < pop : 'a option > ;;

let total_area (shape_stacks : shape readonly_stack list) =
  let stack_area acc st =
    let rec loop acc =
      match st#pop with
        | Some s -> loop (acc +. s#area)
        | None -> acc
    in
      loop acc
  in
    List.fold ~init:0.0 ~f:stack_area shape_stacks ;;

total_area [(square_stack :> shape readonly_stack); (circle_stack :> shape readonly_stack)] ;;

type shape = < variant : repr; area : float >
and cirlce = < variant : repr; area : float; radius : int >
and line = < variant : repr; area : float; length : int >
and repr =
    | Circle of circle
    | Line of line;;

let is_barbell = function
  | [s1; s2; s3] ->
      (match s1#variant, s2#variant, s3#variant with
         | Circle c1, Line _, Circle c2 when c1#radius = c2#radius -> true
         | _ -> false)
  | _ -> false;;

let remove_large l =
  List.filter ~f:(fun s -> s#area <= 100.) l;;

let squares : < area : float; width : int > list =
  [square 5; square 15; square 10] ;;
remove_large squares ;;
