type point2d = { x : float; y : float };;

let p = { x = 3.; y = -4. };;

let q = { x = 6.; y = -8. };;

let magnitude { x = x_pos; y = y_pos } =
  sqrt (x_pos ** 2. +. y_pos ** 2.);;

(* field punning *)
let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);;

let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;

type circle_desc = { center : point2d; radius : float }
type rect_desc = { lower_left : point2d; width : float; height : float }
type segment_desc = { endpoint1 : point2d; endpoint2 : point2d } ;;

type scene_element =
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc
;;

let is_inside_scene_element point scene_element =
  match scene_element with
    | Circle { center; radius } ->
        distance center point < radius
    | Rect { lower_left; width; height } ->
        point.x    > lower_left.x && point.x < lower_left.x +. width
        && point.y > lower_left.y && point.y < lower_left.y +. height
    | Segment { endpoint1; endpoint2 } -> false
;;

let is_inside_scene point scene =
  List.exists scene
    ~f:(fun el -> is_inside_scene_element point el)
;;

is_inside_scene { x = 3.; y = 7. }
  [ Circle { center = { x = 3. ; y = 4. }; radius = 0.5 } ];;

is_inside_scene { x = 3.; y = 7. }
  [ Circle { center = { x = 3. ; y = 4. }; radius = 5. } ];;

let numbers = [| 1; 2; 3; 4 |];;

type running_sum =
    { mutable sum : float;
      mutable sum_sq : float; (* sum of squares *)
      mutable samples : int;
    }
;;

let mean rsum = rsum.sum /. float rsum.samples
let stdev rsum =
  sqrt (rsum.sum_sq /. float rsum.samples
        -. (rsum.sum /. float rsum.samples) ** 2.) ;;

let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
let update rsum x =
  rsum.samples <- rsum.samples + 1;
  rsum.sum     <- rsum.sum     +. x;
  rsum.sum_sq  <- rsum.sum_sq  +. x *. x
;;

let rsum = create ();;

List.iter [1.; 3.; 2.; -7.; 4.; 5.] ~f:(fun x -> update rsum x);;

stdev rsum;;

let x = { contents = 0 };;

type 'a ref = { mutable contents : 'a }
let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x
;;

let sum list =
  let sum = ref 0 in
    List.iter list ~f:(fun x -> sum := !sum + x);
    !sum
;;

let permute array =
  let length = Array.length array in
    for i = 0 to length - 2 do
      (* pick a j to swap with *)
      let j = i + Random.int (length - i) in
        (* Swap i and j *)
      let tmp = array.(i) in
        array.(i) <- array.(j);
        array.(j) <- tmp
    done
;;

let ar = Array.init 20 ~f:(fun i -> i);;
permute ar;;
ar;;

let find_first_negative_entry array =
  let pos = ref 0 in
    while !pos < Array.length array && array.(!pos) >= 0 do
      pos := !pos + 1
    done;
    if !pos = Array.length array then None else Some !pos
;;

find_first_negative_entry [|1;-2;0;3|];;
