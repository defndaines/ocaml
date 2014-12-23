let x = ref 1;;
!x;;
x := !x + 1;;

for i = 3 downto 0 do printf "i = %d\n" i done;;

let rev_inplace ar =
  let i = ref 0 in
  let j = ref (Array.length ar - 1) in
    while !i < !j do
      let tmp = ar.(!i) in
        ar.(!i) <- ar.(!j);
        ar.(!j) <- tmp;
        incr i;
        decr j;
    done
;;

let nums = [|1;2;3;4;5|];;
rev_inplace nums;;

(* cyclic data structure *)
let rec endless_loop = 1 :: 2 :: 3 :: endless_loop;;

(* Laziness *)
let v = lazy (print_string "performing lazy computation\n"; sqrt 16.);;
Lazy.force v;;
Lazy.force v;;

type 'a lazy_state =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exn of exn
;;

let create_lazy f = ref (Delayed f);;
let v = create_lazy
          (fun () -> print_string "performing lazy computation\n"; sqrt 16.);;

let force v =
  match !v with
    | Value x -> x
    | Exn e -> raise e
    | Delayed f ->
        try
          let x = f () in
            v := Value x;
            x
        with exn ->
          v := Exn exn;
          raise exn
;;

force v;;

(* Memoization *)
let memoize f =
  let table = Hashtbl.Poly.create () in
    (fun x ->
       match Hashtbl.find table x with
         | Some y -> y
         | None ->
             let y = f x in
               Hashtbl.add_exn table ~key:x ~data:y;
               y
    )
;;

let rec edit_distance s t =
  match String.length s, String.length t with
    | (0,x) | (x,0) -> x
    | (len_s,len_t) ->
        let s' = String.drop_suffix s 1 in
        let t' = String.drop_suffix t 1 in
        let cost_to_drop_both =
          if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
        in
          List.reduce_exn ~f:Int.min
            [ edit_distance s' t  + 1;
              edit_distance s  t' + 1;
              edit_distance s' t' + cost_to_drop_both ]
;;

edit_distance "OCaml" "ocaml";;

let time f =
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
    printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
    x
;;

time (fun () -> edit_distance "OCaml" "ocaml");;
time (fun () -> edit_distance "OCaml 4.01" "ocaml 4.01");;

let rec fib i =
  if i <= 1 then 1 else fib (i - 1) + fib (i - 2)
;;

time (fun () -> fib 20);;
time (fun () -> fib 40);;

let fib = memoize fib;;

let fib_norec fib i =
  if i <= 1 then i
  else fib (i - 1) + fib (i - 2)
;;

let rec fib i = fib_norec fib i;;

let make_rec f_norec =
  let rec f x = f_norec f x in
    f
;;

let fib = make_rec fib_norec;;

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
    fref := f;
    f x
;;

let fib = memo_rec fib_norec;;

let fib = memo_rec (fun fib i -> if i <= 1 then 1 else fib (i - 1) + fib (i - 2));;

let edit_distance = memo_rec (fun edit_distance (s,t) ->
  match String.length s, String.length t with
    | (0,x) | (x,0) -> x
    | (len_s,len_t) ->
        let s' = String.drop_suffix s 1 in
        let t' = String.drop_suffix t 1 in
        let cost_to_drop_both =
          if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
        in
          List.reduce_exn ~f:Int.min
            [ edit_distance (s',t)  + 1;
              edit_distance (s,t') + 1;
              edit_distance (s',t') + cost_to_drop_both ]
)
;;

time (fun () -> edit_distance ("OCaml 4.01","ocaml 4.01"));;

let lazy_memo_rec f_norec x =
  let rec f = lazy (memoize (fun x -> f_norec (Lazy.force f) x)) in
    (Lazy.force f) x
;;

time (fun () -> lazy_memo_rec fib_norec 40);;

(* I/O *)

printf "%i is an integer, %F is a float, \"%s\" is a string\n" 3 4.5 "five";;

let fmt : ('a, 'b, 'c) format = "%i is an integer, %F is a float, \"%s\" is a string\n";;
printf fmt 3 4.5 "five";;

let create_number_file filename numbers =
  let outc = Out_channel.create filename in
    List.iter numbers ~f:(fun x -> fprintf outc "%d\n" x);
    Out_channel.close outc
;;

let sum_file filename =
  let file = In_channel.create filename in
  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
  let sum = List.fold ~init:0 ~f:(+) numbers in
    In_channel.close file;
    sum
;;

create_number_file "numbers.txt" [1;2;3;4;5];;
sum_file "numbers.txt";;

let sum_file filename =
  let file = In_channel.create filename in
    protect ~f:(fun () ->
                  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
                  List.fold ~init:0 ~f:(+) numbers)
      ~finally:(fun () -> In_channel.close file)
;;

let sum_file filename =
  In_channel.with_file filename
    ~f:(fun file -> In_channel.fold_lines file ~init:0
                      ~f:(fun sum line -> sum + Int.of_string line))
;;

let x = lazy (sin 120.) in
let y = lazy (sin 75.)  in
let z = lazy (sin 128.) in
  List.exists ~f:(fun x -> Lazy.force x < 0.) [x;y;z]
;;

List.exists ~f:(fun x -> x < 0.)
  [ (printf "1\n"; sin 120.);
    (printf "2\n"; sin 75.);
    (printf "3\n"; sin 128.); ]
;;

let remember =
  let cache = ref None in
    (fun x ->
       match !cache with
         | Some y -> y
         | None -> cache := Some x; x)
;;

let identity x = x;;
identity 3;;
identity "five";;

let remember_three () = remember 3;;
remember;;
remember "avocado";;

List.init;;
List.init 10 ~f:Int.to_string;;

let list_init_10 = List.init 10;;

let list_init_10 ~f = List.init 10 ~f;;

module Concat_list : sig
  type +'a t
  val empty : 'a t
  val singleton : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
end = struct

  type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t

  let empty = Empty
  let singleton x = Singleton x
  let concat x y = Concat (x,y)

  let rec to_list_with_tail t tail =
    match t with
      | Empty -> tail
      | Singleton x -> x :: tail
      | Concat(x,y) -> to_list_with_tail x (to_list_with_tail y tail)

  let to_list t =
    to_list_with_tail t []

end;;

Concat_list.empty;;
identity Concat_list.empty;;
