class istack = object
  val mutable v = [0; 2]
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
end;;

let s = new istack ;;
s#pop ;;
s#push 5 ;;
s#pop ;;

type istack = < pop : int option; push : int -> unit > ;;

class ['a] stack init = object
  val mutable v : 'a list = init
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
end ;;

type 'a iterator = < get : 'a; has_value : bool; next : unit > ;;

class ['a] list_iterator init = object
  val mutable current : 'a list = init
  method has_value = current <> []
  method get =
    match current with
      | hd :: tl -> hd
      | [] -> raise (Invalid_argument "no value")
  method next =
    match current with
      | hd :: tl -> current <- tl
      | [] -> raise (Invalid_argument "no value")
end ;;

class ['a] stack init = object
  val mutable v : 'a list = init
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
  method iterator : 'a iterator =
    new list_iterator v
end ;;

let s = new stack [] ;;
s#push 5 ;;
s#push 4 ;;
let it = s#iterator ;;
it#get ;;
it#next ;;
it#get ;;
it#next ;;
it#has_value ;;

class ['a] stack init = object
  val mutable v : 'a list = init
  method pop =
    match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None
  method push hd =
    v <- hd :: v
  method iter f =
    List.iter ~f v
  method fold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
    (fun f init -> List.fold ~f ~init v)
end ;;

class sstack init = object
  inherit [string] stack init
  method print =
    List.iter ~f:print_string v
end ;;

class double_stack init = object
  inherit [int] stack init as super
  method push hd =
    super#push (hd * 2)
end ;;

type doc =
  | Heading of string
  | Paragraph of text_item list
  | Definition of string list_item list
and text_item =
  | Raw of string
  | Bold of text_item list
  | Enumerate of int list_item list
  | Quote of doc
and 'a list_item =
  { tag: 'a;
    text: text_item list }

class square w = object(self : 'self)
  method width = w
  method area = Float.of_int (self#width * self#width)
  method equals (other : 'self) = other#width = self#width
end ;;
class circle r = object(self : 'self)
  method radius = r
  method area = 3.14 *. (Float.of_int self#radius) ** 2.0
  method equals (other : 'self) = other#radius = self#radius
end ;;

(new square 5)#equals (new square 5) ;;
(new circle 10)#equals (new circle 7) ;;

class obj x =
  let () = printf "Creating obj %d\n" x in
object
  val field = printf "Initializing field\n"; x
end ;;
let o = new obj 3;;
