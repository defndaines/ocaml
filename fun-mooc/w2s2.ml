type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  let { x; y; z } = p
  and { dx; dy; dz } = dp in
  { x = x +. dx; y = y +. dy; z = z +. dz };;

let next obj =
  let { position; velocity } = obj in
  { position = move position velocity; velocity };;

let will_collide_soon p1 p2 =
  let np1 = next p1
      and np2 = next p2 in
  let distance = sqrt(
    (np1.position.x -. np2.position.x) ** 2.
    +. (np1.position.y -. np2.position.y) ** 2.
    +. (np1.position.z -. np2.position.z) ** 2.) in
  distance < 2.;;

type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed date =
  date.year >= 1
  && 1 <= date.month && date.month <= 5
  && 1 <= date.day && date.day <= 4
  && 0 <= date.hour && date.hour <= 2
  && 0 <= date.minute && date.minute <= 1;;

let next date =
  let rec bump date =
    match date with
    | _ when date.minute > 1 -> bump { date with hour = date.hour + 1;
                                                 minute = date.minute - 2 }
    | _ when date.hour > 2 -> bump { date with day = date.day + 1;
                                               hour = date.hour - 3 }
    | _ when date.day > 4 -> bump { date with month = date.month + 1;
                                              day = date.day - 4 }
    | _ when date.month > 5 -> bump {date with year = date.year + 1;
                                               month = date.month - 5 }
    | _ -> date in
  bump { date with minute = date.minute + 1 };;

let of_int minutes =
  next { the_origin_of_time with minute = minutes - 1 };;
