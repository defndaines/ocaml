open Core.Std

type basic_color =
    [ `Black | `Red     | `Green | `Yellow
    | `Blue  | `Magenta | `Cyan  | `White ]

type color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB  of int * int * int ]

type extended_color =
  [ color
  | `RGBA of int * int * int * int ]

val color_to_int          : color -> int
val extended_color_to_int : extended_color -> int
