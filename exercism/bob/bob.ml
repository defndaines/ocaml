open Core.Std

let question phrase =
  let last = String.length phrase - 1 in
    '?' = phrase.[last]

let shout phrase =
  let up = String.uppercase phrase in
    up = phrase && up <> String.lowercase phrase

let silence phrase =
  String.strip phrase = ""

let response_for = function
  | phrase when silence phrase -> "Fine. Be that way!"
  | phrase when shout phrase -> "Whoa, chill out!"
  | phrase when question phrase -> "Sure."
  | _ -> "Whatever."