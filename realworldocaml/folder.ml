open Core.Std

class ['a] folder = object(self)
  method doc acc = function
    | Heading _ -> acc
    | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
    | Definition list -> List.fold ~f:self#list_item ~init:acc list

  method list_item: 'b. 'a -> 'b list_item -> 'a =
    fun acc {tag; text} ->
      List.fold ~f:self#text_item ~init:acc text

  method text_item acc = function
    | Raw _ -> acc
    | Bold text -> List.fold ~f:self#text_item ~init:acc text
    | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
    | Quote doc -> self#doc acc doc
end
