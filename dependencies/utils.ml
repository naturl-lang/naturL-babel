let get_indentation depth =
  let rec _get_indentation depth =
    if depth = 0 then
      ""
    else
      "    " ^ _get_indentation (depth - 1)
  in if depth < 0 then
    failwith "Internal error: get_indentation: depth can't be negative"
  else
    _get_indentation depth

let rec find_assoc key = function
    | (k, value) :: t -> if key = k then Some value else find_assoc key t
    | [] -> None
