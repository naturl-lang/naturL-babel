module Location = struct
  type range = {
    start: int;
    end_: int
  }

  type t = {
    line: int;
    range: range;
  }

  let to_string (location: t) =
    "line " ^ (string_of_int location.line) ^ ", characters " ^ (string_of_int location.range.start) ^  "-" ^ string_of_int location.range.end_

  let to_string_fr location =
    "ligne " ^ (string_of_int location.line) ^ ", caractères " ^ (string_of_int location.range.start) ^  "-" ^ string_of_int location.range.end_
end
