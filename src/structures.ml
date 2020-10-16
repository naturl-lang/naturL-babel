module Location = struct
  type range = {
    start: int;
    end_: int
  }

  type t = {
    line: int;
    range: range;
  }
end
