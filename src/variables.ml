open Utils
open Structures

type var_def = {
  name: string;
  type_: Type.t;
  location: Location.t
}

module Var_def_set = Set.Make(struct type t = var_def let compare = compare end)

let parsing_variables = ref StringMap.empty
let add_parsing_variable name (type_: string) (line: int) =
  !parsing_variables |> StringMap.add name (type_, line)
