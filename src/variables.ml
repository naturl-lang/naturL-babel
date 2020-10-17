open Utils
open Structures

(* Map of all declared variables (initialized during the parsing process) *)
let declared_variables: Location.t StringMap.t ref = ref StringMap.empty

let is_var_declared name = StringMap.mem name !declared_variables

let declare_variable ?(overwrite = false) name location =
  let name = String.trim name in
  if is_alphanum name && (overwrite || not (is_var_declared name)) then
    declared_variables := !declared_variables |> StringMap.add name location

let print_declared_variables () =
  let rec print_declared_variable: (string * Location.t) list -> unit = function
    | (name, location) :: t ->
      Printf.printf "\tVariable %s defined at line %d, columns %d-%d\n"
        name location.line location.range.start location.range.end_;
      print_declared_variable t
    | _ -> ()
  in print_declared_variable (StringMap.bindings !declared_variables)
