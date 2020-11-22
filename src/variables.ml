open Utils
open Structures

type variable = {
  name: string;
  location: Location.t;
  type_: Type.t option
}

module VarSet = Set.Make (
  struct
    type t = variable
    let compare = compare
  end)

(* Map of all defined variables (initialized during the parsing process) *)
let defined_variables = ref VarSet.empty

let is_var_defined searched_name =
  !defined_variables
  |> VarSet.exists (fun { name; location = _; type_ = _  } -> name = searched_name)

let var_type_opt searched_name searched_location vars =
  let (let*) = Option.bind in
  let* var =
    vars |> find_in_set_opt (module VarSet)
      (fun { name; location; type_ = _ } ->
         name = searched_name && location = searched_location)
  in var.type_

let define_variable name location =
  (* If the variable is not declared, *)
  let name = String.trim name in
  defined_variables :=
    !defined_variables
    |> VarSet.add { name; location; type_ = None }

let update_type searched_name searched_location type_ =
  let searched_name = String.trim searched_name
  and type_ = Some type_ in
  defined_variables :=
    !defined_variables
    |> VarSet.map (function { name; location; type_ = _ } as var ->
        if name = searched_name && location = searched_location then
          { var with type_ }
        else
          var)

let print_defined_variables () =
  let rec print_defined_variable = function
    | { name; location; type_ = _  } :: t ->
      Printf.printf "\tVariable %s defined at line %d, columns %d-%d\n"
        name location.line location.range.start location.range.end_;
      print_defined_variable t
    | _ -> ()
  in print_defined_variable (VarSet.elements !defined_variables)

(*********************************)

(* Associate to each line the variables that are defined *)
let locale_variables = ref IntMap.empty

let add_locale_variables location (map: Location.t StringMap.t) =
  locale_variables := !locale_variables |> IntMap.add location map

let get_locale_variables location =
  !locale_variables
  |> IntMap.find_opt location
  |> Option.value ~default: StringMap.empty

(*****************************************************************)

let reset () =
  defined_variables := VarSet.empty;
  locale_variables := IntMap.empty
