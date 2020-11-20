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

(* Map of all declared variables (initialized during the parsing process) *)
let declared_variables: VarSet.t ref = ref VarSet.empty

let is_var_declared searched_name =
  !declared_variables
  |> VarSet.exists (fun { name; location = _; type_ = _  } -> name = searched_name)

let var_type_opt searched_name searched_location vars =
  let (let*) = Option.bind in
  let* var =
    vars |> find_in_set_opt (module VarSet)
      (fun { name; location; type_ = _ } ->
         name = searched_name && location = searched_location)
  in var.type_

let declare_variable name location =
  let name = String.trim name in
  declared_variables :=
    !declared_variables
    |> VarSet.add { name; location; type_ = None }

let update_type searched_name searched_location type_ =
  let searched_name = String.trim searched_name
  and type_ = Some type_ in
  declared_variables :=
    !declared_variables
    |> VarSet.map (function { name; location; type_ = _ } as var ->
        if name = searched_name && location = searched_location then
          { var with type_ }
        else
          var)

let print_declared_variables () =
  let rec print_declared_variable = function
    | { name; location; type_ = _  } :: t ->
      Printf.printf "\tVariable %s defined at line %d, columns %d-%d\n"
        name location.line location.range.start location.range.end_;
      print_declared_variable t
    | _ -> ()
  in print_declared_variable (VarSet.elements !declared_variables)
