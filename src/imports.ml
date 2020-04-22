type import = {
  namespace: string;
  element: string option
}

module ImportSet = Set.Make(struct type t = import let compare = compare end)


let imports_bakup = ref ImportSet.empty
let imports = ref ImportSet.empty


let save_imports () = imports_bakup := !imports
let clear_imports () = imports := ImportSet.empty
let restore_imports () = imports := !imports_bakup


let add_import namespace element =
  imports := ImportSet.add { namespace; element } !imports

let are_imports_empty () = ImportSet.is_empty !imports

let is_namespace_imported namespace =
  ImportSet.exists (fun { namespace = n; _ } -> namespace = n) !imports

let rec write_imports = function
    [] -> ""
  | { namespace; element = Some element } :: t -> "from " ^ namespace ^ " import " ^ element ^ "\n" ^ write_imports t
  | { namespace; element = None } :: t -> "import " ^ namespace ^ "\n" ^ write_imports t

let get_imports () =
  write_imports (ImportSet.elements !imports)
