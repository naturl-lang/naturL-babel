type import = {
  namespace: string;
  element: string option
}

module ImportSet = Set.Make(struct type t = import let compare = compare end)

let imports = ref ImportSet.empty

let add_import namespace element =
  imports := ImportSet.add { namespace; element } !imports

let are_imports_empty () = ImportSet.is_empty !imports

let is_namespace_imported namespace =
  ImportSet.exists (fun { namespace = n; _ } -> namespace = n) !imports

let get_imports () =
  let rec __get_imports = function
      [] -> ""
    | { namespace; element = Some element } :: t -> "from " ^ namespace ^ " import " ^ element ^ "\n" ^ __get_imports t
    | { namespace; element = None } :: t -> "import " ^ namespace ^ "\n" ^ __get_imports t
  in __get_imports (ImportSet.elements !imports)
