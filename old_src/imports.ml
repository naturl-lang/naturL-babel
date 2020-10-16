type import = {
  namespace: string;
  element: string option
}

module ImportSet = Set.Make(struct type t = import let compare = compare end)

let is_imported = ref false
let imports = ref ImportSet.empty

let clear_imports () = imports := ImportSet.empty

let add_import ?(userdefined = false) namespace element =
  if not !is_imported || not userdefined then
    imports := ImportSet.add { namespace; element } !imports
  else if element = None then
    imports := ImportSet.add { namespace = "."; element = Some namespace} !imports
  else
    imports := ImportSet.add { namespace = "." ^ namespace; element } !imports

let are_imports_empty () = ImportSet.is_empty !imports

let is_namespace_imported namespace =
  ImportSet.exists (fun { namespace = n; element } -> namespace = n || n = "." && element = Some namespace) !imports

let rec write_imports = function
    [] -> ""
  | { namespace; element = Some element } :: t -> "from " ^ namespace ^ " import " ^ element ^ "\n" ^ write_imports t
  | { namespace; element = None } :: t -> "import " ^ namespace ^ "\n" ^ write_imports t

let get_imports () =
  write_imports (ImportSet.elements !imports)
