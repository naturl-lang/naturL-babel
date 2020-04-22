type import
module ImportSet: Set.S
val imports: ImportSet.t ref
val add_import: string -> string option -> unit
val are_imports_empty: unit -> bool
val is_namespace_imported: string -> bool
val get_imports: unit -> string
val clear_imports: unit -> unit
