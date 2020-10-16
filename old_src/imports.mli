type import
module ImportSet: Set.S
val is_imported: bool ref
val imports: ImportSet.t ref
val add_import: ?userdefined:bool -> string -> string option -> unit
val are_imports_empty: unit -> bool
val is_namespace_imported: string -> bool
val get_imports: unit -> string
val clear_imports: unit -> unit
