open Structures

val raise_syntax_error: ?location:Location.t -> string -> 'a
val raise_name_error: ?location:Location.t -> string -> 'a
val raise_type_error: ?location:Location.t -> string -> 'a
val raise_import_error: ?location:Location.t -> string -> 'a
val raise_unexpected_type_error: ?location:Location.t -> string -> string -> 'a
val raise_unexpected_type_error_with_name: ?location:Location.t -> string -> string -> string -> 'a
val raise_function_error: ?location:Location.t -> string -> (string * string) list -> string list -> 'a
val raise_bug: string -> 'a

val try_update_err: Location.t -> (unit -> 'a) -> 'a
val try_catch: ?raise_errors:bool -> out_channel -> (unit -> 'a) -> 'a
val try_execute: (unit -> 'a) -> on_success:('a -> unit) -> on_failure:(string * Location.t -> unit) -> unit

val get_errors: (unit -> 'a) -> (string * Location.t) list
