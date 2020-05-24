val raise_syntax_error: ?line:int -> string -> 'a
val raise_name_error: ?line:int -> string -> 'a
val raise_type_error: ?line:int -> string -> 'a
val raise_import_error: ?line:int -> string -> 'a
val raise_unexpected_type_error: ?line:int -> string -> string -> 'a
val raise_unexpected_type_error_with_name: ?line:int -> string -> string -> string -> 'a

val try_update_err: int -> (unit -> 'a) -> 'a
val try_catch: ?raise_errors:bool -> out_channel -> (unit -> 'a) -> 'a
val try_execute: (unit -> 'a) -> on_success:('a -> unit) -> on_failure:(string * int -> unit) -> unit

val get_errors: (unit -> 'a) -> (string * int) list
