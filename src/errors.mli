val syntax_error : string -> int -> out_channel -> 'a
val type_error : string -> int -> out_channel -> 'a
val name_error : string -> int -> out_channel -> 'a

val raise_syntax_error: ?line: int -> string -> 'a
val raise_name_error: ?line: int -> string -> 'a
val raise_type_error: ?line: int -> string -> 'a
val raise_unexpected_type_error: ?line: int -> string -> string -> 'a
val raise_unexpected_type_error_with_name: ?line: int -> string -> string -> string -> 'a

val try_update_err: int -> (unit -> 'a) -> 'a
val try_catch : out_channel -> (unit -> 'a) -> 'a
