val syntax_error : string -> int -> out_channel -> 'a
val type_error : string -> int -> out_channel -> 'a
val name_error : string -> int -> out_channel -> 'a

exception SyntaxError of string
exception TypeError of string
val raise_unexpected_type_error: string -> string -> 'a
exception NameError of string

val translate_exception : exn -> int -> out_channel -> 'a
