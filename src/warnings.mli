type warning
val add_warning: ?line:int -> string -> int -> unit
val try_update_warnings: line:int -> unit
val print_warnings: severity:int -> unit
