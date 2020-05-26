type warning
val get_warnings: unit -> (string * int) list
val add_warning: ?line:int -> string -> int -> unit
val try_update_warnings: line:int -> unit
val print_warnings: severity:int -> unit