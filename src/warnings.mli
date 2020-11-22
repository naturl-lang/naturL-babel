open Structures

type warning
val clear_warnings: unit -> unit
val get_warnings: unit -> (string * Location.t) list
val add_warning: ?location:Location.t -> string -> int -> unit
val try_update_warnings: location:Location.t -> unit
val print_warnings: severity:int -> unit
