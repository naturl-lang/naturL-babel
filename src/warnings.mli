type warning
val add_warning: ?line:int -> message:string -> unit
val try_update_warnings: line:int -> unit
val warnings: warning Queue.t
