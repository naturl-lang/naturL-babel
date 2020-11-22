type import_mode =
    Write_nothing
  | Moderated
  | Overwrite

let debug = ref false
let import_mode = ref Overwrite

let set_import_mode mode =
  import_mode :=
    match mode with
      "write-nothing" -> Write_nothing
    | "moderated" -> Moderated
    | "overwrite" -> Overwrite
    | _ -> assert false

let naturL_path =
  match Sys.getenv_opt "NATURLPATH" with
  | Some _ as s -> s
  | None -> prerr_endline "The variable NATURLPATH is not defined"; None
