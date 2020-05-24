type warning = {
  message: string;
  severity: int;
  line: int option
}

let final = Queue.create()
let staged  = Queue.create()


let add_warning ?line message severity =
  let warning = { message; severity; line } in
  match line with
    | None -> Queue.add warning staged
    | _ -> Queue.add warning final

let try_update_warnings ~line =
  let line = Some line in
  staged |> Queue.iter
    (fun { message; severity; _ } -> Queue.add { message; severity; line } final);
  Queue.clear staged

let get_warnings () =
  Utils.list_of_queue final |> List.map (fun { message; severity = _; line} -> message, Option.get line)

let print_warning min_severity = function
    { message; severity; line = Some line} when severity >= min_severity ->
    Printf.fprintf stderr "Warning at line %d: %s.\n" line message
  | _ -> ()

let print_warnings ~severity =
  final |> Queue.iter
    (fun warning -> print_warning severity warning)
