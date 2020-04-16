type warning = {
  message: string;
  severity: int;
  line: int option
}

let final = Queue.create()
let staged  = Queue.create()


let add_warning ?line ?(severity = 0) ~message =
  let warning = { message; severity; line } in
  match line with
    | None -> Queue.add warning staged
    | _ -> Queue.add warning final

let try_update_warnings ~line =
  let line = Some line in
  staged |> Queue.iter
    (fun { message; severity; _ } -> Queue.add { message; severity; line } final);
  Queue.clear staged

let warnings = Queue.copy final
