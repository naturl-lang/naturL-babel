type warning = { message: string; line: int option }

let final = Queue.create()
let staged  = Queue.create()


let add_warning ?line ~message =
  let warning = { message; line } in
  match line with
    | None -> Queue.add warning staged
    | _ -> Queue.add warning final

let try_update_warnings ~line =
  let line = Some line in
  staged |> Queue.iter
    (fun { message; _ } -> Queue.add { message; line } final);
  Queue.clear staged

let warnings = Queue.copy final
