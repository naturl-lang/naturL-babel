open Structures

type warning = {
  message: string;
  severity: int;
  location: Location.t option
}

let final = Queue.create()
let staged  = Queue.create()


let add_warning ?location message severity =
  let warning = { message; severity; location } in
  match location with
    | None -> Queue.add warning staged
    | _ -> Queue.add warning final

let try_update_warnings ~location =
  let location = Some location in
  staged |> Queue.iter
    (fun { message; severity; _ } -> Queue.add { message; severity; location } final);
  Queue.clear staged

let get_warnings () =
  Utils.list_of_queue final |> List.map (fun { message; severity = _; location} -> message, Option.get location)

let print_warning min_severity = function
    { message; severity; location = Some location} when severity >= min_severity ->
    prerr_endline (Internationalisation.Translation.get_string
                     Warning ^ (Location.to_string location) ^ " : " ^ message)
  | _ -> ()

let print_warnings ~severity =
  final |> Queue.iter
    (fun warning -> print_warning severity warning)
