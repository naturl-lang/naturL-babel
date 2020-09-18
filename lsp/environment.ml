open Types

module UriMap = Map.Make(struct
    type t = Types.DocumentUri.t
    let compare = compare
  end)

let client_capabilities = ref None
let files = ref UriMap.empty
let initialized = ref false
let shutdown = ref false


(* If the server tries to access client capabilities, it means that it is initialized so they can't be None *)
let get_client_capabilities () =
  Option.get !client_capabilities
let set_client_capabilities (capabilities: ClientCapabilities.t) =
  client_capabilities := Some capabilities

let get_content uri =
  match !files |> UriMap.find_opt uri with
  | None -> Uri.read_uri uri
  | Some content -> content

let add_uri uri (content: string) =
  files := !files |> UriMap.add uri content
let update_uri uri new_content =
  files := !files |> UriMap.update uri (fun _ -> Some new_content)
let remove_uri uri =
  files := !files |> UriMap.remove uri


let initialize () = initialized := true
let initialized () = !initialized = true

let is_shutdown () = !shutdown = true
let shutdown () = shutdown := true
