module UriMap = Map.Make(struct
    type t = Types.DocumentUri.t
    let compare = compare
  end)

let files = ref UriMap.empty
let initialized = ref false
let exited = ref false


let add_uri uri (content: string) =
  files := !files |> UriMap.add uri content

let update_uri uri new_content =
  files := !files |> UriMap.update uri (fun _ -> Some new_content)

let remove_uri uri =
  files := !files |> UriMap.remove uri

let initialize () = initialized := true
let initialized () = !initialized = true

let exit () = exited := true
let exited = !exited = true
