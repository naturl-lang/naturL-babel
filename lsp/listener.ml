let handle_notification : Client_notification.t -> unit = function
  | Initialized -> Environment.initialize ()
  | Exit -> Environment.exit ()
  | TextDocumentDidOpen params -> Environment.add_uri params.textDocument.uri params.textDocument.text
  | TextDocumentDidChange params -> Environment.update_uri params.textDocument.uri params.contentChanged.text
  | TextDocumentDidClose params -> Environment.remove_uri params.textDocument.uri
  | Unknown_notification name -> print_endline ("Unknown notification: " ^ name)

let handle_request id : Request.t -> unit = function
  | Shutdown -> print_endline "shutdown"
  | Initialize _ -> print_endline "initialized"
  | TextDocumentDefinition _ -> print_endline "definition"
  | TextDocumentCompletion _ -> print_endline "completion"
  | UnknownRequest name -> Sender.send_response stdout
            Jsonrpc.(Response.error id Response.Error.(make ~code:MethodNotFound ~message:("Unknown method " ^ name) ()))


let start ic =
  let rec read_content ic length =
    if length = 0 then
      ""
    else
      let s = String.make 1 (input_char ic) in
      s ^ read_content ic (length - 1)
  in
  let header = Header.read ic
  in let content = read_content ic header.content_length in
  let yojson = Yojson.Safe.from_string content in
  let jsonrpc = Jsonrpc.Request.t_of_yojson yojson in
  match jsonrpc.id with
  (* Request *)
  | Some id -> (match Request.of_jsonrpc jsonrpc with
      | Ok request ->
        if Environment.initialized () || (function Request.Initialize _ -> true | _ -> false) request then
          handle_request id request
        else
          Sender.send_response stdout
            Jsonrpc.(Response.error id Response.Error.(make ~code:ServerNotInitialized ~message:"Please first initialize the server" ()))
      | Error _ -> print_endline "error")
  (* Notification *)
  | None -> (match Client_notification.of_jsonrpc jsonrpc with
      | Ok notification -> handle_notification notification
      | Error _ -> print_endline "error")
