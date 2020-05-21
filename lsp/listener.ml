let handle_notification : Client_notification.t -> unit = function
  | Initialized -> Environment.initialize ()
  | Exit -> let code = if Environment.is_shutdown () then 0 else 1 in exit code
  | TextDocumentDidOpen params -> Environment.add_uri params.textDocument.uri params.textDocument.text
  | TextDocumentDidChange params -> Environment.update_uri params.textDocument.uri params.contentChanged.text
  | TextDocumentDidClose params -> Environment.remove_uri params.textDocument.uri
  | Unknown_notification _ -> ()

let handle_request id : Request.t -> unit = function
  | Shutdown -> Environment.shutdown ()
  | Initialize params -> Environment.set_client_capabilities params.capabilities; Sender.initialize stdout id
  | TextDocumentDefinition params -> Functions.definition stdout params
  | TextDocumentCompletion params -> Functions.completion stdout params
  | UnknownRequest name -> Sender.send_response stdout
            Jsonrpc.(Response.error id Response.Error.(make ~code:MethodNotFound ~message:("Unknown method " ^ name) ()))


let rec listen ic =
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
  begin
    match jsonrpc.id with
    (* Request *)
    | Some id -> (try (match Request.of_jsonrpc jsonrpc with
        | Ok request ->
          if Environment.is_shutdown () then
            Sender.send_response stdout
              Jsonrpc.(Response.error id Response.Error.(make ~code:InvalidRequest ~message:"The server is shut down" ()))
          else if Environment.initialized () || (function Request.Initialize _ -> true | _ -> false) request then
            handle_request id request
          else
            Sender.send_response stdout
              Jsonrpc.(Response.error id Response.Error.(make ~code:ServerNotInitialized ~message:"Please first initialize the server" ()))
        | Error _ -> print_endline "error")
       with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error _ ->
         Sender.send_response stdout
           Jsonrpc.(Response.error id Response.Error.(make ~code:InvalidRequest ~message:"Invalid request format" ())))
    (* Notification *)
    | None -> (match Client_notification.of_jsonrpc jsonrpc with
        | Ok notification ->
          if not (Environment.is_shutdown () && notification <> Exit) || notification = Initialized &&
             (Environment.initialized () || !Environment.client_capabilities <> None && notification = Initialized) then
            handle_notification notification
        | Error _ -> print_endline "error")
  end;
  print_endline ".";
  listen ic

let start = listen
