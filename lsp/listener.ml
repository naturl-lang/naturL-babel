let handle_notification : Client_notification.t -> unit = function
  | Initialized -> Environment.initialize ()
  | Exit -> let code = if Environment.is_shutdown () then 0 else 1 in exit code
  | TextDocumentDidOpen params -> let uri = params.textDocument.uri in
    Environment.add_uri uri params.textDocument.text; Functions.diagnostic stdout
  | TextDocumentDidChange params -> let uri = params.textDocument.uri in
    Environment.add_uri uri (List.hd params.contentChanges).text; Functions.diagnostic stdout
  | TextDocumentDidClose params -> Environment.remove_uri params.textDocument.uri
  | Unknown_notification _ -> ()

let handle_request id : Request.t -> unit = function
  | Shutdown -> Environment.shutdown (); Sender.send_response stdout Jsonrpc.(Response.ok id `Null)
  | Initialize params -> Environment.set_client_capabilities params.capabilities; Sender.initialize stdout id ;
    (match params.initializationOptions with
       Some lang -> Internationalisation__Translation.set_lang_of_string lang
     | None -> ())
  | TextDocumentDefinition params -> Functions.definition stdout id params
  | TextDocumentCompletion params -> Functions.completion stdout id params
  | TextDocumentFormat params -> Functions.reformat stdout id params
  | UnknownRequest name -> Sender.send_response stdout
                             Jsonrpc.(Response.error id Response.Error.(make ~code:MethodNotFound ~message:("Unknown method " ^ name) ()))


let rec listen ic =
  let read_content ic length =
    let rec read_content ic length accu =
      let line = input_line ic in
      let length = length - String.length line - (if Sys.unix then 1 else 2)
      and accu = accu ^ line in
      if length <= 0 then
        accu
      else
        read_content ic length accu

    in read_content ic length ""
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
  listen ic

let start = listen
