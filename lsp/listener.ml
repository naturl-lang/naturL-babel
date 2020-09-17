let handle_notification ic oc : Client_notification.t -> unit = function
  | Initialized -> Environment.initialize ()
  | Exit ->
     (try
        flush oc;
        close_in ic
      with _ -> close_in_noerr ic);
     let code = if Environment.is_shutdown () then 0 else 1 in exit code
  | TextDocumentDidOpen params -> let uri = params.textDocument.uri in
                                  Environment.add_uri uri params.textDocument.text; Functions.diagnostic oc
  | TextDocumentDidChange params -> let uri = params.textDocument.uri in
                                    Environment.add_uri uri (List.hd params.contentChanges).text; Functions.diagnostic oc
  | TextDocumentDidClose params -> Environment.remove_uri params.textDocument.uri
  | Unknown_notification _ -> ()

let handle_request oc id : Request.t -> unit = function
  | Shutdown -> Environment.shutdown (); Sender.send_response oc Jsonrpc.(Response.ok id `Null)
  | Initialize params -> Environment.set_client_capabilities params.capabilities; Sender.initialize oc id ;
    (match params.initializationOptions with
       Some lang -> Internationalisation__Translation.set_lang_of_string lang
     | None -> ())
  | TextDocumentDefinition params -> Functions.definition oc id params
  | TextDocumentCompletion params -> Functions.completion oc id params
  | TextDocumentFormat params -> Functions.reformat oc id params
  | UnknownRequest name -> Sender.send_response oc
                             Jsonrpc.(Response.error id Response.Error.(make ~code:MethodNotFound ~message:("Unknown method " ^ name) ()))


let rec listen ic oc =
  let read_content ic =
    let rec read_content ic accu =
    try
      let s = String.make 1 (input_char ic) in
      read_content ic (accu ^ s)
    with End_of_file -> accu
    in read_content ic ""
  in
  print_string "Reading header...";
  let _ = Header.read ic in
  print_endline "Done.";
  let content = read_content ic in
  print_endline (String.make 10 '*' ^ "Received message " ^ content ^ (String.make 10 '*'));
  let yojson = Yojson.Safe.from_string content in
  let jsonrpc = Jsonrpc.Request.t_of_yojson yojson in
  begin
    match jsonrpc.id with
    (* Request *)
    | Some id -> (try (match Request.of_jsonrpc jsonrpc with
        | Ok request ->
          if Environment.is_shutdown () then
            Sender.send_response oc
              Jsonrpc.(Response.error id Response.Error.(make ~code:InvalidRequest ~message:"The server is shut down" ()))
          else if Environment.initialized () || (function Request.Initialize _ -> true | _ -> false) request then
            handle_request oc id request
          else
            Sender.send_response oc
              Jsonrpc.(Response.error id Response.Error.(make ~code:ServerNotInitialized ~message:"Please first initialize the server" ()))
        | Error _ -> prerr_endline "error")
       with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error _ ->
         Sender.send_response oc
           Jsonrpc.(Response.error id Response.Error.(make ~code:InvalidRequest ~message:"Invalid request format" ())))
    (* Notification *)
    | None -> (match Client_notification.of_jsonrpc jsonrpc with
        | Ok notification ->
           if not (Environment.is_shutdown () && notification <> Exit) ||
                notification = Initialized &&
                  (Environment.initialized () || !Environment.client_capabilities <> None && notification = Initialized) then
            handle_notification ic oc notification
        | Error _ -> prerr_endline "error")
  end;
  listen ic oc

let start = listen
