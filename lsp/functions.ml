open Types
open Src.Utils
open Src.Translation

let definition oc id (params: DefinitionParams.t) =
  let uri = params.textDocument.uri in
  try
    let content = Environment.get_content uri in
    try
      let index = get_index_at params.position.line params.position.character content in
      Src__Errors.try_execute (fun () -> get_code_context ~raise_errors:true ~max_index:index uri content)
        ~on_success: (fun context ->
            let word = get_word_at_index index content in
            let line_infos = match context.defs |> StringMap.find_opt word with
              | Some value -> value
              | None -> let open Src.Structures in
                context.defs |> StringMap.find (get_current_class_name context ^ "." ^ word)
            in
            let location: Location.t = {
              uri;
              range = {
                start = { line = line_infos.line - 1; character = 0};
                end_ = { line = line_infos.line - 1; character = get_line_length (line_infos.line - 1) content - 1 }
              }
            }
            in Sender.send_response oc (Jsonrpc.Response.ok id (Location.yojson_of_t location)))
        ~on_failure: (function msg, line ->
            Sender.send_response oc
              Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make
                                                                   ~code:Code.InternalError
                                                                   ~message: ("Error at line " ^ (string_of_int line) ^ ": " ^ msg) ())))
    with Not_found -> Sender.send_response oc
                        Jsonrpc.Response.
                          (error id Jsonrpc.Response.Error.
                                      (make
                                         ~code:Code.InvalidParams
                                         ~message: ("Invalid position (line " ^ (string_of_int params.position.line) ^ ", character " ^ (string_of_int params.position.character) ^ ")") ()))
  with Not_found -> Sender.send_response oc
                      Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make
                                                                           ~code:Code.InternalError
                                                                           ~message: ("Unknown uri " ^ params.textDocument.uri ^ ". Have you sent an open notification ?") ()))

let completion oc id (params: CompletionParams.t) =
  let send_completion items =
    let items = items @ Src__Builtins.accessible_keywords
                |> List.map (function (name, type_) ->
                    let open CompletionItem in
                    {
                      label = name;
                      detail = Some (Src.Structures.Type.to_string type_)
                    })
    in Sender.send_response oc (Jsonrpc.Response.ok id (`List (items |> List.map CompletionItem.yojson_of_t)))
    in
  let uri = params.textDocument.uri in
  let content = Environment.get_content uri in
  try
    let index = get_index_at params.position.line params.position.character content - 1 in
    Src__Errors.try_execute (fun () -> get_code_context ~raise_errors:true ~max_index:index uri content)
      ~on_success:(fun context -> send_completion (StringMap.bindings context.vars))
      ~on_failure:(fun _ -> send_completion [])  (* Even when there is an error, the builtin functions are sent *)
  with Not_found -> send_completion []

let diagnostic oc =
  (* Convert a couple (message, line) to a diagnostic *)
  let to_diagnostic severity content (message, line) : Diagnostic.t =
    {
      range = {
        start = { line = line - 1; character = 0 };
        end_ = { line = line - 1; character = get_line_length (line - 1) content - 1 }
      };
      severity = Some severity;
      message
    }
  in
  try
    !Environment.files |> Environment.UriMap.bindings |> List.iter
      (function uri, content ->
         let context = { Src__Structures.empty_context with code = format_code content } in
         let diagnostics = (Src__Errors.get_errors (fun () -> eval_code context ) |> List.map (to_diagnostic Error content)) in
         let diagnostics = diagnostics @
                           (Src__Warnings.get_warnings () |> List.map (to_diagnostic Warning content)) in
         let params: PublishDiagnosticsParams.t = {
             uri;
             version = None;
             diagnostics
           } in
         let json = PublishDiagnosticsParams.yojson_of_t params in
         Sender.send_notification oc Jsonrpc.Request.(make ~id:None ~params:(Some json) ~method_:"textDocument/publishDiagnostics"))
  with Not_found -> ()
