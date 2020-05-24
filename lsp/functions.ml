open Types
open Src.Utils
open Src.Translation

let definition oc id (params: DefinitionParams.t) =
  let uri = params.textDocument.uri in
  let content = Environment.get_content uri in
  try
    let index = get_index_at (params.position.line - 1) (params.position.character - 1) content in
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
              start = { line = line_infos.line; character = 1};
              end_ = { line = line_infos.line; character = get_line_length (line_infos.line - 1) content }
            }
          }
          in Sender.send_response oc (Jsonrpc.Response.ok id (Location.yojson_of_t location)))
      ~on_failure: (function msg, line ->
          Sender.send_response oc
            Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make
                                                                 ~code:Code.InternalError
                                                                 ~message: ("Error at line " ^ (string_of_int line) ^ ": " ^ msg) ())))
  with Not_found -> Sender.send_response oc
                      Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make ~code:Code.InvalidParams ~message: "Invalid position" ()))

let completion oc id (params: CompletionParams.t) =
  let uri = params.textDocument.uri in
  let content = Environment.get_content uri in
  try
    let index = get_index_at (params.position.line - 1) (params.position.character - 1) content in
    Src__Errors.try_execute (fun () -> get_code_context ~raise_errors:true ~max_index:index uri content)
      ~on_success:(fun context ->
          let items = context.vars |> StringMap.bindings |> List.map (function name, type_ ->
              let open CompletionItem in
              {
                label = name;
                detail = Some (Src.Structures.Type.to_string type_)
              })
          in Sender.send_response oc (Jsonrpc.Response.ok id (`List (items |> List.map CompletionItem.yojson_of_t))))
      ~on_failure:(function msg, line ->
          Sender.send_response oc
            Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make
                                                                 ~code:Code.InternalError
                                                                 ~message: ("Error at line " ^ (string_of_int line) ^ ": " ^ msg) ())))
  with Not_found -> Sender.send_response oc
                      Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make ~code:Code.InvalidParams ~message: "Invalid position" ()))

let diagnostic oc =
  (* Convert a couple (message, line) to a diagnostic *)
  let to_diagnostic severity content (message, line) : Diagnostic.t =
    {
      range = {
        start = { line; character = 1 };
        end_ = { line; character = get_line_length (line - 1) content }
      };
      severity = Some severity;
      message
    }
  in
  try
    let diagnostics = !Environment.files |> Environment.UriMap.bindings |> List.map
                        (function _, content ->
                           let context = { Src__Structures.empty_context with code = content } in
                           let diagnostics = (Src__Errors.get_errors (fun () -> eval_code context ) |> List.map (to_diagnostic Warning content))
                           in diagnostics @
                           (Src__Warnings.get_warnings () |> List.map (to_diagnostic Warning content)))
                      |> List.flatten
                      |> List.map Diagnostic.yojson_of_t
    in Sender.send_notification oc Jsonrpc.Request.(make ~id:None ~params:(Some (`List diagnostics)) ~method_:"textDocument/publishDiagnostics")
  with Not_found -> ()
