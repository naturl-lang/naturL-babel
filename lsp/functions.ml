open Js_of_ocaml
open Types
open Src.Utils
open Src.Parser
open Src.Check_semantic
module Variables = Src.Variables

let definition callback id (params: DefinitionParams.t) =
  let uri = params.textDocument.uri in
  try
    let content = Environment.get_content uri in
    try
      let index = get_index_at params.position.line params.position.character content in
      Src.Errors.try_execute (fun () -> content |> parse_body |> check_semantic)
        ~on_success: (fun () ->
            let word = get_word_at_index index content
            and _, variables = !Variables.locale_variables
                               |> IntMap.find_last (fun l -> l <= params.position.line + 1)
            in
            let location = StringMap.find word variables
            in
            let location: Location.t = {
              uri;
              range = {
                start = { line = location.line - 1; character = location.range.start - 1};
                end_ = { line = location.line - 1; character = location.range.end_ - 1}
              }
            }
            in Sender.send_response callback (Jsonrpc.Response.ok id (Location.yojson_of_t location)))
        ~on_failure: (function msg, location ->
            Sender.send_response callback
              Jsonrpc.Response.
                (error id Jsonrpc.Response.Error.
                            (make
                               ~code:Code.InternalError
                               ~message: ("Error at line " ^ (string_of_int location.line) ^ ": " ^ msg) ())))
    with Not_found -> Sender.send_response callback
                        Jsonrpc.Response.
                          (error id Jsonrpc.Response.Error.
                                      (make
                                         ~code:Code.InvalidParams
                                         ~message: ("Invalid position (line " ^ (string_of_int params.position.line) ^
                                                    ", character " ^ (string_of_int params.position.character) ^ ")") ()))
  with Not_found -> Sender.send_response callback
                      Jsonrpc.Response.
                        (error id
                           Jsonrpc.Response.Error.
                             (make
                                ~code:Code.InternalError
                                ~message: ("Unknown uri " ^ params.textDocument.uri ^ ". Have you sent an open notification ?") ()))

let completion callback id (params: CompletionParams.t) =
  let send_completion items =
    let items = items @ Src.Builtins.accessible_keywords
                |> List.map (function (name, type_) ->
                    let open CompletionItem in
                    {
                      label = name;
                      detail = Some (Src.Type.to_string type_)
                    })
    in Sender.send_response callback (Jsonrpc.Response.ok id (`List (items |> List.map CompletionItem.yojson_of_t)))
    in
  let uri = params.textDocument.uri in
  try
    let content = Environment.get_content uri in
    Src.Errors.try_execute (fun () -> content |> parse_body |> check_semantic)
      ~on_success:(fun () ->
          let _, variables = !Variables.locale_variables
                             |> IntMap.find_last (fun l -> l <= params.position.line + 1)
          in let variables = variables
                             |> StringMap.mapi (fun name -> fun location ->
                                 !Variables.defined_variables
                                 |> Variables.var_type_opt name location
                                 |> Option.value ~default:Src.Type.Any)
                           |> StringMap.bindings
          in
          send_completion variables)
      ~on_failure:(fun _ -> send_completion [])  (* Even when there is an error, the builtin functions are sent *)
  with Not_found -> send_completion []

let reformat callback id (params: DocumentFormattingParams.t) =
  try
    let content = Environment.get_content params.textDocument.uri in
    let formatted = Reformat.reformat content params.options.tabSize params.options.insertSpaces in
    let end_line, end_char = get_last_position content in
    let edits = [TextEdit.{
        range = {
          start = { line = 0; character = 0 };
          end_ = { line = end_line; character = end_char}
        };
        newText = formatted
      }] |> List.map TextEdit.yojson_of_t
    in Sender.send_response callback (Jsonrpc.Response.ok id (`List edits))
  with Not_found ->  Sender.send_response callback
                       Jsonrpc.Response.(error id Jsonrpc.Response.Error.(
                           make
                             ~code:Code.InternalError
                             ~message: ("Unknown uri " ^ params.textDocument.uri ^
                                        ". Have you sent an open notification ?") ()))

let diagnostic callback =
  (* Convert a couple (message, line) to a diagnostic *)
  let to_diagnostic severity (message, (location: Src.Structures.Location.t))  : Diagnostic.t =
    {
      range = {
        start = { line = location.line - 1; character = location.range.start - 1 };
        end_ = { line = location.line - 1; character = location.range.end_ - 1 }
      };
      severity = Some severity;
      message
    }
  in
  try
      !Environment.files |> Environment.UriMap.iter
      (fun uri -> fun  _ ->
        let diagnostics =
          (Environment.get_content uri |> parse_body |> check_semantic;
           Src.Errors.get_errors ()
           |> List.map @@ to_diagnostic Error)
          @
          (Src.Warnings.get_warnings ()
           |> List.map @@ to_diagnostic Warning) in
        let params: PublishDiagnosticsParams.t = {
          uri;
          version = None;
          diagnostics
        } in
        let json = PublishDiagnosticsParams.yojson_of_t params in
        Sender.send_notification callback
           Jsonrpc.Request.(make ~id:None ~params:(Some json) ~method_:"textDocument/publishDiagnostics"))
  with Not_found -> ()

let transpile callback error_callback _ code =
  let code = Js.to_string code in
  Src.Errors.try_execute
    (fun () ->
       Src.Python.naturl_to_python
         ~annotate:true
         ~code
       |> Js.string)
    ~on_success: callback
    ~on_failure: (fun (msg, line) -> error_callback (Js.string msg) line)
