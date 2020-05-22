open Types
open Src.Utils
open Src.Translation

let definition oc id (params: DefinitionParams.t) =
  let uri = params.textDocument.uri in
  let content = Environment.get_content uri in
  try
    let index = get_index_at (params.position.line - 1) (params.position.character - 1) content in
    let context = get_code_context ~max_index:index uri content in
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
    in Sender.send_response oc (Jsonrpc.Response.ok id (Location.yojson_of_t location))
  with Not_found -> Sender.send_response oc Jsonrpc.Response.(error id Jsonrpc.Response.Error.(make ~code:Code.InvalidParams ~message: "Can't get definition" ()))

let completion oc id (params: CompletionParams.t) =
  let uri = params.textDocument.uri in
  let content = Environment.get_content uri in
  let index = get_index_at (params.position.line - 1) (params.position.character - 1) content in
  let context = get_code_context ~max_index:index uri content in
  try
    let items = context.vars |> StringMap.bindings |> List.map (function name, type_ ->
        let open CompletionItem in
        {
          label = name;
          detail = Some (Src.Structures.Type.to_string type_)
        })
    in Sender.send_response oc (Jsonrpc.Response.ok id (`List (items |> List.map CompletionItem.yojson_of_t)))
  with Not_found -> ()
