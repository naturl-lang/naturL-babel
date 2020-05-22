open Types

type t =
  | Shutdown
  | Initialize of InitializeParams.t
  | TextDocumentDefinition of DefinitionParams.t
  | TextDocumentCompletion of CompletionParams.t
  | UnknownRequest of string


let of_jsonrpc (request: Jsonrpc.Request.t) =
  match request.method_ with
  | "shutdown" -> Ok Shutdown
  | "initialize" -> request |> Jsonrpc.Request.params (fun json -> Initialize (InitializeParams.t_of_yojson json))
  | "textDocument/declaration" | "textDocument/definition" ->
    request |> Jsonrpc.Request.params (fun json -> TextDocumentDefinition (DefinitionParams.t_of_yojson json))
  | "textDocument/completion" -> request |> Jsonrpc.Request.params (fun json -> TextDocumentCompletion (CompletionParams.t_of_yojson json))
  | m -> Ok (UnknownRequest m)
