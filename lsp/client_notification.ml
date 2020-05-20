open Types

type t =
    TextDocumentDidOpen of DidOpenParams.t
  | TextDocumentDidChange of DidChangeParams.t
  | TextDocumentDidClose of DidCloseParams.t
  | Initialized
  | Exit
  | Unknown_notification of string

let of_jsonrpc (request: Jsonrpc.Request.t) =
  match request.method_ with
  | "initialized" -> Ok Initialized
  | "exit" -> Ok Exit
  | "textDocument/didOpen" -> request |> Jsonrpc.Request.params (fun json -> TextDocumentDidOpen (DidOpenParams.t_of_yojson json))
  | "textDocument/didChange" -> request |> Jsonrpc.Request.params (fun json -> TextDocumentDidChange (DidChangeParams.t_of_yojson json))
  | "textDocument/didClose" -> request |> Jsonrpc.Request.params (fun json -> TextDocumentDidClose (DidCloseParams.t_of_yojson json))
  | n -> Ok (Unknown_notification n)
