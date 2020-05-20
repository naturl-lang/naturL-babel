open Types

type t =
    PublishDiagnostics of PublishDiagnosticsParams.t
  | Unknown_notification of string

let method_ = function
    PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | Unknown_notification _ -> assert false

let yojson_of_t = function
    PublishDiagnostics params -> PublishDiagnosticsParams.yojson_of_t params
  | Unknown_notification _ -> assert false

let to_jsonrpc t =
  let method_ = method_ t
  and params = Some (yojson_of_t t) in
  Jsonrpc.Request.make ~id:None ~params ~method_
