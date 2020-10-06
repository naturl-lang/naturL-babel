open Jsonrpc

type packet =
    Response of Response.t
  | Notification of Request.t

let send_packet callback packet =
  let yojson = match packet with
    | Response response -> Response.yojson_of_t response
    | Notification notification -> Request.yojson_of_t notification
  in let json = Yojson.Safe.pretty_to_string yojson
  in let content_length = String.length json in
  let header ="Content-Length:" ^ (string_of_int content_length) ^ "\r\n\r\n"
  in callback header json

let send_response callback response =
  send_packet callback (Response response)

let send_notification callback notification =
  send_packet callback (Notification notification)

let initialize callback id =
  let open Types in
  let params = InitializeResult.yojson_of_t {
      capabilities = Some {
        textDocumentSync = Some Full;
        completionProvider = Some {
          triggerCharacters = Some ['.']
        };
        signatureHelp = None;
        declarationProvider = Some true;
        definitionProvider = Some true;
        renameProvider = None;
        foldingRangeSupport = None
      };
      serverInfo = Some {
          name = "naturl-lsp-sender";
          version = None
        }
  } in
  send_response callback (Response.ok id params)
