open Jsonrpc

type packet =
    Response of Response.t
  | Notification of Request.t

let send_packet oc packet =
  let yojson = match packet with
    | Response response -> Response.yojson_of_t response
    | Notification notification -> Request.yojson_of_t notification
  in let json = Yojson.Safe.to_string yojson
  in let content_length = String.length json in
  Header.(write oc (make ~content_length ()));
  output_string oc (json ^ "\n")

let send_response oc response =
  send_packet oc (Response response)

let send_notification oc notification =
  send_packet oc (Notification notification)

let initialize oc id =
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
  send_response oc (Response.ok id params)
