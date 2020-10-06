open Js_of_ocaml

let send_to_lsp_server callback message =
  Listener.receive callback @@ Js.to_string message

let () =
  Js.Unsafe.global##.send := Js.wrap_callback send_to_lsp_server;
  Js.export "send" send_to_lsp_server;

(*
let () = send_to_lsp_server print_endline
"\r\n\r\n{
  \"id\": 1,
  \"method\": \"initialize\",
  \"params\": {
    \"processId\": null,
    \"rootUri\": null,
    \"capabilities\": {
      \"textDocument\": {
        \"completion\": { },
        \"definition\": { },
        \"publishDiagnostics\": {
          \"relatedInformation\": true
        }
      }
    }
  }
}";;
*)
