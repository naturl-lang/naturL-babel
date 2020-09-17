type t = {
  content_length: int;
  content_type: string;
}


exception Invalid_line of string
exception Unknown_field of string
exception Missing_content_length

module Field = struct
  type t =
      Content_length
    | Content_type

  let to_string = function
      Content_length -> "Content-Length"
    | Content_type -> "Content-Type"

  let of_string = function
      "Content-Length" -> Content_length
    | "Content-Type" -> Content_type
    | field -> raise (Unknown_field field)
end


let default_type = "application/vscode-jsonrpc; charset: utf-8"
let empty = { content_length = -1;
              content_type = default_type }

let read ic =
  let rec read t =
    let line = input_line ic in print_endline line;
    match String.split_on_char ':' line with
    | ("" :: [] | []) when Sys.win32 || Sys.cygwin -> t
    | "\r" :: [] when Sys.unix -> t
    | field :: value :: [] -> let value = String.trim value in
      (match Field.of_string field with
       | Content_length -> read { t with content_length = int_of_string value }
       | Content_type -> read { t with content_type = value})
    | l -> print_endline ("Invalid line - '" ^ (String.escaped (String.concat ":" l)) ^ "'"); raise (Invalid_line (String.concat ":" l))
  in let header = read empty
  in if header.content_length = empty.content_length then
    begin
      print_endline "Missing content length";
      raise Missing_content_length
    end
  else
    header

let crlf = "\r\n"
let write oc { content_length; content_type } =
  output_string oc (Field.to_string Content_length ^ ": ");
  output_string oc (string_of_int content_length);
  output_string oc crlf;
  output_string oc (Field.to_string Content_type ^ ": ");
  output_string oc content_type;
  output_string oc crlf;
  output_string oc crlf

let make ?content_type ~content_length () =
  { content_length; content_type = Option.value content_type ~default:default_type }
