module Id = struct
  type t =
      String of string
    | Int of int

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `String s -> String s
    | `Int i -> Int i
    | json -> Json.error "Id.t" json

  let yojson_of_t : t -> Yojson.Safe.t = function
    | String s -> `String s
    | Int i -> `Int i
end

module Message = struct
  let jsonrpc = "2.0"
end


module Request = struct
  include Message

  type t = {
    jsonrpc: string [@default jsonrpc];
    id: Id.t option; [@yojson.option]
    method_: string [@key "method"];
    params: Json.t option [@yojson.opton]
  }
  [@@deriving yojson]

  let make ~id ~params ~method_ = { jsonrpc; id; method_; params }

  let params f t = match t.params with
      Some params -> Ok (f params)
    | None -> Error "params are required"

end

module Response = struct
  include Message

  module Error = struct
    module Code = struct
      type t =
        | ParseError
        | InvalidRequest
        | MethodNotFound
        | InvalidParams
        | InternalError
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestCancelled
        | ContentModified

      let of_int = function
        | -32700 -> ParseError
        | -32600 -> InvalidRequest
        | -32601 -> MethodNotFound
        | -32602 -> InvalidParams
        | -32603 -> InternalError
        | -32099 -> ServerErrorStart
        | -32000 -> ServerErrorEnd
        | -32002 -> ServerNotInitialized
        | -32001 -> UnknownErrorCode
        | -32800 -> RequestCancelled
        | -32801 -> ContentModified
        | _ -> UnknownErrorCode

      let to_int = function
        | ParseError -> -32700
        | InvalidRequest -> -32600
        | MethodNotFound -> -32601
        | InvalidParams -> -32602
        | InternalError -> -32603
        | ServerErrorStart -> -32099
        | ServerErrorEnd -> -32000
        | ServerNotInitialized -> -32002
        | UnknownErrorCode -> -32001
        | RequestCancelled -> -32800
        | ContentModified -> -32801

      let yojson_of_t code = `Int(to_int code)
      let t_of_yojson = function
        | `Int i -> of_int i
        | _ -> UnknownErrorCode
    end

    type t = {
      code: Code.t;
      message: string;
      data: Json.t option [@yojson.option]
    }
    [@@deriving yojson]

    let make ?data ~code ~message () = { code; message; data }

    let of_exn exn =
      let message = Printexc.to_string exn in
      make ~code: InternalError ~message
  end

  type t = {
    jsonrpc: string; [@default jsonrpc]
    id: Id.t;
    result: Json.t option; [@yojson.option]
    error: Error.t option; [@yojson.option]
  }
  [@@deriving yojson]

  let make ~id ~result = match result with
    | Ok result -> { jsonrpc; id; result = Some result; error = None }
    | Error error -> { jsonrpc; id; result = None; error = Some error }

  let ok id result = make ~id ~result: (Ok result)
  let error id error = make ~id ~result: (Error error)
end
(*
type packet =
  | Request of Request.t
  | Reponse of Response.t
  | Notification of Notification.t
*)
