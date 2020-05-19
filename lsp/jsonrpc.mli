module Id : sig
  type t = String of string | Int of int

  include Json.Jsonable.S with type t := t
end

module Notification : sig
  type t = {
    jsonrpc: string;
    method_: string;
    params: Json.t option
  }

  val make: ?params: Json.t -> method_ : string -> t

  include Json.Jsonable.S with type t := t
end

module Request : sig
  type t = {
    jsonrpc: string;
    id: Id.t;
    method_: string;
    params: Json.t option
  }

  val make: ?params: Json.t -> id: Id.t -> method_ : string -> t

  include Json.Jsonable.S with type t := t
end

module Response : sig
  module Error : sig
    module Code : sig
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
    end

    type t = {
      code: Code.t;
      message: string;
      data: Json.t option
    }

    val make: ?data: Json.t -> code: Code.t -> message: string -> t
    val of_exn: exn -> ?data: Json.t -> t
  end

  module ResponseResult : sig
    type t
    include Json.Jsonable.S with type t := t
  end

  type t = {
    jsonrpc: string;
    id: Id.t;
    result: ResponseResult.t
  }

  val make: id: Id.t -> result: ResponseResult.t -> t
  val ok: Id.t -> Json.t -> t
  val error: Id.t -> Error.t -> t

  include Json.Jsonable.S with type t := t
end
