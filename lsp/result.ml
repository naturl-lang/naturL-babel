module type ResultType = sig
  module Ok: sig
    type t
    val t_of_yojson: Yojson.Safe.t -> t
    val yojson_of_t: t -> Yojson.Safe.t
  end
  module Error: sig
    type t
    val t_of_yojson: Yojson.Safe.t -> t
    val yojson_of_t: t -> Yojson.Safe.t
  end
end

module Result(Q: ResultType) = struct
  type t =
    | Ok of Q.Ok.t
    | Error of Q.Error.t

  let yojson_of_t : t -> Yojson.Safe.t = function
    | Ok result -> `Assoc ["result", Q.Ok.yojson_of_t result]
    | Error error -> `Assoc ["error", Q.Error.yojson_of_t error]

  let t_of_yojson: Yojson.Safe.t -> t = function
    | `Assoc ["result", result] -> Ok (Q.Ok.t_of_yojson result)
    | `Assoc ["error", error] -> Error (Q.Error.t_of_yojson error)
    | json -> Json.error "Result.t" json
end
