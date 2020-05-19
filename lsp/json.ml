type t = Yojson.Safe.t

let yojson_of_t x : t = x

let t_of_yojson x : t = x

let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

module Jsonable = Ppx_yojson_conv_lib.Yojsonable
