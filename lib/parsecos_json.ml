open! Core

let model json =
  try Ok (Parsecos_parsed_ast.model_of_yojson json) with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json) ->
    Error (Parsecos_error.Json_decode (Exn.to_string exn ^ ": " ^ Yojson.Safe.to_string json))

let problem json = Result.bind (model json) ~f:Parsecos_elaborate.problem

let problem_of_string text =
  try problem (Yojson.Safe.from_string text) with
  | Yojson.Json_error error -> Error (Parsecos_error.Json_parse error)
