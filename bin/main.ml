open! Core
open Lwt.Infix
module Header = Cohttp.Header
module Request = Cohttp.Request
module Response = Cohttp_lwt_unix.Server

let json_headers = Header.init_with "content-type" "application/json"

let respond_json ~status json =
  Response.respond_string
    ~status
    ~headers:json_headers
    ~body:(Yojson.Safe.pretty_to_string json ^ "\n")
    ()
;;

let error_json ~status message =
  respond_json ~status (`Assoc [ "error", `String message ])
;;

let error_to_message error = Sexp.to_string_hum (Parsecos.Error.sexp_of_t error)

let ecos_response result =
  match result with
  | Ok data -> respond_json ~status:`OK (Parsecos.Ecos_data.yojson_of_t data)
  | Error error -> error_json ~status:`Bad_request (error_to_message error)
;;

let template_response result =
  match result with
  | Ok template -> respond_json ~status:`OK (Parsecos.Ecos_template.yojson_of_t template)
  | Error error -> error_json ~status:`Bad_request (error_to_message error)
;;

let docs_json =
  `Assoc
    [ "service", `String "parsecos"
    ; ( "endpoints"
      , `List
          [ `Assoc
              [ "method", `String "POST"
              ; "path", `String "/v1/dsl"
              ; "content_type", `String "text/plain"
              ; "description", `String "Parse the DSL and return ECOS_BB matrices as JSON"
              ]
          ; `Assoc
              [ "method", `String "POST"
              ; "path", `String "/v1/template/dsl"
              ; "content_type", `String "text/plain"
              ; ( "description"
                , `String "Parse the DSL and return a reusable ECOS template as JSON" )
              ]
          ; `Assoc
              [ "method", `String "POST"
              ; "path", `String "/v1/json"
              ; "content_type", `String "application/json"
              ; ( "description"
                , `String
                    "Parse a Parsecos.Parsed_ast.model JSON payload and return ECOS_BB \
                     matrices as JSON" )
              ]
          ; `Assoc
              [ "method", `String "POST"
              ; "path", `String "/v1/template/json"
              ; "content_type", `String "application/json"
              ; ( "description"
                , `String
                    "Parse a Parsecos.Parsed_ast.model JSON payload and return a \
                     reusable ECOS template as JSON" )
              ]
          ; `Assoc
              [ "method", `String "GET"
              ; "path", `String "/health"
              ; "description", `String "Health check"
              ]
          ] )
    ]
;;

let callback _connection request body =
  let meth = Request.meth request in
  let path = Uri.path (Request.uri request) in
  match meth, path with
  | `GET, "/" -> respond_json ~status:`OK docs_json
  | `GET, "/health" -> respond_json ~status:`OK (`Assoc [ "status", `String "ok" ])
  | `POST, "/v1/dsl" ->
    Cohttp_lwt.Body.to_string body
    >>= fun body -> ecos_response (Parsecos.ecos_data_of_text body)
  | `POST, "/v1/template/dsl" ->
    Cohttp_lwt.Body.to_string body
    >>= fun body -> template_response (Parsecos.ecos_template_of_text body)
  | `POST, "/v1/json" ->
    Cohttp_lwt.Body.to_string body
    >>= fun body -> ecos_response (Parsecos.ecos_data_of_json_string body)
  | `POST, "/v1/template/json" ->
    Cohttp_lwt.Body.to_string body
    >>= fun body -> template_response (Parsecos.ecos_template_of_json_string body)
  | `POST, _ -> error_json ~status:`Not_found "unknown endpoint"
  | _ -> error_json ~status:`Method_not_allowed "unsupported method"
;;

let port = ref 8080

let () =
  Stdlib.Arg.parse
    [ "-port", Stdlib.Arg.Set_int port, "Port to listen on" ]
    (fun argument -> failwith (Printf.sprintf "unexpected argument: %s" argument))
    "parsecos";
  printf "Listening on http://127.0.0.1:%d\n%!" !port;
  Lwt_main.run
    (Cohttp_lwt_unix.Server.create
       ~mode:(`TCP (`Port !port))
       (Cohttp_lwt_unix.Server.make ~callback ()))
;;
