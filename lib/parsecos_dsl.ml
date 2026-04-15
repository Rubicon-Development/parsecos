open! Core
open Angstrom

let parse_all parser input =
  match parse_string ~consume:All parser input with
  | Ok value -> Ok value
  | Error message -> Error message

let is_identifier_start char = Char.is_alpha char || Char.equal char '_'
let is_identifier_continue char = Char.is_alphanum char || Char.equal char '_'
let whitespace = skip_while Char.is_whitespace
let lex parser = whitespace *> parser <* whitespace

let number_text =
  let sign = option "" ((char '+' <|> char '-') >>| String.of_char) in
  let digits = take_while1 Char.is_digit in
  let fractional = option "" (char '.' *> digits >>| fun digits -> "." ^ digits) in
  let exponent =
    option "" ((char 'e' <|> char 'E') *> sign >>= fun sign -> digits >>| fun digits -> "e" ^ sign ^ digits)
  in
  lift4 (fun sign digits fractional exponent -> sign ^ digits ^ fractional ^ exponent) sign digits fractional exponent

let number = lex (number_text >>| Float.of_string)
let integer = lex (take_while1 Char.is_digit >>| Int.of_string)

let identifier =
  lex
    (satisfy is_identifier_start >>= fun first ->
     take_while is_identifier_continue >>| fun rest -> String.of_char first ^ rest)

let expr =
  fix (fun expr ->
    let parens = lex (char '(') *> expr <* lex (char ')') in
    let indices = lex (char '[') *> sep_by1 (lex (char ',')) integer <* lex (char ']') in
    let reference =
      identifier >>= fun name ->
      option None (indices >>| Option.some) >>= function
      | None -> return (Parsecos_parsed_ast.Scalar_ref name)
      | Some [ index ] -> return (Parsecos_parsed_ast.Array1_ref { name; index })
      | Some [ index1; index2 ] -> return (Parsecos_parsed_ast.Array2_ref { name; index1; index2 })
      | Some _ -> fail "only one or two indices are supported"
    in
    let atom =
      choice
        [ parens
        ; (number >>| fun value -> Parsecos_parsed_ast.Constant value)
        ; (reference >>| fun reference -> Parsecos_parsed_ast.Var reference)
        ]
    in
    let negated expr = Parsecos_parsed_ast.Scale { coefficient = -1.0; expr } in
    let term =
      choice
        [ lift2 (fun coefficient expr -> Parsecos_parsed_ast.Scale { coefficient; expr }) (number <* lex (char '*')) atom
        ; lex (char '-') *> atom >>| negated
        ; atom
        ]
    in
    let operator = lex ((char '+' >>| fun _ -> `Add) <|> (char '-' >>| fun _ -> `Sub)) in
    term >>= fun first ->
    many (lift2 (fun operator rhs -> operator, rhs) operator term)
    >>| List.fold ~init:first ~f:(fun acc (operator, rhs) ->
      match operator with
      | `Add -> Parsecos_parsed_ast.Sum [ acc; rhs ]
      | `Sub -> Parsecos_parsed_ast.Sub (acc, rhs)))

let domain =
  lex
    (choice
       [ string "continuous" *> return Parsecos_parsed_ast.Continuous
       ; string "boolean" *> return Parsecos_parsed_ast.Boolean
       ; string "integer" *> return Parsecos_parsed_ast.Integer
       ])

let var_decl =
  let dimensions = lex (char '[') *> sep_by1 (lex (char ',')) integer <* lex (char ']') in
  domain >>= fun domain ->
  identifier >>= fun name ->
  option None (dimensions >>| Option.some) >>= function
  | None -> return (Parsecos_parsed_ast.Scalar { name; domain })
  | Some [ length ] -> return (Parsecos_parsed_ast.Array1 { name; domain; length })
  | Some [ dim1; dim2 ] -> return (Parsecos_parsed_ast.Array2 { name; domain; dim1; dim2 })
  | Some _ -> fail "only one or two dimensions are supported"

let constraint_ =
  let relation =
    lex
      (choice
         [ string "<=" *> return (fun lhs rhs -> Parsecos_parsed_ast.Le { lhs; rhs })
         ; string ">=" *> return (fun lhs rhs -> Parsecos_parsed_ast.Ge { lhs; rhs })
         ; string "=" *> return (fun lhs rhs -> Parsecos_parsed_ast.Eq { lhs; rhs })
         ])
  in
  let norm_constraint =
    string "norm" *> whitespace *> char '(' *> sep_by1 (lex (char ',')) expr <* lex (char ')')
    >>= fun xs ->
    lex (string "<=") *> expr >>| fun t -> Parsecos_parsed_ast.Soc { t; xs }
  in
  lex (norm_constraint <|> (expr >>= fun lhs -> relation >>= fun relation -> expr >>| fun rhs -> relation lhs rhs))

type section =
  | Vars
  | Minimize
  | Constraints

type accumulator =
  { current : section option
  ; vars : (int * string) list
  ; objective : (int * string) list
  ; constraints : (int * string) list
  }

let empty = { current = None; vars = []; objective = []; constraints = [] }

let add_line acc line_number line =
  match acc.current with
  | None -> Error (Parsecos_error.Dsl_parse (Printf.sprintf "line %d: expected a section header" line_number))
  | Some Vars -> Ok { acc with vars = acc.vars @ [ line_number, line ] }
  | Some Minimize -> Ok { acc with objective = acc.objective @ [ line_number, line ] }
  | Some Constraints -> Ok { acc with constraints = acc.constraints @ [ line_number, line ] }

let handle_header acc section rest line_number =
  let acc = { acc with current = Some section } in
  if String.is_empty rest then Ok acc else add_line acc line_number rest

let model text =
  let lines = String.split_lines text in
  let parse_lines =
    List.foldi lines ~init:(Ok empty) ~f:(fun index acc_result raw_line ->
      let line_number = index + 1 in
      let line = String.strip raw_line in
      Result.bind acc_result ~f:(fun acc ->
        if String.is_empty line || String.is_prefix line ~prefix:"#"
        then Ok acc
        else if String.is_prefix line ~prefix:"vars:"
        then handle_header acc Vars (String.strip (String.drop_prefix line 5)) line_number
        else if String.is_prefix line ~prefix:"minimize:"
        then handle_header acc Minimize (String.strip (String.drop_prefix line 9)) line_number
        else if String.is_prefix line ~prefix:"subject to:"
        then handle_header acc Constraints (String.strip (String.drop_prefix line 11)) line_number
        else if String.is_prefix line ~prefix:"constraints:"
        then handle_header acc Constraints (String.strip (String.drop_prefix line 12)) line_number
        else add_line acc line_number line))
  in
  Result.bind parse_lines ~f:(fun acc ->
    if List.is_empty acc.objective
    then Error (Parsecos_error.Dsl_parse "missing minimize section")
    else
      let parse_line parser kind (line_number, line) =
        parse_all (whitespace *> parser <* whitespace) line
        |> Result.map_error ~f:(fun message ->
          Parsecos_error.Dsl_parse (Printf.sprintf "line %d: %s while parsing %s" line_number message kind))
      in
      Result.bind
        (List.map acc.vars ~f:(parse_line var_decl "variable declaration") |> Result.all)
        ~f:(fun vars ->
          Result.bind
            (parse_all (whitespace *> expr <* whitespace) (acc.objective |> List.map ~f:snd |> String.concat ~sep:" ")
             |> Result.map_error ~f:(fun message -> Parsecos_error.Dsl_parse (Printf.sprintf "objective: %s" message)))
            ~f:(fun objective ->
              List.map acc.constraints ~f:(parse_line constraint_ "constraint")
              |> Result.all
              |> Result.map ~f:(fun constraints ->
                { Parsecos_parsed_ast.vars; objective = Minimize objective; constraints }))))

let problem text = Result.bind (model text) ~f:Parsecos_elaborate.problem
