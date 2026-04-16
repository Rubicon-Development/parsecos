open! Core
open Angstrom

let parse_all parser input =
  match parse_string ~consume:All parser input with
  | Ok value -> Ok value
  | Error message -> Error message
;;

let is_identifier_start char = Char.is_alpha char || Char.equal char '_'
let is_identifier_continue char = Char.is_alphanum char || Char.equal char '_'
let whitespace = skip_while Char.is_whitespace
let lex parser = whitespace *> parser <* whitespace

let number_text =
  let sign = option "" (char '+' <|> char '-' >>| String.of_char) in
  let digits = take_while1 Char.is_digit in
  let fractional = option "" (char '.' *> digits >>| fun digits -> "." ^ digits) in
  let exponent =
    option
      ""
      ((char 'e' <|> char 'E') *> sign
       >>= fun sign -> digits >>| fun digits -> "e" ^ sign ^ digits)
  in
  lift4
    (fun sign digits fractional exponent -> sign ^ digits ^ fractional ^ exponent)
    sign
    digits
    fractional
    exponent
;;

let number = lex (number_text >>| Float.of_string)
let integer = lex (take_while1 Char.is_digit >>| Int.of_string)

let signed_integer_text =
  let sign = option "" (char '+' <|> char '-' >>| String.of_char) in
  let digits = take_while1 Char.is_digit in
  lift2 (fun sign digits -> sign ^ digits) sign digits
;;

let signed_integer = lex (signed_integer_text >>| Int.of_string)

let identifier =
  lex
    (satisfy is_identifier_start
     >>= fun first ->
     take_while is_identifier_continue >>| fun rest -> String.of_char first ^ rest)
;;

let reference =
  let indices = lex (char '[') *> sep_by1 (lex (char ',')) integer <* lex (char ']') in
  identifier
  >>= fun name ->
  option None (indices >>| Option.some)
  >>= function
  | None -> return (Parsecos_parsed_ast.Scalar_ref name)
  | Some [ index ] -> return (Parsecos_parsed_ast.Array1_ref { name; index })
  | Some [ index1; index2 ] ->
    return (Parsecos_parsed_ast.Array2_ref { name; index1; index2 })
  | Some _ -> fail "only one or two indices are supported"
;;

let expr =
  fix (fun expr ->
    let parens = lex (char '(') *> expr <* lex (char ')') in
    let atom =
      choice
        [ parens
        ; (number >>| fun value -> Parsecos_parsed_ast.Constant value)
        ; (reference >>| fun reference -> Parsecos_parsed_ast.Var reference)
        ]
    in
    let negated expr = Parsecos_parsed_ast.Scale { coefficient = -1.0; expr } in
    let unary = choice [ lex (char '-') *> atom >>| negated; atom ] in
    let product =
      unary
      >>= fun first ->
      many (lex (char '*') *> unary)
      >>| List.fold ~init:first ~f:(fun left right ->
        Parsecos_parsed_ast.Mul (left, right))
    in
    let operator = lex (char '+' >>| (fun _ -> `Add) <|> (char '-' >>| fun _ -> `Sub)) in
    product
    >>= fun first ->
    many (lift2 (fun operator rhs -> operator, rhs) operator product)
    >>| List.fold ~init:first ~f:(fun acc (operator, rhs) ->
      match operator with
      | `Add -> Parsecos_parsed_ast.Sum [ acc; rhs ]
      | `Sub -> Parsecos_parsed_ast.Sub (acc, rhs)))
;;

let domain =
  lex
    (choice
       [ string "continuous" *> return Parsecos_parsed_ast.Continuous
       ; string "boolean" *> return Parsecos_parsed_ast.Boolean
       ; string "integer" *> return Parsecos_parsed_ast.Integer
       ])
;;

let dimensions = lex (char '[') *> sep_by1 (lex (char ',')) integer <* lex (char ']')

let var_decl =
  domain
  >>= fun domain ->
  identifier
  >>= fun name ->
  option None (dimensions >>| Option.some)
  >>= function
  | None -> return (Parsecos_parsed_ast.Scalar { name; domain })
  | Some [ length ] -> return (Parsecos_parsed_ast.Array1 { name; domain; length })
  | Some [ dim1; dim2 ] ->
    return (Parsecos_parsed_ast.Array2 { name; domain; dim1; dim2 })
  | Some _ -> fail "only one or two dimensions are supported"
;;

let param_decl =
  identifier
  >>= fun name ->
  option None (dimensions >>| Option.some)
  >>= function
  | None -> return (Parsecos_param.scalar name)
  | Some [ length ] -> return (Parsecos_param.array1 ~name ~length)
  | Some [ dim1; dim2 ] -> return (Parsecos_param.array2 ~name ~dim1 ~dim2)
  | Some _ -> fail "only one or two dimensions are supported"
;;

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
    string "norm" *> whitespace *> char '(' *> sep_by1 (lex (char ',')) expr
    <* lex (char ')')
    >>= fun xs -> lex (string "<=") *> expr >>| fun t -> Parsecos_parsed_ast.Soc { t; xs }
  in
  lex
    (norm_constraint
     <|> (expr
          >>= fun lhs -> relation >>= fun relation -> expr >>| fun rhs -> relation lhs rhs
         ))
;;

let parameter_value =
  identifier
  >>= fun name ->
  lex (char '=') *> lex (char '[') *> sep_by (lex (char ',')) number
  <* lex (char ']')
  >>| fun values -> name, Array.of_list values
;;

let for_header =
  string "for" *> whitespace *> identifier
  >>= fun index ->
  whitespace *> string "in" *> whitespace *> signed_integer
  >>= fun start ->
  string ".." *> signed_integer <* lex (char ':') >>| fun finish -> index, start, finish
;;

type section =
  | Params
  | Vars
  | Minimize
  | Constraints

type accumulator =
  { current : section option
  ; params : (int * string) list
  ; vars : (int * string) list
  ; objective : (int * string) list
  ; constraints : (int * string) list
  }

let empty = { current = None; params = []; vars = []; objective = []; constraints = [] }

let dsl_error line_number message =
  Parsecos_error.Dsl_parse (Printf.sprintf "line %d: %s" line_number message)
;;

let add_line acc line_number line =
  match acc.current with
  | None -> Error (dsl_error line_number "expected a section header")
  | Some Params -> Ok { acc with params = acc.params @ [ line_number, line ] }
  | Some Vars -> Ok { acc with vars = acc.vars @ [ line_number, line ] }
  | Some Minimize -> Ok { acc with objective = acc.objective @ [ line_number, line ] }
  | Some Constraints ->
    Ok { acc with constraints = acc.constraints @ [ line_number, line ] }
;;

let handle_header acc section rest line_number =
  let acc = { acc with current = Some section } in
  if String.is_empty rest then Ok acc else add_line acc line_number rest
;;

let format_number value = Printf.sprintf "%g" value

let eval_index_expr index_env line_number text =
  let text = String.filter text ~f:(fun char -> not (Char.is_whitespace char)) in
  if String.is_empty text
  then Error (dsl_error line_number "empty index expression")
  else (
    match Int.of_string_opt text with
    | Some value -> Ok value
    | None ->
      if not (is_identifier_start text.[0])
      then
        Error
          (dsl_error line_number (Printf.sprintf "unsupported index expression %S" text))
      else (
        let rec split position =
          if position = String.length text || not (is_identifier_continue text.[position])
          then position
          else split (position + 1)
        in
        let split_at = split 1 in
        let name = String.prefix text split_at in
        let suffix = String.drop_prefix text split_at in
        Result.bind
          (Map.find index_env name
           |> Result.of_option
                ~error:
                  (dsl_error line_number (Printf.sprintf "unknown loop index %s" name)))
          ~f:(fun base ->
            if String.is_empty suffix
            then Ok base
            else (
              let sign = suffix.[0] in
              let digits = String.drop_prefix suffix 1 in
              match Int.of_string_opt digits with
              | None ->
                Error
                  (dsl_error
                     line_number
                     (Printf.sprintf "unsupported index offset %S" suffix))
              | Some offset ->
                Ok
                  (if Char.equal sign '+'
                   then base + offset
                   else if Char.equal sign '-'
                   then base - offset
                   else base)))))
;;

let substitute_line ~params ~index_env (line_number, line) =
  let buffer = Buffer.create (String.length line) in
  let line_length = String.length line in
  let rec find_closing_bracket index depth =
    if index >= line_length
    then Error (dsl_error line_number "unclosed index bracket")
    else if Char.equal line.[index] ']'
    then if depth = 0 then Ok index else find_closing_bracket (index + 1) (depth - 1)
    else if Char.equal line.[index] '['
    then find_closing_bracket (index + 1) (depth + 1)
    else find_closing_bracket (index + 1) depth
  in
  let rec loop index =
    if index >= line_length
    then Ok (Buffer.contents buffer)
    else if is_identifier_start line.[index]
    then (
      let rec consume_identifier position =
        if position < line_length && is_identifier_continue line.[position]
        then consume_identifier (position + 1)
        else position
      in
      let identifier_end = consume_identifier (index + 1) in
      if identifier_end < line_length && Char.equal line.[identifier_end] '['
      then
        Result.bind
          (find_closing_bracket (identifier_end + 1) 0)
          ~f:(fun closing_bracket ->
            let name = String.sub line ~pos:index ~len:(identifier_end - index) in
            let raw_indices =
              String.sub
                line
                ~pos:(identifier_end + 1)
                ~len:(closing_bracket - identifier_end - 1)
            in
            let evaluate_indices =
              String.split raw_indices ~on:','
              |> List.map ~f:String.strip
              |> List.map ~f:(eval_index_expr index_env line_number)
              |> Result.all
            in
            Result.bind evaluate_indices ~f:(fun indices ->
              match Map.find params name with
              | Some values ->
                (match indices with
                 | [ position ] ->
                   if position < 0 || position >= Array.length values
                   then
                     Error
                       (dsl_error
                          line_number
                          (Printf.sprintf
                             "parameter %s index %d out of bounds"
                             name
                             position))
                   else (
                     Buffer.add_string buffer (format_number values.(position));
                     loop (closing_bracket + 1))
                 | _ ->
                   Error
                     (dsl_error
                        line_number
                        (Printf.sprintf "parameter %s expects one index" name)))
              | None ->
                Buffer.add_string buffer name;
                Buffer.add_char buffer '[';
                Buffer.add_string
                  buffer
                  (indices |> List.map ~f:Int.to_string |> String.concat ~sep:",");
                Buffer.add_char buffer ']';
                loop (closing_bracket + 1)))
      else (
        Buffer.add_substring buffer line ~pos:index ~len:(identifier_end - index);
        loop identifier_end))
    else (
      Buffer.add_char buffer line.[index];
      loop (index + 1))
  in
  loop 0
;;

let parse_for_header line_number line =
  parse_all (whitespace *> for_header <* whitespace) line
  |> Result.map_error ~f:(fun message ->
    dsl_error line_number (Printf.sprintf "%s while parsing loop header" message))
;;

let parse_parameter_value line_number line =
  parse_all (whitespace *> parameter_value <* whitespace) line
  |> Result.map_error ~f:(fun message ->
    dsl_error line_number (Printf.sprintf "%s while parsing parameter" message))
;;

let parse_param_decl line_number line =
  parse_all (whitespace *> param_decl <* whitespace) line
  |> Result.map_error ~f:(fun message ->
    dsl_error
      line_number
      (Printf.sprintf "%s while parsing parameter declaration" message))
;;

let rec take_loop_body start_line depth acc = function
  | [] -> Error (dsl_error start_line "missing end for loop")
  | (line_number, line) :: rest ->
    if String.equal line "end"
    then
      if depth = 0
      then Ok (List.rev acc, rest)
      else take_loop_body start_line (depth - 1) ((line_number, line) :: acc) rest
    else if String.is_prefix line ~prefix:"for "
    then take_loop_body start_line (depth + 1) ((line_number, line) :: acc) rest
    else take_loop_body start_line depth ((line_number, line) :: acc) rest
;;

let loop_range start finish =
  if start <= finish
  then List.range start (finish + 1)
  else List.range ~stride:(-1) start (finish - 1)
;;

let rec expand_lines ~params ~index_env = function
  | [] -> Ok []
  | (line_number, line) :: rest ->
    if String.equal line "end"
    then Error (dsl_error line_number "unexpected end")
    else if String.is_prefix line ~prefix:"for "
    then
      Result.bind
        (parse_for_header line_number line)
        ~f:(fun (index_name, start, finish) ->
          Result.bind (take_loop_body line_number 0 [] rest) ~f:(fun (body, remaining) ->
            Result.bind
              (loop_range start finish
               |> List.map ~f:(fun value ->
                 expand_lines
                   ~params
                   ~index_env:(Map.set index_env ~key:index_name ~data:value)
                   body)
               |> Result.all)
              ~f:(fun expanded_blocks ->
                Result.map (expand_lines ~params ~index_env remaining) ~f:(fun tail ->
                  List.concat expanded_blocks @ tail))))
    else
      Result.bind
        (substitute_line ~params ~index_env (line_number, line))
        ~f:(fun expanded_line ->
          Result.map (expand_lines ~params ~index_env rest) ~f:(fun tail ->
            (line_number, expanded_line) :: tail))
;;

let collect_param_values lines =
  List.fold_result lines ~init:String.Map.empty ~f:(fun params (line_number, line) ->
    Result.bind (parse_parameter_value line_number line) ~f:(fun (name, values) ->
      if Map.mem params name
      then Error (dsl_error line_number (Printf.sprintf "duplicate parameter %s" name))
      else Ok (Map.set params ~key:name ~data:values)))
;;

let parse_sections text =
  let lines = String.split_lines text in
  List.foldi lines ~init:(Ok empty) ~f:(fun index acc_result raw_line ->
    let line_number = index + 1 in
    let line = String.strip raw_line in
    Result.bind acc_result ~f:(fun acc ->
      if String.is_empty line || String.is_prefix line ~prefix:"#"
      then Ok acc
      else if String.is_prefix line ~prefix:"params:"
      then handle_header acc Params (String.strip (String.drop_prefix line 7)) line_number
      else if String.is_prefix line ~prefix:"vars:"
      then handle_header acc Vars (String.strip (String.drop_prefix line 5)) line_number
      else if String.is_prefix line ~prefix:"minimize:"
      then
        handle_header acc Minimize (String.strip (String.drop_prefix line 9)) line_number
      else if String.is_prefix line ~prefix:"subject to:"
      then
        handle_header
          acc
          Constraints
          (String.strip (String.drop_prefix line 11))
          line_number
      else if String.is_prefix line ~prefix:"constraints:"
      then
        handle_header
          acc
          Constraints
          (String.strip (String.drop_prefix line 12))
          line_number
      else add_line acc line_number line))
;;

let parse_line parser kind (line_number, line) =
  parse_all (whitespace *> parser <* whitespace) line
  |> Result.map_error ~f:(fun message ->
    Parsecos_error.Dsl_parse
      (Printf.sprintf "line %d: %s while parsing %s" line_number message kind))
;;

let build_model ~params ~vars ~objective_lines ~constraint_lines =
  Result.bind
    (List.map vars ~f:(parse_line var_decl "variable declaration") |> Result.all)
    ~f:(fun vars ->
      Result.bind
        (parse_all
           (whitespace *> expr <* whitespace)
           (objective_lines |> List.map ~f:snd |> String.concat ~sep:" + ")
         |> Result.map_error ~f:(fun message ->
           Parsecos_error.Dsl_parse (Printf.sprintf "objective: %s" message)))
        ~f:(fun objective ->
          Result.map
            (List.map constraint_lines ~f:(parse_line constraint_ "constraint")
             |> Result.all)
            ~f:(fun constraints ->
              ({ params; vars; objective = Minimize objective; constraints }
               : Parsecos_parsed_ast.model))))
;;

let model text =
  Result.bind (parse_sections text) ~f:(fun acc ->
    if List.is_empty acc.objective
    then Error (Parsecos_error.Dsl_parse "missing minimize section")
    else
      Result.bind
        (List.map acc.params ~f:(fun (line_number, line) ->
           parse_param_decl line_number line)
         |> Result.all)
        ~f:(fun params ->
          Result.bind
            (expand_lines
               ~params:String.Map.empty
               ~index_env:String.Map.empty
               acc.objective)
            ~f:(fun objective_lines ->
              Result.bind
                (expand_lines
                   ~params:String.Map.empty
                   ~index_env:String.Map.empty
                   acc.constraints)
                ~f:(fun constraint_lines ->
                  build_model ~params ~vars:acc.vars ~objective_lines ~constraint_lines))))
;;

let template_problem text = Result.bind (model text) ~f:Parsecos_elaborate.problem

let problem text =
  Result.bind (parse_sections text) ~f:(fun acc ->
    if List.is_empty acc.objective
    then Error (Parsecos_error.Dsl_parse "missing minimize section")
    else
      Result.bind (collect_param_values acc.params) ~f:(fun params ->
        Result.bind
          (expand_lines ~params ~index_env:String.Map.empty acc.objective)
          ~f:(fun objective_lines ->
            Result.bind
              (expand_lines ~params ~index_env:String.Map.empty acc.constraints)
              ~f:(fun constraint_lines ->
                Result.bind
                  (build_model
                     ~params:[]
                     ~vars:acc.vars
                     ~objective_lines
                     ~constraint_lines)
                  ~f:Parsecos_elaborate.problem))))
;;
