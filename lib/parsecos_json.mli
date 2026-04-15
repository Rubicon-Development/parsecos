val model : Yojson.Safe.t -> (Parsecos_parsed_ast.model, Parsecos_error.t) result
val problem : Yojson.Safe.t -> (Parsecos_problem.t, Parsecos_error.t) result
val problem_of_string : string -> (Parsecos_problem.t, Parsecos_error.t) result
