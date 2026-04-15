module Var = Parsecos_var
module Affine = Parsecos_affine
module Constraint = Parsecos_constraint
module Csc_matrix = Parsecos_csc_matrix
module Ecos_data = Parsecos_ecos_data
module Error = Parsecos_error
module Problem = Parsecos_problem
module Parsed_ast = Parsecos_parsed_ast
module Elaborate = Parsecos_elaborate
module Json = Parsecos_json
module Dsl = Parsecos_dsl

val problem_of_json : Yojson.Safe.t -> (Problem.t, Error.t) result
val problem_of_json_string : string -> (Problem.t, Error.t) result
val problem_of_text : string -> (Problem.t, Error.t) result
val ecos_data_of_json : Yojson.Safe.t -> (Ecos_data.t, Error.t) result
val ecos_data_of_json_string : string -> (Ecos_data.t, Error.t) result
val ecos_data_of_text : string -> (Ecos_data.t, Error.t) result
