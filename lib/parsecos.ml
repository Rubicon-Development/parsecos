open! Core
module Var = Parsecos_var
module Param = Parsecos_param
module Formula = Parsecos_formula
module Affine = Parsecos_affine
module Constraint = Parsecos_constraint
module Csc_matrix = Parsecos_csc_matrix
module Ecos_data = Parsecos_ecos_data
module Ecos_template = Parsecos_ecos_template
module Error = Parsecos_error
module Problem = Parsecos_problem
module Parsed_ast = Parsecos_parsed_ast
module Elaborate = Parsecos_elaborate
module Json = Parsecos_json
module Dsl = Parsecos_dsl

let problem_of_json = Parsecos_json.problem
let problem_of_json_string = Parsecos_json.problem_of_string
let problem_of_text = Parsecos_dsl.problem

let ecos_template_of_json json =
  Result.map (problem_of_json json) ~f:Parsecos_problem.to_ecos_template
;;

let ecos_template_of_json_string text =
  Result.map (problem_of_json_string text) ~f:Parsecos_problem.to_ecos_template
;;

let ecos_template_of_text text =
  Result.map (Parsecos_dsl.template_problem text) ~f:Parsecos_problem.to_ecos_template
;;

let ecos_data_of_json json =
  Result.bind (problem_of_json json) ~f:Parsecos_problem.to_ecos_data_result
;;

let ecos_data_of_json_string text =
  Result.bind (problem_of_json_string text) ~f:Parsecos_problem.to_ecos_data_result
;;

let ecos_data_of_text text =
  Result.bind (problem_of_text text) ~f:Parsecos_problem.to_ecos_data_result
;;
